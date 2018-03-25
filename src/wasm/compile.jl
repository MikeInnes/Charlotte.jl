using Base.Meta
using WebAssembly, WebAssembly.Instructions
using WebAssembly: WType, Func

walk(x, inner, outer) = outer(x)

function walk(x::Expr, inner, outer)
  y = Expr(x.head, map(inner, x.args)...)
  y.typ = x.typ
  return outer(y)
end

prepostwalk(f, g, x) = walk(f(x), x -> prepostwalk(f, g, x), g)
prewalk(f, x)  = prepostwalk(f, identity, x)
postwalk(f, x) = prepostwalk(identity, f, x)

deref(x) = x
deref(x::GlobalRef) = getfield(x.mod, x.name)

exprtype(code::CodeInfo, x) = typeof(deref(x))
exprtype(code::CodeInfo, x::Expr) = x.typ
exprtype(code::CodeInfo, x::QuoteNode) = typeof(x.value)
exprtype(code::CodeInfo, x::SSAValue) = code.ssavaluetypes[x.id+1]
exprtype(code::CodeInfo, x::SlotNumber) = code.slottypes[x.id]

# We don't want SSAValues on a stack machine.
# If it's only used once, just inline it.
# If more than once, we have to turn it into a local.

function ssacounts(code)
  counts = Dict{SSAValue,Any}()
  inc(x) = counts[x] = get(counts, x, 0) + 1
  for c in code
    isexpr(c, :(=)) && (c = c.args[2])
    prewalk(x -> (x isa SSAValue && inc(x); x), c)
  end
  return counts
end

apply_rhs(f, x) =
    isexpr(x, :(=)) ?
      Expr(:(=), x.args[1], f(x.args[2])) :
      f(x)

function inlinessa(code)
  counts = ssacounts(code)
  values = Dict{SSAValue,Any}()
  code′ = []
  for c in code
    c = apply_rhs(c) do x
      prewalk(x -> get(values, x, x), x)
    end
    isexpr(c, :(=)) && c.args[1] isa SSAValue && counts[c.args[1]] == 1 ?
      (values[c.args[1]] = c.args[2]) :
      push!(code′, c)
  end
  return code′
end

function ssalocals(cinfo, ex)
  n = length(cinfo.slotnames)
  ls = Dict{SSAValue,SlotNumber}()
  ex = prewalk(ex) do x
    x isa SSAValue || return x
    haskey(ls, x) && return ls[x]
    push!(cinfo.slottypes, exprtype(cinfo, x))
    ls[x] = SlotNumber(n += 1)
  end
  return ex
end

function rmssa(c)
  is = copy(c.code)
  while is[1] isa NewvarNode shift!(is) end
  ex = Expr(:block, inlinessa(is)...)
  ssalocals(c, ex)
end

# Julia -> WASM expression

wasmfuncs = Dict()
opcodes = Dict{String,UInt8}()

int_unary_ops = [       # i32   i64
  (:cttz_int, :ctz,       0x68, 0x7a)
]

int_binary_ops = [      # i32   i64
  (:add_int,  :add,       0x6a, 0x7c),
  (:sub_int,  :sub,       0x6b, 0x7d),
  (:mul_int,  :mul,       0x6c, 0x7e),
  (:and_int,  :and,       0x71, 0x83),
  (:or_int,   :or,        0x72, 0x84),
  (:xor_int,  :xor,       0x73, 0x85),
  (:shl_int,  :shl,       0x74, 0x86),
  (:lshr_int, :shr_u,     0x76, 0x88),
  (:ashr_int, :shr_s,     0x75, 0x87),
  (:(===),    :eq,        0x46, 0x51),
  (:slt_int,  :lt_s,      0x48, 0x53),
  (:sle_int,  :le_s,      0x4c, 0x57),
  (:ult_int,  :lt_u,      0x49, 0x54),
  (:ule_int,  :le_u,      0x4d, 0x58),
]

float_unary_ops = [     # f32   f64
  (:abs_float, :abs,      0x8b, 0x99),
  (:neg_float, :neg,      0x8c, 0x9a),
  (:rint_llvm, :nearest,  0x90, 0x9e),
]

float_binary_ops = [    # f32   f64
  (:add_float, :add,      0x92, 0xa0),
  (:sub_float, :sub,      0x93, 0xa1),
  (:mul_float, :mul,      0x94, 0xa2),
  (:div_float, :div,      0x95, 0xa3),
  (:eq_float,  :eq,       0x5b, 0x61),
  (:ne_float,  :ne,       0x5c, 0x62),
  (:lt_float,  :lt,       0x5d, 0x63),
  (:le_float,  :le,       0x5f, 0x65),
]

for (t,tops) in [("i",int_unary_ops),("f",float_unary_ops)]
  for (j, w, o32, o64) in tops
    wasmfuncs[GlobalRef(Base, j)] = function (T)
      Op(WType(T), w)
    end
    opcodes["$(t)32.$(w)"] = o32
    opcodes["$(t)64.$(w)"] = o64
  end
end

for (t,tops) in [("i",int_binary_ops),("f",float_binary_ops)]
  for (j, w, o32, o64) in int_binary_ops
    wasmfuncs[GlobalRef(Base, j)] = function (A,B)
      Op(WType(A), w)
    end
    opcodes["$(t)32.$(w)"] = o32
    opcodes["$(t)64.$(w)"] = o64
  end
end

wasmfuncs[GlobalRef(Base, :not_int)] = function (A)
  Op(WType(A), :eqz)
end

wasmfunc(f, xs...) = wasmfuncs[f](xs...)

wasmcalls = Dict()

# Intrinsics without wasm equivalents

wasmcalls[GlobalRef(Base, :muladd_float)] = function (i, x, y, z)
  xy = Expr(:call, GlobalRef(Base, :mul_float), x, y)
  xy.typ = exprtype(i, x)
  Expr(:call, nop,
    Expr(:call, GlobalRef(Base, :add_float), xy, z))
end

wasmcalls[GlobalRef(Base, :neg_int)] = function (i, x)
  Expr(:call, nop,
    Expr(:call, GlobalRef(Base, :sub_int), exprtype(i, x)(0), x))
end

wasmcalls[GlobalRef(Base, :flipsign_int)] = function (i, x, y)
  T = exprtype(i, x)
  cond = Expr(:call, GlobalRef(Base, :slt_int), x, T(0))
  sign = Expr(:call, GlobalRef(Base, :select_value), cond, T(-1), T(1))
  sign.typ = T
  Expr(:call, nop,
    Expr(:call, GlobalRef(Base, :mul_int), sign, x))
end

# TODO: should probably not do this
wasmcalls[GlobalRef(Base, :check_top_bit)] = function (i, x)
  nop
end

# More complex intrinsics

wasmcalls[GlobalRef(Base, :select_value)] = function (i, c, a, b)
  return Expr(:call, Select(), a, b, c)
end

wasmcalls[GlobalRef(Base, :bitcast)] = function (i, T, x)
  T isa GlobalRef && (T = getfield(T.mod, T.name))
  X = exprtype(i, x)
  WType(T) == WType(X) && return Expr(:call, nop, x)
  @assert sizeof(T) == sizeof(X)
  Expr(:call, Convert(WType(T), WType(X), :reinterpret), x)
end

wasmcalls[GlobalRef(Base, :fptosi)] = function (i, T, x)
  T isa GlobalRef && (T = getfield(T.mod, T.name))
  X = exprtype(i, x)
  Expr(:call, Convert(WType(T), WType(X), :trunc_s), x)
end

wasmcalls[GlobalRef(Base, :sitofp)] = function (i, T, x)
  T isa GlobalRef && (T = getfield(T.mod, T.name))
  X = exprtype(i, x)
  Expr(:call, Convert(WType(T), WType(X), :convert_s), x)
end

wasmcall(i, f, xs...) =
  haskey(wasmcalls, f) ? wasmcalls[f](i, xs...) :
  Expr(:call, wasmfunc(f, exprtype.(i, xs)...), xs...)

isprimitive(x) = false
isprimitive(x::GlobalRef) =
  deref(x) isa Core.IntrinsicFunction ||
  deref(x) isa Core.Builtin

function lowercalls(c::CodeInfo, code)
  prewalk(code) do x
    if isexpr(x, :call) && deref(x.args[1]) == Base.throw
      unreachable
    elseif (isexpr(x, :call) && isprimitive(x.args[1]))
      wasmcall(c, x.args...)
    elseif isexpr(x, :(=)) && x.args[1] isa SlotNumber
      Expr(:call, SetLocal(false, x.args[1].id-2), x.args[2])
    elseif x isa SlotNumber
      Local(x.id-2)
    elseif isexpr(x, :gotoifnot)
      Expr(:call, Goto(true, x.args[2]),
           Expr(:call, GlobalRef(Base, :not_int), x.args[1]))
    elseif x isa GotoNode
      Goto(false, x.label)
    elseif x isa LabelNode
      Label(x.label)
    elseif isexpr(x, :return)
      Expr(:call, Return(), x.args[1])
    else
      x
    end
  end
end

iscontrol(ex) = isexpr(ex, :while) || isexpr(ex, :if)

lower(c::CodeInfo) = lowercalls(c, rmssa(c))

# Convert to WASM instructions

towasm_(xs, is = Instruction[]) = (foreach(x -> towasm(x, is), xs); is)

function towasm(x, is = Instruction[])
  if x isa Instruction
    push!(is, x)
  elseif isexpr(x, :block)
    push!(is, Block(towasm_(x.args)))
  elseif isexpr(x, :call) && x.args[1] isa Instruction
    foreach(x -> towasm(x, is), x.args[2:end])
    push!(is, x.args[1])
  elseif isexpr(x, :if)
    towasm(x.args[1], is)
    push!(is, If(towasm_(x.args[2].args), towasm_(x.args[3].args)))
  elseif isexpr(x, :while)
    push!(is, Block([Loop(towasm_(x.args[2].args))]))
  elseif deref(x) isa Number
    push!(is, Const(deref(x)))
  elseif x isa LineNumberNode || isexpr(x, :inbounds) || isexpr(x, :meta)
  else
    error("Can't convert to wasm: $x")
  end
  return is
end

function code_wasm(ex, A)
  cinfo, R = code_typed(ex, A)[1]
  body = towasm_(lower(cinfo).args) |> Block |> WebAssembly.restructure |> WebAssembly.optimise
  Func([WType(T) for T in A.parameters],
       [WType(R)],
       [WType(P) for P in cinfo.slottypes[length(A.parameters)+2:end]],
       body)
end

macro code_wasm(ex)
  isexpr(ex, :call) || error("@code_wasm f(xs...)")
  :(code_wasm($(esc(ex.args[1])), Base.typesof($(esc.(ex.args[2:end])...))))
end
