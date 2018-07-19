using Base.Meta
using WebAssembly, WebAssembly.Instructions
using WebAssembly: WType, Func, Module, FuncType, Func, Table, Mem, Global, Elem, Data, Import, Export

# This struct stores state as a wasm module is built up.
struct ModuleState
  imports::Dict{Symbol, Import}
  exports::Dict{Symbol, Export}
  funcs::Dict{Symbol, Func}
  data::Dict{Symbol, Data}
end
ModuleState() = ModuleState(Dict{Symbol, Import}(), Dict{Symbol, Export}(), Dict{Symbol, Func}(), Dict{Symbol, Data}())

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

int_unary_ops = [
  (:cttz_int, :ctz)
]

int_binary_ops = [
  (:add_int, :add),
  (:sub_int, :sub),
  (:mul_int, :mul),
  (:and_int, :and),
  (:or_int,  :or),
  (:xor_int, :xor),
  (:shl_int, :shl),
  (:lshr_int, :shr_u),
  (:ashr_int, :shr_s),
  (:(===),   :eq),
  (:slt_int, :lt_s),
  (:sle_int, :le_s),
  (:ult_int, :lt_u),
  (:ule_int, :le_u),
]

float_unary_ops = [
  (:abs_float, :abs),
  (:neg_float, :neg),
  (:rint_llvm, :nearest),
]

float_binary_ops = [
  (:add_float, :add),
  (:sub_float, :sub),
  (:mul_float, :mul),
  (:div_float, :div),
  (:eq_float,  :eq),
  (:ne_float,  :ne),
  (:lt_float,  :lt),
  (:le_float,  :le),
]

for (j, w) in vcat(int_unary_ops, float_unary_ops)
  wasmfuncs[GlobalRef(Base, j)] = function (T)
    Op(WType(T), w)
  end
end

for (j, w) in vcat(int_binary_ops, float_binary_ops)
  wasmfuncs[GlobalRef(Base, j)] = function (A,B)
    Op(WType(A), w)
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

wasmcalls[GlobalRef(Base, :checked_trunc_sint)] = function (i, T, x)
  T isa GlobalRef && (T = getfield(T.mod, T.name))
  X = exprtype(i, x)
  @show (T, X)
  @show T == Int32 && X == Int64
  (T == Int32 && X == Int64) && return Expr(:call, Convert(WType(T), WType(X), :wrap), x)
  Expr(:call, Convert(WType(T), WType(X), :trunc_s), x)
end

wasmcalls[GlobalRef(Base, :sitofp)] = function (i, T, x)
  T isa GlobalRef && (T = getfield(T.mod, T.name))
  X = exprtype(i, x)
  Expr(:call, Convert(WType(T), WType(X), :convert_s), x)
end

wasmcalls[GlobalRef(Base, :arraylen)] = function (i, xs)
  a = Expr(:call, Call(Symbol("main/arraylen_", WType(eltype(exprtype(i, xs))))), xs)
  Expr(:call, Convert(WType(Int64), WType(Int32), :extend_u), a)
end

wasmcalls[GlobalRef(Base, :arraysize)] = function (i, xs, dim)
  if dim == 1
    return Expr(:call, Call(Symbol("main/arraylen_", WType(eltype(exprtype(i, xs))))), xs)
  else
    error("Multi dim arrays not supported")
  end
  nop
end

wasmcalls[GlobalRef(Base, :arrayref)] = function (i, xs, idx)
  a = Expr(:call, GlobalRef(Base, :sub_int), idx, 1)
  if exprtype(i, idx) == Int64 # Will need to change this for wasm64
    a = Expr(:call, Convert(WType(Int32), WType(Int64), :wrap), a)
  end
  a = Expr(:call, Call(Symbol("main/arrayref_", WType(eltype(exprtype(i, xs))))), xs, a)
end

wasmcalls[GlobalRef(Base, :arrayset)] = function (i, xs, val, idx)
  a = Expr(:call, GlobalRef(Base, :sub_int), idx, 1)
  if exprtype(i, idx) == Int64
    a = Expr(:call, Convert(WType(Int32), WType(Int64), :wrap), a)
  end
  Expr(:call, Call(Symbol("main/arrayset_", WType(eltype(exprtype(i, xs))))), xs, val, a)
end


wasmcalls[GlobalRef(Base, :sext_int)] = function (i, T, x)
  T isa GlobalRef && (T = getfield(T.mod, T.name))
  X = exprtype(i, x)
  Expr(:call, Convert(WType(T), WType(X), :extend_s), x)
end

wasmcalls[GlobalRef(Base, :zext_int)] = function (i, T, x)
  T isa GlobalRef && (T = getfield(T.mod, T.name))
  X = exprtype(i, x)
  Expr(:call, Convert(WType(T), WType(X), :extend_u), x)
end

wasmcall(i, f, xs...) =
  haskey(wasmcalls, f) ? wasmcalls[f](i, xs...) :
  Expr(:call, wasmfunc(f, exprtype.(i, xs)...), xs...)

isprimitive(x) = false
isprimitive(x::GlobalRef) =
  deref(x) isa Core.IntrinsicFunction ||
  deref(x) isa Core.Builtin

function lowercalls(m::ModuleState, c::CodeInfo, code)
  prewalk(code) do x
    if isexpr(x, :call) && deref(x.args[1]) == Base.throw
      unreachable
    elseif (isexpr(x, :call) && isprimitive(x.args[1]))
      wasmcall(c, x.args...)
    elseif isexpr(x, :invoke)
      lower_invoke(m, x.args)
    elseif isexpr(x, :foreigncall)
      lower_ccall(m, x.args)
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

function lower_invoke(m::ModuleState, args)
  # This lowers the function invoked. `args` is the Any[] from the :invoke Expr.
  # If the function has not been compiled, compile it.
  # Generate the WASM call.
  tt = argtypes(args[1])
  name = createfunname(args[1], tt)
  if !haskey(m.funcs, name)
    mi = args[1]
    ci = Base.uncompressed_ast(mi.def, mi.inferred)
    R = mi.rettype
    m.funcs[name] = code_wasm(m::ModuleState, name, tt, ci, R)
  end
  return Expr(:call, Call(name), args[3:end]...)
end

function lower_ccall(m::ModuleState, args)
  (fnname, env) = args[1]
  name = Symbol(env, :_, fnname)
  m.imports[name] = Import(env, fnname, :func, map(WType, args[3]), WType(args[2]))
  return Expr(:call, Call(name), args[4:2:end]...)
end

argtypes(x::Core.MethodInstance) = Tuple{x.specTypes.parameters[2:end]...}
argtypes(x::Method) = Tuple{x.sig.parameters[2:end]...}

fixName(s) = replace(replace(s, r"{|}|(|)|[|]" => ""), "," => ":")

createfunname(fun::Symbol, argtypes) = Symbol(fun, "_", fixName(join(collect(argtypes.parameters), "_")))
createfunname(fun::Function, argtypes) = createfunname(typeof(fun), argtypes)
createfunname(funtyp::DataType, argtypes) = Symbol(funtyp, "_", fixName(join(collect(argtypes.parameters), "_")))
createfunname(mi::Core.MethodInstance, argtypes) = createfunname(mi.def.name, argtypes)

basename(f::Function) = Base.function_name(f)
basename(f::Core.IntrinsicFunction) = Symbol(unsafe_string(ccall(:jl_intrinsic_name, Cstring, (Core.IntrinsicFunction,), f)))
basename(x::GlobalRef) = x.name
basename(m::Core.MethodInstance) = basename(m.def)
basename(m::Method) = m.name == :Type ? m.sig.parameters[1].parameters[1].name.name : m.name

iscontrol(ex) = isexpr(ex, :while) || isexpr(ex, :if)

lower(m::ModuleState, c::CodeInfo) = lowercalls(m, c, rmssa(c))

# Convert to WASM instructions

towasm_(m::ModuleState, xs, is = Instruction[]) = (foreach(x -> towasm(m, x, is), xs); is)

function towasm(m::ModuleState, x, is = Instruction[])
  if x isa Instruction
    push!(is, x)
  elseif isexpr(x, :block)
    push!(is, Block(towasm_(m, x.args)))
  elseif isexpr(x, :call) && x.args[1] isa Instruction
    foreach(x -> towasm(m, x, is), x.args[2:end])
    push!(is, x.args[1])
  elseif isexpr(x, :if)
    towasm(m, x.args[1], is)
    push!(is, If(towasm_(m, x.args[2].args), towasm_(m, x.args[3].args)))
  elseif isexpr(x, :while)
    push!(is, Block([Loop(towasm_(m, x.args[2].args))]))
  elseif deref(x) isa Number
    push!(is, Const(deref(x)))
  elseif x isa LineNumberNode || isexpr(x, :inbounds) || isexpr(x, :meta) || x isa Void
  else
    error("Can't convert to wasm: $x")
  end
  return is
end

funname(f::Function) = Base.function_name(f)
funname(s::Symbol) = s

function code_wasm(m::ModuleState, ex, A)
  cinfo, R = code_typed(ex, A)[1]
  code_wasm(m, createfunname(ex, A), A, cinfo, R)
end

function code_wasm(m::ModuleState, name::Symbol, A, cinfo::CodeInfo, R)
  body = towasm_(m, lower(m, cinfo).args) |> Block |> WebAssembly.restructure |> WebAssembly.optimise
  # @show A.parameters
  # @show R
  # @show cinfo.slottypes[length(A.parameters)+2:end]
  Func(name,
       [WType(T) for T in A.parameters],
       [WType(R)],
       [WType(P) for P in cinfo.slottypes[length(A.parameters)+2:end]],
       body)
end

macro code_wasm(ex)
  isexpr(ex, :call) || error("@code_wasm f(xs...)")
  :(code_wasm(ModuleState(), $(esc(ex.args[1])), Base.typesof($(esc.(ex.args[2:end])...))))
end

"""
    wasm_module(funpairlist)

Return a compiled WebAssembly ModuleState that includes every function defined by `funpairlist`.

`funpairlist` is a vector of pairs. Each pair includes the function to include and
a tuple type of the arguments to that function. For example, here is the invocation to
return a wasm ModuleState with the `mathfun` and `anotherfun` included.

    m = wasm_module([mathfun => Tuple{Float64},
                     anotherfun => Tuple{Int, Float64}])
"""
function wasm_module(funpairlist)
  m = ModuleState()
  for (fun, tt) in funpairlist
    internalname = createfunname(fun, tt)
    exportedname = funname(fun)
    m.funcs[internalname] = Func(:name, [], [], [], Block([]))
    m.funcs[internalname] = code_wasm(m, fun, tt)
    m.exports[exportedname] = Export(exportedname, internalname, :func)
  end
  if length(m.data) > 0
    m.exports[:memory] = Export(:memory, :memory, :memory)
  end
  return Module(FuncType[], collect(values(m.funcs)), Table[], [Mem(:m, 1, nothing)], Global[], Elem[],
                collect(values(m.data)), nothing, collect(values(m.imports)), collect(values(m.exports)))
end
