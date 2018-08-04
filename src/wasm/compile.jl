using Base.Meta
import Base: CodeInfo, SSAValue, SlotNumber, GotoNode, NewvarNode
using WebAssembly, WebAssembly.Instructions
using WebAssembly: WType, Func, Module, FuncType, Func, Table, Mem, Global, Elem, Data, Import, Export, Block

using Core.Compiler: Argument
using Core.Compiler
import Core: SSAValue
import Core.Compiler: IRCode, IncrementalCompact, UseRef, UseRefIterator, StmtRange

for T in :[IRCode, IncrementalCompact, UseRef, UseRefIterator].args
  @eval begin
    Base.getindex(ir::$T, a...) = Compiler.getindex(ir, a...)
    Base.setindex!(ir::$T, a...) = Compiler.setindex!(ir, a...)
  end
end

Base.getindex(r::StmtRange, i) = (r.first:r.last)[i]

for T in :[UseRefIterator, IncrementalCompact, Pair].args
  @eval Base.iterate(x::Compiler.$T, a...) = Compiler.iterate(x, a...)
end

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
  # y.typ = x.typ
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
# exprtype(code::CodeInfo, x::TypedSlot) = x.typ

exprtype(code::IRCode, x) = typeof(deref(x))
exprtype(code::IRCode, x::Expr) = error("Expr.typ no longer exists")#x.typ
exprtype(code::IRCode, x::QuoteNode) = typeof(x.value)
exprtype(code::IRCode, x::SSAValue) = code.types[x.id]
exprtype(code::IRCode, x::Argument) = code.argtypes[x.n]
# exprtype(code::CodeInfo, x::SlotNumber) = code.slottypes[x.id]

function rmCompilerArgs(stmts)
  isArg(x) = x isa Core.Compiler.Argument
  for i in eachindex(stmts)
    stmts[i] = symbolmap(stmts[i]) do x
      return (isArg(x) ? SlotNumber(x.n - 2) : x)
    end
  end
end

function addVar(i, stmt, used)
  if i in used
    return Expr(:(=), SSAValue(i), stmt)
  else
    return stmt
  end
end

function addVars(stmts)
  used = Base.IdSet{Int}()
  foreach(stmt->Core.Compiler.scan_ssa_use!(push!, used, stmt), stmts)
  for i in eachindex(stmts)
    stmts[i] = addVar(i, stmts[i], used)
  end
end

# Take ir and restructure statements as array of arrays representing blocks.
function getBlocks(ir)
  blocked = []
  for block in ir.cfg.blocks
    push!(blocked, ir.stmts[block.stmts])
  end
  return blocked
end

function isPhi(stmt)
  return isexpr(stmt, :(=)) && stmt.args[2] isa Compiler.PhiNode
end

function rmPhiNodes(ir)
  blocked = getBlocks(ir)
  for block in ir.cfg.blocks
    for stmt in ir.stmts[block.stmts]
      # Find the phinodes, restructure
      if isexpr(stmt, :(=)) && stmt.args[2] isa Compiler.PiNode
        stmt.args[2] = stmt.args[2].val
      elseif isPhi(stmt)
        phi = stmt.args[2]
        for i in eachindex(phi.edges)
          s = blocked[phi.edges[i]]
          inserted = false
          for j in eachindex(s)
            if isexpr(s[j], :(=)) && s[j].args[1] == phi.values[i]
              insert!(s, j + 1, Expr(:(=), stmt.args[1], phi.values[i]))
              inserted = true
              break
            end
          end
          if !inserted
            pushfirst!(blocked[phi.edges[i]], Expr(:(=), stmt.args[1], phi.values[i]))
          end
        end
      end
    end
  end
  # return filter(stmt -> !(isPhi(stmt)), vcat(blocked...))
  return map(line -> filter(isPhi |> !, line), blocked)
end

# Somewhat archaic, but adding labels allows reuse of old code for the time
# being
function addLabels(blocks)
  stmts = []
  for i in eachindex(blocks)
    push!(stmts, Label(i), blocks[i]...)
  end
  return stmts
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
  (:eq_int,  :eq),
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

for (j, w) in vcat(int_unary_ops, float_unary_ops)
  wasmfuncs[GlobalRef(Core, j)] = function (T)
    Op(WType(T), w)
  end
end

for (j, w) in vcat(int_binary_ops, float_binary_ops)
  wasmfuncs[GlobalRef(Core, j)] = function (A,B)
    Op(WType(A), w)
    # Op(WType(Int32), w)
  end
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
  sign = Expr(:call, GlobalRef(Base, :ifelse), cond, T(-1), T(1))
  # sign.typ = T
  Expr(:call, nop,
    Expr(:call, GlobalRef(Base, :mul_int), sign, x))
end

# TODO: should probably not do this
wasmcalls[GlobalRef(Base, :check_top_bit)] = function (i, x)
  nop
end

# More complex intrinsics

wasmcalls[GlobalRef(Base, :ifelse)] = function (i, c, a, b)
  return Expr(:call, Select(), a, b, c)
end

wasmcalls[GlobalRef(Main, :ifelse)] = function (i, c, a, b)
  return Expr(:call, Select(), a, b, c)
end

wasmcalls[GlobalRef(Base, :bitcast)] = function (i, T, x)
  T isa GlobalRef && (T = getfield(T.mod, T.name))
  X = exprtype(i, x)
  WType(T) == WType(X) && return Expr(:call, nop, x)
  @assert sizeof(T) == sizeof(X)
  Expr(:call, Convert(WType(T), WType(X), :reinterpret), x)
end

# # Just try having it exactly the same who knows.
wasmcalls[GlobalRef(Core, :trunc_int)] = function (i, T, x)
  T isa GlobalRef && (T = getfield(T.mod, T.name))
  X = exprtype(i, x)
  WType(T) == WType(Int32) && WType(X) == WType(Int64) && return Expr(:call, Convert(WType(T), WType(X), :wrap), x)
  WType(T) == WType(X) && return Expr(:call, nop, x)
  T isa Signed ? Expr(:call, Convert(WType(T), WType(X), :trunc_s), x) :
  Expr(:call, Convert(WType(T), WType(X), :trunc_u), x)
end

wasmcalls[GlobalRef(Base, :fptosi)] = function (i, T, x)
  T isa GlobalRef && (T = getfield(T.mod, T.name))
  X = exprtype(i, x)
  Expr(:call, Convert(WType(T), WType(X), :trunc_s), x)
end

wasmcalls[GlobalRef(Base, :checked_trunc_sint)] = function (i, T, x)
  T isa GlobalRef && (T = getfield(T.mod, T.name))
  X = exprtype(i, x)
  (T == Int32 && X == Int64) && return Expr(:call, Convert(WType(T), WType(X), :wrap), x)
  Expr(:call, Convert(WType(T), WType(X), :trunc_s), x)
end

wasmcalls[GlobalRef(Base, :sitofp)] = function (i, T, x)
  T isa GlobalRef && (T = getfield(T.mod, T.name))
  X = exprtype(i, x)
  Expr(:call, Convert(WType(T), WType(X), :convert_s), x)
end

wasmcalls[GlobalRef(Base, :arraylen)] = function (i, xs)
  p = Expr(:call, GlobalRef(Base, :add_int), xs, (exprtype(i, xs) |> WType |> WebAssembly.jltype)(8))
  a = Expr(:call, Call(Symbol("main/arraylen_", WType(eltype(exprtype(i, xs))))), p)
  Expr(:call, Convert(WType(Int64), WType(Int32), :extend_u), a)
end

wasmcalls[GlobalRef(Base, :arraysize)] = function (i, xs, dim)
  a = Expr(:call, GlobalRef(Base, :sub_int), dim, 1)
  if exprtype(i, dim) == Int64 # Will need to change this for wasm64
    a = Expr(:call, Convert(WType(Int32), WType(Int64), :wrap), a)
  end

  # load the index in the shape array. The xs pointer should curently be pointing
  # to it.
  a = Expr(:call, Call(Symbol("main/arrayref_", WType(eltype(exprtype(i, xs))))), xs, a)
  Expr(:call, Convert(WType(Int64), WType(Int32), :extend_u), a)
end

# TODO: Ignoring the bool because I don't know what it does.
wasmcalls[GlobalRef(Base, :arrayref)] = function (i, bool, xs, idxs...)
  idxs = Any[t for t in idxs]
  map!(idxs, idxs) do idx
    t = exprtype(i, idx)
    if t == Int64
      Int32, Expr(:call, Convert(WType(Int32), WType(Int64), :wrap), idx)
    else
      t, idx
    end
  end
  p = Expr(:call, GlobalRef(Base, :add_int), xs, Int32(8))
  idx = Expr(:invoke_new, convertIndex, collect(zip((pushfirst!(idxs, (exprtype(i, xs), xs)))...))...)
  a = Expr(:call, Call(Symbol("main/arrayref_", WType(eltype(exprtype(i, xs))))), p, idx)
end

function convertIndex(array, x, y)
  return Int32((x - one(x)) * stride(array, 1) + (y - one(y)) * stride(array, 2))
end

function convertIndex(array, x)
  return Int32(x - one(x))
end

wasmcalls[GlobalRef(Base, :arrayset)] = function (i, bool, xs, val, idxs...)
  idxs = Any[t for t in idxs]
  map!(idxs, idxs) do idx
    t = exprtype(i, idx)
    if t == Int64
      Int32, Expr(:call, Convert(WType(Int32), WType(Int64), :wrap), idx)
    else
      t, idx
    end
  end
  p = Expr(:call, GlobalRef(Base, :add_int), xs, Int32(8))
  idx = Expr(:invoke_new, convertIndex, collect(zip((pushfirst!(idxs, (exprtype(i, xs), xs)))...))...)
  a = Expr(:call, Call(Symbol("main/arrayset_", WType(eltype(exprtype(i, xs))))), p, val, idx)
end

wasmcalls[GlobalRef(Core, :sext_int)] = function (i, T, x)
  T isa GlobalRef && (T = getfield(T.mod, T.name))
  X = exprtype(i, x)
  Expr(:call, Convert(WType(T), WType(X), :extend_s), x)
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

wasmcalls[GlobalRef(Base, :not_int)] = function (i, x)
  T = exprtype(i, x)
  T == Bool ?
  Expr(:call, Op(WType(T), :eqz), x) :
  Expr(:call, Op(WType(T), :xor), x, T(-1))
end

wasmcall(i, f, xs...) =
  haskey(wasmcalls, f) ? wasmcalls[f](i, xs...) :
  Expr(:call, wasmfunc(f, exprtype.(i, xs)...), xs...)

isprimitive(x) = false
isprimitive(x::GlobalRef) =
  deref(x) isa Core.IntrinsicFunction ||
  deref(x) isa Core.Builtin

function lowercalls(m::ModuleState, c::IRCode, code)
  num_args = length(c.argtypes)
  prewalk(code) do x
    if (isexpr(x, :invoke) && deref(x.args[2]) == Core.throw_inexacterror) || (isexpr(x, :call) && deref(x.args[1]) == Base.throw)
      unreachable
    elseif (isexpr(x, :call) && isprimitive(x.args[1]))
      wasmcall(c, x.args...)
    elseif isexpr(x, :invoke)
      lower_invoke(m, x.args)
    elseif isexpr(x, :foreigncall)
      lower_ccall(m, x.args)
    elseif isexpr(x, :(=)) && x.args[1] isa SSAValue
      Expr(:call, SetLocal(false, x.args[1].id + num_args -2), x.args[2])
    elseif x isa SSAValue
      Local(x.id+num_args-2)
    elseif x isa Argument
      Local(x.n-2)
    elseif x isa Compiler.GotoIfNot
      Expr(:call, Goto(true, x.dest),
           Expr(:call, GlobalRef(Base, :not_int), x.cond))
    elseif x isa Core.GotoNode
      Goto(false, x.label)
    elseif x isa Core.Compiler.ReturnNode
      isdefined(x, :val) ? Expr(:call, Return(), x.val) : unreachable
    elseif x isa Nothing
      Expr(:call, Nop())
    elseif isexpr(x, :invoke_new)
      lower_new(m, x.args)
    else
      x
    end
  end
end

function lower_invoke(m::ModuleState, args)
  # This lowers the function invoked. `args` is the Any[] from the :invoke Expr.
  # If the function has not been compiled, compile it.
  # Generate the WASM call.
  println("just invoke the function")
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

function lower_new(m::ModuleState, args)
  tt = Tuple{args[2]...}
  name = createfunname(args[1], tt)
  if !haskey(m.funcs, name)
    cinfo, R = code_typed(args[1], tt)[1]
    m.funcs[name] = code_wasm(m::ModuleState, name, tt, cinfo, R)
  end
  return Expr(:call, Call(name), args[3]...)
end


# This doesn't work anymore
function lower_ccall(m::ModuleState, args)
  (fnname, env) = args[1]
  name = Symbol(env, :_, fnname)
  m.imports[name] = Import(env, fnname, :func, map(WType, args[3]), WType(args[2]))
  return Expr(:call, Call(name), args[4:2:end]...)
end

argtypes(x::Core.MethodInstance) = Tuple{x.specTypes.parameters[2:end]...}
argtypes(x::Method) = Tuple{x.sig.parameters[2:end]...}

createfunname(fun::Symbol, argtypes::UnionAll) = Symbol(fun, "_", fixName(join(argtypes, "_")))
createfunname(fun::Symbol, argtypes) = Symbol(fun, "_", fixName(join(collect(argtypes.parameters), "_")))
createfunname(fun::Function, argtypes) = createfunname(funname(fun), argtypes)
# createfunname(funtyp::DataType, argtypes) = Symbol(funtyp, "_", join(collect(argtypes.parameters), "_"))
createfunname(mi::Core.MethodInstance, argtypes) = createfunname(mi.def.name, argtypes)

fixName(s) = replace(replace(replace(s, r"\{|\(|\[" => "<") , r"\}|\)|\]" => ">"), "," => ":")

basename(f::Function) = Base.nameof(f)
basename(f::Core.IntrinsicFunction) = Symbol(unsafe_string(ccall(:jl_intrinsic_name, Cstring, (Core.IntrinsicFunction,), f)))
basename(x::GlobalRef) = x.name
basename(m::Core.MethodInstance) = basename(m.def)
basename(m::Method) = m.name == :Type ? m.sig.parameters[1].parameters[1].name.name : m.name

iscontrol(ex) = isexpr(ex, :while) || isexpr(ex, :if)

function dePhi(c)
  addVars(c.stmts)
  Expr(:block, addLabels(rmPhiNodes(c))...)
end

lower(m::ModuleState, c::IRCode) = lowercalls(m, c, dePhi(c))

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
  elseif x isa LineNumberNode || isexpr(x, :inbounds) || isexpr(x, :meta) || x == nothing
  else
    error("Can't convert to wasm: $x :: $(typeof(x))")
  end
  return is
end

funname(f::Function) = Base.nameof(f)
funname(s::Symbol) = s

function code_wasm(m::ModuleState, ex, A)
  cinfo, R = code_typed(ex, A)[1]
  code_wasm(m, createfunname(ex, A), A, cinfo, R)
end

function code_wasm(m::ModuleState, name::Symbol, A, cinfo::CodeInfo, R)
  ircode = Core.Compiler.inflate_ir(cinfo)
  ircode.argtypes[2:end] = [t for t in A.parameters]

  # This is being calculated twice but once the register allocation is changed
  # it won't work anymore anyway.
  used = Base.IdSet{Int}()
  foreach(stmt->Core.Compiler.scan_ssa_use!(push!, used, stmt), ircode.stmts)

  body = towasm_(m, lower(m, ircode).args) |> Block |> WebAssembly.restructure |> WebAssembly.optimise
  Func(name,
       [WType(T) for T in A.parameters],
       [WType(R)],
       [WType(i ∈ used ? ircode.types[i] : Int32) for i in eachindex(ircode.types)],
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
