using Base.Meta
import Base: CodeInfo, SSAValue, SlotNumber, GotoNode, NewvarNode, LabelNode
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

exprtype(code::IRCode, x) = typeof(deref(x))
exprtype(code::IRCode, x::Expr) = x.typ
exprtype(code::IRCode, x::QuoteNode) = typeof(x.value)
exprtype(code::IRCode, x::SSAValue) = code.types[x.id]
exprtype(code::IRCode, x::Argument) = code.argtypes[x.n]
# exprtype(code::CodeInfo, x::SlotNumber) = code.slottypes[x.id]

apply_rhs(f, x) =
    isexpr(x, :(=)) ?
      Expr(:(=), x.args[1], f(x.args[2])) :
      f(x)

function symbolmap(f, stmt)
  urs = Core.Compiler.userefs(stmt)
  for op in urs
    val = op[]
    op[] = f(val)
  end
  return urs[]
end

function ssawalk(f, stmts)
  for i in eachindex(stmts)
    stmts[i] = symbolmap(f, stmts[i])
  end
  return stmts
end

function rmCompilerArgs(stmts)
  isArg(x) = x isa Core.Compiler.Argument
  for i in eachindex(stmts)
    stmts[i] = symbolmap(stmts[i]) do x
      # return (isArg(x) ? Symbol("_$(x.n - 2)") : x)
      return (isArg(x) ? SlotNumber(x.n - 2) : x)
      # @show x
    end
  end
end


function addVar(i, stmt, used)
  # for i in eachindex(code.stmts)



  if i in used
    # return Expr(:(=), Symbol("_$(i + num_args)"), stmt)
    # return Expr(:(=), SlotNumber(i + num_args), stmt)
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


# symbolmap(p_ir.stmts[2])

# @show p_ir.stmts

# Hold on to this because it shows how to construct a quote
# function toquote(code)
#   statements = []
#   for i in eachindex(code.stmts)
#     s = code.stmts[i]
#     push!(statements, toexpr(i, code))
#   end
#   return Expr(:block, statements...)
# end
#
# pq = toquote(p_ir)
# @show rmssa(pq)

# Now that we've removed the SSAValues (TODO: Remove Compiler Args), remove
# restructure into array of arrays (the blocks), append assignements to the
# end of each block.

# Ideally there would be some kind of system to plug and play different
# algorithms for register assignment, but for the time being this ought to work.

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

      if isPhi(stmt)
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
            push!(blocked[phi.edges[i]], Expr(:(=), stmt.args[1], phi.values[i]))
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
    push!(stmts, Base.LabelNode(i), blocks[i]...)
  end
  return stmts
end

function rmssa(c)
  @show c
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
  # if A is
  # if A isa pair
  #   Op(WType(A[2]))

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

function lowercalls(m::ModuleState, c::IRCode, code)
  num_args = length(c.argtypes)
  @show c.argtypes
  prewalk(code) do x
    # @show x, typeof(x)
    if isexpr(x, :call) && deref(x.args[1]) == Base.throw
      unreachable
    elseif (isexpr(x, :call) && isprimitive(x.args[1]))
      wasmcall(c, x.args...)
    elseif isexpr(x, :invoke)
      lower_invoke(m, x.args)
    elseif isexpr(x, :foreigncall)
      lower_ccall(m, x.args)
    elseif isexpr(x, :(=)) && x.args[1] isa SSAValue
      Expr(:call, SetLocal(false, x.args[1].id + num_args), x.args[2])
    elseif x isa SSAValue
      Local(x.id+num_args)
    elseif x isa Argument
      @show x
      @show Local(x.n-2)
    elseif x isa Compiler.GotoIfNot
      # typ = x.cond isa Argument ? c.argtypes[x.cond.n] : x.cond isa SSAValue ? c.types[x.cond.id] : typeof(x.cond)
      # Expr(:call, Goto(true, x.dest),
      #      Expr(:call, GlobalRef(Base, :not_int), (x.cond, typ)))
      Expr(:call, Goto(true, x.dest),
           Expr(:call, GlobalRef(Base, :not_int), x.cond))
    elseif x isa Core.GotoNode
      # @show "this never happens"
      Goto(false, x.label)
    elseif x isa Base.LabelNode
      Label(x.label)
    elseif x isa Core.Compiler.ReturnNode
      Expr(:call, Return(), x.val)
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

createfunname(fun::Symbol, argtypes) = Symbol(fun, "_", join(collect(argtypes.parameters), "_"))
createfunname(fun::Function, argtypes) = createfunname(typeof(fun), argtypes)
createfunname(funtyp::DataType, argtypes) = Symbol(funtyp, "_", join(collect(argtypes.parameters), "_"))
createfunname(mi::Core.MethodInstance, argtypes) = createfunname(mi.def.sig.parameters[1], argtypes)

basename(f::Function) = Base.function_name(f)
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
  elseif x isa LineNumberNode || isexpr(x, :inbounds) || isexpr(x, :meta)
  elseif x isa Nothing
    push!(is, Nop())
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
  body = towasm_(m, @show lower(m, Core.Compiler.inflate_ir(cinfo)).args) |> Block |> WebAssembly.restructure |> WebAssembly.optimise
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
    m.funcs[internalname] = code_wasm(m, fun, tt)
    m.exports[exportedname] = Export(exportedname, internalname, :func)
  end
  if length(m.data) > 0
    m.exports[:memory] = Export(:memory, :memory, :memory)
  end
  return Module(FuncType[], collect(values(m.funcs)), Table[], [Mem(:m, 1, nothing)], Global[], Elem[],
                collect(values(m.data)), Ref(0), collect(values(m.imports)), collect(values(m.exports)))
end
