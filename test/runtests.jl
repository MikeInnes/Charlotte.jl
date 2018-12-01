using Charlotte
using WebAssembly
using Test


# @testset "Import" begin

@noinline function mathfun(x)
    # x + sin(x)
    2x
end
@noinline function mathfun1(x)
    4x
end
function mathfun2(x, y)
    mathfun(3x) + mathfun1(y)
end


m = wasm_module([mathfun => Tuple{Float64},
                 mathfun2 => Tuple{Float64, Float64}])

function docos(x)
    ccall((:jscos, "imports"), Float64, (Float64,), x)
end
m1 = wasm_module([docos => Tuple{Float64}])

function docos2(x)
    ccall((:jscos, "imports"), Float64, (Float64,Float64), x, 33.3)
end
m2 = wasm_module([docos2 => Tuple{Float64}])


# BROKEN stuff to try to test memory
# const s = "hello"
# function stringtest()
#     # length(s)
#     s[1]
# end
# ms = wasm_module([stringtest => Tuple{}])
# const a = [1,2,3]
# function arraytest()
#     a[3]
# end
# ma = wasm_module([arraytest => Tuple{}])

# ms = wasm_module([stringtest => Tuple{}])

## Better UI:
# m = @wasm begin
#     mathfun(Float64)
#     mathfun2(Float64, Float64)
#     # Enter more functions here...
# end

# write("test.wat", m)


# end

# WA = WebAssembly
#
# relu(x) = ifelse(x < 0, 0, x)
# result = @code_wasm relu(1)
# expected = WA.Func(Symbol("#relu_Int64"), [WA.i64], [WA.i64], [], WA.Block([WA.Const(0), WA.Local(0), WA.Local(0), WA.Const(0), WA.Op(WA.i64, :lt_s), WA.Select(), WA.Return()]))
# @test result.params == expected.params
# @test result.returns == expected.returns
# @test result.locals == expected.locals
# @test result.body.body == expected.body.body

using WebAssembly: interpret_module, jltype
relu(x) = ifelse(x < 0, 0, x)

function pow(x, n)
  r = 1
  while n > 0
    r *= x
    n -= 1
  end
  return r
end

fib(x) = x <= 1 ? 1 : fib(x - 1) + fib(x - 2)
# TODO: These tests work, commented for speed
# m = wasm_module([relu => Tuple{Int64}, pow => Tuple{Int, Int}, fib => Tuple{Int}])
#
# funcs_wasm = interpret_module(m)
# es = Dict(e.internalname => e.name for e in m.exports)
# funcs = map(x -> es[x.name] |> eval, m.funcs)
#
# for (f, fwi, fw) in zip(funcs, funcs_wasm, m.funcs)
#   for i in 1:10
#     args = [rand(jltype(p)) % 10 for p in fw.params]
#     @test f(args...) == fwi(args...)[1]
#   end
# end

# using Charlotte: wasm_module
using WebAssembly: mergeWithBase, interpret_module_dict

function sum2arr(xss)
  tot = 0
  for xs in xss
    for x in xs
      tot += x
    end
  end
  return Int32(tot)
end

function sumarr(xs::Vector{Int32})
  tot = Int32(0)
  for x in xs
    tot += x
  end
  return tot
end

function arrayref_i32_(xs)
  return xs[10]
end

m = wasm_module([arrayref_i32_ => Tuple{Vector{Int32}}, sum2arr => Tuple{Array{Int32, 2}}, sumarr => Tuple{Array{Int32, 1}}]) |> mergeWithBase
write("this.wast", string(m))
write("this.wasm", WebAssembly.getModule(m))
# s = State(m)
# push!(s.fs, :func_0 => (2, (xs...) -> [show(("error: ", xs))]))
# efs = filter(e->e.typ==:func, m.exports)
# defs = Dict(e.name => (xs...) -> s.fs[e.internalname][2](xs...)[1] for e in efs)

defs = interpret_module_dict(m)
# @show defs
p = defs[:allocate](100)
defs[:arrayset_i32](p, 3, 9) == p
@test defs[:arrayref_i32](p, 9) == 3

# This one should work
# for i in 0:9
#   defs[:arrayset_i32](p, Int32(10), Int32(i))
# end
#
# @test defs[:sumarr](p) == 100


# defs = interpret_module_dict(m)
#
# p = defs[:allocate](1)
# # defs[:arrayset_i32](p, 3, 9) == p
# # @test defs[:arrayref_i32_](p) == 3
# @show p
# for i in 0:9
#   g = defs[:allocate](1)
#   @show g
#   # @show typeof(g)
#   for j in 0:5
#     defs[:arrayset_i32](g, Int32(j), Int32(j))
#     @show defs[:arrayref_i32](g, j)
#   end
#   defs[:arrayset_i32](p, g, i)
# end
#
# @show defs[:sum2arr](p)
