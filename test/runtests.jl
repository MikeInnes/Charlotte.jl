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

# These fail now.
# function docos(x)
#     ccall((:jscos, "imports"), Float64, (Float64,), x)
# end
# m1 = wasm_module([docos => Tuple{Float64}])
#
# function docos2(x)
#     ccall((:jscos, "imports"), Float64, (Float64,Float64), x, 33.3)
# end
# m2 = wasm_module([docos2 => Tuple{Float64}])

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

m = wasm_module([relu => Tuple{Int64}, pow => Tuple{Int, Int}, fib => Tuple{Int}])

funcs_wasm = interpret_module(m)
es = Dict(e.internalname => e.name for e in m.exports)
funcs = map(x -> es[x.name] |> eval, m.funcs)

for (f, fwi, fw) in zip(funcs, funcs_wasm, m.funcs)
  for i in 1:10
    args = [rand(jltype(p)) % 10 for p in fw.params]
    @test f(args...) == fwi(args...)[1]
  end
end
