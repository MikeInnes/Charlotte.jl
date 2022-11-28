using Charlotte
using WebAssembly
using Base.Test


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


m = @wasm mathfun(Float64), mathfun2(Float64, Float64)
function docos(x)
    ccall((:jscos, "imports"), Float64, (Float64,), x)
end

m1 = @wasm docos(Float64)

function docos2(x)
    ccall((:jscos, "imports"), Float64, (Float64,Float64), x, 33.3)
end
m2 = @wasm docos2(Float64)

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

# write("test.wat", m)


# end

WA = WebAssembly

relu(x) = ifelse(x < 0, 0, x)
result = @code_wasm relu(1)
expected = WA.Func(Symbol("#relu_Int64"), [WA.i64], [WA.i64], [], WA.Block([WA.Const(0), WA.Local(0), WA.Local(0), WA.Const(0), WA.Op(WA.i64, :lt_s), WA.Select(), WA.Return()]))
@test result.params == expected.params
@test result.returns == expected.returns
@test result.locals == expected.locals
@test result.body.body == expected.body.body
