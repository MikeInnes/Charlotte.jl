using Charlotte
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
