using Charlotte
using Base.Test


# @testset "Import" begin

@wasm_import sin(Float64)::Float64 in env

function mathfun(x)
    # x + sin(x)
    2x
end
function mathfun2(x, y)
    2x + y
end


m = wasm_module([mathfun => Tuple{Float64},
                 mathfun2 => Tuple{Float64, Float64}])
f = first(m.funcs)
## Better UI:
# m = @wasm begin
#     mathfun(Float64)
#     mathfun2(Float64, Float64)
#     # Enter more functions here...
# end

# write("test.wat", m)


# end
