using Charlotte
using Base.Test


@testset "Import" begin

@wasm_import sin(Float64)::Float64 in env

dump(code_typed(sin, Tuple{Float64}))

function mathfun(x)
    # x + sin(x)
    2x
end

m = wasm_module([mathfun => Tuple{Float64}])

## Better UI:
# m = @wasm begin
#     mathfun(Float64)
#     # Enter more functions here...
# end

# write("test.wat", m)


end
