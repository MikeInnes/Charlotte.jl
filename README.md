# Charlotte

*This package was a not-particularly-functional prototype that hasn't been touched since 2018. I'm not sure how you got here, but if you want to run Julia on wasm, you should probably go [here](https://github.com/Keno/julia-wasm).*

```julia
Pkg.clone("https://github.com/MikeInnes/WebAssembly.jl")
Pkg.clone("https://github.com/MikeInnes/Charlotte.jl")
```

Charlotte is an experimental compiler for the [Julia](https://julialang.org/) language that targets web browsers, via JavaScript and [WebAssembly](https://github.com/MikeInnes/WebAssembly.jl).

Charlotte's WebAssembly backend currently supports simple numerical kernels, and could be productively used as a sort of scripting language over WASM.

```julia
julia> relu(x) = ifelse(x < 0, 0, x)
relu (generic function with 1 method)

julia> @code_wasm relu(1)
(func (param i64) (result i64)
  (i64.const 0)
  (get_local 0)
  (get_local 0)
  (i64.const 0)
  (i64.lt_s)
  (select)
  (return))
```

## Design

Julia code secretly comes in two dialects:

1. *Dynamic* Julia is used like JavaScript; it makes heavy use of GC, untyped data and dynamic dispatch. Tracing JIT compilers and runtimes, like V8, are extremely good at handling this kind of code.
2. *Static* Julia is more like C, with tight numerical loops that operate over arrays and other well-typed containers. This code can be aggressively optimised by ahead-of-time compilers, like LLVM.

In both cases, it's important to use the right tool for the job; tracing static code, or specialising dynamic code ahead of time, would lead to unnecessary work being done by the browser. Charlotte therefore uses a hybrid approach, compiling Julia code to a mix of JavaScript _and_ WebAssembly. This way we can get the best of both worlds, supporting both flexible DOM-manipulation code and tight numerical loops in one language, and getting good performance and code size everywhere.

In early prototypes the difference between the dialects will be fairly stark; the dynamic subset is essentially a syntax layer over JS, while the static subset only supports basic numerics. As more features are added both should converge to standard Julia, and the difference will become seamless for most users.

See [here](wasm.md) for a more detailed outline of the static compile pipeline.
