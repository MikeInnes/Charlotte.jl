# WASM Compile Pipeline

```julia
julia> function sumn(n)
         x = 0
         for i = 1:n
           x += i
         end
         return x
       end
sumn (generic function with 1 method)
```

Compilation starts with Julia's `@code_typed`, an intermediate representation in which control flow is removed, functions are inlined and expressions have type information attached.

```julia
julia> c = @code_typed sumn(10)
CodeInfo(:(begin
        x = 0 # line 3:
        SSAValue(2) = (Base.select_value)((Base.sle_int)(1, n)::Bool, n, (Base.sub_int)(1, 1)::Int64)::Int64
        #temp# = 1
        5:
        unless (Base.not_int)((#temp# === (Base.add_int)(SSAValue(2), 1)::Int64)::Bool)::Bool goto 15
        SSAValue(3) = #temp#
        SSAValue(4) = (Base.add_int)(#temp#, 1)::Int64
        i = SSAValue(3)
        #temp# = SSAValue(4) # line 4:
        x = (Base.add_int)(x, i)::Int64
        13:
        goto 5
        15:  # line 6:
        return x
    end))=>Int64
```

We remove SSA form variables so that only locals, function calls and gotos are present.

```julia
julia> Charlotte.rmssa(c[1])
quote
    _5 = 0 # line 3:
    _4 = 1
    5:
    unless (Base.not_int)((_4 === (Base.add_int)((Base.select_value)((Base.sle_int)(1, _2)::Bool, _2, (Base.sub_int)(1, 1)::Int64)::Int64, 1)::Int64)::Bool)::Bool goto 15
    _3 = _4
    _4 = (Base.add_int)(_4, 1)::Int64 # line 4:
    _5 = (Base.add_int)(_5, _3)::Int64
    13:
    goto 5
    15:  # line 6:
    return _5
end
```

We translate Julia primitives (e.g. floating point operations) into WebAssembly primitives via a lookup table.

```julia
julia> ex = Charlotte.lowercalls(c[1], ex)
quote
    (set_local 3)(0) # line 3:
    (set_local 2)(1)
    label $5
    (gotounless $15)((i32.eqz)((i32.eqz)((i64.eq)(get_local 2, (i64.add)((select)(get_local 0,(i64.sub)(1, 1), (i64.le_s)(1, get_local 0)), 1)))))
    (set_local 1)(get_local 2)
    (set_local 2)((i64.add)(get_local 2, 1)) # line 4:
    (set_local 3)((i64.add)(get_local 3, get_local 1))
    label $13
    goto $5
    label $15 # line 6:
    (return)(get_local 3)
end
```

Then we flatten all function calls into WebAssembly's stack machine format.

```julia
julia> Charlotte.towasm(ex)
1-element Array{WebAssembly.Instruction,1}:
 block
  (i64.const 0)
  (set_local 3)
  (i64.const 1)
  (set_local 2)
  (label $5)
  (get_local 2)
  (get_local 0)
  (i64.const 1)
  (i64.const 1)
  (i64.sub)
  (i64.const 1)
  (get_local 0)
  (i64.le_s)
  (select)
  (i64.const 1)
  (i64.add)
  (i64.eq)
  (i32.eqz)
  (i32.eqz)
  (gotounless $15)
  (get_local 2)
  (set_local 1)
  (get_local 2)
  (i64.const 1)
  (i64.add)
  (set_local 2)
  (get_local 3)
  (get_local 1)
  (i64.add)
  (set_local 3)
  (label $13)
  (goto $5)
  (label $15)
  (get_local 3)
  (return)
```

You might notice that this contains `goto`s, which are not allowed by WebAssembly directly. WebAssembly.jl provides a `restructure` utility to recover structured control flow from gotos, as well as optimising the resulting output. Putting it all together we get `@code_wasm`:

```julia
julia> @code_wasm sumn(1)
(func (param i64) (result i64)
  (local i64) (local i64) (local i64)
  (i64.const 0)
  (set_local 3)
  (i64.const 1)
  (set_local 2)
  (block
    (loop
      (get_local 2)
      (get_local 0)
      (i64.const 1)
      (i64.const 1)
      (i64.sub)
      (i64.const 1)
      (get_local 0)
      (i64.le_s)
      (select)
      (i64.const 1)
      (i64.add)
      (i64.eq)
      (i32.eqz)
      (i32.eqz)
      (br_if 1)
      (get_local 2)
      (set_local 1)
      (get_local 2)
      (i64.const 1)
      (i64.add)
      (set_local 2)
      (get_local 3)
      (get_local 1)
      (i64.add)
      (set_local 3)
      (br 0)))
  (get_local 3)
  (return))
```
