module wasm

export addTwo, pow, callFib, subTwo

@noinline function addTwo(x::Int, y::Int)
    return x + y
end

@noinline function addTwo(x::Float64, y::Float64)
    return x + y
end

@noinline function pow(x::Int, n::Int)
  r = 1
  while n > 0
    r *= x
    n -= 1
  end
  return r
end

@noinline function callFib(x::Int)
  return fib(x)
end

@noinline fib(x) = x <= 1 ? 1 : fib(x - 1) + fib(x - 2)

function subTwo(x, y)
  return x - y
end

end
