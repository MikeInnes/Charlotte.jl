maxlabel(code) = maximum(l isa LabelNode ? l.label : -1 for l in code)

isgoto(::GotoNode) = true
isgoto(x) = isexpr(x, :gotoifnot)
isjump(x) =
  isgoto(x) || isexpr(x, :return) ||
  (isexpr(x, :call) && x.args[1] == Return())

function blocks(code)
  l = maxlabel(code)
  bs = Dict{Int,Vector{Any}}()
  label = 1
  current = bs[1] = []
  for x in code
    if x isa LabelNode
      label = x.label
      isjump(current[end]) || push!(current, GotoNode(label))
      bs[label] = current = []
    elseif isexpr(x, :gotoifnot)
      label = (l += 1)
      push!(current, x, GotoNode(label))
      bs[label] = current = []
    else
      push!(current, x)
    end
  end
  return bs
end

function branches(code, ignore = ())
  a = Int[]
  if code[end] isa GotoNode
    push!(a, code[end].label)
    length(code) > 1 &&
      isexpr(code[end-1], :gotoifnot) && push!(a, code[end-1].args[2])
  end
  return setdiff(a, ignore)
end

function splitcondition(code)
  code[end] isa GotoNode || return (code, nothing)
  (length(code) > 1 && isexpr(code[end-1], :gotoifnot)) || return (code[1:end-1], nothing)
  code[1:end-2], code[end-1].args[1]
end

function accessible(blocks, label, ignore = ())
  nu = branches(blocks[label], ignore)
  seen = Int[]
  while !isempty(nu)
    nu[1] in seen && (shift!(nu); continue)
    push!(seen, shift!(nu))
    append!(nu, branches(blocks[seen[end]], ignore))
  end
  return seen
end

accessible(blocks, label, labels...) = union(accessible(blocks, label), accessible(blocks, labels...))

function loop_next(bs, label)
  branches(bs[label])[2]
end

# NOTE: this will break if arbitrary gotos are used.
function restructure(bs::Associative, entries = 1, loops = [], cond = nothing)
  if isempty(entries)
    []
  elseif length(entries) == 1
    l = entries[1]
    if !isempty(loops)
      l == loops[end] && return [:(continue)]
      l in loops && return [:(break)]
      l == loop_next(bs, loops[end]) && return [:(break)]
    end
    code, cond = splitcondition(bs[l])
    if l ∉ accessible(bs, l, loops)
      [code..., restructure(bs, branches(bs[l]), loops, cond)...]
    else
      [Expr(:while, true, Expr(:block, code..., restructure(bs, branches(bs[l]), [loops..., l], cond)...)),
       restructure(bs, loop_next(bs, l), loops)...]
    end
  elseif length(entries) == 2 && cond != nothing
    as = accessible(bs, entries...)
    [Expr(:if, cond,
      Expr(:block, restructure(bs, entries[1], loops)...),
      Expr(:block, restructure(bs, entries[2], loops)...))]
  else
    error("Rebuild error")
  end
end

restructure(code) = Expr(:block, restructure(blocks(code))...)
