function code_js(ex, A)
  cinfo = code_lowered(ex, A)[1]
  restructure(cinfo.code)
end

macro code_js(ex)
  isexpr(ex, :call) || error("@code_wasm f(xs...)")
  :(code_js($(esc(ex.args[1])), Tuple{$(map(_ -> Any, ex.args[2:end])...)}))
end
