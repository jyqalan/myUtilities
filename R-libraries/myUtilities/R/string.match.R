string.match <-
function(pattern, text) {
  # this doesnt crash?
  ans <- regexpr(pattern, text)
  ans<-(1:length(ans))[ans!= -1]
  return(ans)
}
