Intersect <-
function(...)
{
  stuff <- lapply(list(...), unique)
  answer <- stuff[[1]]
  for(i in seq(along=stuff)[-1])
  answer <- answer[match(stuff[[i]], answer, nomatch = 0)]
  return(answer)
}
