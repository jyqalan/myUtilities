Var <-
function(x, y = x) {
  new.x <- x[!is.na(x)]
  new.y <- y[!is.na(y)]
  return(var(new.x, new.y))
}
