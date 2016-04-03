dectodeg <-
function(x)
{
return((trunc(x) * 100) + ((((x - trunc(x)) * 0.6) * 100)))
}
