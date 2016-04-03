degtodec <-
function(x)
{
return(round((((x/100) - trunc(x/100))/0.6) + trunc(x/100), 3))
}
