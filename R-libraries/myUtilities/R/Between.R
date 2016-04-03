Between <-
function(x0, x1, x2)
{
# Determines whether x0 is between x1 & x2, returning
# 1 if x0<xlo, 2 if x0=xlo, 3 if xlo<x0<xhi, 4 if xo=xhi and 5 if x>xhi
# where xlo=min(x1,x2), xhi=max(x1,x2)
# For the special case where x1==x2, never returns 3 or 4
# returning 2 if x0=x1=x2.
# 
if(x0 < x1) {
out <- if(x0 < x2) 1 else if(x0 == x2)
2
else 3
}
else if(x0 > x1) {
out <- if(x0 < x2) 3 else if(x0 == x2)
4
else 5
}
else {
out <- if(x0 < x2) 2 else if(x0 == x2)
2
else 4
}
out
}
