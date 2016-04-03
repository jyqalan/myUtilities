get.top.n <-
function(x, y, FUN=sum, n=10, na.rm=TRUE, zap.zeros=TRUE,...)
{
    TAB <- tapply(y, x, FUN, na.rm=TRUE, ...)
    TAB <- sort(TAB, decreasing=TRUE)

    if(zap.zeros)
    {
        TAB <- TAB[TAB > 0]

        if(length(TAB) < n)
        {
            N <- length(TAB)
            index <- 1:N
            warning(paste("You have fewer non-zero names available than your specified n, ", N, " names only will (can) be returned", sep=""))
        }
        else
        {
            index <- 1:n
        }
    }
    else
    {
        index <- 1:n
    }

    out <- names(TAB)[index]

    return(out)
}
