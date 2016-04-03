find.most.common <-
function(xin, break.ties.at.random=FALSE)
{
    # Finds and returns the maximum value in a vector of data

    # Ties can be broken RANDOMLY using "which.is.max" from "nnet"

    # You were warned...

    # MJM Sometime during late 2003

    # How about preserving the attributes, working on a coerced
    # character version of the object, and resetting the attributes
    # later?

    if(break.ties.at.random) {require(nnet)}

    tmp.xin <- as.character(xin)

    tmp.table <- table(tmp.xin)

    if(length(tmp.table)==0)
    {
        return("NA")#<-- NA trap...
    }
    else
    {
        tmp.max <- if(break.ties.at.random) {which.is.max(tmp.table)} else {which.max(tmp.table)}
        tmp.labels <- labels(tmp.table[tmp.max])
        return(tmp.labels)
    }
}
