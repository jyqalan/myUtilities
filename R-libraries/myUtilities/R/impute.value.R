impute.value <-
function(VAR, fix.index, group.index, FUN = median,...)
{
    # VAR is the variable to correct
    # fix.index is a true/false vector of the values of VAR to correct
    # group.index is the grouping index vector

    # Alistair Dunn (undated) with modifications by MJM 2007-05-25

    VAR[fix.index] <- NA
    group.use <- group.index %in% unique(group.index[fix.index])
    temp <- tapply(VAR[group.use], group.index[group.use], FUN,...)#<-- Adding the group.use index greatly decreases the run time...
    temp.index <- names(temp)
    VAR[fix.index] <- temp[match(group.index[fix.index], temp.index)]
    return(VAR)
}
