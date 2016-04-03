summarySEwithin <-
function(data=NULL, measurevar, betweenvars=NULL, withinvars=NULL,
                            idvar=NULL, na.rm=FALSE, conf.interval=.95, .drop=TRUE) {

    # Ensure that the betweenvars and withinvars are factors
    factorvars <- sapply(data[, c(betweenvars, withinvars), drop=FALSE], FUN=is.factor)
    if (!all(factorvars)) {
        nonfactorvars <- names(factorvars)[!factorvars]
        message("Automatically converting the following non-factors to factors: ",
                paste(nonfactorvars, collapse = ", "))
        data[nonfactorvars] <- lapply(data[nonfactorvars], factor)
    }

    # Norm each subject's data    
    data <- normDataWithin(data, idvar, measurevar, betweenvars, na.rm, .drop=.drop)

    # This is the name of the new column
    measureNormedVar <- paste(measurevar, "Normed", sep="")

    # Replace the original data column with the normed one
    data[,measurevar] <- data[,measureNormedVar]

    # Collapse the normed data - now we can treat between and within vars the same
    datac <- summarySE(data, measurevar, groupvars=c(betweenvars, withinvars), na.rm=na.rm,
                       conf.interval=conf.interval, .drop=.drop)

    # Apply correction from Morey (2008) to the standard error and confidence interval
    #  Get the product of the number of conditions of within-S variables
    nWithinGroups    <- prod(sapply(datac[,withinvars, drop=FALSE], FUN=nlevels))
    correctionFactor <- sqrt( nWithinGroups / (nWithinGroups-1) )

    # Apply the correction factor
    datac$sd <- datac$sd * correctionFactor
    datac$se <- datac$se * correctionFactor
    datac$ci <- datac$ci * correctionFactor

    return(datac)
}
