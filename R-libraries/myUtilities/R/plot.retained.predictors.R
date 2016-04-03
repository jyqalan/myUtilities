plot.retained.predictors <-
function(object, labels=NULL, log.scale=TRUE, plotvalues=FALSE, se.fit=TRUE, sort.vars=TRUE, do.layout=TRUE)
{
    # Need a little bit of sleight of hand to get around the
    # (possible) presence of interaction terms in the retained
    # predictors. Do this by adding any interaction terms on to the
    # end of the DRAW list, but first remove any possible interaction
    # terms from all available retained predictors.

    # This is going to need another rewrite to deal with (arbitrary)
    # interaction terms...

    DATA <- object$data

    ATTR <- attributes(object$terms)$term.labels

    if(length(grep(":", ATTR)) > 0)
    {ATTRnoI <- ATTR[-grep(":", ATTR)]}
    else
    {ATTRnoI <- ATTR}

    DRAW <- names(unlist(sapply(names(DATA), grep, ATTRnoI)))

    #DRAW <- names(unlist(sapply(names(DATA), grep, attributes(object$terms)$term.labels)))

    if(sort.vars)
    {DRAW <- sort(DRAW)}
    
    N <- length(DRAW)
    newN <- if(((N/2) - floor(N/2)) > 0){N + 1}else{N}
    matN <- matrix(1:newN, ncol=2, byrow=TRUE)

    if(do.layout)
    {
        par(mai=par("mai")/2)
        par(oma=c(3, 3, 1, 1))
        layout(matN)
    }

    if(is.null(labels))
    {
        labels <- DRAW
    }

    if(!is.null(labels))
    {
        if(length(labels) != length(DRAW))
        {stop("The length of your labels vector does not equal the number of retained predictors in your model")}
    }

    print(data.frame("Retained.variables"=DRAW, "Axis.labels"=labels))
    cat("\n")
    
    for(i in 1:length(DRAW))
    {
        # I need to rationalise what's going on here so I can follow it next time!
        
        PRETTY <- if(is.factor(DATA[[DRAW[i]]]))
        {
            levels(DATA[[DRAW[i]]]) 
        }
        else
        {
            pretty(DATA[[DRAW[i]]])
        }        
        
        plot.type <- if(is.factor(DATA[[DRAW[i]]])){"p"}else{"l"}
        axis.at <- if(is.factor(DATA[[DRAW[i]]]))
        {
            if(DRAW[i] == "vessel_key")
            {
                if(length(levels(DATA[[DRAW[i]]])) <=10) 1:length(PRETTY) else seq(1, length(levels(DATA[[DRAW[i]]])), 5)
                #pretty(c(1, length(levels(DATA[[DRAW[i]]]))))
                #seq(1, length(levels(DATA[[DRAW[i]]])), 5)
            }
            else
            {1:length(PRETTY)}
        }
        else{PRETTY}
        axis.las <- if(is.factor(DATA[[DRAW[i]]])){3}else{1}
        axis.labels <- if(is.factor(DATA[[DRAW[i]]])){PRETTY[axis.at]}else{PRETTY}
        #this.label <- paste("(", letters[i], ") ", labels[i], sep="")
        this.label <- labels[i]

        ans <- plot.predictCPUE(DRAW[i], DATA, object, log.scale=log.scale, plotvalues=plotvales, se.fit=se.fit, type=plot.type, xlab="", ylab="", axis.labels=FALSE)
        axis(side=1, at=axis.at, labels=axis.labels, las=axis.las)
        #just for
        tmp.x <- if(plot.type=="l"){as.numeric(names(ans$fit))}else{1:length(ans$fit)}
        text(tmp.x[1], ceiling(max(ans$fit + 2*ans$se.fit, na.rm=TRUE)), this.label, font=2, adj=c(0, 1))

    }

    mtext(side=2, "Expected catch rate", outer=TRUE, line=1)
    mtext(side=1, "Levels or values of retained predictor variables", outer=TRUE, line=1)

}
