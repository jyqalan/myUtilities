plot.predictCPUE <-
function(variable, data, cpue.fit = NULL, nonzero.fit = NULL, xcts = F,
             fixed = list(), nmin = 0, plotit=F, se.fit = T, nobs = (nmin > 0),
             incl.fixed = F, log.scale = F, type = "l", ylab = NULL, xlab = NULL,axis.labels=TRUE,at=NULL,labels=NULL,plot.se=T,se.lty=1,
             plotvalues=T,xlim=NULL,ylim=NULL,scale=F,...)

  {
    RESULTS <- predictCPUE(variable, data, cpue.fit, nonzero.fit, xcts,
             fixed, nmin, plotit, se.fit, nobs,incl.fixed, log.scale, type, ylab,xlab,
             plotvalues,xlim,...)

    #tmp.x <- 1:length(RESULTS$fit)
    tmp.x <- if(type=="l"){as.numeric(names(RESULTS$fit))}else{1:length(RESULTS$fit)}    
    tmp.y <- RESULTS$fit
    tmp.se <- RESULTS$se.fit
    if(scale){
        if(plot.se) stop("can not plot std when fits are scaled!")
        tmp.y=tmp.y/mean(tmp.y)
    }
    if(is.null(xlim)==TRUE){
        xlim <- if(type == "l"){c(min(pretty(tmp.x)), max(pretty(tmp.x)))}else{c(1,length(tmp.x))}
    }

    if(is.null(ylim)==TRUE){
        ylim <- c(floor(min(tmp.y - 2*tmp.se, na.rm=TRUE)), ceiling(max(tmp.y + 2*tmp.se, na.rm=TRUE)))
    }
    
    #if(is.null(xlim)==TRUE)
    #  {xlim <- c(1,length(tmp.x))}

    #if(is.null(ylim)==TRUE)
    #  {ylim <- c(0,ceiling(max(tmp.y,na.rm=TRUE)))}

    plot(tmp.x,tmp.y,type=type,xlim=xlim,ylim=ylim,xlab=xlab,ylab=ylab,xaxt="n",...)
        
    if(plot.se==TRUE){
        if(type=="l")
          {
            points(tmp.x,tmp.y+(2*tmp.se),type="l",lty=se.lty)
            points(tmp.x,tmp.y-(2*tmp.se),type="l",lty=se.lty)
          }
        else
          {
            segments(tmp.x,tmp.y,tmp.x,tmp.y + (2*tmp.se))
            segments(tmp.x,tmp.y,tmp.x,tmp.y - (2*tmp.se))
          }
    }
    if(axis.labels==TRUE){
        if(is.null(labels)){
            labels = names(RESULTS$fit)
        }
        if(is.null(at)){
            at =tmp.x
        }
        axis(1,at=at,labels=labels)
    }
    
    return(RESULTS)
  }
