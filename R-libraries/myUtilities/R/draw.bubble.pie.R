draw.bubble.pie <-
function(x,y,z,kat,xlevels=NULL,ylevels=NULL,katlevels=NULL, pal=terrain.colors(length(levels(as.factor(kat))))
                            ,xat,yat,xnames.arg,ynames.arg,rescale=FALSE,fg="black",bg="NA",main = NULL, xlab = NULL, ylab = NULL,inches=TRUE)
{
  
    x <- re.factor(x,xlevels)
    y <- re.factor(y,ylevels)
    kat <- re.factor(kat,katlevels)
    cID <- paste(x,y)
    nl <- length(levels(as.factor(kat)))
    if(length(pal) != length(levels(kat)))
        stop("length of pal not equal to the number of levels for kat")
    mat <- tapply(z,list(y,x),FUN=Sum)
    if(missing(xat)) xat=1:ncol(mat)
    if(missing(yat)) yat=1:nrow(mat)
    if(missing(xnames.arg)) xnames.arg=colnames(mat)
    if(missing(ynames.arg)) ynames.arg=rownames(mat)    
    nx <- ncol(mat)
    ny <- nrow(mat)
    xlim=c(1,nx)
    ylim=c(1,ny)

    tmp.x <- 1:ncol(mat)
    tmp.y <- 1:nrow(mat)
    tmp.z <- as.vector(mat)
    tmp.x <- unlist(lapply(tmp.x,rep,ny))
    tmp.y <- rep(tmp.y,nx)

    cIDlevels <-  paste(unlist(lapply(levels(x),rep,ny)), rep(levels(y),nx))
    cID <- re.factor(cID,cIDlevels)
    tt <- tapply(z,list(kat,cID),Sum)
    tt <- ifelse(is.na(tt),0,tt)
    
    
    if(rescale){
       inches <- inches*sqrt(tmp.z/rescale) # because need to plot circle one at a time to deal with NA
    } else {
       inches <- inches*sqrt(tmp.z) # because need to plot circle one at a time to deal with NA
    }
    tmp.z <- sqrt(tmp.z/pi)
    
    plot.new();
    plot.window(xlim=xlim,ylim=ylim,xaxt="n",yaxt="n")
    for(i in 1:length(tmp.z)){
       if(!is.na(tmp.z[i])){         
            #symbols(tmp.x[i],tmp.y[i],circles=tmp.z[i],inches=inches[i],add=TRUE,fg=fg,bg=bg)
            if(sum(tt[,i])==0) next
            tp <- pi/2 - 2*pi*c(0,cumsum(tt[,i])/sum(tt[,i])) #torespontok
            for (j in 1:nl) {
                if (tp[j+1]==tp[j]) next
                pp <- seq(tp[j], tp[j+1],length=floor((tp[j]-tp[j+1])*4)+2) #polygon-pontok
                rx <- xinch(inches[i])
                ry <- yinch(inches[i])
                xi <- tmp.x[i]+c(0,rx*cos(pp))
                yi <- tmp.y[i]+c(0,ry*sin(pp))
                polygon(xi,yi, col=pal[j], border=NA)
            }
           polygon(tmp.x[i]+rx*cos((1:180)*pi/90),tmp.y[i]+ry*sin((1:180)*pi/90), col=NA, border=fg)
        }
     }

    box()
    axis(1,at=xat,labels=xnames.arg)
    axis(2,at=yat,labels=ynames.arg)
    title(main=main,xlab=xlab,ylab=ylab)
    return (Max(mat))
}
