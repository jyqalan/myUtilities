draw.bubble <-
function(mat,xat=1:ncol(mat),yat=1:nrow(mat),xnames.arg=colnames(mat),ynames.arg=rownames(mat),
 rescale=FALSE,fg="black",bg="lightgrey",main = NULL, xlab = NULL, ylab = NULL,inches=TRUE,plot.NA=F,add=F,...)
  {
    #from Dan, best of best
    nx <- ncol(mat)
    ny <- nrow(mat)
    xlim=c(1,nx)
    ylim=c(1,ny)

    tmp.x <- 1:ncol(mat)
    tmp.y <- 1:nrow(mat)
    tmp.z <- as.vector(mat)
    tmp.x <- unlist(lapply(tmp.x,rep,ny))
    tmp.y <- rep(tmp.y,nx)
    tmp.z <- sqrt(tmp.z/pi)    
    
    if(rescale){
       inches.mat <- inches*sqrt(mat/rescale) # because need to plot circle one at a time to deal with NA
       inches <- inches*sqrt(Max(mat)/rescale)     
    } else {
       inches.mat <- inches*sqrt(mat) # because need to plot circle one at a time to deal with NA
       inches <- inches*sqrt(Max(mat))     
   }        
    mat <- sqrt(mat/pi)
    if(add==F){
        plot.new();
        plot.window(xlim=xlim,ylim=ylim,xaxt="n",yaxt="n")
    }
    if(plot.NA){
       tmp.z <- ifelse(is.na(tmp.z),0,tmp.z)
       symbols(tmp.x,tmp.y,circles=tmp.z,inches=inches,add=TRUE,fg=fg,bg=bg,...)
    }else{
       for(j in 1:ny){
           for(i in 1:nx){
               if(!is.na(mat[j,i])) 
                   symbols(i,j,circles=mat[j,i],inches=inches.mat[j,i],add=TRUE,fg=fg,bg=bg,...)
           }
       }
}

    box()
    axis(1,at=xat,labels=xnames.arg)
    axis(2,at=yat,labels=ynames.arg)
    title(main=main,xlab=xlab,ylab=ylab)
   
  }
