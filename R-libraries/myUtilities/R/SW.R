SW <-
function(std=NULL,width=NULL,height=NULL,size=NULL,pointsize=NULL,rescale=NULL,bg=NULL,mar=NULL,oma=NULL,xaxs=NULL,yaxs=NULL,las=NULL,mfrow=NULL,...)
{
     if(is.null(pointsize))
         pointsize <- 10
     if(is.null(rescale))
         rescale = "fixed"
     if(is.null(bg))
         bg = "white"
     if(is.null(size))
         size=1
     if(is.null(xaxs))
         xaxs="r"
     if(is.null(yaxs))
         yaxs="r"
     if(is.null(las))
         las=0
     if(is.null(mar))
         mar=c(5.1,4.1,4.1,2.1);
     if(is.null(oma))
         oma=c(0,0,0,0);  
     if(is.null(std)){
         if(is.null(width)) width= (21/2.54)*0.8;
         if(is.null(height)) height=(29.7/2.54)*0.8*size;
         if(is.null(mfrow)) mfrow=c(1,1);
     } else if (std==0){
         if(is.null(width)) width=7;
         if(is.null(height)) height=7;
         if(is.null(mfrow)) mfrow=c(1,1);
    } else if (std==1){
            width=5.75;height=7.4
            if(is.null(mfrow)) mfrow=c(2,1)       
   } else {
           stop(paste("std=",std,",not supported!",sep="")) 
    }           
   windows.options(reset=TRUE)  
   windows(width=width,height=height,pointsize=pointsize,rescale=rescale,bg=bg)
   par(mar=mar,oma=oma,mfrow=mfrow,xaxs=xaxs,yaxs=yaxs,las=las,...)
}
