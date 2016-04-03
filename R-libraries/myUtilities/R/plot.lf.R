plot.lf <-
function(data,lgth,cv, xlim,ylim,ylim2,xat,yat,yat2,xnames,ynames,ynames2,line=0.5,cex=0.8,col=0,col2=0,cex2=0.8,border="black",offset=0,tck=-0.025,outer=F,add=F)
{
par(xaxs="i",yaxs="i")
#par(xpd=T)
if(length(data)!=length(lgth)){
stop("lengthof data not equal to that of lgth")
}
if(missing(xlim)){
xlim <- c(0,max(lgth))
}
if(missing(ylim)){
ylim <- c(0,max(pretty(data)))
}   
if(missing(xat))
xat=lgth[lgth %% 20==0];
if(missing(yat))
yat=ylim
if(missing(xnames))
xnames=xat
if(missing(ynames))
ynames=yat
if(!missing(cv)){
if(missing(ylim2)){
ylim2 = c(0,max(pretty(cv)))
}
if(missing(yat2)){
yat2 = ylim2
}          
if(missing(ynames2)){
ynames2 = yat2
}
if(length(yat2)!=length(ynames2))
stop("yat2 and ynames2 not of the same length")      
}
if(length(xat)!=length(xnames))
stop("xat and xnames not of the same length")
if(length(yat)!=length(ynames))
stop("yat and ynames not of the same length")

offset = ylim[2]*offset
ylim_prime = ylim
yat_prime = yat
ylim_prime[2] = ylim_prime[2]+offset
yat_prime = yat_prime+offset

if(!missing(cv)){
yat2 = yat2/ylim2[2]*ylim[2]
ylim2_prime = ylim2
yat2_prime = yat2
ylim2_prime[2] = ylim2_prime[2]+offset
yat2_prime = yat2_prime+offset
}
xx <- barplot(data,xlim=xlim,ylim=ylim_prime,space=0,density=-1,err=-1,xlab= "",ylab = "", yaxt = "n",xaxt="n",col=col,border=border,offset=offset,add=add)
if(outer){
zz <- axis(1, at = xat+(diff(xx)/2)[1], labels = xnames,tck = tck,outer=T)
} else {
zz <- axis(1, at = xat+(diff(xx)/2)[1], labels = F,tck = tck,outer=F)
mtext(side=1,at=xat+(diff(xx)/2)[1],text=xnames,line=line,cex=cex,outer=F)
}
#text(zz,-diff(ylim)*line,xnames,cex=par("cex.axis"))
if(outer){
axis(2, at = yat_prime, labels = ynames,tck=tck,outer=T)
} else {
axis(2, at = yat_prime, labels = F,tck=tck,outer=F)       
mtext(side=2,at=yat_prime,text=ynames,line=line,cex=cex,outer=F)
}
if(!missing(cv)){
points(xx,(cv)/ylim2[2]*ylim[2]+offset,pch=16,cex=cex2,col=col2)
lines(xx,(cv)/ylim2[2]*ylim[2]+offset,col=col2)
if(outer){
axis(4,at=yat2_prime,labels=ynames2,tck=tck,outer=T)
} else {
axis(4,at=yat2_prime,labels=F,tck=tck,outer=F)    
mtext(side=4,at=yat2_prime,text=ynames2,line=line,cex=cex,outer=F)
}
}
abline(h=offset,col="gray")
box()
#par(xpd=F)
invisible()
}
