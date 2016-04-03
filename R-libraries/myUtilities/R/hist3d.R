hist3d <-
function(x,y,z,FUN=Sum,xbreaks,ybreaks,...){
    # equivalent function of cross.tab for continuous variables x,y
    # turn x into factors with levels being the midpoints of xbreaks
    x <- Cut(x,xbreaks) 
    y <- Cut(y,ybreaks)
    z <- tapply(z,list(x,y),FUN,...)
    return(list(x=as.numeric(levels(x)),y=as.numeric(levels(y)),z=z))
}
