hist2d <-
function(x,z,FUN=Sum,xbreaks,...){
    # equivalent function of single.tab for continuous variables x,y
    # turn x into factors with levels being the midpoints of xbreaks
    x <- Cut(x,xbreaks) 
    z <- tapply(z,list(x),FUN,...)
    return(list(x=as.numeric(levels(x)),z=z))
}
