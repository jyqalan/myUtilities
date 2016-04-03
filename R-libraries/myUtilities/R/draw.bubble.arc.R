draw.bubble.arc <-
function(mat,proportions,rescale,inches,...){
    # the proportions is a matrix of the same dimention as mat, with the each element
    # reprenting the proportion within each cell of mat
    nx <- ncol(mat)
    ny <- nrow(mat)    
    if(rescale){
       inches.mat <- inches*sqrt(mat/rescale) # because need to plot circle one at a time to deal with NA
       inches <- inches*sqrt(Max(mat)/rescale)     
    } else {
       inches.mat <- inches*sqrt(mat) # because need to plot circle one at a time to deal with NA
       inches <- inches*sqrt(Max(mat))     
    }        
    mat <- sqrt(mat/pi)
    for(j in 1:ny){
        for(i in 1:nx){
            if(!is.na(proportions[j,i]) & !is.na(mat[j,i])){
                rx <- xinch(inches.mat[j,i])
                ry <- yinch(inches.mat[j,i])
                angles <- seq(-round(proportions[j,i]*180),round(proportions[j,i]*180),by=1)/360*2*pi
                x <- i+rx*cos(angles)
                y <- j+ry*sin(angles)
                polygon(c(i,x),c(j,y),...)      
           }
       }
    }
}
