draw.bubble.cross <-
function(mat,proportions,rescale,inches,byrow=T,...){
    # the proportions is a matrix of the same dimention as mat, with the each row (if byrow=T, otherwise column)representing
    # a vector of proportions summing up to one, and the largest proportion within each row should
    # have the same radious as the largest bubble for that row
    # This is ususally used to show whether sampling is representative of the catch with respect to
    # a coviriate
    nx <- ncol(mat)
    ny <- nrow(mat)    
    if(rescale){
       inches.mat <- inches*sqrt(mat/rescale) # because need to plot circle one at a time to deal with NA
       inches <- inches*sqrt(Max(mat)/rescale)     
    } else {
       inches.mat <- inches*sqrt(mat) # because need to plot circle one at a time to deal with NA
       inches <- inches*sqrt(Max(mat))     
    }
    if(byrow){
        for(j in 1:ny){
            sum.mat <- Sum(mat[j,])
            for(i in 1:nx){
                if(!is.na(proportions[j,i])){
                    rx <- xinch(inches.mat[j,i]*sqrt((proportions[j,i]/(mat[j,i]/sum.mat)))) 
                    ry <- yinch(inches.mat[j,i]*sqrt((proportions[j,i]/(mat[j,i]/sum.mat))))
                    segments(i-rx,j,i+rx,j,...)
                    segments(i,j+ry,i,j-ry,...)
                }
            }
        }
    } else {
        for(i in 1:nx){
            sum.mat <- Sum(mat[,i])
            for(j in 1:ny){
                if(!is.na(proportions[j,i])){
                    rx <- xinch(inches.mat[j,i]*sqrt((proportions[j,i]/(mat[j,i]/sum.mat)))) 
                    ry <- yinch(inches.mat[j,i]*sqrt((proportions[j,i]/(mat[j,i]/sum.mat))))
                    segments(i-rx,j,i+rx,j,...)
                    segments(i,j+ry,i,j-ry,...)
                }
            }
        }
        
    }
}
