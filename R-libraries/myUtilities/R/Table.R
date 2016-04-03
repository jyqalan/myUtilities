Table <-
function(...)
{
    if (length(list(...))==1) return(c(table(...,exclude=c()),"TOTAL.OBS"=length(...)))
    else {
        ANS<-table(...,exclude=c())
        return(rbind(ANS,"TOTAL.OBS"=apply(ANS,2,sum)))
    }
}
