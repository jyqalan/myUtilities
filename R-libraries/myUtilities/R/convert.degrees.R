convert.degrees <-
function(x, digits = 3, symbol = F)
{
# returns a character represenation of decimal degrees
# in degrees, minutes and decimal minutes.
# Assumes that negative numbers are latitudes
degree <- abs(trunc(x))
minute <- (abs(x) - degree) * 60
if(symbol) {
  RES <- paste(Format(degree, 0, pad=T), "\260", Format(minute, digits, pad=T), "'", sep = "")
  a1<-substring(RES,1,regexpr(" ",RES)-1)
  a2<-substring(RES,regexpr(" ",RES)+1,nchar(RES))
  RES<-ifelse(nchar(a1)>0,paste(a1,"0",a2,sep=""),a2)
} else RES <- paste(Format(degree, 0, pad=T), " ", Format(minute, digits, pad=T), sep = "")
return(RES)
}
