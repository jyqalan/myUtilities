inRegion <-
function(corners, rx, ry)
{
# Ian Doonan
# check if rx,ry are in polygon "corners"
# corners$x, $y corners of polygon
#
# Bodged by Alistair Dunn to fix a weird error
#
In <- rep(F, length(rx))
a1 <- rep(0, length(rx))
n <- length(corners$x)
for(j in c(1:(n + 1))) {
  i <- j
  if(j > n)  i <- 1
  Inn <- ((corners$y[i] == ry) & (corners$x[i] == rx))
  In[Inn] <- T
  Atan <- atan2(corners$y[i] - ry, corners$x[i] - rx)
  if(j == 1)  prevAtan <- Atan
  a0 <- Atan - prevAtan
# the next line is a bodge to fix a weird error
  a0[!is.finite(a0)] <- 0
  a0[a0 > pi] <- a0[a0 > pi] - 2 * pi
  a0[a0 <  - pi] <- a0[a0 <  - pi] + 2 * pi
  a1 <- a1 + a0
  prevAtan <- Atan
  }
In[!In] <- abs(abs(a1[!In]) - 2 * pi) < 0.001
#  sum=360 if inside; 0 if out
if(length(In[!In]) > 0) {
  Inn <- abs(a1[!In]) > 0.001
  if(length(Inn[Inn]) > 0) {
  # outside points not sum to 0
    print("Queer points .. CHECK them")
    print(t(matrix(c(a1[!In][Inn]/2/pi*360,c(1:n)[!In][Inn]),ncol=2,dimnames=list(NULL,c("Sum angles","Index of point")))))
    }
  }
# print(a1/2/pi*360)
return(In)
}
