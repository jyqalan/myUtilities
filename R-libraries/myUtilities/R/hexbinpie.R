hexbinpie <-
function(x, y, z,kat,xbnds=range(x), ybnds=range(y),xbin=20,shape=1,
   pal=terrain.colors(length(levels(as.factor(kat)))),
   hex="gray", circ=NA, cnt="black", ...) {
  # A small modification on original hexbinpie by calculating the proportion of z for each level of kat at each cell   \
  require(hexbin)
  hb  <- hexbin(x, y, xbnds=xbnds, ybnds=ybnds, IDs=TRUE, xbin = xbin,shape=shape )
  hbc <- hcell2xy(hb)
  rx  <- diff(hb@xbnds) / (2 * hb@xbins)
  ry  <- diff(hb@ybnds) / (2 * hb@xbins*hb@shape)
  hexC <-  hexcoords(dx=rx, dy=ry/sqrt(3), n=1)
  nl <- length(levels(as.factor(kat)))
  ztab <- tapply(z,hb@cID,Sum)
  zbnds <- quantile(ztab,prob=c(.05,.95), na.rm=TRUE )
  maxztab <- max(ztab)
  zz <- pmax(pmin(sqrt(ztab/zbnds[2]),1),0.2)
  tt <- tapply(z,list(kat,hb@cID),Sum)
  tt <- ifelse(is.na(tt),0,tt)
  for (i in seq(along=zz)) {
    if (!is.na(hex)) polygon(hbc$x[i]+hexC$x, hbc$y[i]+hexC$y, col=NA,border=hex)
    if(sum(tt[,i])==0) next
    tp <- pi/2 - 2*pi*c(0,cumsum(tt[,i])/sum(tt[,i])) #torespontok
    for (j in 1:nl) {
      if (tp[j+1]==tp[j]) next
      pp <- seq(tp[j], tp[j+1],
         length=floor((tp[j]-tp[j+1])*4)+2) #polygon-pontok
      xi <- hbc$x[i]+c(0,zz[i]*rx*cos(pp))
      yi <- hbc$y[i]+c(0,zz[i]*ry*sin(pp))
      polygon(xi,yi, col=pal[j], border=NA)
      }
    if (!is.na(circ)) polygon(hbc$x[i]+rx*zz[i]*cos((1:18)*pi/9),
       hbc$y[i]+ry*zz[i]*sin((1:18)*pi/9), col=NA, border=circ)
    }
  return (maxztab)
  }
