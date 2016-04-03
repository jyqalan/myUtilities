DemoPointsLines <-
function()
{     
     Pex <- 3 
     ipch <- 1:(np <- 25+11); k <- floor(sqrt(np)); dd <- c(-1,1)/2
     rx <- dd + range(ix <- (ipch-1) %/% k)
     ry <- dd + range(iy <- 3 + (k-1)-(ipch-1) %% k)
     pch <- as.list(ipch)
     pch[25+ 1:11] <- as.list(c("*",".", "o","O","0","+","-",":","|","%","#"))
     plot(rx, ry, type="n", axes = FALSE, xlab = "", ylab = "")
     #abline(v = ix, h = iy, col = "lightgray", lty = "dotted")
     for(i in 1:np) {
       pc <- pch[[i]]
       points(ix[i], iy[i]+0.5, pch = pc, cex = Pex)
       ## red symbols with a yellow interior (where available)
       text(ix[i] - .5, iy[i]+0.5, pc, cex = 1)
     }
     ilty=1:6
     lty=as.list(ilty)
     for(i in 1:6){
       lines(x=c(ix[i]-0.5,ix[i]+0.5),y=c(iy[i],iy[i]),lty=lty[[i]],lwd=2)
     }
}
