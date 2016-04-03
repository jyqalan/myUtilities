DemoColors <-
function()
{
cols <- c("blue","blue1","blue2","blue3","blue4","purple","purple1","purple2","purple3","purple4", 
                 "green","green1","green2","green3","green4","cyan","cyan1","cyan2","cyan3","cyan4",
                 "coral","coral1","coral2","coral3","coral4","tomato","tomato1","tomato2","tomato3","tomato4",
   "red", "red1", "red2","red3","red4","brown","brown1","brown2","brown3","brown4",
                 "plum","plum1", "plum2","plum3","plum4","orchid","orchid1","orchid2","orchid3","orchid4",
                 "tan","tan1","tan2","tan3","tan4","wheat","wheat1","wheat2","wheat3","wheat4",
                 "pink1","pink1","pink2","pink3","pink3","orange","orange1","orange2","orange3","orange4",
                 "yellow","yellow1","yellow2","yellow3","yellow4","gold","gold1","gold2","gold3","gold4",
   "gray10","gray20","gray30","gray40","gray50","gray60","gray70","gray80","gray90","gray100",
          "ivory","ivory1","ivory2","ivory3","ivory4","khaki","khaki1","khaki2","khaki3","khaki4"
               )
        ncols <-100
        dd <- c(-1,1)/2
        ix<-rep(1:10,10);
        iy=rep(1:10,each=10);
        rx <- dd + range(ix)
        ry <- dd + range(iy)
        plot(rx, ry, type="n", axes = FALSE, xlab = "", ylab = "")
        for(i in 1:ncols){
        polygon(x=c(ix[i]-0.25,ix[i]-0.25,ix[i]+0.25,ix[i]+0.25),
                        y=c(iy[i]-0.25,iy[i]+0.25,iy[i]+0.25,iy[i]-0.25),border=NULL,col=cols[i])
text(ix[i], iy[i]-0.5, cols[i], adj=0.5,cex = 0.8)
        
            }
}
