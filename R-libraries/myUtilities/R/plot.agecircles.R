plot.agecircles <-
function(mat, b4spawn, expand = 1, xlab = "Year", ylab = "Year class", plotit
 = T, ylim = NULL, xlim = NULL, only5thcohorts = T)
{
#
# Given a matrix in which each column is a vector of numbers (or proportions)
# at age for a given year, makes a bubble plot in which each of these vectors
# is represented as a vertical line of circles (areas proportional to the
# vector values) and bubbles at the same horizontal level are from the
# same year class.  dimnames(mat) should contain ages and years.  
#
# b4spawn: T if sample was collected before the spawning season in that
#     fishing year.
# (NB year.class = sample.year - age - ifelse(b4spawn,1,0))
#
# When expand = 1 the size of the biggest circle is adjusted so that it would
# just touch (either horizontally or vertically, whichever is closer) an
# adjacent circle of the same size.  When expand != 1 all circle radii are
# multiplied by expand.
#
        original.mat <- mat
        mat <- sqrt(abs(mat))
yrs <- as.numeric(dimnames(mat)[[2]])
nyr <- dim(mat)[2]
ages <- as.numeric(dimnames(mat)[[1]])
nage <- dim(mat)[1]
yrcl <- matrix(0, nage, nyr)
for(i in 1:nyr)
yrcl[, i] <- yrs[i] - ages - ifelse(b4spawn, 1, 0)
ylim <- if(is.null(ylim)) range( - yrcl) + c(-1, 1) else rev( - ylim)
xlim <- if(is.null(xlim)) range(yrs) + c(-1, 1) else xlim
if(plotit) {
plot(0, 0, type = "n", xlim = xlim, ylim = ylim, xlab = xlab, 
ylab = ylab, yaxt = "n")
uyrcl <- unique(yrcl)
#if(only5thcohorts)
uyrcl <- uyrcl[uyrcl %% 5 == 0]
axis(2,  - uyrcl, paste(uyrcl))
#mtext(paste(uyrcl), 2, 0.5, at =  - uyrcl, adj = 1)
}
        pin <- par()$pin
        usr <- par()$usr
        xyin=c(pin[1]/(usr[2]-usr[1]),pin[2]/(usr[4]-usr[3]))
yrinc <- ifelse(length(yrs) > 1, min(diff(yrs)), 1)
maxradxin <- 0.5 * expand * yrinc * xyin[1]
maxradyin <- 0.5 * expand * xyin[2]
maxradin <- min(maxradxin, maxradyin)
tmp <- cos(seq(0, pi, len = 13))
max.circle <- list(x = (tmp[c(1:13, 12:1)] * maxradin)/(xyin[1] * max(
mat)), y = (tmp[c(7:1, 2:13, 12:7)] * maxradin)/(xyin[2] * max(
mat)))
if(plotit) {
for(i in 1:nyr) {
for(j in 1:nage) {
polygon(max.circle$x * mat[j, i] + yrs[i], 
  max.circle$y * mat[j, i] - yrcl[j, i],col=ifelse(original.mat[j,i]>0,"white","black"))
}
}
invisible()
}
else list(max.circle = max.circle, mat = mat, yrs = yrs, yrcl = yrcl)
}
