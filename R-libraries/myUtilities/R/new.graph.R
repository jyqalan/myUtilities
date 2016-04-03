new.graph <-
function(size=1,pointsize=10,xaxs="i",yaxs="i",las=1,...) {
  windows.options(reset=TRUE)
  # Set up window size: Make sure this agrees with SavePlot
  width<-(21/2.54)*0.8
  height<-(29.7/2.54)*0.8
  windows(width=width,height=height*size,pointsize=pointsize,bg="white",rescale="fixed")
  # Set sefault par parameters
  par(xaxs = xaxs, yaxs = yaxs, las = las, ...)
  .par<<-par(no.readonly = TRUE)
  .par$size<<-size
  .par$pointsize<<-pointsize
  invisible()
}
