SavePlot <-
function(filename="",path="",size=1,pointsize=12,res=300,type="jpg") {
  # Valid types are JPG (default), PNG, and PDF.
  # the type is automatically added as an extension
  # size represents a fraction of the height of an a4 page
  # res = the resolution in 'dpi'. Values that are too high cause errors (and this value appears to be machine dependant)
  # dimensions (width & height) specify 70% of an A4 page (in inches)
  width<-(21/2.54)*0.8
  height<-(29.7/2.54)*0.8
  if(is.null(filename)) stop("Please specify a filename")
  this.image<-recordPlot()
  valid<-c("jpg","png","pdf","wmf")
  Type<-valid[match(tolower(type),valid)]
  if(is.na(Type)) {
    stop(paste(type,"is an invalid file type"))
  } else if(Type=="png") {
    filename <- paste(make.filename(filename,path),".png",sep="")
    png(filename=filename,width=width,height=height*size,units="in",pointsize=pointsize,bg="white",res=res,restoreConsole=TRUE)
    replayPlot(this.image)
    dev.off()
  } else if(type=="jpg") {
    filename <- paste(make.filename(filename,path),".jpg",sep="")
    jpeg(filename=filename,width=width,height=height*size,units="in",quality=100,pointsize=pointsize,bg="white",res=res,restoreConsole=TRUE)
    replayPlot(this.image)
    dev.off()
  } else if(type=="pdf") {
    filename <- paste(make.filename(filename,path),".pdf",sep="")
    pdf(file=filename,width=width,height=height*size,pointsize=pointsize,bg="white")
    replayPlot(this.image)
    dev.off()
  } else if(type=="wmf") {
    filename <- paste(make.filename(filename,path),".wmf",sep="")
    savePlot(filename,type="wmf")
  }
  return(filename)
}
