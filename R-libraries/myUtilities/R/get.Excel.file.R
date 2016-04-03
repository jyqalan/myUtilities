get.Excel.file <-
function(file,path,sheet="") {
  filename<-make.filename(file,path)
  channel <- odbcConnectExcel(filename)
  indata <- sqlFetch(channel,sheet)
  odbcClose(channel)
  return(indata)
}
