#Norbert Billet - IRD
#occurences_file_path <- "/home/norbert/Boulot/iMarine/WPS/Occurences/NOAA_simple.csv"

kernelDensityContour <- function(dataset, x_column_name="longitude", y_column_name="latitude", targeted_percents=c(25, 50, 75, 80, 90, 95, 98, 99))
{
  if(missing(dataset))
  {
    stop("The dataset is missing, stupid ass")
  } 
  
  #load requested libraries
  if(! require(MASS) | ! require(sp))
  {
    stop("Missing library")
  }
  
  #read th csv input file
  csvDf <- dataset
  
  #test for the x column
  if(is.na(match(x_column_name, names(csvDf))))
  {
    stop(paste("Cannot find the x column:", x_column_name))
  }
  
  #test for the y column
  if(is.na(match(y_column_name, names(csvDf))))
  {
    stop(paste("Cannot find the y column:", y_column_name))
  }
  
  #keep only usefull columns
  occurencesDf <- data.frame(x=csvDf[, x_column_name], y=csvDf[, y_column_name])
  rm(csvDf)
  
  #compute bandwith as mean of normal reference distribution bandwith for x and y
  bandwidth <- (bandwidth.nrd(occurencesDf$x) + bandwidth.nrd(occurencesDf$y)) / 2
  
  #grid limits for density estimation
  minX <- max(floor(min(occurencesDf$x) - bandwidth), -180)
  maxX <- min(ceiling(max(occurencesDf$x) + bandwidth), 180)
  minY <- max(floor(min(occurencesDf$y) - bandwidth), -90)
  maxY <- min(ceiling(max(occurencesDf$y) + bandwidth), 90)
  rangeX <- maxX - minX
  rangeY <- maxY - minY
  
  #two-dimensional kernel density estimation
  kernelDensity <- kde2d(x=occurencesDf$x, y=occurencesDf$y, h=bandwidth, n=c(rangeX,rangeY), lims=c(minX, maxX, minY, maxY))
  
  #sum of estimated density
  kernelDensity.total <- sum(kernelDensity$z)
  
  #compute all distincts levels of density
  densityLevels <- as.vector(kernelDensity$z)
  densityLevels <- unique(sort(densityLevels[densityLevels > 0]))
  
  #order targeted percents
  targeted_percents <- sort(targeted_percents[targeted_percents > 0 & targeted_percents < 100], decreasing=TRUE)
  
  #browse the sorted density levels to look for match with targeted percents
  targetedDensityLevels <- vector()
  adjustedTargetedPercents <- vector()
  i <- 1
  currentTargetedPercent <- 1
  while(i <= length(densityLevels) && currentTargetedPercent <= length(targeted_percents))
  {
    #compute the percentage of estimated density greater than the current density level
    currentDensityPercent <- 100*sum(kernelDensity$z[kernelDensity$z > densityLevels[i]]) / kernelDensity.total
    #if lower than the current targeted percentage then we found a requested levels
    if(currentDensityPercent < targeted_percents[currentTargetedPercent])
    {
      adjustedTargetedPercents[currentTargetedPercent] <- currentDensityPercent
      targetedDensityLevels[currentTargetedPercent] <- densityLevels[i]
      currentTargetedPercent <- currentTargetedPercent + 1
    }
    i <- i + 1
  }
  
  #extract contour lines for each targeted density level
  densityContourLines <- contourLines(kernelDensity, levels=targetedDensityLevels)
  
  #build the sp object
  Srl <- list()
  
  for(i in 1:length(targetedDensityLevels))
  {
    polygonList <- list()
    for(j in 1:length(densityContourLines))
    {
      if(densityContourLines[[j]]$level == targetedDensityLevels[i])
      {
        polygonList[[length(polygonList) + 1]] <- Polygon(cbind(densityContourLines[[j]]$x, densityContourLines[[j]]$y))
      }
    }
    Srl[[i]] <- Polygons(srl=polygonList, ID=targeted_percents[i])
  }
  Sr <- SpatialPolygons(Srl, proj4string=CRS("+init=epsg:4326"))
  spDf <- SpatialPolygonsDataFrame(Sr, data.frame(level=targetedDensityLevels, percent=targeted_percents, real_percent=adjustedTargetedPercents, row.names=targeted_percents), match.ID = TRUE)
  return(spDf)
}
