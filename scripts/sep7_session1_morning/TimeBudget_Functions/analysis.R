########## Methods to process and analyse data ###########
#For Animal movement course 2023

getMeanPos <- function(PAdata) {
  ids <- sort(unique(PAdata$id)) # Get tag IDs
  meanPos <- data.frame(id = ids, x = rep(NA, length(ids)), y = rep(NA, length(ids)), t = rep(NA, length(ids)))
  
  for (id in ids) {
    data <- PAdata[which(PAdata$id == id), ]
    
    sel <- which(data$activity == 3) # "In cubicle" activity only
    
    
    times <- data$t2[sel] - data$t1[sel]
    
    x <- weighted.mean(data$x[sel], times)
    y <- weighted.mean(data$y[sel], times)
    t <- sum(times) / 1000 / 60 / 60 # In hours
    
    meanPos$x[which(meanPos$id == id)] <- x
    meanPos$y[which(meanPos$id == id)] <- y
    meanPos$t[which(meanPos$id == id)] <- t
  }
  
  return(meanPos)
}

getMeanPosTag <- function(PAdata) {
  tags <- sort(unique(PAdata$tag)) # Get tag IDs
  meanPos <- data.frame(tag = tags, x = rep(NA, length(tags)), y = rep(NA, length(tags)), t = rep(NA, length(tags)))
  
  for (tag in tags) {
    data <- PAdata[which(PAdata$tag == tag), ]
    
    sel <- which(data$activity == 3) # "In cubicle" activity only
    
    
    times <- data$t2[sel] - data$t1[sel]
    
    x <- weighted.mean(data$x[sel], times)
    y <- weighted.mean(data$y[sel], times)
    t <- sum(times) / 1000 / 60 / 60 # In hours
    
    meanPos$x[which(meanPos$tag == tag)] <- x
    meanPos$y[which(meanPos$tag == tag)] <- y
    meanPos$t[which(meanPos$tag == tag)] <- t
  }
  
  return(meanPos)
}

getTimeBudget <- function(data,selectedIDs) {
  columns = c("tag","Standing","Walking","Rest","Feeding") 
  TBS = data.frame(matrix(nrow = 0, ncol = length(columns))) 
  colnames(TBS) = columns
  for (tagidx in 1:length(selectedIDs)) {
    i <- which(data$tag == selectedIDs[tagidx])
 
     TBStag<- data.frame(tag=data$tag[i],x = data$x[i], y = data$y[i], t = (data$t2[i] - data$t1[i]) / 1000 / 60 / 60, act=data$activity[i])
     tag<-selectedIDs[tagidx]
     TB1<-sum(TBStag[which(TBStag$act == 1),]$t)/sum(TBStag$t)
    
     TB2<-sum(TBStag[which(TBStag$act == 2),]$t)/sum(TBStag$t)
     
     TB3<-sum(TBStag[which(TBStag$act == 3),]$t)/sum(TBStag$t)
     
     TB4<-sum(TBStag[which(TBStag$act == 4),]$t)/sum(TBStag$t)
     TBS[tagidx,]<-c(tag,TB1,TB2,TB3,TB4)
     
  }
 
  return(TBS)
  
}

# Cubicle usage heatmap
# selectedTagIDs - selected tag IDs, e.g. those in first lactation
# maxHours <- 0 # Maximum time spent in any cubicle (in hours)
getDailyCubicleUsageHeatmap <- function(data, selectedTagIDs, 
                                        barn,
                                        units = c("bed1", "bed2", "bed3", "bed4", "bed5", "bed6"),
                                        rows = rep(16, 6),
                                        cols = rep(2, 6),
                                        title = "", maxHours = 0, bPlot = TRUE) {
  require(raster)
  
  bRot <- F
  hmList <- list() # List of rasters for heatmaps
  
  # Prepare rasters for each bed
  for (uIndex in 1:length(units)) {
    unit <- units[uIndex]
    cat(unit)
    
    sel <- which(barn$Unit == unit)
    grid <- getGrid(c(barn$x1[sel], barn$x3[sel]), c(barn$y1[sel], barn$y3[sel]), 
                    nrow = rows[uIndex], ncol = cols[uIndex], bRot)
    
    bedLayer <- 0
    for (tag in selectedTagIDs) {
      i <- which(data$tag == tag)
      if (length(i) == 0)
        next
      df <- data.frame(x = data$x[i], y = data$y[i], t = (data$t2[i] - data$t1[i]) / 1000 / 60 / 60)
      coordinates(df) <- ~x+y
      r <- rasterize(df, grid, field = "t", fun = "sum", background = 0)
      
      if (is.na(r@data@max) | is.infinite(r@data@max) | r@data@max == 0)
        next
      
      if (r@data@max > 0)
        bedLayer <- bedLayer + r
    }
    
    if (!is.numeric(bedLayer))
      maxHours <- max(maxHours, bedLayer@data@max)
    
    names(bedLayer) <- unit
    
    hmList[[unit]] <- bedLayer
    
    cat("... ")
  }
  cat("Done!\n")
  
  # Make a summary plot
  if (bPlot & length(units) == 6) {
    opar <- par(mfrow = c(2, 3))
    for (i in c(2,4,6, 1,3,5)) {
      plot(hmList[[i]], zlim = c(0, maxHours), 
           main = "", bty = "n", axes = FALSE, legend.lab = "", legend.only = FALSE)
      mtext(names(hmList[[i]]), side = 3, line = 0.25, cex = 0.8)
    }
    mtext(title, side = 3, line = -1.5, outer = TRUE) #  Title
    par(opar)
  }
  
  return(hmList)
}

