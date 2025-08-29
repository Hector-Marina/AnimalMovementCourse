######### Functions to plot data ###########
#For Animal movement course 2023


##################################
#' Plot the barn layout
#' @param barn Dataframe with rectangles
#' @param bRotated Logical, if the layout should be rotated
#' @param bAdd Logical, if the new plot should be generated
#' @param bText Logical, if the unit names should be plotted
#' @param ... Additional graphic parameters
#' @export
#' 
plotBarn <- function(barn, bRotated = FALSE, bAdd = FALSE, bText = FALSE, ...) {
  if (!bRotated) {
    # Original layout
    
    if (!bAdd)
      plot(c(barn$x1[1], barn$x3[1]), c(barn$y1[1], barn$y3[1]), 
           asp = 1, cex = 0, xlab = "", ylab = "", ...)
    
    rect(barn$x1[1], barn$y1[1], barn$x3[1], barn$y3[1], col = "gray90") # Base
    
    rect(barn$x1[-1], barn$y1[-1], barn$x3[-1], barn$y3[-1], border = "black") #  Units
    
    if (bText)
      text((barn$x1 + barn$x3) / 2, (barn$y1 + barn$y3) / 2, barn$Unit, cex = 0.5)
  } else {
    # Rotated layout
    
    if (!bAdd)
      plot(c(barn$y1[1], barn$y3[1]), c(-barn$x1[1], -barn$x3[1]), 
           asp = 1, cex = 0, xlab = "", ylab = "", ...)
    
    rect(barn$y1[1], -barn$x1[1], barn$y3[1], -barn$x3[1], col = "gray90") # Base
    rect(barn$y1[-1], -barn$x1[-1], barn$y3[-1], -barn$x3[-1], border = "black") # Units
    
    if (bText)
      text((barn$y1 + barn$y3) / 2, -(barn$x1 + barn$x3) / 2, barn$Unit, cex = 0.5)
  }
}


#####################################
#'Add barn's specific features for plot barn's blueprint
#'ONLY for bRotated = FALSE in plotBarn function

addBarnFeatures <- function(barn, textSize = 0.75) {
  mid <- (barn$x3[which(barn$Unit == "bed3")] + barn$x1[which(barn$Unit == "bed3")]) / 2
  
  sel <- which(barn$Unit == "feed")
  rect(barn$x1[sel], barn$y1[sel], barn$x3[sel], barn$y3[sel], col = adjustcolor("yellowgreen", alpha.f = 0.5),
       border = "black")
  
  
  for (i in which(#barn$Unit != "feed" & 
    barn$Unit != "Base")) {
    srt <- 90
    
    if (barn$Name[i] == "bed7" | barn$Name[i] == "bed8")
      srt <- 0
    
    if (barn$Name[i] %in% c("1", "2", "3", "4", "5", "6", "7", "8")) {
      srt <- 0
      text((barn$x1[i] + barn$x3[i]) / 2, (barn$y1[i] + barn$y3[i]) / 2, barn$Name[i], cex = textSize * 2, 
           srt = srt,
           col = adjustcolor("black", 0.7))
    } else
      text((barn$x1[i] + barn$x3[i]) / 2, (barn$y1[i] + barn$y3[i]) / 2, barn$Name[i], cex = textSize, srt = srt)
  }
  
  
  text(mid, 1300, "Milking area")
  
  segments(x0 = mid, y0 = barn$y1[which(barn$Unit == "bed3")], y1 = barn$y3[which(barn$Unit == "Base")], col = "black")
  
  segments(x0 = mid, y0 = barn$y1[which(barn$Unit == "bed3")], 
           x1 = barn$x3[which(barn$Unit == "robotbed1")], y1 = barn$y3[which(barn$Unit == "robotbed1")], col = "black")
  segments(x0 = mid, y0 = barn$y1[which(barn$Unit == "bed3")], 
           x1 = barn$x1[which(barn$Unit == "robotbed2")], y1 = barn$y3[which(barn$Unit == "robotbed2")], col = "black")
  
  
  segments(x0 = barn$x3[which(barn$Unit == "feed")[1]], y0 = barn$y3[which(barn$Unit == "robotbed1")], 
           x1 = barn$x1[which(barn$Unit == "robotbed1")], col = "black")
  
  segments(x0 = barn$x1[which(barn$Unit == "feed")[2]], y0 = barn$y3[which(barn$Unit == "robotbed2")], 
           x1 = barn$x3[which(barn$Unit == "robotbed2")], col = "black")
  
  
  for (unit in c("bed1", "bed2", "bed5", "bed6")) {
    segments(x0 = ((barn$x1 + barn$x3) / 2)[which(barn$Unit == unit)], 
             y0 = barn$y1[which(barn$Unit == unit)], y1 = barn$y3[which(barn$Unit == unit)], col = "black")
  }
}



######################################################
#' Plot the individual trajectory during the given time
plotPATrajectory <- function(PAdata, individual, startTime = NULL, endTime = NULL) {
  
  plotBarn(barn, axes = FALSE, bText = FALSE)
  title(paste0(individual, ": ", startTime, " - ", endTime))
  

  data <- PAdata[which(PAdata$id == individual), ]
  
  if (!is.null(startTime)) {
    data <- data[which(as.POSIXct(data$t1 / 1000, origin = "1970-01-01") >= startTime & 
                         as.POSIXct(data$t2 / 1000, origin = "1970-01-01") <= endTime), ]
  }
  
  lines(data$x, data$y, pch = 19, col = adjustcolor("gray", alpha.f = 0.6), cex = 0.5)
  points(data$x, data$y, pch = 19, col = data$activity + 1, cex = 0.5)
  
  # Start of cubicle/feeding
  for (i in 2:nrow(data)) {
    start <- as.POSIXct(data$t1[i] / 1000, origin = "1970-01-01")
    end <- as.POSIXct(data$t2[i] / 1000, origin = "1970-01-01")
    
    # Print all activities
    print(paste0(data$activity[i], ": ",
                 format(start, "%H:%M:%S"), " - ", format(end, "%H:%M:%S")))
    
    if ((data$activity[i - 1] == 1 | data$activity[i - 1] == 2) & 
        (data$activity[i] == 3 | data$activity[i] == 4)) {
      k <- i
      while (data$activity[i] == data$activity[k]) {
        if (k == nrow(data))
          break
        # Update end time for consecutive records for the same activity
        end <- as.POSIXct(data$t2[k] / 1000, origin = "1970-01-01")
        k <- k + 1
      }
      
      text(data$x[i], data$y[i], 
           paste0(format(start, "%H:%M:%S"), " - ", format(end, "%H:%M:%S")), 
           cex = 0.7, pos = 4, col = data$activity[i] + 1)
      
      # Print only cubicle/feeding times
      # print(paste0(ifelse(data$activity[i] == 3, "In cubicle: ", "Feeding: "), 
      #              format(start, "%H:%M:%S"), " - ", format(end, "%H:%M:%S")))
    }
  }
  
  legend("bottomright", legend = c("0 Unknown", "1 Standing", "2 Walking", "3 In cubicle", 
                                   "4 At feed", "5 At drinker", "998 Out def", "999 Outside"), 
         title = "Activity", col = c(0, 1, 2, 3, 4, 5, 998, 999) + 1, pch = 19, bg = NA)
}

