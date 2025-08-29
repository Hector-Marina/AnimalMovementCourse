######### Functions to read, clean and manipulate data ###########
#For Animal movement course 2023



#Function to extract data from one individual
#' Function to extract data from one individual
#' @param FAdata Dataframe with FA data
#' @param cowID ID of selected cow
#' @return Dataframe with a subset of FA data
#' @export
#' 

getIndividual <- function(FAdata, cowID) {
  if (!("id" %in% colnames(FAdata)))
    stop("FAdata has incorrect structure: column id is missing")
  
  FAdata.ID1 <- FAdata[FAdata$id == cowID, ]
  
  return(FAdata.ID1)
}


#' Function to extract data within a certain time interval
#' @param FAdata Dataframe with FA data
#' @param start Start of the time interval
#' @param end End of the time interval
#' @export
#' 
getInterval <- function(FAdata,  
                        start = "2019-11-15 01:00:00 CET", 
                        end = "2019-11-17 02:05:00 CET") {
  start <- as.POSIXct(strptime(start, "%Y-%m-%d %H:%M:%S"))
  end <- as.POSIXct(strptime(end, "%Y-%m-%d %H:%M:%S"))
  
  start.epoch <- as.integer(start)
  end.epoch <- as.integer(end)
  
  data <- FAdata[FAdata$time / 1000 >= start.epoch & FAdata$time / 1000 <= end.epoch, ]
  
  newStart <- as.POSIXct(min(as.numeric(data$time)) / 1000, origin = "1970-01-01")
  newEnd <- as.POSIXct(max(as.numeric(data$time)) / 1000, origin = "1970-01-01")
  
  print(paste0("The data for cows ", paste0(unique(FAdata$id),collapse = ", "), 
               " spans between ", newStart,
               " and ", newEnd))
  
  return(data)
}
#' Prints details of FA data (dimensions, time span, number of tags)
#' 
#' @param FAdata Dataframe with FA data
#' @export
#'
getInfo <- function(FAdata) {
  print(paste0(ncol(FAdata), " columns"))
  print(paste0(nrow(FAdata), " rows"))
  
  start <- as.POSIXct(min(as.numeric(FAdata$time)) / 1000, origin = "1970-01-01")
  end <- as.POSIXct(max(as.numeric(FAdata$time)) / 1000, origin = "1970-01-01")
  
  print(paste0("Time period: from ", start,  " to ", end))
  
  print(paste0(length(unique(FAdata$id)), " tags"))
}

#' Gets time span  of the FA data
#' @param FAdata Dataframe with FA data
#' @return Vector with two elements: start and end of the time span
#' @export
#'
getTimeRange <- function(FAdata) {
  start <- as.POSIXct(min(as.numeric(FAdata$time)) / 1000, origin = "1970-01-01")
  end <- as.POSIXct(max(as.numeric(FAdata$time)) / 1000, origin = "1970-01-01")
  
  return(c(start, end))
}

#' Obtain raster grid 
#' @param x X coordinates of points to be divided into grid (only min and max will be used)
#' @param y Y coordinates of points to be divided into grid (only min and max will be used)
#' @param bRotated Logical, if the raster is rotated
#' @param nrow Number of grid rows
#' @param ncol Number of grid columns
#' @return Raster object with grid cells
#' @export
#' 
getGrid <- function(x, y, bRotated = F, nrow = 100, ncol = 100) {
  require(raster)
  
  if (bRotated) {
    tmp <- x
    x <- y
    y <- -tmp
  }
  
  grid <- raster(nrows = nrow, ncols = ncol, xmn = min(x), xmx = max(x), ymn = min(y), ymx = max(y))
  
  return(grid)
}

#' Read the barn's blue print from .csv file

readBarnData <- function(file) {
  barn <- read.csv(file, header=TRUE)
  
  # barn[which(barn$Unit == "robotbed1"), 1] <- "mbed1"
  newRow <- barn[which(barn$Unit == "robotbed2"), ]
  newRow[1] <- "mbed2"
  barn <- rbind(barn, newRow)
  
  sel <- which(barn$Unit == "mbed2")
  newX <- barn$x1[sel] + (barn$x4[sel] - barn$x1[sel]) / 7
  barn$x1[sel] <- newX
  barn$x2[sel] <- newX
  
  # Add names to units
  barn$Name <- barn$Unit
  barn$Name[which(barn$Unit == "robotbed1")] <- "bed7"
  barn$Name[which(barn$Unit == "robotbed2")] <- ""
  barn$Name[which(barn$Unit == "mbed2")] <- "bed8"
  barn$Name[which(barn$Unit == "feed")] <- "Feed table"
  barn$Name[which(barn$Unit == "deepstraw1")] <- ""
  barn$Name[which(barn$Unit == "deepstraw2")] <- ""
  barn$Name[which(barn$Unit == "bed8")] <- ""
  barn$Name[which(barn$Unit == "bed9")] <- ""
  
  
  # Use numbers without "bed"
  barn$Name[which(barn$Unit == "bed1")] <- "1"
  barn$Name[which(barn$Unit == "bed2")] <- "2"
  barn$Name[which(barn$Unit == "bed3")] <- "3"
  barn$Name[which(barn$Unit == "bed4")] <- "4"
  barn$Name[which(barn$Unit == "bed5")] <- "5"
  barn$Name[which(barn$Unit == "bed6")] <- "6"
  barn$Name[which(barn$Unit == "robotbed1")] <- "7"
  barn$Name[which(barn$Unit == "mbed2")] <- "8"
  
  return(barn)
}




#' Reads PA data as a dataframe using \code{vroom} package
#' 
#' @param file Input file with location data
#' @return Dataframe with data
#' @examples \code{PAdata <- read.PAData(file)}
#' @export
#'

read.PAData <- function(file) {
  require(vroom)
  
  start <- Sys.time()
  PAdata <- vroom(file, col_names = c("FileType", "id", "tag", "t1", "t2", "x", "y", "z", "activity", "dist"), delim = ",",show_col_types = FALSE)
  print(paste0("Read in ", Sys.time() - start, " seconds"))
  
  return(PAdata)
}


#' Reads PAA data as a dataframe using \code{vroom} package
#' 
#' @param file Input file with location data
#' @return Dataframe with data
#' @examples \code{PAAdata <- read.PAAData(file)}
#' @export
#'
read.PAAData <- function(file) {
  require(vroom)
  
  start <- Sys.time()
  PAAdata <- vroom(file, col_names = c("FileType", "id", "tag", "time", "interval", 
                                       "activity", "dist", "periods", "duration"), 
                   delim = ",")
  print(paste0("Read in ", Sys.time() - start, " seconds"))
  
  return(PAAdata)
}

