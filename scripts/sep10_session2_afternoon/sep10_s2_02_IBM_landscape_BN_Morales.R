#' ---
#' title: An introduction to movement ecology individual based models
#' author: The following pieces of codes were kindly provided by Juan Morales
#' date: Animal Movement Course, SLU-Sweden, September 2023
#' ---

# Load packages ----
library(KernSmooth)
library(MASS)
library(lattice)
library(circular)
#------------------

####################################################################
#' first we'll simulate a landscape with two habitat types
#' The function below builds a patchy landscape
#' (modified from Matthiopoulos et al. 2015)

environ <- function(d,x,mx,bw,sl){
  ar<-array(0, dim=c(d,d))
  # Places seeds in arena
  slope<- sl # 0.05 # Introduces SW-to-NE gradient in resource
  cox<-cbind(1+(d-1)*runif(x, min=0, max=1)^slope, 1+(d-1)*runif(x, min=0, max=1)^slope)
  if(slope == 0) cox <- cbind(runif(x,0,d),runif(x,0,d))
  # Smooths seeds to create spatial autocorrelation
  sarx<-KernSmooth::bkde2D(cox, bandwidth = c(bw,bw), gridsize=c(d,d),range.x=list(c(1,d),c(1,d)))
  sarx$fhat<-mx*(sarx$fhat/max(sarx$fhat))
  return(sarx$fhat)
}

# set landscape parameters
set.seed(1234)
d <- 400   # landscape size
x <- 400   # number of seeds for patches (patch centers)
mx <- 1    # scaling of bumps
bw <- rpois(x, 10)+1   # bandwith of patches
sl <- 0.5           # slope

# simulate environment
E <- environ(d,x,mx,bw,sl) # bumpy landscape...
mapPalette <- colorRampPalette(c("gold", "darkgreen"))

# plot
levelplot(E,useRaster=T, col.regions=mapPalette(100), xlab="x", ylab="y")

# plot with terra
terra::plot(terra::flip(terra::rast(E), "vertical"))


#-------------------------------------------------------------------
# simulate a movement track

#' We now simulate a movement track in this landscape
#' The animal movement randomly but the step lengths are higher when close
#' (or far, depending on the signal of the parameter b1) of the green patches.
#' 
#' 
b0 <- log(10) # intercept (like the intercept of a RSF)
b1 <- 0 # how much the abundance of green resources affect the step length
# (positive = walk long distances in the green areas; 
# negative = walk short distances in the green areas)
s_shape <- 1 
mu <- circular(0)
rho <- 0

nsteps <- 500
nind <- 1

# simulate steps and turns
steps <- matrix(NA,nsteps,nind)
turns <- matrix(NA,nsteps,nind)

# build trajectory
X <- matrix(NA,nsteps,nind)
Y <- matrix(NA,nsteps,nind)
C <- matrix(NA,nsteps,nind)
meanstep <- matrix(NA,nsteps,nind)
for(i in 1:nind){
  X[1,i] <- runif(1,ceiling(d/2-50),ceiling(d/2+50))
  Y[1,i] <- runif(1,ceiling(d/2-50),ceiling(d/2+50))
  C[1,i] <- runif(1)*2*pi  # compass heading

  for(t in 2:nsteps){  

    turns[t,i] <- rwrappedcauchy(1, mu = circular(mu), rho = rho)
    if(turns[t,i] >pi){
      turns[t,i] <- turns[t,i] - 2*pi     
    } 
    meanstep[t,i] <- exp(b0+b1*E[ceiling(X[t-1,i]),ceiling(Y[t-1,i])])
    steps[t,i] <- rgamma(1, shape=s_shape, scale=meanstep[t,i]/s_shape)
    C[t,i] <- C[t-1,i] + turns[t,i]
    X[t,i] <- (X[t-1,i] + cos(C[t,i]) * steps[t,i]) 
    if(X[t,i]<0 | X[t,i]>d ) X[t,i] <- X[t-1,i] 
    Y[t,i] <- (Y[t-1,i] + sin(C[t,i]) * steps[t,i])
    if(Y[t,i]<0 | Y[t,i]>d ) Y[t,i] <- Y[t-1,i] 
  }
}

# plot trajectories
image(1:d,1:d, E, col=mapPalette(100), xlab="x", ylab="y")
matlines(X,Y,type="l",pch=16, col=rainbow(nind),lwd=2)

#' QUESTION: what happens when you change the b1 parameter? Try it with negative,
#' zero, and positive values, and also with small and large values.