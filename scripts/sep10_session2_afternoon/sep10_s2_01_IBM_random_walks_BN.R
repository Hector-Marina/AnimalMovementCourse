#' ---
#' title: An introduction to movement ecology individual based models
#' author: The following pieces of codes were kindly provided by Juan Morales and modified by Bernardo Niebuhr
#' date: Animal Movement Course, SLU-Sweden, September 2023
#' ---

# Load packages ----
library(circular)
#------------------

#' There are several R packages that deal with animal movement simulation.
#' Instead of going for those, here we simulate animal movement from scratch =)
#' We can think most animal movement models in dicrete time have the random walk
#' theory underlying, so individual tracks are understood as a stochastic (probabilistic)
#' process which can then depend on different things - environmental covariates,
#' points of attraction etc. This means that there might be factors modulating how
#' animals move, but it is still a stochastic process. 
#' 
#' And: there are different ways of being random. We start by exploring some 
#' probability distributions, which are the building blocks for simulating random 
#' walk-like movement tracks.

#---------
#' # Circular distributions for angles
#' 
#' Circular distributions are interesting to represent angles because they 
#' recognize that a turning angle of -180 is the same as a turning angle of +180
#' degrees, and there is continuity between them.
#' 
#' Here we explore the wrapped Cauchy and von Mises distributions. Each of them
#' have two parameters, which are explained below.

#---
# wrapped Cauchy distribution
?dwrappedcauchy
# Parameters
mu.d <- rad(0) # Mean direction of the distribution - value around which the distribution is concentrated
rho.d <- 0.44 # "Concentration"/"perseverance" parameter, inside the interval [0,1] - for wrapped Cauchy distribution

# draw random angles from this ditribution
data1 <- rwrappedcauchy(100, mu = mu.d, rho = rho.d, 
                        control.circular = list(units="degrees"))
# plot the data
plot(data1)
# circular histogram
rose.diag(data1, cex = 2, prop = 3)

# we can also plot the original distribution from which the data was sampled
ff1 <- function(x) dwrappedcauchy(x, mu = mu.d, rho = rho.d)
curve.circular(ff1, join = T, xlim = c(-2, 2), ylim = c(-2,2), cex = 0.5) # wrapped Cauchy

#------
# von Mises distribution
?dvonmises
# Parameters
mu.d <- rad(150) # Mean direction of the distribution - value around which the distribution is concentrated
kappa.d <- 14 # "Concentration" parameter, positive number - for von Mises distribution

# draw random angles from this ditribution
data2 <- rvonmises(100, mu.d, kappa.d, control.circular = list(units="degrees"))
# plot
plot(data2)

# plot original distribution
ff2 <- function(x) dvonmises(x, mu = mu.d, kappa = kappa.d)
curve.circular(ff2, join = T, xlim = c(-2, 2), ylim = c(-2,2), cex = 0.5) # von Mises

#' QUESION: Feel free to change the values above. What changes if you increase mu.d from
#' 0 to 180 (or -180)?
#' What changes if you change the rho.d (or kappa.d) from 0 to 1 (or to a big positive number)?
#'  

######################################################################

#------
# Distributions for step lengths

#' Now we explore some commonly used distributions for step lengths.
#' Why these? Mostly because they are continuous and defined for positive values only, 
#' and step lengths are real numbers that can only be positive.

#---
# Exponential distribution
?dexp
# Parameters
lambda.d <- 1/150 # here, 150 in the expected value of the distribution

# sample data from this distribution
data3 <- rexp(100, rate = lambda.d)
# histogram
hist(data3, xlab = "Step length", main = "Exponential distribution")
# mean step length
mean(data3)

#' Notice that there might be steps considerably longer than the average step length.

# Distribution
ff3 <- function(x) dexp(x, rate = lambda.d)
curve(ff3, from = 0, to = 1000, xlab = "Step length", main = "Exponential distribution", 
      ylab = "Probability density")

#' QUESION: What changes if you change the lambda.d parameter?

#---
# Weibull distribution

#' The Weibull (and the Gamma distribution; see dgamma and rgamma) have two parameters,
#' scale and shape. The scale is more directly related to the expected (average)
#' step length, while the shape makes the distribution very flexible. Depending on 
#' the shape, it can resamble an exponential distribution (shape = 1), a 
#' power-law distribution (shape < 1), a Gaussian distribution (shape >> 1), or
#' something in-between.

?dweibull
# Parameters
shape.d <- 0.5
scale.d <- 150
scale.d * gamma(1 + 1/shape.d) # expected value of the distribution

# sample
data4 <- rweibull(100, shape = shape.d, scale = scale.d)
# histogram
hist(data4, xlab = "Step length", main = "Weibull distribution")
# mean
mean(data4)

# distribution
ff4 <- function(x) dweibull(x, shape = shape.d, scale = scale.d)
curve(ff4, from = 0, to = 1000, xlab = "Step length", main = "Weibull distribution", 
      ylab = "Probability density")

#' QUESION: What changes if you change the scale.d parameter?
#' What about the shape.d parameter? Try different values (<1, close to 1, >1, >>1).


######################################################################
#' Simple Random Walk
#' 
#' We start by simulating a simple random walk for an individual. The step lengths
#' are drawn from a a Weibull distribution, and the turning angles from a 
#' a wrapped Cauchy distribution

# just reviewing the distributions
?rweibull
op <- par(mfrow=c(3,1))
hist(rweibull(10000,scale=1,shape=1), freq=F, main="shape = 1")
hist(rweibull(10000,scale=1,shape=2), freq=F, main="shape = 2")
hist(rweibull(10000,scale=1,shape=0.5), freq=F, main="shape = 0.5")
par(op)

?rwrappedcauchy
op <- par(mfrow=c(2,2))
rose.diag(rwrappedcauchy(1000,mu=circular(0),rho=0),bins=18,prop=3)
rose.diag(rwrappedcauchy(1000,mu=circular(pi),rho=0.7),bins=18,prop=2)
rose.diag(rwrappedcauchy(1000,mu=circular(0),rho=0.9),bins=18,prop=2)
rose.diag(rwrappedcauchy(1000,mu=circular(pi),rho=0.1),bins=18,prop=3)
par(op)

#-------------------------------------------------------------------
# simulate a movement track
s_scale <- 10 # Weibull scale
s_shape <- 2 # Weibull shape
mu <- circular(0) # wrapped Cauchy mean
rho <- 0.8 # wrapped Cauchy variation

nsteps <- 100
x <- numeric(nsteps) # x position
y <- numeric(nsteps) # y position
h <- numeric(nsteps) # absolute angle of the movement
steps <- rweibull(nsteps, scale=s_scale, shape=s_shape)
turns <- rwrappedcauchy(nsteps, mu=mu, rho=rho)
turns <- ifelse(turns>pi, turns-2*pi,turns)
h[1] <- runif(1,0,2*pi)
for(t in 2:nsteps){
  x[t] <- x[t-1] + steps[t-1]*cos(h[t-1])
  y[t] <- y[t-1] + steps[t-1]*sin(h[t-1])
  h[t] <- h[t-1] + turns[t]
}

# plot track
plot(x,y, type="o", pch=16, asp=1)
# start point
points(x[1], y[1], pch=16, col="blue", cex=2)
# end point
points(x[nsteps], y[nsteps], pch=16, col="red", cex=2)

#' QUESTION: What changes if you change the parameters of the distribution of
#' step lengths and turning angles?

#-------------------------------------------------------------------
# simulate several individuals

# we repeat the same for 10 individuals

ntracks <- 10
X <- matrix(NA,nsteps,ntracks)
Y <- matrix(NA,nsteps,ntracks)

for(i in 1:ntracks){
  x <- numeric(nsteps)
  y <- numeric(nsteps)
  h <- numeric(nsteps)
  steps <- rweibull(nsteps, scale=s_scale[i], shape=s_shape)
  turns <- rwrappedcauchy(nsteps, mu=mu, rho=rho)
  turns <- ifelse(turns>pi, turns-2*pi,turns)
  h[1] <- runif(1,0,2*pi)
  for(t in 2:nsteps){
    x[t] <- x[t-1] + steps[t-1]*cos(h[t-1])
    y[t] <- y[t-1] + steps[t-1]*sin(h[t-1])
    h[t] <- h[t-1] + turns[t]
  }
  X[,i] <- x
  Y[,i] <- y
}

# plot the trajectories
matplot(X,Y, type="l", pch=16, col=rainbow(ntracks), asp=1)
points(X[1,], Y[1,], pch=16, col="blue", cex=2)
points(X[nsteps,], Y[nsteps,], pch=16, col="red", cex=2)


#--------------------------------------------------------------------------------
##### it is interesting to see how displacement changes with time
# we can look at the net squared displacement here
time = 0:(nsteps-1)
matplot(time, sqrt((X-X[1,])^2 + (Y-Y[1,])^2), type ="l", pch=16, xlab="time", ylab="squared displacement", col="gray")
lines(time, rowMeans(sqrt((X-X[1,])^2 + (Y-Y[1,])^2)), lwd=4, col=2)
abline(lm(rowMeans(sqrt((X-X[1,])^2 + (Y-Y[1,])^2)) ~ time-1), lwd=4, lty=2, col=1)

# notice that they are walking randomly, with no dispersal/migration period nor
# attraction/homerange process. As such, their NSD grows linearly through time.

########################################################################################
# Biased random walk
# This is a random walk with a central attractor

# In this case, the final angle is drawn from a wrapped Cauchy distribuion with a mean direction 
#  (mu) defined not only by the previous direction of the animal step, 
#  but by an attraction factor
# The magnitude of the attraction factor is larger the more distant the animal is from the 
#  attraction point; besides, it is larger the larger the values of the attraction coefficient, beta
# The attraction coefficient may also be negative; in this case, we simulate a random walk with
#  repulsion

#-------------------------------------------------------------------
# simulate a movement track

require(circular)
nsteps <- 100
beta <- 1.5 # Coefficient of attraction or bias - positive values correpond to attraction, negative values correspond to avvoidance
rho <- 0.6 # Concentration parameter around the bias absolute angle
scale <- 1
shape <- 1

# Coordinates of the point of attraction/repulsion
xh <- 30
yh <- 30

x <- numeric(nsteps)
y <- numeric(nsteps) 
h <- numeric(nsteps)
steps <- numeric(nsteps)

h[1] <- runif(1,1,2*pi)
x[1] <- rnorm(1,0,1)
y[1] <- rnorm(1,0,1)

for(t in 2:nsteps){  
  adj <- xh - x[t-1]
  op  <- yh - y[t-1]
  r   <- sqrt(adj^2 + op^2)
  ya <- sin(h[t-1]) + beta*(op/r)
  xa <- cos(h[t-1]) + beta*(adj/r)    
  m_t <- atan2(ya,xa)
  h[t] <- rwrappedcauchy(1, mu=circular(m_t), rho=rho)
  steps[t-1] <- rweibull(1, scale=scale, shape=shape)
  x[t] <- x[t-1] + cos(h[t])*steps[t-1]
  y[t] <- y[t-1] + sin(h[t])*steps[t-1]
}            

plot(x,y, type="l",lwd=2, xlim=c(-30,30),ylim=c(-20,20), asp=1)     
points(x[1],y[1], col="red", cex=2, pch=16)

# ... E: Exercise ---- 
#' How can you use this structure to simulate a home range behavior, e.g. 
#' the movement of a central-place forager?
#' Try this and change the attraction coefficient beta to see what changes.





















#-------------------------------------------------------------------
# simulate several individuals

ntracks <- 10
X <- matrix(NA,nsteps,ntracks)
Y <- matrix(NA,nsteps,ntracks)

beta <- 0.5 # we can change this here and check what changes
xh <- 0
yh <- 0

# notice that now we changed the attraction point to the origin (the nest?)
for(i in 1:ntracks){
  x <- numeric(nsteps)
  y <- numeric(nsteps) 
  h <- numeric(nsteps)
  steps <- numeric(nsteps)
  
  h[1] <- runif(1,1,2*pi)
  x[1] <- rnorm(1,0,1)
  y[1] <- rnorm(1,0,1)
  
  for(t in 2:nsteps){  
    adj <- xh - x[t-1]
    op  <- yh - y[t-1]
    r   <- sqrt(adj^2 + op^2)
    ya <- sin(h[t-1]) + beta*(op/r)
    xa <- cos(h[t-1]) + beta*(adj/r)    
    m_t <- atan2(ya,xa)
    h[t] <- rwrappedcauchy(1,mu=circular(m_t),rho=rho)
    steps[t-1] <- rweibull(1,scale=scale, shape=shape)
    x[t] <- x[t-1] + cos(h[t])*steps[t-1]
    y[t] <- y[t-1] + sin(h[t])*steps[t-1]
  } 
  X[,i] <- x
  Y[,i] <- y
}
# all individuals
matplot(X,Y, type="l", pch=16, col=rainbow(ntracks), asp=1, xlim=c(-10,10),ylim=c(-10,10))

# one individual
matplot(X[,2],Y[,2], type="l", pch=16, col=rainbow(ntracks), asp=1, xlim=c(-10,10),ylim=c(-10,10))

#--------------------------------------------------------------------------------------------
# it is interesting to see how displacement changes with time in case
# we again use the NSD
time = 0:(nsteps-1)
matplot(time,sqrt((X-X[1,])^2 + (Y-Y[1,])^2), type ="l", pch=16, xlab="time", ylab="squared displacement", col="gray")
lines(time, rowMeans(sqrt((X-X[1,])^2 + (Y-Y[1,])^2)), lwd=4, col=2)
abline(lm(rowMeans(sqrt((X-X[1,])^2 + (Y-Y[1,])^2)) ~ time-1), lwd=4, lty=2, col=1)

# Now the observed pattern is quite different from a linear increase (our Brownian motion 
# from the first example) because of the attraction to the central place.

########################################################################################
# Simulation of a predator-prey chase
# 
# In this case, we simulate preys as performing correlated random walks
# Predators, in turn, perform biased random walks; at each step, each predator compute which prey is 
#  the closest and is "attracted" to its position

nsteps <- 100
nprey <- 1
npredator <- 1
ntracks <- nprey + npredator
X <- matrix(NA,nsteps,ntracks)
Y <- matrix(NA,nsteps,ntracks)

# Parameters of the prey
py_scale <- 1
py_shape <- 1
py_mu <- circular(0)
py_rho <- 0

# Parameters of the predator
pd_beta <- 1 # Coefficient of attraction or bias
pd_rho <- 0.6 # Concentration parameter around the bias absolute angle
pd_scale <- 1
pd_shape <- 1

for(i in 1:ntracks){
  # Movement of the prey
  # Prey animals follow a correlated random walk
  if(i <= nprey) {
    x <- numeric(nsteps)
    y <- numeric(nsteps)
    h <- numeric(nsteps)
    steps <- rweibull(nsteps, scale=py_scale, shape=py_shape)
    turns <- rwrappedcauchy(nsteps, mu=py_mu, rho=py_rho)
    turns <- ifelse(turns>pi, turns-2*pi,turns)
    h[1] <- runif(1,0,2*pi)
    x[1] <- rnorm(1,0,10)
    y[1] <- rnorm(1,0,10)
    for(t in 2:nsteps){
      x[t] <- x[t-1] + steps[t-1]*cos(h[t-1])
      y[t] <- y[t-1] + steps[t-1]*sin(h[t-1])
      h[t] <- h[t-1] + turns[t]
    }
    X[,i] <- x
    Y[,i] <- y
  } else {
    # Movement of the predator
    # They follow the nearest prey
    x <- numeric(nsteps)
    y <- numeric(nsteps) 
    h <- numeric(nsteps)
    steps <- numeric(nsteps)
    
    h[1] <- runif(1,1,2*pi)
    x[1] <- rnorm(1,0,5)
    y[1] <- rnorm(1,0,5)
    
    for(t in 2:nsteps){
      # position of the nearest prey is the point of attraction
      col <- which.min((X[t-1,1:nprey] - x[t-1])^2 + (Y[t-1,1:nprey] - y[t-1])^2)
      xh <- X[t-1,col]
      yh <- Y[t-1,col]
      
      adj <- xh - x[t-1]
      op  <- yh - y[t-1]
      r   <- sqrt(adj^2 + op^2)
      ya <- sin(h[t-1]) + pd_beta*(op/r)
      xa <- cos(h[t-1]) + pd_beta*(adj/r)    
      m_t <- atan2(ya,xa)
      h[t] <- rwrappedcauchy(1, mu=circular(m_t), rho=pd_rho)
      steps[t-1] <- rweibull(1, scale=pd_scale, shape=pd_shape)
      x[t] <- x[t-1] + cos(h[t])*steps[t-1]
      y[t] <- y[t-1] + sin(h[t])*steps[t-1]
    } 
    X[,i] <- x
    Y[,i] <- y
  }
  
}

# prey
matplot(X[,1:nprey], Y[,1:nprey], type="l", pch=16, col="red", asp=1, xlim=c(min(X),max(X)), ylim=c(min(Y),max(Y)))
# predator
matplot(X[,(nprey+1):ntracks],Y[,(nprey+1):ntracks], type="l", pch=16, col="blue", asp=1, add=T)
legend("topleft", legend = c("prey", "predator"), col = c("red", "blue"), lwd=1.1, cex = 0.7, bty = "n")


# ... E: Advanced exercise ----
#' (to do it later, not in class)
#' Let's say that our prey learned it is not good to be close to the predator, and 
#' started to avoid it. How would you model this system including prey avoidance behavior?
#' 
#' hint: remember avoidance here = negative attraction

