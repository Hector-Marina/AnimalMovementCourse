#' ---
#' title: An introduction to movement ecology individual based models
#' author: The following pieces of codes were kindly provided by Juan Morales and then modified by Bernardo Niebuhr
#' date: 
#' 
#' output:
#' pdf_document:
#'   toc: true
#'   toc_depth: 2
#'   number_sections: true
#' ---

# --------------- label=print_options, warning=FALSE, message=FALSE, echo=FALSE

# for printout as pdf
# require("ezknitr")
# require("knitr")
# # Print options
# options(width = 165)
# opts_chunk$set(error = FALSE, message = FALSE, warning = FALSE, cache = FALSE, echo = TRUE, results = TRUE)

# --------------- label=load_packages

##### Circular distributions for angles

# Generates random values from a circular distribution and plots them
# Plots a circular distribution
# Here we use wrapped Cauchy and von Mises distributions

library(circular)

# wrapped Cauchy distribution
?dwrappedcauchy
# Parameters
mu.d <- rad(0) # Mean direction of the distribution - value around which the distribution is concentrated
rho.d <- 0.44 # "Concentration"/"perseverance" parameter, inside the interval [0,1] - for wrapped Cauchy distribution

data1 <- rwrappedcauchy(100, mu = mu.d, rho = rho.d, 
                        control.circular = list(units="degrees"))
plot(data1)
rose.diag(data1, cex = 2, )

ff1 <- function(x) dwrappedcauchy(x, mu = mu.d, rho = rho.d)
curve.circular(ff1, join = T, xlim = c(-2, 2), ylim = c(-2,2), cex = 0.5) # wrapped Cauchy

# von Mises distribution
?dvonmises
# Parameters
mu.d <- rad(150) # Mean direction of the distribution - value around which the distribution is concentrated
kappa.d <- 14 # "Concentration" parameter, positive number - for von Mises distribution

data2 <- rvonmises(100, mu.d, kappa.d, control.circular = list(units="degrees"))
plot(data2)

ff2 <- function(x) dvonmises(x, mu = mu.d, kappa = kappa.d)
curve.circular(ff2, join = T, xlim = c(-2, 2), ylim = c(-2,2), cex = 0.5) # von Mises

######################################################################

##### Distributions for step lengths

# Exponential distribution
?dexp
# Parameters
lambda.d <- 1/150 # here, 150 in the expected value of the distribution

data3 <- rexp(100, rate = lambda.d)
hist(data3, xlab = "Step length", main = "Exponential distribution")
mean(data3)

ff3 <- function(x) dexp(x, rate = lambda.d)
curve(ff3, from = 0, to = 1000, xlab = "Step length", main = "Exponential distribution", 
      ylab = "Probability density")

# Weibull distribution
?dweibull
# Parameters
shape.d <- 0.5
scale.d <- 150
scale.d * gamma(1 + 1/shape.d) # expected value of the distribution

data4 <- rweibull(100, shape = shape.d, scale = scale.d)
hist(data4, xlab = "Step length", main = "Weibull distribution")
mean(data4)

ff4 <- function(x) dweibull(x, shape = shape.d, scale = scale.d)
curve(ff4, from = 0, to = 1000, xlab = "Step length", main = "Weibull distribution", 
      ylab = "Probability density")

# Power-law (Levy) distribution

# Parameters
mu.d <- 2.5 # Value in the interval (1,3] - 1 = long steps more probable, 3 = all steps in the same scale
xmin.d <- 1 # Minimum value of a step

# Creating the power-law distribution density function
dpowlaw <- function(x, alfa, xmin, log=FALSE){
  c <- (alfa-1)*xmin^(alfa-1)
  if(log) ifelse(x < xmin, 0, log(c*x^(-alfa)))
  else ifelse(x < xmin, 0, c*x^(-alfa))
}
# Testing the function
integrate(dpowlaw, -Inf, Inf, alfa = mu.d, xmin = xmin.d)

# Plot the function
curve(dpowlaw(x, alfa = mu.d, xmin = xmin.d), from = 0, to = 100, log="")
# log axes
curve(dpowlaw(x, alfa = mu.d, xmin = xmin.d), from = 1, to = 100, log = "xy", ylim = c(1E-6, 2),
      ylab = "Probability density", xlab = "Step length") # mu = 2.5
curve(dpowlaw(x, alfa = 3, xmin = xmin.d), from = 1, to = 100, col = "green", log = "xy", add = T) # mu = 3
curve(dpowlaw(x, alfa = 1.5, xmin = xmin.d), from = 1, to = 100, col = "blue", log = "xy", add = T) # mu = 1.5
# comparing to an exponential distribution
curve(dexp(x, rate = 1/8), from = 1, to = 100, col = "red", log = "xy", add = T)

# Creating a power-law distribution random number generator
rpowlaw <- function(n, alpha, xmin = 1, maximum = 1000000) {
  random <- runif(n * 10000, xmin, maximum)
  prob.powlaw <- dpowlaw(random, alfa = alpha, xmin = xmin)
  sample(random, size = n, prob = prob.powlaw)
}

# Random data
data5 <- rpowlaw(100, alpha = mu.d, xmin = xmin.d)
hist(data5, xlab = "Step length", main = "Power-law distribution", breaks = 20)
mean(data5)

######################################################################
# Simple Random Walk
# movement will be described based on steps and turns
# steps will follow a Weibull distribution
# turns will follow a wrapped Cauchy distribution
# review the distributions:

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
s_scale <- 10 # s_scale <- c(1, 1.5, 2, 5, 0.3)
s_scale <- rgamma(10, shape = 5, scale = 3)
s_shape <- 2
mu <- circular(0)
rho <- 0.8

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

plot(x,y, type="o", pch=16, asp=1)

points(x[1], y[1], pch=16, col="blue", cex=2)
points(x[nsteps], y[nsteps], pch=16, col="red", cex=2)

#-------------------------------------------------------------------
# simulate several individuals

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
# we usually look at the squared displacement
time = 0:(nsteps-1)
matplot(time,(X-X[1,])^2 + (Y-Y[1,])^2, type ="l", pch=16, xlab="time", ylab="squared displacement", col="gray")
lines(time, rowMeans((X-X[1,])^2 + (Y-Y[1,])^2), lwd=4, col=2)
abline(lm(rowMeans((X-X[1,])^2 + (Y-Y[1,])^2) ~ time-1), lwd=4, lty=2, col=1)

########################################################################################
# Biased random walk
# This is a random walk with a central attractor

# In this case, the final angle is drawn from a wrapped Cauchy distribuion with a mean direction 
#  (mu) defined not only by the previous direction of the animal step, 
#  but by an attraction factor
# The magnitude of the attraction factor is larger the more distant the animal is from the 
#  attraction point; besides, it is larger the larger the values of the attraction coefficient, beta
# The attraction coefficient may also be negaive; in this case, we simulate a random walk with
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
lines(x,y, col=2,lwd=1, xlim=c(-15,15),ylim=c(-10,10), asp=1)     

#-------------------------------------------------------------------
# simulate several individuals

ntracks <- 10
X <- matrix(NA,nsteps,ntracks)
Y <- matrix(NA,nsteps,ntracks)

xh <- 0
yh <- 0
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
# it is interesting to see how displacement changes with time
# we usually look at the squared displacement
time = 0:(nsteps-1)
matplot(time,(X-X[1,])^2 + (Y-Y[1,])^2, type ="l", pch=16, xlab="time", ylab="squared displacement", col="gray")
lines(time, rowMeans((X-X[1,])^2 + (Y-Y[1,])^2), lwd=4, col=2)
abline(lm(rowMeans((X-X[1,])^2 + (Y-Y[1,])^2) ~ time-1), lwd=4, lty=2, col=1)

########################################################################################
# Simulation of a predator-prey chase
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
matplot(X[,1:nprey], Y[,1:nprey], type="l", pch=16, col="red", asp=1, xlim=c(min(X),max(X)), ylim=c(min(Y),max(Y)))
matplot(X[,(nprey+1):ntracks],Y[,(nprey+1):ntracks], type="l", pch=16, col="blue", asp=1, add=T)
legend("topleft", legend = c("prey", "predator"), col = c("red", "blue"), lwd=1.1, cex = 0.7, bty = "n")
