#This code assumes age is Normally Distributed
#Within the sample / organization being sample

#Entitlement is negatively associated with age

#Sample has more GenX-er's than Baby Boomers or Millenials

# There is measurement error in the Entitlement outcome variable

##################

library(randomizr)
library(mvtnorm)
library(reshape2)
library(MASS)
library(sn)
library(arm)

sim_data <- function(r2 = .2, n = 1e6, p = 5, adjusted.r.square = FALSE) {
  
  # Make symmetric covariance matrix with ...
  # ... all variance = 1 and covariances = 0 ...
  sig.mat <- diag(p + 1)
  # ... With exception for covariance between outcome and predictors
  # given by r = sqrt(R2 / p)
  if (adjusted.r.square) r2 <- unadj.r2(adj.r2, n, p)
  sig.mat[1, -1] <- sig.mat[-1, 1] <- sqrt(r2 / p)
  
  # Data frame with data from multinormal data with desired R2
  d <- as.data.frame(mvtnorm::rmvnorm(n, sigma = sig.mat))
  names(d) <- c("Y", paste0("X", seq_len(p)))
  
  # We also return R2 and RMSE for this data to use later
  fit      <- lm(Y ~ ., data = d)
  Rsquared <- summary(fit)$r.squared
  RMSE     <- sqrt(mean(residuals(fit) ^ 2))
  structure(d, Rsquared = Rsquared, RMSE = RMSE,
            class = c("sim_data", "data.frame"))
}

agesim <- function(x,y){
  
  #Simulate data with known 
  #R^2
  #Sample Size
  #Number of factors
  #Doing a simple univariate
  #regression
  
  age_data = sim_data(r2 = x,
                      n = y,
                      p = 1)
  
  #Created an age variable that rescales X1 to vary between 22 (entry into workforce) and 75 (leaving workforce)
  #Rounding it to 1's place to make it as realistic as possible, noone ever enters decimals for age
  #Note this relationship is negative
  #As you get older, you are less entitiled 
  age_data$age <- round(scales::rescale(-age_data$X1, to = c(22,75)),0)
  
  #Creating a fictional "entitlement" outcome and rescaling to be a Likert type response
  
  #Adding in measurement error as a N ~ (0,1) rv to the original Y true score (not rescaled yet) variable
   
  age_data$entitlement <- round(scales::rescale(age_data$Y, to = c(10,50)),0)
  
  #Creating generational bins how it's done now
  age_data$gen1[age_data$age<=35] <- "Millenial"
  age_data$gen1[35<age_data$age & age_data$age<=54] <- "GenX"
  age_data$gen1[54<age_data$age] <- "BB"
  
  #Setting generation bins as a factor
  age_data$gen1 <- as.factor(age_data$gen1)
  
  #Randomly assigning responses to a generation
  age_data$gen2 <- complete_ra(N = nrow(age_data), num_arms = 3)
  
  #Replacing randomzier names with generational names
  age_data$gen2 <- gsub("T1", "Millenial", age_data$gen2)
  
  age_data$gen2 <- gsub("T2", "GenX", age_data$gen2)
  
  age_data$gen2 <- gsub("T3", "BB", age_data$gen2)
  
  age_data$gen2 <- as.factor(age_data$gen2)
  
  #Regressing entitlement on age
  mod.a <- lm(entitlement ~ age, age_data)
  
  #Regressing entitlement on rescaled age
  mod.b <- lm(entitlement ~ gen1, age_data)
  
  #Regressing entitlement on randomly assigned age
  mod.c <- lm(entitlement ~ gen2, age_data)
  
  #Function for mean squared error
  rmse <- function(error)
  {
    sqrt(mean(error^2))
  }
  
  #Function for mean absolute error
  mae <- function(error)
  {
    mean(abs(error))
  }
  
  #Gets these three things for each model
  
  return( matrix(c(
    rmse(mod.a$residuals),
    rmse(mod.b$residuals),
    rmse(mod.c$residuals),
    mae(mod.a$residuals),
    mae(mod.b$residuals), 
    mae(mod.c$residuals)
  )))
}

a <- replicate(50,t(apply(matrix(seq(0.1,0.9,.1)), 1, FUN = agesim, y=300))[,1])

b <- replicate(50,t(apply(matrix(seq(0.1,0.9,.1)), 1, FUN = agesim, y=300))[,2])

c <- replicate(50,t(apply(matrix(seq(0.1,0.9,.1)), 1, FUN = agesim, y=300))[,3])

matplot(a, type = "c",
        ylim = c(0,1),
        ylab = "RMSE", 
        col = "black",
        xaxt = "n")

axis(side = 1, at = seq(0,9,1), label = seq(0.0,0.9,.1))

title(main = "Age Normally Distributed w/i sample \nAge negatively related w/ Entitlement \nn = 300, 50 repititions", sub = "R^2")

matlines(b, type = "c", ylim = c(0,1), col = "black")

matlines(c, type = "c", ylim = c(0,1), col = "black")

matlines(rowMeans(a), type = "s", lwd = 5, col = "yellow")

matlines(rowMeans(b), type = "s", lwd = 5, col = "orange")

matlines(rowMeans(c), type = "s", lwd = 5, col = "red")

legend("bottomleft",legend = c("Random Generations", "Normal Generations", "Continuous Age"), col = c("red", "orange", "yellow"), lwd = "2")

d <- replicate(50,t(apply(matrix(seq(0.1,0.9,.1)), 1, FUN = agesim, y=300))[,4])

e <- replicate(50,t(apply(matrix(seq(0.1,0.9,.1)), 1, FUN = agesim, y=300))[,5])

f <- replicate(50,t(apply(matrix(seq(0.1,0.9,.1)), 1, FUN = agesim, y=300))[,6])

matplot(d, type = "c",
        ylim = c(0,1),
        ylab = "MAE", 
        col = "black",
        xaxt = "n")

axis(side = 1, at = seq(0,9,1), label = seq(0.0,0.9,.1))

title(main = "Age Normally Distributed w/i sample \nAge negatively related w/ Entitlement \nn = 300, 50 repititions", sub = "R^2")

matlines(e, type = "c", ylim = c(0,5), col = "black")

matlines(f, type = "c", ylim = c(0,5), col = "black")

matlines(rowMeans(d), type = "s", lwd = 5, col = "yellow")

matlines(rowMeans(e), type = "s", lwd = 5, col = "orange")

matlines(rowMeans(f), type = "s", lwd = 5, col = "red")

legend("bottomleft",legend = c("Random Generations", "Normal Generations", "Continuous Age"), col = c("red", "orange", "yellow"), lwd = "2")
