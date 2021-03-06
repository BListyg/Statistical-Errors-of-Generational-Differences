agesim <- function(x,y){
  
  sim_data <- function(r2, n, p) {
    
    p = 1
    
    # Let's keep it simple, 
    mu <- rep(0,p+1)
    Sigma <- matrix(sqrt(r2), nrow=p+1, ncol=p+1) + diag(p+1)*(1-sqrt(r2))
    
    rawvars <- mvrnorm(n=n, mu=mu, Sigma=Sigma)
    
    # We can see our normal sample produces results very similar to our 
    #specified covariance levels.
    
    # No lets transform some variables
    pvars <- pnorm(rawvars)
    
    age_data <- matrix(qsn(pvars, 5, 2, 5), ncol = p+1, nrow = n)
    
    age_data <- data.frame(age_data)
    
    colnames(age_data) = c("Y", "X1")
    
    return(age_data)
    
  }
  
  #Simulate data with known 
  #R^2
  #Sample Size
  #Number of factors
  #Doing a simple univariate
  #regression
  
  age_data <- sim_data(r2 = x,
                        n = y)
  
  #Created an age variable that rescales X1 to vary between 22 (entry into workforce) and 75 (leaving workforce)
  #Rounding it to 1's place to make it as realistic as possible, noone ever enters decimals for age
  #Note this relationship is negative
  #As you get older, you are less entitiled 
  age_data$age <- round(scales::rescale(age_data$X1, to = c(22,75)),0)
  
  #Creating a fictional "entitlement" outcome and rescaling to be a Likert type response  
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
  
  age_data$entitlement <- ((age_data$entitlement - max(age_data$entitlement))*-1)+1
  
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
        ylim = c(0,15),
        ylab = "RMSE", 
        col = "black",
        xaxt = "n")

axis(side = 1, at = seq(0,9,1), label = seq(0.0,0.9,.1))

title(main = "Age skewed towards younger participants w/i sample \nAge negatively related w/ Entitlement \nn = 300, 50 repititions", sub = "R^2")

matlines(b, type = "c", ylim = c(0,15), col = "black")

matlines(c, type = "c", ylim = c(0,15), col = "black")

matlines(rowMeans(a), type = "l", lwd = 5, col = "yellow")

matlines(rowMeans(b), type = "l", lwd = 5, col = "orange")

matlines(rowMeans(c), type = "l", lwd = 5, col = "red")

legend("bottomleft",legend = c("Random Generations", "Normal Generations", "Continuous Age"), col = c("red", "orange", "yellow"), lwd = "2")

d <- replicate(50,t(apply(matrix(seq(0.1,0.9,.1)), 1, FUN = agesim, y=300))[,4])

e <- replicate(50,t(apply(matrix(seq(0.1,0.9,.1)), 1, FUN = agesim, y=300))[,5])

f <- replicate(50,t(apply(matrix(seq(0.1,0.9,.1)), 1, FUN = agesim, y=300))[,6])

matplot(d, type = "c",
        ylim = c(0,15),
        ylab = "MAE", 
        col = "black",
        xaxt = "n")

axis(side = 1, at = seq(0,9,1), label = seq(0.0,0.9,.1))

title(main = "Age skewed towards younger participants w/i sample \nAge negatively related w/ Entitlement \nn = 300, 50 repititions", sub = "R^2")

matlines(e, type = "c", ylim = c(0,15), col = "black")

matlines(f, type = "c", ylim = c(0,15), col = "black")

matlines(rowMeans(d), type = "l", lwd = 5, col = "yellow")

matlines(rowMeans(e), type = "l", lwd = 5, col = "orange")

matlines(rowMeans(f), type = "l", lwd = 5, col = "red")

legend("bottomleft",legend = c("Random Generations", "Normal Generations", "Continuous Age"), col = c("red", "orange", "yellow"), lwd = "2")
