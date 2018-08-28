library(MASS)
library(arm)

#Only age as a predictor
age_sim <- function(nPeople,cr,nItems=5){

d.f <- setNames(data.frame(mvrnorm(n = nPeople, mu = c(0,0), Sigma = matrix(c(1,cr,cr,1),byrow = T,ncol=2))), c('age','ent'))
  
d.f$age_r <- rescale(x = d.f$age, c(22,70))
d.f$ent_r <- rescale(x = d.f$ent, c(nItems,nItems*5))
  
d.f<-cbind(d.f,setNames(data.frame(apply(d.f[,c(3,4)], 2, round, 0)), c("age_round","ent_round")))
  
d.f$gen <- as.factor(cut(d.f$age_round, c(-Inf, 34, 54, Inf), labels=F))
  
return(
d.f
  )
  
}

age_sim_AIC <- function(nPeople,cr,nItems){
  
    d.f <- age_sim(nPeople = nPeople, cr = cr, nItems = nItems)
    
  return(
    AIC(glm(ent_round ~ ordered(gen), data = d.f)) - 
    AIC(glm(ent_round ~ age_round,    data = d.f))
        )
    
}

age_sim_R2 <- function(nPeople,cr,nItems){
  
  d.f <- age_sim(nPeople = nPeople, cr = cr, nItems = nItems)
  
  return(
    summary(lm(ent_round ~ age_round, data = d.f))$r.squared - 
    summary(lm(ent_round ~ ordered(gen), data = d.f))$r.squared
  )
  
}


age_sim_BIC <- function(nPeople,cr,nItems){
  
  d.f <- age_sim(nPeople = nPeople, cr = cr, nItems = nItems)
  
  return(
    BIC(glm(ent_round ~ ordered(gen), data = d.f)) - 
      BIC(glm(ent_round ~ age_round,    data = d.f))
  )
  
}

age_sim_SIGMA <- function(nPeople,cr,nItems){
  
  d.f <- age_sim(nPeople = nPeople, cr = cr, nItems = nItems)
  
  return(
    sigma(glm(ent_round ~ ordered(gen), data = d.f))-
      sigma(glm(ent_round ~ age_round, data = d.f))
  )
  
}

age_sim_RMSE <- function(nPeople,cr,nItems){
  
  d.f <- age_sim(nPeople = nPeople, cr = cr, nItems = nItems)
  
  return(
    sqrt(mean((glm(ent_round ~ ordered(gen), data = d.f)$residuals^2)))-
    sqrt(mean((glm(ent_round ~ age_round, data = d.f)$residuals^2)))
  )
  
}

age_sim_MAE <- function(nPeople,cr,nItems){
  
  d.f <- age_sim(nPeople = nPeople, cr = cr, nItems = nItems)
  
  return(
    mean(abs(glm(ent_round ~ ordered(gen), data = d.f)$residuals))-
    mean(abs(glm(ent_round ~ age_round, data = d.f)$residuals))
  )
  
}


age_sim_rep_AIC <- function(nPeople, cv, nItems, reps){
  replicate(reps, age_sim_AIC(nPeople, cv, nItems))
}

age_sim_rep_R2 <- function(nPeople, cv, nItems, reps){
  replicate(reps, age_sim_R2(nPeople, cv, nItems))
}

age_sim_rep_BIC <- function(nPeople, cv, nItems, reps){
  replicate(reps, age_sim_BIC(nPeople, cv, nItems))
}

age_sim_rep_SIGMA <- function(nPeople, cv, nItems, reps){
  replicate(reps, age_sim_SIGMA(nPeople, cv, nItems))
}

age_sim_rep_MAE <- function(nPeople, cv, nItems, reps){
  replicate(reps, age_sim_MAE(nPeople, cv, nItems))
}

age_sim_rep_RMSE <- function(nPeople, cv, nItems, reps){
  replicate(reps, age_sim_RMSE(nPeople, cv, nItems))
}

plot(colMeans(do.call(mapply, c(age_sim_rep_AIC, unname(expand.grid(seq(100,500,100),seq(-0.1,-0.9,-0.1),5,10))))))

plot(colMeans(do.call(mapply, c(age_sim_rep_R2, unname(expand.grid(seq(100,500,100),seq(-0.1,-0.9,-0.1),5,50))))))

plot(colMeans(do.call(mapply, c(age_sim_rep_AIC, unname(expand.grid(seq(100,500,100),seq(-0.1,-0.9,-0.1),5,50))))))

plot(colMeans(do.call(mapply, c(age_sim_rep_BIC, unname(expand.grid(seq(100,500,100),seq(-0.1,-0.9,-0.1),c(5,10),50))))))

plot(colMeans(do.call(mapply, c(age_sim_rep_SIGMA, unname(expand.grid(seq(100,500,100),seq(-0.1,-0.9,-0.1),c(5,10),50))))))

plot(colMeans(do.call(mapply, c(age_sim_rep_RMSE, unname(expand.grid(seq(100,500,100),seq(-0.1,-0.9,-0.1),c(5,10),50))))))

plot(colMeans(do.call(mapply, c(age_sim_rep_MAE, unname(expand.grid(seq(100,500,100),seq(-0.1,-0.9,-0.1),c(5,10),50))))))

#Multiple regression example

library(MASS)

age_sim <- function(nPeople,nItems,x){

  cr <- data.frame(expand.grid(c(0.1, 0.3, 0.5, 0.7),
                               c(0.1, 0.3, 0.5, 0.7),
                               c(0.1, 0.3, 0.5, 0.7)))[x,]
  
  d.f <- setNames(data.frame(mvrnorm(n = nPeople, mu = c(0,0,0), Sigma = matrix(c(1,cr$Var1,cr$Var2,
                                                                                  cr$Var1,1,cr$Var3,
                                                                                  cr$Var2,cr$Var3,1),byrow = T,ncol=3),empirical = T)), c('pers','age','ent'))
  
  d.f$age_r <- rescale(x = d.f$age, c(22,70))
  d.f$ent_r <- rescale(x = d.f$ent, c(nItems,nItems*5))
  d.f$pers_r <- rescale(x = d.f$pers, c(nItems,nItems*5))
  
  d.f<-cbind(d.f,setNames(data.frame(apply(d.f[,c(4,5,6)], 2, round, 0)), c("age_round","ent_round","pers_round")))
  
  d.f$gen <- cut(d.f$age_round, c(-Inf, 34, 54, Inf), labels=F)
  
  return(
    data.frame(age_round = d.f$age_round, ent_round = d.f$ent_round, pers_round = d.f$pers_round,gen = d.f$gen)
  )
  
}

age_sim_AIC <- function(nPeople,nItems,x){
  
  d.f <- age_sim(nPeople = nPeople, nItems = nItems, x=x)
  
  return(
    AIC(glm(ent_round ~ ordered(gen) + pers_round, data = d.f)) - 
    AIC(glm(ent_round ~ age_round    + pers_round,    data = d.f))
  )
  
}


age_sim_BIC <- function(nPeople,nItems,x){
  
  d.f <- age_sim(nPeople = nPeople, nItems = nItems,x=x)
  
  return(
    BIC(glm(ent_round ~ ordered(gen) + pers_round, data = d.f)) - 
    BIC(glm(ent_round ~ age_round    + pers_round,    data = d.f))
  )
  
}

age_sim_SIGMA <- function(nPeople,nItems,x){
  
  d.f <- age_sim(nPeople = nPeople, nItems = nItems,x=x)
  
  return(
    sigma(glm(ent_round ~ ordered(gen), data = d.f))-
    sigma(glm(ent_round ~ age_round + pers_round, data = d.f))
  )
  
}

age_sim_RMSE <- function(nPeople,nItems,x){
  
  d.f <- age_sim(nPeople = nPeople, nItems = nItems, x=x)
  
  return(
    sqrt(mean((glm(ent_round ~ ordered(gen) + pers_round, data = d.f)$residuals^2)))-
      sqrt(mean((glm(ent_round ~ age_round + pers_round, data = d.f)$residuals^2)))
  )
  
}

age_sim_MAE <- function(nPeople,nItems,x){
  
  d.f <- age_sim(nPeople = nPeople, nItems = nItems, x=x)
  
  return(
    mean(abs(glm(ent_round ~ ordered(gen), data = d.f)$residuals))-
      mean(abs(glm(ent_round ~ age_round, data = d.f)$residuals))
  )
  
}


age_sim_rep_AIC <- function(nPeople, x, nItems, reps){
  replicate(reps, age_sim_AIC(nPeople, nItems, x))
}

age_sim_rep_BIC <- function(nPeople, x, nItems, reps){
  replicate(reps, age_sim_BIC(nPeople, nItems, x))
}

age_sim_rep_SIGMA <- function(nPeople, x, nItems, reps){
  replicate(reps, age_sim_SIGMA(nPeople, nItems,x))
}

age_sim_rep_MAE <- function(nPeople, x, nItems, reps){
  replicate(reps, age_sim_MAE(nPeople, nItems,x))
}

age_sim_rep_RMSE <- function(nPeople, x, nItems, reps){
  replicate(reps, age_sim_RMSE(nPeople, nItems, x))
}

plot(colMeans(do.call(mapply, c(age_sim_rep_AIC, unname(expand.grid(seq(100,500,100),seq(1,64,1),5,10))))))

plot(colMeans(do.call(mapply, c(age_sim_rep_AIC, unname(expand.grid(seq(100,500,100),seq(1,125,1),5,50))))))

plot(colMeans(do.call(mapply, c(age_sim_rep_BIC, unname(expand.grid(seq(100,500,100),seq(1,125,1),c(5,10),50))))))

plot(colMeans(do.call(mapply, c(age_sim_rep_SIGMA, unname(expand.grid(seq(100,500,100),seq(1,125,1),c(5,10),50))))))

plot(colMeans(do.call(mapply, c(age_sim_rep_RMSE, unname(expand.grid(seq(100,500,100),seq(1,125,1),c(5,10),50))))))

plot(colMeans(do.call(mapply, c(age_sim_rep_MAE, unname(expand.grid(seq(100,500,100),seq(1,125,1),c(5,10),50))))))

