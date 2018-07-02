

# Housekeeping ----------------------------------------------------------------
rm(list= ls())
cat("\014")

library(MASS)
library(MCMCpack)

#source("/Users/malooney/Google Drive/digitalLibrary/*Decesion_Theory/Decision_Theory/scripts_and_functions/agreement_Stats_Sim.R")



# Create Data -----------------------------------------------------------------
# Combination 1, rho=0.99 -----------------------------------------------------

sample.size= 30
inner.sim= 5000
outer.sim= 1000

mu.1 <- c(100, 100)
sig.matrix <- matrix(c(100, 0.99, 0.99, 100), nrow=2, ncol=2)

sim.data.1 <<- mvrnorm(n= sample.size, mu.1, sig.matrix, empirical = T)

S.1 <- cov(sim.data.1)

Result = matrix(0, nrow=inner.sim, ncol=5)
colnames(Result) <- c("X", "Y", "sig2_x", "cov_xy", "sig2_y")
main_results <- matrix(0, nrow=outer.sim, ncol=7)
colnames(main_results) <- c("X", "Y", "sig2_x", "cov_xy", "sig2_y", "rho", 
                            "CCC_Bayes")

for(i in seq_len(outer.sim)){
j=1
while(j<=inner.sim){
  
S.wish <- riwish(sample.size-1, S.1)
U <- runif(1, 0, 1)
sigma.wish.x <- sqrt(S.wish[1,1])
sigma.wish.y <- sqrt(S.wish[1,1])
cov.xy <- S.wish[1,2]

pi.ij <- 1/ ( sigma.wish.x* sigma.wish.y* (1- cov.xy*cov.xy)^(3/2) )

pi.R.rho <- 1/ ( sigma.wish.x* sigma.wish.y* (1-cov.xy*cov.xy) )

cond.pi <- pi.R.rho/ pi.ij

if(U <= cond.pi && cond.pi != "NaN"){
  Result[j, 3:5] = c( S.wish[1,1], S.wish[1,2], S.wish[2,2]) 
  Result[j, c(1,2)]  = mvrnorm(n= 1, mu= c(mean(sim.data.1[,1] ), 
                                           mean(sim.data.1[,2] )), 
                               S.wish/sample.size)
  j<-j+1
  } else {}
}

main_results[i,1:5] <- colMeans(Result[,1:5]) 
main_results[i,6] <- mean(Result[,4]/ (sqrt(Result[,3]* Result[,5])))
main_results[i, 7] <- mean(2* Result[,"cov_xy"]/  ( (Result[,"X"]- Result[,"Y"])^2 + Result[,"sig2_x"]+ Result[,"sig2_y"] ))
}

mean(main_results[,6])
median(main_results[,6])
median(main_results[,7])
mean(main_results[,7])
#hist(100*main_results[,6], freq = FALSE)

write.csv(main_results, "/Users/malooney/Google Drive/digitalLibrary/*Decesion_Theory/Decision_Theory/data/JRP_30_100_100_100_100_99_main_results.csv")


# Create Data -----------------------------------------------------------------
# Combination 1, rho=0.90 -----------------------------------------------------

sample.size= 30
inner.sim= 5000
outer.sim= 1000

mu.1 <- c(100, 100)
sig.matrix <- matrix(c(100, 0.90, 0.90, 100), nrow=2, ncol=2)

sim.data.1 <<- mvrnorm(n= sample.size, mu.1, sig.matrix, empirical = T)

S.1 <- cov(sim.data.1)

Result = matrix(0, nrow=inner.sim, ncol=5)
colnames(Result) <- c("X", "Y", "sig2_x", "cov_xy", "sig2_y")
main_results <- matrix(0, nrow=outer.sim, ncol=7)
colnames(main_results) <- c("X", "Y", "sig2_x", "cov_xy", "sig2_y", "rho", 
                            "CCC_Bayes")

for(i in seq_len(outer.sim)){
  j=1
  while(j<=inner.sim){
    
    S.wish <- riwish(sample.size-1, S.1)
    U <- runif(1, 0, 1)
    sigma.wish.x <- sqrt(S.wish[1,1])
    sigma.wish.y <- sqrt(S.wish[1,1])
    cov.xy <- S.wish[1,2]
    
    pi.ij <- 1/ ( sigma.wish.x* sigma.wish.y* (1- cov.xy*cov.xy)^(3/2) )
    
    pi.R.rho <- 1/ ( sigma.wish.x* sigma.wish.y* (1-cov.xy*cov.xy) )
    
    cond.pi <- pi.R.rho/ pi.ij
    
    if(U <= cond.pi && cond.pi != "NaN"){
      Result[j, 3:5] = c( S.wish[1,1], S.wish[1,2], S.wish[2,2]) 
      Result[j, c(1,2)]  = mvrnorm(n= 1, mu= c(mean(sim.data.1[,1] ), 
                                               mean(sim.data.1[,2] )), 
                                   S.wish/sample.size)
      j<-j+1
    } else {}
  }
  
  main_results[i,1:5] <- colMeans(Result[,1:5]) 
  main_results[i,6] <- mean(Result[,4]/ (sqrt(Result[,3]* Result[,5])))
  main_results[i, 7] <- mean(2* Result[,"cov_xy"]/  ( (Result[,"X"]- Result[,"Y"])^2 + Result[,"sig2_x"]+ Result[,"sig2_y"] ))
}

mean(main_results[,6])
median(main_results[,6])
median(main_results[,7])
mean(main_results[,7])
#hist(100*main_results[,6], freq = FALSE)

write.csv(main_results, "/Users/malooney/Google Drive/digitalLibrary/*Decesion_Theory/Decision_Theory/data/JRP_30_100_100_100_100_90_main_results.csv")


# Create Data -----------------------------------------------------------------
# Combination 1, rho=0.90 -----------------------------------------------------

sample.size= 30
inner.sim= 5000
outer.sim= 1000

mu.1 <- c(100, 100)
sig.matrix <- matrix(c(100, 0.80, 0.80, 100), nrow=2, ncol=2)

sim.data.1 <<- mvrnorm(n= sample.size, mu.1, sig.matrix, empirical = T)

S.1 <- cov(sim.data.1)

Result = matrix(0, nrow=inner.sim, ncol=5)
colnames(Result) <- c("X", "Y", "sig2_x", "cov_xy", "sig2_y")
main_results <- matrix(0, nrow=outer.sim, ncol=7)
colnames(main_results) <- c("X", "Y", "sig2_x", "cov_xy", "sig2_y", "rho", 
                            "CCC_Bayes")

for(i in seq_len(outer.sim)){
  j=1
  while(j<=inner.sim){
    
    S.wish <- riwish(sample.size-1, S.1)
    U <- runif(1, 0, 1)
    sigma.wish.x <- sqrt(S.wish[1,1])
    sigma.wish.y <- sqrt(S.wish[1,1])
    cov.xy <- S.wish[1,2]
    
    pi.ij <- 1/ ( sigma.wish.x* sigma.wish.y* (1- cov.xy*cov.xy)^(3/2) )
    
    pi.R.rho <- 1/ ( sigma.wish.x* sigma.wish.y* (1-cov.xy*cov.xy) )
    
    cond.pi <- pi.R.rho/ pi.ij
    
    if(U <= cond.pi && cond.pi != "NaN"){
      Result[j, 3:5] = c( S.wish[1,1], S.wish[1,2], S.wish[2,2]) 
      Result[j, c(1,2)]  = mvrnorm(n= 1, mu= c(mean(sim.data.1[,1] ), 
                                               mean(sim.data.1[,2] )), 
                                   S.wish/sample.size)
      j<-j+1
    } else {}
  }
  
  main_results[i,1:5] <- colMeans(Result[,1:5]) 
  main_results[i,6] <- mean(Result[,4]/ (sqrt(Result[,3]* Result[,5])))
  main_results[i, 7] <- mean(2* Result[,"cov_xy"]/  ( (Result[,"X"]- Result[,"Y"])^2 + Result[,"sig2_x"]+ Result[,"sig2_y"] ))
}

mean(main_results[,6])
median(main_results[,6])
median(main_results[,7])
mean(main_results[,7])
#hist(100*main_results[,6], freq = FALSE)

write.csv(main_results, "/Users/malooney/Google Drive/digitalLibrary/*Decesion_Theory/Decision_Theory/data/JRP_30_100_100_100_100_80_main_results.csv")


# Create Data -----------------------------------------------------------------
# Combination 1, rho=0.90 -----------------------------------------------------

sample.size= 30
inner.sim= 5000
outer.sim= 1000

mu.1 <- c(100, 100)
sig.matrix <- matrix(c(100, 0.40, 0.40, 100), nrow=2, ncol=2)

sim.data.1 <<- mvrnorm(n= sample.size, mu.1, sig.matrix, empirical = T)

S.1 <- cov(sim.data.1)

Result = matrix(0, nrow=inner.sim, ncol=5)
colnames(Result) <- c("X", "Y", "sig2_x", "cov_xy", "sig2_y")
main_results <- matrix(0, nrow=outer.sim, ncol=7)
colnames(main_results) <- c("X", "Y", "sig2_x", "cov_xy", "sig2_y", "rho", 
                            "CCC_Bayes")

for(i in seq_len(outer.sim)){
  j=1
  while(j<=inner.sim){
    
    S.wish <- riwish(sample.size-1, S.1)
    U <- runif(1, 0, 1)
    sigma.wish.x <- sqrt(S.wish[1,1])
    sigma.wish.y <- sqrt(S.wish[1,1])
    cov.xy <- S.wish[1,2]
    
    pi.ij <- 1/ ( sigma.wish.x* sigma.wish.y* (1- cov.xy*cov.xy)^(3/2) )
    
    pi.R.rho <- 1/ ( sigma.wish.x* sigma.wish.y* (1-cov.xy*cov.xy) )
    
    cond.pi <- pi.R.rho/ pi.ij
    
    if(U <= cond.pi && cond.pi != "NaN"){
      Result[j, 3:5] = c( S.wish[1,1], S.wish[1,2], S.wish[2,2]) 
      Result[j, c(1,2)]  = mvrnorm(n= 1, mu= c(mean(sim.data.1[,1] ), 
                                               mean(sim.data.1[,2] )), 
                                   S.wish/sample.size)
      j<-j+1
    } else {}
  }
  
  main_results[i,1:5] <- colMeans(Result[,1:5]) 
  main_results[i,6] <- mean(Result[,4]/ (sqrt(Result[,3]* Result[,5])))
  main_results[i, 7] <- mean(2* Result[,"cov_xy"]/  ( (Result[,"X"]- Result[,"Y"])^2 + Result[,"sig2_x"]+ Result[,"sig2_y"] ))
}

mean(main_results[,6])
median(main_results[,6])
median(main_results[,7])
mean(main_results[,7])
#hist(100*main_results[,6], freq = FALSE)

write.csv(main_results, "/Users/malooney/Google Drive/digitalLibrary/*Decesion_Theory/Decision_Theory/data/JRP_30_100_100_100_100_40_main_results.csv")








# Create Data -----------------------------------------------------------------
# Combination 1, rho=0.99 -----------------------------------------------------

sample.size= 100
inner.sim= 5000
outer.sim= 1000

mu.1 <- c(100, 100)
sig.matrix <- matrix(c(100, 0.99, 0.99, 100), nrow=2, ncol=2)

sim.data.1 <<- mvrnorm(n= sample.size, mu.1, sig.matrix, empirical = T)

S.1 <- cov(sim.data.1)

Result = matrix(0, nrow=inner.sim, ncol=5)
colnames(Result) <- c("X", "Y", "sig2_x", "cov_xy", "sig2_y")
main_results <- matrix(0, nrow=outer.sim, ncol=7)
colnames(main_results) <- c("X", "Y", "sig2_x", "cov_xy", "sig2_y", "rho", 
                            "CCC_Bayes")

for(i in seq_len(outer.sim)){
  j=1
  while(j<=inner.sim){
    
    S.wish <- riwish(sample.size-1, S.1)
    U <- runif(1, 0, 1)
    sigma.wish.x <- sqrt(S.wish[1,1])
    sigma.wish.y <- sqrt(S.wish[1,1])
    cov.xy <- S.wish[1,2]
    
    pi.ij <- 1/ ( sigma.wish.x* sigma.wish.y* (1- cov.xy*cov.xy)^(3/2) )
    
    pi.R.rho <- 1/ ( sigma.wish.x* sigma.wish.y* (1-cov.xy*cov.xy) )
    
    cond.pi <- pi.R.rho/ pi.ij
    
    if(U <= cond.pi && cond.pi != "NaN"){
      Result[j, 3:5] = c( S.wish[1,1], S.wish[1,2], S.wish[2,2]) 
      Result[j, c(1,2)]  = mvrnorm(n= 1, mu= c(mean(sim.data.1[,1] ), 
                                               mean(sim.data.1[,2] )), 
                                   S.wish/sample.size)
      j<-j+1
    } else {}
  }
  
  main_results[i,1:5] <- colMeans(Result[,1:5]) 
  main_results[i,6] <- mean(Result[,4]/ (sqrt(Result[,3]* Result[,5])))
  main_results[i, 7] <- mean(2* Result[,"cov_xy"]/  ( (Result[,"X"]- Result[,"Y"])^2 + Result[,"sig2_x"]+ Result[,"sig2_y"] ))
}

mean(main_results[,6])
median(main_results[,6])
median(main_results[,7])
mean(main_results[,7])
#hist(100*main_results[,6], freq = FALSE)

write.csv(main_results, "/Users/malooney/Google Drive/digitalLibrary/*Decesion_Theory/Decision_Theory/data/JRP_100_100_100_100_100_99_main_results.csv")


# Create Data -----------------------------------------------------------------
# Combination 1, rho=0.90 -----------------------------------------------------

sample.size= 100
inner.sim= 5000
outer.sim= 1000

mu.1 <- c(100, 100)
sig.matrix <- matrix(c(100, 0.90, 0.90, 100), nrow=2, ncol=2)

sim.data.1 <<- mvrnorm(n= sample.size, mu.1, sig.matrix, empirical = T)

S.1 <- cov(sim.data.1)

Result = matrix(0, nrow=inner.sim, ncol=5)
colnames(Result) <- c("X", "Y", "sig2_x", "cov_xy", "sig2_y")
main_results <- matrix(0, nrow=outer.sim, ncol=7)
colnames(main_results) <- c("X", "Y", "sig2_x", "cov_xy", "sig2_y", "rho", 
                            "CCC_Bayes")

for(i in seq_len(outer.sim)){
  j=1
  while(j<=inner.sim){
    
    S.wish <- riwish(sample.size-1, S.1)
    U <- runif(1, 0, 1)
    sigma.wish.x <- sqrt(S.wish[1,1])
    sigma.wish.y <- sqrt(S.wish[1,1])
    cov.xy <- S.wish[1,2]
    
    pi.ij <- 1/ ( sigma.wish.x* sigma.wish.y* (1- cov.xy*cov.xy)^(3/2) )
    
    pi.R.rho <- 1/ ( sigma.wish.x* sigma.wish.y* (1-cov.xy*cov.xy) )
    
    cond.pi <- pi.R.rho/ pi.ij
    
    if(U <= cond.pi && cond.pi != "NaN"){
      Result[j, 3:5] = c( S.wish[1,1], S.wish[1,2], S.wish[2,2]) 
      Result[j, c(1,2)]  = mvrnorm(n= 1, mu= c(mean(sim.data.1[,1] ), 
                                               mean(sim.data.1[,2] )), 
                                   S.wish/sample.size)
      j<-j+1
    } else {}
  }
  
  main_results[i,1:5] <- colMeans(Result[,1:5]) 
  main_results[i,6] <- mean(Result[,4]/ (sqrt(Result[,3]* Result[,5])))
  main_results[i, 7] <- mean(2* Result[,"cov_xy"]/  ( (Result[,"X"]- Result[,"Y"])^2 + Result[,"sig2_x"]+ Result[,"sig2_y"] ))
}

mean(main_results[,6])
median(main_results[,6])
median(main_results[,7])
mean(main_results[,7])
#hist(100*main_results[,6], freq = FALSE)

write.csv(main_results, "/Users/malooney/Google Drive/digitalLibrary/*Decesion_Theory/Decision_Theory/data/JRP_100_100_100_100_100_90_main_results.csv")


# Create Data -----------------------------------------------------------------
# Combination 1, rho=0.80 -----------------------------------------------------

sample.size= 100
inner.sim= 5000
outer.sim= 1000

mu.1 <- c(100, 100)
sig.matrix <- matrix(c(100, 0.80, 0.80, 100), nrow=2, ncol=2)

sim.data.1 <<- mvrnorm(n= sample.size, mu.1, sig.matrix, empirical = T)

S.1 <- cov(sim.data.1)

Result = matrix(0, nrow=inner.sim, ncol=5)
colnames(Result) <- c("X", "Y", "sig2_x", "cov_xy", "sig2_y")
main_results <- matrix(0, nrow=outer.sim, ncol=7)
colnames(main_results) <- c("X", "Y", "sig2_x", "cov_xy", "sig2_y", "rho", 
                            "CCC_Bayes")

for(i in seq_len(outer.sim)){
  j=1
  while(j<=inner.sim){
    
    S.wish <- riwish(sample.size-1, S.1)
    U <- runif(1, 0, 1)
    sigma.wish.x <- sqrt(S.wish[1,1])
    sigma.wish.y <- sqrt(S.wish[1,1])
    cov.xy <- S.wish[1,2]
    
    pi.ij <- 1/ ( sigma.wish.x* sigma.wish.y* (1- cov.xy*cov.xy)^(3/2) )
    
    pi.R.rho <- 1/ ( sigma.wish.x* sigma.wish.y* (1-cov.xy*cov.xy) )
    
    cond.pi <- pi.R.rho/ pi.ij
    
    if(U <= cond.pi && cond.pi != "NaN"){
      Result[j, 3:5] = c( S.wish[1,1], S.wish[1,2], S.wish[2,2]) 
      Result[j, c(1,2)]  = mvrnorm(n= 1, mu= c(mean(sim.data.1[,1] ), 
                                               mean(sim.data.1[,2] )), 
                                   S.wish/sample.size)
      j<-j+1
    } else {}
  }
  
  main_results[i,1:5] <- colMeans(Result[,1:5]) 
  main_results[i,6] <- mean(Result[,4]/ (sqrt(Result[,3]* Result[,5])))
  main_results[i, 7] <- mean(2* Result[,"cov_xy"]/  ( (Result[,"X"]- Result[,"Y"])^2 + Result[,"sig2_x"]+ Result[,"sig2_y"] ))
}

mean(main_results[,6])
median(main_results[,6])
median(main_results[,7])
mean(main_results[,7])
#hist(100*main_results[,6], freq = FALSE)

write.csv(main_results, "/Users/malooney/Google Drive/digitalLibrary/*Decesion_Theory/Decision_Theory/data/JRP_100_100_100_100_100_80_main_results.csv")


# Create Data -----------------------------------------------------------------
# Combination 1, rho=0.40 -----------------------------------------------------

sample.size= 100
inner.sim= 5000
outer.sim= 1000

mu.1 <- c(100, 100)
sig.matrix <- matrix(c(100, 0.40, 0.40, 100), nrow=2, ncol=2)

sim.data.1 <<- mvrnorm(n= sample.size, mu.1, sig.matrix, empirical = T)

S.1 <- cov(sim.data.1)

Result = matrix(0, nrow=inner.sim, ncol=5)
colnames(Result) <- c("X", "Y", "sig2_x", "cov_xy", "sig2_y")
main_results <- matrix(0, nrow=outer.sim, ncol=7)
colnames(main_results) <- c("X", "Y", "sig2_x", "cov_xy", "sig2_y", "rho", 
                            "CCC_Bayes")

for(i in seq_len(outer.sim)){
  j=1
  while(j<=inner.sim){
    
    S.wish <- riwish(sample.size-1, S.1)
    U <- runif(1, 0, 1)
    sigma.wish.x <- sqrt(S.wish[1,1])
    sigma.wish.y <- sqrt(S.wish[1,1])
    cov.xy <- S.wish[1,2]
    
    pi.ij <- 1/ ( sigma.wish.x* sigma.wish.y* (1- cov.xy*cov.xy)^(3/2) )
    
    pi.R.rho <- 1/ ( sigma.wish.x* sigma.wish.y* (1-cov.xy*cov.xy) )
    
    cond.pi <- pi.R.rho/ pi.ij
    
    if(U <= cond.pi && cond.pi != "NaN"){
      Result[j, 3:5] = c( S.wish[1,1], S.wish[1,2], S.wish[2,2]) 
      Result[j, c(1,2)]  = mvrnorm(n= 1, mu= c(mean(sim.data.1[,1] ), 
                                               mean(sim.data.1[,2] )), 
                                   S.wish/sample.size)
      j<-j+1
    } else {}
  }
  
  main_results[i,1:5] <- colMeans(Result[,1:5]) 
  main_results[i,6] <- mean(Result[,4]/ (sqrt(Result[,3]* Result[,5])))
  main_results[i, 7] <- mean(2* Result[,"cov_xy"]/  ( (Result[,"X"]- Result[,"Y"])^2 + Result[,"sig2_x"]+ Result[,"sig2_y"] ))
}

mean(main_results[,6])
median(main_results[,6])
median(main_results[,7])
mean(main_results[,7])
#hist(100*main_results[,6], freq = FALSE)

write.csv(main_results, "/Users/malooney/Google Drive/digitalLibrary/*Decesion_Theory/Decision_Theory/data/JRP_100_100_100_100_100_40_main_results.csv")
















# Create Data -----------------------------------------------------------------
# Combination 1, rho=0.99 -----------------------------------------------------

sample.size= 30
inner.sim= 5000
outer.sim= 1000

mu.1 <- c(100, 100)
sig.matrix <- matrix(c(100, 0.99, 0.99, 125), nrow=2, ncol=2)

sim.data.1 <<- mvrnorm(n= sample.size, mu.1, sig.matrix, empirical = T)

S.1 <- cov(sim.data.1)

Result = matrix(0, nrow=inner.sim, ncol=5)
colnames(Result) <- c("X", "Y", "sig2_x", "cov_xy", "sig2_y")
main_results <- matrix(0, nrow=outer.sim, ncol=7)
colnames(main_results) <- c("X", "Y", "sig2_x", "cov_xy", "sig2_y", "rho", 
                            "CCC_Bayes")

for(i in seq_len(outer.sim)){
  j=1
  while(j<=inner.sim){
    
    S.wish <- riwish(sample.size-1, S.1)
    U <- runif(1, 0, 1)
    sigma.wish.x <- sqrt(S.wish[1,1])
    sigma.wish.y <- sqrt(S.wish[1,1])
    cov.xy <- S.wish[1,2]
    
    pi.ij <- 1/ ( sigma.wish.x* sigma.wish.y* (1- cov.xy*cov.xy)^(3/2) )
    
    pi.R.rho <- 1/ ( sigma.wish.x* sigma.wish.y* (1-cov.xy*cov.xy) )
    
    cond.pi <- pi.R.rho/ pi.ij
    
    if(U <= cond.pi && cond.pi != "NaN"){
      Result[j, 3:5] = c( S.wish[1,1], S.wish[1,2], S.wish[2,2]) 
      Result[j, c(1,2)]  = mvrnorm(n= 1, mu= c(mean(sim.data.1[,1] ), 
                                               mean(sim.data.1[,2] )), 
                                   S.wish/sample.size)
      j<-j+1
    } else {}
  }
  
  main_results[i,1:5] <- colMeans(Result[,1:5]) 
  main_results[i,6] <- mean(Result[,4]/ (sqrt(Result[,3]* Result[,5])))
  main_results[i, 7] <- mean(2* Result[,"cov_xy"]/  ( (Result[,"X"]- Result[,"Y"])^2 + Result[,"sig2_x"]+ Result[,"sig2_y"] ))
}

mean(main_results[,6])
median(main_results[,6])
median(main_results[,7])
mean(main_results[,7])
#hist(100*main_results[,6], freq = FALSE)

write.csv(main_results, "/Users/malooney/Google Drive/digitalLibrary/*Decesion_Theory/Decision_Theory/data/JRP_30_100_100_100_125_99_main_results.csv")


# Create Data -----------------------------------------------------------------
# Combination 1, rho=0.90 -----------------------------------------------------

sample.size= 30
inner.sim= 5000
outer.sim= 1000

mu.1 <- c(100, 100)
sig.matrix <- matrix(c(100, 0.90, 0.90, 125), nrow=2, ncol=2)

sim.data.1 <<- mvrnorm(n= sample.size, mu.1, sig.matrix, empirical = T)

S.1 <- cov(sim.data.1)

Result = matrix(0, nrow=inner.sim, ncol=5)
colnames(Result) <- c("X", "Y", "sig2_x", "cov_xy", "sig2_y")
main_results <- matrix(0, nrow=outer.sim, ncol=7)
colnames(main_results) <- c("X", "Y", "sig2_x", "cov_xy", "sig2_y", "rho", 
                            "CCC_Bayes")

for(i in seq_len(outer.sim)){
  j=1
  while(j<=inner.sim){
    
    S.wish <- riwish(sample.size-1, S.1)
    U <- runif(1, 0, 1)
    sigma.wish.x <- sqrt(S.wish[1,1])
    sigma.wish.y <- sqrt(S.wish[1,1])
    cov.xy <- S.wish[1,2]
    
    pi.ij <- 1/ ( sigma.wish.x* sigma.wish.y* (1- cov.xy*cov.xy)^(3/2) )
    
    pi.R.rho <- 1/ ( sigma.wish.x* sigma.wish.y* (1-cov.xy*cov.xy) )
    
    cond.pi <- pi.R.rho/ pi.ij
    
    if(U <= cond.pi && cond.pi != "NaN"){
      Result[j, 3:5] = c( S.wish[1,1], S.wish[1,2], S.wish[2,2]) 
      Result[j, c(1,2)]  = mvrnorm(n= 1, mu= c(mean(sim.data.1[,1] ), 
                                               mean(sim.data.1[,2] )), 
                                   S.wish/sample.size)
      j<-j+1
    } else {}
  }
  
  main_results[i,1:5] <- colMeans(Result[,1:5]) 
  main_results[i,6] <- mean(Result[,4]/ (sqrt(Result[,3]* Result[,5])))
  main_results[i, 7] <- mean(2* Result[,"cov_xy"]/  ( (Result[,"X"]- Result[,"Y"])^2 + Result[,"sig2_x"]+ Result[,"sig2_y"] ))
}

mean(main_results[,6])
median(main_results[,6])
median(main_results[,7])
mean(main_results[,7])
#hist(100*main_results[,6], freq = FALSE)

write.csv(main_results, "/Users/malooney/Google Drive/digitalLibrary/*Decesion_Theory/Decision_Theory/data/JRP_30_100_100_100_125_90_main_results.csv")


# Create Data -----------------------------------------------------------------
# Combination 1, rho=0.80 -----------------------------------------------------

sample.size= 30
inner.sim= 5000
outer.sim= 1000

mu.1 <- c(100, 100)
sig.matrix <- matrix(c(100, 0.80, 0.80, 125), nrow=2, ncol=2)

sim.data.1 <<- mvrnorm(n= sample.size, mu.1, sig.matrix, empirical = T)

S.1 <- cov(sim.data.1)

Result = matrix(0, nrow=inner.sim, ncol=5)
colnames(Result) <- c("X", "Y", "sig2_x", "cov_xy", "sig2_y")
main_results <- matrix(0, nrow=outer.sim, ncol=7)
colnames(main_results) <- c("X", "Y", "sig2_x", "cov_xy", "sig2_y", "rho", 
                            "CCC_Bayes")

for(i in seq_len(outer.sim)){
  j=1
  while(j<=inner.sim){
    
    S.wish <- riwish(sample.size-1, S.1)
    U <- runif(1, 0, 1)
    sigma.wish.x <- sqrt(S.wish[1,1])
    sigma.wish.y <- sqrt(S.wish[1,1])
    cov.xy <- S.wish[1,2]
    
    pi.ij <- 1/ ( sigma.wish.x* sigma.wish.y* (1- cov.xy*cov.xy)^(3/2) )
    
    pi.R.rho <- 1/ ( sigma.wish.x* sigma.wish.y* (1-cov.xy*cov.xy) )
    
    cond.pi <- pi.R.rho/ pi.ij
    
    if(U <= cond.pi && cond.pi != "NaN"){
      Result[j, 3:5] = c( S.wish[1,1], S.wish[1,2], S.wish[2,2]) 
      Result[j, c(1,2)]  = mvrnorm(n= 1, mu= c(mean(sim.data.1[,1] ), 
                                               mean(sim.data.1[,2] )), 
                                   S.wish/sample.size)
      j<-j+1
    } else {}
  }
  
  main_results[i,1:5] <- colMeans(Result[,1:5]) 
  main_results[i,6] <- mean(Result[,4]/ (sqrt(Result[,3]* Result[,5])))
  main_results[i, 7] <- mean(2* Result[,"cov_xy"]/  ( (Result[,"X"]- Result[,"Y"])^2 + Result[,"sig2_x"]+ Result[,"sig2_y"] ))
}

mean(main_results[,6])
median(main_results[,6])
median(main_results[,7])
mean(main_results[,7])
#hist(100*main_results[,6], freq = FALSE)

write.csv(main_results, "/Users/malooney/Google Drive/digitalLibrary/*Decesion_Theory/Decision_Theory/data/JRP_30_100_100_100_125_80_main_results.csv")


# Create Data -----------------------------------------------------------------
# Combination 1, rho=0.40 -----------------------------------------------------

sample.size= 30
inner.sim= 5000
outer.sim= 1000

mu.1 <- c(100, 100)
sig.matrix <- matrix(c(100, 0.40, 0.40, 125), nrow=2, ncol=2)

sim.data.1 <<- mvrnorm(n= sample.size, mu.1, sig.matrix, empirical = T)

S.1 <- cov(sim.data.1)

Result = matrix(0, nrow=inner.sim, ncol=5)
colnames(Result) <- c("X", "Y", "sig2_x", "cov_xy", "sig2_y")
main_results <- matrix(0, nrow=outer.sim, ncol=7)
colnames(main_results) <- c("X", "Y", "sig2_x", "cov_xy", "sig2_y", "rho", 
                            "CCC_Bayes")

for(i in seq_len(outer.sim)){
  j=1
  while(j<=inner.sim){
    
    S.wish <- riwish(sample.size-1, S.1)
    U <- runif(1, 0, 1)
    sigma.wish.x <- sqrt(S.wish[1,1])
    sigma.wish.y <- sqrt(S.wish[1,1])
    cov.xy <- S.wish[1,2]
    
    pi.ij <- 1/ ( sigma.wish.x* sigma.wish.y* (1- cov.xy*cov.xy)^(3/2) )
    
    pi.R.rho <- 1/ ( sigma.wish.x* sigma.wish.y* (1-cov.xy*cov.xy) )
    
    cond.pi <- pi.R.rho/ pi.ij
    
    if(U <= cond.pi && cond.pi != "NaN"){
      Result[j, 3:5] = c( S.wish[1,1], S.wish[1,2], S.wish[2,2]) 
      Result[j, c(1,2)]  = mvrnorm(n= 1, mu= c(mean(sim.data.1[,1] ), 
                                               mean(sim.data.1[,2] )), 
                                   S.wish/sample.size)
      j<-j+1
    } else {}
  }
  
  main_results[i,1:5] <- colMeans(Result[,1:5]) 
  main_results[i,6] <- mean(Result[,4]/ (sqrt(Result[,3]* Result[,5])))
  main_results[i, 7] <- mean(2* Result[,"cov_xy"]/  ( (Result[,"X"]- Result[,"Y"])^2 + Result[,"sig2_x"]+ Result[,"sig2_y"] ))
}

mean(main_results[,6])
median(main_results[,6])
median(main_results[,7])
mean(main_results[,7])
#hist(100*main_results[,6], freq = FALSE)

write.csv(main_results, "/Users/malooney/Google Drive/digitalLibrary/*Decesion_Theory/Decision_Theory/data/JRP_30_100_100_100_125_40_main_results.csv")















# Create Data -----------------------------------------------------------------
# Combination 1, rho=0.99 -----------------------------------------------------

sample.size= 100
inner.sim= 5000
outer.sim= 1000

mu.1 <- c(100, 100)
sig.matrix <- matrix(c(100, 0.99, 0.99, 125), nrow=2, ncol=2)

sim.data.1 <<- mvrnorm(n= sample.size, mu.1, sig.matrix, empirical = T)

S.1 <- cov(sim.data.1)

Result = matrix(0, nrow=inner.sim, ncol=5)
colnames(Result) <- c("X", "Y", "sig2_x", "cov_xy", "sig2_y")
main_results <- matrix(0, nrow=outer.sim, ncol=7)
colnames(main_results) <- c("X", "Y", "sig2_x", "cov_xy", "sig2_y", "rho", 
                            "CCC_Bayes")

for(i in seq_len(outer.sim)){
  j=1
  while(j<=inner.sim){
    
    S.wish <- riwish(sample.size-1, S.1)
    U <- runif(1, 0, 1)
    sigma.wish.x <- sqrt(S.wish[1,1])
    sigma.wish.y <- sqrt(S.wish[1,1])
    cov.xy <- S.wish[1,2]
    
    pi.ij <- 1/ ( sigma.wish.x* sigma.wish.y* (1- cov.xy*cov.xy)^(3/2) )
    
    pi.R.rho <- 1/ ( sigma.wish.x* sigma.wish.y* (1-cov.xy*cov.xy) )
    
    cond.pi <- pi.R.rho/ pi.ij
    
    if(U <= cond.pi && cond.pi != "NaN"){
      Result[j, 3:5] = c( S.wish[1,1], S.wish[1,2], S.wish[2,2]) 
      Result[j, c(1,2)]  = mvrnorm(n= 1, mu= c(mean(sim.data.1[,1] ), 
                                               mean(sim.data.1[,2] )), 
                                   S.wish/sample.size)
      j<-j+1
    } else {}
  }
  
  main_results[i,1:5] <- colMeans(Result[,1:5]) 
  main_results[i,6] <- mean(Result[,4]/ (sqrt(Result[,3]* Result[,5])))
  main_results[i, 7] <- mean(2* Result[,"cov_xy"]/  ( (Result[,"X"]- Result[,"Y"])^2 + Result[,"sig2_x"]+ Result[,"sig2_y"] ))
}

mean(main_results[,6])
median(main_results[,6])
median(main_results[,7])
mean(main_results[,7])
#hist(100*main_results[,6], freq = FALSE)

write.csv(main_results, "/Users/malooney/Google Drive/digitalLibrary/*Decesion_Theory/Decision_Theory/data/JRP_100_100_100_100_125_99_main_results.csv")


# Create Data -----------------------------------------------------------------
# Combination 1, rho=0.90 -----------------------------------------------------

sample.size= 100
inner.sim= 5000
outer.sim= 1000

mu.1 <- c(100, 100)
sig.matrix <- matrix(c(100, 0.90, 0.90, 125), nrow=2, ncol=2)

sim.data.1 <<- mvrnorm(n= sample.size, mu.1, sig.matrix, empirical = T)

S.1 <- cov(sim.data.1)

Result = matrix(0, nrow=inner.sim, ncol=5)
colnames(Result) <- c("X", "Y", "sig2_x", "cov_xy", "sig2_y")
main_results <- matrix(0, nrow=outer.sim, ncol=7)
colnames(main_results) <- c("X", "Y", "sig2_x", "cov_xy", "sig2_y", "rho", 
                            "CCC_Bayes")

for(i in seq_len(outer.sim)){
  j=1
  while(j<=inner.sim){
    
    S.wish <- riwish(sample.size-1, S.1)
    U <- runif(1, 0, 1)
    sigma.wish.x <- sqrt(S.wish[1,1])
    sigma.wish.y <- sqrt(S.wish[1,1])
    cov.xy <- S.wish[1,2]
    
    pi.ij <- 1/ ( sigma.wish.x* sigma.wish.y* (1- cov.xy*cov.xy)^(3/2) )
    
    pi.R.rho <- 1/ ( sigma.wish.x* sigma.wish.y* (1-cov.xy*cov.xy) )
    
    cond.pi <- pi.R.rho/ pi.ij
    
    if(U <= cond.pi && cond.pi != "NaN"){
      Result[j, 3:5] = c( S.wish[1,1], S.wish[1,2], S.wish[2,2]) 
      Result[j, c(1,2)]  = mvrnorm(n= 1, mu= c(mean(sim.data.1[,1] ), 
                                               mean(sim.data.1[,2] )), 
                                   S.wish/sample.size)
      j<-j+1
    } else {}
  }
  
  main_results[i,1:5] <- colMeans(Result[,1:5]) 
  main_results[i,6] <- mean(Result[,4]/ (sqrt(Result[,3]* Result[,5])))
  main_results[i, 7] <- mean(2* Result[,"cov_xy"]/  ( (Result[,"X"]- Result[,"Y"])^2 + Result[,"sig2_x"]+ Result[,"sig2_y"] ))
}

mean(main_results[,6])
median(main_results[,6])
median(main_results[,7])
mean(main_results[,7])
#hist(100*main_results[,6], freq = FALSE)

write.csv(main_results, "/Users/malooney/Google Drive/digitalLibrary/*Decesion_Theory/Decision_Theory/data/JRP_100_100_100_100_125_90_main_results.csv")


# Create Data -----------------------------------------------------------------
# Combination 1, rho=0.80 -----------------------------------------------------

sample.size= 100
inner.sim= 5000
outer.sim= 1000

mu.1 <- c(100, 100)
sig.matrix <- matrix(c(100, 0.80, 0.80, 125), nrow=2, ncol=2)

sim.data.1 <<- mvrnorm(n= sample.size, mu.1, sig.matrix, empirical = T)

S.1 <- cov(sim.data.1)

Result = matrix(0, nrow=inner.sim, ncol=5)
colnames(Result) <- c("X", "Y", "sig2_x", "cov_xy", "sig2_y")
main_results <- matrix(0, nrow=outer.sim, ncol=7)
colnames(main_results) <- c("X", "Y", "sig2_x", "cov_xy", "sig2_y", "rho", 
                            "CCC_Bayes")

for(i in seq_len(outer.sim)){
  j=1
  while(j<=inner.sim){
    
    S.wish <- riwish(sample.size-1, S.1)
    U <- runif(1, 0, 1)
    sigma.wish.x <- sqrt(S.wish[1,1])
    sigma.wish.y <- sqrt(S.wish[1,1])
    cov.xy <- S.wish[1,2]
    
    pi.ij <- 1/ ( sigma.wish.x* sigma.wish.y* (1- cov.xy*cov.xy)^(3/2) )
    
    pi.R.rho <- 1/ ( sigma.wish.x* sigma.wish.y* (1-cov.xy*cov.xy) )
    
    cond.pi <- pi.R.rho/ pi.ij
    
    if(U <= cond.pi && cond.pi != "NaN"){
      Result[j, 3:5] = c( S.wish[1,1], S.wish[1,2], S.wish[2,2]) 
      Result[j, c(1,2)]  = mvrnorm(n= 1, mu= c(mean(sim.data.1[,1] ), 
                                               mean(sim.data.1[,2] )), 
                                   S.wish/sample.size)
      j<-j+1
    } else {}
  }
  
  main_results[i,1:5] <- colMeans(Result[,1:5]) 
  main_results[i,6] <- mean(Result[,4]/ (sqrt(Result[,3]* Result[,5])))
  main_results[i, 7] <- mean(2* Result[,"cov_xy"]/  ( (Result[,"X"]- Result[,"Y"])^2 + Result[,"sig2_x"]+ Result[,"sig2_y"] ))
}

mean(main_results[,6])
median(main_results[,6])
median(main_results[,7])
mean(main_results[,7])
#hist(100*main_results[,6], freq = FALSE)

write.csv(main_results, "/Users/malooney/Google Drive/digitalLibrary/*Decesion_Theory/Decision_Theory/data/JRP_100_100_100_100_125_80_main_results.csv")


# Create Data -----------------------------------------------------------------
# Combination 1, rho=0.40 -----------------------------------------------------

sample.size= 100
inner.sim= 5000
outer.sim= 1000

mu.1 <- c(100, 100)
sig.matrix <- matrix(c(100, 0.40, 0.40, 125), nrow=2, ncol=2)

sim.data.1 <<- mvrnorm(n= sample.size, mu.1, sig.matrix, empirical = T)

S.1 <- cov(sim.data.1)

Result = matrix(0, nrow=inner.sim, ncol=5)
colnames(Result) <- c("X", "Y", "sig2_x", "cov_xy", "sig2_y")
main_results <- matrix(0, nrow=outer.sim, ncol=7)
colnames(main_results) <- c("X", "Y", "sig2_x", "cov_xy", "sig2_y", "rho", 
                            "CCC_Bayes")

for(i in seq_len(outer.sim)){
  j=1
  while(j<=inner.sim){
    
    S.wish <- riwish(sample.size-1, S.1)
    U <- runif(1, 0, 1)
    sigma.wish.x <- sqrt(S.wish[1,1])
    sigma.wish.y <- sqrt(S.wish[1,1])
    cov.xy <- S.wish[1,2]
    
    pi.ij <- 1/ ( sigma.wish.x* sigma.wish.y* (1- cov.xy*cov.xy)^(3/2) )
    
    pi.R.rho <- 1/ ( sigma.wish.x* sigma.wish.y* (1-cov.xy*cov.xy) )
    
    cond.pi <- pi.R.rho/ pi.ij
    
    if(U <= cond.pi && cond.pi != "NaN"){
      Result[j, 3:5] = c( S.wish[1,1], S.wish[1,2], S.wish[2,2]) 
      Result[j, c(1,2)]  = mvrnorm(n= 1, mu= c(mean(sim.data.1[,1] ), 
                                               mean(sim.data.1[,2] )), 
                                   S.wish/sample.size)
      j<-j+1
    } else {}
  }
  
  main_results[i,1:5] <- colMeans(Result[,1:5]) 
  main_results[i,6] <- mean(Result[,4]/ (sqrt(Result[,3]* Result[,5])))
  main_results[i, 7] <- mean(2* Result[,"cov_xy"]/  ( (Result[,"X"]- Result[,"Y"])^2 + Result[,"sig2_x"]+ Result[,"sig2_y"] ))
}

mean(main_results[,6])
median(main_results[,6])
median(main_results[,7])
mean(main_results[,7])
#hist(100*main_results[,6], freq = FALSE)

write.csv(main_results, "/Users/malooney/Google Drive/digitalLibrary/*Decesion_Theory/Decision_Theory/data/JRP_100_100_100_100_125_40_main_results.csv")

















# Create Data -----------------------------------------------------------------
# Combination 1, rho=0.99 -----------------------------------------------------

sample.size= 30
inner.sim= 5000
outer.sim= 1000

mu.1 <- c(100, 105)
sig.matrix <- matrix(c(100, 0.99, 0.99, 125), nrow=2, ncol=2)

sim.data.1 <<- mvrnorm(n= sample.size, mu.1, sig.matrix, empirical = T)

S.1 <- cov(sim.data.1)

Result = matrix(0, nrow=inner.sim, ncol=5)
colnames(Result) <- c("X", "Y", "sig2_x", "cov_xy", "sig2_y")
main_results <- matrix(0, nrow=outer.sim, ncol=7)
colnames(main_results) <- c("X", "Y", "sig2_x", "cov_xy", "sig2_y", "rho", 
                            "CCC_Bayes")

for(i in seq_len(outer.sim)){
  j=1
  while(j<=inner.sim){
    
    S.wish <- riwish(sample.size-1, S.1)
    U <- runif(1, 0, 1)
    sigma.wish.x <- sqrt(S.wish[1,1])
    sigma.wish.y <- sqrt(S.wish[1,1])
    cov.xy <- S.wish[1,2]
    
    pi.ij <- 1/ ( sigma.wish.x* sigma.wish.y* (1- cov.xy*cov.xy)^(3/2) )
    
    pi.R.rho <- 1/ ( sigma.wish.x* sigma.wish.y* (1-cov.xy*cov.xy) )
    
    cond.pi <- pi.R.rho/ pi.ij
    
    if(U <= cond.pi && cond.pi != "NaN"){
      Result[j, 3:5] = c( S.wish[1,1], S.wish[1,2], S.wish[2,2]) 
      Result[j, c(1,2)]  = mvrnorm(n= 1, mu= c(mean(sim.data.1[,1] ), 
                                               mean(sim.data.1[,2] )), 
                                   S.wish/sample.size)
      j<-j+1
    } else {}
  }
  
  main_results[i,1:5] <- colMeans(Result[,1:5]) 
  main_results[i,6] <- mean(Result[,4]/ (sqrt(Result[,3]* Result[,5])))
  main_results[i, 7] <- mean(2* Result[,"cov_xy"]/  ( (Result[,"X"]- Result[,"Y"])^2 + Result[,"sig2_x"]+ Result[,"sig2_y"] ))
}

mean(main_results[,6])
median(main_results[,6])
median(main_results[,7])
mean(main_results[,7])
#hist(100*main_results[,6], freq = FALSE)

write.csv(main_results, "/Users/malooney/Google Drive/digitalLibrary/*Decesion_Theory/Decision_Theory/data/JRP_30_100_105_100_125_99_main_results.csv")


# Create Data -----------------------------------------------------------------
# Combination 1, rho=0.90 -----------------------------------------------------

sample.size= 30
inner.sim= 5000
outer.sim= 1000

mu.1 <- c(100, 105)
sig.matrix <- matrix(c(100, 0.90, 0.90, 125), nrow=2, ncol=2)

sim.data.1 <<- mvrnorm(n= sample.size, mu.1, sig.matrix, empirical = T)

S.1 <- cov(sim.data.1)

Result = matrix(0, nrow=inner.sim, ncol=5)
colnames(Result) <- c("X", "Y", "sig2_x", "cov_xy", "sig2_y")
main_results <- matrix(0, nrow=outer.sim, ncol=7)
colnames(main_results) <- c("X", "Y", "sig2_x", "cov_xy", "sig2_y", "rho", 
                            "CCC_Bayes")

for(i in seq_len(outer.sim)){
  j=1
  while(j<=inner.sim){
    
    S.wish <- riwish(sample.size-1, S.1)
    U <- runif(1, 0, 1)
    sigma.wish.x <- sqrt(S.wish[1,1])
    sigma.wish.y <- sqrt(S.wish[1,1])
    cov.xy <- S.wish[1,2]
    
    pi.ij <- 1/ ( sigma.wish.x* sigma.wish.y* (1- cov.xy*cov.xy)^(3/2) )
    
    pi.R.rho <- 1/ ( sigma.wish.x* sigma.wish.y* (1-cov.xy*cov.xy) )
    
    cond.pi <- pi.R.rho/ pi.ij
    
    if(U <= cond.pi && cond.pi != "NaN"){
      Result[j, 3:5] = c( S.wish[1,1], S.wish[1,2], S.wish[2,2]) 
      Result[j, c(1,2)]  = mvrnorm(n= 1, mu= c(mean(sim.data.1[,1] ), 
                                               mean(sim.data.1[,2] )), 
                                   S.wish/sample.size)
      j<-j+1
    } else {}
  }
  
  main_results[i,1:5] <- colMeans(Result[,1:5]) 
  main_results[i,6] <- mean(Result[,4]/ (sqrt(Result[,3]* Result[,5])))
  main_results[i, 7] <- mean(2* Result[,"cov_xy"]/  ( (Result[,"X"]- Result[,"Y"])^2 + Result[,"sig2_x"]+ Result[,"sig2_y"] ))
}

mean(main_results[,6])
median(main_results[,6])
median(main_results[,7])
mean(main_results[,7])
#hist(100*main_results[,6], freq = FALSE)

write.csv(main_results, "/Users/malooney/Google Drive/digitalLibrary/*Decesion_Theory/Decision_Theory/data/JRP_30_100_105_100_125_90_main_results.csv")


# Create Data -----------------------------------------------------------------
# Combination 1, rho=0.80 -----------------------------------------------------

sample.size= 30
inner.sim= 5000
outer.sim= 1000

mu.1 <- c(100, 105)
sig.matrix <- matrix(c(100, 0.80, 0.80, 125), nrow=2, ncol=2)

sim.data.1 <<- mvrnorm(n= sample.size, mu.1, sig.matrix, empirical = T)

S.1 <- cov(sim.data.1)

Result = matrix(0, nrow=inner.sim, ncol=5)
colnames(Result) <- c("X", "Y", "sig2_x", "cov_xy", "sig2_y")
main_results <- matrix(0, nrow=outer.sim, ncol=7)
colnames(main_results) <- c("X", "Y", "sig2_x", "cov_xy", "sig2_y", "rho", 
                            "CCC_Bayes")

for(i in seq_len(outer.sim)){
  j=1
  while(j<=inner.sim){
    
    S.wish <- riwish(sample.size-1, S.1)
    U <- runif(1, 0, 1)
    sigma.wish.x <- sqrt(S.wish[1,1])
    sigma.wish.y <- sqrt(S.wish[1,1])
    cov.xy <- S.wish[1,2]
    
    pi.ij <- 1/ ( sigma.wish.x* sigma.wish.y* (1- cov.xy*cov.xy)^(3/2) )
    
    pi.R.rho <- 1/ ( sigma.wish.x* sigma.wish.y* (1-cov.xy*cov.xy) )
    
    cond.pi <- pi.R.rho/ pi.ij
    
    if(U <= cond.pi && cond.pi != "NaN"){
      Result[j, 3:5] = c( S.wish[1,1], S.wish[1,2], S.wish[2,2]) 
      Result[j, c(1,2)]  = mvrnorm(n= 1, mu= c(mean(sim.data.1[,1] ), 
                                               mean(sim.data.1[,2] )), 
                                   S.wish/sample.size)
      j<-j+1
    } else {}
  }
  
  main_results[i,1:5] <- colMeans(Result[,1:5]) 
  main_results[i,6] <- mean(Result[,4]/ (sqrt(Result[,3]* Result[,5])))
  main_results[i, 7] <- mean(2* Result[,"cov_xy"]/  ( (Result[,"X"]- Result[,"Y"])^2 + Result[,"sig2_x"]+ Result[,"sig2_y"] ))
}

mean(main_results[,6])
median(main_results[,6])
median(main_results[,7])
mean(main_results[,7])
#hist(100*main_results[,6], freq = FALSE)

write.csv(main_results, "/Users/malooney/Google Drive/digitalLibrary/*Decesion_Theory/Decision_Theory/data/JRP_30_100_105_100_125_80_main_results.csv")


# Create Data -----------------------------------------------------------------
# Combination 1, rho=0.40 -----------------------------------------------------

sample.size= 30
inner.sim= 5000
outer.sim= 1000

mu.1 <- c(100, 105)
sig.matrix <- matrix(c(100, 0.40, 0.40, 125), nrow=2, ncol=2)

sim.data.1 <<- mvrnorm(n= sample.size, mu.1, sig.matrix, empirical = T)

S.1 <- cov(sim.data.1)

Result = matrix(0, nrow=inner.sim, ncol=5)
colnames(Result) <- c("X", "Y", "sig2_x", "cov_xy", "sig2_y")
main_results <- matrix(0, nrow=outer.sim, ncol=7)
colnames(main_results) <- c("X", "Y", "sig2_x", "cov_xy", "sig2_y", "rho", 
                            "CCC_Bayes")

for(i in seq_len(outer.sim)){
  j=1
  while(j<=inner.sim){
    
    S.wish <- riwish(sample.size-1, S.1)
    U <- runif(1, 0, 1)
    sigma.wish.x <- sqrt(S.wish[1,1])
    sigma.wish.y <- sqrt(S.wish[1,1])
    cov.xy <- S.wish[1,2]
    
    pi.ij <- 1/ ( sigma.wish.x* sigma.wish.y* (1- cov.xy*cov.xy)^(3/2) )
    
    pi.R.rho <- 1/ ( sigma.wish.x* sigma.wish.y* (1-cov.xy*cov.xy) )
    
    cond.pi <- pi.R.rho/ pi.ij
    
    if(U <= cond.pi && cond.pi != "NaN"){
      Result[j, 3:5] = c( S.wish[1,1], S.wish[1,2], S.wish[2,2]) 
      Result[j, c(1,2)]  = mvrnorm(n= 1, mu= c(mean(sim.data.1[,1] ), 
                                               mean(sim.data.1[,2] )), 
                                   S.wish/sample.size)
      j<-j+1
    } else {}
  }
  
  main_results[i,1:5] <- colMeans(Result[,1:5]) 
  main_results[i,6] <- mean(Result[,4]/ (sqrt(Result[,3]* Result[,5])))
  main_results[i, 7] <- mean(2* Result[,"cov_xy"]/  ( (Result[,"X"]- Result[,"Y"])^2 + Result[,"sig2_x"]+ Result[,"sig2_y"] ))
}

mean(main_results[,6])
median(main_results[,6])
median(main_results[,7])
mean(main_results[,7])
#hist(100*main_results[,6], freq = FALSE)

write.csv(main_results, "/Users/malooney/Google Drive/digitalLibrary/*Decesion_Theory/Decision_Theory/data/JRP_30_100_105_100_125_40_main_results.csv")















# Create Data -----------------------------------------------------------------
# Combination 1, rho=0.99 -----------------------------------------------------

sample.size= 100
inner.sim= 5000
outer.sim= 1000

mu.1 <- c(100, 105)
sig.matrix <- matrix(c(100, 0.99, 0.99, 125), nrow=2, ncol=2)

sim.data.1 <<- mvrnorm(n= sample.size, mu.1, sig.matrix, empirical = T)

S.1 <- cov(sim.data.1)

Result = matrix(0, nrow=inner.sim, ncol=5)
colnames(Result) <- c("X", "Y", "sig2_x", "cov_xy", "sig2_y")
main_results <- matrix(0, nrow=outer.sim, ncol=7)
colnames(main_results) <- c("X", "Y", "sig2_x", "cov_xy", "sig2_y", "rho", 
                            "CCC_Bayes")

for(i in seq_len(outer.sim)){
  j=1
  while(j<=inner.sim){
    
    S.wish <- riwish(sample.size-1, S.1)
    U <- runif(1, 0, 1)
    sigma.wish.x <- sqrt(S.wish[1,1])
    sigma.wish.y <- sqrt(S.wish[1,1])
    cov.xy <- S.wish[1,2]
    
    pi.ij <- 1/ ( sigma.wish.x* sigma.wish.y* (1- cov.xy*cov.xy)^(3/2) )
    
    pi.R.rho <- 1/ ( sigma.wish.x* sigma.wish.y* (1-cov.xy*cov.xy) )
    
    cond.pi <- pi.R.rho/ pi.ij
    
    if(U <= cond.pi && cond.pi != "NaN"){
      Result[j, 3:5] = c( S.wish[1,1], S.wish[1,2], S.wish[2,2]) 
      Result[j, c(1,2)]  = mvrnorm(n= 1, mu= c(mean(sim.data.1[,1] ), 
                                               mean(sim.data.1[,2] )), 
                                   S.wish/sample.size)
      j<-j+1
    } else {}
  }
  
  main_results[i,1:5] <- colMeans(Result[,1:5]) 
  main_results[i,6] <- mean(Result[,4]/ (sqrt(Result[,3]* Result[,5])))
  main_results[i, 7] <- mean(2* Result[,"cov_xy"]/  ( (Result[,"X"]- Result[,"Y"])^2 + Result[,"sig2_x"]+ Result[,"sig2_y"] ))
}

mean(main_results[,6])
median(main_results[,6])
median(main_results[,7])
mean(main_results[,7])
#hist(100*main_results[,6], freq = FALSE)

write.csv(main_results, "/Users/malooney/Google Drive/digitalLibrary/*Decesion_Theory/Decision_Theory/data/JRP_100_100_105_100_125_99_main_results.csv")


# Create Data -----------------------------------------------------------------
# Combination 1, rho=0.90 -----------------------------------------------------

sample.size= 100
inner.sim= 5000
outer.sim= 1000

mu.1 <- c(100, 105)
sig.matrix <- matrix(c(100, 0.90, 0.90, 125), nrow=2, ncol=2)

sim.data.1 <<- mvrnorm(n= sample.size, mu.1, sig.matrix, empirical = T)

S.1 <- cov(sim.data.1)

Result = matrix(0, nrow=inner.sim, ncol=5)
colnames(Result) <- c("X", "Y", "sig2_x", "cov_xy", "sig2_y")
main_results <- matrix(0, nrow=outer.sim, ncol=7)
colnames(main_results) <- c("X", "Y", "sig2_x", "cov_xy", "sig2_y", "rho", 
                            "CCC_Bayes")

for(i in seq_len(outer.sim)){
  j=1
  while(j<=inner.sim){
    
    S.wish <- riwish(sample.size-1, S.1)
    U <- runif(1, 0, 1)
    sigma.wish.x <- sqrt(S.wish[1,1])
    sigma.wish.y <- sqrt(S.wish[1,1])
    cov.xy <- S.wish[1,2]
    
    pi.ij <- 1/ ( sigma.wish.x* sigma.wish.y* (1- cov.xy*cov.xy)^(3/2) )
    
    pi.R.rho <- 1/ ( sigma.wish.x* sigma.wish.y* (1-cov.xy*cov.xy) )
    
    cond.pi <- pi.R.rho/ pi.ij
    
    if(U <= cond.pi && cond.pi != "NaN"){
      Result[j, 3:5] = c( S.wish[1,1], S.wish[1,2], S.wish[2,2]) 
      Result[j, c(1,2)]  = mvrnorm(n= 1, mu= c(mean(sim.data.1[,1] ), 
                                               mean(sim.data.1[,2] )), 
                                   S.wish/sample.size)
      j<-j+1
    } else {}
  }
  
  main_results[i,1:5] <- colMeans(Result[,1:5]) 
  main_results[i,6] <- mean(Result[,4]/ (sqrt(Result[,3]* Result[,5])))
  main_results[i, 7] <- mean(2* Result[,"cov_xy"]/  ( (Result[,"X"]- Result[,"Y"])^2 + Result[,"sig2_x"]+ Result[,"sig2_y"] ))
}

mean(main_results[,6])
median(main_results[,6])
median(main_results[,7])
mean(main_results[,7])
#hist(100*main_results[,6], freq = FALSE)

write.csv(main_results, "/Users/malooney/Google Drive/digitalLibrary/*Decesion_Theory/Decision_Theory/data/JRP_100_100_105_100_125_90_main_results.csv")


# Create Data -----------------------------------------------------------------
# Combination 1, rho=0.80 -----------------------------------------------------

sample.size= 100
inner.sim= 5000
outer.sim= 1000

mu.1 <- c(100, 105)
sig.matrix <- matrix(c(100, 0.80, 0.80, 125), nrow=2, ncol=2)

sim.data.1 <<- mvrnorm(n= sample.size, mu.1, sig.matrix, empirical = T)

S.1 <- cov(sim.data.1)

Result = matrix(0, nrow=inner.sim, ncol=5)
colnames(Result) <- c("X", "Y", "sig2_x", "cov_xy", "sig2_y")
main_results <- matrix(0, nrow=outer.sim, ncol=7)
colnames(main_results) <- c("X", "Y", "sig2_x", "cov_xy", "sig2_y", "rho", 
                            "CCC_Bayes")

for(i in seq_len(outer.sim)){
  j=1
  while(j<=inner.sim){
    
    S.wish <- riwish(sample.size-1, S.1)
    U <- runif(1, 0, 1)
    sigma.wish.x <- sqrt(S.wish[1,1])
    sigma.wish.y <- sqrt(S.wish[1,1])
    cov.xy <- S.wish[1,2]
    
    pi.ij <- 1/ ( sigma.wish.x* sigma.wish.y* (1- cov.xy*cov.xy)^(3/2) )
    
    pi.R.rho <- 1/ ( sigma.wish.x* sigma.wish.y* (1-cov.xy*cov.xy) )
    
    cond.pi <- pi.R.rho/ pi.ij
    
    if(U <= cond.pi && cond.pi != "NaN"){
      Result[j, 3:5] = c( S.wish[1,1], S.wish[1,2], S.wish[2,2]) 
      Result[j, c(1,2)]  = mvrnorm(n= 1, mu= c(mean(sim.data.1[,1] ), 
                                               mean(sim.data.1[,2] )), 
                                   S.wish/sample.size)
      j<-j+1
    } else {}
  }
  
  main_results[i,1:5] <- colMeans(Result[,1:5]) 
  main_results[i,6] <- mean(Result[,4]/ (sqrt(Result[,3]* Result[,5])))
  main_results[i, 7] <- mean(2* Result[,"cov_xy"]/  ( (Result[,"X"]- Result[,"Y"])^2 + Result[,"sig2_x"]+ Result[,"sig2_y"] ))
}

mean(main_results[,6])
median(main_results[,6])
median(main_results[,7])
mean(main_results[,7])
#hist(100*main_results[,6], freq = FALSE)

write.csv(main_results, "/Users/malooney/Google Drive/digitalLibrary/*Decesion_Theory/Decision_Theory/data/JRP_100_100_105_100_125_80_main_results.csv")


# Create Data -----------------------------------------------------------------
# Combination 1, rho=0.40 -----------------------------------------------------

sample.size= 100
inner.sim= 5000
outer.sim= 1000

mu.1 <- c(100, 105)
sig.matrix <- matrix(c(100, 0.40, 0.40, 125), nrow=2, ncol=2)

sim.data.1 <<- mvrnorm(n= sample.size, mu.1, sig.matrix, empirical = T)

S.1 <- cov(sim.data.1)

Result = matrix(0, nrow=inner.sim, ncol=5)
colnames(Result) <- c("X", "Y", "sig2_x", "cov_xy", "sig2_y")
main_results <- matrix(0, nrow=outer.sim, ncol=7)
colnames(main_results) <- c("X", "Y", "sig2_x", "cov_xy", "sig2_y", "rho", 
                            "CCC_Bayes")

for(i in seq_len(outer.sim)){
  j=1
  while(j<=inner.sim){
    
    S.wish <- riwish(sample.size-1, S.1)
    U <- runif(1, 0, 1)
    sigma.wish.x <- sqrt(S.wish[1,1])
    sigma.wish.y <- sqrt(S.wish[1,1])
    cov.xy <- S.wish[1,2]
    
    pi.ij <- 1/ ( sigma.wish.x* sigma.wish.y* (1- cov.xy*cov.xy)^(3/2) )
    
    pi.R.rho <- 1/ ( sigma.wish.x* sigma.wish.y* (1-cov.xy*cov.xy) )
    
    cond.pi <- pi.R.rho/ pi.ij
    
    if(U <= cond.pi && cond.pi != "NaN"){
      Result[j, 3:5] = c( S.wish[1,1], S.wish[1,2], S.wish[2,2]) 
      Result[j, c(1,2)]  = mvrnorm(n= 1, mu= c(mean(sim.data.1[,1] ), 
                                               mean(sim.data.1[,2] )), 
                                   S.wish/sample.size)
      j<-j+1
    } else {}
  }
  
  main_results[i,1:5] <- colMeans(Result[,1:5]) 
  main_results[i,6] <- mean(Result[,4]/ (sqrt(Result[,3]* Result[,5])))
  main_results[i, 7] <- mean(2* Result[,"cov_xy"]/  ( (Result[,"X"]- Result[,"Y"])^2 + Result[,"sig2_x"]+ Result[,"sig2_y"] ))
}

mean(main_results[,6])
median(main_results[,6])
median(main_results[,7])
mean(main_results[,7])
#hist(100*main_results[,6], freq = FALSE)

write.csv(main_results, "/Users/malooney/Google Drive/digitalLibrary/*Decesion_Theory/Decision_Theory/data/JRP_100_100_105_100_125_40_main_results.csv")
















# Create Data -----------------------------------------------------------------
# Combination 1, rho=0.99 -----------------------------------------------------

sample.size= 100
inner.sim= 5000
outer.sim= 1000

mu.1 <- c(135, 135)
sig.matrix <- matrix(c(88, 0.99, 0.99, 88), nrow=2, ncol=2)

sim.data.1 <<- mvrnorm(n= sample.size, mu.1, sig.matrix, empirical = T)

S.1 <- cov(sim.data.1)

Result = matrix(0, nrow=inner.sim, ncol=5)
colnames(Result) <- c("X", "Y", "sig2_x", "cov_xy", "sig2_y")
main_results <- matrix(0, nrow=outer.sim, ncol=7)
colnames(main_results) <- c("X", "Y", "sig2_x", "cov_xy", "sig2_y", "rho", 
                            "CCC_Bayes")

for(i in seq_len(outer.sim)){
  j=1
  while(j<=inner.sim){
    
    S.wish <- riwish(sample.size-1, S.1)
    U <- runif(1, 0, 1)
    sigma.wish.x <- sqrt(S.wish[1,1])
    sigma.wish.y <- sqrt(S.wish[1,1])
    cov.xy <- S.wish[1,2]
    
    pi.ij <- 1/ ( sigma.wish.x* sigma.wish.y* (1- cov.xy*cov.xy)^(3/2) )
    
    pi.R.rho <- 1/ ( sigma.wish.x* sigma.wish.y* (1-cov.xy*cov.xy) )
    
    cond.pi <- pi.R.rho/ pi.ij
    
    if(U <= cond.pi && cond.pi != "NaN"){
      Result[j, 3:5] = c( S.wish[1,1], S.wish[1,2], S.wish[2,2]) 
      Result[j, c(1,2)]  = mvrnorm(n= 1, mu= c(mean(sim.data.1[,1] ), 
                                               mean(sim.data.1[,2] )), 
                                   S.wish/sample.size)
      j<-j+1
    } else {}
  }
  
  main_results[i,1:5] <- colMeans(Result[,1:5]) 
  main_results[i,6] <- mean(Result[,4]/ (sqrt(Result[,3]* Result[,5])))
  main_results[i, 7] <- mean(2* Result[,"cov_xy"]/  ( (Result[,"X"]- Result[,"Y"])^2 + Result[,"sig2_x"]+ Result[,"sig2_y"] ))
}

mean(main_results[,6])
median(main_results[,6])
median(main_results[,7])
mean(main_results[,7])
#hist(100*main_results[,6], freq = FALSE)

write.csv(main_results, "/Users/malooney/Google Drive/digitalLibrary/*Decesion_Theory/Decision_Theory/data/JRP_100_135_135_88_88_99_main_results.csv")


# Create Data -----------------------------------------------------------------
# Combination 1, rho=0.90 -----------------------------------------------------

sample.size= 100
inner.sim= 5000
outer.sim= 1000

mu.1 <- c(135, 135)
sig.matrix <- matrix(c(88, 0.90, 0.90, 88), nrow=2, ncol=2)

sim.data.1 <<- mvrnorm(n= sample.size, mu.1, sig.matrix, empirical = T)

S.1 <- cov(sim.data.1)

Result = matrix(0, nrow=inner.sim, ncol=5)
colnames(Result) <- c("X", "Y", "sig2_x", "cov_xy", "sig2_y")
main_results <- matrix(0, nrow=outer.sim, ncol=7)
colnames(main_results) <- c("X", "Y", "sig2_x", "cov_xy", "sig2_y", "rho", 
                            "CCC_Bayes")

for(i in seq_len(outer.sim)){
  j=1
  while(j<=inner.sim){
    
    S.wish <- riwish(sample.size-1, S.1)
    U <- runif(1, 0, 1)
    sigma.wish.x <- sqrt(S.wish[1,1])
    sigma.wish.y <- sqrt(S.wish[1,1])
    cov.xy <- S.wish[1,2]
    
    pi.ij <- 1/ ( sigma.wish.x* sigma.wish.y* (1- cov.xy*cov.xy)^(3/2) )
    
    pi.R.rho <- 1/ ( sigma.wish.x* sigma.wish.y* (1-cov.xy*cov.xy) )
    
    cond.pi <- pi.R.rho/ pi.ij
    
    if(U <= cond.pi && cond.pi != "NaN"){
      Result[j, 3:5] = c( S.wish[1,1], S.wish[1,2], S.wish[2,2]) 
      Result[j, c(1,2)]  = mvrnorm(n= 1, mu= c(mean(sim.data.1[,1] ), 
                                               mean(sim.data.1[,2] )), 
                                   S.wish/sample.size)
      j<-j+1
    } else {}
  }
  
  main_results[i,1:5] <- colMeans(Result[,1:5]) 
  main_results[i,6] <- mean(Result[,4]/ (sqrt(Result[,3]* Result[,5])))
  main_results[i, 7] <- mean(2* Result[,"cov_xy"]/  ( (Result[,"X"]- Result[,"Y"])^2 + Result[,"sig2_x"]+ Result[,"sig2_y"] ))
}

mean(main_results[,6])
median(main_results[,6])
median(main_results[,7])
mean(main_results[,7])
#hist(100*main_results[,6], freq = FALSE)

write.csv(main_results, "/Users/malooney/Google Drive/digitalLibrary/*Decesion_Theory/Decision_Theory/data/JRP_100_135_135_88_88_90_main_results.csv")


# Create Data -----------------------------------------------------------------
# Combination 1, rho=0.80 -----------------------------------------------------

sample.size= 100
inner.sim= 5000
outer.sim= 1000

mu.1 <- c(135, 135)
sig.matrix <- matrix(c(88, 0.80, 0.80, 88), nrow=2, ncol=2)

sim.data.1 <<- mvrnorm(n= sample.size, mu.1, sig.matrix, empirical = T)

S.1 <- cov(sim.data.1)

Result = matrix(0, nrow=inner.sim, ncol=5)
colnames(Result) <- c("X", "Y", "sig2_x", "cov_xy", "sig2_y")
main_results <- matrix(0, nrow=outer.sim, ncol=7)
colnames(main_results) <- c("X", "Y", "sig2_x", "cov_xy", "sig2_y", "rho", 
                            "CCC_Bayes")

for(i in seq_len(outer.sim)){
  j=1
  while(j<=inner.sim){
    
    S.wish <- riwish(sample.size-1, S.1)
    U <- runif(1, 0, 1)
    sigma.wish.x <- sqrt(S.wish[1,1])
    sigma.wish.y <- sqrt(S.wish[1,1])
    cov.xy <- S.wish[1,2]
    
    pi.ij <- 1/ ( sigma.wish.x* sigma.wish.y* (1- cov.xy*cov.xy)^(3/2) )
    
    pi.R.rho <- 1/ ( sigma.wish.x* sigma.wish.y* (1-cov.xy*cov.xy) )
    
    cond.pi <- pi.R.rho/ pi.ij
    
    if(U <= cond.pi && cond.pi != "NaN"){
      Result[j, 3:5] = c( S.wish[1,1], S.wish[1,2], S.wish[2,2]) 
      Result[j, c(1,2)]  = mvrnorm(n= 1, mu= c(mean(sim.data.1[,1] ), 
                                               mean(sim.data.1[,2] )), 
                                   S.wish/sample.size)
      j<-j+1
    } else {}
  }
  
  main_results[i,1:5] <- colMeans(Result[,1:5]) 
  main_results[i,6] <- mean(Result[,4]/ (sqrt(Result[,3]* Result[,5])))
  main_results[i, 7] <- mean(2* Result[,"cov_xy"]/  ( (Result[,"X"]- Result[,"Y"])^2 + Result[,"sig2_x"]+ Result[,"sig2_y"] ))
}

mean(main_results[,6])
median(main_results[,6])
median(main_results[,7])
mean(main_results[,7])
#hist(100*main_results[,6], freq = FALSE)

write.csv(main_results, "/Users/malooney/Google Drive/digitalLibrary/*Decesion_Theory/Decision_Theory/data/JRP_100_135_135_88_88_80_main_results.csv")


# Create Data -----------------------------------------------------------------
# Combination 1, rho=0.40 -----------------------------------------------------

sample.size= 100
inner.sim= 5000
outer.sim= 1000

mu.1 <- c(135, 135)
sig.matrix <- matrix(c(88, 0.40, 0.40, 88), nrow=2, ncol=2)

sim.data.1 <<- mvrnorm(n= sample.size, mu.1, sig.matrix, empirical = T)

S.1 <- cov(sim.data.1)

Result = matrix(0, nrow=inner.sim, ncol=5)
colnames(Result) <- c("X", "Y", "sig2_x", "cov_xy", "sig2_y")
main_results <- matrix(0, nrow=outer.sim, ncol=7)
colnames(main_results) <- c("X", "Y", "sig2_x", "cov_xy", "sig2_y", "rho", 
                            "CCC_Bayes")

for(i in seq_len(outer.sim)){
  j=1
  while(j<=inner.sim){
    
    S.wish <- riwish(sample.size-1, S.1)
    U <- runif(1, 0, 1)
    sigma.wish.x <- sqrt(S.wish[1,1])
    sigma.wish.y <- sqrt(S.wish[1,1])
    cov.xy <- S.wish[1,2]
    
    pi.ij <- 1/ ( sigma.wish.x* sigma.wish.y* (1- cov.xy*cov.xy)^(3/2) )
    
    pi.R.rho <- 1/ ( sigma.wish.x* sigma.wish.y* (1-cov.xy*cov.xy) )
    
    cond.pi <- pi.R.rho/ pi.ij
    
    if(U <= cond.pi && cond.pi != "NaN"){
      Result[j, 3:5] = c( S.wish[1,1], S.wish[1,2], S.wish[2,2]) 
      Result[j, c(1,2)]  = mvrnorm(n= 1, mu= c(mean(sim.data.1[,1] ), 
                                               mean(sim.data.1[,2] )), 
                                   S.wish/sample.size)
      j<-j+1
    } else {}
  }
  
  main_results[i,1:5] <- colMeans(Result[,1:5]) 
  main_results[i,6] <- mean(Result[,4]/ (sqrt(Result[,3]* Result[,5])))
  main_results[i, 7] <- mean(2* Result[,"cov_xy"]/  ( (Result[,"X"]- Result[,"Y"])^2 + Result[,"sig2_x"]+ Result[,"sig2_y"] ))
}

mean(main_results[,6])
median(main_results[,6])
median(main_results[,7])
mean(main_results[,7])
#hist(100*main_results[,6], freq = FALSE)

write.csv(main_results, "/Users/malooney/Google Drive/digitalLibrary/*Decesion_Theory/Decision_Theory/data/JRP_100_135_135_88_88_40_main_results.csv")