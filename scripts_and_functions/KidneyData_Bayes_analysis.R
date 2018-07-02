
# Housekeeping ----------------------------------------------------------------
rm(list= ls())
cat("\014")

library(MCMCpack)

source("/Users/malooney/Google Drive/digitalLibrary/*Decesion_Theory/Decision_Theory/scripts_and_functions/agreement_Stats.R")


# Import Data -----------------------------------------------------------------

IPIA_Data <- read.csv("/Users/malooney/Google Drive/digitalLibrary/*Decesion_Theory/Decision_Theory/data/IPIA_Data.csv")

sample.size= length(IPIA_Data$Kidney)
inner.sim= 5000
outer.sim= 1000

mu.1 <- mean(IPIA_Data$Urography)
mu.2 <- mean(IPIA_Data$Tomography)


S.1 <- cov(IPIA_Data[,-1])

Result = matrix(0, nrow=inner.sim, ncol=5)
colnames(Result) <- c("X", "Y", "sig2_x", "cov_xy", "sig2_y")

main_results <- matrix(0, nrow=outer.sim, ncol=7)
colnames(main_results) <- c("X", "Y", "sig2_x", "cov_xy", "sig2_y", "rho", 
                            "CCC_Bayes")

for(i in seq_len(outer.sim)){ 
  
  for(j in seq_len(inner.sim)){ 
    Sig.1.post <- riwish(sample.size, S.1)
    Result[j, 3:5] = c( Sig.1.post[1,1], Sig.1.post[1,2], Sig.1.post[2,2]) 
    Result[j, c(1,2)]  = mvrnorm(n= 1, mu= c(mu.1, mu.2), 
                                 Sig.1.post/sample.size)
  } 
  main_results[i,1:5] <- colMeans(Result[,1:5]) 
  main_results[i,6] <- mean(Result[,4]/sqrt(Result[,3]*Result[,5]))
  main_results[i, 7] <- mean(2* Result[,"cov_xy"]/  ( (Result[,"X"]- Result[,"Y"])^2 + Result[,"sig2_x"]+ Result[,"sig2_y"] ) )
}

write.csv(main_results, "/Users/malooney/Google Drive/digitalLibrary/*Decesion_Theory/Decision_Theory/data/JP_KidneyData_Bayes.csv")
