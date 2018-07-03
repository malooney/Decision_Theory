

CCC <- function(data, plot_CCC=0){
  
  mu_X <- mean(data$X)
  mu_Y <- mean(data$Y)
  sig2_X <- (data$sig2_x)
  sig2_Y <- (data$sig2_y)
  cov_xy <- (data$cov_xy)
  
  CCC= (2* cov_xy / ( (mu_X- mu_Y)^2 + sig2_X+ sig2_Y ))
  if(plot_CCC==1) {
    hist(CCC, prob = T, main="Histogram of CCC", xlab="CCC")
  } else {
    CIL <- mean(CCC)-1.96*sd(CCC)
    CIU <- mean(CCC)+1.96*sd(CCC)
    list(c(CCC=mean(CCC), CIL=CIL, CIU=CIU))
    }

  
}
