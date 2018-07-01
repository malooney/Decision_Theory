

CCC <- function(data){
  
  mu_X <- mean(data$X)
  mu_Y <- mean(data$Y)
  sig2_X <- mean(data$sig2_x)
  sig2_Y <- mean(data$sig2_y)
  cov_xy <- mean(data$cov_xy)
  
  CCC= (2* cov_xy / ( (mu_X- mu_Y)^2 + sig2_X+ sig2_Y ))
  return(CCC)
  
}
