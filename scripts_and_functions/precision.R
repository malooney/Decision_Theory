

precision <- function(data){
  
  mu_X <- mean(data$X)
  mu_Y <- mean(data$Y)
  sig2_X <- mean(data$sig2_x)
  sig2_Y <- mean(data$sig2_y)
  cov_xy <- mean(data$cov_xy)
  sd_x <- sqrt(sig2_X)
  sd_y <- sqrt(sig2_Y)
  
  accuracy= ( 2/ ( (sig2_X/ sig2_Y)+ (1/ (sig2_X/ sig2_Y))+ 
         ((mu_X- mu_Y)^2/ (sd_x* sd_y))  ) )
  
  ccc= (2* cov_xy / ( (mu_X- mu_Y)^2 + sig2_X+ sig2_Y ))
  
  precision= (ccc/ accuracy)
  return(precision)
  
}
