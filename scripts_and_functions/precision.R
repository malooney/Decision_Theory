

precision <- function(data, plot_precision=0){
  
  mu_X <- mean(data$X)
  mu_Y <- mean(data$Y)
  sig2_X <- (data$sig2_x)
  sig2_Y <- (data$sig2_y)
  cov_xy <- (data$cov_xy)
  sd_x <- sqrt(sig2_X)
  sd_y <- sqrt(sig2_Y)
  
  accuracy= ( 2/ ( (sig2_X/ sig2_Y)+ (1/ (sig2_X/ sig2_Y))+ 
         ((mu_X- mu_Y)^2/ (sd_x* sd_y))  ) )
  
  ccc= (2* cov_xy / ( (mu_X- mu_Y)^2 + sig2_X+ sig2_Y ))
  
  precision= (ccc/ accuracy)
  
  if(plot_precision==1) {
    hist(precision, prob = T, main="Histogram of Precision", xlab="Precision")
  } else {}
  
  mean(precision)
  
}
