

accuracy <- function(data){
  
  mu_X <- mean(data$X)
  mu_Y <- mean(data$Y)
  sig2_X <- mean(data$sig2_x)
  sig2_Y <- mean(data$sig2_y)
  sd_x <- sqrt(sig2_X)
  sd_y <- sqrt(sig2_Y)
  
  accuracy= (2/ ( (sig2_X/ sig2_Y)+ (1/ (sig2_X/ sig2_Y))+ 
         ((mu_X- mu_Y)^2/ (sd_x* sd_y))  ))
  
  return(accuracy)
  
}
