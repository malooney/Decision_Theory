

accuracy_Sim <- function(data, plot_accuracy=0){
  
  mu_X <- mean(data$X)
  mu_Y <- mean(data$Y)
  sig2_X <- (data$sig2_x)
  sig2_Y <- (data$sig2_y)
  sd_x <- sqrt(sig2_X)
  sd_y <- sqrt(sig2_Y)
  
  accuracy= (2/ ( (sig2_X/ sig2_Y)+ (1/ (sig2_X/ sig2_Y))+ 
                    ((mu_X- mu_Y)^2/ (sd_x* sd_y))  ))
  
  if(plot_accuracy==1) {
    hist(accuracy, prob = T, main="Histogram of Accuracy", xlab="Accuracy")
  } else {  CIL <- mean(accuracy)-1.96*sd(accuracy)
  CIU <- mean(accuracy)+1.96*sd(accuracy)
  list(c(accuracy=median(accuracy), CIL=CIL, CIU=CIU))
  }
}
