

MSD_Sim <- function(data, plot_MSD=0){
  
  mu_X <- mean(data$X)
  mu_Y <- mean(data$Y)
  sig2_X <- (data$sig2_x)
  sig2_Y <- (data$sig2_y)
  cov_xy <- (data$cov_xy)
  
  MSD= ((mu_X- mu_Y)^2 + sig2_X+ sig2_Y- 2* cov_xy)
  
  if(plot_MSD==1) {
    hist(MSD, prob = T, main="Histogram of MSD", xlab="MSD")
  } else {  CIL <- mean(MSD)-1.96*sd(MSD)
  CIU <- mean(MSD)+1.96*sd(MSD)
  list(c(MSD=median(MSD), CIL=CIL, CIU=CIU))
  }
}
