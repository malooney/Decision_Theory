#############################################
# Lecture 2 code

###################
# Television data
y = 692
n = 1048


######################
# posterior and prior
par(mar = c(4, 4, 1, 1))
theta <- seq(0.001, 0.999, by = 0.001)
plot(theta, dbeta(theta, y + 1, n - y + 1), t="l",lty=1,
     ylab="Posterior Density",
     xlab=expression(theta))
lines(theta, dbeta(theta, 1,1), lty=2)
legend(0.15, 25, c("Posterior", "Prior"), lty=c(1,2))


#########################################
## Quantile-based intervals
qbeta(c(0.025, 0.975), y + 1, n - y + 1)


##############################################################
# Function: Computes the HPD interval associated with a 
#           particular height variable h which ranges between
#           0 and 1.


HPD.beta.h = function(y, n, h=.1, a=1, b=1, plot=T, ...){
  apost= y + a
  bpost= n - y + b
  if (apost > 1 & bpost > 1) {
    mode = (apost - 1)/(apost + bpost - 2)
    dmode = dbeta(mode, apost, bpost)}
  else return("mode at 0 or 1: HPD not implemented yet")
  lt = uniroot(f=function(x){
    dbeta(x,apost, bpost)/dmode - h},
    lower=0, upper=mode)$root
  ut = uniroot(f=function(x){ dbeta(x,apost, bpost)/dmode - h},
               lower=mode, upper=1)$root
  coverage = pbeta(ut, apost, bpost) - pbeta(lt, apost, bpost)
  if (plot) {
    th = seq(0, 1, length=1000)
    plot(th, dbeta(th, apost, bpost),
         t="l", lty=1,xlab=expression(theta),
         ylab="Posterior Density", ...)
    abline(h=h*dmode)
    segments(ut,0,ut,dbeta(ut,apost,bpost))
    segments(lt,0,lt,dbeta(lt,apost,bpost))
    title(bquote(paste("P(", .(round(lt, 2))," < ", theta, " < ",             
                       .(round(ut,2)), " | " , y, ") = ",
                       .(round(coverage, 2)))))
  }
  return(c(lt,ut,coverage,h))
}


###########################################################################
# HPD based interval:
# Note: h here is between 0 and 1 and each value of h in this region 
#       provides and HPD interval with the coverage reported by the 
#       function HPD.beta.h(....) 

HPD.beta.h(y, n, h = 0.14, xlim = c(0.4, 0.8))
HPD.beta.h(y, n, h = 0.15, xlim = c(0.4, 0.8))
HPD.beta.h(y, n, h = 0.145, xlim = c(0.4, 0.8))

# After fiddeling around I found h = 0.146348 gives us basically what we want
HPD.beta.h(y, n, h = 0.146348, xlim = c(0.4, 0.8))

# Lets get the exact HPD interval

Dev.HPD.beta.h<-function(h, y, n, alpha){
  cov<-HPD.beta.h(y, n, h, plot=F)[3]
  res<-(cov-(1-alpha))^2
  return(res)
}

h.final<-optimize(Dev.HPD.beta.h,c(0,1),y=y,n=n,alpha=0.05)$minimum
HPD.beta.h(y, n, h.final, plot=F)




