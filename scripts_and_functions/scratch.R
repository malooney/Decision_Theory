#set.seed(1)
###################
# Beer data
sum_y = sum(mainData$units)
n = length(mainData$units)
mu_yFreq <- mean(mainData$units)
var_yFreq <- var(mainData$units)

######################
# posterior and prior
#par(mar = c(4, 4, 1, 1))
mu_0 <- 4
sigma2_0 <-8

b0 <- mu_0 / sigma2_0
a0 <- mu_0 * b0

lambda.grid = seq(0.1, 20, length=10000)

lambda.grid.priorpmf = dgamma(lambda.grid, shape = a0, rate = b0)

priorpmf.grid = dgamma(lambda.grid, shape=a0, rate=b0)

plot(lambda.grid, priorpmf.grid,
     col='red', type="l", 
     main="Prior Density")

# Plot Likelihood
# Sum(x) is Poisson(lambda.sum) where lambda.sum=n*lambda
likelihood.grid = dpois(sum_y, lambda = n * lambda.grid)

plot(lambda.grid, likelihood.grid,
     main="Likelihood of lambda", 
     col='green',type="l")

# Plot Posterior
# Posterior parameters

a1= a0 + sum_y
b1 = b0 + n

posteriorpmf.grid=dgamma(lambda.grid,shape= a1, rate= b1)


plot(lambda.grid, posteriorpmf.grid,
     main="Posterior Density", col='blue', type="l")

#     Plot of all densities together ----
par(mfcol=c(1,1))

plot(lambda.grid, posteriorpmf.grid,ylab="density",
     main="Densities \n Gamma Prior/Posterior (red/blue)\n",
     type="n")

lines(lambda.grid, priorpmf.grid, col='red')

lines(lambda.grid, posteriorpmf.grid, col='blue')

lines(density(mainData$units))

lines(lambda.grid,likelihood.grid, col="orange")


posterior.mean = a1/b1

# #     Add case of Uniform prior ----
# lines(lambda.grid, (1 + 0 * priorpmf.grid) * 0.1 / 30, col='orange')
# posteriorpmf.uniformprior.grid = dgamma(lambda.grid, shape = 1 + sum_y,
#                                         rate= n)
# 
# lines(lambda.grid, posteriorpmf.uniformprior.grid, col='green')
# 
# title(main="\n\nUniform Prior/Posterior (orange/green)")

#########################################
## Quantile-based intervals
qgamma(c(0.025, 0.975), a1, b1)





plot(density(rgamma(10000, a0, b0), main="prior distribution of
             lambda"))
abline(v=a0/b0)

plot(density(rgamma(10000, sum_y + a0, n + b0)), t="l",lty=1,
     ylab="Posterior Density",
     xlab=expression(lambda))

mu_yHat <- mean(rgamma(10000, sum_y + a0, n + b0))

lines(theta, dbeta(theta, 1, 1), lty=2)
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
  
  ut = uniroot(f=function(x){ 
    dbeta(x,apost, bpost)/dmode - h}, 
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
  return(c(lt, ut, coverage,h))
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




