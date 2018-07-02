

ccc.Freq <- function (x, y, ci = "z-transform", conf.level = 0.95, na.rm = FALSE) 
{
  dat <- data.frame(x, y)
  if (na.rm) 
    dat <- na.omit(dat)
  N. <- 1 - ((1 - conf.level)/2)
  zv <- qnorm(N., mean = 0, sd = 1)
  lower <- "lwr.ci"
  upper <- "upr.ci"
  k <- length(dat$y)
  yb <- mean(dat$y)
  sy2 <- var(dat$y) * (k - 1)/k
  sd1 <- sd(dat$y)
  xb <- mean(dat$x)
  sx2 <- var(dat$x) * (k - 1)/k
  sd2 <- sd(dat$x)
  r <- cor(dat$x, dat$y)
  sl <- r * sd1/sd2
  sxy <- r * sqrt(sx2 * sy2)
  p <- 2 * sxy/(sx2 + sy2 + (yb - xb)^2)
  delta <- (dat$x - dat$y)
  rmean <- apply(dat, MARGIN = 1, FUN = mean)
  blalt <- data.frame(mean = rmean, delta)
  v <- sd1/sd2
  u <- (yb - xb)/((sx2 * sy2)^0.25)
  C.b <- p/r
  sep = sqrt(((1 - ((r)^2)) * (p)^2 * (1 - ((p)^2))/(r)^2 + 
                (2 * (p)^3 * (1 - p) * (u)^2/r) - 0.5 * (p)^4 * 
                (u)^4/(r)^2)/(k - 2))
  ll = p - zv * sep
  ul = p + zv * sep
  t <- log((1 + p)/ (1 - p))/2
  set = sep/(1 - ((p)^2))
  llt = t - zv * set
  ult = t + zv * set
  llt = (exp(2 * llt) - 1)/(exp(2 * llt) + 1)
  ult = (exp(2 * ult) - 1)/(exp(2 * ult) + 1)
  if (ci == "asymptotic") {
    rho.c <- as.data.frame(cbind(p, ll, ul))
    names(rho.c) <- c("est", lower, upper)
    rval <- list(rho.c = rho.c, s.shift = v, l.shift = u, 
                 C.b = C.b, blalt = blalt)
  }
  else if (ci == "z-transform") {
    rho.c <- as.data.frame(cbind(p, llt, ult))
    names(rho.c) <- c("est", lower, upper)
    rval <- list(rho.c = rho.c, s.shift = v, l.shift = u, 
                 C.b = C.b, blalt = blalt)
  }
  return(rval)
}
