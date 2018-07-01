

# Import Data -----------------------------------------------------------------

IPIA_Data <- read.csv("/Users/malooney/Google Drive/digitalLibrary/*Decesion_Theory/Decision_Theory/data/IPIA_Data.csv")

sample.size= length(IPIA_Data)
inner.sim= 5000
outer.sim= 1000

mu.1 <- c(100, 100)
sig.matrix <- matrix(c(100, 0.99, 0.99, 100), nrow=2, ncol=2)

sim.data.1 <<- mvrnorm(n= sample.size, mu.1, sig.matrix, empirical = T)

S.1 <- cov(sim.data.1)

Result = matrix(0, nrow=inner.sim, ncol=5)
colnames(Result) <- c("X", "Y", "sig2_x", "cov_xy", "sig2_y")

main_results <- matrix(0, nrow=outer.sim, ncol=7)
colnames(main_results) <- c("X", "Y", "sig2_x", "cov_xy", "sig2_y", "rho", 
                            "CCC_Bayes")
