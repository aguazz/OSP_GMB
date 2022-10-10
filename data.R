# Loading functions
source(file = "functions.R")
#### Data 1: Brownian bridge (Accuracy) ####
# Brownian motion's coefficients
f1 <- function(t) 0
f2 <- function(t) 0
f3 <- function(c) function(t) c
# Setting parameters
N <- 500; t.final <- 1; t.line <- log(seq(exp(0), exp(t.final), l = N+1)); x.final <- 0
c <- c(0.5, 1, 2)
# Computing the optimal stopping boundary of a Brownian bridge
bnd.bb <- sapply(c, function(c) osb_gm(f1 = f1, f2 = f2, f3 = f3(c = c), t.line = t.line,
                                       err.tol = 1e-3, errors = TRUE, x.final = x.final))
log.err.bb <- sapply(bnd.bb[c(2, 4, 6)], log, simplify = TRUE)
bnd.bb <- matrix(unlist(bnd.bb[c(1, 3, 5)]), ncol = 3, byrow = FALSE)
# Saving data
save(bnd.bb, log.err.bb, N, t.final, t.line, x.final, c, file = "data/FigBB.RData")
#### Data 2: Partition size (Convergence) ####
# Brownian motion's coefficients
f1 <- function(t) 0
f2 <- function(t) 0
f3 <- function(c) function(t) c
# Setting parameters
N <- c(20, 50, 500); t.final <- 1; x.final <- 0; c <- 1
t.line <- sapply(N, function(n) log(seq(exp(0), exp(t.final), l = n)), simplify = FALSE) 
# Computing optimal stopping boundaries
bnd.conv <- sapply(1:3, function(i) osb_gm(f1 = f1, f2 = f2, f3 = f3(c = c), t.line = t.line[[i]],
                                       err.tol = 1e-3, errors = TRUE, x.final = x.final))
# Transforming and saving data
log.err.conv <- sapply(bnd.conv[c(2, 4, 6)], log, simplify = TRUE)
bnd.conv <- bnd.conv[c(1, 3, 5)]
save(bnd.conv, log.err.conv, N, t.final, t.line, x.final, c, file = "data/FigConvergence.RData")
#### Data 3: Partition effect ####
# Brownian motion's coefficients
f1 <- function(t) 0
f2 <- function(t) 0
f3 <- function(c) function(t) c
# Setting parameters
N <- 20; t.final <- 1; x.final <- 0; c <- 1
t.line.equal <- seq(0, t.final, l = N+1)
t.line.shrink <- c(t.line.equal[1:(N-1)], t.final - 1/(N-1)/10, t.final)
t.line.log <- log(seq(exp(0), exp(t.final), l = N+1))
# Computing optimal stopping boundaries
bnd.equal <- osb_gm(f1 = f1, f2 = f2, f3 = f3(c = c), t.line = t.line.equal,
                    err.tol = 1e-3, errors = TRUE, x.final = x.final)
bnd.shrink <- osb_gm(f1 = f1, f2 = f2, f3 = f3(c = c), t.line = t.line.shrink,
                    err.tol = 1e-3, errors = TRUE, x.final = x.final)
bnd.log <- osb_gm(f1 = f1, f2 = f2, f3 = f3(c = c), t.line = t.line.log,
                    err.tol = 1e-3, errors = TRUE, x.final = x.final)
# Saving data
save(bnd.equal, bnd.shrink, bnd.log, N, t.final, t.line.equal, t.line.shrink, t.line.log, 
     x.final, c, file = "data/FigPartition.RData")
#### Data 3.1: Partition error ####
# Brownian motion's coefficients
f1 <- function(t) 0
f2 <- function(t) 0
f3 <- function(c) function(t) c
# Setting parameters
nn <- 5; NN <- 500; Nby <- 5
N <- seq(nn, NN, by = Nby); t.final <- 1; x.final <- 0; c <- 1
error.equal <- c(); error.shrink <- c(); error.log <- c()
for (n in N) {
  print(paste("N =", n))
  t.line.equal <- seq(0, t.final, l = n+1)
  t.line.shrink <- c(t.line.equal[1:n], t.final - 1/n/4, t.final)
  t.line.log <- log(seq(exp(0), exp(t.final), l = n+1))  
  
  # Computing optimal stopping boundaries
  bnd.equal <- osb_gm(f1 = f1, f2 = f2, f3 = f3(c = c), t.line = t.line.equal,
                      err.tol = 1e-3, errors = FALSE, x.final = x.final)
  bnd.shrink <- osb_gm(f1 = f1, f2 = f2, f3 = f3(c = c), t.line = t.line.shrink,
                       err.tol = 1e-3, errors = FALSE, x.final = x.final)
  bnd.log <- osb_gm(f1 = f1, f2 = f2, f3 = f3(c = c), t.line = t.line.log,
                    err.tol = 1e-3, errors = FALSE, x.final = x.final)
  
  # Computing the relative L2 error
  error.equal <- c(error.equal, 
                   sum((bnd.equal[1:(n-1)] - 0.8399 * c * sqrt(t.final - t.line.equal[1:(n-1)]))^2 * diff(t.line.equal)) /
                   sum((0.8399 * c * sqrt(t.final - t.line.equal[1:(n-1)]))^2 * diff(t.line.equal)))
  error.shrink <- c(error.shrink, 
                    sum((bnd.shrink[1:(n-1)] - 0.8399 * c * sqrt(t.final - t.line.shrink[1:(n-1)]))^2 * diff(t.line.shrink)) /
                      sum((0.8399 * c * sqrt(t.final - t.line.shrink[1:(n-1)]))^2 * diff(t.line.shrink)))
  error.log <- c(error.log, 
                 sum((bnd.log[1:(n-1)] - 0.8399 * c * sqrt(t.final - t.line.log[1:(n-1)]))^2 * diff(t.line.log)) / 
                 sum((0.8399 * c * sqrt(t.final - t.line.log[1:(n-1)]))^2 * diff(t.line.log)))
}
# Saving data
save(nn, NN, N, Nby, t.final, x.final, c, error.equal, error.shrink, error.log, file = "data/PartitionError.RData")
#### Data 4: Changing the pulling level ####
# Defining GM coefficients
f1 <- function(a) function(t) a
f2 <- list(
  function(b) function(t) b + b/2*sin(8*pi*t),
  function(b) function(t) b/2 * (2*pnorm(50*t - 25) - 1),
  function(b) function(t) -b/2
)
f3 <- function(c) function(t) c
# Setting parameters
a <- 3; b <- 1; c <- 1
N <- 500; t.final <- 1; t.line <- log(seq(exp(0), exp(t.final), l = N+1)); x.final <- 0
# Getting GM's coefficients (for given parameters)
f1 <- f1(a = a); f3 <- f3(c = c)
f2 <- sapply(f2, function(f) f(b = b))
# Evaluating GM's coefficients in t = t.line
f1.t <- matrix(rep(Vectorize(f1)(t.line), 3), ncol = 3)
f2.t <- sapply(f2, function(f) Vectorize(f)(t.line))
# Getting GMB coefficients and evaluate them at t.line
f.bridge <- sapply(f2, function(f) gm_to_gmb(f1 = f1, f2 = f, f3 = f3, t.final = t.final, x.final = x.final) )
f1.bridge.t <- sapply(1:3, function(i) Vectorize(f.bridge[1, i]$f1)(t.line))
f2.bridge.t <- sapply(1:3, function(i) Vectorize(f.bridge[2, i]$f2)(t.line))
f2.bridge.t[N+1, ] <- x.final
# Computing optimal stopping boundaries
bnd.level <- sapply(f2, function(f) osb_gm(f1 = f1, f2 = f, f3 = f3, t.line = t.line,
                                          err.tol = 1e-3, errors = TRUE, x.final = x.final))
log.err.level <- sapply(bnd.level[c(2, 4, 6)], log, simplify = TRUE)
bnd.level <- matrix(unlist(bnd.level[c(1, 3, 5)]), ncol = 3, byrow = FALSE)
# Saving data
# load(file = "data/PullingLevel.RData")
save(bnd.level, log.err.level, f1.t, f2.t, f1.bridge.t, f2.bridge.t, 
     a, b, c, N, t.final, t.line, x.final, file = "data/PullingLevel.RData")
#### Data 5: Changing the slope ####
# Defining GM coefficients
f1 <- list(
  function(a) function(t) exp(a*t),
  function(a) function(t) 1 + dnorm(20*t - 10) * sqrt(2*pi) * 5*a,
  function(a) function(t) exp(-a*t)
)
f2 <- function(b) function(t) b
f3 <- function(c) function(t) c
# Setting parameters
a <- 5; b <- -1; c <- 1
N <- 500; t.final <- 1; t.line <- log(seq(exp(0), exp(t.final), l = N+1)); x.final <- 0
# Getting GM's coefficients (for given parameters)
f2 <- f2(b = b); f3 <- f3(c = c)
f1 <- sapply(f1, function(f) f(a = a))
# Evaluating GM's coefficients in t = t.line
f1.t <- sapply(f1, function(f) Vectorize(f)(t.line))
f2.t <- matrix(rep(Vectorize(f2)(t.line), 3), ncol = 3)
# Getting GMB coefficients and evaluate them at t.line
f.bridge <- sapply(f1, function(f) gm_to_gmb(f1 = f, f2 = f2, f3 = f3, t.final = t.final, x.final = x.final) )
f1.bridge.t <- sapply(1:3, function(i) Vectorize(f.bridge[1, i]$f1)(t.line))
f2.bridge.t <- sapply(1:3, function(i) Vectorize(f.bridge[2, i]$f2)(t.line))
f2.bridge.t[N+1, ] <- x.final
# Computing optimal stopping boundaries
bnd.slope <- sapply(f1, function(f) osb_gm(f1 = f, f2 = f2, f3 = f3, t.line = t.line,
                                            err.tol = 1e-3, errors = TRUE, x.final = x.final))
log.err.slope <- sapply(bnd.slope[c(2, 4, 6)], log, simplify = TRUE)
bnd.slope <- matrix(unlist(bnd.slope[c(1, 3, 5)]), ncol = 3, byrow = FALSE)
# Saving data
# load(file = "data/Slope.RData")
save(bnd.slope, log.err.slope, f1.t, f2.t, f1.bridge.t, f2.bridge.t,
     a, b, c, N, t.final, t.line, x.final, file = "data/Slope.RData")
#### Data 6: Changing the volatility ####
# Defining GM coefficients
f1 <- function(a) function(t) a
f2 <- function(b) function(t) b
f3 <- list(
  function(c) function(t) 3*c + 3*c*pnorm(100*t - 50),
  function(c) function(t) 3*c + 2*c*cos(8*pi*t), # (dnorm(5*t - 1) + dnorm(5*t - 1)) * sqrt(2*pi),
  function(c) function(t) c + 5*c*dnorm(50*t - 25) * sqrt(2*pi)
)
# Setting parameters
a <- 1; b <- 1; c <- 1
N <- 500; t.final <- 1; t.line <- log(seq(exp(0), exp(t.final), l = N+1)); x.final <- 0
# Getting GM's coefficients (for given parameters)
f2 <- f2(b = b); f1 <- f1(a = a)
f3 <- sapply(f3, function(f) f(c = c))
# Evaluating GM's coefficients in t = t.line
f1.t <- matrix(rep(Vectorize(f1)(t.line), 3), ncol = 3)
f2.t <- matrix(rep(Vectorize(f2)(t.line), 3), ncol = 3)
# Getting GMB coefficients and evaluate them at t.line
f.bridge <- sapply(f3, function(f) gm_to_gmb(f1 = f1, f2 = f2, f3 = f, t.final = t.final, x.final = x.final) )
f1.bridge.t <- sapply(1:3, function(i) Vectorize(f.bridge[1, i]$f1)(t.line))
f2.bridge.t <- sapply(1:3, function(i) Vectorize(f.bridge[2, i]$f2)(t.line))
f2.bridge.t[N+1, ] <- x.final
# Computing optimal stopping boundaries
bnd.vol <- sapply(f3, function(f) osb_gm(f1 = f1, f2 = f2, f3 = f, t.line = t.line,
                                          err.tol = 1e-3, errors = TRUE, x.final = x.final))
log.err.vol <- sapply(bnd.vol[c(2, 4, 6)], log, simplify = TRUE)
bnd.vol <- matrix(unlist(bnd.vol[c(1, 3, 5)]), ncol = 3, byrow = FALSE)
# Saving data
# load(file = "data/Volatility.RData")
save(bnd.vol, log.err.vol, f1.t, f2.t, f1.bridge.t, f2.bridge.t,
     a, b, c, N, t.final, t.line, x.final, file = "data/Volatility.RData")






















