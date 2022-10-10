#### FUNCTION: gm_to_gmb ####
# Transform functions f1 and f2 that defines a GM process of the form
# dX(t) = f1(t)(f2(t) - X(t))dt + f3(t)dB(t),
# f(3) is any function, and doesn't need to be transformed as it remains the same 
# after the process is turned into a bridged
gm_to_gmb <- function(f1, f2, f3, t.final, x.final, rel.tol = 1e-3){
  
  # Define auxiliary function
  f.exp <- function(t){
    
    exp(-integrate(Vectorize(f1), lower = t, upper = t.final, rel.tol = rel.tol)$value)
    
  }
  
  f1_ <- function(t){
    
    f1(t) + (f3(t) * f.exp(t))^2 / 
      integrate(Vectorize(function(s) (f3(s) * f.exp(s))^2), lower = t, upper = t.final, rel.tol = rel.tol)$value 
    
  }
  f2_ <- function(t){
    
    I1 <- integrate(Vectorize(function(s) f2(s) * f1(s) * f.exp(s)), lower = t, upper = t.final, rel.tol = rel.tol)$value
    I2 <- integrate(Vectorize(function(s) (f3(s) * f.exp(s))^2), lower = t, upper = t.final, rel.tol = rel.tol)$value 
    
    (f2(t) * f1(t) + f3(t)^2 * f.exp(t) * (x.final - I1) / I2) / (f1(t) + (f3(t) * f.exp(t))^2 / I2)
    
  }
  f3_ <- function(t) {f3(t)}
  
  return(list(f1 = f1_, f2 = f2_, f3 = f3_))
  
}
#### EXAMPLE: gm_to_gmb ####
# # Example 1: Brownian motion
# # Brownian motion's coefficients
# f1 <- function(t) 0
# f2 <- function(t) 0
# f3 <- function(vol) function(t) vol
# # Brownian bridge's coefficients
# f1_bb <- function(t) 1/(t.final - t)
# f2_bb <- function(t, x.final) x.final
# f3_bb <- function(t, vol) vol
# # Define parameters
# t.final <- 1; x.final <- 1; vol <- 30; rel.tol = 1e-10
# # Brownian bridge's coefficients according to gm_to_gmb
# f <- gm_to_gmb(f1 = f1, f2 = f2, f3 = f3(vol = vol), t.final = t.final, x.final = x.final, rel.tol = rel.tol)
# f1_ <- Vectorize(f$f1); f2_ <- Vectorize(f$f2); f3_ <- Vectorize(f$f3)
# # Define comparison parameters
# N <- 100; t.line <- seq(0, t.final, l = N)
# # Compare
# sum(abs( f1_(t = t.line[-N]) - f1_bb(t.line[-N]) ))
# sum(abs( f2_(t = t.line[-N]) - f2_bb(t.line[-N], x.final = x.final) ))
# sum(abs( f3_(t = t.line[-N]) - f3_bb(t.line[-N], vol = vol) ))
# # Example 2: Ornstein-Uhlenbeck
# # Ornstein-Uhlenbeck coefficients
# f1 <- function(a) function(t) a
# f2 <- function(b) function(t) b
# f3 <- function(c) function(t) c
# # Ornstein-Uhlenbeck bridge's coefficients (See CITATION)
# f1_ou <- function(t, a) a * cosh(a*(t.final - t)) / sinh(a*(t.final - t))
# f2_ou <- function(t, x.final, a, b) (a * x.final / sinh(a*(t.final - t)) + b * a * tanh(a*(t.final - t)/2)) /
#                                     (a * cosh(a*(t.final - t)) / sinh(a*(t.final - t)))
# f3_ou <- function(t, c) c
# # Ornstein-Uhlenbeck bridge's coefficients according to gm_to_gmb
# t.final <- 1; x.final <- 2; a <- 2; b <- 2; c <- 1; rel.tol <- 1e-3
# f <- gm_to_gmb(f1 = f1(a = a), f2 = f2(b = b), f3 = f3(c = c), t.final = t.final, x.final = x.final, rel.tol = rel.tol)
# f1_ <- Vectorize(f$f1); f2_ <- Vectorize(f$f2); f3_ <- Vectorize(f$f3)
# # Parameter and variable's definition
# N <- 100; t.line <- seq(0, t.final, l = N)
# # Comparison
# sum(abs( f1_(t = t.line[-N]) - f1_ou(t.line[-N], a = a) ))
# sum(abs( f2_(t = t.line[-N]) - f2_ou(t.line[-N], x.final = x.final, a = a, b = b) ))
# sum(abs( f3_(t = t.line[-N]) - f3_ou(t.line[-N], c = c) ))
#### FUNCTION: osb_gm ####
# Computes, via Picard's iterations (until the relative error is less than err.tol), 
# the optimal stopping boundary of the bridge process derived from the GM process X(t) (bridge or not) given by the SDE
# dX(t) = f1(t)(f2(t) - X(t))dt + f3(t)dB(t),
# with terminal value (t.final, x.final), and associated to the finite-horizon optimal stopping problem
# V(t, x) = sup_s E[X_s]; X_t = x; t < s < t.final being a stopping time of X(t).
# See COMPANION PAPER 1 (time-dependent OU) and COMPANION PAPER 2 (GM bridges).
# If errors = TRUE, the relative errors of each iteration are returned alongside the boundary.
osb_gm <- function(f1, f2, f3, t.final = 1, N = 100, t.line, err.tol = 1e-3, 
                      errors = FALSE, x.final = 0, rel.tol = 1e-3) {
  
  # Check time partition and create time increments
  if (missing(t.line)) t.line <- seq(0, t.final, l = N)
  else N <- length(t.line); t.final <- t.line[N]
  t.delta <-  t.line[2:N] - t.line[1:(N-1)]; 
  t0 <- t.line[1]; t1 <- t.final; x1 <- x.final
  
  # Initialize errors vector if required
  if (errors) err <- c()
  
  # Set initial boundary
  bnd <- rep(x1, N)
  
  # Pre-computing auxiliary functions to compute the mean and variance without redundant computations
  f2_ <- function(t) f1(t)*f2(t)
  # Defining integrands
  F1 <- function(t) exp(-integrate(Vectorize(f1), lower = t0, upper = t, rel.tol = 1e-3)$value)
  F2 <- function(t) integrate(Vectorize(function(s) f2_(s) / F1(s)), lower = t0, upper = t, rel.tol = rel.tol)$value
  F3 <- function(t) integrate(Vectorize(function(s) (f3(s) / F1(s))^2), lower = t0, upper = t, rel.tol = rel.tol)$value
  # Loading doParallel package and exporting integrands
  require(doParallel)
  cl <- makeCluster(detectCores()); 
  registerDoParallel(cl)
  clusterExport(cl, c("f1", "f2", "f3", "f2_", "F1", "F2", "F3"), envir = environment(F1))
  # Calling functions f1 f2 and f3 to avoid lazy-evaluation error
  f1(t0); f2(t0); f3(t0); f2_(t0);
  # Paralleling computing of integrals
  I1 <- c(F1(t0), foreach(t = t.line[-1], .combine = "c") %dopar% F1(t))
  I2 <- c(F2(t0), foreach(t = t.line[-1], .combine = "c") %dopar% F2(t))
  I3 <- c(F3(t0), foreach(t = t.line[-1], .combine = "c") %dopar% F3(t))
  
  # Running Picard's iteration algorithm
  e <- 1 # set initial error
  while (e > err.tol) {
    
    bnd.old <- bnd
    # Computing the boundary recursively
    for (i in (N-2):1) {
      
      cat("\r error:", e, "time:", t.line[i])
      
      # Initial conditions
      t0 <- t.line[i]
      x0 <- bnd.old[i]
      # Future values
      t_ <- t.line[(i+1):(N-1)]
      x_ <- bnd.old[(i+1):(N-1)]
      # Mean and variance at (t, x) with initial condition (t0, x0)
      mean.t <- I1[(i+1):(N-1)] * (x0 / I1[i] + (I2[(i+1):(N-1)] - I2[i])) * (I3[N] - I3[(i+1):(N-1)]) / (I3[N] - I3[i]) + 
                I1[(i+1):(N-1)] / I1[N] * (x.final - I1[N] * (I2[N] - I2[(i+1):(N-1)])) * (I3[(i+1):(N-1)] - I3[i]) / (I3[N] - I3[i])
      var.t <-  I1[(i+1):(N-1)]^2 * (I3[(i+1):(N-1)] - I3[i]) * (I3[N] - I3[(i+1):(N-1)]) / (I3[N] - I3[i])
      # Standardized x-values
      x_ <- (x_ - mean.t) / sqrt(var.t)
      # Kernel computation
      kernel_ <- foreach(j = 1:length(t_), .combine = "c") %dopar%
        {
          ( f2_(t_[j]) + f3(t_[j])^2 * (x1/I1[i+j]/I1[N] - (I2[N] - I2[i+j])) / (I1[i+j] * (I3[N] - I3[i+j])) ) *
            pnorm(x_[j], lower.tail = FALSE) -
          ( f1(t_[j]) + f3(t_[j])^2 / I1[i+j]^2 / (I3[N] - I3[i+j]) ) *
          ( mean.t[j] * pnorm(x_[j], lower.tail = FALSE) + sqrt(var.t[j]) * dnorm(x_[j]) )
        }
      
      # Boundary update
      bnd[i] <- x1 - sum(kernel_ * t.delta[(i+1):(N-1)])
      
    }
    
    cat("\n")
    # Update the error and save it if errors = TRUE
    e <- sum((bnd - bnd.old)[1:(N-1)]^2 * t.delta) / sum(bnd[1:(N-1)]^2 * t.delta)
    if (errors) err <- c(err, e)
    
  }
  
  # Stopping the core clustering
  stopCluster(cl)
  if (errors) return(list(boundary = bnd, errors = err))
  return(bnd)
  
}
#### EXAMPLE: osb_gm_v2 ####
# # Example 1: Brownian bridge
# # Brownian motion's coefficients
# f1_bm <- function(t) 0
# f2_bm <- function(t) 0
# f3_bm <- function(c) function(t) c
# # Setting parameters
# t.final <- 1; x.final <- 0; c <- 1
# N <- 200; t.line <- log(seq(exp(0), exp(t.final), l = N+1))
# # Computing the optimal stopping boundary of a Brownian bridge
# bnd_bb <- osb_gm_v2(f1 = f1_bm, f2 = f2_bm, f3 = f3_bm(c = c), t.line = t.line, errors = TRUE, x.final = x.final)
# plot(t.line, bnd_bb$boundary, type = "l")
# lines(t.line, x.final + c * 0.8399 * sqrt(t.final - t.line), col = "red")
# # Example 2: Vasicek model (changing pulling level)
# # Vasicek's coefficients
# f1_vas <- function(a) function(t) a
# f2_vas <- function(b) function(t) b
# f3_vas <- function(c) function(t) c
# # Setting parameters
# a <- 1; b <- (-5):5; c <- 1; rel.tol <- 1e-3
# N <- 50; t.final <- 1; t.line <- log(seq(exp(0), exp(t.final), l = N)); x.final <- 0
# # Computing optimal stopping boundary of a Brownian bridge (following osb_gm function)
# bnd_vasb <- sapply(1:length(b), function(b) {
#   osb_gm(f1 = f1_vas(a = a), f2 = f2_vas(b = b), f3 = f3_vas(c = c), t.line = t.line,
#          err.tol = 1e-3, errors = FALSE, x.final = x.final, rel.tol = rel.tol)
# })
# # Plot
# matplot(t.line, bnd_vasb, type = "l", lty = 1)

















