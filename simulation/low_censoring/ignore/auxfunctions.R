transform <- list(
  logit = function(x, lower = 0, upper = 1) {
    x <- (x - lower) / (upper - lower)
    return(log(x / (1 - x)))
  },
  inv.logit = function(x, lower = 0, upper = 1) {
    p <- exp(x) / (1 + exp(x))
    p <- p * (upper - lower) + lower
    return(p)
  }
)

mhupdate <- function(acc, att, mh, nattempts = 50, lower = 0.8, higher = 1.2){
  acc.rate     <- acc / att
  these.update <- att > nattempts
  these.low    <- (acc.rate < 0.30) & these.update
  these.high   <- (acc.rate > 0.50) & these.update
  
  mh[these.low]  <- mh[these.low] * lower
  mh[these.high] <- mh[these.high] * higher
  
  acc[these.update] <- 0
  att[these.update] <- 0
  
  results <- list(acc = acc, att = att, mh = mh)
  return(results)
}

CorFx <- function(d, gamma, rho, nu) {
  if (rho < 1e-6) {
    n <- nrow(d)
    cor <- diag(1, nrow = n)
  } else {
    if (nu == 0.5) {
      cor <- gamma * simple.cov.sp(D = d, sp.type = "exponential",
                                   sp.par = c(1, rho),
                                   error.var = 0, smoothness = nu,
                                   finescale.var = 0)
    } else {
      cor <- tryCatch(simple.cov.sp(D = d, sp.type = "matern",
                                    sp.par = c(1, rho),
                                    error.var = 0, smoothness = nu,
                                    finescale.var = 0),
                      warning=function(e) {
                        cat("rho = ", rho, "\n")
                        cat("nu = ", nu, "\n")
                      })
      cor <- gamma * cor
    }
    
    diag(cor) <- 1
  }
  
  return(cor)
}
