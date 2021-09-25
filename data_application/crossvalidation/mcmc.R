mcmc.GP <- function(Y, S, S.pred, X, X.pred, cutoff, censored.locs,
                    beta.init = NULL, Sigma.init = NULL, latent.init = NULL,
                    phi.init = 5, r.init = 0.5,
                    # priors
                    sd.beta = 100,
                    shape.Sigma = 0.01, rate.Sigma = 0.01,
                    phi.upper = NULL,
                    # mcmc settings
                    iters = 4000, burn = 2000, thin = 5){
  
  tick <- proc.time()[3]
  
  library(fields)
  
  ns <- nrow(Y)
  np <- ncol(Y)
  ncov <- ncol(X)
  
  distmat <- rdist.earth(S, miles = F)
  
  ns.pred <- nrow(S.pred)
  S.all <- rbind(S.pred, S)
  distmat.all <- rdist.earth(S.all, miles = F)
  
  if(is.null(beta.init)){
    B.mat <- apply(Y, 2, function(yy){
      lmm <- lm(yy ~ 0 + X)
      as.vector(lmm$coefficients)})
    beta <- c(t(B.mat))
  }else{beta <- beta.init
  B.mat <- t(matrix(beta, np, ncov))
  }
  
  if(is.null(Sigma.init)){
    Sigma <- cov(Y, use = "pairwise.complete.obs")
  }else{Sigma <- Sigma.init}
  chol.Sigma <- chol(Sigma)
  Sigma.inv <- chol2inv(chol.Sigma)
  chol.Sigma.inv <- chol(Sigma.inv)
  
  Y[censored.locs, 1] <- cutoff - sqrt(Sigma[1, 1])
  
  if(is.null(latent.init)){
    latent <- matrix(0, ns, np)
  }else{latent <- latent.init}
  
  phi <- phi.init
  if(is.null(phi.upper)){phi.upper <- Inf}
  
  if(is.null(r.init)){
    r <- 0.5
  }else{r <- r.init}
  
  crossprod.X <- crossprod(X)
  
  cormat.details <- cormat.update(distmat, phi)
  cormat.inv <- cormat.details$cormat.inv
  cormat.logdet <- cormat.details$cormat.logdet
  
  acc.phi <- att.phi <- mh.phi <- 1
  acc.r <- att.r <- mh.r <- 0.1
  
  keepers.beta <- matrix(NA, iters, np * ncov)
  keepers.Sigma <- array(NA, dim = c(iters, np, np))
  keepers.phi <- rep(NA, iters)
  keepers.r <- rep(NA, iters)
  keepers.censored <- matrix(NA, iters, length(censored.locs))
  keepers.pred <- array(NA, dim = c(iters, ns.pred, np))
  
  latent.sum <- matrix(0, ns, np)
  latent2.sum <- matrix(0, ns, np)
  
  return.iters <- (burn + 1):iters
  
  for(iter in 1:iters){for(ttt in 1:thin){
    beta <- beta.update(np, ncov, X, crossprod.X, Y, latent, r, chol.Sigma, sd.beta)
    B.mat <- t(matrix(beta, np, ncov))
    resid <- Y - X %*% B.mat - latent
    cur.rss.matrix <- crossprod(resid)
    cur.ss.latent.matrix <- crossprod(latent, cormat.inv %*% latent)
    quad.beta.matrix <- crossprod(B.mat) / (sd.beta^2)
    
    Sigma.update.details <- Sigma.update(ns, np, ncov, cur.rss.matrix, cur.ss.latent.matrix, 
                                         quad.beta.matrix, r, shape.Sigma, rate.Sigma)
    Sigma <- Sigma.update.details$Sigma
    Sigma.inv <- Sigma.update.details$Sigma.inv
    chol.Sigma <- Sigma.update.details$chol.Sigma
    chol.Sigma.inv <- Sigma.update.details$chol.Sigma.inv
    
    latent <- latent.update(ns, np, Y, X, B.mat, cormat.inv, chol.Sigma, r)
    
    latent.chol.Sigma.inv <- latent %*% t(chol.Sigma.inv)
    cur.ss.latent <- sum(latent.chol.Sigma.inv * (cormat.inv %*% latent.chol.Sigma.inv))
    
    phi.update.details <- phi.update(np, phi, r, phi.upper = phi.upper, distmat, latent, chol.Sigma.inv, 
                                     cormat.inv, cormat.logdet, cur.ss.latent, att.phi, acc.phi, mh.phi)
    phi <- phi.update.details$phi
    cormat.inv <- phi.update.details$cormat.inv 
    cormat.logdet <- phi.update.details$cormat.logdet
    cur.ss.latent <- phi.update.details$cur.ss.latent
    att.phi <- phi.update.details$att.phi
    acc.phi <- phi.update.details$acc.phi
    
    resid.chol.Sigma.inv <- resid %*% t(chol.Sigma.inv)
    cur.rss <- sum(resid.chol.Sigma.inv^2)
    
    r.update.details <- r.update(ns, np, r, cur.rss, cur.ss.latent, att.r, acc.r, mh.r)
    r <- r.update.details$r
    att.r <- r.update.details$att.r
    acc.r <- r.update.details$acc.r
    
    Y <- censored.update(censored.locs, cutoff, Y, X, B.mat, latent, Sigma, r)
    Y.pred <- Y.pred.update(ns, ns.pred, np, X.pred, B.mat, latent, distmat.all, phi, chol.Sigma, r)
    
    if(iter < (burn / 2)){
      this.update <- mhupdate(acc = acc.phi, att = att.phi, mh = mh.phi)
      acc.phi <- this.update$acc
      att.phi <- this.update$att
      mh.phi <- this.update$mh
      
      this.update <- mhupdate(acc = acc.r, att = att.r, mh = mh.r)
      acc.r <- this.update$acc
      att.r <- this.update$att
      mh.r <- this.update$mh
    }
    
  }
    
    if(iter > burn){
      latent.sum <- latent.sum + latent
      latent2.sum <- latent2.sum + latent^2
    }
    
    # storage
    keepers.beta[iter, ] <- beta
    keepers.Sigma[iter, , ] <- Sigma
    keepers.phi[iter] <- phi
    keepers.r[iter] <- r
    keepers.censored[iter, ] <- Y[censored.locs, 1]
    keepers.pred[iter, , ] <- Y.pred
    
    if((iter %% 50 == 0)&(iter > 50)){
      par(mfrow = c(2, 3))
      plot(50:iter, keepers.beta[50:iter, 1], type = "l")
      plot(50:iter, keepers.Sigma[50:iter, 1, 1], type = "l")
      plot(50:iter, keepers.phi[50:iter], type = "l")
      plot(50:iter, keepers.r[50:iter], type = "l")
      plot(50:iter, keepers.censored[50:iter, 1], type = "l")
      plot(50:iter, keepers.pred[50:iter, 1, 1], type = "l")
    }
    
    cat("\t iter", iter, "\n")
  } #end iters
  
  latent.posmean <- latent.sum / length(return.iters)
  latent.posvar <- latent2.sum / length(return.iters) - latent.posmean^2
  
  tock <- proc.time()[3]
  
  results <- list(beta = keepers.beta,
                  Sigma = keepers.Sigma,
                  phi = keepers.phi,
                  r = keepers.r,
                  Y.censored = keepers.censored,
                  Y.pred = keepers.pred,
                  latent.posmean = latent.posmean,
                  latent.posvar = latent.posvar,
                  minutes = (tock - tick) / 60
  )
  
  return(results)}