
cormat.update <- function(distmat, phi, fudge = 1e-5){
  cormat <- exp(-distmat / phi)
  E <- eigen(cormat)
  E$values <- ifelse(E$values < fudge, fudge, E$values)
  cormat <- E$vectors %*% diag(E$values) %*% t(E$vectors)
  chol.cormat <- chol(cormat)
  cormat.logdet <- 2 * sum(log(diag(chol.cormat)))
  cormat.inv <- chol2inv(chol.cormat)
  list(cormat.inv = cormat.inv, cormat.logdet = cormat.logdet)}

# crossprod.X <- crossprod(X)

beta.update <- function(np, ncov, X, crossprod.X, Y, latent, r, chol.Sigma, sd.beta){
  cov.beta.inv.part1 <- crossprod.X / (1- r) + diag(ncov) / (sd.beta^2)
  cov.beta.part1 <- chol2inv(chol(cov.beta.inv.part1))
  mean.beta <- c(t(cov.beta.part1 %*% crossprod(X, Y - latent))) / (1 - r)
  beta <- mean.beta + c(t(kronecker(chol(cov.beta.part1), chol.Sigma)) %*% rnorm(np * ncov))
  beta}

# B.mat <- t(matrix(beta, np, ncov))
# resid <- Y - X %*% B.mat - latent
# cur.rss.matrix <- crossprod(resid)
# cur.ss.latent.matrix <- crosspord(latent, crossprod(cormat.inv %*% latent))
# quad.beta.matrix <- crossprod(B.mat) / (sd.beta^2)

Sigma.update <- function(ns, np, ncov, cur.rss.matrix, cur.ss.latent.matrix, quad.beta.matrix, r, shape.Sigma, rate.Sigma){
  scale.mat <- cur.rss.matrix / (1 - r) + cur.ss.latent.matrix / r + quad.beta.matrix
  Sigma.inv <- rWishart(1, shape.Sigma + 2 * ns + ncov, chol2inv(chol(rate.Sigma * diag(np) + scale.mat)))[ , , 1]
  chol.Sigma.inv <- chol(Sigma.inv)
  Sigma <- chol2inv(chol.Sigma.inv)
  chol.Sigma <- chol(Sigma)
  list(Sigma = Sigma, Sigma.inv = Sigma.inv, chol.Sigma = chol.Sigma, chol.Sigma.inv = chol.Sigma.inv)}

# latent.chol.Sigma.inv <- latent %*% chol.Sigma.inv
# cur.ss.latent <- sum(latent.chol.Sigma.inv * (cormat.inv %*% latent.chol.Sigma.inv))

phi.update <- function(np, phi, r, phi.upper = Inf, distmat, latent, chol.Sigma.inv, 
                       cormat.inv, cormat.logdet, cur.ss.latent,
                       att.phi, acc.phi, mh.phi){
  att.phi <- att.phi + 1
  
  phi.star <- transform$logit(phi, lower = 0, upper = phi.upper)
  can.phi.star <- rnorm(1, phi.star, mh.phi)
  can.phi <- transform$inv.logit(can.phi.star, lower = 0, upper = phi.upper)
  
  can.cormat.details <- cormat.update(distmat, can.phi)
  can.cormat.inv <- can.cormat.details$cormat.inv
  can.cormat.logdet <- can.cormat.details$cormat.logdet
  
  latent.chol.Sigma.inv <- latent %*% t(chol.Sigma.inv)
  can.ss.latent <- sum(latent.chol.Sigma.inv * (can.cormat.inv %*% latent.chol.Sigma.inv))
  
  R <- -0.5 * (can.ss.latent - cur.ss.latent) / r - 
    0.5 * np * (can.cormat.logdet - cormat.logdet) +
    log(can.phi - 0) + log(phi.upper - can.phi) -
    log(phi - 0) - log(phi.upper - phi)
  
  if(log(runif(1)) < R){
    phi <- can.phi
    cormat.inv <- can.cormat.inv
    cormat.logdet <- can.cormat.logdet
    acc.phi <- acc.phi + 1
    cur.ss.latent <- can.ss.latent}
  
  results <- list(phi = phi, cormat.inv = cormat.inv, cormat.logdet = cormat.logdet, 
                  cur.ss.latent = cur.ss.latent, att.phi = att.phi, acc.phi = acc.phi)
  results}

# resid.chol.Sigma.inv <- resid %*% chol.Sigma.inv
# cur.rss <- sum(resid.chol.Sigma.inv^2)

r.update <- function(ns, np, r, cur.rss, cur.ss.latent, att.r, acc.r, mh.r){
  att.r <- att.r + 1
  
  r.star <- transform$logit(r, 1e-4, 0.9999)
  can.r.star <- rnorm(1, r.star, mh.r)
  can.r <- transform$inv.logit(can.r.star, 1e-4, 0.9999)
  
  ratio1 <- -0.5 * ns * np * (log(can.r) - log(r)) - 0.5 * cur.ss.latent * (1 / can.r - 1 / r)
  ratio2 <- -0.5 * ns * np * (log(1 - can.r) - log(1 - r)) - 0.5 * cur.rss * (1 / (1 - can.r) - 1 / (1 - r))
  ratio <- ratio1 + ratio2 + log(can.r - 1e-4) + log(0.9999 - can.r) - log(r - 1e-4) - log(0.9999 - r)
  
  if(log(runif(1)) < ratio){
    r <- can.r
    acc.r <- acc.r + 1}
  results <- list(r = r, att.r = att.r, acc.r = acc.r)
  return(results)}

latent.update <- function(ns, np, Y, X, B.mat, cormat.inv, chol.Sigma, r){
  eigen.cormat.inv <- eigen(cormat.inv)
  eigenval.cormat.inv <- eigen.cormat.inv$values
  eigenvec.cormat.inv <- eigen.cormat.inv$vectors
  latent.cov.part1 <- eigenvec.cormat.inv %*% diag(1 / (1 / (1 - r) + eigenval.cormat.inv / r)) %*% t(eigenvec.cormat.inv)
  mean.latent <- c(t(latent.cov.part1 %*% (Y - X %*% B.mat) / (1 - r)))
  latent <- mean.latent + c(t(kronecker(chol(latent.cov.part1), chol.Sigma)) %*% rnorm(ns * np))
  latent <- t(matrix(latent, np, ns))
  latent}

# S.all <- rbind(S.pred, S)
# distmat.all <- rdist.earth(S.all, miles = F)

Y.pred.update <- function(ns, ns.pred, np, X.pred, B.mat, latent, distmat.all, phi, chol.Sigma, r){
  
  cormat.all <- exp(-distmat.all / phi)
  
  cormat11 <- cormat.all[1:ns.pred, 1:ns.pred]
  cormat12 <- cormat.all[1:ns.pred, (ns.pred + 1):(ns + ns.pred)]
  cormat21 <- cormat.all[(ns.pred + 1):(ns + ns.pred), 1:ns.pred]
  cormat22 <- cormat.all[(ns.pred + 1):(ns + ns.pred), (ns.pred + 1):(ns + ns.pred)]
  cormat22.inv <- chol2inv(chol(cormat22))
  
  cormat.predict <- cormat11 - (cormat12 %*% cormat22.inv %*% cormat21)
  
  latent.pred.mean <- cormat12 %*% cormat22.inv %*% latent
  latent.pred <- latent.pred.mean + sqrt(r) * (t(chol(cormat.predict)) %*% matrix(rnorm(ns.pred * np), ns.pred, np)) %*% chol.Sigma
  predict.mean <- X.pred %*% B.mat + latent.pred
  predict <- predict.mean + sqrt(1 - r) * matrix(rnorm(ns.pred * np), ns.pred, np) %*% chol.Sigma
  predict}
