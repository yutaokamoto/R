rm(list=ls())
library(maxLik)
setwd("c:/usr/doc/dropbox/daigakuin/beer/")
data <-read.csv("beer2008.csv",header=F)
ns <- nrow(data)  ## ns is number of samples
n.alt <- 2   ## number of alternatives
n.var <- 4   ## number of parameters
b0 <- rep(0,n.var)  ## set initial parameters
x <- array(0,c(ns,n.alt,n.var))  ## make independent variable matrix
y  <- array(0,c(ns,n.alt))  ## make choice result matrix

## set matrix of variables
  x[,1,1] <- data[,3]; x[,2,1] <- data[,4]
  x[,1,2] <- data[,5]; x[,2,2] <- data[,6]
  x[,1,3] <- data[,7]; x[,2,3] <- data[,8]
  x[,1,4] <- 1
  y <- data[,1:2]

## Log-Likelihood function of Logit Model
logit <- function(b) {
  u <- matrix(0, nr=ns, nc=n.alt) ## utility function
  p <- matrix(0, nr=ns, nc=n.alt) ## probability
  for(i in 1:n.alt) u[,i] <- exp(x[,i,] %*% b)
  for(i in 1:n.alt) p[,i] <- u[,i]/rowSums(u)
  a <- p*y; prb <- rowSums(a)
  LL <- sum(log(prb))
  return(LL)
}

## Log-Likelihood function of BinaryProbit Model
probit <- function(b) {
  u <- matrix(0, nr=ns, nc=n.alt)
  p <- matrix(0, nr=ns, nc=n.alt)
  for(i in 1:n.alt) u[,i] <- x[,i,] %*% b
  p[,1] <- pnorm((u[,1]-u[,2]), mean=0, sd=1)
  p[,2] <- 1-p[,1]
  a <- p*y; prb <- rowSums(a)
  LL <- sum(log(prb))
  return(LL)
}

## Maximization of Log-Likelihood by gmaxLik"
  res <- maxLik(logit,start=rep(0,n.var))
#  res <- maxLik(probit,start=rep(0,n.var))
## estimated parameter & Hessian
  b <- res$estimate; hhh<-res$hessian
## estimating t-value
  tval <- b/sqrt(-diag(solve(hhh)))
## Initial Log-likelihood
  L0 <- ns*log(1/n.alt)
## Final Log-Likelihood
  LL <- res$maximum
## Goodness of Fit
## rho-square
  print((L0-LL)/L0)
## rho-square adjusted by degree of freedom
  print((L0-(LL-length(b)))/L0)
  print(b); print(tval)
