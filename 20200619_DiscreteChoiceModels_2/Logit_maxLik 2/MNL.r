rm(list=ls())
setwd("c:/usr/doc/dropbox/daigakuin/forComp/ChoiceModels/")
library(maxLik)

dt <-read.csv("d1000.csv",header=T)

ns <- nrow(dt)  ## ns is number of samples
n.alt <- 5   ## number of alternatives
n.var <- 10   ## number of parameters
b0 <- rep(0,n.var)  ## set initial parameters
x <- array(0,c(ns,n.alt,n.var))  ## make independent variable matrix
y  <- array(0,c(ns,n.alt))  ## make choice result matrix
m.av <- array(1,c(ns,n.alt)) ## mode availability

## calculating choice probability
prob <- function(b) {
  u <- matrix(0, nr=ns, nc=n.alt) ## utility function
  p <- matrix(0, nr=ns, nc=n.alt) ## probability
  for(i in 1:n.alt) u[,i] <- exp(x[,i,] %*% b)*m.av[,i]
  for(i in 1:n.alt) p[,i] <- u[,i]/rowSums(u)
  return(p)
}

## Log-Likelihood function of Logit Model
logit <- function(b) {
  p <- prob(b); prb <- rowSums(p*y)
  LL <- sum(log(prb)) ; print(LL)
  return(LL)
}

## gradient function
logra <- function(b) {
  p <- prob(b); g <- numeric(n.var)
  for(i in 1:n.var) g[i] <- sum((x[,,i]-rowSums(x[,,i]*p))*y)
  return(g)
}
## set matrix of variables: x[, mode, parameter.num]
  x[,1,1] <- dt[, 2]; x[,2,1] <- dt[, 3]; x[,3,1] <- dt[, 4]; x[,4,1] <- dt[, 5]; x[,5,1] <- dt[, 6]
  x[,1,2] <- dt[, 7]; x[,2,2] <- dt[, 8]; x[,3,2] <- dt[, 9]; x[,4,2] <- dt[,10]; x[,5,2] <- dt[,11]
  x[,1,3] <- dt[,12]; x[,2,3] <- dt[,13]; x[,3,3] <- dt[,14]; x[,4,3] <- dt[,15]; x[,5,3] <- dt[,16]
  x[,1,4] <- dt[,17]; x[,2,4] <- dt[,18]; x[,3,4] <- dt[,19]; x[,4,4] <- dt[,20]; x[,5,4] <- dt[,21]
  x[,1,5] <- dt[,22]; x[,2,5] <- dt[,23]; x[,3,5] <- dt[,24]; x[,4,5] <- dt[,25]; x[,5,5] <- dt[,26]
  x[,1,6] <- dt[,27]; x[,2,6] <- dt[,28]; x[,3,6] <- dt[,29]; x[,4,6] <- dt[,30]; x[,5,6] <- dt[,31]
  x[,2,7] <- 1; x[,3,8] <- 1; x[,4,9] <- 1; x[,5,10] <- 1
  y <- array(0,c(ns,n.alt))
  for(n in 1:ns) y[n,dt[n,1]] <- 1


## Maximization of Log-Likelihood
  t1 <- Sys.time()      # start
## without gradient function
# res <- maxLik(logit, start=rep(0,n.var))
## with gradient function
 res <- maxLik(logit, grad=logra, start=rep(0,n.var))
  t2 <- Sys.time()      # finish
  print(difftime(t2,t1))
## estimated parameter & Hessian
  b <- res$estimate; hhh<-res$hessian
## calculate t-value
  tval <- b/sqrt(-diag(solve(hhh)))
## Initial Log-likelihood
  L0 <- 0; for(i in 1:n.alt) L0 <- L0 + sum(y[,i])*log(sum(y[,i])/ns)
## Final Log-Likelihood
  LL <- res$maximum

  cat(" L(0)=",L0,"\n"); cat(" L(*)=",LL,"\n")
  cat(" rho=",(L0-LL)/L0,"  rho-bar=",(L0-(LL-length(b)))/L0,"\n")
  for(i in 1:n.var) cat(i,",",signif(b[i],digits=7),",",round(tval[i],digits=2),"\n")

## making Hit table
est <- prob(b); emax <- numeric(ns)
for(n in 1:ns) emax[n] <- which.max(est[n,])
hit <- as.matrix(table(yy,emax))
cat("Hit ratio(%): ",sum(diag(hit))/sum(hit)*100,"\n")
a <- apply(hit,1,sum); hit <- cbind(hit,a)
a <- apply(hit,2,sum); hit <- rbind(hit,a)
hit <- data.frame(hit)
a <- c(seq(1,n.alt),"total")
colnames(hit) <- a; rownames(hit) <- a
print(hit)
