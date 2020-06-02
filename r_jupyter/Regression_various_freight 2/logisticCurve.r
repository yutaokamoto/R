rm(list=ls())
setwd("d:/usr/doc/dropbox/daigakuin/")

set.seed(1234)
a <- 1; b <- 15; c <- 10
t <- seq(1,20); e <- rnorm(20)/2
y <- c/(1+exp(-a*t+b)) + e + 2
plot(t,y)

for(n in 0:10){
  c <- ceiling(max(y)) + n
  yy <- log((c-y)/y)
  res <- lm(yy~t)
  cat("c value= ",c,"\n")
  print(summary(res)$coefficients)
  print(summary(res)$adj.r.squared)
}

dat <- data.frame(cbind(t,y))
res.nls <- nls(y ~ c/(1+exp(-a*t+b))+d, dat, 
               start=list(a=1, b=10, c=10, d=1))
summary(res.nls)
