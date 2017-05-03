# Renjin is a JVM-based interpreter for the R language for statistical computing. http://www.renjin.org/
# http://www.inp.nsk.su/~baldin/DataAnalysis/R/R-10-hpc.pdf
# http://www.tandfonline.com/doi/pdf/10.1623/hysj.51.6.1092
# https://arxiv.org/pdf/1503.00855.pdf
require(compiler)
enableJIT(3)

ds <- function(Xa, X) {
  value <- (X - Xa) %*% t(X - Xa)
  return(as.numeric(value))
}

pattern <- function(Xa, X, sigma) {
  res <- exp( - ds(Xa, X) / (2 * sigma ^ 2) )
  return(as.numeric(res))
}

patterns <- function(Xa, X, sigma)
  apply(Xa, 1, pattern, X, sigma)

K <- function(Xa, Ya, X, sigma) {
  patterns1 <- patterns(Xa, X, sigma)
  f <- sum(Ya * patterns1) / sum(patterns1)
  return(f)
}

sim <- function(Xa, Ya, Ga, sigma){
  len <- length(Ga[,1])
  res <- as.matrix(1:len)
  for(i in 1:len) {
    res[i] <- K(Xa, Ya, Ga[i,], sigma)
  }
  return(res)
}
n <- 10000; set.seed(123456)
x <- as.matrix(runif(n, -2, 2))
y <- as.matrix(x^3 + rnorm(n, 0, .1))
plot(x,y, col="blue")
K(x, y, -2, 0.1)
K(x, y, 0, 0.1)
K(x, y, 2, 0.1)

x.sample <- as.matrix(sample(x, 50))
sim(x, y, x.sample, 0.01)
