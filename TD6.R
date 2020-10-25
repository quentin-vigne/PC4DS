library(Rcpp)
library(ggplot2)

## Exercice 1 ####

f <- function(n) {
  if (n < 2) return(n)
  return(f(n - 1) + f(n - 2))
}
sapply(0:11, f)


Rcpp::cppFunction("int g(int n) {
if (n < 2) return(n);
return(g(n-1) + g(n-2)); }")
sapply(0:11, g)

microbenchmark::microbenchmark(f(20), g(20))


## Exercice 3 ####

Rcpp::cppFunction("int g(int n) {
if (n < 2) return(n);
return(g(n-1) + g(n-2)); }")

#celle du DO2
fonctionRejet <- function(data,k){
  x <- numeric(nrow(data))
  y <- numeric(nrow(data))
  #pour indicage des vecteurs x,y
  n <- 1
  for (i in 1:nrow(data)){
    #rejet
    if ((data[i,1]^(1/k)+data[i,2]^(1/k))<=1) {
      x[n] <- data[i,1]
      y[n] <- data[i,2]
      n = n+1
    }
  }
  df_rejet <- data.frame(x, y)
  return(df_rejet)
}

## Exercice 4 ####
library(bigmemory)
x <- read.big.matrix(filename="mnist.train.csv", type="integer")