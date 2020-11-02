## Devoir Obligatoire 2
## VIGNE QUENTIN - PC4DS M2DM

##############################
######## Exercice DO2 ########
##############################

## Vider env + import lib ####
rm(list = ls())
library(microbenchmark)
library(ggplot2)

u = runif(min = 0, max = 1, n = 10000)
v = runif(min = 0, max = 1, n = 10000)

points <- data.frame(u,v)


## Fonction de rejet ####
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


<<<<<<< HEAD
for (k in 1:9){
  for (i in 1:100){
    v = runif(min = 0, max = (1 - u[i]^(1/k))^k, n = 100)
    simuRejec(u,v,k)
  }
}

matrix(,1,9)
=======


## Test de la fonction ####
results <- fonctionRejet(points,2)
plot(results)
# pas de soucis, ça rejete bien les points au dessus de 1

## Benchmark du temps de calcul : ####

mb <- microbenchmark(fonctionRejet(points,1),
               fonctionRejet(points,5),
               fonctionRejet(points,10),
               fonctionRejet(points,20),
               times=10)
autoplot(mb)

# Unit: milliseconds
# expr      min       lq     mean   median       uq      max neval
# fonctionRejet(points, 1) 307.6131 310.7151 313.9445 313.8570 317.2503 323.1151    10
# fonctionRejet(points, 5) 209.5509 211.4783 214.1646 212.3906 216.4156 220.3735    10
# fonctionRejet(points, 10) 209.3837 209.9065 221.1311 211.2308 212.5177 311.0771    10
# fonctionRejet(points, 20) 211.0309 212.6861 217.0996 216.6153 218.8806 231.5863    10
>>>>>>> 1696dce69999d61bb354ec08fdec634cbcd15d38
