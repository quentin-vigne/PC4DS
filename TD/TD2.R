## Vigné Quentin
## TD2 Parallel Computing for Data Science
## 22/09/2020

## Exercice 1 ####
joe=function(x,k){
  n=length(x)
  r=NULL
  for(i in 1:(n-k+1)) if(all(x[i:(i+k-1)]==1)) r<-c(r,i)
  r
}
x <- c(1,0,0,1,1,0,1,1,1)
k <- 2
joe(x=x, k=k)

debug(joe)
joe(x=x, k=k)

## Exercice 2 ####
# Calcul de la somme des n premiers entiers
f1 = function(n) {
  res <- 0
  for(i in 1:n) {
    res<-res+i
  }
  res
}
debug(f1)
f1(5)

## Exercice 3 ####

f1 <- function(n){
  res <- 0 
  for (i in 1:n) res <- res + i
  
  return(res)
}

f2 <- function(n) sum(seq(n))

f3 <- function(n) n * (n + 1) / 2

n <- 1e15
#system.time(f1(n))
system.time(f2(n))
system.time(f3(n))
library(ggplot2)
library(microbenchmark)

autoplot(microbenchmark(f2(n),f3(n)))

## Exercice 4 ####

f_tri <- function(x) {
  SUP01 <- (0 <= x) & (x < 1)
  SUP12 <- (1 <= x) & (x < 2) 
  if (SUP01) {
    return(x)
  } else {if (SUP12) { 
    return(2 - x)
  } else {
    return(0)
  }
  }
}
f_tri(1) #1
f_tri(0)
f_tri(.5) #.5
f_tri(c(0, .5, 1, 1.5, 2)) #problème, ça ne marche pas
f_tri2 <- Vectorize(f_tri)
f_tri2(c(0, .5, 1, 1.5, 2))

# Version optimizée
f_tri3 <- function(x) {
   SUP01 <- (0 <  x) & (x < 1)
   SUP12 <- (1 <= x) & (x < 2) 
   ifelse(SUP01, x, ifelse(SUP12, 2 - x, 0))
}
f_tri3(c(0, .5, 1, 1.5, 2))

# Et si on mesurait les performances ? 
library(microbenchmark)
library(ggplot2)
xs <- seq(-3,3, length.out = 1e3)
mb <- microbenchmark(f_tri(xs), f_tri2(xs), f_tri3(xs))
mb
# Forcément des erreurs vu que la fonction 1 renvoie des erreurs à chaque appel
# Elle est rapide mais pas juste
# La 2 est vectorisé rapidement mais lente
autoplot(mb)
