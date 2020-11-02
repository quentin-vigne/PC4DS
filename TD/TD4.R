## Vigné Quentin
## TD2 Parallel Computing for Data Science
## 22/09/2020

rm(list = ls())

## Exercice 1 ####
dormir <- function(i) {
  Sys.sleep(i)
  return(paste("le fils", Sys.getpid(), "a dormi", i, "secondes"))
}

# Ce code met en "pause" pendant un laps de temps i la tâche ayant l'identifiant de processus
# correspondant à rsession.exe. Il renvoi un message perso donnant les infos de pid et de temps i
dormir(2)

# Les unités de calculs se répartissents les tâches et les sous tâches en fonction des besoins

temps <- list(5,10,5,30)

library(parallel)
# Avec multicore (si disponible)
# mclapply(temps, dormir, mc.cores = 2)
# Avec SNOW
cl <- makeCluster(2)
system.time(res <- parLapply(cl, temps, dormir))
stopCluster(cl)

# Diminuer temps
temps2 <- list(c(5, 5, 10), 30)
cl <- makeCluster(2)
system.time(res <- parLapply(cl, temps2, dormir))
stopCluster(cl)
library(ggplot2)
autoplot


## Ex. 2 ####
doichunk <- function(ichunk) {
  tot <- 0
  nr  <- nrow(lnks) # lnks global at worker
  
  for(i in ichunk) {
    tmp <- lnks[(i + 1):nr , ] %*% lnks[i , ]
    tot <- tot + sum(tmp)
  }
  tot
}

mutoutpar <- function(cls, lnks) {
  require(parallel)
  
  nr <- nrow(lnks) # lnks global at worker
  clusterExport(cls, "lnks")
  
  ichunks <- 1:(nr - 1) # each "chunk" has only 1 value of i , for now
  tots <- clusterApply(cls, ichunks, doichunk)
  
  Reduce(sum, tots ) / nr
}

mutoutparB <- function(cls, lnks) {
  require(parallel)
  
  nr <- nrow(lnks) # lnks global at worker
  clusterExport(cls, "lnks")
  
  ichunks <- (nr - 1):1 # each "chunk" has only 1 value of i , for now
  tots <- clusterApply(cls, ichunks, doichunk)
  
  Reduce(sum, tots ) / nr
}

mutoutparC <- function(cls, lnks) {
  require(parallel)
  
  nr <- nrow(lnks) # lnks global at worker
  clusterExport(cls, "lnks")
  
  ichunks <- clusterSplit(cls, 1:(nr - 1)) # each "chunk" has only 1 value of i , for now
  tots <- clusterApply(cls, ichunks, doichunk)
  
  Reduce(sum, tots ) / nr
}

mutoutparD <- function(cls, lnks) {
  require(parallel)
  
  nr <- nrow(lnks) # lnks global at worker
  clusterExport(cls, "lnks")
  
  smp <- sample(nr - 1)
  ichunks <- clusterSplit(cls, smp) 
  tots <- clusterApply(cls, ichunks, doichunk)
  
  Reduce(sum, tots ) / nr
}

mutoutparE <- function(cls, lnks) {
  require(parallel)
  
  nr <- nrow(lnks) # lnks global at worker
  clusterExport(cls, "lnks")
  
  ichunks <- lapply(1:length(cls), function(i) seq(i, nr - 1, by = i))
  tots <- clusterApply(cls, ichunks, doichunk)
  
  Reduce(sum, tots ) / nr
}

## Benchmark and timings with a simulated dataset ####

# Simulate a link matrix of size n (e.g. n = 500)
n   <- 500
lnks <- matrix(sample(0:1 , n^2, replace = TRUE), nrow = n)

#calcul s�quentiel
system.time(lapply(1:(nrow(lnks) - 1), function(i) doichunk(i)))

## Paralle computation with snow
library(parallel)
cls <- makeCluster(4) 
library(microbenchmark)
library(ggplot2)
microbenchmark(mutoutpar(cls=cls, lnks=lnks))
microbenchmark(mutoutparB(cls, lnks))
microbenchmark(mutoutparC(cls, lnks))
microbenchmark(mutoutparD(cls, lnks))
microbenchmark(mutoutparE(cls, lnks))

autoplot(microbenchmark(mutoutpar(cls=cls, lnks=lnks),mutoutparB(cls, lnks),mutoutparC(cls, lnks),
                                  mutoutparD(cls, lnks), mutoutparE(cls, lnks)))
