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

temps <- list(5, 30, 5, 10)

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
