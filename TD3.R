## Vigné Quentin
## TD2 Parallel Computing for Data Science
## 22/09/2020

## Introduction ####
#1er essai
nbproc<-4
nb<-1e6
y_vec<-rnorm(nbproc*nb)
(sum1<-sum(y_vec))

#2ème essai
y_list<-split(y_vec, rep(1:nbproc, each=nb))
(s_list<-lapply(y_list, sum))
(sum2<-Reduce('+', s_list))
identical(sum1, sum2)

#3ème essai: Parallélism
install.packages("parallel")
library(parallel)
detectCores()
cl<-makeCluster(2)
s_parlist<-clusterApply(cl,y_list, sum)
(sum3<-Reduce('+', s_parlist))
stopCluster(cl);
rm(cl)
## Exercice 1 ####
library(parallel)
cl <- makeCluster(2)
vec <- unlist(clusterCall(cl, rnorm, 15))
stopCluster(cl);
rm(cl)

library(foreach)
foreach(i=1:30) %do%
  vec2 <- rnorm
## Exercice 2 ####
# 1)
nb<-1e5
y_vec<-rnorm(nb)
(sum1<-sum(y_vec))
# 2)
y_list = split(y_vec,20)
# 3)
s_list = lapply(y_list,sum)
(sum2<-Reduce('+', s_list))
identical(sum1,sum2)
# 4) 
library(parallel)
detectCores()
cl<-makeCluster(2)
s_parlist<-clusterApply(cl,y_list, sum)
(sum3<-Reduce('+', s_parlist))
stopCluster(cl);
rm(cl)
identical(sum3,sum2)
# 5)
library(microbenchmark)
library(ggplot2)
mb <- microbenchmark(sum1,sum2, sum3)
autoplot(mb)
