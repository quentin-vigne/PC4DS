## Intro ####

library(bigmemory)
rm(list=ls())

if (!file.exists("big.desc")) {
  
  # 100 000 000 lignes et 8 colonness =
  #           800 M d'éntrées * 8 bytes ~ 6 GB
  # ceci prends du temps :
  x <- big.matrix(nrow = 1e8,
                  ncol = 8,
                  init = 0, # Optionel mais souvent une bonné idée
                  backingfile    = "big.bin",
                  descriptorfile = "big.desc")
  
} else x <- attach.big.matrix("big.desc")

system.time(if (!file.exists("big.desc")) {
  
  # 100 000 000 lignes et 8 colonness =
  #           800 M d'éntrées * 8 bytes ~ 6 GB
  # ceci prends du temps :
  x <- big.matrix(nrow = 1e8,
                  ncol = 8,
                  init = 0, # Optionel mais souvent une bonné idée
                  backingfile    = "big.bin",
                  descriptorfile = "big.desc")
  
} else x <- attach.big.matrix("big.desc"))

gc(reset=TRUE)
system.time(x[,3] <- runif(nrow(x)))
gc()
gc(reset=TRUE)

hist(x[,3])


## Exercice 1 ####
gc(reset=TRUE)
rm(list=ls())

library(bigmemory)

if (!file.exists("big.desc")) {
  
  # 10 000 lignes et 5 colonness =
  #           50k d'éntrées * 8 bytes ~ 400mo
  # ceci prends du temps :
  x <- big.matrix(nrow = 1e4,
                  ncol = 5,
                  init = 0, # Optionel mais souvent une bonné idée
                  backingfile    = "big.bin",
                  descriptorfile = "big.desc")
  
} else x <- attach.big.matrix("big.desc")

system.time(for(i in 1:ncol(x)){
  for(j in 1:nrow(x)){
    x[j,i] <- runif(min=0, max=1, n=1)
  }
})

system.time(for(i in 1:ncol(x)){
  x[,i] <- runif(nrow(x), min=0, max=1)
})

x[1:10,]

matriceCor <- matrix(1, ncol = nrow(x), nrow=ncol(x))
for(i in 1:(ncol(x)-1)){
  for(j in (i+1):ncol(x)){
    matriceCor[i,j] <- matriceCor[j,i] <-  cor(x[,i], x[,j])
  }
}

decomp <- svd(matriceCor)


library(Rcpp)

Rcpp::cppFunction("int g(int n) {
if (n < 2) return(n);
return(g(n-1) + g(n-2)); }")

sapply(0:11, g)
