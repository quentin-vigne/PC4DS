## Vign? Quentin
## TD1 Parallel Computing for Data Science
## 21/09/2020


## Exercice 1 ####
# 1) is.integer(2) -> FALSE c'est un double
is.integer(2)
# 2) if(sqrt(2) * sqrt(2) != 2) print("what ?!") -> renvoi what car pas double 2
if(sqrt(2) * sqrt(2) != 2) print("what ?!") 
# 3) if(0.1 + 0.2 == 0.3) print("result is ok") -> 
if(0.1 + 0.2 == 0.3) print("result is ok")
# 4) if(0.1 + 0.2 != 0.3) print("no way !!!!")
if(0.1 + 0.2 != 0.3) print("no way !!!!")

## Exercice 2 ####
# 1)
f <- function(x) sin(x)^2 + sqrt(abs(x-3))
f2 <- function(x) -f(x)
# 2)
curve(f, from=-6, to=4, main='Courbe de f(x)')
grid(lty=1)
# 3)
integrate(f, lower = -6, upper = 4)
# 4)
optimise(f,c(-6,4))
# 5)
## Exercice 3 ####
# 1)
simuData <- function(n){
  set.seed(2020)
  runif(n)
} 
# 2)
perte <- function(s, y=simuData(20), p=2) (sum(abs(s-y) ^ p)) ^ (1 / p)
perte1 <- function(s, y=simuData(20), p=1) (sum(abs(s-y) ^ p)) ^ (1 / p)

# 3)
y <- simuData(20)

ps <- c(1, 2, 5, 1/2)
lps <- length(ps)
results <- data.frame(p         = numeric(lps),
                      minimum   = numeric(lps),
                      objective = numeric(lps))

for (p in seq_along(ps)) {
  print(paste("Begin computation for p = ", ps[p]))
  res <- optimise(f = perte, interval = range(y), y = y, p = ps[p])
  results[p, ] <- c(ps[p], res)
}
# 4)
vectorized_perte <- Vectorize(perte)
curve(vectorized_perte, from = 0, to = 2)
vectorized_perte1 <- Vectorize(perte1)
curve(vectorized_perte1, from = 0, to = 2)
abline(h=results$objective[2], col='red')

# 5)
median(y) # should be minimum for p = 1
mean(y)   # should be minimum for p = 2

# 6)
abline(h=results$objective[1], col='blue')
## Exercice 4 ####
