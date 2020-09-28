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
simuData <- function(n) rnorm(n)
# 2)
perte <- function(s, y, p) (sum(abs(s-y) ^ p)) ^ (1 / p)

# 3)
y <- simuData(20)
for (p in c(1, 2, 5, 1/2)){
  print(paste("Computation for p = ", p))
  print(optimise(f = perte, interval = range(y), y=y, p=p))
}


## Exercice 4 ####
