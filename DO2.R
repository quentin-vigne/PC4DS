simuRejec <- function(u, v, k){
  val = FALSE
  if ((u^(1/k) + v^(1/k))<=1) {
    val = TRUE
  }
  val
}

# simulation de U
u = runif(min = 0, max = 1, n = 100)
# simulation de V [0, (u - 1^(1/k))^k]

for (k in 1:9){
  for (i in 1:100){
    v = runif(min = 0, max = (1 - u[i]^(1/k))^k, n = 100)
    simuRejec(u,v,k)
  }
}