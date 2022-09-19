##
##

library(advr38pkg)

system.time(rnorm(1e6))

select()

### Debugging
my_fun <- function(a, b) {
  # browser()
  la <- my_log(a) 
  lb <- my_log(b)
  la + lb
}

debugonce(my_fun)  # This is nice!!
my_fun(1,0)
###

### Exercise on coercing
(mat <- matrix(sample(c(TRUE, FALSE), 12, replace = TRUE), 3))

matrix(as.numeric(mat),nrow=nrow(mat))

mat + 0
###

advr38pkg::sum_every(1:10, 2)

# debugonce(advr38pkg::sum_every)

# 3.3.3.2
col_mean <- function(x,n){
  mean(x[[n]],na.rm=TRUE)
}

colMeans(iris[sapply(iris,is.numeric)])

# 3.3.3.3
mat <- matrix(0, 10, 2); mat[c(5, 8, 9, 12, 15, 16, 17, 19)] <- 1; mat

(decode <- matrix(c(0, NA, 1, 2), 2))

decode

decode[mat+1]

skim(mat)
