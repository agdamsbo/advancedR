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

`if`(FALSE,1,3:4) ## This is suposedly better than ifelse
if (TRUE) 1 else 3:4

## 3.4.5.1

cv_split <- function(x, n){
  indices <- sample(rep_len(seq_len(n),length(x)))
  split(x,indices)
}

cv_split(sample(30),4)


## 3.4.5.2

set.seed(1);(x <- rnorm(10))

mean(x)

sample(100,replace = TRUE)

s <- replicate(1000, {
  x2 <- sample(x, replace = TRUE)
  mean(x2)
  })

ci <- quantile(s,probs=c(.025,.975))

hist(s)
abline(v=ci)

# 3.4.5.3
my_mtcars <- mtcars[c("mpg", "hp")]
my_mtcars$my_col <- sample(c("mpg", "hp"), size = nrow(my_mtcars), replace = TRUE)
head(my_mtcars)


# Selects only col 1:2 to keep as numeric when converted to matrix. 
# Subsets by a matrix by match function. Much faster than for loop
my_mtcars$my_val <- my_mtcars[1:2][cbind(seq_along(my_mtcars$my_col),
                     match(my_mtcars$my_col,
                           colnames(my_mtcars)[1:2]))]
