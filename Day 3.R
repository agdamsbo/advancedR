library(tidyverse)
library(nycflights13)

# 5.6

delays <- flights %>% 
  group_by(dest) %>% 
  summarise(
    count = n(),
    dist = mean(distance, na.rm = TRUE),
    delay = mean(arr_delay, na.rm = TRUE)
  ) %>% 
  filter(count > 20, dest != "HNL")

ggplot(data = delays, mapping = aes(x = dist, y = delay)) +
  geom_point(aes(size = count), alpha = 1/3) +
  geom_smooth(se = FALSE)

library(microbenchmark)
microbenchmark::microbenchmark(
  flights |> 
    rowwise() |>  # loops rowwise, instead of vectorised.
    mutate(arr_time2 = arr_time + 1) |> 
    ungroup() # Reverses the rowwise
  
  flights |> 
    mutate(arr_time2 = arr_time + 1)
)

# 5.6.1 Exercises

flights |> 
  group_by(flight) |> 
  summarise(med = median(arr_delay, na.rm=TRUE)) |> 
  filter(med==-15)

flights |> 
  group_by(flight) |> 
  summarise(med = median(arr_delay, na.rm=TRUE)) |> 
  filter(med==15)

flights |> 
  filter(arr_delay==10) |> 
  group_by(flight) |> 
  summarise(n=n()) |> 
  filter(n >= 10) |> 
  arrange(desc(n))
  

flights |> 
  group_by(flight) |> 
  summarise(early = mean(arr_delay >= 0, na.rm = T),
            n = n())

not_cancelled <- flights %>% 
  filter(!is.na(dep_delay), !is.na(arr_delay))

not_cancelled %>% count(dest)

flights |> 
  filter(!is.na(dep_delay), !is.na(arr_delay)) |> 
  group_by(dest) |> 
  summarise(n= n()) 

flights |> 
  filter(!is.na(dep_delay), !is.na(arr_delay)) |> 
  group_by(tailnum) |> 
  summarise(n = sum(distance)) 

not_cancelled %>% count(tailnum, wt = distance)

flights |> 
  (\(.) filter(., complete.cases(.))) ()

flights |> 
  group_by(year, month, day) |> 
  summarise(canc = sum(is.na(dep_delay)),
            n = n()) |> 
  arrange(desc(canc))


flights |> 
  group_by(year, month, day) |> 
  summarise(canc = mean(is.na(dep_delay)), # calculates the proportion of NAs per day
            del = mean(dep_delay, na.rm=TRUE)) |> 
  ggplot(aes(canc,del)) +
  geom_point() +
  geom_smooth() +
  theme_bw(18)

# 5.7.1 Exercises

flights |> 
  group_by(tailnum) |> 
  filter(sum(!is.na(arr_delay))>1) |> 
  summarise(m_del = max(arr_delay, na.rm = TRUE)) |> 
  slice_max(m_del, n = 1)

flights |> 
  group_by(hour) |> 
  summarise(mean_del=mean(arr_delay , na.rm = TRUE)) |> 
  ggplot(aes(hour,mean_del)) +
  geom_point() +
  geom_smooth()

flights %>%
  group_by(dest) %>%
  mutate(prop_delay = arr_delay / sum(arr_delay, na.rm = TRUE)) %>%
  relocate(prop_delay)


# Relational data

airports |> 
  right_join(flights |> 
               group_by(dest) |> 
               summarise(avg_del = mean(arr_delay,na.rm=TRUE)), 
             c("faa" = "dest")) |> 
  ggplot(aes(lon, lat, size = avg_del, color = avg_del)) +
  borders("state") +
  geom_point(alpha=.6) +
  coord_quickmap()+
  theme_bw(18)+
  scale_color_viridis_c(direction = -1)


flights |> 
  group_by(tailnum) |> 
  summarise(avg_del = mean(arr_delay,na.rm=TRUE)) |> 
  left_join(planes |> select(tailnum, year), 
             c("tailnum")) |> 
  (\(.) filter(.,complete.cases(.))) () |> 
  ggplot(aes(2014-year, avg_del, color = avg_del)) +
  geom_point(alpha=.6)+
  theme_bw(18)+
  scale_color_viridis_c(direction = -1) +
  geom_smooth() +
  theme(aspect.ratio = 0.8, legend.key.width = unit(3, "line"))+ # Setting plot ratio
  labs(y="Mean delay", x="Age")


# Memory

x <- rnorm(2e4)  # Try also with n = 1e5

## The old way:
system.time({
  current_sum <- 0
  res <- c()
  for (x_i in x) {
    current_sum <- current_sum + x_i
    res <- c(res, current_sum)
  }
})

## Be smart, do like this:

system.time({
  current_sum <- 0
  res2 <- double(length(x))
  for (i in seq_along(x)) {
    current_sum <- current_sum + x[i]
    res2[i] <- current_sum
  }
})

# From R >= 3.4
system.time({
  current_sum <- 0
  res4 <- c()
  for (i in seq_along(x)) {
    current_sum <- current_sum + x[i]
    res4[i] <- current_sum
  }
})

n <- 1e3
max <- 1:1000
system.time({
  mat <- NULL
  for (m in max) {
    mat <- cbind(mat, runif(n, max = m))
  }
})

## Vectorisation

### Slow
monte_carlo <- function(N) {
  
  hits <- 0
  for (i in seq_len(N)) {
    u1 <- runif(1)
    u2 <- runif(1)
    if (u1 ^ 2 > u2) {
      hits <- hits + 1
    }
  }
  
  hits / N
}

### Hurtig
monte_carlo2 <- function(N) {
  mean(runif(N) ^ 2 > runif(N))
}

### Test
N=1e3
microbenchmark::microbenchmark(
  monte_carlo(N),
  monte_carlo2(N)
)


# Algebra

## Optimisation exercise
set.seed(1)
N <- 1e4
x <- 0
count <- 0
for (i in seq_len(N)){
  y <- rnorm(1)
  x <- x + y
  if (x < 0) count <- count + 1
}
count / N

mean(cumsum(rnorm(N)) < 0) # Equivalent

## Optimisation exercise
mat <- as.matrix(mtcars)
ind <- seq_len(nrow(mat))
mat_big <- mat[rep(ind, 1000), ]  ## 1000 times bigger dataset
last_row <- mat_big[nrow(mat_big), ]

### Orig
system.time({
  for (j in 1:ncol(mat_big)) {
    for (i in 1:nrow(mat_big)) {
      mat_big[i, j] <- 10 * mat_big[i, j] * last_row[j]
    }
  }
})

### Optimised
system.time(sweep(mat_big, MARGIN = 2, STATS = 10 * last_row, FUN = '*'))




