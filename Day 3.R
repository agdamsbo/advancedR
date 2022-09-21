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

library(lubridate)
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


weather
