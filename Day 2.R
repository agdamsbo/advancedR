library(tidyverse)

#
#

head(iris)
library(ggplot2)
library(viridis)
theme_set(theme_bw(18))

iris |> # Data set
  pivot_longer(cols = -Species) |> # Pivot to long
  ggplot() + # base plot
  geom_density(aes(value, fill = Species), alpha = 0.6, linetype=3)+ # Density base plot
  facet_wrap( ~name, nrow=2, scales="free")+ # Facet wrapping with free scales
  theme(aspect.ratio = 0.8, legend.key.width = unit(3, "line"))+ # Setting plot ratio
  scale_fill_viridis(discrete = T) # Auto scales colors for color blind friendliness


df <- dplyr::filter(gapminder::gapminder, year == 1992)

head(df)

library(hrbrthemes)

df |> ggplot(aes(gdpPercap,lifeExp,size=pop,color=continent)) +
  geom_point(alpha=0.5) +
  scale_size(range = c(1, 20)) +
  scale_x_log10()+
  # theme_ipsum() +
  scale_fill_viridis(discrete=TRUE, guide = "none", option="A") +
  theme(aspect.ratio = 0.8, legend.key.width = unit(3, "line"))+ # Setting plot ratio
  labs(title = "Gapminder for 1992",
       x = "Gross Domestic Product (log scale)",
       y = "Life Expectancy at birth (years)",
       color = "Continent", size = "Population") 

library(gganimate)

ggplot(gapminder::gapminder,aes(gdpPercap,lifeExp,size=pop,color=continent)) +
  geom_point(alpha=0.5) +
  scale_size(range = c(1, 20)) +
  scale_x_log10()+
  # theme_ipsum() +
  scale_fill_viridis(discrete=TRUE, guide = "none", option="A") +
  theme(aspect.ratio = 0.8, legend.key.width = unit(3, "line"))+
  labs(title = 'Year: {frame_time}', x = 'GDP per capita', y = 'life expectancy') +
  transition_time(year) +
  ease_aes('linear') # Does not combine the output photos


ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = displ), color = "blue")  

sapply(mpg,is.factor)

head(mpg)
?mpg

ggplot(data = mpg, aes(x = displ, y = hwy, color = displ, size = , shape = )) + 
  geom_point()  


df <- data.frame(abc = 1, xyz = "a")
df$x # Partial matching
df[, "xyz"] # Giving vector
df[, c("abc", "xyz")] # Giving data frame

df <- tibble(abc = 1, xyz = "a")
df$x
df[, "xyz"] # Keeping data frame
df[, c("abc", "xyz")]

var<-"mpg"
head(mtcars)

mtcars[var]

df <- tibble("1" = 1:4, "2" = 3:6)
df[["1"]]
plot(df$`1`,df$`2`)
df$`3` <- (df$`1`/df$`2`)


### dplyr

library(nycflights13)
library(tidyverse)

flights

?flights

flights |> 
  filter(month == 1, day == 1)

flights |> 
  filter(arr_delay > 120)

flights |> 
  filter(dest %in% c("IAH", "HOU"))

print(airlines)

flights |> 
  filter(carrier %in% c("UA", "AA", "DL"))

flights |> 
  filter(month %in% c(7:9))

flights |> 
  filter(arr_delay > 120 & dep_delay==0)

flights |> 
  filter(dep_time<=600)
#
# https://r4ds.had.co.nz/transform.html
#
flights |> arrange(!is.na(dep_time))

flights |> arrange(desc(dep_delay),dep_time)

flights |> mutate(speed=distance/air_time) |> arrange(desc(speed)) |> print(width=Inf)

flights |> arrange(desc(distance))

flights |> arrange(distance)

# Selections

flights |> select(all_of(c("dep_time", "dep_delay", "arr_time", "arr_delay")))
flights |> select(dep_time, dep_delay, arr_time, arr_delay)
flights |> select(dep_time, dep_delay, arr_time, arr_delay)

flights |> select(match(c("dep_time", "dep_delay", "arr_time", "arr_delay"),names(flights)))

flights |> select(any_of(vars))

vars <- c("year", "month", "day", "dep_delay", "arr_delay")

select(flights, contains("TIME", ignore.case = TRUE))

# Mutate
library(lubridate)

flights |> mutate(
  dep_time_n = 
)

min_trans <- function(x){
  ifelse(x== 2400, 0, x %/% 100*60 + x %% 100)
}

flights |> transmute(across(c(dep_time,sched_dep_time),min_trans))

flights |> transmute(
  air_time = air_time,
  air_time_c = arr_time-dep_time # Differences due to time zones
) |> 
  slice_sample(n = 5e3) |> 
  ggplot()+
  geom_point(aes(air_time,air_time_c))+
  geom_abline(color="red")


flights |> 
  transmute(
    sched_dep_time, dep_delay, dep_time
  )

1:3+1:10


