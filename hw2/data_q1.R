library(tidyverse, caret)

df <- 
  tibble(
    temperature = c(rep("Hot", 4), rep("Cool", 4)),
    humidity = rep(c(rep("High", 2), rep("Low", 2)), 2),
    sky = rep(c("Cloudy", "Clear"), 4),
    rainy = c(9, 5, 6, 3, 7, 2, 3, 1)
  ) %>% 
    mutate(id = row_number()) %>% 
    group_by(id) %>% 
    nest() %>% 
    mutate(df = map(data, function(df){
      bind_rows(
        df %>% mutate(is_rain = 1) %>% slice(rep(1:n(), df$rainy)) %>% select(-rainy),
        df %>% mutate(is_rain = 0) %>% slice(rep(1:n(), 10 - df$rainy)) %>% select(-rainy)
      )
    })) %>% 
    select(df) %>% 
    unnest() 


df %>% 
 write_csv("data/q1.csv")


entropy <- function(x) {
  p = mean(x == 1)
  q = 1-p
  # print(p)
  # print(q)
  -(p*log2(p) + q*log2(q))
}

entropy(df$is_rain)


df %>% 
  filter(sky == "Cloudy") %>% 
  group_by(humidity) %>% 
  summarise(ent = entropy(is_rain),
            is_rain = mean(is_rain),
            n = n()) %>% 
  mutate(n_is_rain = is_rain*n,
         not_is_rain = 1 - is_rain,
         n_not_is_rain = n - n_is_rain) %>% 
  print() %>% 
  summarise(ent = mean(ent))

df %>% 
  count(sky, humidity, temperature, wt = is_rain)
  

df %>% 
  group_by(temperature) %>% 
  summarise(ent = entropy(is_rain))
