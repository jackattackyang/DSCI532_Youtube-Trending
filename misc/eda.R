library(tidyverse)
library(lubridate)
library(scales)

df <- readRDS("data/clean_df.rds")

df$category <- df$category %>% as_factor() 

df %>% 
  count(category, wt=likes) %>% 
  arrange(n)

df %>% 
  group_by(category) %>% 
  count()

df %>% 
  select(category, views, likes, dislikes) %>% 
  filter(category %in% "Nonprofits & Activism") %>% 
  arrange(desc(views))

df %>% 
  group_by(category) %>% 
  summarise(likes = sum(as.numeric(views)), n = n(), avg = likes/n) %>% 
  ggplot() + 
  geom_col(aes(fct_reorder(category, avg), avg)) +
  scale_y_continuous(labels = comma) +
  labs(x="", y="Likes per Video") +
  coord_flip()

df %>% 
  ggplot() +
  geom_boxplot(aes(fct_reorder(category, views), views)) +
  scale_y_log10(labels = comma) +
  labs(x="", y="Likes per Video") +
  coord_flip()

df %>% 
  select(publish_time, category) %>% 
  ggplot() + geom_bar(aes(wday(publish_time, label = TRUE)))

df %>% 
  select(publish_time, category) %>% 
  ggplot() + geom_bar(aes(mday(publish_time)))

df %>% 
  select(publish_time, category) %>% 
  mutate(hours = hour(publish_time),
         minutes = minute(publish_time),
         seconds = second(publish_time),
         time = make_datetime(hour = hours, min = minutes, sec = seconds)) %>% 
  ggplot() + geom_freqpoly(aes(time)) +
  scale_x_datetime(date_breaks = "3 hours", date_labels = "%H:%M")
