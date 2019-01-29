library(tidyverse)
library(wordcloud2)
library(tidytext)

df <- read_rds("data/clean_df.rds")

my_stopwords <- c(as.character(1:10), "nhttp", "http",
                                    "https", "nhttps", "bit.ly",
                                    "www.youtube.com", "youtube", "2017",
                                    "2018", "goo.gl", "nfollow", "video",
                                    "videos", "youtu.be", "facebook", "twitter",
                                    "ninstagram", "nfacebook", "ntwitter",
                                    "nsubscribe", "nwatch", "n n n", "iqid", "n n", " n ", " n", "n ", "ncheck", ".com", 
                                    ".to", ".st", ".co", "www", "it's", "i'm", ".it", ".pt", "don't", "haven't",
                                    ".ly", ".uk", ".pn")
my_stopwords <- paste(my_stopwords, collapse="|")

stop_words <- stop_words %>% select(ngram=word)

df_title_1 <- df %>%
  unnest_tokens(ngram, title, token = "ngrams", n = 1) %>% 
  select(ngram, category) %>% 
  anti_join(stop_words) %>% 
  filter(!str_detect(ngram, my_stopwords)) %>% 
  filter(str_detect(ngram, "[^0-9a-zA-Z]"))

df_descript_1 <- df %>%
  unnest_tokens(ngram, description, token = "ngrams", n = 1) %>% 
  select(ngram, category) %>% 
  anti_join(stop_words) %>% 
  filter(!str_detect(ngram, my_stopwords)) %>% 
  filter(str_detect(ngram, "[^0-9a-zA-Z]"))

df_title_2 <- df %>%
  unnest_tokens(ngram, title, token = "ngrams", n = 2) %>% 
  select(ngram, category)%>% 
  filter(!str_detect(ngram, my_stopwords)) %>% 
  filter(str_detect(ngram, "[^0-9a-zA-Z]"))

df_title_3 <- df %>%
  unnest_tokens(ngram, title, token = "ngrams", n = 3) %>% 
  select(ngram, category) %>% 
  filter(!str_detect(ngram, my_stopwords)) %>% 
  filter(str_detect(ngram, "[^0-9a-zA-Z]"))

df_descript_2 <- df %>%
  unnest_tokens(ngram, description, token = "ngrams", n = 2) %>% 
  select(ngram, category) %>% 
  filter(!str_detect(ngram, my_stopwords)) %>% 
  filter(str_detect(ngram, "[^0-9a-zA-Z]"))

df_descript_3 <- df %>%
  unnest_tokens(ngram, description, token = "ngrams", n = 3) %>% 
  select(ngram, category) %>% 
  filter(!str_detect(ngram, my_stopwords)) %>% 
  filter(str_detect(ngram, "[^0-9a-zA-Z]"))

saveRDS(df_title_1, file="data/df_title_1.rds")
saveRDS(df_title_2, file="data/df_title_2.rds")
saveRDS(df_title_3, file="data/df_title_3.rds")

saveRDS(df_descript_1, file="data/df_descript_1.rds")
saveRDS(df_descript_2, file="data/df_descript_2.rds")
saveRDS(df_descript_3, file="data/df_descript_3.rds")
