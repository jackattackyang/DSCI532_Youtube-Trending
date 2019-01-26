library(tidyverse)
library(tidytext)

df <- read_rds("data/clean_df.rds")
dat <- df

tidytitle <- dat %>%
  unnest_tokens(word, title)

my_stopwords <- data_frame(word = c(as.character(1:10), "nhttp", "http",
                                    "https", "nhttps", "bit.ly",
                                    "www.youtube.com", "youtube", "2017",
                                    "2018", "goo.gl", "nfollow", "video",
                                    "videos", "youtu.be", "facebook", "twitter",
                                    "ninstagram", "nfacebook", "ntwitter",
                                    "nsubscribe", "nwatch"))

df_title <- df %>%
  unnest_tokens(word, title) %>%
  anti_join(stop_words) %>%
  anti_join(my_stopwords) %>%
  filter(!str_detect(word, "[^0-9a-zA-Z]"))

df_descript <- df %>%
  unnest_tokens(word, description) %>%
  anti_join(stop_words) %>%
  anti_join(my_stopwords) %>%
  filter(!str_detect(word, "[^0-9a-zA-Z]"))

saveRDS(df_title, file="data/df_title.rds")
saveRDS(df_descript, file="data/df_descript.rds")
