library(jsonlite)
library(tidyverse)
library(listviewer)

file = "data/US_category_id.json"
df = read_csv("data/USvideos/USvideos.csv")

category_id <- fromJSON(file)

id <- category_id[["items"]][["id"]]
id <- as.integer(id) 

category <- category_id[["items"]][["snippet"]][["title"]]

category_df <- tibble(
  "id" = id,
  "category" = category
)

df <- df %>% left_join(category_df, by=c("category_id" = "id"))

df %>% View()

saveRDS(df, file="data/clean_df.rds")