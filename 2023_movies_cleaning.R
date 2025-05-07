library(tidyverse)
library(httr)
library(jsonlite)
library(tidytext)
library(usethis)
library(dplyr)
library(readr)
library(rvest)

#1
# scraping data from mojo and save as a file
url="https://www.boxofficemojo.com/year/2023/"

movies_2023 <- url %>% read_html() %>% html_elements("table")%>% html_table(fill = TRUE) %>% pluck(1)

#movies_2023
# filter movie, gross, release_date and save as a new file
movies_2023_MGRD <- movies_2023 %>% mutate(release_date = as.Date(paste(movies_2023$`Release Date`, "2023"),"%b %d %Y")) %>% 
  mutate(gross = as.numeric(str_remove_all(Gross,"[$,]"))) %>% select(movie = "Release",gross,release_date)

#movies_2023_MGRD
#write_csv(movies_2023_MGRD, "C:/Users/polynLin/Desktop/mypart/movies_2023_MGRD.csv")

#read the sheet
movies_2023_MGRDL <- read_csv("data/movies&link&gross&releaseddate.csv")

#combine these two sheet into one
colnames(movies_2023_MGRDL) <- tolower(colnames(movies_2023_MGRDL))
colnames(movies_2023_MGRD) <- tolower(colnames(movies_2023_MGRD))

movies_2023_MGRDL <- movies_2023_MGRDL %>%
  mutate(release_date = as.Date(release_date, format = "%d/%m/%Y"))
movies_2023_MGRD <- movies_2023_MGRD %>%
  mutate(release_date = as.Date(release_date))

# combine them and save the value that are similar, and keep the values from scraped sheet if any two values are not the same
#the new sheet has movie, gross, release_date, link
movies_merged_final<- full_join(movies_2023_MGRDL, movies_2023_MGRD, by = "movie") %>%
  mutate(
    gross = coalesce(movies_2023_MGRD$gross, movies_2023_MGRDL$gross),
    release_date = coalesce(movies_2023_MGRD$release_date, movies_2023_MGRDL$release_date),
    link = movies_2023_MGRDL$link
  ) %>%
  select(movie, gross, release_date, link)

# save as a new CSV
#write_csv(movies_merged_final, "C:/Users/polynLin/Desktop/mypart/movies_merged_final.csv")


#2scraping comments from YouTube
#getting the data from API
library(httr)
library(jsonlite)
library(dplyr)

movies_merged_final$release_date <- as.Date(movies_merged_final$release_date)

movies_merged_final$data <- vector("list", nrow(movies_merged_final))

api_key = "AIzaSyDIjgqQFmPSa9ExEfy0mHDYI7KOfO-M3OA"

# clean the link values 
movies_merged_final$link <- gsub('^["\']+|["\']+$', '', movies_merged_final$link)

movies_merged_final <- movies_merged_final %>%
  filter(tolower(link) != "link")

#scarping comments
for (i in 1:nrow(movies_merged_final)) {
  cat("Processing:", movies_merged_final$movie[i], "\n")
  
  video_id <- movies_merged_final$link[i]
  release_date <- movies_merged_final$release_date[i]
  
  url <- paste0("https://www.googleapis.com/youtube/v3/commentThreads?part=snippet&videoId=", 
                video_id, "&maxResults=100&order=relevance&key=", api_key)
  
  res <- tryCatch(GET(url), error = function(e) return(NULL))
  
  if (!is.null(res) && status_code(res) == 200) {
    data <- fromJSON(content(res, "text", encoding = "UTF-8"))
    
    if (!is.null(data$items) && length(data$items) > 0) {
      comments <- data.frame(
        DatePosted = as.Date(sapply(data$items$snippet$topLevelComment$snippet$publishedAt, substr, 1, 10)),
        Comment = sapply(data$items$snippet$topLevelComment$snippet$textDisplay, identity),
        stringsAsFactors = FALSE
      )
      
      date_filtered <- comments %>%
        filter(DatePosted < release_date)
      #filter the movies
      movies_merged_final$data[[i]] <- date_filtered
    }
  }
}

movies_merged_final

# check the comments number of movies
#sorted_movies <- movies_merged_final %>%
#  arrange(comment_count) %>%
#  select(movie, comment_count)
#head(sorted_movies, 10)

# renew the number of comments
movies_merged_final$comment_count <- sapply(movies_merged_final$data, function(x) length(x$Comment))

# filter comments==0 or movies include "re-release" 
filtered_movies <- movies_merged_final %>%
  filter(comment_count > 0) %>%
  filter(!grepl("re[- ]release", movie, ignore.case = TRUE)) %>%
  arrange(comment_count) %>%
  select(movie, comment_count)
head(filtered_movies, 10)
#write_csv(filtered_movies, "C:/Users/polynLin/Desktop/mypart/filtered_movies.csv")

#3
library(tidytext)
library(dplyr)
library(tidyr)

# loading "bing" stop words
bing <- get_sentiments("bing")
data("stop_words")
# combine all these comments
all_comments <- movies_merged_final %>%
  filter(lengths(data) > 0) %>%
  select(movie, data) %>%
  unnest(cols = c(data)) %>%
  filter(!is.na(Comment) & Comment != "")
# split comments
comments_tokenized <- all_comments %>%
  unnest_tokens(word, Comment) %>%
  anti_join(stop_words, by = "word") %>%
  inner_join(bing, by = "word")
# count the numbers of words
movie_sentiment_stats <- comments_tokenized %>%
  count(movie, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  mutate(
    sentiment_score = positive - negative,
    total_words = positive + negative,
    avg_sentiment = sentiment_score / total_words
  ) %>%
  arrange(desc(avg_sentiment))

movies_final_sentiment <- filtered_movies %>%
  left_join(movie_sentiment_stats, by = "movie")
head(movies_final_sentiment, 10)

movies_final_model <- movies_final_sentiment %>%
  left_join(movies_merged_final %>% select(movie, gross), by = "movie") %>%
  filter(!is.na(sentiment_score), !is.na(gross), gross > 0) %>%
  mutate(log_gross = log(gross))

movies_final_model

#write_csv(movies_final_model, "data/movies_2023_tidy")

