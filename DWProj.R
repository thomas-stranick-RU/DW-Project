library(tidyverse)
library(httr)
library(jsonlite)
library(tidytext)
library(usethis)

use_git()
use_github()

#box office numbers
library(rvest)
url="https://www.boxofficemojo.com/year/2024/"

movies_2024 <- url %>% read_html() %>% html_elements("table")%>% html_table(fill = TRUE) %>% pluck(1)

movies_2024_tidy <- movies_2024 %>% mutate(release_date = as.Date(paste(movies_2024$`Release Date`, "2024"),"%b %d %Y")) %>% 
  mutate(gross = as.numeric(str_remove_all(Gross,"[$,]"))) %>% select(movie = "Release",gross,release_date)

movies_2024_tidy

#setting up youtube links

youtube_links <- readxl::read_xlsx("Data Wrangling Project.xlsx") %>% 
  mutate(code = str_remove_all(link, "https?://(www\\.)?youtube\\.com/watch\\?v=")) %>% 
  select(movie,code,rottentomato)

youtube_links

#left join to keep only movies with links

movies_with_links <- left_join(youtube_links,movies_2024_tidy, by= join_by(movie))

#getting the data from API

api_key = "AIzaSyDIjgqQFmPSa9ExEfy0mHDYI7KOfO-M3OA"

for (i in 1:nrow(youtube_links)) {
  url <- paste0("https://www.googleapis.com/youtube/v3/commentThreads?part=snippet&videoId=", 
                movies_with_links$code[i], "&maxResults=100&order=relevance&key=", api_key)
  
  res <- GET(url)
  data <- fromJSON(content(res, "text"))
  
  comments <- data.frame(
    DatePosted = as.Date(unlist(data$items$snippet$topLevelComment$snippet$publishedAt)),
    Comment = unlist(data$items$snippet$topLevelComment$snippet$textDisplay)
  )

  date_filtered <- comments %>% 
    filter(DatePosted < movies_with_links$release_date[i])
  
  movies_with_links$data[i] <- list(date_filtered)
}

print(movies_with_links,n=45)

#denest by token and remove all timestamps, non-words, and stop words

for (i in 1:nrow(movies_with_links)) {
  movies_with_links$data[[i]] <- movies_with_links$data[[i]] %>% select(Comment) %>% 
    mutate(Comment = str_remove_all(Comment,"<a[^>]*?>.*?</a>")) %>% 
    unnest_tokens(word, Comment) %>%
    anti_join(stop_words) %>% 
    mutate(word = str_replace_all(word,"[^[:alpha:]\\s]","")) %>% 
    filter(word != "")
}

#get the overall sentiment score
bing_sentiment <- get_sentiments("bing")
for (i in 1:nrow(movies_with_links)) {
  sentiment_scores <- movies_with_links$data[[i]] %>%
    inner_join(bing_sentiment, by = "word") %>%
    count(sentiment)
  movies_with_links$score[i] <- sentiment_scores$n[2] - sentiment_scores$n[1]
}

print(movies_with_links,n=45)

movies_with_links %>% ggplot(aes(x=gross,y=score)) + geom_point()
