library(tidyverse)
library(httr)
library(jsonlite)
library(tidytext)
library(rvest)

#2024 Cleaning

#Box Office Mojo: Box Office Numbers

url="https://www.boxofficemojo.com/year/2024/"

#Web scraping site for top grossing movies list
movies_mojo_2024 <- url %>% read_html() %>% html_elements("table")%>% html_table(fill = TRUE) %>% pluck(1)

#Data tidying; updating release date, gross revenue
movies_mojo_2024_tidy <- movies_mojo_2024 %>% mutate(release_date = as.Date(paste(movies_mojo_2024$`Release Date`, "2024"),"%b %d %Y")) %>% 
  mutate(gross = as.numeric(str_remove_all(Gross,"[$,]"))) %>% select(movie = "Release",gross,release_date)

#YouTube Codes
youtube_links <- readxl::read_xlsx("data/Youtube Links 2024.xlsx") %>% 
  mutate(code = str_remove_all(link, "https?://(www\\.)?youtube\\.com/watch\\?v=")) %>% 
  select(movie,code)

#left join to merge mojo table with YouTube codes
movies_mojo_codes <- left_join(youtube_links,movies_mojo_2024_tidy, by= join_by(movie))

#YouTube API
api_key = "AIzaSyDIjgqQFmPSa9ExEfy0mHDYI7KOfO-M3OA"

movies_gross_comments <- movies_mojo_codes %>% select(movie,gross)

#for each movie, getting comments, filtering by release date of movie
for (i in 1:nrow(movies_mojo_codes)) {
  url <- paste0("https://www.googleapis.com/youtube/v3/commentThreads?part=snippet&videoId=", 
                movies_mojo_codes$code[i], "&maxResults=100&order=relevance&key=", api_key)
  
  res <- GET(url)
  data <- fromJSON(content(res, "text"))
  
  comments <- data.frame(
    DatePosted = as.Date(unlist(data$items$snippet$topLevelComment$snippet$publishedAt)),
    Comment = unlist(data$items$snippet$topLevelComment$snippet$textDisplay)
  )

  date_filtered <- comments %>% 
    filter(DatePosted < movies_mojo_codes$release_date[i])
  
  movies_gross_comments$data[i] <- list(date_filtered)
}

#head(movies_gross_comments$data[[1]],3)

#removing release date and YouTube code
movies_sentiment <- movies_gross_comments %>% select(movie,gross)

#denest by token and remove all timestamps, non-words, and stop words
for (i in 1:nrow(movies_gross_comments)) {
  movies_sentiment$data[[i]] <- movies_gross_comments$data[[i]] %>% select(Comment) %>% 
    mutate(Comment = str_remove_all(Comment,"<a[^>]*?>.*?</a>")) %>% 
    unnest_tokens(word, Comment) %>%
    anti_join(stop_words) %>% 
    mutate(word = str_replace_all(word,"[^[:alpha:]\\s]","")) %>% 
    filter(word != "")
}

#get the overall sentiment score with bing
bing_sentiment <- get_sentiments("bing")
for (i in 1:nrow(movies_sentiment)) {
  sentiment_scores <- movies_sentiment$data[[i]] %>%
    inner_join(bing_sentiment, by = "word") %>%
    count(sentiment)
  movies_sentiment$score[i] <- sentiment_scores$n[2] - sentiment_scores$n[1]
}
movies_2024 <- movies_sentiment %>% select(movie,gross,score)

write.csv(movies_2024,"data/movies_2024_tidy.csv")

