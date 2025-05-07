library(tidyverse)
library(httr)
library(jsonlite)
library(tidytext)

#2024 Cleaning

#Box Office Mojo: Box Office Numbers
library(rvest)
url="https://www.boxofficemojo.com/year/2024/"

#Web scraping site for top grossing movies list
movies_mojo_2024 <- url %>% read_html() %>% html_elements("table")%>% html_table(fill = TRUE) %>% pluck(1)

#Data tidying; updating release date, gross revenue
movies_mojo_2024_tidy <- movies_2024 %>% mutate(release_date = as.Date(paste(movies_2024$`Release Date`, "2024"),"%b %d %Y")) %>% 
  mutate(gross = as.numeric(str_remove_all(Gross,"[$,]"))) %>% select(movie = "Release",gross,release_date)

#YouTube Codes
youtube_links <- readxl::read_xlsx("Youtube Links 2024.xlsx") %>% 
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

write.csv(movies_2024,"movies_2024_tidy")

#sentiment visualization
nrc_sentiment <- get_sentiments("nrc")

min_sentiment <- movies_with_links %>% filter(score == min(score))
max_sentiment <- movies_with_links %>% filter(score == max(score))
inner_join(max_sentiment$data[[1]],nrc_sentiment) %>% 
  group_by(sentiment) %>% summarise(count = n()) %>% ggplot(aes(x = sentiment, y = count, fill=sentiment)) +
  geom_bar(stat = "identity")

library(plotly)
ggplotly(movies_with_links %>% 
           ggplot(aes(x=score,y=log(gross),color=score, text=paste("Movie:", movie))) + 
           scale_color_gradient(low = "red", high="green")+
           geom_point(),tooltip = "text")

#afinn scoring
afinn_sentiment <- get_sentiments("afinn")
for (i in 1:nrow(movies_with_links)) {
  sentiment_score <- movies_with_links$data[[i]] %>%
    inner_join(afinn_sentiment, by = "word") %>% 
    summarise(avg_sentiment = mean(value, na.rm = TRUE),total_words = n())
  movies_with_links$avg_sentiment[i] <- sentiment_score$avg_sentiment
}

movies_with_links

sentiment_2024 <- movies_with_links %>% select(movie,gross,score)

sentiment_2024 %>% ggplot(aes(x=log(gross),y=score)) + geom_point()

#for bottom of model
pred <- predict(model, newdata = movies_with_links)
res <- log(movies_with_links$gross) - pred

mse <- mean(res^2)
mse
range(log(movies_with_links$gross))
