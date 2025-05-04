library(httr)
library(jsonlite)
library(dplyr)
library(readr)
library(tidytext)

# 设置 API Key
api_key <- "AIzaSyAPGgSFlZNVbCBaBXrx9Sfs-pamFjmUQlA"

# 读取你上传的文件
movies_df <- read_csv("C:/Users/polynLin/Desktop/movies_2023_tidy.csv")

# 定义改进版函数
get_comments_safe <- function(movie_title, video_id, api_key, max_results = 100) {
  cat("Fetching comments for:", movie_title, "\n")
  
  base_url <- "https://www.googleapis.com/youtube/v3/commentThreads"
  url <- paste0(base_url, "?part=snippet&videoId=", video_id, 
                "&maxResults=", max_results, "&order=relevance&key=", api_key)
  
  res <- tryCatch(GET(url), error = function(e) return(NULL))
  
  if (is.null(res) || status_code(res) != 200) {
    warning(paste("Failed to fetch for video ID:", video_id))
    return(data.frame(
      Movie = movie_title,
      VideoID = video_id,
      DatePosted = NA,
      Comment = NA
    ))
  }
  
  data <- fromJSON(content(res, "text", encoding = "UTF-8"))
  
  if (!is.null(data$items) && length(data$items) > 0) {
    dates <- sapply(data$items$snippet$topLevelComment$snippet$publishedAt, function(x) substr(x, 1, 10))
    comments <- sapply(data$items$snippet$topLevelComment$snippet$textDisplay, identity)
    comments_df <- data.frame(
      Movie = movie_title,
      VideoID = video_id,
      DatePosted = as.Date(dates, format = "%Y-%m-%d"),
      Comment = comments,
      stringsAsFactors = FALSE
    )
  } else {
    comments_df <- data.frame(
      Movie = movie_title,
      VideoID = video_id,
      DatePosted = NA,
      Comment = NA
    )
  }
  
  return(comments_df)
}

# 批量抓取所有电影评论
all_comments_list <- lapply(1:nrow(movies_df), function(i) {
  get_comments_safe(movies_df$movie[i], movies_df$link[i], api_key)
})

# 合并为单个数据框
all_comments_df <- bind_rows(all_comments_list)

# 去除评论为 NA 或空字符串的行
cleaned_comments_df <- all_comments_df %>%
  filter(!is.na(Comment) & Comment != "")

# 查看前几行
print(head(cleaned_comments_df, 10))

# 可选：保存结果
write_csv(cleaned_comments_df, "movies_youtube_comments.csv")
####

library(tidytext)
library(dplyr)
library(readr)
library(stringr)
# 获取 AFINN 情感词典
afinn <- get_sentiments("afinn")
# 拆分评论为单词（token）
comments_tokenized <- cleaned_comments_df %>%
  unnest_tokens(word, Comment) %>%
  inner_join(afinn, by = "word")
# 按电影汇总平均情绪分数
movie_sentiment_scores <- comments_tokenized %>%
  group_by(Movie) %>%
  summarise(avg_sentiment = mean(value, na.rm = TRUE),
            total_words = n()) %>%
  arrange(desc(avg_sentiment))
print(head(movie_sentiment_scores, 10))
write_csv(movie_sentiment_scores, "C:/Users/polynLin/Desktop/movie_sentiment_scores.csv")
####
library(dplyr)
library(readr)

# 读取两个数据文件
sentiment_scores <- read_csv("C:/Users/polynLin/Desktop/movie_sentiment_scores.csv")
movies_info <- read_csv("C:/Users/polynLin/Desktop/movies_2023_tidy.csv")

# 合并：将 gross 添加到情感得分数据中
merged_data <- sentiment_scores %>%
  left_join(movies_info %>% select(movie, gross), by = c("Movie" = "movie"))

# 查看合并结果前几行
head(merged_data)

# 可选：保存合并后的结果
write_csv(merged_data, "C:/Users/polynLin/Desktop/merged_movie_sentiment_gross.csv")
