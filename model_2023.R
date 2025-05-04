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
###
library(ggplot2)

ggplot(merged_data, aes(x = avg_sentiment, y = gross)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(title = "Gross vs. Sentiment Score",
       x = "Average Sentiment Score",
       y = "Box Office Gross ($)") +
  theme_minimal()

cor(merged_data$avg_sentiment, merged_data$gross, use = "complete.obs")
model <- lm(gross ~ avg_sentiment, data = merged_data)
summary(model)
