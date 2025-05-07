library(tidyverse)
library(ggplot2)
library(plotly)
library(ggrepel)

#Sentiment Analysis and Model

movies_2023_tidy <- tibble(read.csv("movies_2023_tidy") %>% select(movie,gross,score = sentiment_score))
movies_2023_tidy

movies_2024_tidy <- tibble(read.csv("movies_2024_tidy") %>% select(movie,gross,score))
movies_2024_tidy

movies_2023_combine <- movies_2023_tidy %>% mutate(year = 2023)
movies_2024_combine <- movies_2024_tidy %>% mutate(year = 2024)
combined_movies <- rbind(movies_2023_combine,movies_2024_combine)
combined_movies

#Trend of 2023 Data
trend_2023 <- ggplot(movies_2023_tidy, aes(x = score, y = log(gross))) +
  geom_point(color = "blue", alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(
    title = "2023 Sentiment Score vs Log(Gross)",
    x = "Sentiment Score",
    y = "Log(Gross Revenue)"
  ) +
  theme_minimal()

png("trend_2023.png", width = 800, height = 600)
plot(cars)
dev.off()

#2024 trend
ggplot(movies_2024_tidy, aes(x = score, y = log(gross))) +
  geom_point(color = "blue", alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(
    title = "2024 Sentiment Score vs Log(Gross)",
    x = "Sentiment Score",
    y = "Log(Gross Revenue)"
  ) +
  theme_minimal()

#combined trend
extreme_points <- combined_movies %>%
  filter(score > quantile(score, 0.99) | score < quantile(score, 0.01) | 
           gross > quantile(gross, 0.99) | gross < quantile(gross, 0.01))

 ggplot(combined_movies, aes(x = score, y = log(gross))) +
  geom_point(color = "blue", alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
   geom_point(data = extreme_points, aes(x = score, y = log(gross)),
              color = "orange", size = 3, shape = 17) +  # Highlight extreme points
   geom_text_repel(data = extreme_points, aes(x = score, y = log(gross), label = movie), 
             color = "black", size = 3, vjust = -1, hjust = 1) +
  labs(
    title = "2023-2024 Sentiment Score vs Log(Gross)",
    x = "Sentiment Score",
    y = "Log(Gross Revenue)"
  ) +
  theme_minimal()

#Select the top 15 positive review movies
top_sentiment_movies <- combined_movies %>%
  filter(!is.na(score)) %>%
  slice_max(order_by = score, n = 15)

top_sentiment_movies <- top_sentiment_movies[-12,]

ggplot(top_sentiment_movies, aes(x = reorder(movie, score), y = score)) +
  geom_col(fill = "lightgreen") +
  coord_flip() +
  labs(
    title = "Top 15 Movies by Sentiment Score",
    x = "Movie",
    y = "Sentiment Score (positive - negative)"
  ) +
  theme_minimal()

#The 15 lowest sentiment score 
low_sentiment_movies <- combined_movies %>%
  filter(!is.na(score)) %>%
  slice_min(order_by = score, n = 15)

ggplot(low_sentiment_movies, aes(x = reorder(movie, score), y = score)) +
  geom_col(fill = "tomato") +
  coord_flip() +
  labs(
    title = "Bottom 15 Movies by Sentiment Score",
    x = "Movie",
    y = "Sentiment Score (positive - negative)"
  ) +
  theme_minimal()

#Predictive linear model

model <- lm(gross ~ score, data = movies_2023_tidy)
summary(model)

prediction <- predict(model, newdata = movies_2024_tidy)

predicted <- data.frame(actual = movies_2024_tidy$gross, predicted = prediction)

ggplot(predicted,aes(x = actual, y = predicted)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(
    title = "2024 Movies Actual vs Predicted Gross Income",
    x = "Actual Gross Income",
    y = "Predicted Gross Income"
  )
