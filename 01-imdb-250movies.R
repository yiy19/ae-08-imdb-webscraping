# Load packages ---------------------------------------------------------------
library(tidyverse)
library(rvest)
library("robotstxt")

# Determine whether web scraping is allowed 
paths_allowed(
  domain = "https://www.imdb.com/chart/top"
)

# Read html page ---------------------------------------------------------------
page <- read_html("https://www.imdb.com/chart/top")
typeof(page)
class(page)

page %>% 
  html_nodes(".titleColumn a") %>% 
  html_text()

# Titles -----------------------------------------------------------------------
titles <- page %>%
  html_nodes(".titleColumn a") %>%
  html_text()
titles

# Years-------------------------------------------------------------------------
years <- page %>%
  html_nodes(".secondaryInfo") %>%
  html_text() %>%
  str_remove("\\(") %>%  # remove (
  str_remove("\\)") %>%  # remove )
  as.numeric()
years

# Scores -----------------------------------------------------------------------
scores <- page %>%
  html_nodes("#main strong") %>%
  html_text() %>%
  as.numeric()
scores

# Put it all in a data frame ---------------------------------------------------
imdb_top_250 <- tibble(
  title = titles,
  year = years,
  rating = scores
)
glimpse(imdb_top_250)

# Add variable for rank
imdb_top_250 <- imdb_top_250 %>% 
  mutate(rank = 1:nrow(imdb_top_250)) %>% 
  relocate(rank)
imdb_top_250

# Which years have the most movies on the list?
imdb_top_250 %>% 
  count(year, sort = TRUE) # sort = TRUE begins the rows with the largest n

# Which 1995 movies made the list?
imdb_top_250 %>% 
  filter(year == 1995) %>% 
  print()

# Visualize the average yearly rating for movies that made it on the top 250 list over time.
imdb_top_250 %>% 
  group_by(year) %>%
  summarize(avg_score = mean(rating)) %>%
  ggplot(aes(y = avg_score, x = year)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Year", y = "Average score")



