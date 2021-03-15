# load packages ----------------------------------------------------------------
library(tidyverse)
library(rvest)
library("robotstxt")

# check if web scraping is allowed
paths_allowed(
  domain = "http://www.imdb.com/chart/tvmeter"
)

# read in http://www.imdb.com/chart/tvmeter ------------------------------------
page2 <- read_html("http://www.imdb.com/chart/tvmeter")
typeof(page2)
class(page2)

# titles
titles <- page2 %>% 
  html_nodes(".titleColumn a") %>% 
  html_text()
titles

# years ------------------------------------------------------------------------
years <- page2 %>%
  html_nodes("a+ .secondaryInfo") %>%
  html_text() %>%
  str_remove("\\(") %>%  # remove (
  str_remove("\\)") %>%  # remove )
  as.numeric()
years

# scores -----------------------------------------------------------------------
scores <- page2 %>%
  html_nodes("strong") %>%
  html_text(trim = TRUE) %>% replace(!nzchar(.), NA) %>% 
  as.numeric()
scores

scores <- page2 %>%
  html_nodes(".imdbRating") %>%
  html_text() %>% 
  as.numeric()
scores

# tvshows dataframe ------------------------------------------------------------
tvshows <- tibble(
  rank = 1:100,
  name = titles,
  score = scores,
  year = years

)

# add new variables ------------------------------------------------------------
tvshows <- tvshows %>%
  mutate(
    genre = NA,
    runtime = NA,
    n_episode = NA,
    keywords = NA
  )
glimpse(tvshows)
typeof(tvshows$keywords)

# add new info for first show --------------------------------------------------
tvshows$genre[1] <- "Action"
tvshows$runtime[1] <- "30"
tvshows$n_episode[1] <- "9"
tvshows$keywords[[1]] <- c("Wanda", "Vision", "conceal", "powers", "boss")

# add new info for second show --------------------------------------------------
tvshows$genre[2] <- "Comedy"
tvshows$runtime[2] <- "58"
tvshows$n_episode[2] <- "10"
tvshows$keywords[[2]] <- c("Georgia Miller", "husband", "car accident", "death", "Wellsbury")

# add new info for third show --------------------------------------------------
tvshows$genre[3] <- "Thriller"
tvshows$runtime[3] <- "67"
tvshows$n_episode[3] <- "177"
tvshows$keywords[[3]] <- c("Rick Grimes", "coma", "search", "family", "undead")

glimpse(tvshows)

