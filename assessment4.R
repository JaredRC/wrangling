#************* Assessment Part 1: Dates, Times, and Text Mining *********
library(dslabs)
library(lubridate)
options(digits = 3)    # 3 significant digits
# Question 1 - YYYY-MM-DD

# Question 2
dates <- c("09-01-02", "01-12-07", "02-03-04")
dmy(dates)

# Question 3
data(brexit_polls)
brexit_polls %>% filter(month(startdate) == 4)
brexit_polls %>% mutate(week = round_date(enddate,"week") ) %>%
  filter(week == "2016-06-12") %>% select(week)

# Question 4
brexit_polls %>% mutate(weekdy = weekdays(enddate) ) %>% 
  select(weekdy) %>% group_by(weekdy) %>% summarize(n = n())

# Question 5
data(movielens)
hrmax <- movielens %>% mutate(timest = as_datetime(timestamp), 
                     yr = year(timest),
                     hr = hour(timest) ) %>%
  select(timest,yr,hr) %>% group_by(hr) %>% summarize(n = n())
max(yrmax)
max(hrmax)

#************* Assessment Part 2: Dates, Times, and Text Mining ************
library(tidyverse)
library(gutenbergr)
library(tidytext)
options(digits = 3)
# Question 6
gutids <- gutenberg_metadata %>% filter(str_detect(title,"Pride and Prejudice")) %>%
  select(gutenberg_id, title)

# Question 7
gutenberg_works(title == "Pride and Prejudice",languages = "en")

# Question 8
words <- gutenberg_download(1342)
words <- words %>% unnest_tokens(word, text)

# Question 9
words2 <- words %>% filter(!word %in% stop_words$word )

# Question 10
words_nodigits <- words2 %>% filter(str_detect(word,"\\d+",negate = TRUE))

# Question 11
wordgrp <- words_nodigits %>% group_by(word) %>% 
  summarize(n = n()) %>% filter(n > 100)

# Question 12
afinn <- get_sentiments("afinn")
afinn_sentiments <- inner_join(words_nodigits,afinn, by = "word") %>% 
  group_by(word) %>% summarize(value)
val4 <- afinn_sentiments %>% filter(value == 4)
pos <- afinn_sentiments %>% filter(value > 0)
mean(afinn_sentiments$value > 0)
