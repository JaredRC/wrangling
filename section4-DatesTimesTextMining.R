#************ Section 4: Dates, Times, and Text Mining ***************
#******** Dates and Times *************
# inspect the startdate column of 2016 polls data, a Date type
library(tidyverse)
library(dslabs)
data("polls_us_election_2016")
polls_us_election_2016$startdate %>% head
class(polls_us_election_2016$startdate)
as.numeric(polls_us_election_2016$startdate) %>% head

# ggplot is aware of dates
polls_us_election_2016 %>% filter(pollster == "Ipsos" & state =="U.S.") %>%
  ggplot(aes(startdate, rawpoll_trump)) +
  geom_line()

# lubridate: the tidyverse date package
library(lubridate)

# select some random dates from polls
set.seed(2)
dates <- sample(polls_us_election_2016$startdate, 10) %>% sort
dates

# extract month, day, year from date strings
data.frame(date = dates, 
           month = month(dates),
           day = day(dates),
           year = year(dates))

month(dates, label = TRUE)    # extract month label

# ymd works on mixed date styles
x <- c(20090101, "2009-01-02", "2009 01 03", "2009-1-4",
       "2009-1, 5", "Created on 2009 1 6", "200901 !!! 07")
ymd(x)

# different parsers extract year, month and day in different orders
x <- "09/01/02"
ymd(x)
mdy(x)
ydm(x)
myd(x)
dmy(x)
dym(x)

now()    # current time in your time zone
now("GMT")    # current time in GMT
now() %>% hour()    # current hour
now() %>% minute()    # current minute
now() %>% second()    # current second

# parse time
x <- c("12:34:56")
hms(x)

#parse datetime
x <- "Nov/2/2012 12:34:56"
mdy_hms(x)


#********************** Text Mining ********************************
#*library(tidyverse)
library(ggplot2)
library(lubridate)
library(tidyr)
library(scales)
set.seed(1)

url <- 'https://drive.google.com/file/d/16wm-2NTKohhcA26w-kaWfhLIGwl_oX95/view'
trump_tweets <- map(2009:2017, ~sprintf(url, .x)) %>%
  map_df(jsonlite::fromJSON, simplifyDataFrame = TRUE) %>%
  filter(!is_retweet & !str_detect(text, '^"')) %>%
  mutate(created_at = parse_date_time(created_at, orders = "a b! d! H!:M!:S! z!* Y!", tz="EST")) 

library(dslabs)
data("trump_tweets")
head(trump_tweets)
names(trump_tweets)
trump_tweets %>% select(text) %>% head
trump_tweets %>% count(source) %>% arrange(desc(n))
trump_tweets %>% 
  extract(source, "source", "Twitter for (.*)") %>%
  count(source) 

campaign_tweets <- trump_tweets %>% 
  extract(source, "source", "Twitter for (.*)") %>%
  filter(source %in% c("Android", "iPhone") &
           created_at >= ymd("2015-06-17") & 
           created_at < ymd("2016-11-08")) %>%
  filter(!is_retweet) %>%
  arrange(created_at)

ds_theme_set()
campaign_tweets %>%
  mutate(hour = hour(with_tz(created_at, "EST"))) %>%
  count(source, hour) %>%
  group_by(source) %>%
  mutate(percent = n / sum(n)) %>%
  ungroup %>%
  ggplot(aes(hour, percent, color = source)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = percent_format()) +
  labs(x = "Hour of day (EST)",
       y = "% of tweets",
       color = "")

library(tidytext)
example <- data_frame(line = c(1, 2, 3, 4),
                        text = c("Roses are red,", "Violets are blue,", "Sugar is sweet,", "And so are you."))
example
example %>% unnest_tokens(word, text)

i <- 3008
campaign_tweets$text[i]
campaign_tweets[i,] %>% 
  unnest_tokens(word, text) %>%
  select(word)

pattern <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"
campaign_tweets[i,] %>% 
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
  select(word)
campaign_tweets[i,] %>% 
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
  select(word)
tweet_words <- campaign_tweets %>% 
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
  unnest_tokens(word, text, token = "regex", pattern = pattern) 
tweet_words %>% 
  count(word) %>%
  arrange(desc(n))

tweet_words <- campaign_tweets %>% 
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
  filter(!word %in% stop_words$word ) 
tweet_words %>% 
  count(word) %>%
  top_n(10, n) %>%
  mutate(word = reorder(word, n)) %>%
  arrange(desc(n))
tweet_words <- campaign_tweets %>% 
    mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
    unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
    filter(!word %in% stop_words$word &
             !str_detect(word, "^\\d+$")) %>%
    mutate(word = str_replace(word, "^'", ""))

android_iphone_or <- tweet_words %>%
  count(word, source) %>%
  spread(source, n, fill = 0) %>%
  mutate(or = (Android + 0.5) / (sum(Android) - Android + 0.5) / 
           ( (iPhone + 0.5) / (sum(iPhone) - iPhone + 0.5)))
android_iphone_or %>% arrange(desc(or))
android_iphone_or %>% arrange(or)

android_iphone_or %>% filter(Android+iPhone > 100) %>%
  arrange(desc(or))

android_iphone_or %>% filter(Android+iPhone > 100) %>%
  arrange(or)

get_sentiments("bing")
get_sentiments("afinn")
get_sentiments("loughran") %>% count(sentiment)
get_sentiments("nrc") %>% count(sentiment)
nrc <- get_sentiments("nrc") %>%
    select(word, sentiment)
tweet_words %>% inner_join(nrc, by = "word") %>% 
    select(source, word, sentiment) %>% sample_n(10)
sentiment_counts <- tweet_words %>%
    left_join(nrc, by = "word") %>%
    count(source, sentiment) %>%
    spread(source, n) %>%
    mutate(sentiment = replace_na(sentiment, replace = "none"))
tweet_words %>% group_by(source) %>% summarize(n = n())
sentiment_counts %>%
    mutate(Android = Android / (sum(Android) - Android) , 
           iPhone = iPhone / (sum(iPhone) - iPhone), 
           or = Android/iPhone) %>%
    arrange(desc(or))

library(broom)
  log_or <- sentiment_counts %>%
    mutate( log_or = log( (Android / (sum(Android) - Android)) / (iPhone / (sum(iPhone) - iPhone))),
            se = sqrt( 1/Android + 1/(sum(Android) - Android) + 1/iPhone + 1/(sum(iPhone) - iPhone)),
            conf.low = log_or - qnorm(0.975)*se,
            conf.high = log_or + qnorm(0.975)*se) %>%
    arrange(desc(log_or))
  
log_or %>%
    mutate(sentiment = reorder(sentiment, log_or),) %>%
    ggplot(aes(x = sentiment, ymin = conf.low, ymax = conf.high)) +
    geom_errorbar() +
    geom_point(aes(sentiment, log_or)) +
    ylab("Log odds ratio for association between Android and sentiment") +
    coord_flip() 
android_iphone_or %>% inner_join(nrc) %>%
    filter(sentiment == "disgust" & Android + iPhone > 10) %>%
    arrange(desc(or))
android_iphone_or %>% inner_join(nrc, by = "word") %>%
    mutate(sentiment = factor(sentiment, levels = log_or$sentiment)) %>%
    mutate(log_or = log(or)) %>%
    filter(Android + iPhone > 10 & abs(log_or)>1) %>%
    mutate(word = reorder(word, log_or)) %>%
    ggplot(aes(word, log_or, fill = log_or < 0)) +
    facet_wrap(~sentiment, scales = "free_x", nrow = 2) + 
    geom_bar(stat="identity", show.legend = FALSE) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
  
  