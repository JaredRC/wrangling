#*********** Assessment: String Processing Part 1 *************
# Question 1 - Formatting is not parsing

# Question
cat(" LeBron James is 6’8\" ")

# Question 3

# Question 4
dat <- read.table("monthly-sales-profits.tsv")
dat <- setNames(dat,dat[1,])
dat <- setdiff(dat,dat[1,])
dat %>% mutate_at(2:3, parse_number)
dat %>% mutate_at(2:3, as.numeric)
dat %>% mutate_all(parse_number)
dat %>% mutate_at(2:3, funs(str_replace_all(., c("\\$|,"), ""))) %>% 
  mutate_at(2:3, as.numeric)

#************** Assessment: String Processing Part 2 ***********
# Question 1
not_inches <- function(x, smallest = 50, tallest = 84) {
  inches <- suppressWarnings(as.numeric(x))
  ind <- is.na(inches) | inches < smallest | inches > tallest 
  ind
}

# Question 2
not_inches(c(85))

# Question 3
not_inches(c(175,"5'8\"",70,85))

# Question 4
s <- c("70","5 ft","4'11","",".","Six feet")
pattern <- "\\d|ft"
str_view_all(s, pattern)

# Question 5
animals <- c("cat", "puppy", "Moose", "MONKEY")
pattern <- "[a-z]"
str_detect(animals, pattern)

# Question 6
animals <- c("cat", "puppy", "Moose", "MONKEY")
pattern <- "[A-Z]$"
str_detect(animals, pattern)

# Question 7
animals <- c("cat", "puppy", "Moose", "MONKEY")
pattern <- "[a-z]{4,5}"
str_detect(animals, pattern)

# Question 8
animals <- c("moose", "monkey", "meerkat", "mountain lion")
pattern <- "mo*"
pattern <- "mo?"
pattern <- "mo+"
pattern <- "moo*"
str_detect(animals, pattern)

# Question 9
schools <- c("U. Kentucky","Univ New Hampshire",
             "Univ. of Massachusetts","University Georgia",       
             "U California","California State University")
schools %>% 
  str_replace("^Univ\\.?\\s|^U\\.?\\s", "University ") %>%
  str_replace("^University of |^University ", "University of ")

# Question 10
problems <- c("5.3", "5,5", "6 1", "5 .11", "5, 12")
pattern_with_groups <- "^([4-7])[,\\.](\\d*)$"
str_replace(problems, pattern_with_groups, "\\1'\\2")

# Question 11
problems <- c("5.3", "5,5", "6 1", "5 .11", "5, 12")
pattern_with_groups <- "^([4-7])[,\\.\\s](\\d*)$"
str_replace(problems, pattern_with_groups, "\\1'\\2")

# Question 12
converted <- problems %>% 
  str_replace("feet|foot|ft", "'") %>% 
  str_replace("inches|in|''|\"", "") %>% 
  str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2")
pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
index <- str_detect(converted, pattern)
converted[!index]

# Question 13
yes <- c("5 feet 7inches", "5 7")
no <- c("5ft 9 inches", "5 ft 9 inches")
s <- c(yes, no)
converted <- s %>% 
  str_replace("feet|foot|ft", "'") %>% 
  str_replace("inches|in|''|\"", "") %>% 
  str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2")
converted <- s %>% 
  str_replace("\\s*(feet|foot|ft)\\s*", "'") %>% 
  str_replace("\\s*(inches|in|''|\")\\s*", "") %>% 
  str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2")

pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
str_detect(converted, pattern)

#********* Assessment Part 1: String Processing Part 3 ************
# Question 2
schedule <- "Mandy, Chris and Laura"
str_split(schedule, ", | and ")
str_split(schedule, ",\\s|\\sand\\s")

# Question 3
schedule <- data.frame(days = c("Monday","Tuesday"),
                       staff = c("Mandy, Chris and Laura","Steve, Ruth and Frank"))
tidy <- schedule %>% 
  mutate(staff = str_split(staff, ", | and ")) %>% unnest()
tidy <- separate(schedule, staff, into = c("s1","s2","s3"), sep = ",") %>% 
  gather(key = s, value = staff, s1:s3)
tidy <- schedule %>%
  mutate(staff = str_split(staff, ", | and ", simplify = TRUE)) %>% unnest()

# Question 4
library(dslabs)
data("gapminder")
dat <- gapminder %>% filter(region == "Middle Africa") %>% 
  mutate(country_short = recode(country, 
                                "Central African Republic" = "CAR", 
                                "Congo, Dem. Rep." = "DRC",
                                "Equatorial Guinea" = "Eq. Guinea"))

#********* Assessment Part 2: String Processing Part 3 *****************
library(rvest)
library(tidyverse)
library(stringr)
url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
tab <- read_html(url) %>% html_nodes("table")
polls <- tab[[5]] %>% html_table(fill = TRUE)
# Question 5
colls <- c("dates", "remain", "leave", "undecided", "lead", "samplesize", "pollster", "poll_type", "notes")
polls <- setNames(polls, colls)
polls <- polls %>% filter(str_detect(remain, "%"))

# Question 6
as.numeric(str_replace(polls$remain, "%", ""))/100
parse_number(polls$remain)/100

# Question 7
str_replace(polls$undecided,"N/A","0")

# Question 8 - take last element (handles polls that cross month boundaries)
pattern1 <- "\\d?\\s[a-zA-Z]?"
pattern2 <- "\\d+\\s[a-zA-Z]+"
pattern3 <- "\\d+\\s[A-Z]+"
pattern4 <- "[0-9]+\\s[a-zA-Z]+"
pattern5 <- "\\d{1,2}\\s[a-zA-Z]+"
pattern6 <- "\\d{1,2}[a-zA-Z]+"
pattern7 <- "\\d+\\s[a-zA-Z]{3,5}"
temp <- str_extract_all(polls$dates, pattern = pattern7)
end_date <- sapply(temp, function(x) x[length(x)])
end_date
