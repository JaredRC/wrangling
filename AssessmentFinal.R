#***** Comprehensive Assessment: Puerto Rico Hurricane Mortality ********
library(tidyverse)
library(pdftools)
options(digits = 3)    # report 3 significant digits
#******* Puerto Rico Hurricane Mortality: Part 1 ********
# Question 1
fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")
system2("open", args = fn)

# Question 2
txt <- pdf_text(fn)

# Question 3
x <- str_split(txt[9],"\n")
class(x)

# Question 4
s <- x[[1]]
class(s)

# Question 5
s <- str_trim(s)
s[1]

# Question 6
header_index <- str_which(s,"2015")

# Question 7
header <- s[header_index]
head2 <- str_split(header[1],"\\s+", simplify = TRUE)
month <- head2[1]
month
head2[3]

# Question 8
tail_index <- str_which(s,"Total")

# Question 9
n <- str_count(s,pattern = "\\d+")
sum(n == 1)

# Question 10
onenum_index <- which(n == 1)
out <- c(1:header_index[1], which(n==1), tail_index:length(s))
s2 <- s[-out]

# Question 11
s3 <- s2 %>% str_remove_all("[^\\d\\s]")

# Question 12
s3 <- str_split_fixed(s3, "\\s+", n = 6)[,1:5]
tab <- data.frame(s3) %>% 
  setNames(c("day",header)) %>%
  mutate(month = month, day = as.numeric(day),
         `2015` = as.numeric(`2015`),`2016` = as.numeric(`2016`),
         `2017` = as.numeric(`2017`),`2018` = as.numeric(`2018`))
mean(tab$`2015`)
mean(tab$`2016`)
beforeSep20_2017 <- tab %>% filter(day < 20) %>% .$`2017`
mean(beforeSep20_2017)
posthurricane <- tab %>% filter(day >= 20) %>% .$`2017`
mean(posthurricane)

# Question 13
tab <- tab %>% gather(year, deaths, -day) %>%
  mutate(deaths = as.numeric(deaths))
tab

# Question 14
tab %>% filter(year < 2018) %>% group_by(year) %>%
  ggplot(aes(day,deaths)) + 
  geom_point(aes(color = year)) +
  geom_vline(xintercept = 20)
  
