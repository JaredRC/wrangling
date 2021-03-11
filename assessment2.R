#*************** Assessment 2.1 ***************
# Question 3
d <- read_csv("times.csv")
d %>% gather(year,time,`2015`:`2017`)

# Question 4
test_grid <- read_xlsx("Disease.xlsx")
head(test_grid)
dis <- test_grid %>% gather(Disease,Count,`HepatitisA`:`Rubella`)
head(dis)
dat_tidy <- test_grid %>% gather(key = disease, value = count, HepatitisA:Rubella)

# Question 5
times <- read_csv("times.csv")
times_tidy <- times %>% gather(year,time,`2015`:`2017`)
times_tidy %>% spread(year,time)

# Question 6
m <- read.csv("popu.csv")
m %>% spread(key = var, value = people)

# Question 7
d <- read_csv("times2.csv")
d %>% gather(key = "key", value = "value", -age_group) %>%
  separate(key, into = c("year","varname"), sep = "_") %>%
  spread(key = varname, value = value)

# Question 8
head(stats)
dim <- read_tsv("dimensions.tab")
dim %>% separate(key,into = c("player","varname"),sep = "_") %>%
  spread(key = varname, value = value)
dim %>% 
  separate(col = key, into = c("player", "varname"), sep = "_", extra = "merge") %>% 
  spread(key = varname, value = value)
dim %>%
  separate(col = key, into = c("player", "variable_name1", "variable_name2"), sep = "_", fill = "right") %>% 
  unite(col = variable_name, variable_name1, variable_name2, sep = "_") %>% 
  spread(key = variable_name, value = value)

#************** Assessment Part 2: Reshaping Data ****************
library(tidyverse)
library(dslabs)
# Question 9
co2

# Question 10
co2_wide <- data.frame(matrix(co2, ncol = 12, byrow = TRUE)) %>% 
  setNames(1:12) %>%
  mutate(year = as.character(1959:1997))
co2_tidy <- gather(co2_wide,month,co2,-year)

# Question 11
co2_tidy %>% ggplot(aes(as.numeric(month), co2, color = year)) + 
  geom_line()

# Question 12
data(admissions)
dat <- admissions %>% select(-applicants)
dat %>% spread(gender,admitted)

# Question 13
tmp <- gather(admissions, key, value, admitted:applicants)
tmp2 <- unite(tmp, column_name, c(key, gender))

# Question 14
tmp2 %>% spread(column_name,-major)

#****************** 2.2: Combining Tables ******************
# Question 1
tab1 <- slice(murders, c(1:3,8:9)) %>% select(state, population)
tab2 <- arrange(results_us_election_2016,state) %>% 
  slice(c(1:3,5:7)) %>% select(state, electoral_votes)
dat <- left_join(tab1, tab2, by = "state")

# Question 2
dat <- semi_join(tab1, tab2, by = "state")

# Question 3

# Question 4
df1 <- bind_cols(x = c("a","b"), y = "a")
df2 <- bind_cols(x = "a", y = c("a","b"))
setdiff(df1,df2)
#****************#
library(Lahman)
top <- Batting %>% 
  filter(yearID == 2016) %>%
  arrange(desc(HR)) %>%    # arrange by descending HR count
  slice(1:10)    # take entries 1-10
top %>% as_tibble()
Master %>% as_tibble()
# Question 5
top_names <- top %>% left_join(Master) %>%
  select(playerID, nameFirst, nameLast, HR)

# Question 6
head(Salaries %>% filter(yearID == 2016))
top_salary <- Salaries %>% filter(yearID == 2016) %>%
  right_join(top_names) %>%
  select(playerID,nameFirst, nameLast, teamID, HR, salary)
Salaries %>% 
  filter(yearID == 2016 & playerID %in% c("trumbma01","cartech02","encared01"))

# Question 7
head(AwardsPlayers %>% filter(yearID == 2016))
won_award <- AwardsPlayers %>% filter(yearID == 2016) %>%
  inner_join(top_names)
awards2016 <- AwardsPlayers %>% filter(yearID == 2016) %>%
  group_by(playerID) %>% summarize()
won_nottop10 <- awards2016 %>% anti_join(top_names)

#*********************** Assessment: Web Scraping ***********************
library(rvest)
url <- "https://web.archive.org/web/20181024132313/http://www.stevetheump.com/Payrolls.htm"
h <- read_html(url)
nodes <- html_nodes(h, "table")
html_text(nodes[[8]])
html_table(nodes[[8]])
# Question 1
df1 <- html_table(nodes[[1]])
df2 <- html_table(nodes[[2]])
df3 <- html_table(nodes[[3]])
df4 <- html_table(nodes[[4]])

# Question 2

# Question 3
tab_1 <- html_table(nodes[[10]])
tab_2 <- html_table(nodes[[19]])
tab_1 <- setdiff(tab_1,tab_1[1,]) %>% select(-X1) %>%
  setNames(c("Team", "Payroll", "Average"))
tab_2 <- setdiff(tab_2,tab_2[1,]) %>%
  setNames(c("Team", "Payroll", "Average"))
tab12 <- full_join(tab_1,tab_2, by = "Team")

# Question 4 *************
url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
h <- read_html(url)
tab <- html_nodes(h, "table")
html_table(tab[[40]])

# Question 5
dftab <- html_table(tab[[2]],fill = TRUE)
dftab <- html_table(tab[[3]],fill = TRUE)
dftab <- html_table(tab[[4]],fill = TRUE)
dftab <- html_table(tab[[5]],fill = TRUE)
