#************ Assessment 1 *****************
# setwd(/Volumes/Data2/Users/jaredcoffin/projects) this won't work
dir <- "/Volumes/Data2/Users/jaredcoffin/projects"
setwd(dir)
getwd()

# Question 5
getwd()
# ie "C:/Users/UNIVERSITY/Documents/Analyses/HarvardX-Wrangling"
setwd("wrangling/data")
filename <- "murders.csv"
path <- system.file("extdata", package = "dslabs")
fullpath <- file.path(path, filename)
file.copy(fullpath, getwd())

# Question 6
read_lines("murders.csv")

# Question 8
read_csv("times.txt")
read.csv("times.txt")
read_csv("times.txt", col_names = TRUE)
read_delim("times.txt", delim = ",")

# Question 9
times_2016 <- read_excel("times.xlsx", sheet = 2)
times_2016 <- read_xlsx("times.xlsx", sheet = "2016")

# Question 10
race_times <- read.csv("times.csv")
class(race_times$initials)

# Question 11
library(readr)

# Question 12
race_times <- read.csv("times.csv", stringsAsFactors = F)
class(race_times)

# Question 13
url <- "https://github.com/JaredRC/productivity-tools/tree/main/data/Jared.csv"
dat <- read_csv(url)
download.file(url, "JCoffin.csv")

# Question 14
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data"
dat <- read_csv(url)

# Question 15
dat <- read_csv(url, col_names=FALSE)

# Question 16
dat <- read_csv(url, col_names=FALSE)
