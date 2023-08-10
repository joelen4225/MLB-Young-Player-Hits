# This is Where I Input Data to Test the Day of the Game

# Initializing the Packages
install.packages("tidyverse")
install.packages("dplyr")
install.packages("baseballr")

# Loading the Libraries
library(tidyverse)
library(dplyr)
library(baseballr)

# Writing Down the Batter Statistics
OBP <- .360
BOP <- 3
RE24 <- 0.43
DFS.DK. <- 12.00
DFS.FD. <- 15.70
teams_home_league_record_pct <- .542

# Retrieving the Pitcher Statistics
trial_run_db <- bref_daily_pitcher("2023-03-31", "2023-07-31")
trial_run_db <- trial_run_db[trial_run_db$Name == "Alex Cobb", ]
trial_run_db <- trial_run_db[, c(4, 7, 8, 14, 15, 29, 30, 33, 37)]

# Merging the Batter and Pitcher Statistics
left_join_db <- data.frame(OBP, BOP, RE24, DFS.DK., DFS.FD., teams_home_league_record_pct)
trial_run_db <- cross_join(trial_run_db, left_join_db)

# Predicting the Number of Hits
predict(hitsLinReg, newdata = trial_run_db)
