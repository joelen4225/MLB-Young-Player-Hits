# This is Where I Input Data to Test the Day of the Game

# Initializing the Packages
install.packages("tidyverse")
install.packages("dplyr")
install.packages("baseballr")
install.packages("caret")
install.packages("randomForest")
install.packages("readxl")

# Loading the Libraries
library(tidyverse)
library(dplyr)
library(baseballr)
library(caret)
library(randomForest)
library(readxl)

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
BOP <- 9
RE24 <- -0.81
DFS.DK. <- 0.00
DFS.FD. <- 0.00

# Retrieving the Pitcher Statistics
trial_run_db <- bref_daily_pitcher("2023-03-31", "2023-08-09")
trial_run_db <- trial_run_db[trial_run_db$Name == "Hunter Brown", ]
trial_run_db <- trial_run_db[, c(7, 8, 29, 37)]

# Merging the Batter and Pitcher Statistics
left_join_db <- data.frame(OBP, BOP, RE24, DFS.DK., DFS.FD.)
trial_run_db <- cross_join(trial_run_db, left_join_db)

# Predicting the Number of Hits
predict(result_rfe1, trial_run_db)
