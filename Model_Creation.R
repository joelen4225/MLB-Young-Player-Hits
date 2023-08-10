# This File is to Be Used After 'Data_Collect_Organize.R'
# We Will Be Creating Our Initial Linear Regression Model

# Initializing the Packages
install.packages("tidyverse")
install.packages("dplyr")

# Loading the Libraries
library(tidyverse)
library(dplyr)

# Replacing Certain NA Values with 0
battersDB <- battersDB %>%
  mutate_at(c(27:66), ~replace_na(., 0))
battersDB <- na.omit(battersDB)
trainDB <- battersDB[c(1:172), c(4:16, 18, 20:22, 26:66)]
trainDB$teams_away_league_record_pct <- as.numeric(trainDB$teams_away_league_record_pct)
trainDB$teams_home_league_record_pct <- as.numeric(trainDB$teams_home_league_record_pct)
testDB <- battersDB[c(173:343), c(4:16, 18, 20:22, 26:66)]
testDB$teams_away_league_record_pct <- as.numeric(testDB$teams_away_league_record_pct)
testDB$teams_home_league_record_pct <- as.numeric(testDB$teams_home_league_record_pct)

# Creating the Linear Regression Model
hitsLinReg <- lm(H ~ ., data = trainDB)

# Testing the Model 
testDB$H_predicted <- round(predict(hitsLinReg, newdata = testDB), 1)
testDB$H_diff <- abs(testDB$H - testDB$H_predicted)
testDB$abs_perc_err <- round(testDB$H_diff / (testDB$H + 1), 3)
MAPE <- round(mean(testDB$abs_perc_err) * 100, 2)
