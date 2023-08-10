# This File is to Be Used After 'Model_Creation.R'
# We Will Be Improving and Optimizing our Base Regression Model

# Initializing the Packages
install.packages("tidyverse")
install.packages("dplyr")
install.packages("baseballr")
install.packages("readxl")

# Loading the Libraries
library(tidyverse)
library(dplyr)
library(baseballr)
library(readxl)

# Optimization 1
# We Will Reduce the Factors to Only Those 80% Significant
hitsLinReg <- lm(H ~ OBP + BOP + RE24 + DFS.DK. + DFS.FD. + teams_home_league_record_pct +
                   Age + G + GS + R + ER + SB + CS + Pit + GB.FB, data = trainDB)
# We Will Add in Data to the Testing Set
ellyJuly26DB <- read_excel("Elly_July_26.xlsx")
corbinJuly26DB <- read_excel("Corbin_July_26.xlsx")
davisJuly26DB <- read_excel("Davis_July_26.xlsx")
gunnarJuly26DB <- read_excel("Gunnar_July_26.xlsx")
adleyJuly26DB <- read_excel("Adley_July_26.xlsx")
cowserJuly26DB <- read_excel("Cowser_July_26.xlsx")
alvarezJuly26DB <- read_excel("Alvarez_July_26.xlsx")
jungJuly26DB <- read_excel("Jung_July_26.xlsx")
volpeJuly26DB <- read_excel("Volpe_July_26.xlsx")
mcLainJuly26DB <- read_excel("McLain_July_26.xlsx")
# Making a DB List for the Batters
july26List <- list(ellyJuly26DB, corbinJuly26DB, davisJuly26DB, gunnarJuly26DB, 
                   adleyJuly26DB, cowserJuly26DB, alvarezJuly26DB, jungJuly26DB,
                   volpeJuly26DB, mcLainJuly26DB)
# Shifting the Rows to Account for Stats From Previous Game
res_2 <- lapply(july26List, shift_rows)
ellyJuly26DB <- data.frame(res_2[1])
corbinJuly26DB <- data.frame(res_2[2])
davisJuly26DB <- data.frame(res_2[3])
gunnarJuly26DB <- data.frame(res_2[4])
adleyJuly26DB <- data.frame(res_2[5])
cowserJuly26DB <- data.frame(res_2[6])
alvarezJuly26DB <- data.frame(res_2[7])
jungJuly26DB <- data.frame(res_2[8])
volpeJuly26DB <- data.frame(res_2[9])
mcLainJuly26DB <- data.frame(res_2[10])
# Creating and Altering the Batter Table
secBattersDB <- rbind(ellyJuly26DB, corbinJuly26DB, davisJuly26DB, gunnarJuly26DB, 
                      adleyJuly26DB, cowserJuly26DB, alvarezJuly26DB, jungJuly26DB,
                      volpeJuly26DB, mcLainJuly26DB)
secBattersDB <- na.omit(secBattersDB[, c(4:5, 7, 13, 28:41)])
# Renaming Certain Team Abbreviations
secBattersDB$Opp[secBattersDB$Opp == "ARI"] <- "AZ"
secBattersDB$Opp[secBattersDB$Opp == "CHW"] <- "CWS"
secBattersDB$Opp[secBattersDB$Opp == "KCR"] <- "KC"
secBattersDB$Opp[secBattersDB$Opp == "SDP"] <- "SD"
secBattersDB$Opp[secBattersDB$Opp == "SFG"] <- "SF"
secBattersDB$Opp[secBattersDB$Opp == "TBR"] <- "TB"
secBattersDB$Opp[secBattersDB$Opp == "WSN"] <- "WSH"
secBattersDB$Opp[secBattersDB$Tm == "ARI"] <- "AZ"
secBattersDB$Opp[secBattersDB$Tm == "CHW"] <- "CWS"
secBattersDB$Opp[secBattersDB$Tm == "KCR"] <- "KC"
secBattersDB$Opp[secBattersDB$Tm == "SDP"] <- "SD"
secBattersDB$Opp[secBattersDB$Tm == "SFG"] <- "SF"
secBattersDB$Opp[secBattersDB$Tm == "TBR"] <- "TB"
secBattersDB$Opp[secBattersDB$Tm == "WSN"] <- "WSH"
# Merging the Schedule Table to Retrieve 'game_pk'
scheduleDB$date <- as.Date(scheduleDB$date)
secTmAwayDB <- left_join(secBattersDB, scheduleDB[, c(1:2, 7:8, 11, 14:15)], 
                      by = c("Date" = "date",
                             "Tm" = "away_abbrev",
                             "Opp" = "home_abbrev"))
secTmAwayDB <- na.omit(secTmAwayDB)
secTmHomeDB <- left_join(secBattersDB, scheduleDB[, c(1:2, 7:8, 11, 14:15)], 
                      by = c("Date" = "date",
                             "Opp" = "away_abbrev",
                             "Tm" = "home_abbrev"))
secTmHomeDB <- na.omit(secTmHomeDB)
# Merging the Opponent Team Full Names
secBattersDB <- rbind(secTmAwayDB, secTmHomeDB)
teamDB <- teamDB[teamDB$sport_name == "Major League Baseball", ]
secBattersDB <- left_join(secBattersDB, teamDB[, c(3, 8)],
                       by = c("Opp" = "team_abbreviation"))
secPitchersDB <- data.frame()
for (i in 1:nrow(secBattersDB)){
  secPitchersDB <- rbind(secPitchersDB, mlb_probables(secBattersDB[i, 19]))
}
secPitchersDB$game_date <- as.Date(secPitchersDB$game_date)
secBattersDB <- left_join(secBattersDB, secPitchersDB[, c(1:5)],
                       by = c("game_pk" = "game_pk",
                              "Date" = "game_date",
                              "team_full_name" = "team"))
secBattersDB <- unique(secBattersDB)
# Finding the Pitchers Stats From Up to the Game Date
secDateList <- unique(secBattersDB$Date)
secPitchersStatsDB <- data.frame()
for (i in 1:length(secDateList)) {
  tempDB <- bref_daily_pitcher("2023-07-07", as.character(secDateList[i] - 1))
  tempDB$Date <- secDateList[i]
  secPitchersStatsDB <- rbind(secPitchersStatsDB, tempDB)
}
# Merging the Pitcher Stats to the Batter Table
secBattersDB <- na.omit(secBattersDB)
secBattersDB <- left_join(secBattersDB, secPitchersStatsDB[, c(3:4, 7:47)], 
                       by = c("fullName" = "Name",
                              "Date" = "Date"))
names(secBattersDB)[names(secBattersDB) == "H.x"] <- "H"
names(secBattersDB)[names(secBattersDB) == "H.y"] <- "H_pitcher"
# Replacing Certain NA Values with 0
secBattersDB <- secBattersDB %>%
  mutate_at(c(27:66), ~replace_na(., 0))
secBattersDB <- na.omit(secBattersDB)
# Merging the New Data with the Test Data
testDB <- battersDB[c(173:343), c(4:16, 18, 20:22, 26:66)]
tempDB <- secBattersDB[, c(4:16, 18, 20:22, 26:66)]
test2DB <- rbind(testDB, tempDB)
test2DB$teams_away_league_record_pct <- as.numeric(test2DB$teams_away_league_record_pct)
test2DB$teams_home_league_record_pct <- as.numeric(test2DB$teams_home_league_record_pct)
testDB$teams_away_league_record_pct <- as.numeric(testDB$teams_away_league_record_pct)
testDB$teams_home_league_record_pct <- as.numeric(testDB$teams_home_league_record_pct)
# Testing the Model 
testDB$H_predicted <- round(predict(hitsLinReg, newdata = testDB), 1)
testDB$H_diff <- abs(testDB$H - testDB$H_predicted)
testDB$abs_perc_err <- round(testDB$H_diff / (testDB$H + 1), 3)
MAPE <- round(mean(testDB$abs_perc_err) * 100, 2)

test2DB$H_predicted <- round(predict(hitsLinReg, newdata = test2DB), 1)
test2DB$H_diff <- abs(test2DB$H - test2DB$H_predicted)
test2DB$abs_perc_err <- round(test2DB$H_diff / (test2DB$H + 1), 3)
MAPE <- round(mean(test2DB$abs_perc_err) * 100, 2)

ggplot(testDB, aes(x = CS, y = H)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Original Data")
# Optimization 2
# Decreasing the Factors to 95% Significance Post Removal of All Factors <80% Significant 
hitsLinReg <- lm(H ~ BOP + RE24 + DFS.DK. + DFS.FD. + G + GS + SB + GB.FB, data = trainDB)
# Testing the Model with Data Base 1 and 2
testDB$H_predicted <- round(predict(hitsLinReg, newdata = testDB), 1)
testDB$H_diff <- abs(testDB$H - testDB$H_predicted)
testDB$abs_perc_err <- round(testDB$H_diff / (testDB$H + 1), 3)
MAPE <- round(mean(testDB$abs_perc_err) * 100, 2)
test2DB$H_predicted <- round(predict(hitsLinReg, newdata = test2DB), 1)
test2DB$H_diff <- abs(test2DB$H - test2DB$H_predicted)
test2DB$abs_perc_err <- round(test2DB$H_diff / (test2DB$H + 1), 3)
MAPE <- round(mean(test2DB$abs_perc_err) * 100, 2)
# Creating a RFE Model using Random Forest to Create a More Accurate Model
control <- rfeControl(functions = rfFuncs,
                      method = "repeatedcv",
                      repeats = 5,
                      number = 10)
# Splitting the Data into Training and Testing Sub-Datasets
x_train <- trainDB[, -c(1, 59:64)]
x_test_1 <- testDB[, -c(1, 59:61)]
x_test_2 <- test2DB[, -c(1, 59:61)]
y_train <- trainDB[, c("H")]
y_test_1 <- testDB[, c("H")]
y_test_2 <- test2DB[, c("H")]
# Inputting the Training Data to the Model
result_rfe1 <- rfe(x = x_train,
                   y = y_train,
                   sizes = c(1:13),
                   rfeControl = control)
# Predicting the Number of Hits and Calculating MAPE
y_test_1 <- data.frame(y_test_1)
y_test_1$H_predicted <- predict(result_rfe1, x_test_1)
y_test_1$H_diff <- abs(y_test_1[, 1] - y_test_1[, 2])
y_test_1$abs_perc_err <- round(y_test_1[, 3] / (y_test_1[, 1] + 1), 3)
MAPE <- round(mean(y_test_1[, 4]) * 100, 2)
y_test_2 <- data.frame(y_test_2)
y_test_2$H_predicted <- predict(result_rfe1, x_test_2)
y_test_2$H_diff <- abs(y_test_2[, 1] - y_test_2[, 2])
y_test_2$abs_perc_err <- round(y_test_2[, 3] / (y_test_2[, 1] + 1), 3)
MAPE <- round(mean(y_test_2[, 4]) * 100, 2)

