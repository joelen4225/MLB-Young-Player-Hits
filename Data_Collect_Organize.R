# Initializing The Packages
install.packages("tidyverse")
install.packages("dplyr")
install.packages("readxl")
install.packages("baseballr")
install.packages("data.table")

# Loading the Libraries
library(tidyverse)
library(dplyr)
library(readxl)
library(baseballr)
library(data.table)

# Creating a List for Column Types
colTypeList <- c("numeric", "numeric", "numeric", "date", "text",
                "text", "text", "text", "text", "numeric",
                "numeric", "numeric", "numeric", "numeric", "numeric",
                "numeric", "numeric", "numeric", "numeric", "numeric",
                "numeric", "numeric", "numeric", "numeric", "numeric",
                "numeric", "numeric", "numeric", "numeric", "numeric",
                "numeric", "numeric", "numeric", "numeric", "numeric",
                "numeric", "numeric", "numeric", "numeric", "text")

# Loading the XLSX Files
eDeLaCruzDB <- read_excel("EllyDeLaCruz_Stats.xlsx", col_types = colTypeList)
cCarrollDB <- read_excel("CorbinCarroll_Stats.xlsx", col_types = colTypeList)
hDavisDB <- read_excel("HenryDavis_Stats.xlsx", col_types = colTypeList)
gHendersonDB <- read_excel("GunnarHenderson_Stats.xlsx", col_types = colTypeList)
aRutschmanDB <- read_excel("AdleyRutschman_Stats.xlsx", col_types = colTypeList)
cCowserDB <- read_excel("ColtonCowser_Stats.xlsx", col_types = colTypeList)
fAlvarezDB <- read_excel("FranciscoAlvarez_Stats.xlsx", col_types = colTypeList)
jJungDB <- read_excel("JoshJung_Stats.xlsx", col_types = colTypeList)
aVolpeDB <- read_excel("AnthonyVolpe_Stats.xlsx", col_types = colTypeList)
mMcLainDB <- read_excel("MattMcLain_Stats.xlsx", col_types = colTypeList)

# Making a DB List for the Batters
battersList <- list(eDeLaCruzDB, cCarrollDB, hDavisDB, gHendersonDB, 
                 aRutschmanDB, cCowserDB, fAlvarezDB, jJungDB,
                 aVolpeDB, mMcLainDB)

# Loading in the MLB Schedule & Teams Table
scheduleDB <- mlb_schedule(2023, level_ids = 1)
scheduleDB <- scheduleDB[, c(1, 6, 13, 15, 16, 20, 29, 42, 43, 44, 52, 53, 54)]
teamDB <- mlb_teams(2023)
scheduleDB <- scheduleDB %>% left_join(teamDB[, c(2, 8)], by = 
                                         c("teams_away_team_id" = "team_id"))
names(scheduleDB)[names(scheduleDB) == "team_abbreviation"] <- "away_abbrev"
scheduleDB <- scheduleDB %>% left_join(teamDB[, c(2, 8)], by = 
                                         c("teams_home_team_id" = "team_id"))
names(scheduleDB)[names(scheduleDB) == "team_abbreviation"] <- "home_abbrev"

# Removing the Final Row From Each Batter Table
remove_final_row <- function(x)
  x <- x %>%
  filter(row_number() <= n() - 1)
res <- lapply(battersList, remove_final_row)
eDeLaCruzDB <- data.frame(res[1])
cCarrollDB <- data.frame(res[2])
hDavisDB <- data.frame(res[3])
gHendersonDB <- data.frame(res[4])
aRutschmanDB <- data.frame(res[5])
cCowserDB <- data.frame(res[6])
fAlvarezDB <- data.frame(res[7])
jJungDB <- data.frame(res[8])
aVolpeDB <- data.frame(res[9])
mMcLainDB <- data.frame(res[10])

# Shifting the Rows to Account for Stats From Previous Game
shift_rows <- function(x)
  x <- x %>%
  mutate(hits_before = lag(H),
         BA = lag(BA),
         OBP = lag(OBP),
         SLG = lag(SLG),
         OPS = lag(OPS),
         BOP = lag(BOP),
         aLI = lag(aLI),
         WPA = lag(WPA),
         acLI = lag(acLI),
         cWPA = lag(cWPA))
res <- lapply(res, shift_rows) 
eDeLaCruzDB <- data.frame(res[1])
cCarrollDB <- data.frame(res[2])
hDavisDB <- data.frame(res[3])
gHendersonDB <- data.frame(res[4])
aRutschmanDB <- data.frame(res[5])
cCowserDB <- data.frame(res[6])
fAlvarezDB <- data.frame(res[7])
jJungDB <- data.frame(res[8])
aVolpeDB <- data.frame(res[9])
mMcLainDB <- data.frame(res[10])

# Creating and Altering the Batter Table
battersDB <- rbind(aRutschmanDB, aVolpeDB, cCarrollDB, cCowserDB, eDeLaCruzDB,
                   fAlvarezDB, gHendersonDB, hDavisDB, jJungDB, mMcLainDB)
battersDB <- na.omit(battersDB[, c(4:5, 7, 13, 28:41)])

# Renaming Certain Team Abbreviations
battersDB$Opp[battersDB$Opp == "ARI"] <- "AZ"
battersDB$Opp[battersDB$Opp == "CHW"] <- "CWS"
battersDB$Opp[battersDB$Opp == "KCR"] <- "KC"
battersDB$Opp[battersDB$Opp == "SDP"] <- "SD"
battersDB$Opp[battersDB$Opp == "SFG"] <- "SF"
battersDB$Opp[battersDB$Opp == "TBR"] <- "TB"
battersDB$Opp[battersDB$Opp == "WSN"] <- "WSH"
battersDB$Opp[battersDB$Tm == "ARI"] <- "AZ"
battersDB$Opp[battersDB$Tm == "CHW"] <- "CWS"
battersDB$Opp[battersDB$Tm == "KCR"] <- "KC"
battersDB$Opp[battersDB$Tm == "SDP"] <- "SD"
battersDB$Opp[battersDB$Tm == "SFG"] <- "SF"
battersDB$Opp[battersDB$Tm == "TBR"] <- "TB"
battersDB$Opp[battersDB$Tm == "WSN"] <- "WSH"

# Merging the Schedule Table to Retrieve 'game_pk'
scheduleDB$date <- as.Date(scheduleDB$date)
tmAwayDB <- left_join(battersDB, scheduleDB[, c(1:2, 7:8, 11, 14:15)], 
                  by = c("Date" = "date",
                         "Tm" = "away_abbrev",
                         "Opp" = "home_abbrev"))
tmAwayDB <- na.omit(tmAwayDB)
tmHomeDB <- left_join(battersDB, scheduleDB[, c(1:2, 7:8, 11, 14:15)], 
                   by = c("Date" = "date",
                          "Opp" = "away_abbrev",
                          "Tm" = "home_abbrev"))
tmHomeDB <- na.omit(tmHomeDB)

# Merging the Opponent Team Full Names
battersDB <- rbind(tmAwayDB, tmHomeDB)
teamDB <- teamDB[teamDB$sport_name == "Major League Baseball", ]
battersDB <- left_join(battersDB, teamDB[, c(3, 8)],
                       by = c("Opp" = "team_abbreviation"))
pitchersDB <- data.frame()
for (i in 1:nrow(battersDB)){
  pitchersDB <- rbind(pitchersDB, mlb_probables(battersDB[i, 19]))
}
pitchersDB$game_date <- as.Date(pitchersDB$game_date)
battersDB <- left_join(battersDB, pitchersDB[, c(1:5)],
                  by = c("game_pk" = "game_pk",
                         "Date" = "game_date",
                         "team_full_name" = "team"))
battersDB <- unique(battersDB)

# Finding the Pitchers Stats From Up to the Game Date
dateList <- unique(battersDB$Date)
pitchersStatsDB <- data.frame()
for (i in 1:length(dateList)) {
  tempDB <- bref_daily_pitcher("2023-03-31", as.character(dateList[i] - 1))
  tempDB$Date <- dateList[i]
  pitchersStatsDB <- rbind(pitchersStatsDB, tempDB)
}

# Merging the Pitcher Stats to the Batter Table
battersDB <- left_join(battersDB, pitchersStatsDB[, c(3:4, 7:47)], 
                      by = c("fullName" = "Name",
                             "Date" = "Date"))
names(battersDB)[names(battersDB) == "H.x"] <- "H"
names(battersDB)[names(battersDB) == "H.y"] <- "H_pitcher"
