################################################################################
# Case Study RB - Relationship between transfer spending and league performance
# in German Bundesliga 19/20 - 23/24
#
# 1. setwd() 
# 2. In working directory should be 3 folders (data, logos, plots)
# 3. Run Code 
#
# Tobias Großauer - 10/24
################################################################################



# Bibliotheken -----------------------------------------------------------------
rm(list=ls())
library(dplyr)
library(data.table)
library(openxlsx)
library(ggplot2)
library(ggimage)



# Pfade ------------------------------------------------------------------------
setwd("C:/Case Study RB 2024")
pathData <- file.path(getwd(), "data")
pathLogos <- file.path(getwd(), "logos")
pathPlots <- file.path(getwd(), "plots")



# Hilfsfunktion ----------------------------------------------------------------
cleanCheckStBL <- function(data, cols_numeric) {
  
  # Clean Data
  setnames(data, new = c("RANK_CLASS", "TEAM", "GAMES_HOME", "WINS_HOME", "DRAWS_HOME", "LOSSES_HOME", "GAMES_AWAY", "WINS_AWAY", "DRAWS_AWAY", "LOSSES_AWAY", "GAMES_TOTAL", "WINS_TOTAL", "DRAWS_TOTAL", "LOSSES_TOTAL", "GOALS_SCORED", "GOALS_AGAINST", "GOALS_DIFF", "POINTS"))
  
  data <- data[TEAM != ""] %>% copy()
  data[, (cols_numeric) := lapply(.SD, as.numeric), .SDcols = cols_numeric]
  data[, RANK := 1:18]
  
  setorderv(data, cols = c("POINTS", "GOALS_DIFF", "GOALS_SCORED"), order = c(-1, -1, -1))
  
  # Check Data on consistency
  checks <- c(
    all(data$RANK == 1:18),
    all(data$GAMES_HOME == data$WINS_HOME + data$DRAWS_HOME + data$LOSSES_HOME),
    all(data$GAMES_AWAY == data$WINS_AWAY + data$DRAWS_AWAY + data$LOSSES_AWAY),
    all(data$GAMES_TOTAL == data$WINS_TOTAL + data$DRAWS_TOTAL + data$LOSSES_TOTAL),
    all(data$GAMES_TOTAL == data$GAMES_HOME + data$GAMES_AWAY),
    all(data$WINS_TOTAL == data$WINS_HOME + data$WINS_AWAY),
    all(data$DRAWS_TOTAL == data$DRAWS_HOME + data$DRAWS_AWAY),
    all(data$LOSSES_TOTAL == data$LOSSES_HOME + data$LOSSES_AWAY),
    all(data$GOALS_DIFF == data$GOALS_SCORED - data$GOALS_AGAINST),
    all(data$POINTS == data$WINS_TOTAL * 3 + data$DRAWS_TOTAL)
  )
  
  if (!all(checks)) {
    stop("Inconsistent data table!")
  }
  else {
    return(data)
    checks <- NULL
  }
}



# Daten und Logos einlesen -----------------------------------------------------
data_stBL_19_20 <- fread(file = file.path(pathData,"standings-bundesliga-19-20.csv"), sep = ",", dec = ".", encoding = "Latin-1")
data_stBL_20_21 <- fread(file = file.path(pathData,"standings-bundesliga-20-21.csv"), sep = ",", dec = ".", encoding = "Latin-1")
data_stBL_21_22 <- fread(file = file.path(pathData,"standings-bundesliga-21-22.csv"), sep = ",", dec = ".", encoding = "Latin-1")
data_stBL_22_23 <- fread(file = file.path(pathData,"standings-bundesliga-22-23.csv"), sep = ",", dec = ".", encoding = "Latin-1")
data_stBL_23_24 <- fread(file = file.path(pathData,"standings-bundesliga-23-24.csv"), sep = ",", dec = ".", encoding = "Latin-1")

data_trans_19_24 <- fread(file = file.path(pathData,"transfers-bundesliga.csv"), sep = ",", dec = ".", encoding = "UTF-8")

logosBL <- data.table("TEAM" = c("Bayern Munich", "Borussia Dortmund", "RB Leipzig", "Borussia Mönchengladbach", "Bayer 04 Leverkusen", "1899 Hoffenheim", "VfL Wolfsburg", "SC Freiburg",
                                 "Eintracht Frankfurt", "Hertha Berlin", "1. FC Union Berlin", "FC Schalke 04", "Mainz 05", "1. FC Köln", "FC Augsburg", "Werder Bremen",
                                 "Fortuna Düsseldorf", "SC Paderborn 07", "VfB Stuttgart", "Arminia Bielefeld", "VfL Bochum", "Greuther Fürth", "1. FC Heidenheim", "Darmstadt 98"),
                      "LOGO" = c(file.path(pathLogos,"BAY.png"), file.path(pathLogos,"BVB.png"), file.path(pathLogos,"RBL.png"), file.path(pathLogos,"BMG.png"), file.path(pathLogos,"B04.png"), file.path(pathLogos,"HOF.png"), file.path(pathLogos,"WOL.png"), file.path(pathLogos,"SCF.png"),
                                 file.path(pathLogos,"FRA.png"), file.path(pathLogos,"BSC.png"), file.path(pathLogos,"FCU.png"), file.path(pathLogos,"S04.png"), file.path(pathLogos,"M05.png"), file.path(pathLogos,"FCK.png"), file.path(pathLogos,"AUG.png"), file.path(pathLogos,"BRE.png"),
                                 file.path(pathLogos,"FDU.png"), file.path(pathLogos,"SCP.png"), file.path(pathLogos,"VFB.png"), file.path(pathLogos,"BIE.png"), file.path(pathLogos,"BOC.png"), file.path(pathLogos,"GFU.png"), file.path(pathLogos,"HEI.png"), file.path(pathLogos,"DAR.png")))
logosBL[, SIZE := case_when(TEAM %in% c("RB Leipzig") ~ 0.1,
                            TEAM %in% c("Hertha Berlin", "Bayer 04 Leverkusen", "1. FC Union Berlin", "SC Paderborn 07", "Greuther Fürth", "Darmstadt 98") ~ 0.075,
                            TEAM  %in% c("SC Freiburg") ~ 0.035,
                            .default = 0.05)]



# Tabellen aufbereiten ---------------------------------------------------------
cols_numeric <- c("GAMES_HOME", "WINS_HOME", "DRAWS_HOME", "LOSSES_HOME", "GAMES_AWAY", "WINS_AWAY", "DRAWS_AWAY", "LOSSES_AWAY", "GAMES_TOTAL", "WINS_TOTAL", "DRAWS_TOTAL", "LOSSES_TOTAL", "GOALS_SCORED", "GOALS_AGAINST", "GOALS_DIFF", "POINTS")

data_stBL_19_20 <- cleanCheckStBL(data_stBL_19_20 %>% copy(), cols_numeric)
data_stBL_20_21 <- cleanCheckStBL(data_stBL_20_21 %>% copy(), cols_numeric)
data_stBL_21_22 <- cleanCheckStBL(data_stBL_21_22 %>% copy(), cols_numeric)
data_stBL_22_23 <- cleanCheckStBL(data_stBL_22_23 %>% copy(), cols_numeric)
data_stBL_23_24 <- cleanCheckStBL(data_stBL_23_24 %>% copy(), cols_numeric)

# Internationale Plätze und Absteiger ergänzen
data_stBL_19_20[, SEASON := "19/20"]
data_stBL_19_20[, RANK_CLASS := case_when(RANK %in% 1:4 ~ "Champions League",
                                          RANK %in% 5:7 ~ "Europa League",
                                          RANK %in% 8:16 ~ "National",
                                          RANK %in% 17:18 ~ "Relegated")]

data_stBL_20_21[, SEASON := "20/21"]
data_stBL_20_21[, RANK_CLASS := case_when(RANK %in% 1:4 ~ "Champions League",
                                          RANK %in% 5:6 ~ "Europa League",
                                          RANK == 7 ~ "Europa Conference League",
                                          RANK %in% 8:16 ~ "National",
                                          RANK %in% 17:18 ~ "Relegated")]

data_stBL_21_22[, SEASON := "21/22"]
data_stBL_21_22[, RANK_CLASS := case_when(RANK %in% 1:4 ~ "Champions League",
                                          RANK %in% 5:6 ~ "Europa League",
                                          RANK == 7 ~ "Europa Conference League",
                                          RANK %in% 8:16 ~ "National",
                                          RANK %in% 17:18 ~ "Relegated")]

data_stBL_22_23[, SEASON := "22/23"]
data_stBL_22_23[, RANK_CLASS := case_when(RANK %in% 1:4 ~ "Champions League",
                                          RANK %in% 5:6 ~ "Europa League",
                                          RANK == 7 ~ "Europa Conference League",
                                          RANK %in% 8:16 ~ "National",
                                          RANK %in% 17:18 ~ "Relegated")]

data_stBL_23_24[, SEASON := "23/24"]
data_stBL_23_24[, RANK_CLASS := case_when(RANK %in% 1:5 ~ "Champions League",
                                          RANK %in% 6:7 ~ "Europa League",
                                          RANK == 8 ~ "Europa Conference League",
                                          RANK %in% 9:16 ~ "National",
                                          RANK %in% 17:18 ~ "Relegated")]

stBL_19_24 <- rbindlist(list(data_stBL_19_20,
                             data_stBL_20_21,
                             data_stBL_21_22,
                             data_stBL_22_23,
                             data_stBL_23_24), use.names = TRUE)

setcolorder(stBL_19_24, neworder = c("SEASON", "RANK"))

# Teamnamen anpassen
stBL_19_24[, TEAM := case_when(TEAM == "1 FC Heidenheim" ~ "1. FC Heidenheim",
                               TEAM == "1 FC Köln" ~ "1. FC Köln",
                               TEAM == "1 FC Union Berlin" ~ "1. FC Union Berlin",
                               .default = TEAM)]

# Season Previous/Next, Points Away/Home hinzufügen
stBL_19_24[, SEASON_PREVIOUS := case_when(SEASON == "19/20" ~ NA,
                                          SEASON == "20/21" ~ "19/20",
                                          SEASON == "21/22" ~ "20/21",
                                          SEASON == "22/23" ~ "21/22",
                                          SEASON == "23/24" ~ "22/23")]

stBL_19_24[, SEASON_NEXT := case_when(SEASON == "19/20" ~ "20/21",
                                      SEASON == "20/21" ~ "21/22",
                                      SEASON == "21/22" ~ "22/23",
                                      SEASON == "22/23" ~ "23/24",
                                      SEASON == "23/24" ~ NA)]

stBL_19_24[, POINTS_HOME := WINS_HOME * 3 + DRAWS_HOME]
stBL_19_24[, POINTS_AWAY := WINS_AWAY * 3 + DRAWS_AWAY]

# Allgemeine Teamdaten
teamsBL <- stBL_19_24[, .(SEASON, TEAM)]
teamsBL[, YEARS_IN_BL := .N, by = c("TEAM")]



# Transfers aufbereiten --------------------------------------------------------
setnames(data_trans_19_24, new = toupper(names(data_trans_19_24)))
data_trans_19_24[, ':='(V1 = NULL, SEASON_ID = NULL, FROM.CLUB_ID = NULL, TO.CLUB_ID = NULL, UPDATED_AT = NULL)]
setorderv(data_trans_19_24, cols = c("TRANSFER_DATETIME", "TRANSFER_ID"))

# Check Data on consistency
if (all(data_trans_19_24$TRANSFER_FEE_EURO/1e6 == data_trans_19_24$TRANSFER_FEE_MILLION, na.rm = TRUE)) { data_trans_19_24[, TRANSFER_FEE_EURO := NULL] }
if (all(data_trans_19_24$MARKET_VALUE_EURO/1e6 == data_trans_19_24$MARKET_VALUE_MILLION, na.rm = TRUE)) { data_trans_19_24[, MARKET_VALUE_EURO := NULL] }
data_trans_19_24 <- data_trans_19_24[!(TRANSFER_ID == 2609494 & TO.COMPETITION_ID == "E3G2")] %>% copy()                                                            # doppelten Transfer entfernen

# Teamnamen vereinheitlichen
data_trans_19_24[, FROM.CLUB_NAME := case_when(FROM.CLUB_NAME == "1.FC Köln" ~ "1. FC Köln",
                                               FROM.CLUB_NAME == "1.FSV Mainz 05" ~ "Mainz 05",
                                               FROM.CLUB_NAME == "Arm. Bielefeld" ~ "Arminia Bielefeld",
                                               FROM.CLUB_NAME == "B. Leverkusen" ~ "Bayer 04 Leverkusen",
                                               FROM.CLUB_NAME == "Bor. Dortmund" ~ "Borussia Dortmund",
                                               FROM.CLUB_NAME == "Bor. M'gladbach" ~ "Borussia Mönchengladbach",
                                               FROM.CLUB_NAME == "E. Frankfurt" ~ "Eintracht Frankfurt",
                                               FROM.CLUB_NAME == "F. Düsseldorf" ~ "Fortuna Düsseldorf",
                                               FROM.CLUB_NAME == "Heidenheim" ~ "1. FC Heidenheim",
                                               FROM.CLUB_NAME == "Hertha BSC" ~ "Hertha Berlin",
                                               FROM.CLUB_NAME == "SC Paderborn" ~ "SC Paderborn 07",
                                               FROM.CLUB_NAME == "TSG Hoffenheim" ~ "1899 Hoffenheim",
                                               FROM.CLUB_NAME == "Union Berlin" ~ "1. FC Union Berlin",
                                               .default = FROM.CLUB_NAME)]

data_trans_19_24[, TO.CLUB_NAME := case_when(TO.CLUB_NAME == "1.FC Köln" ~ "1. FC Köln",
                                             TO.CLUB_NAME == "1.FSV Mainz 05" ~ "Mainz 05",
                                             TO.CLUB_NAME == "Arm. Bielefeld" ~ "Arminia Bielefeld",
                                             TO.CLUB_NAME == "B. Leverkusen" ~ "Bayer 04 Leverkusen",
                                             TO.CLUB_NAME == "Bor. Dortmund" ~ "Borussia Dortmund",
                                             TO.CLUB_NAME == "Bor. M'gladbach" ~ "Borussia Mönchengladbach",
                                             TO.CLUB_NAME == "E. Frankfurt" ~ "Eintracht Frankfurt",
                                             TO.CLUB_NAME == "F. Düsseldorf" ~ "Fortuna Düsseldorf",
                                             TO.CLUB_NAME == "Heidenheim" ~ "1. FC Heidenheim",
                                             TO.CLUB_NAME == "Hertha BSC" ~ "Hertha Berlin",
                                             TO.CLUB_NAME == "SC Paderborn" ~ "SC Paderborn 07",
                                             TO.CLUB_NAME == "TSG Hoffenheim" ~ "1899 Hoffenheim",
                                             TO.CLUB_NAME == "Union Berlin" ~ "1. FC Union Berlin",
                                             .default = TO.CLUB_NAME)]

# Leihen aufarbeiten (minimale Leihdauer > 0)
trans_19_24_loans <- data_trans_19_24[TRANSFER_TYPE %in% c("free loan transfer", "paid loan transfer")] %>%
  .[, .(SEASON, TRANSFER_DATE_START = TRANSFER_DATETIME, TRANSFER_ID_START = TRANSFER_ID, TRANSFER_TYPE_START = TRANSFER_TYPE, PLAYER_ID, PLAYER_NAME, PLAYER_AGE_AT_TRANSFER, TRANSFER_FEE_MILLION, MARKET_VALUE_START = MARKET_VALUE_MILLION,
        FROM.CLUB_NAME, FROM.COMPETITION_ID, TO.CLUB_NAME, TO.COMPETITION_ID, DATE_OF_BIRTH = DATE_OF_BIRTH_DATETIME, CITIZENSHIP, POSITION.MAIN, POSITION.OTHER, HEIGHT_CM, FOOT)] %>%
  left_join(data_trans_19_24[TRANSFER_TYPE %in% c("end of loan"), .(PLAYER_ID, TO.CLUB_NAME, FROM.CLUB_NAME, TRANSFER_ID_END = TRANSFER_ID, TRANSFER_TYPE_END = TRANSFER_TYPE, TRANSFER_DATE_END = TRANSFER_DATETIME, MARKET_VALUE_END = MARKET_VALUE_MILLION)],
            by = c("PLAYER_ID", "FROM.CLUB_NAME" = "TO.CLUB_NAME", "TO.CLUB_NAME" = "FROM.CLUB_NAME"), relationship = "many-to-many") %>%
  copy()

trans_19_24_loans[TRANSFER_DATE_START < TRANSFER_DATE_END, STAY_DURATION := difftime(TRANSFER_DATE_END, TRANSFER_DATE_START, units = "days")]
trans_19_24_loans[, STAY_DURATION_MIN := min(STAY_DURATION, na.rm = TRUE), by = c("TRANSFER_ID_START")]
trans_19_24_loans <- trans_19_24_loans[STAY_DURATION == STAY_DURATION_MIN | is.na(TRANSFER_DATE_END)] %>% copy()
trans_19_24_loans[, STAY_DURATION_MIN := NULL]

# Transfers aufarbeiten
trans_19_24_other <- data_trans_19_24[TRANSFER_TYPE %in% c("", "free transfer", "paid transfer", "unknown")] %>%
  .[, .(SEASON, TRANSFER_DATE_START = TRANSFER_DATETIME, TRANSFER_ID_START = TRANSFER_ID, TRANSFER_TYPE_START = TRANSFER_TYPE, PLAYER_ID, PLAYER_NAME, PLAYER_AGE_AT_TRANSFER, TRANSFER_FEE_MILLION, MARKET_VALUE_START = MARKET_VALUE_MILLION,
        FROM.CLUB_NAME, FROM.COMPETITION_ID, TO.CLUB_NAME, TO.COMPETITION_ID, DATE_OF_BIRTH = DATE_OF_BIRTH_DATETIME, CITIZENSHIP, POSITION.MAIN, POSITION.OTHER, HEIGHT_CM, FOOT)] %>%
  left_join(data_trans_19_24[, .(PLAYER_ID, TRANSFER_ID_END = TRANSFER_ID, TRANSFER_TYPE_END = TRANSFER_TYPE, TRANSFER_DATE_END = TRANSFER_DATETIME, MARKET_VALUE_END = MARKET_VALUE_MILLION)],
            by = c("PLAYER_ID"), relationship = "many-to-many") %>%
  copy()

trans_19_24_other[TRANSFER_ID_START == TRANSFER_ID_END, ':='(TRANSFER_ID_END = NA, TRANSFER_TYPE_END = NA, TRANSFER_DATE_END = NA, MARKET_VALUE_END = NA)]

trans_19_24_other[TRANSFER_DATE_START < TRANSFER_DATE_END, STAY_DURATION := difftime(TRANSFER_DATE_END, TRANSFER_DATE_START, units = "days")]
trans_19_24_other[, STAY_DURATION_MIN := min(STAY_DURATION, na.rm = TRUE), by = c("TRANSFER_ID_START")]
trans_19_24_other <- trans_19_24_other[STAY_DURATION == STAY_DURATION_MIN | (is.na(STAY_DURATION_MIN) & is.na(STAY_DURATION) & is.na(TRANSFER_DATE_END))] %>% unique() %>% copy()
trans_19_24_other[, STAY_DURATION_MIN := NULL]

# Transfers zusammensetzen
trans_19_24 <- rbindlist(list(trans_19_24_loans, trans_19_24_other), use.names = TRUE)
trans_19_24[, MARKET_VALUE_CHANGE := MARKET_VALUE_END - MARKET_VALUE_START]
setcolorder(trans_19_24, neworder = c("SEASON", "TRANSFER_DATE_START", "TRANSFER_ID_START", "TRANSFER_TYPE_START", "TRANSFER_DATE_END", "TRANSFER_ID_END", "TRANSFER_TYPE_END", "PLAYER_ID", "PLAYER_NAME", "FROM.COMPETITION_ID", "FROM.CLUB_NAME", "TO.COMPETITION_ID", 
                                      "TO.CLUB_NAME", "TRANSFER_FEE_MILLION", "MARKET_VALUE_START", "MARKET_VALUE_END", "MARKET_VALUE_CHANGE", "STAY_DURATION", "DATE_OF_BIRTH", "PLAYER_AGE_AT_TRANSFER", "HEIGHT_CM", "FOOT", "CITIZENSHIP", "POSITION.MAIN", "POSITION.OTHER"))

setorderv(trans_19_24, cols = c("TRANSFER_DATE_START", "FROM.COMPETITION_ID", "FROM.CLUB_NAME", "PLAYER_NAME"))

# Transfers checken
trans_19_24[TRANSFER_ID_START == 3088455, SEASON := "20/21"]
# trans_19_24[TRANSFER_ID_START == 3398147, SEASON := "20/21"]                  

all(
  all(trans_19_24$SEASON == case_when(as.Date("2019-06-15") <= trans_19_24$TRANSFER_DATE_START & trans_19_24$TRANSFER_DATE_START <= as.Date("2020-06-14") ~ "19/20",
                                      as.Date("2020-06-15") <= trans_19_24$TRANSFER_DATE_START & trans_19_24$TRANSFER_DATE_START <= as.Date("2021-06-14") ~ "20/21",
                                      as.Date("2021-06-15") <= trans_19_24$TRANSFER_DATE_START & trans_19_24$TRANSFER_DATE_START <= as.Date("2022-06-14") ~ "21/22",
                                      as.Date("2022-06-15") <= trans_19_24$TRANSFER_DATE_START & trans_19_24$TRANSFER_DATE_START <= as.Date("2023-06-14") ~ "22/23",
                                      as.Date("2023-06-15") <= trans_19_24$TRANSFER_DATE_START & trans_19_24$TRANSFER_DATE_START <= as.Date("2024-06-14") ~ "23/24")),
  trans_19_24$TRANSFER_ID_START %>% unique() %>% length() == trans_19_24 %>% nrow(),
  trans_19_24$TRANSFER_ID_START %>% unique() %>% length() == data_trans_19_24[TRANSFER_TYPE != "end of loan"] %>% .[, .(TRANSFER_ID)] %>% unique() %>% nrow(),
  trans_19_24$TRANSFER_TYPE_START %>% table() == data_trans_19_24[TRANSFER_TYPE != "end of loan"] %>% .[, .(TRANSFER_TYPE)] %>% table(),
  sum(trans_19_24$TRANSFER_FEE_MILLION, na.rm = TRUE) == sum(data_trans_19_24$TRANSFER_FEE_MILLION, na.rm = TRUE)
)

# Marktwert klassifizieren (Top 10%, Unteren 50%)
trans_19_24[, MW_CLASS := case_when(MARKET_VALUE_START > quantile(MARKET_VALUE_START, na.rm = TRUE, probs = 0.90) ~ "Top Player",
                                    MARKET_VALUE_START < quantile(MARKET_VALUE_START, na.rm = TRUE, probs = 0.50) ~ "Low Player",
                                    .default = "Average Player")]

trans_19_24$MW_CLASS <- factor(trans_19_24$MW_CLASS, levels = c("Low Player", "Average Player", "Top Player"))

# Erfahrung klassifizieren (27 oder älter, 22 oder jünger)
trans_19_24[, EXP_CLASS := case_when(PLAYER_AGE_AT_TRANSFER >= 27 ~ "Experienced Player",
                                     PLAYER_AGE_AT_TRANSFER <= 22 ~ "Young Player",
                                     .default = "Average Player")]

trans_19_24$EXP_CLASS <- factor(trans_19_24$EXP_CLASS, levels = c("Young Player", "Average Player", "Experienced Player"))

# Position klassifizieren (GK, DEF, MID, FOR)
trans_19_24[, POS_CLASS := case_when(POSITION.MAIN == "Goalkeeper" ~ "GK",
                                     POSITION.MAIN %in% c("Left-Back", "Centre-Back", "Right-Back") ~ "DEF",
                                     POSITION.MAIN %in% c("Left Midfield", "Defensive Midfield", "Central Midfield", "Right Midfield", "Attacking Midfield") ~ "MID",
                                     POSITION.MAIN %in% c("Left Winger", "Right Winger", "Centre-Forward", "Second Striker") ~ "FOR",
                                     .default = NA)]

trans_19_24$POS_CLASS <- factor(trans_19_24$POS_CLASS, levels = c("GK", "DEF", "MID", "FOR"))



# Bundesliga analysieren -------------------------------------------------------

# Anzahl
transBL_19_24_N <- trans_19_24[TO.COMPETITION_ID == "L1" & FROM.COMPETITION_ID != "L1", .(N_IN = .N), by = c("SEASON")] %>% 
  left_join(trans_19_24[FROM.COMPETITION_ID == "L1" & TO.COMPETITION_ID != "L1", .(N_OUT = .N), by = c("SEASON")], by = c("SEASON")) %>%
  left_join(trans_19_24[FROM.COMPETITION_ID == "L1" & TO.COMPETITION_ID == "L1", .(N_BETWEEN = .N), by = c("SEASON")], by = c("SEASON")) %>%
  left_join(trans_19_24[, .(N_TOTAL = .N), by = c("SEASON")], by = c("SEASON")) %>%
  copy()

transBL_19_24_N[, N_DIFF := N_IN - N_OUT]
transBL_SUM_N <- transBL_19_24_N[, lapply(.SD, sum, na.rm=TRUE), .SDcols = is.numeric] %>% cbind("SEASON" = "SUM")
transBL_MEAN_N <- transBL_19_24_N[, lapply(.SD, mean, na.rm=TRUE), .SDcols = is.numeric] %>% cbind("SEASON" = "MEAN")

transBL_N <- rbindlist(list(transBL_19_24_N, transBL_SUM_N, transBL_MEAN_N), use.names = TRUE)

# Beträge
transBL_19_24_EURO <- trans_19_24[TO.COMPETITION_ID == "L1" & FROM.COMPETITION_ID != "L1", .(EURO_OUT = sum(TRANSFER_FEE_MILLION, na.rm = TRUE)), by = c("SEASON")] %>% 
  left_join(trans_19_24[FROM.COMPETITION_ID == "L1" & TO.COMPETITION_ID != "L1", .(EURO_IN = sum(TRANSFER_FEE_MILLION, na.rm = TRUE)), by = c("SEASON")], by = c("SEASON")) %>%
  left_join(trans_19_24[FROM.COMPETITION_ID == "L1" & TO.COMPETITION_ID == "L1", .(EURO_BETWEEN = sum(TRANSFER_FEE_MILLION, na.rm = TRUE)), by = c("SEASON")], by = c("SEASON")) %>%
  copy()

transBL_19_24_EURO[, UMSATZ := EURO_OUT + EURO_IN + 2*EURO_BETWEEN]
transBL_19_24_EURO[, EURO_DIFF := - EURO_OUT + EURO_IN]
transBL_SUM_EURO <- transBL_19_24_EURO[, lapply(.SD, sum, na.rm=TRUE), .SDcols = is.numeric] %>% cbind("SEASON" = "SUM")
transBL_MEAN_EURO <- transBL_19_24_EURO[, lapply(.SD, mean, na.rm=TRUE), .SDcols = is.numeric] %>% cbind("SEASON" = "MEAN")

transBL_EURO <- rbindlist(list(transBL_19_24_EURO, transBL_SUM_EURO, transBL_MEAN_EURO), use.names = TRUE)



# Bundesliga Vereine analysieren -----------------------------------------------

# Anzahl
transTEAM_19_24_N <- trans_19_24[TO.COMPETITION_ID == "L1", .(N_IN = .N), by = c("SEASON", "CLUB_NAME" = "TO.CLUB_NAME")] %>% 
  left_join(trans_19_24[FROM.COMPETITION_ID == "L1", .(N_OUT = .N), by = c("SEASON", "CLUB_NAME" = "FROM.CLUB_NAME")], by = c("SEASON", "CLUB_NAME")) %>%
  copy()

transTEAM_19_24_N[, N_TOTAL := N_IN + N_OUT]
transTEAM_19_24_N[, N_DIFF := N_IN - N_OUT]
transTEAM_SUM_N <- transTEAM_19_24_N[, lapply(.SD, sum, na.rm=TRUE), .SDcols = is.numeric, by = c("CLUB_NAME")] %>% cbind("SEASON" = "SUM")
transTEAM_MEAN_N <- transTEAM_19_24_N[, lapply(.SD, mean, na.rm=TRUE), .SDcols = is.numeric, by = c("CLUB_NAME")] %>% cbind("SEASON" = "MEAN")

transTEAM_N <- rbindlist(list(transTEAM_19_24_N, transTEAM_SUM_N, transTEAM_MEAN_N), use.names = TRUE)

# Beiträge
transTEAM_19_24_EURO <- trans_19_24[TO.COMPETITION_ID == "L1", .(EURO_OUT = sum(TRANSFER_FEE_MILLION, na.rm = TRUE)), by = c("SEASON", "CLUB_NAME" = "TO.CLUB_NAME")] %>% 
  left_join(trans_19_24[FROM.COMPETITION_ID == "L1", .(EURO_IN = sum(TRANSFER_FEE_MILLION, na.rm = TRUE)), by = c("SEASON", "CLUB_NAME" = "FROM.CLUB_NAME")], by = c("SEASON", "CLUB_NAME")) %>%
  copy()

transTEAM_19_24_EURO[, UMSATZ := EURO_OUT + EURO_IN]
transTEAM_19_24_EURO[, EURO_DIFF := - EURO_OUT + EURO_IN]
transTEAM_SUM_EURO <- transTEAM_19_24_EURO[, lapply(.SD, sum, na.rm=TRUE), .SDcols = is.numeric, by = c("CLUB_NAME")] %>% cbind("SEASON" = "SUM")
transTEAM_MEAN_EURO <- transTEAM_19_24_EURO[, lapply(.SD, mean, na.rm=TRUE), .SDcols = is.numeric, by = c("CLUB_NAME")] %>% cbind("SEASON" = "MEAN")

transTEAM_EURO <- rbindlist(list(transTEAM_19_24_EURO, transTEAM_SUM_EURO, transTEAM_MEAN_EURO), use.names = TRUE)



# Transferbilanzen mit Tabellen verknüpfen -------------------------------------

# Average Points
stBL_trans_MEAN <- stBL_19_24[, lapply(.SD, mean, na.rm=TRUE), .SDcols = c("RANK", "POINTS"), by = c("TEAM")] %>% cbind("SEASON" = "MEAN") %>%
  left_join(transTEAM_EURO[SEASON == "MEAN"], by = c("SEASON", "TEAM" = "CLUB_NAME")) %>% 
  left_join(logosBL, by = c("TEAM")) %>%
  copy()

setorderv(stBL_trans_MEAN, cols = c("POINTS", "EURO_DIFF"), order = c(-1, -1))

stBL_trans_MEAN_plot <- ggplot(data = stBL_trans_MEAN, aes(x = EURO_DIFF, y = POINTS)) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  geom_image(aes(image = LOGO) , size = 1.2*stBL_trans_MEAN$SIZE, position = "jitter") +
  coord_cartesian(xlim = c(-50, 25), ylim = c(15, 80)) +
  labs(x = "\nAvg. Transfer Balance 19/20-23/24 in mEUR", y = "Avg. Points in BL 19/20-23/24\n")
# ggsave(filename = "stBL_trans_MEAN_points.png", plot = stBL_trans_MEAN_plot, path = pathPlots, dpi=700, width = 20, height = 12, units = "cm")

# lm(POINTS ~ EURO_DIFF, data = stBL_trans_MEAN) %>% summary()                    # LM Check liefert p > 0.05

# Average Rank
stBL_trans_MEAN_rank_plot <- ggplot(data = stBL_trans_MEAN, aes(x = EURO_DIFF, y = RANK)) +
  scale_y_reverse() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  geom_image(aes(image = LOGO) , size = 1.2*stBL_trans_MEAN$SIZE, position = "jitter") +
  labs(x = "\nAvg. Transfer Balance 19/20-23/24 in mEUR", y = "Avg. Rank in BL 19/20-23/24\n")
# ggsave(filename = "stBL_trans_MEAN_rank.png", plot = stBL_trans_MEAN_rank_plot, path = pathPlots, dpi=700, width = 20, height = 12, units = "cm")

# lm(RANK ~ EURO_DIFF, data = stBL_trans_MEAN) %>% summary()                      # LM Check liefert p > 0.05

# Each Points
stBL_trans_19_24 <- stBL_19_24[, .(SEASON, TEAM, RANK, POINTS)] %>%
  left_join(transTEAM_EURO, by = c("SEASON", "TEAM" = "CLUB_NAME")) %>% 
  left_join(logosBL, by = c("TEAM")) %>%
  copy()

setorderv(stBL_trans_19_24, cols = c("TEAM", "SEASON"), order = c(1, 1))

stBL_trans_19_24_plot <- ggplot(data = stBL_trans_19_24, aes(x = EURO_DIFF, y = POINTS)) +
  #geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  geom_image(aes(image = LOGO) , size = stBL_trans_19_24$SIZE, position = "jitter") +
  labs(x = "\nTransfer Balance 19/20-23/24 in mEUR", y = "Points in BL 19/20-23/24\n")
# ggsave(filename = "stBL_trans_19_24_points.png", plot = stBL_trans_19_24_plot, path = pathPlots, dpi=300, width = 20, height = 12, units = "cm")
# lm(POINTS ~ EURO_DIFF, data = stBL_trans_19_24) %>% summary()                   # LM Check liefert p > 0.05

# Each Rank
stBL_trans_19_24_rank_plot <- ggplot(data = stBL_trans_19_24, aes(x = EURO_DIFF, y = RANK)) +
  scale_y_reverse() +
  #geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  geom_image(aes(image = LOGO) , size = stBL_trans_19_24$SIZE) +
  labs(x = "\nTransfer Balance 19/20-23/24 in mEUR", y = "Rank in BL 19/20-23/24\n")
# ggsave(filename = "stBL_trans_19_24_rank.png", plot = stBL_trans_19_24_rank_plot, path = pathPlots, dpi=300, width = 20, height = 12, units = "cm")
# lm(RANK ~ EURO_DIFF, data = stBL_trans_19_24) %>% summary()                     # LM Check liefert p > 0.05

# Each Rank connected
stBL_trans_19_24_rank_combined_plot <- stBL_trans_19_24_rank_plot + 
  geom_path(data = stBL_trans_19_24[TEAM == "Hertha Berlin"], aes(group = TEAM))
# ggsave(filename = "stBL_trans_19_24_rank_combined_Hertha.png", plot = stBL_trans_19_24_rank_combined_plot, path = pathPlots, dpi=300, width = 20, height = 12, units = "cm")

stBL_trans_19_24_rank_combined_plot <- stBL_trans_19_24_rank_plot + 
  geom_path(data = stBL_trans_19_24[TEAM == "Bayern Munich"], aes(group = TEAM))
# ggsave(filename = "stBL_trans_19_24_rank_combined_Bayern.png", plot = stBL_trans_19_24_rank_combined_plot, path = pathPlots, dpi=300, width = 20, height = 12, units = "cm")

stBL_trans_19_24_rank_combined_plot <- stBL_trans_19_24_rank_plot + 
  geom_path(data = stBL_trans_19_24[TEAM == "1. FC Union Berlin"], aes(group = TEAM))
# ggsave(filename = "stBL_trans_19_24_rank_combined_Union.png", plot = stBL_trans_19_24_rank_combined_plot, path = pathPlots, dpi=300, width = 20, height = 12, units = "cm")

stBL_trans_19_24_rank_combined_plot <- stBL_trans_19_24_rank_plot + 
  geom_path(data = stBL_trans_19_24[TEAM == "VfB Stuttgart"], aes(group = TEAM))
# ggsave(filename = "stBL_trans_19_24_rank_combined_Stuttgart.png", plot = stBL_trans_19_24_rank_combined_plot, path = pathPlots, dpi=300, width = 20, height = 12, units = "cm")

stBL_trans_19_24_rank_combined_plot <- stBL_trans_19_24_rank_plot + 
  geom_path(data = stBL_trans_19_24[TEAM == "Borussia Mönchengladbach"], aes(group = TEAM))
# ggsave(filename = "stBL_trans_19_24_rank_combined_Gladbach.png", plot = stBL_trans_19_24_rank_combined_plot, path = pathPlots, dpi=300, width = 20, height = 12, units = "cm")



# Impactanalyse ----------------------------------------------------------------

# Klassifizierung Spieler mit Tabellen verknüpfen
player_class <- stBL_19_24 %>% 
  left_join(stBL_19_24[, .(SEASON, TEAM, 
                           RANK_PREVIOUS = RANK, 
                           RANK_CLASS_PREVIOUS = RANK_CLASS, 
                           POINTS_PREVIOUS = POINTS, 
                           POINTS_HOME_PREVIOUS = POINTS_HOME, 
                           POINTS_AWAY_PREVIOUS = POINTS_AWAY, 
                           GOALS_SCORED_PREVIOUS = GOALS_SCORED, 
                           GOALS_AGAINST_PREVIOUS = GOALS_AGAINST,
                           GOALS_DIFF_PREVIOUS = GOALS_DIFF)], by = c("SEASON_PREVIOUS" = "SEASON", "TEAM")) %>%
  left_join(stBL_19_24[, .(SEASON, TEAM, 
                           RANK_NEXT = RANK, 
                           RANK_CLASS_NEXT = RANK_CLASS, 
                           POINTS_NEXT = POINTS, 
                           POINTS_HOME_NEXT = POINTS_HOME, 
                           POINTS_AWAY_NEXT = POINTS_AWAY, 
                           GOALS_SCORED_NEXT = GOALS_SCORED, 
                           GOALS_AGAINST_NEXT = GOALS_AGAINST,
                           GOALS_DIFF_NEXT = GOALS_DIFF)], by = c("SEASON_NEXT" = "SEASON", "TEAM")) %>%
  full_join(trans_19_24[TO.COMPETITION_ID == "L1"], by = c("SEASON", "TEAM" = "TO.CLUB_NAME")) %>%
  copy()

# Änderung nach 1 Jahr
player_class[, RANK_DIFF_T1 := RANK - RANK_PREVIOUS]
player_class[, POINTS_DIFF_T1 := POINTS - POINTS_PREVIOUS]
player_class[, POINTS_HOME_DIFF_T1 := POINTS_HOME - POINTS_HOME_PREVIOUS]
player_class[, POINTS_AWAY_DIFF_T1 := POINTS_AWAY - POINTS_AWAY_PREVIOUS]
player_class[, GOALS_DIFF_DIFF_T1 := GOALS_DIFF - GOALS_DIFF_PREVIOUS]
player_class[, GOALS_SCORED_DIFF_T1 := GOALS_SCORED - GOALS_SCORED_PREVIOUS]
player_class[, GOALS_AGAINST_DIFF_T1 := GOALS_AGAINST - GOALS_AGAINST_PREVIOUS]

# Änderung nach 2 Jahren
player_class[, RANK_DIFF_T2 := RANK_NEXT - RANK_PREVIOUS]
player_class[, POINTS_DIFF_T2 := POINTS_NEXT - POINTS_PREVIOUS]
player_class[, POINTS_HOME_DIFF_T2 := POINTS_HOME_NEXT - POINTS_HOME_PREVIOUS]
player_class[, POINTS_AWAY_DIFF_T2 := POINTS_AWAY_NEXT - POINTS_AWAY_PREVIOUS]
player_class[, GOALS_DIFF_DIFF_T2 := GOALS_DIFF_NEXT - GOALS_DIFF_PREVIOUS]
player_class[, GOALS_SCORED_DIFF_T2 := GOALS_SCORED_NEXT - GOALS_SCORED_PREVIOUS]
player_class[, GOALS_AGAINST_DIFF_T2 := GOALS_AGAINST_NEXT - GOALS_AGAINST_PREVIOUS]

# Consistency Top Team vs. Rest
transTEAM_N[CLUB_NAME %in% c("RB Leipzig", "Bayern Munich", "Borussia Dortmund", "Bayer 04 Leverkusen") & !(SEASON %in% c("SUM", "MEAN"))] %>%
  .[, .(mean(N_IN))]
transTEAM_N[!CLUB_NAME %in% c("RB Leipzig", "Bayern Munich", "Borussia Dortmund", "Bayer 04 Leverkusen") & !(SEASON %in% c("SUM", "MEAN"))] %>%
  .[, .(mean(N_IN))]

# Age Top Team vs. Rest
player_class[, TEAM_CLASS := case_when(TEAM %in% c("RB Leipzig", "Bayern Munich", "Borussia Dortmund", "Bayer 04 Leverkusen") ~ "Top Teams",
                                       !TEAM %in% c("RB Leipzig", "Bayern Munich", "Borussia Dortmund", "Bayer 04 Leverkusen") ~ "Others",
                                       .default = NA)]

trans_19_24$TEAM_CLASS <- factor(trans_19_24$TEAM_CLASS, levels = c("Top Teams", "Others"))

player_class[TEAM_CLASS == "Top Teams"]$PLAYER_AGE_AT_TRANSFER %>% mean(na.rm = TRUE)
player_class[TEAM_CLASS == "Others"]$PLAYER_AGE_AT_TRANSFER %>% mean(na.rm = TRUE)

age_plot <- ggplot(data = player_class, aes(PLAYER_AGE_AT_TRANSFER, group = TEAM_CLASS, fill = TEAM_CLASS)) +
  geom_density(alpha = 0.5) + 
  labs(x = "\nAge of Signings")
# ggsave(filename = "age_plot.png", plot = age_plot, path = pathPlots, dpi=700, width = 20, height = 12, units = "cm")

# Citizenship Top Team vs. Rest
player_class[TEAM_CLASS == "Top Teams" & grepl('Germany', CITIZENSHIP)] %>% nrow()
player_class[TEAM_CLASS == "Top Teams" & !grepl('Germany', CITIZENSHIP)] %>% nrow()
player_class[TEAM_CLASS == "Others" & grepl('Germany', CITIZENSHIP)] %>% nrow()
player_class[TEAM_CLASS == "Others" & !grepl('Germany', CITIZENSHIP)] %>% nrow()

# Transfer Fee Top Team vs. Rest
player_class[TRANSFER_TYPE_START == "free loan transfer", TRANSFER_FEE_MILLION := 0]
player_class[TEAM_CLASS == "Top Teams"]$TRANSFER_FEE_MILLION %>% mean(na.rm = TRUE)
player_class[TEAM_CLASS == "Others"]$TRANSFER_FEE_MILLION %>% mean(na.rm = TRUE)

# Transfer Realisation
player_class[, REALISATION_DURATION := case_when(SEASON == "19/20" ~ round(difftime("2019-07-01", TRANSFER_DATE_START, units = "days")),
                                                 SEASON == "20/21" ~ round(difftime("2020-07-01", TRANSFER_DATE_START, units = "days")),
                                                 SEASON == "21/22" ~ round(difftime("2021-07-01", TRANSFER_DATE_START, units = "days")),
                                                 SEASON == "22/23" ~ round(difftime("2022-07-01", TRANSFER_DATE_START, units = "days")),
                                                 SEASON == "23/24" ~ round(difftime("2023-07-01", TRANSFER_DATE_START, units = "days")))]

player_class[TEAM_CLASS == "Top Teams" & months(TRANSFER_DATE_START) %in% c("Juli", "August")]$REALISATION_DURATION %>% mean()
player_class[TEAM_CLASS == "Others" & months(TRANSFER_DATE_START) %in% c("Juli", "August")]$REALISATION_DURATION %>% mean()

# Top Player vs Low Player
player_class[MW_CLASS == "Top Player" & RANK_PREVIOUS %in% c(3:16)]$RANK_DIFF_T1 %>% mean(na.rm = TRUE)
player_class[MW_CLASS == "Top Player"]$POINTS_DIFF_T1 %>% mean(na.rm = TRUE)

player_class[MW_CLASS == "Low Player" & RANK_PREVIOUS %in% c(3:16)]$RANK_DIFF_T1 %>% mean(na.rm = TRUE)
player_class[MW_CLASS == "Low Player"]$POINTS_DIFF_T1 %>% mean(na.rm = TRUE)

player_class[RANK_PREVIOUS %in% c(3:16)]$RANK_DIFF_T1 %>% mean(na.rm = TRUE)
player_class$POINTS_DIFF_T1 %>% mean(na.rm = TRUE)

top_low_plot <- ggplot(data = player_class[MW_CLASS %in% c("Top Player", "Low Player") & RANK_PREVIOUS %in% c(3:16)], aes(x = MW_CLASS, y = RANK_DIFF_T1, fill = MW_CLASS)) +
  geom_boxplot(outliers = FALSE, na.rm = TRUE, show.legend = FALSE, staplewidth = 0.25) +
  labs(x = "\nLow Players vs. Top Players", y = "Avg. Change in League Position after one Season\n")
# ggsave(filename = "top_low_plot.png", plot = top_low_plot, path = pathPlots, dpi=700)

# Experienced vs. Young
player_class[EXP_CLASS == "Experienced Player" & RANK_PREVIOUS %in% c(3:16)]$RANK_DIFF_T1 %>% mean(na.rm = TRUE)
player_class[EXP_CLASS == "Experienced Player"]$POINTS_DIFF_T1 %>% mean(na.rm = TRUE)

player_class[EXP_CLASS == "Young Player" & RANK_PREVIOUS %in% c(3:16)]$RANK_DIFF_T1 %>% mean(na.rm = TRUE)
player_class[EXP_CLASS == "Young Player"]$POINTS_DIFF_T1 %>% mean(na.rm = TRUE)

player_class[RANK_PREVIOUS %in% c(3:16)]$RANK_DIFF_T1 %>% mean(na.rm = TRUE)
player_class$POINTS_DIFF_T1 %>% mean(na.rm = TRUE)

exp_young_plot <- ggplot(data = player_class[EXP_CLASS %in% c("Experienced Player", "Young Player")], aes(x = EXP_CLASS, y = POINTS_DIFF_T1, fill = EXP_CLASS)) +
  geom_boxplot(outliers = FALSE, na.rm = TRUE, show.legend = FALSE, staplewidth = 0.25) +
  labs(x = "\nYoung Players vs. Experienced Players", y = "Avg. Change in League Points after one Season\n")
# ggsave(filename = "exp_young_plot.png", plot = exp_young_plot, path = pathPlots, dpi=700)

# Verkreuzung MW x EXP
player_class[, EXP_CLASS_2 := case_when(PLAYER_AGE_AT_TRANSFER <= 18 ~ "0-18",
                                        PLAYER_AGE_AT_TRANSFER %in% 19:22 ~ "19-22",
                                        PLAYER_AGE_AT_TRANSFER %in% 23:26 ~ "23-26",
                                        PLAYER_AGE_AT_TRANSFER %in% 27:30 ~ "27-30",
                                        PLAYER_AGE_AT_TRANSFER %in% 31:34 ~ "31-34",
                                        PLAYER_AGE_AT_TRANSFER >= 35 ~ "35+",
                                        .default = NA)]
player_class[, MW_CLASS_2 := case_when(MARKET_VALUE_START < 1 ~ "0-1",
                                       MARKET_VALUE_START >= 1 & MARKET_VALUE_START < 3 ~ "1-3",
                                       MARKET_VALUE_START >= 3 & MARKET_VALUE_START < 5 ~ "3-5",
                                       MARKET_VALUE_START >= 5 & MARKET_VALUE_START < 10 ~ "5-10",
                                       MARKET_VALUE_START >= 10 & MARKET_VALUE_START < 15 ~ "10-15",
                                       MARKET_VALUE_START >= 15 & MARKET_VALUE_START < 25 ~ "15-25",
                                       MARKET_VALUE_START >= 25 ~ "25+",
                                       .default = NA)]

player_class$EXP_CLASS_2 <- factor(player_class$EXP_CLASS_2, levels = c("0-18", "19-22", "23-26", "27-30", "31-34", "35+"))
player_class$MW_CLASS_2 <- factor(player_class$MW_CLASS_2, levels = c("0-1", "1-3", "3-5", "5-10", "10-15", "15-25", "25+"))

data_exp_mw <- player_class[, .(EXP_CLASS_2, MW_CLASS_2)] %>%
  cbind("RANK_DIFF_T1_STD" = scale(player_class$RANK_DIFF_T1) %>% as.vector()) %>%                                          # Standardisiere Rangentwicklung
  copy()

data_exp_mw <- data_exp_mw[!is.na(EXP_CLASS_2) & !is.na(MW_CLASS_2), .(RANK_AVG_STD = mean(RANK_DIFF_T1_STD, na.rm = TRUE)), by = c("EXP_CLASS_2", "MW_CLASS_2")] %>% copy()
data_exp_mw$RANK_AVG_STD <- round(data_exp_mw$RANK_AVG_STD, 2)

exp_mw_plot <- ggplot(data = data_exp_mw, aes(x = EXP_CLASS_2, y = MW_CLASS_2)) +
  geom_tile(aes(fill = RANK_AVG_STD), na.rm = TRUE, color = "white", lwd = 1.5) +
  scale_fill_gradient2(low = "darkblue", mid = "white", high = "red", name = "Avg. std. Change in League Position") +
  guides(fill = guide_colourbar(barwidth = 0.5, barheight = 25, title.position = "right")) +
  geom_text(aes(label = RANK_AVG_STD), color = "black", size = 3) +
  theme(legend.key.height = unit(2.5, "cm"),
        legend.title = element_text(size = 12, angle = 90, hjust = 0.5),
        legend.direction = "vertical") + 
  labs(x = "Age", y = "Market Value in mEUR\n")
# ggsave(filename = "mw_exp_plot.png", plot = exp_mw_plot, path = pathPlots, dpi=700)

# Position of players 
player_class[POS_CLASS == "GK" & RANK_PREVIOUS %in% c(3:16)]$RANK_DIFF_T1 %>% mean(na.rm = TRUE)
player_class[POS_CLASS == "GK"]$POINTS_DIFF_T1 %>% mean(na.rm = TRUE)

player_class[POS_CLASS == "DEF" & RANK_PREVIOUS %in% c(3:16)]$RANK_DIFF_T1 %>% mean(na.rm = TRUE)
player_class[POS_CLASS == "DEF"]$POINTS_DIFF_T1 %>% mean(na.rm = TRUE)

player_class[POS_CLASS == "MID" & RANK_PREVIOUS %in% c(3:16)]$RANK_DIFF_T1 %>% mean(na.rm = TRUE)
player_class[POS_CLASS == "MID"]$POINTS_DIFF_T1 %>% mean(na.rm = TRUE)

player_class[POS_CLASS == "FOR" & RANK_PREVIOUS %in% c(3:16)]$RANK_DIFF_T1 %>% mean(na.rm = TRUE)
player_class[POS_CLASS == "FOR"]$POINTS_DIFF_T1 %>% mean(na.rm = TRUE)

player_class[RANK_PREVIOUS %in% c(3:16)]$RANK_DIFF_T1 %>% mean(na.rm = TRUE)
player_class$POINTS_DIFF_T1 %>% mean(na.rm = TRUE)

data_pos <- player_class[, .(POS_CLASS)] %>%
  cbind("RANK_DIFF_T1_STD" = scale(player_class$RANK_DIFF_T1) %>% as.vector()) %>%                                          # Standardisiere Rangentwicklung
  copy()

pos_plot <- ggplot(data = data_pos, aes(x = POS_CLASS, y = RANK_DIFF_T1_STD, fill = POS_CLASS)) +
  geom_boxplot(outliers = FALSE, na.rm = TRUE, show.legend = FALSE, staplewidth = 0.25) +
  labs(x = "\nPosition of Players", y = "Avg. std. Change in League Position after one Season\n")
# ggsave(filename = "pos_plot.png", plot = pos_plot, path = pathPlots, dpi=700)

# Verkreuzung MW x Position
data_pos_mw <- player_class[, .(POS_CLASS, MW_CLASS_2)] %>%
  cbind("RANK_DIFF_T1_STD" = scale(player_class$RANK_DIFF_T1) %>% as.vector()) %>%                                          # Standardisiere Rangentwicklung
  copy()

data_pos_mw <- data_pos_mw[!is.na(POS_CLASS) & !is.na(MW_CLASS_2), .(RANK_AVG_STD = mean(RANK_DIFF_T1_STD, na.rm = TRUE)), by = c("POS_CLASS", "MW_CLASS_2")] %>% copy()
data_pos_mw$RANK_AVG_STD <- round(data_pos_mw$RANK_AVG_STD, 2)

pos_mw_plot <- ggplot(data = data_pos_mw, aes(x = POS_CLASS, y = MW_CLASS_2)) +
  geom_tile(aes(fill = RANK_AVG_STD), na.rm = TRUE, color = "white", lwd = 1.5) +
  scale_fill_gradient2(low = "darkblue", mid = "white", high = "red", name = "Avg. std. Change in League Position") +
  guides(fill = guide_colourbar(barwidth = 0.5, barheight = 25, title.position = "right")) +
  geom_text(aes(label = RANK_AVG_STD), color = "black", size = 3) +
  theme(legend.key.height = unit(2.5, "cm"),
        legend.title = element_text(size = 12, angle = 90, hjust = 0.5),
        legend.direction = "vertical") + 
  labs(x = "Position", y = "Market Value in mEUR\n")
# ggsave(filename = "mw_pos_plot.png", plot = pos_mw_plot, path = pathPlots, dpi=700)

# Goals Scored durch Stürmertransfers erklärbar?
data_FOR <- player_class[POS_CLASS == "FOR", .(TRANSFER_FEE_MILLION = sum(TRANSFER_FEE_MILLION, na.rm = TRUE)), by = c("SEASON", "TEAM")] %>%
  left_join(player_class[, .(SEASON, TEAM, GOALS_SCORED_PREVIOUS, GOALS_SCORED, GOALS_SCORED_NEXT, GOALS_SCORED_DIFF_T1, GOALS_SCORED_DIFF_T2)] %>% unique(), by = c("SEASON", "TEAM")) %>%
  left_join(logosBL, by = c("TEAM")) %>%
  copy()

for_plot_19_24 <- ggplot(data = data_FOR, aes(x = TRANSFER_FEE_MILLION, y = GOALS_SCORED)) +
  #geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  geom_image(aes(image = LOGO) , size = 0.5 * data_FOR$SIZE, position = "jitter") +
  labs(x = "\nTransfer Spendings in Forwards 19/20-23/24 in mEUR", y = "Goals Scored in BL 19/20-23/24\n")
# ggsave(filename = "for_plot_19_24.png", plot = for_plot_19_24, path = pathPlots, dpi=300, width = 20, height = 12, units = "cm")
# lm(GOALS_SCORED ~ TRANSFER_FEE_MILLION, data = data_FOR) %>% summary()

# Goals Against durch Verteidigertransfers erklärbar?
data_DEF <- player_class[POS_CLASS == "DEF", .(TRANSFER_FEE_MILLION = sum(TRANSFER_FEE_MILLION, na.rm = TRUE)), by = c("SEASON", "TEAM")] %>%
  left_join(player_class[, .(SEASON, TEAM, GOALS_AGAINST_PREVIOUS, GOALS_AGAINST, GOALS_AGAINST_NEXT, GOALS_AGAINST_DIFF_T1, GOALS_AGAINST_DIFF_T2)] %>% unique(), by = c("SEASON", "TEAM")) %>%
  left_join(logosBL, by = c("TEAM")) %>%
  copy()

def_plot_19_24 <- ggplot(data = data_DEF, aes(x = TRANSFER_FEE_MILLION, y = GOALS_AGAINST)) +
  #geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  geom_image(aes(image = LOGO) , size = 0.5 * data_DEF$SIZE, position = "jitter") +
  labs(x = "\nTransfer Spendings in Defense 19/20-23/24 in mEUR", y = "Goals Against in BL 19/20-23/24\n")
# ggsave(filename = "def_plot_19_24.png", plot = def_plot_19_24, path = pathPlots, dpi=300, width = 20, height = 12, units = "cm")
# lm(GOALS_AGAINST ~ TRANSFER_FEE_MILLION, data = data_DEF) %>% summary()

# ROI
ROI <- player_class[, .(SEASON, TEAM, RANK_DIFF_T1, RANK_DIFF_T2)] %>% unique() %>%
  left_join(transTEAM_EURO, by = c("SEASON", "TEAM" = "CLUB_NAME")) %>% 
  .[, .(SEASON, TEAM, ROI_T1 = RANK_DIFF_T1 / EURO_OUT, ROI_T2 = RANK_DIFF_T2 / EURO_OUT, EURO_OUT, RANK_DIFF_T1, RANK_DIFF_T2)] %>%
  .[!is.na(ROI_T1) | !is.na(ROI_T2)] %>%
  copy()

setorderv(ROI, cols = "ROI_T1")

# Consistency is key?
data_con <- transTEAM_N[!SEASON %in% c("SUM", "MEAN")] %>%
  left_join(player_class[, .(SEASON, TEAM, RANK, RANK_DIFF_T1, RANK_DIFF_T2, POINTS, POINTS_DIFF_T1, POINTS_DIFF_T2)] %>% unique(), by = c("SEASON", "CLUB_NAME" = "TEAM")) %>%
  left_join(logosBL, by = c( "CLUB_NAME" = "TEAM")) %>%
  copy()

con_plot <- ggplot(data = data_con, aes(x = N_IN, y = POINTS)) +
  #geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  geom_image(aes(image = LOGO) , size = data_con$SIZE, position = "jitter") +
  labs(x = "Number of Signings\n", y = "Points in BL 19/20-23/24\n")
# ggsave(filename = "con_plot.png", plot = con_plot, path = pathPlots, dpi=300, width = 20, height = 12, units = "cm")

# Stürmer bringen mehr Geld ein, Frankreich tolles Scoutingziel 
trans_19_24[FROM.COMPETITION_ID == "L1"] %>%
  .[order(-TRANSFER_FEE_MILLION)] %>%
  .[1:100, .(POS_CLASS)] %>%
  table()

trans_19_24[FROM.COMPETITION_ID == "L1"] %>%
  .[order(-TRANSFER_FEE_MILLION)] %>%
  .[1:100] %>% 
  .[grepl('Germany', CITIZENSHIP)]

trans_19_24[TRANSFER_TYPE_START %in% c("paid transfer"), .(TRANSFER_FEE_AVG = mean(TRANSFER_FEE_MILLION, na.rm = TRUE)), by = c("POS_CLASS")]

# Transfers von Konkurrenz 
trans_19_24[FROM.COMPETITION_ID == "L1" & TO.COMPETITION_ID == "L1"] %>%
  .[, .(TO.CLUB_NAME)] %>%
  table()

trans_19_24[FROM.COMPETITION_ID == "L1" & TO.COMPETITION_ID == "L1"] %>%
  .[TO.CLUB_NAME == "1. FC Union Berlin", .(SEASON)] %>%
  table()