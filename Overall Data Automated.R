library(rvest)
library(httr)
library(dplyr)
library(stringr)
library(tidyverse)
library(readxl)
library(car)
library(imputeTS)
library(shiny)
library(ggplot2)
library(tidyr)
library(lubridate)
library(forecast)
library(caret)
library(rpart)
library(randomForest)
library(gbm)
library(xgboost)


overall_data <- read_excel("C:/Users/vtali/Downloads/Syracuse Classes/Senior Semester 1/SAL 495/Project Data/Overall Data.xlsx")

# creating variables for all of the game urls that I will be using
ravens_game_2022_1 <- "https://www.espn.com/nfl/game/_/gameId/401437632/ravens-jets"
ravens_game_2022_2 <- "https://www.espn.com/nfl/game/_/gameId/401437633/dolphins-ravens"
ravens_game_2022_3 <- "https://www.espn.com/nfl/game/_/gameId/401437631/ravens-patriots"
ravens_game_2022_4 <- "https://www.espn.com/nfl/game/_/gameId/401437750/bills-ravens"
ravens_game_2022_5 <- "https://www.espn.com/nfl/game/_/gameId/401437775/bengals-ravens"
ravens_game_2022_6 <- "https://www.espn.com/nfl/game/_/gameId/401437784/ravens-giants"
ravens_game_2022_7 <- "https://www.espn.com/nfl/game/_/gameId/401437792/browns-ravens"
ravens_game_2022_10 <- "https://www.espn.com/nfl/game/_/gameId/401437846/panthers-ravens"
ravens_game_2022_11 <- "https://www.espn.com/nfl/game/_/gameId/401437862/ravens-jaguars"
ravens_game_2022_12 <- "https://www.espn.com/nfl/game/_/gameId/401437875/broncos-ravens"
ravens_game_2022_13 <- "https://www.espn.com/nfl/game/_/gameId/401437894/ravens-steelers"
ravens_game_2022_16 <- "https://www.espn.com/nfl/game/_/gameId/401437934/steelers-ravens"
ravens_game_2022_17 <- "https://www.espn.com/nfl/game/_/gameId/401437951/ravens-bengals"
ravens_game_2023_1 <- "https://www.espn.com/nfl/game/_/gameId/401547396/texans-ravens"
ravens_game_2023_2 <- "https://www.espn.com/nfl/game/_/gameId/401547412/ravens-bengals"
ravens_game_2023_3 <- "https://www.espn.com/nfl/game/_/gameId/401547427/colts-ravens"
ravens_game_2023_4 <- "https://www.espn.com/nfl/game/_/gameId/401547445/ravens-browns"
ravens_game_2023_5 <- "https://www.espn.com/nfl/game/_/gameId/401547460/ravens-steelers"
ravens_game_2023_6 <- "https://www.espn.com/nfl/game/_/gameId/401547229/ravens-titans"
ravens_game_2023_7 <- "https://www.espn.com/nfl/game/_/gameId/401547489/lions-ravens"
ravens_game_2023_8 <- "https://www.espn.com/nfl/game/_/gameId/401547501/ravens-cardinals"
ravens_game_2023_9 <- "https://www.espn.com/nfl/game/_/gameId/401547513/seahawks-ravens"
ravens_game_2023_10 <- "https://www.espn.com/nfl/game/_/gameId/401547531/browns-ravens"
ravens_game_2023_12 <- "https://www.espn.com/nfl/game/_/gameId/401547564/ravens-chargers"
ravens_game_2023_13 <- "https://www.espn.com/nfl/game/_/gameId/401547585/rams-ravens"
ravens_game_2023_14 <- "https://www.espn.com/nfl/game/_/gameId/401547602/ravens-jaguars"
ravens_game_2023_16 <- "https://www.espn.com/nfl/game/_/gameId/401547625/dolphins-ravens"
bears_game_2022_1 <- "https://www.espn.com/nfl/game/_/gameId/401437647/49ers-bears"
bears_game_2022_2 <- "https://www.espn.com/nfl/game/_/gameId/401437606/bears-packers"
bears_game_2022_3 <- "https://www.espn.com/nfl/game/_/gameId/401437737/texans-bears"
bears_game_2022_4 <- "https://www.espn.com/nfl/game/_/gameId/401437755/bears-giants"
bears_game_2022_5 <- "https://www.espn.com/nfl/game/_/gameId/401437766/bears-vikings"
bears_game_2022_8 <- "https://www.espn.com/nfl/game/_/gameId/401437807/bears-cowboys"
bears_game_2022_9 <- "https://www.espn.com/nfl/game/_/gameId/401437821/dolphins-bears"
bears_game_2022_10 <- "https://www.espn.com/nfl/game/_/gameId/401437834/lions-bears"
bears_game_2022_11 <- "https://www.espn.com/nfl/game/_/gameId/401437845/bears-falcons"
bears_game_2022_12 <- "https://www.espn.com/nfl/game/_/gameId/401437864/bears-jets"
bears_game_2022_13 <- "https://www.espn.com/nfl/game/_/gameId/401437876/packers-bears"
bears_game_2022_14 <- "https://www.espn.com/nfl/game/_/gameId/401437908/eagles-bears"
bears_game_2022_16 <- "https://www.espn.com/nfl/game/_/gameId/401437935/bears-lions"
bears_game_2022_17 <- "https://www.espn.com/nfl/game/_/gameId/401437950/vikings-bears"
bears_game_2023_1 <- "https://www.espn.com/nfl/game/_/gameId/401547407/packers-bears"
bears_game_2023_2 <- "https://www.espn.com/nfl/game/_/gameId/401547420/bears-buccaneers"
bears_game_2023_3 <- "https://www.espn.com/nfl/game/_/gameId/401547438/bears-chiefs"
bears_game_2023_4 <- "https://www.espn.com/nfl/game/_/gameId/401547444/broncos-bears"
bears_game_2023_6 <- "https://www.espn.com/nfl/game/_/gameId/401547475/vikings-bears"
bears_game_2023_7 <- "https://www.espn.com/nfl/game/_/gameId/401547490/raiders-bears"
bears_game_2023_8 <- "https://www.espn.com/nfl/game/_/gameId/401547510/bears-chargers"
bears_game_2023_9 <- "https://www.espn.com/nfl/game/_/gameId/401547516/bears-saints"
bears_game_2023_11 <- "https://www.espn.com/nfl/game/_/gameId/401547546/bears-lions"
bears_game_2023_13 <- "https://www.espn.com/nfl/game/_/gameId/401547586/lions-bears"
bears_game_2023_14 <- "https://www.espn.com/nfl/game/_/gameId/401547605/bears-browns"
bears_game_2023_15 <- "https://www.espn.com/nfl/game/_/gameId/401547619/cardinals-bears"
bears_game_2023_16 <- "https://www.espn.com/nfl/game/_/gameId/401547627/falcons-bears"
bears_game_2023_17 <- "https://www.espn.com/nfl/game/_/gameId/401547643/bears-packers"
lions_game_2022_1 <- "https://www.espn.com/nfl/game/_/gameId/401437648/eagles-lions"
lions_game_2022_2 <- "https://www.espn.com/nfl/game/_/gameId/401437732/commanders-lions"
lions_game_2022_3 <- "https://www.espn.com/nfl/game/_/gameId/401437739/lions-vikings"
lions_game_2022_4 <- "https://www.espn.com/nfl/game/_/gameId/401437752/seahawks-lions"
lions_game_2022_5 <- "https://www.espn.com/nfl/game/_/gameId/401437767/lions-patriots"
lions_game_2022_6 <- "https://www.espn.com/nfl/game/_/gameId/401437795/lions-cowboys"
lions_game_2022_7 <- "https://www.espn.com/nfl/game/_/gameId/401437808/dolphins-lions"
lions_game_2022_8 <- "https://www.espn.com/nfl/game/_/gameId/401437823/packers-lions"
lions_game_2022_9 <- "https://www.espn.com/nfl/game/_/gameId/401437834/lions-bears"
lions_game_2022_10 <- "https://www.espn.com/nfl/game/_/gameId/401437852/lions-giants"
lions_game_2022_12 <- "https://www.espn.com/nfl/game/_/gameId/401437877/jaguars-lions"
lions_game_2022_13 <- "https://www.espn.com/nfl/game/_/gameId/401437892/vikings-lions"
lions_game_2022_14 <- "https://www.espn.com/nfl/game/_/gameId/401437911/lions-jets"
lions_game_2022_16 <- "https://www.espn.com/nfl/game/_/gameId/401437935/bears-lions"
lions_game_2022_17 <- "https://www.espn.com/nfl/game/_/gameId/401437952/lions-packers"
lions_game_2023_2 <- "https://www.espn.com/nfl/game/_/gameId/401547418/seahawks-lions"
lions_game_2023_3 <- "https://www.espn.com/nfl/game/_/gameId/401547433/falcons-lions"
lions_game_2023_5 <- "https://www.espn.com/nfl/game/_/gameId/401547464/panthers-lions"
lions_game_2023_6 <- "https://www.espn.com/nfl/game/_/gameId/401547478/lions-buccaneers"
lions_game_2023_7 <- "https://www.espn.com/nfl/game/_/gameId/401547489/lions-ravens"
lions_game_2023_9 <- "https://www.espn.com/nfl/game/_/gameId/401547530/lions-chargers"
lions_game_2023_10 <- "https://www.espn.com/nfl/game/_/gameId/401547546/bears-lions"
lions_game_2023_12 <- "https://www.espn.com/nfl/game/_/gameId/401547572/lions-saints"
lions_game_2023_13 <- "https://www.espn.com/nfl/game/_/gameId/401547586/lions-bears"
lions_game_2023_15 <- "https://www.espn.com/nfl/game/_/gameId/401547618/lions-vikings"
lions_game_2023_17 <- "https://www.espn.com/nfl/game/_/gameId/401547642/vikings-lions"
colts_game_2022_1 <- "https://www.espn.com/nfl/game/_/gameId/401437637/colts-texans"
colts_game_2022_2 <- "https://www.espn.com/nfl/game/_/gameId/401437639/colts-jaguars"
colts_game_2022_3 <- "https://www.espn.com/nfl/game/_/gameId/401437638/chiefs-colts"
colts_game_2022_4 <- "https://www.espn.com/nfl/game/_/gameId/401437754/titans-colts"
colts_game_2022_6 <- "https://www.espn.com/nfl/game/_/gameId/401437781/jaguars-colts"
colts_game_2022_7 <- "https://www.espn.com/nfl/game/_/gameId/401437797/colts-titans"
colts_game_2022_8 <- "https://www.espn.com/nfl/game/_/gameId/401437814/commanders-colts"
colts_game_2022_9 <- "https://www.espn.com/nfl/game/_/gameId/401437825/colts-patriots"
colts_game_2022_10 <- "https://www.espn.com/nfl/game/_/gameId/401437840/colts-raiders"
colts_game_2022_11 <- "https://www.espn.com/nfl/game/_/gameId/401437849/eagles-colts"
colts_game_2022_13 <- "https://www.espn.com/nfl/game/_/gameId/401437886/colts-cowboys"
colts_game_2022_16 <- "https://www.espn.com/nfl/game/_/gameId/401437939/colts-giants"
colts_game_2022_17 <- "https://www.espn.com/nfl/game/_/gameId/401437953/texans-colts"
colts_game_2023_1 <- "https://www.espn.com/nfl/boxscore/_/gameId/401547404"
colts_game_2023_2 <- "https://www.espn.com/nfl/game/_/gameId/401547419/colts-texans"
colts_game_2023_3 <- "https://www.espn.com/nfl/game/_/gameId/401547427/colts-ravens"
colts_game_2023_4 <- "https://www.espn.com/nfl/game/_/gameId/401547449/rams-colts"
colts_game_2023_5 <- "https://www.espn.com/nfl/boxscore/_/gameId/401547458"
colts_game_2023_6 <- "https://www.espn.com/nfl/game/_/gameId/401547472/colts-jaguars"
colts_game_2023_7 <- "https://www.espn.com/nfl/game/_/gameId/401547484/browns-colts"
colts_game_2023_8 <- "https://www.espn.com/nfl/game/_/gameId/401547507/saints-colts"
colts_game_2023_9 <- "https://www.espn.com/nfl/game/_/gameId/401547517/colts-panthers"
colts_game_2023_10 <- "https://www.espn.com/nfl/game/_/gameId/401547231/colts-patriots"
colts_game_2023_11 <- "https://www.espn.com/nfl/game/_/gameId/401547556/buccaneers-colts"
colts_game_2023_12 <- "https://www.espn.com/nfl/game/_/gameId/401547570/colts-titans"
colts_game_2023_13 <- "https://www.espn.com/nfl/game/_/gameId/401547580/colts-bengals"
colts_game_2023_15 <- "https://www.espn.com/nfl/game/_/gameId/401547616/colts-falcons"
colts_game_2023_17 <- "https://www.espn.com/nfl/game/_/gameId/401547644/texans-colts"
jaguars_game_2022_1 <- "https://www.espn.com/nfl/game/_/gameId/401437646/jaguars-commanders"
jaguars_game_2022_2 <- "https://www.espn.com/nfl/game/_/gameId/401437639/colts-jaguars"
jaguars_game_2022_3 <- "https://www.espn.com/nfl/game/_/gameId/401437743/jaguars-chargers"
jaguars_game_2022_4 <- "https://www.espn.com/nfl/game/_/gameId/401437756/jaguars-eagles"
jaguars_game_2022_5 <- "https://www.espn.com/nfl/game/_/gameId/401437765/texans-jaguars"
jaguars_game_2022_6 <- "https://www.espn.com/nfl/game/_/gameId/401437781/jaguars-colts"
jaguars_game_2022_7 <- "https://www.espn.com/nfl/game/_/gameId/401437796/giants-jaguars"
jaguars_game_2022_8 <- "https://www.espn.com/nfl/game/_/gameId/401435641/broncos-jaguars"
jaguars_game_2022_9 <- "https://www.espn.com/nfl/game/_/gameId/401437824/raiders-jaguars"
jaguars_game_2022_10 <- "https://www.espn.com/nfl/game/_/gameId/401437835/jaguars-chiefs"
jaguars_game_2022_11 <- "https://www.espn.com/nfl/game/_/gameId/401437862/ravens-jaguars"
jaguars_game_2022_12 <- "https://www.espn.com/nfl/game/_/gameId/401437877/jaguars-lions"
jaguars_game_2022_13 <- "https://www.espn.com/nfl/game/_/gameId/401437895/jaguars-titans"
jaguars_game_2022_14 <- "https://www.espn.com/nfl/game/_/gameId/401437910/cowboys-jaguars"
jaguars_game_2022_16 <- "https://www.espn.com/nfl/game/_/gameId/401437936/jaguars-texans"
jaguars_game_2023_1 <- "https://www.espn.com/nfl/game/_/gameId/401547404/jaguars-colts"
jaguars_game_2023_2 <- "https://www.espn.com/nfl/game/_/gameId/401547413/chiefs-jaguars"
jaguars_game_2023_3 <- "https://www.espn.com/nfl/game/_/gameId/401547435/texans-jaguars"
jaguars_game_2023_4 <- "https://www.espn.com/nfl/game/_/gameId/401547227/falcons-jaguars"
jaguars_game_2023_5 <- "https://www.espn.com/nfl/game/_/gameId/401547228/jaguars-bills"
jaguars_game_2023_6 <- "https://www.espn.com/nfl/game/_/gameId/401547472/colts-jaguars"
jaguars_game_2023_8 <- "https://www.espn.com/nfl/game/_/gameId/401547499/jaguars-steelers"
jaguars_game_2023_9 <- "https://www.espn.com/nfl/game/_/gameId/401547532/49ers-jaguars"
jaguars_game_2023_10 <- "https://www.espn.com/nfl/game/_/gameId/401547541/titans-jaguars"
jaguars_game_2023_11 <- "https://www.espn.com/nfl/game/_/gameId/401547555/jaguars-texans"
jaguars_game_2023_13 <- "https://www.espn.com/nfl/game/_/gameId/401547581/jaguars-browns"
jaguars_game_2023_14 <- "https://www.espn.com/nfl/game/_/gameId/401547602/ravens-jaguars"
jaguars_game_2023_15 <- "https://www.espn.com/nfl/game/_/gameId/401547615/jaguars-buccaneers"
jaguars_game_2023_16 <- "https://www.espn.com/nfl/game/_/gameId/401547629/panthers-jaguars"
jaguars_game_2023_17 <- "https://www.espn.com/nfl/game/_/gameId/401547652/jaguars-titans"
titans_game_2022_1 <- "https://www.espn.com/nfl/game/_/gameId/401437640/giants-titans"
titans_game_2022_3 <- "https://www.espn.com/nfl/game/_/gameId/401437741/raiders-titans"
titans_game_2022_4 <- "https://www.espn.com/nfl/game/_/gameId/401437754/titans-colts"
titans_game_2022_5 <- "https://www.espn.com/nfl/game/_/gameId/401437771/titans-commanders"
titans_game_2022_6 <- "https://www.espn.com/nfl/game/_/gameId/401437797/colts-titans"
titans_game_2022_7 <- "https://www.espn.com/nfl/game/_/gameId/401437813/titans-texans"
titans_game_2022_8 <- "https://www.espn.com/nfl/game/_/gameId/401437830/titans-chiefs"
titans_game_2022_9 <- "https://www.espn.com/nfl/game/_/gameId/401437839/broncos-titans"
titans_game_2022_11 <- "https://www.espn.com/nfl/game/_/gameId/401437865/bengals-titans"
titans_game_2022_12 <- "https://www.espn.com/nfl/game/_/gameId/401437881/titans-eagles"
titans_game_2022_13 <- "https://www.espn.com/nfl/game/_/gameId/401437895/jaguars-titans"
titans_game_2022_14 <- "https://www.espn.com/nfl/game/_/gameId/401437913/titans-chargers"
titans_game_2023_1 <- "https://www.espn.com/nfl/game/_/gameId/401547399/titans-saints"
titans_game_2023_2 <- "https://www.espn.com/nfl/game/_/gameId/401547414/chargers-titans"
titans_game_2023_3 <- "https://www.espn.com/nfl/game/_/gameId/401547428/titans-browns"
titans_game_2023_4 <- "https://www.espn.com/nfl/game/_/gameId/401547452/bengals-titans"
titans_game_2023_5 <- "https://www.espn.com/nfl/game/_/gameId/401547458/titans-colts"
titans_game_2023_6 <- "https://www.espn.com/nfl/game/_/gameId/401547229/ravens-titans"
titans_game_2023_7 <- "https://www.espn.com/nfl/game/_/gameId/401547500/falcons-titans"
titans_game_2023_9 <- "https://www.espn.com/nfl/game/_/gameId/401547528/titans-buccaneers"
titans_game_2023_10 <- "https://www.espn.com/nfl/game/_/gameId/401547541/titans-jaguars"
titans_game_2023_11 <- "https://www.espn.com/nfl/game/_/gameId/401547561/panthers-titans"
titans_game_2023_12 <- "https://www.espn.com/nfl/game/_/gameId/401547570/colts-titans"
titans_game_2023_14 <- "https://www.espn.com/nfl/game/_/gameId/401547595/texans-titans"
titans_game_2023_15 <- "https://www.espn.com/nfl/game/_/gameId/401547614/seahawks-titans"
titans_game_2023_16 <- "https://www.espn.com/nfl/game/_/gameId/401547631/titans-texans"
titans_game_2023_17 <- "https://www.espn.com/nfl/game/_/gameId/401547652/jaguars-titans"


# creating a vector to store all of the game urls in
overall_games <- c(ravens_game_2022_1, ravens_game_2022_2, ravens_game_2022_3, ravens_game_2022_4, ravens_game_2022_5,
                   ravens_game_2022_6, ravens_game_2022_7, ravens_game_2022_10, ravens_game_2022_11, ravens_game_2022_12,
                   ravens_game_2022_13, ravens_game_2022_16, ravens_game_2022_17, ravens_game_2023_1, ravens_game_2023_2, 
                   ravens_game_2023_3, ravens_game_2023_4, ravens_game_2023_5, ravens_game_2023_6, ravens_game_2023_7, 
                   ravens_game_2023_8, ravens_game_2023_9, ravens_game_2023_10, ravens_game_2023_12, ravens_game_2023_13, 
                   ravens_game_2023_14, ravens_game_2023_16, bears_game_2022_1, bears_game_2022_2, bears_game_2022_3, 
                   bears_game_2022_4, bears_game_2022_5, bears_game_2022_8, bears_game_2022_9, bears_game_2022_10, 
                   bears_game_2022_11, bears_game_2022_12, bears_game_2022_13, bears_game_2022_14, bears_game_2022_16, 
                   bears_game_2022_17, bears_game_2023_1, bears_game_2023_2, bears_game_2023_3, bears_game_2023_4, 
                   bears_game_2023_6, bears_game_2023_7, bears_game_2023_8, bears_game_2023_9, bears_game_2023_11, 
                   bears_game_2023_13, bears_game_2023_14, bears_game_2023_15, bears_game_2023_16, bears_game_2023_17, 
                   lions_game_2022_1, lions_game_2022_2, lions_game_2022_3, lions_game_2022_4, lions_game_2022_5,
                   lions_game_2022_6, lions_game_2022_7, lions_game_2022_8, lions_game_2022_9, lions_game_2022_10,
                   lions_game_2022_12, lions_game_2022_13, lions_game_2022_14, lions_game_2022_16, lions_game_2022_17,
                   lions_game_2023_2, lions_game_2023_3, lions_game_2023_5, lions_game_2023_6, lions_game_2023_7, 
                   lions_game_2023_9, lions_game_2023_10, lions_game_2023_12, lions_game_2023_13, lions_game_2023_15, 
                   lions_game_2023_17, colts_game_2022_1, colts_game_2022_2, colts_game_2022_3, colts_game_2022_4, 
                   colts_game_2022_6, colts_game_2022_7, colts_game_2022_8, colts_game_2022_9, colts_game_2022_10, 
                   colts_game_2022_11, colts_game_2022_13, colts_game_2022_16, colts_game_2022_17, colts_game_2023_1, 
                   colts_game_2023_2, colts_game_2023_3, colts_game_2023_4, colts_game_2023_5, colts_game_2023_6, colts_game_2023_7, 
                   colts_game_2023_8, colts_game_2023_9, colts_game_2023_10, colts_game_2023_11, colts_game_2023_12, 
                   colts_game_2023_13, colts_game_2023_15, colts_game_2023_17, jaguars_game_2022_1, jaguars_game_2022_2, 
                   jaguars_game_2022_3, jaguars_game_2022_4, jaguars_game_2022_5, jaguars_game_2022_6, jaguars_game_2022_7, 
                   jaguars_game_2022_8, jaguars_game_2022_9, jaguars_game_2022_10, jaguars_game_2022_11, jaguars_game_2022_12, 
                   jaguars_game_2022_13, jaguars_game_2022_14, jaguars_game_2022_16, jaguars_game_2023_1, jaguars_game_2023_2, 
                   jaguars_game_2023_3, jaguars_game_2023_4, jaguars_game_2023_5, jaguars_game_2023_6, jaguars_game_2023_8, 
                   jaguars_game_2023_9, jaguars_game_2023_10, jaguars_game_2023_11, jaguars_game_2023_13, jaguars_game_2023_14, 
                   jaguars_game_2023_15, jaguars_game_2023_16, jaguars_game_2023_17, titans_game_2022_1, titans_game_2022_3, 
                   titans_game_2022_4, titans_game_2022_5, titans_game_2022_6, titans_game_2022_7, titans_game_2022_8, 
                   titans_game_2022_9, titans_game_2022_11, titans_game_2022_12, titans_game_2022_13, titans_game_2022_14,
                   titans_game_2023_1, titans_game_2023_2, titans_game_2023_3, titans_game_2023_4, titans_game_2023_5, 
                   titans_game_2023_6, titans_game_2023_7, titans_game_2023_9, titans_game_2023_10, titans_game_2023_11, 
                   titans_game_2023_12, titans_game_2023_14, titans_game_2023_15, titans_game_2023_16, titans_game_2023_17)


# function to read in the html pages as xml documents to prepare to extract the game info from
load_game_pages <- function(overall_games) {
  games_data <- list()
  for (i in seq_along(overall_games)){
    page <- str_c(overall_games[i])
    games_data[[i]] <- read_html(overall_games[i])
  }
  return(games_data)
}


all_games <- load_game_pages(overall_games)


assign_games <- function(game_list, teams, years, games_per_team) {
  counter <- 1
  for (team in teams) {
    for (year in years) {
      for (game_num in games_per_team[[team]][[as.character(year)]]) {
        var_name <- paste0(team, "_game_", year, "_", game_num)
        assign(var_name, game_list[[counter]], envir = .GlobalEnv)
        counter <- counter + 1
      }
    }
  }
}

teams <- c("ravens", "bears", "lions", "colts", "jaguars", "titans")
years <- c(2022, 2023)
games_per_team <- list(
  ravens = list(
    "2022" = c(1:7, 10:13, 16:17),
    "2023" = c(1:10, 12:14, 16)
  ),
  bears = list(
    "2022" = c(1:5, 8:14, 16:17),
    "2023" = c(1:4, 6:9, 11, 13:17)
  ),
  lions = list(
    "2022" = c(1:10, 12:14, 16:17),
    "2023" = c(2:3, 5:7, 9:10, 12:13, 15, 17)
  ), 
  colts = list(
    "2022" = c(1:4, 6:11, 13, 16:17),
    "2023" = c(1:13, 15, 17)
  ),
  jaguars = list(
    "2022" = c(1:14, 16),
    "2023" = c(1:6, 8:11, 13:17)
  ),
  titans = list(
    "2022" = c(1, 3:9, 11:14),
    "2023" = c(1:7, 9:12, 14:17)
  )
)


assign_games(all_games, teams, years, games_per_team)




# now that we have the urls as xml documents, I am extracting the game information from them in this function
extract_game_info <- function(games_list) {
  game_info_list <- lapply(games_list, function(game) {
    game %>%
      html_nodes(".GameInfo") %>%
      html_text()
  })
  return(game_info_list)
}

# List all game objects
ravens_games_2022 <- list(
  ravens_game_2022_1, ravens_game_2022_2, ravens_game_2022_3, ravens_game_2022_4, 
  ravens_game_2022_5, ravens_game_2022_6, ravens_game_2022_7, ravens_game_2022_10, 
  ravens_game_2022_11, ravens_game_2022_12, ravens_game_2022_13, ravens_game_2022_16, 
  ravens_game_2022_17
)

ravens_games_2023 <- list(
  ravens_game_2023_1, ravens_game_2023_2, ravens_game_2023_3, ravens_game_2023_4, 
  ravens_game_2023_5, ravens_game_2023_6, ravens_game_2023_7, ravens_game_2023_8,
  ravens_game_2023_9, ravens_game_2023_10, ravens_game_2023_12, ravens_game_2023_13, 
  ravens_game_2023_14, ravens_game_2023_16
)

bears_games_2022 <- list(
  bears_game_2022_1, bears_game_2022_2, bears_game_2022_3, bears_game_2022_4,
  bears_game_2022_5, bears_game_2022_8, bears_game_2022_9, bears_game_2022_10,
  bears_game_2022_11, bears_game_2022_12, bears_game_2022_13, bears_game_2022_14,
  bears_game_2022_16, bears_game_2022_17
)

bears_games_2023 <- list(
  bears_game_2023_1, bears_game_2023_2, bears_game_2023_3, bears_game_2023_4,
  bears_game_2023_6, bears_game_2023_7, bears_game_2023_8, bears_game_2023_9,
  bears_game_2023_11, bears_game_2023_13, bears_game_2023_14, bears_game_2023_15,
  bears_game_2023_16, bears_game_2023_17
)

lions_games_2022 <- list(
  lions_game_2022_1, lions_game_2022_2, lions_game_2022_3, lions_game_2022_4,
  lions_game_2022_5, lions_game_2022_6, lions_game_2022_7, lions_game_2022_8,
  lions_game_2022_9, lions_game_2022_10, lions_game_2022_12, lions_game_2022_13,
  lions_game_2022_14, lions_game_2022_16, lions_game_2022_17
)

lions_games_2023 <- list(
  lions_game_2023_2, lions_game_2023_3, lions_game_2023_5, lions_game_2023_6,
  lions_game_2023_7, lions_game_2023_9, lions_game_2023_10, lions_game_2023_12,
  lions_game_2023_13, lions_game_2023_15, lions_game_2023_17
)

colts_games_2022 <- list(
  colts_game_2022_1, colts_game_2022_2, colts_game_2022_3, colts_game_2022_4,
  colts_game_2022_6, colts_game_2022_7, colts_game_2022_8, colts_game_2022_9,
  colts_game_2022_10, colts_game_2022_11, colts_game_2022_13, colts_game_2022_16,
  colts_game_2022_17
)

colts_games_2023 <- list(
  colts_game_2023_1, colts_game_2023_2, colts_game_2023_3, colts_game_2023_4,
  colts_game_2023_5, colts_game_2023_6, colts_game_2023_7, colts_game_2023_8,
  colts_game_2023_9, colts_game_2023_10, colts_game_2023_11, colts_game_2023_12, 
  colts_game_2023_13, colts_game_2023_15, colts_game_2023_17
)

jaguars_games_2022 <- list(
  jaguars_game_2022_1, jaguars_game_2022_2, jaguars_game_2022_3, jaguars_game_2022_4,
  jaguars_game_2022_5, jaguars_game_2022_6, jaguars_game_2022_7, jaguars_game_2022_8,
  jaguars_game_2022_9, jaguars_game_2022_10, jaguars_game_2022_11, jaguars_game_2022_12,
  jaguars_game_2022_13, jaguars_game_2022_14, jaguars_game_2022_16
)

jaguars_games_2023 <- list(
  jaguars_game_2023_1, jaguars_game_2023_2, jaguars_game_2023_3, jaguars_game_2023_4,
  jaguars_game_2023_5, jaguars_game_2023_6, jaguars_game_2023_8, jaguars_game_2023_9,
  jaguars_game_2023_10, jaguars_game_2023_11, jaguars_game_2023_13, jaguars_game_2023_14, 
  jaguars_game_2023_15, jaguars_game_2023_16, jaguars_game_2023_17
)

titans_games_2022 <- list(
  titans_game_2022_1, titans_game_2022_3, titans_game_2022_4, titans_game_2022_5,
  titans_game_2022_6, titans_game_2022_7, titans_game_2022_8, titans_game_2022_9,
  titans_game_2022_11, titans_game_2022_12, titans_game_2022_13, titans_game_2022_14
)

titans_games_2023 <- list(
  titans_game_2023_1, titans_game_2023_2, titans_game_2023_3, titans_game_2023_4,
  titans_game_2023_5, titans_game_2023_6, titans_game_2023_7, titans_game_2023_9,
  titans_game_2023_10, titans_game_2023_11, titans_game_2023_12, titans_game_2023_14, 
  titans_game_2023_15, titans_game_2023_16, titans_game_2023_17
)


# Extract game info for each list
ravens_games_2022_info <- extract_game_info(ravens_games_2022)
ravens_games_2023_info <- extract_game_info(ravens_games_2023)
bears_games_2022_info <- extract_game_info(bears_games_2022)
bears_games_2023_info <- extract_game_info(bears_games_2023)
lions_games_2022_info <- extract_game_info(lions_games_2022)
lions_games_2023_info <- extract_game_info(lions_games_2023)
colts_games_2022_info <- extract_game_info(colts_games_2022)
colts_games_2023_info <- extract_game_info(colts_games_2023)
jaguars_games_2022_info <- extract_game_info(jaguars_games_2022)
jaguars_games_2023_info <- extract_game_info(jaguars_games_2023)
titans_games_2022_info <- extract_game_info(titans_games_2022)
titans_games_2023_info <- extract_game_info(titans_games_2023)



# now that I have the game info, I'm creating a fucntion to extract the betting lines, over/under, and 
# attendance from each game
data_extract <- function(game_info) {
  betting_lines <- str_extract(game_info, "Line: ([A-Za-z]+ [+-]?[0-9.]+)")
  over_under <- str_extract(game_info, "Over/Under: ([^At]*)")
  attendance <- str_extract(game_info, "Attendance: ([^Re]*)")
  return(list(betting_lines, over_under, attendance))
}

# Define a function to apply data_extract across all games in a list
extract_game_data <- function(game_info_list) {
  lapply(game_info_list, data_extract)
}

# Apply the function to each team's game data
ravens_games_2022_extract <- extract_game_data(ravens_games_2022_info)
ravens_games_2023_extract <- extract_game_data(ravens_games_2023_info)
bears_games_2022_extract <- extract_game_data(bears_games_2022_info)
bears_games_2023_extract <- extract_game_data(bears_games_2023_info)
lions_games_2022_extract <- extract_game_data(lions_games_2022_info)
lions_games_2023_extract <- extract_game_data(lions_games_2023_info)
colts_games_2022_extract <- extract_game_data(colts_games_2022_info)
colts_games_2023_extract <- extract_game_data(colts_games_2023_info)
jaguars_games_2022_extract <- extract_game_data(jaguars_games_2022_info)
jaguars_games_2023_extract <- extract_game_data(jaguars_games_2023_info)
titans_games_2022_extract <- extract_game_data(titans_games_2022_info)
titans_games_2023_extract <- extract_game_data(titans_games_2023_info)



# in overall_data dataset to add it into
betting_lines <- list(ravens_games_2022_extract[[1]][[1]], ravens_games_2022_extract[[2]][[1]], ravens_games_2022_extract[[3]][[1]], 
                      ravens_games_2022_extract[[4]][[1]], ravens_games_2022_extract[[5]][[1]], ravens_games_2022_extract[[6]][[1]], 
                      ravens_games_2022_extract[[7]][[1]], ravens_games_2022_extract[[8]][[1]], ravens_games_2022_extract[[9]][[1]], 
                      ravens_games_2022_extract[[10]][[1]], ravens_games_2022_extract[[11]][[1]], ravens_games_2022_extract[[12]][[1]], 
                      ravens_games_2022_extract[[13]][[1]], ravens_games_2023_extract[[1]][[1]], ravens_games_2023_extract[[2]][[1]], 
                      ravens_games_2023_extract[[3]][[1]], ravens_games_2023_extract[[4]][[1]], ravens_games_2023_extract[[5]][[1]], 
                      ravens_games_2023_extract[[6]][[1]], ravens_games_2023_extract[[7]][[1]], ravens_games_2023_extract[[8]][[1]], 
                      ravens_games_2023_extract[[9]][[1]], ravens_games_2023_extract[[10]][[1]], ravens_games_2023_extract[[11]][[1]], 
                      ravens_games_2023_extract[[12]][[1]], ravens_games_2023_extract[[13]][[1]], ravens_games_2023_extract[[14]][[1]],
                      bears_games_2022_extract[[1]][[1]], bears_games_2022_extract[[2]][[1]], bears_games_2022_extract[[3]][[1]], 
                      bears_games_2022_extract[[4]][[1]], bears_games_2022_extract[[5]][[1]], bears_games_2022_extract[[6]][[1]], 
                      bears_games_2022_extract[[7]][[1]], bears_games_2022_extract[[8]][[1]], bears_games_2022_extract[[9]][[1]], 
                      bears_games_2022_extract[[10]][[1]], bears_games_2022_extract[[11]][[1]], bears_games_2022_extract[[12]][[1]], 
                      bears_games_2022_extract[[13]][[1]], bears_games_2022_extract[[14]][[1]], bears_games_2023_extract[[1]][[1]], 
                      bears_games_2023_extract[[2]][[1]], bears_games_2023_extract[[3]][[1]], bears_games_2023_extract[[4]][[1]], 
                      bears_games_2023_extract[[5]][[1]], bears_games_2023_extract[[6]][[1]], bears_games_2023_extract[[7]][[1]], 
                      bears_games_2023_extract[[8]][[1]], bears_games_2023_extract[[9]][[1]], bears_games_2023_extract[[10]][[1]], 
                      bears_games_2023_extract[[11]][[1]], bears_games_2023_extract[[12]][[1]], bears_games_2023_extract[[13]][[1]], 
                      bears_games_2023_extract[[14]][[1]], lions_games_2022_extract[[1]][[1]], lions_games_2022_extract[[2]][[1]], 
                      lions_games_2022_extract[[3]][[1]], lions_games_2022_extract[[4]][[1]], lions_games_2022_extract[[5]][[1]], 
                      lions_games_2022_extract[[6]][[1]], lions_games_2022_extract[[7]][[1]], lions_games_2022_extract[[8]][[1]], 
                      lions_games_2022_extract[[9]][[1]], lions_games_2022_extract[[10]][[1]], lions_games_2022_extract[[11]][[1]], 
                      lions_games_2022_extract[[12]][[1]], lions_games_2022_extract[[13]][[1]], lions_games_2022_extract[[14]][[1]], 
                      lions_games_2022_extract[[15]][[1]], lions_games_2023_extract[[1]][[1]], lions_games_2023_extract[[2]][[1]], 
                      lions_games_2023_extract[[3]][[1]], lions_games_2023_extract[[4]][[1]], lions_games_2023_extract[[5]][[1]], 
                      lions_games_2023_extract[[6]][[1]], lions_games_2023_extract[[7]][[1]], lions_games_2023_extract[[8]][[1]], 
                      lions_games_2023_extract[[9]][[1]], lions_games_2023_extract[[10]][[1]], lions_games_2023_extract[[11]][[1]],
                      colts_games_2022_extract[[1]][[1]], colts_games_2022_extract[[2]][[1]], colts_games_2022_extract[[3]][[1]], 
                      colts_games_2022_extract[[4]][[1]], colts_games_2022_extract[[5]][[1]], colts_games_2022_extract[[6]][[1]], 
                      colts_games_2022_extract[[7]][[1]], colts_games_2022_extract[[8]][[1]], colts_games_2022_extract[[9]][[1]], 
                      colts_games_2022_extract[[10]][[1]], colts_games_2022_extract[[11]][[1]], colts_games_2022_extract[[12]][[1]], 
                      colts_games_2022_extract[[13]][[1]], colts_games_2023_extract[[1]][[1]], colts_games_2023_extract[[2]][[1]], 
                      colts_games_2023_extract[[3]][[1]], colts_games_2023_extract[[4]][[1]], colts_games_2023_extract[[5]][[1]], 
                      colts_games_2023_extract[[6]][[1]], colts_games_2023_extract[[7]][[1]], colts_games_2023_extract[[8]][[1]], 
                      colts_games_2023_extract[[9]][[1]], colts_games_2023_extract[[10]][[1]], colts_games_2023_extract[[11]][[1]], 
                      colts_games_2023_extract[[12]][[1]], colts_games_2023_extract[[13]][[1]], colts_games_2023_extract[[14]][[1]], 
                      colts_games_2023_extract[[15]][[1]], jaguars_games_2022_extract[[1]][[1]], jaguars_games_2022_extract[[2]][[1]], 
                      jaguars_games_2022_extract[[3]][[1]], jaguars_games_2022_extract[[4]][[1]], jaguars_games_2022_extract[[5]][[1]], 
                      jaguars_games_2022_extract[[6]][[1]], jaguars_games_2022_extract[[7]][[1]], jaguars_games_2022_extract[[8]][[1]], 
                      jaguars_games_2022_extract[[9]][[1]], jaguars_games_2022_extract[[10]][[1]], jaguars_games_2022_extract[[11]][[1]], 
                      jaguars_games_2022_extract[[12]][[1]], jaguars_games_2022_extract[[13]][[1]], jaguars_games_2022_extract[[14]][[1]],
                      jaguars_games_2022_extract[[15]][[1]], jaguars_games_2023_extract[[1]][[1]], jaguars_games_2023_extract[[2]][[1]], 
                      jaguars_games_2023_extract[[3]][[1]], jaguars_games_2023_extract[[4]][[1]], jaguars_games_2023_extract[[5]][[1]], 
                      jaguars_games_2023_extract[[6]][[1]], jaguars_games_2023_extract[[7]][[1]], jaguars_games_2023_extract[[8]][[1]], 
                      jaguars_games_2023_extract[[9]][[1]], jaguars_games_2023_extract[[10]][[1]], jaguars_games_2023_extract[[11]][[1]], 
                      jaguars_games_2023_extract[[12]][[1]], jaguars_games_2023_extract[[13]][[1]], jaguars_games_2023_extract[[14]][[1]], 
                      jaguars_games_2023_extract[[15]][[1]], titans_games_2022_extract[[1]][[1]], titans_games_2022_extract[[2]][[1]], 
                      titans_games_2022_extract[[3]][[1]], titans_games_2022_extract[[4]][[1]], titans_games_2022_extract[[5]][[1]], 
                      titans_games_2022_extract[[6]][[1]], titans_games_2022_extract[[7]][[1]], titans_games_2022_extract[[8]][[1]], 
                      titans_games_2022_extract[[9]][[1]], titans_games_2022_extract[[10]][[1]], titans_games_2022_extract[[11]][[1]], 
                      titans_games_2022_extract[[12]][[1]], titans_games_2023_extract[[1]][[1]], titans_games_2023_extract[[2]][[1]], 
                      titans_games_2023_extract[[3]][[1]], titans_games_2023_extract[[4]][[1]], titans_games_2023_extract[[5]][[1]], 
                      titans_games_2023_extract[[6]][[1]], titans_games_2023_extract[[7]][[1]], titans_games_2023_extract[[8]][[1]], 
                      titans_games_2023_extract[[9]][[1]], titans_games_2023_extract[[10]][[1]], titans_games_2023_extract[[11]][[1]], 
                      titans_games_2023_extract[[12]][[1]], titans_games_2023_extract[[13]][[1]], titans_games_2023_extract[[14]][[1]], 
                      titans_games_2023_extract[[15]][[1]])
betting_lines


# this function cleans the betting lines for each game. First, it removes the Line: from the betting_lines
# that I extracted and then checks to see if the team name is in it. If the team name is in it (for example BAL), 
# then it removes the team name, leaving just the score (ex. -5.5). If the team name is not in it, 
# it removes all text and changes the - to a +, signifying that the team were underdogs for this game
clean_betting_lines <- function(betting_lines) {
  # Define team abbreviations
  phrases <- c("BAL", "CHI", "DET", "IND", "JAX", "TEN")
  
  # Remove 'Line:' prefix
  betting_lines <- gsub("Line: ", "", betting_lines)
  
  # Check if the line contains any team abbreviation
  team_match <- sapply(betting_lines, function(line) any(grepl(paste(phrases, collapse = "|"), line)))
  
  # For team matches, remove the team abbreviation
  betting_lines[team_match] <- gsub(paste(phrases, collapse = "|"), "", betting_lines[team_match])
  
  # For non-matches, remove letters and switch '-' to '+'
  betting_lines[!team_match] <- gsub("[A-Za-z]", "", betting_lines[!team_match])
  betting_lines[!team_match] <- gsub("-", "+", betting_lines[!team_match])
  
  return(betting_lines)
}


spread <- clean_betting_lines(betting_lines)


# adding the spread list above into overall_data
overall_data$spread <- spread


# creating a list of all of the Result_Scores column into a list. Will be using this to derive if the team covered or not
cover <- list(overall_data$Result_Score[1:166])


# creating a function called clean_cover_verdict to change the W by 13 or L by 7 in the list above to 
# something like +11 or -5. Doing this to make it easier to compare to the spread to see if the team
# covered or not
clean_cover_verdict <- function(cover) {
  phrase1 = "W"
  phrase2 = "L"
  cover_c <- vector("character", length(cover))
  for (i in seq_along(cover)) {
    cover_subset <- gsub(" by", "", cover[i]) # Fix gsub to work on individual elements
    if(grepl(phrase1, cover[i])) {
      change_symbol <- gsub("W ", "-", cover_subset)
    } else {
      change_symbol <- gsub("L ", "+", cover_subset)
    }
    cover_c[i] <- change_symbol
  }
  return(cover_c)
}

# Assuming 'cover' is a vector containing all the cover results
score_results <- lapply(cover, clean_cover_verdict)




cover_vs_spread <- function(spread, score_results) {
  # Ensure both inputs are numeric vectors
  spread_num <- as.numeric(spread)
  score_num <- as.numeric(score_results)
  
  # Check if lengths match
  if (length(spread_num) != length(score_num)) {
    stop("Length of spread and score_results must match!")
  }
  
  # Initialize results vector
  results <- vector("character", length(spread_num))
  
  # Iterate and compare spreads and score results
  for (i in seq_along(spread_num)) {
    if (is.na(spread_num[i]) || is.na(score_num[i])) {
      results[i] <- "Invalid data"
    } else if ((spread_num[i] < 0 && score_num[i] <= spread_num[i]) || 
               (spread_num[i] > 0 && score_num[i] >= spread_num[i])) {
      results[i] <- "Covered"
    } else {
      results[i] <- "Did not cover"
    }
  }
  
  return(results)
}

# unlisting both spread and score_results
spread <- as.numeric(unlist(spread))

score_results <- as.numeric(unlist(score_results))

# Automating the process of applying the function to all pairs of spread and score_results
cover_results <- lapply(seq_along(spread), function(i) {
  cover_vs_spread(spread[i], score_results[i])
})

# creating a column in overall_data called covered that checks the list above. If the game outputs covered
# then the colum displays yes. If the game doesn't output covered, then the column displays no
overall_data$covered <- ifelse(cover_results == "Covered", 1, 0)


# creating a list called upset that contains if there was an upset win involved in the game. We will be 
# using this to help create a column in overall_data to see if this happened or not
upset <- list(overall_data$Result_Score[1:166])

# below is a function to detect if there was an upset win. By doing this, it checks if there contains a W 
# in the upset list we created above, example (W by 3). If that is the case, and the spread on the game 
# had the Ravens as underdogs, meaning the spread was + a number, than it counts that as an upset win for
# the Ravens

upset_win_detector <- function(upset, spread) {
  
  upset_results <- vector("character", length(upset))
  for (i in seq_along (upset)) {
    if(grepl("W", upset) && grepl("\\+", spread)) {
      upset_results[i] <- 1
    } else {
      upset_results[i] <- 0
    }
  }
  return(upset_results)
}


# takes the function above and creates a lists for each game outputting whether the ravens had an upset win
# in their game or not
upset_win <- list(upset_win_detector(upset[1], spread[1]), upset_win_detector(upset[2], spread[2]), 
                  upset_win_detector(upset[3], spread[3]), upset_win_detector(upset[4], spread[4]), 
                  upset_win_detector(upset[5], spread[5]), upset_win_detector(upset[6], spread[6]), 
                  upset_win_detector(upset[7], spread[7]), upset_win_detector(upset[8], spread[8]), 
                  upset_win_detector(upset[9], spread[9]), upset_win_detector(upset[10], spread[10]), 
                  upset_win_detector(upset[11], spread[11]), upset_win_detector(upset[12], spread[12]), 
                  upset_win_detector(upset[13], spread[13]), upset_win_detector(upset[14], spread[14]),
                  upset_win_detector(upset[15], spread[15]), upset_win_detector(upset[16], spread[16]), 
                  upset_win_detector(upset[17], spread[17]), upset_win_detector(upset[18], spread[18]), 
                  upset_win_detector(upset[19], spread[19]), upset_win_detector(upset[20], spread[20]), 
                  upset_win_detector(upset[21], spread[21]), upset_win_detector(upset[22], spread[22]), 
                  upset_win_detector(upset[23], spread[23]), upset_win_detector(upset[24], spread[24]), 
                  upset_win_detector(upset[25], spread[25]), upset_win_detector(upset[26], spread[26]), 
                  upset_win_detector(upset[27], spread[27]), upset_win_detector(upset[28], spread[28]), 
                  upset_win_detector(upset[29], spread[29]), upset_win_detector(upset[30], spread[30]), 
                  upset_win_detector(upset[31], spread[31]), upset_win_detector(upset[32], spread[32]), 
                  upset_win_detector(upset[33], spread[33]), upset_win_detector(upset[34], spread[34]), 
                  upset_win_detector(upset[35], spread[35]), upset_win_detector(upset[36], spread[36]), 
                  upset_win_detector(upset[37], spread[37]), upset_win_detector(upset[38], spread[38]), 
                  upset_win_detector(upset[39], spread[39]), upset_win_detector(upset[40], spread[40]),
                  upset_win_detector(upset[41], spread[41]), upset_win_detector(upset[42], spread[42]), 
                  upset_win_detector(upset[43], spread[43]), upset_win_detector(upset[44], spread[44]), 
                  upset_win_detector(upset[45], spread[45]), upset_win_detector(upset[46], spread[46]), 
                  upset_win_detector(upset[47], spread[47]), upset_win_detector(upset[48], spread[48]), 
                  upset_win_detector(upset[49], spread[49]), upset_win_detector(upset[50], spread[50]), 
                  upset_win_detector(upset[51], spread[51]), upset_win_detector(upset[52], spread[52]), 
                  upset_win_detector(upset[53], spread[53]), upset_win_detector(upset[54], spread[54]), 
                  upset_win_detector(upset[55], spread[55]), upset_win_detector(upset[56], spread[56]), 
                  upset_win_detector(upset[57], spread[57]), upset_win_detector(upset[58], spread[58]), 
                  upset_win_detector(upset[59], spread[59]), upset_win_detector(upset[60], spread[60]), 
                  upset_win_detector(upset[61], spread[61]), upset_win_detector(upset[62], spread[62]), 
                  upset_win_detector(upset[63], spread[63]), upset_win_detector(upset[64], spread[64]), 
                  upset_win_detector(upset[65], spread[65]), upset_win_detector(upset[66], spread[66]),
                  upset_win_detector(upset[67], spread[67]), upset_win_detector(upset[68], spread[68]), 
                  upset_win_detector(upset[69], spread[69]), upset_win_detector(upset[70], spread[70]), 
                  upset_win_detector(upset[71], spread[71]), upset_win_detector(upset[72], spread[72]), 
                  upset_win_detector(upset[73], spread[73]), upset_win_detector(upset[74], spread[74]), 
                  upset_win_detector(upset[75], spread[75]), upset_win_detector(upset[76], spread[76]), 
                  upset_win_detector(upset[77], spread[77]), upset_win_detector(upset[78], spread[78]), 
                  upset_win_detector(upset[79], spread[79]), upset_win_detector(upset[80], spread[80]), 
                  upset_win_detector(upset[81], spread[81]), upset_win_detector(upset[82], spread[82]), 
                  upset_win_detector(upset[83], spread[83]), upset_win_detector(upset[84], spread[84]), 
                  upset_win_detector(upset[85], spread[85]), upset_win_detector(upset[86], spread[86]), 
                  upset_win_detector(upset[87], spread[87]), upset_win_detector(upset[88], spread[88]), 
                  upset_win_detector(upset[89], spread[89]), upset_win_detector(upset[90], spread[90]), 
                  upset_win_detector(upset[91], spread[91]), upset_win_detector(upset[92], spread[92]),
                  upset_win_detector(upset[93], spread[93]), upset_win_detector(upset[94], spread[94]), 
                  upset_win_detector(upset[95], spread[95]), upset_win_detector(upset[96], spread[96]), 
                  upset_win_detector(upset[97], spread[97]), upset_win_detector(upset[98], spread[98]), 
                  upset_win_detector(upset[99], spread[99]), upset_win_detector(upset[100], spread[100]), 
                  upset_win_detector(upset[101], spread[101]), upset_win_detector(upset[102], spread[102]), 
                  upset_win_detector(upset[103], spread[103]), upset_win_detector(upset[104], spread[104]), 
                  upset_win_detector(upset[105], spread[105]), upset_win_detector(upset[106], spread[106]), 
                  upset_win_detector(upset[107], spread[107]), upset_win_detector(upset[108], spread[108]), 
                  upset_win_detector(upset[109], spread[109]), upset_win_detector(upset[110], spread[110]), 
                  upset_win_detector(upset[111], spread[111]), upset_win_detector(upset[112], spread[112]), 
                  upset_win_detector(upset[113], spread[113]), upset_win_detector(upset[114], spread[114]), 
                  upset_win_detector(upset[115], spread[115]), upset_win_detector(upset[116], spread[116]), 
                  upset_win_detector(upset[117], spread[117]), upset_win_detector(upset[118], spread[118]),
                  upset_win_detector(upset[119], spread[119]), upset_win_detector(upset[120], spread[120]), 
                  upset_win_detector(upset[121], spread[121]), upset_win_detector(upset[122], spread[122]), 
                  upset_win_detector(upset[123], spread[123]), upset_win_detector(upset[124], spread[124]), 
                  upset_win_detector(upset[125], spread[125]), upset_win_detector(upset[126], spread[126]), 
                  upset_win_detector(upset[127], spread[127]), upset_win_detector(upset[128], spread[128]), 
                  upset_win_detector(upset[129], spread[129]), upset_win_detector(upset[130], spread[130]), 
                  upset_win_detector(upset[131], spread[131]), upset_win_detector(upset[132], spread[132]), 
                  upset_win_detector(upset[133], spread[133]), upset_win_detector(upset[134], spread[134]), 
                  upset_win_detector(upset[135], spread[135]), upset_win_detector(upset[136], spread[136]), 
                  upset_win_detector(upset[137], spread[137]), upset_win_detector(upset[138], spread[138]), 
                  upset_win_detector(upset[139], spread[139]), upset_win_detector(upset[140], spread[140]), 
                  upset_win_detector(upset[141], spread[141]), upset_win_detector(upset[142], spread[142]), 
                  upset_win_detector(upset[143], spread[143]), upset_win_detector(upset[144], spread[144]),
                  upset_win_detector(upset[145], spread[145]), upset_win_detector(upset[146], spread[146]), 
                  upset_win_detector(upset[147], spread[147]), upset_win_detector(upset[148], spread[148]), 
                  upset_win_detector(upset[149], spread[149]), upset_win_detector(upset[150], spread[150]), 
                  upset_win_detector(upset[151], spread[151]), upset_win_detector(upset[152], spread[152]), 
                  upset_win_detector(upset[153], spread[153]), upset_win_detector(upset[154], spread[154]), 
                  upset_win_detector(upset[155], spread[155]), upset_win_detector(upset[156], spread[156]), 
                  upset_win_detector(upset[157], spread[157]), upset_win_detector(upset[158], spread[158]), 
                  upset_win_detector(upset[159], spread[159]), upset_win_detector(upset[160], spread[160]), 
                  upset_win_detector(upset[161], spread[161]), upset_win_detector(upset[162], spread[162]), 
                  upset_win_detector(upset[163], spread[163]), upset_win_detector(upset[164], spread[164]), 
                  upset_win_detector(upset[165], spread[165]), upset_win_detector(upset[166], spread[166]))


# creating a column in overall_data called upset_win that checks the list above. If the game had an upset win
# then the column displays yes. If the game didn't have an upset win, then the column displays no
overall_data$upset_win <- upset_win


# below is the same function we used to detect if there was an upset win, only now it's for an upset loss. 
# So now, instead of checking for a W in upset, it checks for an L, and instead of checking to see if the
# Ravens were underdogs (+), it's checking to see if they were favorites
upset_loss_detector <- function(upset, spread) {
  
  upset_results <- vector("character", length(upset))
  for (i in seq_along (upset)) {
    if(grepl("L", upset) && grepl("\\-", spread)) {
      upset_results[i] <- 1
    } else {
      upset_results[i] <- 0
    }
  }
  return(upset_results)
}



# takes the function above and creates a lists for each game outputting whether the ravens had an upset loss
# in their game or not
upset_loss <- list(upset_loss_detector(upset[1], spread[1]), upset_loss_detector(upset[2], spread[2]), 
                  upset_loss_detector(upset[3], spread[3]), upset_loss_detector(upset[4], spread[4]), 
                  upset_loss_detector(upset[5], spread[5]), upset_loss_detector(upset[6], spread[6]), 
                  upset_loss_detector(upset[7], spread[7]), upset_loss_detector(upset[8], spread[8]), 
                  upset_loss_detector(upset[9], spread[9]), upset_loss_detector(upset[10], spread[10]), 
                  upset_loss_detector(upset[11], spread[11]), upset_loss_detector(upset[12], spread[12]), 
                  upset_loss_detector(upset[13], spread[13]), upset_loss_detector(upset[14], spread[14]),
                  upset_loss_detector(upset[15], spread[15]), upset_loss_detector(upset[16], spread[16]), 
                  upset_loss_detector(upset[17], spread[17]), upset_loss_detector(upset[18], spread[18]), 
                  upset_loss_detector(upset[19], spread[19]), upset_loss_detector(upset[20], spread[20]), 
                  upset_loss_detector(upset[21], spread[21]), upset_loss_detector(upset[22], spread[22]), 
                  upset_loss_detector(upset[23], spread[23]), upset_loss_detector(upset[24], spread[24]), 
                  upset_loss_detector(upset[25], spread[25]), upset_loss_detector(upset[26], spread[26]), 
                  upset_loss_detector(upset[27], spread[27]), upset_loss_detector(upset[28], spread[28]), 
                  upset_loss_detector(upset[29], spread[29]), upset_loss_detector(upset[30], spread[30]), 
                  upset_loss_detector(upset[31], spread[31]), upset_loss_detector(upset[32], spread[32]), 
                  upset_loss_detector(upset[33], spread[33]), upset_loss_detector(upset[34], spread[34]), 
                  upset_loss_detector(upset[35], spread[35]), upset_loss_detector(upset[36], spread[36]), 
                  upset_loss_detector(upset[37], spread[37]), upset_loss_detector(upset[38], spread[38]), 
                  upset_loss_detector(upset[39], spread[39]), upset_loss_detector(upset[40], spread[40]),
                  upset_loss_detector(upset[41], spread[41]), upset_loss_detector(upset[42], spread[42]), 
                  upset_loss_detector(upset[43], spread[43]), upset_loss_detector(upset[44], spread[44]), 
                  upset_loss_detector(upset[45], spread[45]), upset_loss_detector(upset[46], spread[46]), 
                  upset_loss_detector(upset[47], spread[47]), upset_loss_detector(upset[48], spread[48]), 
                  upset_loss_detector(upset[49], spread[49]), upset_loss_detector(upset[50], spread[50]), 
                  upset_loss_detector(upset[51], spread[51]), upset_loss_detector(upset[52], spread[52]), 
                  upset_loss_detector(upset[53], spread[53]), upset_loss_detector(upset[54], spread[54]), 
                  upset_loss_detector(upset[55], spread[55]), upset_loss_detector(upset[56], spread[56]), 
                  upset_loss_detector(upset[57], spread[57]), upset_loss_detector(upset[58], spread[58]), 
                  upset_loss_detector(upset[59], spread[59]), upset_loss_detector(upset[60], spread[60]), 
                  upset_loss_detector(upset[61], spread[61]), upset_loss_detector(upset[62], spread[62]), 
                  upset_loss_detector(upset[63], spread[63]), upset_loss_detector(upset[64], spread[64]), 
                  upset_loss_detector(upset[65], spread[65]), upset_loss_detector(upset[66], spread[66]),
                  upset_loss_detector(upset[67], spread[67]), upset_loss_detector(upset[68], spread[68]), 
                  upset_loss_detector(upset[69], spread[69]), upset_loss_detector(upset[70], spread[70]), 
                  upset_loss_detector(upset[71], spread[71]), upset_loss_detector(upset[72], spread[72]), 
                  upset_loss_detector(upset[73], spread[73]), upset_loss_detector(upset[74], spread[74]), 
                  upset_loss_detector(upset[75], spread[75]), upset_loss_detector(upset[76], spread[76]), 
                  upset_loss_detector(upset[77], spread[77]), upset_loss_detector(upset[78], spread[78]), 
                  upset_loss_detector(upset[79], spread[79]), upset_loss_detector(upset[80], spread[80]), 
                  upset_loss_detector(upset[81], spread[81]), upset_loss_detector(upset[82], spread[82]), 
                  upset_loss_detector(upset[83], spread[83]), upset_loss_detector(upset[84], spread[84]), 
                  upset_loss_detector(upset[85], spread[85]), upset_loss_detector(upset[86], spread[86]), 
                  upset_loss_detector(upset[87], spread[87]), upset_loss_detector(upset[88], spread[88]), 
                  upset_loss_detector(upset[89], spread[89]), upset_loss_detector(upset[90], spread[90]), 
                  upset_loss_detector(upset[91], spread[91]), upset_loss_detector(upset[92], spread[92]),
                  upset_loss_detector(upset[93], spread[93]), upset_loss_detector(upset[94], spread[94]), 
                  upset_loss_detector(upset[95], spread[95]), upset_loss_detector(upset[96], spread[96]), 
                  upset_loss_detector(upset[97], spread[97]), upset_loss_detector(upset[98], spread[98]), 
                  upset_loss_detector(upset[99], spread[99]), upset_loss_detector(upset[100], spread[100]), 
                  upset_loss_detector(upset[101], spread[101]), upset_loss_detector(upset[102], spread[102]), 
                  upset_loss_detector(upset[103], spread[103]), upset_loss_detector(upset[104], spread[104]), 
                  upset_loss_detector(upset[105], spread[105]), upset_loss_detector(upset[106], spread[106]), 
                  upset_loss_detector(upset[107], spread[107]), upset_loss_detector(upset[108], spread[108]), 
                  upset_loss_detector(upset[109], spread[109]), upset_loss_detector(upset[110], spread[110]), 
                  upset_loss_detector(upset[111], spread[111]), upset_loss_detector(upset[112], spread[112]), 
                  upset_loss_detector(upset[113], spread[113]), upset_loss_detector(upset[114], spread[114]), 
                  upset_loss_detector(upset[115], spread[115]), upset_loss_detector(upset[116], spread[116]), 
                  upset_loss_detector(upset[117], spread[117]), upset_loss_detector(upset[118], spread[118]),
                  upset_loss_detector(upset[119], spread[119]), upset_loss_detector(upset[120], spread[120]), 
                  upset_loss_detector(upset[121], spread[121]), upset_loss_detector(upset[122], spread[122]), 
                  upset_loss_detector(upset[123], spread[123]), upset_loss_detector(upset[124], spread[124]), 
                  upset_loss_detector(upset[125], spread[125]), upset_loss_detector(upset[126], spread[126]), 
                  upset_loss_detector(upset[127], spread[127]), upset_loss_detector(upset[128], spread[128]), 
                  upset_loss_detector(upset[129], spread[129]), upset_loss_detector(upset[130], spread[130]), 
                  upset_loss_detector(upset[131], spread[131]), upset_loss_detector(upset[132], spread[132]), 
                  upset_loss_detector(upset[133], spread[133]), upset_loss_detector(upset[134], spread[134]), 
                  upset_loss_detector(upset[135], spread[135]), upset_loss_detector(upset[136], spread[136]), 
                  upset_loss_detector(upset[137], spread[137]), upset_loss_detector(upset[138], spread[138]), 
                  upset_loss_detector(upset[139], spread[139]), upset_loss_detector(upset[140], spread[140]), 
                  upset_loss_detector(upset[141], spread[141]), upset_loss_detector(upset[142], spread[142]), 
                  upset_loss_detector(upset[143], spread[143]), upset_loss_detector(upset[144], spread[144]),
                  upset_loss_detector(upset[145], spread[145]), upset_loss_detector(upset[146], spread[146]), 
                  upset_loss_detector(upset[147], spread[147]), upset_loss_detector(upset[148], spread[148]), 
                  upset_loss_detector(upset[149], spread[149]), upset_loss_detector(upset[150], spread[150]), 
                  upset_loss_detector(upset[151], spread[151]), upset_loss_detector(upset[152], spread[152]), 
                  upset_loss_detector(upset[153], spread[153]), upset_loss_detector(upset[154], spread[154]), 
                  upset_loss_detector(upset[155], spread[155]), upset_loss_detector(upset[156], spread[156]), 
                  upset_loss_detector(upset[157], spread[157]), upset_loss_detector(upset[158], spread[158]), 
                  upset_loss_detector(upset[159], spread[159]), upset_loss_detector(upset[160], spread[160]), 
                  upset_loss_detector(upset[161], spread[161]), upset_loss_detector(upset[162], spread[162]), 
                  upset_loss_detector(upset[163], spread[163]), upset_loss_detector(upset[164], spread[164]), 
                  upset_loss_detector(upset[165], spread[165]), upset_loss_detector(upset[166], spread[166]))


# creating a column in overall_data called upset_win that checks the list above. If the game had an upset win
# then the column displays yes. If the game didn't have an upset win, then the column displays no
overall_data$upset_loss <- upset_loss



# creating a list of all the attendances from each game. Doing this to then clean it and create a column 
# in overall_data dataset to add it into
attendance <- list(ravens_games_2022_extract[[1]][[3]], ravens_games_2022_extract[[2]][[3]], ravens_games_2022_extract[[3]][[3]], 
                   ravens_games_2022_extract[[4]][[3]], ravens_games_2022_extract[[5]][[3]], ravens_games_2022_extract[[6]][[3]], 
                   ravens_games_2022_extract[[7]][[3]], ravens_games_2022_extract[[8]][[3]], ravens_games_2022_extract[[9]][[3]], 
                   ravens_games_2022_extract[[10]][[3]], ravens_games_2022_extract[[11]][[3]], ravens_games_2022_extract[[12]][[3]], 
                   ravens_games_2022_extract[[13]][[3]], ravens_games_2023_extract[[1]][[3]], ravens_games_2023_extract[[2]][[3]], 
                   ravens_games_2023_extract[[3]][[3]], ravens_games_2023_extract[[4]][[3]], ravens_games_2023_extract[[5]][[3]], 
                   ravens_games_2023_extract[[6]][[3]], ravens_games_2023_extract[[7]][[3]], ravens_games_2023_extract[[8]][[3]], 
                   ravens_games_2023_extract[[9]][[3]], ravens_games_2023_extract[[10]][[3]], ravens_games_2023_extract[[11]][[3]], 
                   ravens_games_2023_extract[[12]][[3]], ravens_games_2023_extract[[13]][[3]], ravens_games_2023_extract[[14]][[3]],
                   bears_games_2022_extract[[1]][[3]], bears_games_2022_extract[[2]][[3]], bears_games_2022_extract[[3]][[3]], 
                   bears_games_2022_extract[[4]][[3]], bears_games_2022_extract[[5]][[3]], bears_games_2022_extract[[6]][[3]], 
                   bears_games_2022_extract[[7]][[3]], bears_games_2022_extract[[8]][[3]], bears_games_2022_extract[[9]][[3]], 
                   bears_games_2022_extract[[10]][[3]], bears_games_2022_extract[[11]][[3]], bears_games_2022_extract[[12]][[3]], 
                   bears_games_2022_extract[[13]][[3]], bears_games_2022_extract[[14]][[3]], bears_games_2023_extract[[1]][[3]], 
                   bears_games_2023_extract[[2]][[3]], bears_games_2023_extract[[3]][[3]], bears_games_2023_extract[[4]][[3]], 
                   bears_games_2023_extract[[5]][[3]], bears_games_2023_extract[[6]][[3]], bears_games_2023_extract[[7]][[3]], 
                   bears_games_2023_extract[[8]][[3]], bears_games_2023_extract[[9]][[3]], bears_games_2023_extract[[10]][[3]], 
                   bears_games_2023_extract[[11]][[3]], bears_games_2023_extract[[12]][[3]], bears_games_2023_extract[[13]][[3]], 
                   bears_games_2023_extract[[14]][[3]], lions_games_2022_extract[[1]][[3]], lions_games_2022_extract[[2]][[3]], 
                   lions_games_2022_extract[[3]][[3]], lions_games_2022_extract[[4]][[3]], lions_games_2022_extract[[5]][[3]], 
                   lions_games_2022_extract[[6]][[3]], lions_games_2022_extract[[7]][[3]], lions_games_2022_extract[[8]][[3]], 
                   lions_games_2022_extract[[9]][[3]], lions_games_2022_extract[[10]][[3]], lions_games_2022_extract[[11]][[3]], 
                   lions_games_2022_extract[[12]][[3]], lions_games_2022_extract[[13]][[3]], lions_games_2022_extract[[14]][[3]], 
                   lions_games_2022_extract[[15]][[3]], lions_games_2023_extract[[1]][[3]], lions_games_2023_extract[[2]][[3]], 
                   lions_games_2023_extract[[3]][[3]], lions_games_2023_extract[[4]][[3]], lions_games_2023_extract[[5]][[3]], 
                   lions_games_2023_extract[[6]][[3]], lions_games_2023_extract[[7]][[3]], lions_games_2023_extract[[8]][[3]], 
                   lions_games_2023_extract[[9]][[3]], lions_games_2023_extract[[10]][[3]], lions_games_2023_extract[[11]][[3]],
                   colts_games_2022_extract[[1]][[3]], colts_games_2022_extract[[2]][[3]], colts_games_2022_extract[[3]][[3]], 
                   colts_games_2022_extract[[4]][[3]], colts_games_2022_extract[[5]][[3]], colts_games_2022_extract[[6]][[3]], 
                   colts_games_2022_extract[[7]][[3]], colts_games_2022_extract[[8]][[3]], colts_games_2022_extract[[9]][[3]], 
                   colts_games_2022_extract[[10]][[3]], colts_games_2022_extract[[11]][[3]], colts_games_2022_extract[[12]][[3]], 
                   colts_games_2022_extract[[13]][[3]], colts_games_2023_extract[[1]][[3]], colts_games_2023_extract[[2]][[3]], 
                   colts_games_2023_extract[[3]][[3]], colts_games_2023_extract[[4]][[3]], colts_games_2023_extract[[5]][[3]], 
                   colts_games_2023_extract[[6]][[3]], colts_games_2023_extract[[7]][[3]], colts_games_2023_extract[[8]][[3]], 
                   colts_games_2023_extract[[9]][[3]], colts_games_2023_extract[[10]][[3]], colts_games_2023_extract[[11]][[3]], 
                   colts_games_2023_extract[[12]][[3]], colts_games_2023_extract[[13]][[3]], colts_games_2023_extract[[14]][[3]], 
                   colts_games_2023_extract[[15]][[3]], jaguars_games_2022_extract[[1]][[3]], jaguars_games_2022_extract[[2]][[3]], 
                   jaguars_games_2022_extract[[3]][[3]], jaguars_games_2022_extract[[4]][[3]], jaguars_games_2022_extract[[5]][[3]], 
                   jaguars_games_2022_extract[[6]][[3]], jaguars_games_2022_extract[[7]][[3]], jaguars_games_2022_extract[[8]][[3]], 
                   jaguars_games_2022_extract[[9]][[3]], jaguars_games_2022_extract[[10]][[3]], jaguars_games_2022_extract[[11]][[3]], 
                   jaguars_games_2022_extract[[12]][[3]], jaguars_games_2022_extract[[13]][[3]], jaguars_games_2022_extract[[14]][[3]],
                   jaguars_games_2022_extract[[15]][[3]], jaguars_games_2023_extract[[1]][[3]], jaguars_games_2023_extract[[2]][[3]], 
                   jaguars_games_2023_extract[[3]][[3]], jaguars_games_2023_extract[[4]][[3]], jaguars_games_2023_extract[[5]][[3]], 
                   jaguars_games_2023_extract[[6]][[3]], jaguars_games_2023_extract[[7]][[3]], jaguars_games_2023_extract[[8]][[3]], 
                   jaguars_games_2023_extract[[9]][[3]], jaguars_games_2023_extract[[10]][[3]], jaguars_games_2023_extract[[11]][[3]], 
                   jaguars_games_2023_extract[[12]][[3]], jaguars_games_2023_extract[[13]][[3]], jaguars_games_2023_extract[[14]][[3]], 
                   jaguars_games_2023_extract[[15]][[3]], titans_games_2022_extract[[1]][[3]], titans_games_2022_extract[[2]][[3]], 
                   titans_games_2022_extract[[3]][[3]], titans_games_2022_extract[[4]][[3]], titans_games_2022_extract[[5]][[3]], 
                   titans_games_2022_extract[[6]][[3]], titans_games_2022_extract[[7]][[3]], titans_games_2022_extract[[8]][[3]], 
                   titans_games_2022_extract[[9]][[3]], titans_games_2022_extract[[10]][[3]], titans_games_2022_extract[[11]][[3]], 
                   titans_games_2022_extract[[12]][[3]], titans_games_2023_extract[[1]][[3]], titans_games_2023_extract[[2]][[3]], 
                   titans_games_2023_extract[[3]][[3]], titans_games_2023_extract[[4]][[3]], titans_games_2023_extract[[5]][[3]], 
                   titans_games_2023_extract[[6]][[3]], titans_games_2023_extract[[7]][[3]], titans_games_2023_extract[[8]][[3]], 
                   titans_games_2023_extract[[9]][[3]], titans_games_2023_extract[[10]][[3]], titans_games_2023_extract[[11]][[3]], 
                   titans_games_2023_extract[[12]][[3]], titans_games_2023_extract[[13]][[3]], titans_games_2023_extract[[14]][[3]], 
                   titans_games_2023_extract[[15]][[3]])
attendance


# below is a function to clean the attendance from the list above. What this function is doing is removing
# Attendance: so that it just displays the number
clean_attendance <- function(attendance) {
  attendance_c <- vector("character", length(attendance))
  for (i in seq_along(attendance)) {
    cleaned_lines <- gsub("Attendance: ", "", attendance)
  }
  return(cleaned_lines)
}


# below is a list of all the cleaned attendances from the function above. Will be putting this into a column
# in the overall_data dataset
fan_totals <- lapply(attendance[1:166], clean_attendance)

# creating a column in overall_data called attendance that lists the attendance for every game in the season
overall_data$attendance <- fan_totals


overall_data$attendance <- as.numeric(gsub(",", "", overall_data$attendance))

overall_data$attendance <- na_interpolation(overall_data$attendance)

# creating a list of all the over/unders from each game. Doing this to then clean it and create a column 
# in overall_data dataset to add it into
over_under <- list(ravens_games_2022_extract[[1]][[2]], ravens_games_2022_extract[[2]][[2]], ravens_games_2022_extract[[3]][[2]], 
                   ravens_games_2022_extract[[4]][[2]], ravens_games_2022_extract[[5]][[2]], ravens_games_2022_extract[[6]][[2]], 
                   ravens_games_2022_extract[[7]][[2]], ravens_games_2022_extract[[8]][[2]], ravens_games_2022_extract[[9]][[2]], 
                   ravens_games_2022_extract[[10]][[2]], ravens_games_2022_extract[[11]][[2]], ravens_games_2022_extract[[12]][[2]], 
                   ravens_games_2022_extract[[13]][[2]], ravens_games_2023_extract[[1]][[2]], ravens_games_2023_extract[[2]][[2]], 
                   ravens_games_2023_extract[[3]][[2]], ravens_games_2023_extract[[4]][[2]], ravens_games_2023_extract[[5]][[2]], 
                   ravens_games_2023_extract[[6]][[2]], ravens_games_2023_extract[[7]][[2]], ravens_games_2023_extract[[8]][[2]], 
                   ravens_games_2023_extract[[9]][[2]], ravens_games_2023_extract[[10]][[2]], ravens_games_2023_extract[[11]][[2]], 
                   ravens_games_2023_extract[[12]][[2]], ravens_games_2023_extract[[13]][[2]], ravens_games_2023_extract[[14]][[2]],
                   bears_games_2022_extract[[1]][[2]], bears_games_2022_extract[[2]][[2]], bears_games_2022_extract[[3]][[2]], 
                   bears_games_2022_extract[[4]][[2]], bears_games_2022_extract[[5]][[2]], bears_games_2022_extract[[6]][[2]], 
                   bears_games_2022_extract[[7]][[2]], bears_games_2022_extract[[8]][[2]], bears_games_2022_extract[[9]][[2]], 
                   bears_games_2022_extract[[10]][[2]], bears_games_2022_extract[[11]][[2]], bears_games_2022_extract[[12]][[2]], 
                   bears_games_2022_extract[[13]][[2]], bears_games_2022_extract[[14]][[2]], bears_games_2023_extract[[1]][[2]], 
                   bears_games_2023_extract[[2]][[2]], bears_games_2023_extract[[3]][[2]], bears_games_2023_extract[[4]][[2]], 
                   bears_games_2023_extract[[5]][[2]], bears_games_2023_extract[[6]][[2]], bears_games_2023_extract[[7]][[2]], 
                   bears_games_2023_extract[[8]][[2]], bears_games_2023_extract[[9]][[2]], bears_games_2023_extract[[10]][[2]], 
                   bears_games_2023_extract[[11]][[2]], bears_games_2023_extract[[12]][[2]], bears_games_2023_extract[[13]][[2]], 
                   bears_games_2023_extract[[14]][[2]], lions_games_2022_extract[[1]][[2]], lions_games_2022_extract[[2]][[2]], 
                   lions_games_2022_extract[[3]][[2]], lions_games_2022_extract[[4]][[2]], lions_games_2022_extract[[5]][[2]], 
                   lions_games_2022_extract[[6]][[2]], lions_games_2022_extract[[7]][[2]], lions_games_2022_extract[[8]][[2]], 
                   lions_games_2022_extract[[9]][[2]], lions_games_2022_extract[[10]][[2]], lions_games_2022_extract[[11]][[2]], 
                   lions_games_2022_extract[[12]][[2]], lions_games_2022_extract[[13]][[2]], lions_games_2022_extract[[14]][[2]], 
                   lions_games_2022_extract[[15]][[2]], lions_games_2023_extract[[1]][[2]], lions_games_2023_extract[[2]][[2]], 
                   lions_games_2023_extract[[3]][[2]], lions_games_2023_extract[[4]][[2]], lions_games_2023_extract[[5]][[2]], 
                   lions_games_2023_extract[[6]][[2]], lions_games_2023_extract[[7]][[2]], lions_games_2023_extract[[8]][[2]], 
                   lions_games_2023_extract[[9]][[2]], lions_games_2023_extract[[10]][[2]], lions_games_2023_extract[[11]][[2]],
                   colts_games_2022_extract[[1]][[2]], colts_games_2022_extract[[2]][[2]], colts_games_2022_extract[[3]][[2]], 
                   colts_games_2022_extract[[4]][[2]], colts_games_2022_extract[[5]][[2]], colts_games_2022_extract[[6]][[2]], 
                   colts_games_2022_extract[[7]][[2]], colts_games_2022_extract[[8]][[2]], colts_games_2022_extract[[9]][[2]], 
                   colts_games_2022_extract[[10]][[2]], colts_games_2022_extract[[11]][[2]], colts_games_2022_extract[[12]][[2]], 
                   colts_games_2022_extract[[13]][[2]], colts_games_2023_extract[[1]][[2]], colts_games_2023_extract[[2]][[2]], 
                   colts_games_2023_extract[[3]][[2]], colts_games_2023_extract[[4]][[2]], colts_games_2023_extract[[5]][[2]], 
                   colts_games_2023_extract[[6]][[2]], colts_games_2023_extract[[7]][[2]], colts_games_2023_extract[[8]][[2]], 
                   colts_games_2023_extract[[9]][[2]], colts_games_2023_extract[[10]][[2]], colts_games_2023_extract[[11]][[2]], 
                   colts_games_2023_extract[[12]][[2]], colts_games_2023_extract[[13]][[2]], colts_games_2023_extract[[14]][[2]], 
                   colts_games_2023_extract[[15]][[2]], jaguars_games_2022_extract[[1]][[2]], jaguars_games_2022_extract[[2]][[2]], 
                   jaguars_games_2022_extract[[3]][[2]], jaguars_games_2022_extract[[4]][[2]], jaguars_games_2022_extract[[5]][[2]], 
                   jaguars_games_2022_extract[[6]][[2]], jaguars_games_2022_extract[[7]][[2]], jaguars_games_2022_extract[[8]][[2]], 
                   jaguars_games_2022_extract[[9]][[2]], jaguars_games_2022_extract[[10]][[2]], jaguars_games_2022_extract[[11]][[2]], 
                   jaguars_games_2022_extract[[12]][[2]], jaguars_games_2022_extract[[13]][[2]], jaguars_games_2022_extract[[14]][[2]],
                   jaguars_games_2022_extract[[15]][[2]], jaguars_games_2023_extract[[1]][[2]], jaguars_games_2023_extract[[2]][[2]], 
                   jaguars_games_2023_extract[[3]][[2]], jaguars_games_2023_extract[[4]][[2]], jaguars_games_2023_extract[[5]][[2]], 
                   jaguars_games_2023_extract[[6]][[2]], jaguars_games_2023_extract[[7]][[2]], jaguars_games_2023_extract[[8]][[2]], 
                   jaguars_games_2023_extract[[9]][[2]], jaguars_games_2023_extract[[10]][[2]], jaguars_games_2023_extract[[11]][[2]], 
                   jaguars_games_2023_extract[[12]][[2]], jaguars_games_2023_extract[[13]][[2]], jaguars_games_2023_extract[[14]][[2]], 
                   jaguars_games_2023_extract[[15]][[2]], titans_games_2022_extract[[1]][[2]], titans_games_2022_extract[[2]][[2]], 
                   titans_games_2022_extract[[3]][[2]], titans_games_2022_extract[[4]][[2]], titans_games_2022_extract[[5]][[2]], 
                   titans_games_2022_extract[[6]][[2]], titans_games_2022_extract[[7]][[2]], titans_games_2022_extract[[8]][[2]], 
                   titans_games_2022_extract[[9]][[2]], titans_games_2022_extract[[10]][[2]], titans_games_2022_extract[[11]][[2]], 
                   titans_games_2022_extract[[12]][[2]], titans_games_2023_extract[[1]][[2]], titans_games_2023_extract[[2]][[2]], 
                   titans_games_2023_extract[[3]][[2]], titans_games_2023_extract[[4]][[2]], titans_games_2023_extract[[5]][[2]], 
                   titans_games_2023_extract[[6]][[2]], titans_games_2023_extract[[7]][[2]], titans_games_2023_extract[[8]][[2]], 
                   titans_games_2023_extract[[9]][[2]], titans_games_2023_extract[[10]][[2]], titans_games_2023_extract[[11]][[2]], 
                   titans_games_2023_extract[[12]][[2]], titans_games_2023_extract[[13]][[2]], titans_games_2023_extract[[14]][[2]], 
                   titans_games_2023_extract[[15]][[2]])
over_under


# below is a function to clean the over/unders from the list above. What this function is doing is removing
# Over/Under: so that it just displays the number 
clean_over_under <- function(over_under) {
  over_under_c <- vector("character", length(over_under))
  for (i in seq_along(over_under)) {
    cleaned_lines <- gsub("Over/Under: ", "", over_under)
  }
  return(cleaned_lines)
}


# below is a list of all the cleaned over unders from the function above. Will be putting this into a column
# in the overall_data dataset
point_totals <- lapply(over_under[1:166], clean_over_under)


# creating a column in overall_data called points_totals that lists the over/unders for every game 
# in the season
overall_data$over_under <- point_totals


# below is a list of all the scores for every game in the season. Will be cleaning them and then combining 
# them to put into a column in overall_data called total_score
scores <- list(overall_data$Score[1:166])


# below is a function that takes the current score format, example (31-21), and takes the sum of the two 
# number, example (52). First, it using the str_split function to split the two numbers and remove the dash
# between them. Then, it uses the sum function to add them together, with the as.numeric function nested 
# inside it to make the two numbers numeric

calculate_total_scores <- function(scores) {
  # Access the vector of scores inside the first element of the list
  score_vector <- scores[[1]]
  
  # Initialize a numeric vector to store the total scores
  total_scores <- numeric(length(score_vector))
  
  for (i in seq_along(score_vector)) {
    # Split the score into two numbers and sum them
    score_split <- str_split(score_vector[i], "-")[[1]]
    total_scores[i] <- sum(as.numeric(score_split))
  }
  
  return(total_scores)
}
# below is a list of all the total_scores from the function above. Will be putting this in a column in 
# overall_data
total_scores <- calculate_total_scores(scores)

# Taking the list from above and placing it into overall_data
overall_data$total_score <- total_scores

# below is a new column in overall_data called over. Essentially, its calculating if the total score is more 
# than the games over under. If this is the case, then it displays yes. If it's not, then it displays no.
overall_data$over <- ifelse(overall_data$total_score >= overall_data$over_under, 1, 0)

# below is also a new column in overall_data, except this is measuring if the game went under the expected 
# over/under line. If this is the case, then it displays yes. If it's not, then it displays no.
overall_data$under <- ifelse(overall_data$total_score < overall_data$over_under, 1, 0)


hospital_1_reg <- lm(`Hospital_1_Patients` ~ Rival + Home + Result_1 + Temp + Wind_Speed + covered + over + 
                       City_Dummy + Close_Game, data = overall_data)
summary(hospital_1_reg)

hospital_2_reg <- lm(`Hospital_2_Patients` ~ Rival + Home + Result_1 + Temp + covered + over + 
                       City_Dummy + Close_Game, data = overall_data)
summary(hospital_2_reg)

hospital_3_reg <- lm(`Hospital_3_Patients` ~ Rival + Home + Result_1 + Temp + covered + over + 
                       City_Dummy + Close_Game, data = overall_data)
summary(hospital_3_reg)



str(overall_data)


# inlude dummy variable for cities, have name of city in one of the columns



# RF Model Hospital 1

# Convert categorical variables to factors and add a dummy variable for City
overall_data$Hospital_1_Above_Avg <- as.factor(overall_data$Hospital_1_Above_Avg)

overall_data <- overall_data %>%
  mutate(
    Rival = as.factor(Rival),
    Home = as.factor(Home),
    Result = as.factor(Result),
    City_Dummy = ifelse(City == "Chicago", 1, 0), # Add dummy variable for City
    Holiday_Dummy = ifelse(Holiday_Week == "None", 0, 1) # Add dummy variable for holiday
  )

# Split the overall_data into training and testing sets
set.seed(123)
hospital_1_train_index <- createDataPartition(overall_data$Hospital_1_Above_Avg, p = 0.7, list = FALSE)
hospital_1_train_data <- overall_data[hospital_1_train_index, ]
hospital_1_test_data <- overall_data[-hospital_1_train_index, ]

# Adjust levels for any other categorical variables in the test set
hospital_1_test_data$Rival <- factor(hospital_1_test_data$Rival, levels = levels(hospital_1_train_data$Rival))
hospital_1_test_data$Home <- factor(hospital_1_test_data$Home, levels = levels(hospital_1_train_data$Home))
hospital_1_test_data$Result <- factor(hospital_1_test_data$Result, levels = levels(hospital_1_train_data$Result))

# Random forest model including City_Dummy
hospital_1_rf_model <- randomForest(Hospital_1_Above_Avg ~ Rival + Home + Result + Temp + covered + over + 
                                      Close_Game + City_Dummy, data = hospital_1_train_data, ntree = 500, mtry = 2, importance = TRUE)

# View variable importance
importance(hospital_1_rf_model)
varImpPlot(hospital_1_rf_model)

# Predict on the test set and evaluate
hospital_1_rf_pred <- predict(hospital_1_rf_model, hospital_1_test_data)
hospital_1_confusion_matrix <- confusionMatrix(hospital_1_rf_pred, hospital_1_test_data$Hospital_1_Above_Avg)

hospital_1_confusion_matrix <- as.matrix(hospital_1_confusion_matrix)

hospital_1_correct_predictions <- diag(hospital_1_confusion_matrix)
head(hospital_1_correct_predictions)
hospital_1_total_predictions <- sum(hospital_1_confusion_matrix)
head(hospital_1_total_predictions)

hospital_1_accuracy <- hospital_1_correct_predictions / hospital_1_total_predictions
print(hospital_1_accuracy)

# new_input <- data.frame(
 # Rival = factor("No", levels = levels(hospital_1_train_data$Rival)),
 # Home = factor("No", levels = levels(hospital_1_train_data$Home)),
 # Result = factor("Loss", levels = levels(hospital_1_train_data$Result)),
 # Close_Game = factor("Light Rain", levels = levels(hospital_1_train_data$Weather)),
 # Temp = 41,
 # covered = 1,
 # over = 0
# )


# prediction <- predict(hospital_1_rf_model, new_input, type = "class")
# print(prediction)


# RF Model Hospital 2

# Convert categorical variables to factors and add a dummy variable for City

overall_data$Hospital_2_Above_Avg <- as.factor(overall_data$Hospital_2_Above_Avg)

overall_data <- overall_data %>%
  mutate(
    Rival = as.factor(Rival),
    Home = as.factor(Home),
    Result = as.factor(Result),
    City_Dummy = ifelse(City == "Chicago", 1, 0), # Add dummy variable for City
    Holiday_Dummy = ifelse(Holiday_Week == "None", 0, 1)
  )

# Split the overall_data into training and testing sets
set.seed(123)
hospital_2_train_index <- createDataPartition(overall_data$Hospital_2_Above_Avg, p = 0.7, list = FALSE)
hospital_2_train_data <- overall_data[hospital_2_train_index, ]
hospital_2_test_data <- overall_data[-hospital_2_train_index, ]

# Adjust levels for any other categorical variables in the test set
hospital_2_test_data$Rival <- factor(hospital_2_test_data$Rival, levels = levels(hospital_2_train_data$Rival))
hospital_2_test_data$Home <- factor(hospital_2_test_data$Home, levels = levels(hospital_2_train_data$Home))
hospital_2_test_data$Result <- factor(hospital_2_test_data$Result, levels = levels(hospital_2_train_data$Result))
hospital_2_train_data <- na.omit(hospital_2_train_data)
hospital_2_test_data <- na.omit(hospital_2_test_data)

# Random forest model including City_Dummy
hospital_2_rf_model <- randomForest(Hospital_2_Above_Avg ~ Rival + Home + Result + Temp + covered + over + 
                                      Close_Game + City_Dummy, data = hospital_2_train_data, ntree = 500, mtry = 2, importance = TRUE)

# View variable importance
importance(hospital_2_rf_model)
varImpPlot(hospital_2_rf_model)

# Predict on the test set and evaluate
hospital_2_rf_pred <- predict(hospital_2_rf_model, hospital_2_test_data)
hospital_2_confusion_matrix <- confusionMatrix(hospital_2_rf_pred, hospital_2_test_data$Hospital_2_Above_Avg)

hospital_2_confusion_matrix <- as.matrix(hospital_2_confusion_matrix)

hospital_2_correct_predictions <- diag(hospital_2_confusion_matrix)
head(hospital_2_correct_predictions)
hospital_2_total_predictions <- sum(hospital_2_confusion_matrix)
head(hospital_2_total_predictions)

hospital_2_accuracy <- hospital_2_correct_predictions / hospital_2_total_predictions
print(hospital_2_accuracy)


# RF Model Hospital 3

# Convert categorical variables to factors and add a dummy variable for City
overall_data$Hospital_3_Above_Avg <- as.factor(overall_data$Hospital_3_Above_Avg)

overall_data <- overall_data %>%
  mutate(
    Rival = as.factor(Rival),
    Home = as.factor(Home),
    Result = as.factor(Result),
    City_Dummy = ifelse(City == "Chicago", 1, 0), # Add dummy variable for City
    Holiday_Dummy = ifelse(Holiday_Week == "None", 0, 1)
  )

# Split the overall_data into training and testing sets
set.seed(123)
hospital_3_train_index <- createDataPartition(overall_data$Hospital_3_Above_Avg, p = 0.7, list = FALSE)
hospital_3_train_data <- overall_data[hospital_3_train_index, ]
hospital_3_test_data <- overall_data[-hospital_3_train_index, ]

# Adjust levels for any other categorical variables in the test set
hospital_3_test_data$Rival <- factor(hospital_3_test_data$Rival, levels = levels(hospital_3_train_data$Rival))
hospital_3_test_data$Home <- factor(hospital_3_test_data$Home, levels = levels(hospital_3_train_data$Home))
hospital_3_test_data$Result <- factor(hospital_3_test_data$Result, levels = levels(hospital_3_train_data$Result))
hospital_3_train_data <- na.omit(hospital_3_train_data)
hospital_3_test_data <- na.omit(hospital_3_test_data)

# Random forest model including City_Dummy
hospital_3_rf_model <- randomForest(Hospital_3_Above_Avg ~ Rival + Home + Result + Temp + covered + over + 
                                      Close_Game + City_Dummy, data = hospital_3_train_data, ntree = 500, mtry = 2, importance = TRUE)

# View variable importance
importance(hospital_3_rf_model)
varImpPlot(hospital_3_rf_model)

# Predict on the test set and evaluate
hospital_3_rf_pred <- predict(hospital_3_rf_model, hospital_3_test_data)
hospital_3_confusion_matrix <- confusionMatrix(hospital_3_rf_pred, hospital_3_test_data$Hospital_3_Above_Avg)

hospital_3_confusion_matrix <- as.matrix(hospital_3_confusion_matrix)

hospital_3_correct_predictions <- diag(hospital_3_confusion_matrix)
head(hospital_3_correct_predictions)
hospital_3_total_predictions <- sum(hospital_3_confusion_matrix)
head(hospital_3_total_predictions)

hospital_3_accuracy <- hospital_3_correct_predictions / hospital_3_total_predictions
print(hospital_3_accuracy)



ui <- fluidPage(
  titlePanel("Weekly Gameday Visits Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("year", "Select Year:", choices = unique(overall_data$Year)),
      selectInput("city", "Select City:", choices = unique(overall_data$City)),
      selectInput("hospital", "Select Hospital Number:", 
                  choices = c("Hospital_1", "Hospital_2", "Hospital_3"))
    ),
    
    mainPanel(
      plotOutput("weekly_plot")
    )
  )
)

server <- function(input, output) {
  
  # Reactive expression to filter data based on selected year, city, and hospital
  filtered_data <- reactive({
    # Filter by Year and City
    data <- overall_data[overall_data$Year == input$year & overall_data$City == input$city, ]
    
    # Select hospital-specific patient columns and their annual averages based on input
    if (input$hospital == "Hospital_1") {
      data <- data[, c("Week", "Hospital_1_Patients", "Hospital_1_Annual_Patient_Average")]
    } else if (input$hospital == "Hospital_2") {
      data <- data[, c("Week", "Hospital_2_Patients", "Hospital_2_Annual_Patient_Average")]
    } else {
      data <- data[, c("Week", "Hospital_3_Patients", "Hospital_3_Annual_Patient_Average")]
    }
    
    data
  })
  
  # Render the plot for hospital visits per week
  output$weekly_plot <- renderPlot({
    data <- filtered_data()
    
    # Check if data is not empty
    if (nrow(data) == 0 || ncol(data) < 2) {
      return(NULL)
    }
    
    # Extract column names dynamically for plotting
    patients_col <- names(data)[2]
    average_col <- names(data)[3]
    
    # Plot the total patients per week as a line graph with annual average
    ggplot(data, aes(x = Week, y = get(patients_col))) +
      geom_line(color = "blue", size = 1) +
      geom_point(color = "red") +
      geom_hline(aes(yintercept = mean(get(average_col), na.rm = TRUE)), 
                 linetype = "dashed", color = "green", size = 1) +
      theme_minimal() +
      labs(
        title = paste("Weekly Patients vs Annual Average for", input$hospital),
        x = "Week",
        y = "Number of Patients"
      ) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
}

# Run the app
shinyApp(ui = ui, server = server)













# Convert categorical variables to factors (if not done already)
overall_data$Hospital_1_Above_Avg <- as.factor(overall_data$Hospital_1_Above_Avg)

# Split the data into training and testing sets (same as before)
set.seed(123)
train_index <- createDataPartition(overall_data$Hospital_1_Above_Avg, p = 0.7, list = FALSE)
train_data <- overall_data[train_index, ]
test_data <- overall_data[-train_index, ]

train_data$Result <- as.factor(train_data$Result)
train_data$Hospital_1_Above_Avg <- as.numeric(train_data$Hospital_1_Above_Avg)
train_data$Hospital_1_Above_Avg <- ifelse(train_data$Hospital_1_Above_Avg == 1, 0, 1)

test_data$Result <- as.factor(test_data$Result)
test_data$Hospital_1_Above_Avg <- as.numeric(test_data$Hospital_1_Above_Avg)
test_data$Hospital_1_Above_Avg <- ifelse(test_data$Hospital_1_Above_Avg == 1, 0, 1)

# Train the Gradient Boosting Model
gbm_model <- gbm(Hospital_1_Above_Avg ~ Rival + Home + Result + Temp + covered,
                 data = train_data,
                 distribution = "bernoulli",  # For binary classification
                 n.trees = 1000,              # Number of trees
                 interaction.depth = 3,       # Depth of the trees
                 shrinkage = 0.01,            # Learning rate (controls how much each tree contributes)
                 cv.folds = 5,                # Cross-validation folds
                 n.minobsinnode = 10)        # Minimum observations per leaf

# View the summary of the GBM model
summary(gbm_model)



# Make predictions on the test set
gbm_pred <- predict(gbm_model, test_data, n.trees = gbm_model$n.trees, type = "response")
gbm_pred_class <- ifelse(gbm_pred > 0.5, 1, 0)  # Convert probabilities to class labels
gbm_pred_class <- as.factor(gbm_pred_class)

# Convert actual values to factor with the same levels as the predicted values
test_data$Hospital_1_Above_Avg <- factor(test_data$Hospital_1_Above_Avg, levels = c("0", "1"))

# Re-run confusionMatrix
confusionMatrix(gbm_pred_class, test_data$Hospital_1_Above_Avg)



levels(gbm_pred_class)
levels(test_data$Hospital_1_Above_Avg)






# Decision Tree Model

# Convert the target variable to a factor (modify for the relevant hospital column)
overall_data$Hospital_1_Above_Avg <- as.factor(overall_data$Hospital_1_Above_Avg)

overall_data <- overall_data %>%
  mutate(
    Rival = as.factor(Rival),
    Home = as.factor(Home),
    Result = as.factor(Result),
  )

# Split the overall_data into training and testing sets
set.seed(123)
train_index <- createDataPartition(overall_data$Hospital_1_Above_Avg, p = 0.7, list = FALSE)
train_data <- overall_data[train_index, ]
test_data <- overall_data[-train_index, ]


# Similarly, adjust levels for any other categorical variables if needed
test_data$Rival <- factor(test_data$Rival, levels = levels(train_data$Rival))
test_data$Home <- factor(test_data$Home, levels = levels(train_data$Home))
test_data$Result <- factor(test_data$Result, levels = levels(train_data$Result))


# Decision tree model
tree_model <- rpart(Hospital_1_Above_Avg ~ Rival + Home + Result + Temp + covered + over,
                    data = train_data, method = "class")


# Plot the decision tree
plot(tree_model, uniform = TRUE, main = "Decision Tree")
text(tree_model, use.n = TRUE, cex = 0.8)



# Predict on the test set and evaluate
tree_pred <- predict(tree_model, test_data, type = "class")
confusionMatrix(tree_pred, test_data$Hospital_1_Above_Avg)

# Random forest model
rf_model <- randomForest(Hospital_1_Above_Avg ~ Rival + Home + Result + Temp + covered + over,
                         data = train_data, ntree = 500, mtry = 2, importance = TRUE)

# View variable importance
importance(rf_model)
varImpPlot(rf_model)

# Predict on the test set and evaluate
rf_pred <- predict(rf_model, test_data)
confusionMatrix(rf_pred, test_data$Hospital_1_Above_Avg)



new_input <- data.frame(
  Rival = factor("Yes", levels = levels(train_data$Rival)),
  Home = factor("Yes", levels = levels(train_data$Home)),
  Result = factor("Win", levels = levels(train_data$Result)),
  Weather = factor("Sunny", levels = levels(train_data$Weather)),
  Temp = 75,
  covered = 1,
  over = 0
)


prediction <- predict(tree_model, new_input, type = "class")
print(prediction)









# arima model

# Remove any leading/trailing spaces and periods in the month abbreviation
overall_data$Date <- trimws(gsub("\\.", "", overall_data$Date))  # Remove period and trim whitespace

overall_data$Combined_Date <- paste(overall_data$Year, overall_data$Date)  # Combine the Year and Date

# Convert the new column to a proper Date format
overall_data$Combined_Date <- mdy(overall_data$Combined_Date)





help(ts)




overall_data <- overall_data %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>%
  arrange(Date)

# Check structure of overall_data
str(overall_data)

overall_data$attendance <- as.numeric(unlist(overall_data$attendance))
# Filter overall_data for ED visits and game days
# Assuming columns: Date, ED_Visits, Game_Day, Weather, Betting_Lines
ts_data <- ts(overall_data$Hospital_1_Patients, start = c(year(min(overall_data$Date)), month(min(overall_data$Date))), frequency = 365)

# Plot overall_data
autoplot(ts_data) +
  ggtitle("ED Visits Over Time") +
  xlab("Time") + ylab("ED Visits")

# Create ARIMA model with exogenous variables
# Assuming Weather and Betting_Lines are relevant predictors
xreg <- overall_data %>% select(Temp, covered, Rival, Home, Result_1, attendance) %>% as.matrix()
train_size <- round(0.8 * nrow(overall_data))

# Split overall_data into training and testing
train_data <- ts(ts_data[1:train_size])
test_data <- ts(ts_data[(train_size + 1):length(ts_data)])
train_xreg <- xreg[1:train_size, ]
test_xreg <- xreg[(train_size + 1):nrow(xreg), ]

# Fit ARIMA model
arima_model <- auto.arima(train_data, xreg = train_xreg, seasonal = TRUE)
summary(arima_model)

# Forecast future values
forecasted <- forecast(arima_model, xreg = test_xreg, h = nrow(test_xreg))

# Plot forecast
autoplot(forecasted) +
  ggtitle("Forecast of ED Visits") +
  xlab("Time in Days") + ylab("ED Visits")

# Evaluate model performance
actuals <- as.numeric(test_data)
predictions <- as.numeric(forecasted$mean)
MAE <- mean(abs(actuals - predictions))
RMSE <- sqrt(mean((actuals - predictions)^2))

cat("Model Performance:\n")
cat("Mean Absolute Error (MAE):", MAE, "\n")
cat("Root Mean Squared Error (RMSE):", RMSE, "\n")









model_results <- overall_data %>%
  group_by(City, Hospital) %>%
  do({
    # Prepare the time series data for the target variable (Hospital_1_Patients)
    ts_data <- ts(.$Hospital_1_Patients, start = c(year(min(.$Date)), month(min(.$Date))), frequency = 365)
    
    # Prepare the external regressors correctly
    xreg <- cbind(.$Temp, .$covered, .$Rival, .$Home, .$Result_1, .$attendance)  # Use cbind to combine columns correctly
    
    # Ensure the length of xreg matches the length of ts_data
    if (length(ts_data) != nrow(xreg)) {
      stop("Length of xreg does not match length of ts_data.")
    }
    
    # Split the data into training and testing sets
    train_size <- round(0.8 * length(ts_data))
    test_size <- length(ts_data) - train_size  # Ensure the test size is the remaining part
    
    train_data <- ts(ts_data[1:train_size])
    test_data <- ts(ts_data[(train_size + 1):length(ts_data)])
    
    # Ensure xreg is split according to the train/test split
    train_xreg <- xreg[1:train_size, ]
    test_xreg <- xreg[(train_size + 1):(train_size + test_size), ]  # Correct the indexing here
    
    # Fit ARIMA model with external regressors
    arima_model <- auto.arima(train_data, xreg = train_xreg, seasonal = TRUE)
    
    # Forecast future values
    forecasted <- forecast(arima_model, xreg = test_xreg, h = test_size)
    
    # Return the model, fitted values, and forecasted values
    list(
      model = arima_model,
      fitted = fitted(arima_model),
      forecasted = forecasted
    )
  })

# Example: View forecasted values for the first city-hospital combination
model_results[[1]]$forecasted

# Plot forecast for the first city-hospital combination
autoplot(model_results[[1]]$forecasted) +
  ggtitle(paste("Forecast of ED Visits for", model_results[[1]]$City, model_results[[1]]$Hospital)) +
  xlab("Time in Days") + ylab("ED Visits")

# Calculate and print model performance metrics (MAE, RMSE) for each city-hospital combination
model_performance <- model_results %>%
  rowwise() %>%
  mutate(
    # Calculate Mean Absolute Error (MAE)
    MAE = mean(abs(as.numeric(test_data) - as.numeric(forecasted$mean))),
    
    # Calculate Root Mean Squared Error (RMSE)
    RMSE = sqrt(mean((as.numeric(test_data) - as.numeric(forecasted$mean))^2))
  )

# View performance metrics
model_performance %>%
  select(City, Hospital, MAE, RMSE)








