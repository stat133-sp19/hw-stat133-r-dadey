# # Title:  "Make shots as data"
# # Purpose:  Uploads, cleans and consolidates available data on players for analysis.
# # Inputs:  "andre-iguodala.csv",
# # "draymond-green.csv",
# # "kevin-durant.csv",
# # "klay-thompson.csv",
# # "stephen-curry.csv"
# # Ouputs:  "shots-data-summary.txt"
#
# ------------------------------------------------------------------------------------

#Set Working Directory
setwd("C:/Users/RD/Documents/R/workout1/data")

#Define column classes
classes <-
  c(
    "factor",
    #team_name
    "character",
    #game_date
    "factor",
    #season
    "integer",
    #period
    "integer",
    #minutes_remaining
    "real",
    #seconds_remaining
    "character",
    #shot_made_flag
    "factor",
    #action_type
    "factor",
    #shot_type
    "integer",
    #shot_distance
    "factor",
    #opponent
    "integer",
    #x
    "integer"  #y
  )

#Read CSVs

andre.iguodala <- read.csv(file = "./andre-iguodala.csv",
                           colClasses = classes)
draymond.green <- read.csv(file = "./draymond-green.csv",
                           colClasses = classes)
kevin.durant <- read.csv(file = "./kevin-durant.csv",
                         colClasses = classes)
klay.thompson <- read.csv(file = "./klay-thompson.csv",
                          colClasses = classes)
stephen.curry <- read.csv(file = "./stephen-curry.csv",
                          colClasses = classes)

#Insert names
#Create vector summarising file names
files <-
  list(andre.iguodala,
       draymond.green,
       kevin.durant,
       klay.thompson,
       stephen.curry)
names <-
  c("Andre Iguodala",
    "Draymond Green",
    "Kevin Durant",
    "Klay Thompson",
    "Stephen Curry")
len <- 1:length(files)

andre.iguodala$name = rep("Andre Iguodala", length(andre.iguodala[[1]]))
draymond.green$name = rep("Draymond Green", length(draymond.green[[1]]))
kevin.durant$name = rep("Kevin Durant", length(kevin.durant[[1]]))
klay.thompson$name = rep("Klay Thompson", length(klay.thompson[[1]]))
stephen.curry$name = rep("Stephen Curry", length(stephen.curry[[1]]))


#Improve shot_made_flag
andre.iguodala$shot_made_flag[andre.iguodala$shot_made_flag == "y"] <-
  "shot.yes"
andre.iguodala$shot_made_flag[andre.iguodala$shot_made_flag == "n"] <-
  "shot.no"

draymond.green$shot_made_flag[draymond.green$shot_made_flag == "y"] <-
  "shot.yes"
draymond.green$shot_made_flag[draymond.green$shot_made_flag == "n"] <-
  "shot.no"

kevin.durant$shot_made_flag[kevin.durant$shot_made_flag == "y"] <-
  "shot.yes"
kevin.durant$shot_made_flag[kevin.durant$shot_made_flag == "n"] <-
  "shot.no"

klay.thompson$shot_made_flag[klay.thompson$shot_made_flag == "y"] <-
  "shot.yes"
klay.thompson$shot_made_flag[klay.thompson$shot_made_flag == "n"] <-
  "shot.no"

stephen.curry$shot_made_flag[stephen.curry$shot_made_flag == "y"] <-
  "shot.yes"
stephen.curry$shot_made_flag[stephen.curry$shot_made_flag == "n"] <-
  "shot.no"

#Calculate minutes remaning
andre.iguodala$minutes = 12 * andre.iguodala$period - andre.iguodala$minutes_remaining

draymond.green$minutes = 12 * draymond.green$period - draymond.green$minutes_remaining

kevin.durant$minutes = 12 * kevin.durant$period - kevin.durant$minutes_remaining

klay.thompson$minutes = 12 * klay.thompson$period - klay.thompson$minutes_remaining

stephen.curry$minutes = 12 * stephen.curry$period - stephen.curry$minutes_remaining

#Print summary of player data
sink(file = "../output/andre.iguodala.summary.txt")
summary(andre.iguodala)
sink()

sink(file = "../output/draymond.green.summary.txt")
summary(draymond.green)
sink()

sink(file = "../output/kevin.durant.summary.txt")
summary(kevin.durant)
sink()

sink(file = "../output/klay.thompson.summary.txt")
summary(klay.thompson)
sink()

sink(file = "../output/stephen.curry.summary.txt")
summary(stephen.curry)
sink()

#Consolidate player data into one file
shots.data <- rbind(andre.iguodala, draymond.green, kevin.durant, klay.thompson, stephen.curry)
write.csv(shots.data, file = "./shots.data.csv")

#Print summary of consolidated data
sink(file = "../output/shots.data.txt")
summary(shots.data)
sink()

#END

