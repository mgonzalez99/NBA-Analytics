library(readr)
library(caret)
library(dplyr)

setwd("~/Desktop/NBA Stats")

# read in stints for all games in the season
seasons_PM_lineup_data <- read_csv("Aggregated Data/seasons_PM_lineup_data.csv")
seasons_PM_lineup_data <- select(seasons_PM_lineup_data, -X1)
#Parsed with column specification: ### Line subject to change depedning on where this file is saved

# split into train and test set
# TODO: make this split such that all players show up in both sets
set.seed(234)
train_indices_for_lineups <- createDataPartition(seasons_PM_lineup_data$plus_minusPM, p=0.8) %>% unlist()
train_for_lineups <- seasons_PM_lineup_data[train_indices_for_lineups,]
test_for_lineups <- seasons_PM_lineup_data[-train_indices_for_lineups,]

write_csv(train_for_lineups, 'train_for_lineups_full.csv')
write_csv(test_for_lineups, 'test_for_lineups_full.csv')
