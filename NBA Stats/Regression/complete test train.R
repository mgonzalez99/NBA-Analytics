library(readr)
library(caret)
library(dplyr)

setwd("~/Desktop/NBA Stats")

# read in stints for all games in the season
complete_seasons_PM_lineup_data <- read_csv("Aggregated Data/complete_seasons_PM_lineup_data.csv")
complete_seasons_PM_lineup_data <- select(complete_seasons_PM_lineup_data, -X1)

# split into train and test set
# TODO: make this split such that all players show up in both sets
set.seed(234)
train_indices_for_lineups <- createDataPartition(complete_seasons_PM_lineup_data$plus_minusPM, p=0.8) %>% unlist()
complete_train_for_lineups <- complete_seasons_PM_lineup_data[train_indices_for_lineups,]
complete_test_for_lineups <- complete_seasons_PM_lineup_data[-train_indices_for_lineups,]

write_csv(complete_train_for_lineups, 'complete_train_for_lineups_full.csv')
write_csv(complete_test_for_lineups, 'complete_test_for_lineups_full.csv')
