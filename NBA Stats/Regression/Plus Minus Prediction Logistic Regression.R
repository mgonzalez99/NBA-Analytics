library(readr)
setwd("~/Desktop/NBA Stats")

test_for_lineups_full <- read_csv("Regression/test_for_lineups_full.csv")
train_for_lineups_full <- read_csv("Regression/train_for_lineups_full.csv")

set.seed(123)
lineup_PM_logistic <- glm(train_for_lineups_full$plus_minusPM ~ train_for_lineups_full$PPM + train_for_lineups_full$PFPM + train_for_lineups_full$TOVPM
    + train_for_lineups_full$BLKPM + train_for_lineups_full$STLPM + train_for_lineups_full$ASTPM + train_for_lineups_full$REBPM + train_for_lineups_full$DRBPM
    + train_for_lineups_full$ORBPM + train_for_lineups_full$FGPM + train_for_lineups_full$FGAPM +train_for_lineups_full$PM3P +train_for_lineups_full$PM3PA 
    + train_for_lineups_full$PFDPM + train_for_lineups_full$`FT%`, data=train_for_lineups_full)

lineup_PM_logistic_train <- predict(lineup_PM_logistic, train_for_lineups_full) 

#Combines predicted with actuAL DATA
train_for_lineups_full <- cbind(train_for_lineups_full, as.data.frame(lineup_PM_logistic_train))

squared_error_logistic <- data.frame("squared error" = 1:(dim(train_for_lineups_full)[1]))

for (i in 1:(dim(train_for_lineups_full))) {
  squared_error_logistic[i,] <- ((train_for_lineups_full[i,]$plus_minusPM)-(train_for_lineups_full[i,]$lineup_PM_logistic_train))^2 
}

RMSE_logistic <- sqrt(mean(squared_error_logistic$squared.error))

plot(train_for_lineups_full$plus_minusPM,train_for_lineups_full$lineup_PM_logistic_train)
abline(lm(train_for_lineups_full$plus_minusPM~train_for_lineups_full$lineup_PM_logistic_train), col="red")

cor(train_for_lineups_full$plus_minusPM,train_for_lineups_full$lineup_PM_logistic_train)
