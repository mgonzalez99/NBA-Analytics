library(readr)
library (ridge)
library (car)
setwd("~/Desktop/NBA Stats")

test_for_lineups_full <- read_csv("Regression/test_for_lineups_full.csv")
train_for_lineups_full <- read_csv("Regression/train_for_lineups_full.csv")

linRidgeMod <- linearRidge(train_for_lineups_full$plus_minusPM ~ train_for_lineups_full$PPM + train_for_lineups_full$PFPM + train_for_lineups_full$TOVPM
                           + train_for_lineups_full$BLKPM + train_for_lineups_full$STLPM + train_for_lineups_full$ASTPM + train_for_lineups_full$REBPM + train_for_lineups_full$DRBPM
                           + train_for_lineups_full$ORBPM + train_for_lineups_full$FGPM + train_for_lineups_full$FGAPM +train_for_lineups_full$PM3P +train_for_lineups_full$PM3PA 
                           + train_for_lineups_full$PFDPM + train_for_lineups_full$`FT%`, data=train_for_lineups_full)  # the ridge regression model

predicted <- predict(linRidgeMod, train_for_lineups_full)  # predict on test data
compare <- cbind (actual=train_for_lineups_full$plus_minusPM, predicted)  # combine
# mean (apply(compare, 1, min)/apply(compare, 1, max)) # calculate accuracy

#Uses linear regression to predict 

#Combines predicted with actuAL DATA
train_for_lineups_full <- cbind(train_for_lineups_full, as.data.frame(predicted))

squared_error_ridge <- data.frame("squared error" = 1:(dim(train_for_lineups_full)[1]))

for (i in 1:(dim(train_for_lineups_full[1]))) {
  squared_error_ridge[i,] <- ((train_for_lineups_full[i,]$plus_minusPM)-(train_for_lineups_full[i,]$predicted))^2 
}


RMSE_ridge <- sqrt(mean(squared_error_ridge$squared.error))

plot(train_for_lineups_full$plus_minusPM,train_for_lineups_full$predicted)
abline(lm(train_for_lineups_full$plus_minusPM~train_for_lineups_full$predicted), col="red")

cor(train_for_lineups_full$plus_minusPM,train_for_lineups_full$predicted)

