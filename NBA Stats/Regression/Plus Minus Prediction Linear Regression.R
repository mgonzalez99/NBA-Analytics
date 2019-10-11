library(readr)
library(car)
setwd("~/Desktop/NBA Stats")

test_for_lineups_full <- read_csv("Regression/test_for_lineups_full.csv")
train_for_lineups_full <- read_csv("Regression/train_for_lineups_full.csv")

set.seed(123)
lineup_PM_linear <- lm(train_for_lineups_full$plus_minusPM ~ train_for_lineups_full$PPM + train_for_lineups_full$PFPM + train_for_lineups_full$TOVPM
                       + train_for_lineups_full$BLKPM + train_for_lineups_full$STLPM + train_for_lineups_full$ASTPM + train_for_lineups_full$REBPM + train_for_lineups_full$DRBPM
                       + train_for_lineups_full$ORBPM + train_for_lineups_full$FGPM + train_for_lineups_full$FGAPM +train_for_lineups_full$PM3P +train_for_lineups_full$PM3PA 
                       + train_for_lineups_full$PFDPM + train_for_lineups_full$`FT%`, data=train_for_lineups_full)


#Uses linear regression to predict 
lineup_PM_linear_train_predicted <- predict(lineup_PM_linear, train_for_lineups_full) 

#Combines predicted with actuAL DATA
train_for_lineups_full <- cbind(train_for_lineups_full, as.data.frame(lineup_PM_linear_train_predicted))

squared_error <- data.frame("squared error" = 1:(dim(train_for_lineups_full)[1]))

 for (i in 1:(dim(train_for_lineups_full))) {
   squared_error[i,] <- ((train_for_lineups_full[i,]$plus_minusPM)-(train_for_lineups_full[i,]$lineup_PM_linear_train))^2 
 }

RMSE_linear <- sqrt(mean(squared_error$squared.error))

plot(train_for_lineups_full$plus_minusPM,train_for_lineups_full$lineup_PM_linear_train)
abline(lm(train_for_lineups_full$plus_minusPM~train_for_lineups_full$lineup_PM_linear_train), col="red")

cor(train_for_lineups_full$plus_minusPM,train_for_lineups_full$lineup_PM_linear_train)

# compare_linear <- cbind (actual=train_for_lineups_full$plus_minusPM, lineup_PM_linear_train_predicted)  # combine actual and predicted
# mean(apply(compare_linear, 1, min)/apply(compare_linear, 1, max)) # calculate accuracy
# 


complete_test_for_lineups_full <- read_csv("Regression/complete_test_for_lineups_full.csv")
complete_train_for_lineups_full <- read_csv("Regression/complete_train_for_lineups_full.csv")

set.seed(123)
complete_lineup_PM_linear <- lm(complete_train_for_lineups_full$plus_minusPM ~ complete_train_for_lineups_full$PPM + complete_train_for_lineups_full$PFPM + complete_train_for_lineups_full$TOVPM
                       + complete_train_for_lineups_full$BLKPM + complete_train_for_lineups_full$STLPM + complete_train_for_lineups_full$ASTPM + complete_train_for_lineups_full$REBPM + complete_train_for_lineups_full$DRBPM
                       + complete_train_for_lineups_full$ORBPM + complete_train_for_lineups_full$FGPM + complete_train_for_lineups_full$FGAPM +complete_train_for_lineups_full$PM3P +complete_train_for_lineups_full$PM3PA 
                       + complete_train_for_lineups_full$PFDPM + complete_train_for_lineups_full$`FT%`, data=complete_train_for_lineups_full)


#Uses linear regression to predict 
predicted_complete_train_for_lineups_full <- predict(complete_lineup_PM_linear, complete_train_for_lineups_full) 

#Combines predicted with actuAL DATA
complete_train_for_lineups_full <- cbind(complete_train_for_lineups_full, as.data.frame(predicted_complete_train_for_lineups_full))

squared_error <- data.frame("squared error" = 1:(dim(complete_train_for_lineups_full)[1]))

for (i in 1:(dim(complete_train_for_lineups_full))) {
  squared_error[i,] <- ((complete_train_for_lineups_full[i,]$plus_minusPM)-(complete_train_for_lineups_full[i,]$predicted_complete_train_for_lineups_full))^2 
}

complete_RMSE_linear <- sqrt(mean(squared_error$squared.error))

plot(complete_train_for_lineups_full$plus_minusPM,complete_train_for_lineups_full$predicted_complete_train_for_lineups_full)
abline(lm(complete_train_for_lineups_full$plus_minusPM~complete_train_for_lineups_full$predicted_complete_train_for_lineups_full), col="red")

cor(complete_train_for_lineups_full$plus_minusPM,complete_train_for_lineups_full$predicted_complete_train_for_lineups_full)





