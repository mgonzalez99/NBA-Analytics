library(readr)
library(neuralnet)
setwd("~/Desktop/NBA Stats")

test_for_lineups_full <- read_csv("Regression/test_for_lineups_full.csv")
train_for_lineups_full <- read_csv("Regression/train_for_lineups_full.csv")

set.seed(234)
nn <- neuralnet(plus_minusPM ~ PPM + PFPM + TOVPM
                + BLKPM + STLPM + ASTPM + REBPM + DRBPM
                + ORBPM + FGPM + FGAPM +PM3P +PM3PA 
                + PFDPM, data= train_for_lineups_full, hidden=3, stepmax = 1e7)

nn_predict <- predict(nn,train_for_lineups_full)

#Combines predicted with actuAL DATA
train_for_lineups_full <- cbind(train_for_lineups_full, nn_predict)

squared_error <- data.frame("squared error" = 1:(dim(train_for_lineups_full)[1]))

for (i in 1:(dim(train_for_lineups_full))) {
  squared_error[i,] <- ((train_for_lineups_full[i,]$plus_minusPM)-(train_for_lineups_full[i,]$nn_predict))^2 
}

RMSE_nn <- sqrt(mean(squared_error$squared.error))

plot(train_for_lineups_full$plus_minusPM,train_for_lineups_full$nn_predict)
abline(lm(train_for_lineups_full$plus_minusPM~train_for_lineups_full$nn_predict), col="red")
 cor(train_for_lineups_full$plus_minusPM,train_for_lineups_full$nn_predict)
