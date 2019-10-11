library(expss)
library(readr)
library(rpart)
library(gbm)
library(randomForest)
library(adabag)
library(dplyr)
library(e1071)

setwd("~/Desktop/NBA Stats")

#Upload training lineup data
train_for_lineups_full <- read_csv("Regression/train_for_lineups_full.csv")

#Upload testing lineup data
test_for_lineups_full <- read_csv("Regression/test_for_lineups_full.csv")

#Classify as either elite or bad based on plus minus
elite_lineup <- train_for_lineups_full %>% filter(plus_minusPM >= 0) %>% mutate(level = as.factor("elite"))
bad_lineup <- train_for_lineups_full %>% filter(plus_minusPM < 0) %>% mutate(level = as.factor("bad"))

#Classify as either elite or bad based on plus minus
elite_lineup_test <- test_for_lineups_full %>% filter(plus_minusPM >= 0) %>% mutate(level = as.factor("elite"))
bad_lineup_test <- test_for_lineups_full %>% filter(plus_minusPM < 0) %>% mutate(level = as.factor("bad"))

classified_lineups <- rbind(elite_lineup, bad_lineup)
classified_lineups_test <- rbind(elite_lineup_test, bad_lineup_test)


set.seed(4)
decision_tree <- rpart(classified_lineups$level ~ classified_lineups$PPM + classified_lineups$PFPM +classified_lineups$TOVPM +
                         classified_lineups$BLKPM + classified_lineups$STLPM +classified_lineups$ASTPM+classified_lineups$REBPM+
                         classified_lineups$DRBPM+classified_lineups$ORBPM+classified_lineups$FGPM+classified_lineups$FGAPM+
                         classified_lineups$PM3P +classified_lineups$PM3PA+classified_lineups$PFDPM+classified_lineups$`FT%`, data = classified_lineups, method = "class")

#INFO on the classification
plot(decision_tree)
text(decision_tree)
summary(decision_tree)

#Predicts which lineups are elite/terrible 
decision_train_predict <- predict(decision_tree, classified_lineups, type = "class") 

#Adds lineup predictions to original lineup data frame
lineup_decision_check <- cbind(classified_lineups, predicted = decision_train_predict)

#Accuracy of decision tree
accuracy <- (dim(filter(lineup_decision_check,level ==predicted ))[1])/(dim(lineup_decision_check)[1])

#table
cro(lineup_decision_check$level, lineup_decision_check$predicted)


#random forest
set.seed(1)
forest <- randomForest(level ~ PPM + PFPM +TOVPM +
                          BLKPM + STLPM + ASTPM + REBPM +
                          DRBPM + ORBPM + FGPM + FGAPM +
                          PM3P + PM3PA + PFDPM, data = classified_lineups, n.tree = 500)

 #Predicts which lineups are elite/terrible
 forest_train_predict <- predict(forest, classified_lineups, type = "class")
 lineup_forest_check <- cbind(classified_lineups, predicted = forest_train_predict)
 accuracy_forest <- (dim(filter(lineup_forest_check,level == predicted ))[1])/(dim(lineup_forest_check)[1])
 cro(lineup_forest_check$level, lineup_forest_check$predicted)
#
# #test lineups
 forest_test_predict <- predict(forest, classified_lineups_test, type = "class")
 lineup_forest_check_test <- cbind(classified_lineups_test, predicted = forest_test_predict)
 accuracy_forest_test <- (dim(filter(lineup_forest_check_test, level == predicted ))[1])/(dim(lineup_forest_check_test)[1])
 cro(lineup_forest_check_test$level, lineup_forest_check_test$predicted)


# Logistic regression model
library(arm)
logistic <- bayesglm(classified_lineups$level ~ classified_lineups$PPM + classified_lineups$PFPM +classified_lineups$TOVPM +
                       classified_lineups$BLKPM + classified_lineups$STLPM +classified_lineups$ASTPM+classified_lineups$REBPM+
                       classified_lineups$DRBPM+classified_lineups$ORBPM+classified_lineups$FGPM+classified_lineups$FGAPM+
                       classified_lineups$PM3P +classified_lineups$PM3PA+classified_lineups$PFDPM+classified_lineups$`FT%`, data = classified_lineups, family = binomial)

logistic_pred <- predict(logistic, classified_lineups, type = "response")
glm_pred <- rep("elite",dim(classified_lineups)[1])
glm_pred[logistic_pred >.65]="terrible"

logistic_check <- cbind(classified_lineups, predicted = glm_pred)

logistic_check = apply_labels(logistic_check,
                                   predicted = "predicted level",
                                   level = "actual level")

glm_table <- cro(logistic_check$level, logistic_check$predicted)
accuracy_glm <- (as.numeric(glm_table[1,2]) + as.numeric(glm_table[2,3])) / (as.numeric(glm_table[3,3]) + as.numeric(glm_table[3,2]))


#k means
# k- nearest neighbors algorithms with k = 6, you can change k to experiment with overall results
library(class)
set.seed(5)
neighbors <- knn(classified_lineups[-c(1,18)], classified_lineups_test[-c(1,18)], cl = classified_lineups$level, k = 6)

#Adds lineup predictions to original lineup data frame
neighbors_check <- cbind(classified_lineups_test, predicted = neighbors)

neighbors_table <- cro(neighbors_check$level, neighbors_check$predicted)
accuracy_neighbors <- (as.numeric(neighbors_table[1,2]) + as.numeric(neighbors_table[2,3])) / (as.numeric(neighbors_table[3,3]) + as.numeric(neighbors_table[3,2]))



#Boosting 
set.seed(250)
boost <- boosting(level ~ PPM + PFPM +TOVPM +
                    BLKPM + STLPM + ASTPM + REBPM +
                    DRBPM + ORBPM + FGPM + FGAPM +
                    PM3P + PM3PA + PFDPM + `FT%`, data = classified_lineups, n.tree =3, distribution = "gaussian")

#Predicts which lineups are elite/terrible 
#Adds lineup predictions to original lineup data frame
boost_train <- predict(boost, as.data.frame(classified_lineups))
boost_check <- cbind(classified_lineups, predicted = boost_train$class)





# library(readr)
# library(neuralnet)
# 
# nn_level <- neuralnet(level ~ PPM + PFPM + TOVPM
#                 + BLKPM + STLPM + ASTPM + REBPM + DRBPM
#                 + ORBPM + FGPM + FGAPM +PM3P +PM3PA 
#                 + PFDPM, data= classified_lineups, hidden=3,act.fct = "logistic", linear.output = FALSE)
# 
# nn_predict <- compute(nn,test_for_lineups_full)
# nn_predict$net.result
# 
# prob <-nn_predict$net.result
# pred <- ifelse(prob>0.5, 'elite', 'ba\')
# pred
# 
# nn_predict$net.result
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
