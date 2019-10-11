library(readr)
library(dplyr)
library(cluster)
library(FactoMineR)
library(factoextra)
library(broom)
library(rgl)
setwd("~/Desktop/NBA Stats")

# Lineup PM Data #
seasons_PM_lineup <- read_csv("Aggregated Data/seasons_PM_lineup_data.csv")

#Removes X1 column
seasons_PM_lineup <- select(seasons_PM_lineup, -X1)

#PCA on seasons lineup PM
set.seed(235)
pca_PM_lineup <- PCA(seasons_PM_lineup[,2:16])
plot(pca_PM_lineup, type = 'l')

#Extracting PCA Data Points
pca_PM_lineup_coord <- pca_PM_lineup$ind$coord
pca_PM_lineup_coord[is.na(pca_PM_lineup_coord)] <- 0

# Clustering using k means clustering of totals data, number of clusters can be changed anytime
set.seed(123)
k_mean_lineup_PM <- kmeans(as.matrix(pca_PM_lineup_coord[, 1:2]), 6, nstart=25, iter.max=1000)

# adds colums vector of cluster categorizations to both seasons totals and pca data
seasons_PM_lineup<- cbind(seasons_PM_lineup, k_mean_lineup_PM$cluster)
pca_PM_lineup_coord <- cbind(pca_PM_lineup_coord,k_mean_lineup_PM$cluster)
pca_PM_lineup_coord <- as.data.frame(pca_PM_lineup_coord)

lineup_1 <- seasons_PM_lineup %>% filter(`k_mean_lineup_PM$cluster`==1)
lineup_2 <- seasons_PM_lineup %>% filter(`k_mean_lineup_PM$cluster`==2)
lineup_3 <- seasons_PM_lineup %>% filter(`k_mean_lineup_PM$cluster`==3)
lineup_4 <- seasons_PM_lineup %>% filter(`k_mean_lineup_PM$cluster`==4)
lineup_5 <- seasons_PM_lineup %>% filter(`k_mean_lineup_PM$cluster`==5)
lineup_6 <- seasons_PM_lineup %>% filter(`k_mean_lineup_PM$cluster`==6)


lineup_cluster_info_extraction <- data.frame("Cluster Type" = 1:6,
                                             "Mean PPM" = 1:6,
                                             "Mean PFPM" = 1:6,
                                             "Mean TOVPM" = 1:6,
                                             "Mean BLKPM" = 1:6,
                                             "Mean STLPM" = 1:6,
                                             "Mean ASTPM" = 1:6,
                                             "Mean REBPM" = 1:6,
                                             "Mean DRBPM" = 1:6,
                                             "Mean ORBPM" = 1:6,
                                             "Mean FGPM" = 1:6,
                                             "Mean FGAPM" = 1:6,
                                             "Mean 3PMPM" = 1:6,
                                             "Mean 3PAPM" = 1:6,
                                             "Mean PFDPM" = 1:6,
                                             "MEAN Plus minus" = 1:6,
                                             "Mean FT%M" = 1:6)
                                             

for (i in 1:6) {
 lineup_cluster_info_extraction[,2][i] <- mean(as.data.frame(seasons_PM_lineup %>% filter(`k_mean_lineup_PM$cluster` == i))$PPM)
 lineup_cluster_info_extraction[,3][i] <- mean(as.data.frame(seasons_PM_lineup %>% filter(`k_mean_lineup_PM$cluster` == i))$PFPM)
 lineup_cluster_info_extraction[,4][i] <- mean(as.data.frame(seasons_PM_lineup %>% filter(`k_mean_lineup_PM$cluster` == i))$TOVPM)
 lineup_cluster_info_extraction[,5][i] <- mean(as.data.frame(seasons_PM_lineup %>% filter(`k_mean_lineup_PM$cluster` == i))$BLKPM)
 lineup_cluster_info_extraction[,6][i] <- mean(as.data.frame(seasons_PM_lineup %>% filter(`k_mean_lineup_PM$cluster` == i))$STLPM)
 lineup_cluster_info_extraction[,7][i] <- mean(as.data.frame(seasons_PM_lineup %>% filter(`k_mean_lineup_PM$cluster` == i))$ASTPM)
 lineup_cluster_info_extraction[,8][i] <- mean(as.data.frame(seasons_PM_lineup %>% filter(`k_mean_lineup_PM$cluster` == i))$REBPM)
 lineup_cluster_info_extraction[,9][i] <- mean(as.data.frame(seasons_PM_lineup %>% filter(`k_mean_lineup_PM$cluster` == i))$DRBPM)
 lineup_cluster_info_extraction[,10][i] <- mean(as.data.frame(seasons_PM_lineup %>% filter(`k_mean_lineup_PM$cluster` == i))$ORBPM)
 lineup_cluster_info_extraction[,11][i] <- mean(as.data.frame(seasons_PM_lineup %>% filter(`k_mean_lineup_PM$cluster` == i))$FGPM)
 lineup_cluster_info_extraction[,12][i] <- mean(as.data.frame(seasons_PM_lineup %>% filter(`k_mean_lineup_PM$cluster` == i))$FGAPM)
 lineup_cluster_info_extraction[,13][i] <- mean(as.data.frame(seasons_PM_lineup %>% filter(`k_mean_lineup_PM$cluster` == i))$PM3P)
 lineup_cluster_info_extraction[,14][i] <- mean(as.data.frame(seasons_PM_lineup %>% filter(`k_mean_lineup_PM$cluster` == i))$PM3PA)
 lineup_cluster_info_extraction[,15][i] <- mean(as.data.frame(seasons_PM_lineup %>% filter(`k_mean_lineup_PM$cluster` == i))$PFDPM)
 lineup_cluster_info_extraction[,16][i] <- mean(as.data.frame(seasons_PM_lineup %>% filter(`k_mean_lineup_PM$cluster` == i))$plus_minusPM)
 lineup_cluster_info_extraction[,17][i] <- mean(as.data.frame(seasons_PM_lineup %>% filter(`k_mean_lineup_PM$cluster` == i))$`FT%`)
}

#t test for different mean of a specified statistic
t.test(lineup_1$PPM, lineup_4$PPM)
        
        
        

