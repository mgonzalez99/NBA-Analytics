library(readr)
library(dplyr)
library(cluster)
library(FactoMineR)
library(factoextra)
library(broom)
library(rgl)
library(ggfortify)
library(ggplot2)
setwd("~/Desktop/NBA Stats")

# Imports individual data from 2016-2019 seasons (PM = per minute)
seasons_totals_data <- read_csv("Aggregated Data/season_totals_data.csv")

#removes first column
seasons_totals_data <- dplyr::select(seasons_totals_data, -X1)

#PCA on seasons totals
set.seed(235)
pca_totals <- PCA(seasons_totals_data[,2:19])
plot(pca_totals, type = 'l')

#Extracting PCA Data Points
pca_totals_coord <- pca_totals$ind$coord
pca_totals_coord[is.na(pca_totals_coord)] <- 0

# Clustering using k means clustering of totals data, number of clusters can be changed anytime
set.seed(123)
k_mean_totals <- kmeans(as.matrix(pca_totals_coord[, 1:2]), 4, nstart=25, iter.max=1000)

# adds colums vector of cluster categorizations to both seasons totals and pca data
seasons_totals_data <- cbind(seasons_totals_data, k_mean_totals$cluster)
seasons_totals_data <- seasons_totals_data %>% arrange(k_mean_totals$cluster)

# bar plot sample
barplot(seasons_totals_data$`MP▼`,
        main = "Point Scored",
        col = c("red","blue","green","orange")[unclass(seasons_totals_data$`k_mean_totals$cluster`)],
        xlab = "Lineup Cluster",
        ylab = "Total Points")


##### PM INDIVIDUAL BAR PLOTS ######
#Import PM data
seasons_PM_data <- read_csv("Aggregated Data/seasons_PM_data.csv")
seasons_PM_data <- dplyr::select(seasons_PM_data, -X1, -`MP▼`)

#PCA on seasons PM
set.seed(235)
pca_PM <- PCA(seasons_PM_data[,2:17])
plot(pca_PM, type = 'l')

#Extracting PCA Data Points
pca_PM_coord <- pca_PM$ind$coord
pca_PM_coord[is.na(pca_PM_coord)] <- 0

set.seed(123)
k_mean_PM <- kmeans(as.matrix(pca_PM_coord[, 1:2]), 7, nstart=25, iter.max=1000)

# adds colums vector of cluster categorizations to both seasons totals and pca data
seasons_PM_data <- cbind(seasons_PM_data, k_mean_PM$cluster)
pca_PM_coord <- cbind(pca_PM_coord, k_mean_PM$cluster)
pca_PM_coord <- as.data.frame(pca_PM_coord)

#Arranges data in cluster number order
seasons_PM_data <- seasons_PM_data %>% arrange(seasons_PM_data$`k_mean_PM$cluster`)

# bar plot sample
barplot(seasons_PM_data$PPM,
        main = "Total Rebounds",
        col = c("red","blue","green","orange", "purple", "yellow")[unclass(seasons_PM_data$`k_mean_PM$cluster`)],
        xlab = "Lineup Cluster",
        ylab = "Total Rebounds")

##### PM LINEUP BAR PLOTS ######

# Lineup PM Data #
seasons_PM_lineup <- read_csv("Aggregated Data/seasons_PM_lineup_data.csv")

#Removes X1 column
seasons_PM_lineup <- select(seasons_PM_lineup, -X1)

#PCA on seasons lineup totals
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

seasons_PM_lineup <- seasons_PM_lineup %>% arrange(seasons_PM_lineup$`k_mean_lineup_PM$cluster`)
seasons_PM_lineup_1 <- filter(seasons_PM_lineup,seasons_PM_lineup$`k_mean_lineup_PM$cluster`<4) 
seasons_PM_lineup_2 <- filter(seasons_PM_lineup,seasons_PM_lineup$`k_mean_lineup_PM$cluster`>3)  


# bar plot sample
barplot(seasons_PM_lineup_1$ASTPM,
        main = "PPM",
        col = c("red","blue","green","orange", "purple", "yellow")[unclass(seasons_PM_lineup$`k_mean_lineup_PM$cluster`)],
        xlab = "Lineup Cluster",
        ylab = "PPM")

barplot(seasons_PM_lineup_2$ASTPM,
        main = "PPM",
        col = c("orange", "purple", "yellow")[unclass(seasons_PM_lineup$`k_mean_lineup_PM$cluster`)],
        xlab = "Lineup Cluster",
        ylab = "PPM")




