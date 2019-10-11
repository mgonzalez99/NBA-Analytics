library(readr)
library(dplyr)
library(cluster)
library(FactoMineR)
library(factoextra)
library(broom)
library(rgl)
setwd("~/Desktop/NBA Stats")

#Imports totals lineup data
seasons_totals_lineup <- read_csv("Aggregated Data/seasons_totals_lineup.csv")
seasons_totals_lineup <- dplyr::select(seasons_totals_lineup, -X1)

#PCA on seasons lineup totals
set.seed(235)
pca_totals_lineup <- PCA(seasons_totals_lineup[,2:19])
plot(pca_totals_lineup, type = 'l')

#Extracting PCA Data Points
pca_totals_lineup_coord <- pca_totals_lineup$ind$coord
pca_totals_lineup_coord[is.na(pca_totals_lineup_coord)] <- 0

# Clustering using k means clustering of totals data, number of clusters can be changed anytime
set.seed(123)
k_mean_lineup <- kmeans(as.matrix(pca_totals_lineup_coord[, 1:2]), 6, nstart=25, iter.max=1000)

# adds colums vector of cluster categorizations to both seasons totals and pca data
seasons_totals_lineup<- cbind(seasons_totals_lineup, k_mean_lineup$cluster)
pca_totals_lineup_coord <- cbind(pca_totals_lineup_coord,k_mean_lineup$cluster)
pca_totals_lineup_coord <- as.data.frame(pca_totals_lineup_coord)

# Another visual representation using factoextra package
fviz_cluster(
  k_mean_lineup,
  pca_totals_lineup_coord[, 1:2],
  main = ''
)

# Lineup PM Data #
seasons_PM_lineup <- read_csv("Aggregated Data/seasons_PM_lineup_data.csv")

#Removes X1 column
seasons_PM_lineup <- select(seasons_PM_lineup, -X1)

#PCA on seasons lineup totals
set.seed(235)
pca_PM_lineup <- PCA(seasons_PM_lineup[,2:17])
plot(pca_PM_lineup, type = 'l')

#Extracting PCA Data Points
pca_PM_lineup_coord <- pca_PM_lineup$ind$coord
pca_PM_lineup_coord[is.na(pca_PM_lineup_coord)] <- 0

# Clustering using k means clustering of totals data, number of clusters can be changed anytime
set.seed(123)
k_mean_lineup_PM <- kmeans(as.matrix(pca_PM_lineup_coord[, 1:2]), 6, nstart=25, iter.max=1000)

# adds colums vector of cluster categorizations to both seasons totals and pca data
seasons_totals_lineup<- cbind(seasons_PM_lineup, k_mean_lineup_PM$cluster)
pca_PM_lineup_coord <- cbind(pca_PM_lineup_coord,k_mean_lineup_PM$cluster)
pca_PM_lineup_coord <- as.data.frame(pca_PM_lineup_coord)

# Another visual representation using factoextra package
fviz_cluster(
  k_mean_lineup_PM,
  pca_PM_lineup_coord[, 1:2],
  main = ''
)







