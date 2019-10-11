library(readr)
library(dplyr)
library(cluster)
library(FactoMineR)
library(factoextra)
library(broom)
library(rgl)
setwd("~/Desktop/NBA Stats")

# Imports individual data from 2016-2019 seasons (PM = per minute)
seasons_totals_data <- read_csv("Aggregated Data/season_totals_data.csv")

#removes first column
seasons_totals_data <- dplyr::select(seasons_totals_data, -X1, -`MP▼`)

#PCA on seasons totals
set.seed(235)
pca_totals <- PCA(seasons_totals_data[,2:17])
plot(pca_totals, type = 'l')

#Extracting PCA Data Points
pca_totals_coord <- pca_totals$ind$coord
pca_totals_coord[is.na(pca_totals_coord)] <- 0

# Clustering using k means clustering of totals data, number of clusters can be changed anytime
set.seed(123)
k_mean_totals <- kmeans(as.matrix(pca_totals_coord[, 1:2]), 4, nstart=25, iter.max=1000)

# adds colums vector of cluster categorizations to both seasons totals and pca data
seasons_totals_data <- cbind(seasons_totals_data, k_mean_totals$cluster)
pca_totals_coord <- cbind(pca_totals_coord, k_mean_totals$cluster)
pca_totals_coord <- as.data.frame(pca_totals_coord)

# Another visual representation using factoextra package
fviz_cluster(
  k_mean_totals,
  data = pca_totals_coord[, 1:2],
  main = ''
)

plot(x = pca_totals_coord[,1], y = pca_totals_coord[,2],
     main = "Lineup Clustering: K=7",
     xlab = "PCA Dim 1 (77.62%)", xlim = c(-7, 20),
     ylab = "PCA Dim 2 (11.47%)", ylim = c(-10,15),
     pch = c(1,2,3,4,5,6,7)[unclass(pca_totals_coord$V6)],
     col = c("red","blue","green","orange", "purple", "brown", "black")[unclass(pca_totals_coord$V6)])

# 3D visualization wth rgl package (make sure XQuartz is installed), great tool to
# distinguish between clusters in 3D
plot3d(x = pca_totals_coord[,1], y= pca_totals_coord[,2], z =pca_totals_coord[,3],
       main = "Player K-Means Clustering: K=10",
       xlab = 'PCA Dim 1 (77.62.28%)', xlim = c(-5, 15),
       ylab = 'PCA Dim 2 (11.47%)', ylim = c(-7, 15),
       zlab = 'PCA Dim 3 (3.81%)', zlim = c(-7, 7),
       pch = c(1,2,3,4,5,6,7,8,9,10)[unclass(pca_totals_coord$V6)],
       col = c("red","blue","green","orange","purple", "brown", "pink", "black", "yellow", "red")[unclass(pca_totals_coord$V6)])


### PER MINUTE DATA ##
seasons_PM_data <- read_csv("Aggregated Data/seasons_PM_data.csv")
seasons_PM_data <- dplyr::select(seasons_PM_data, -X1, -`MP▼`)

#PCA on seasons totals
set.seed(235)
pca_PM <- PCA(seasons_PM_data[,2:17])
plot(pca_totals, type = 'l')

#Extracting PCA Data Points
pca_PM_coord <- pca_PM$ind$coord
pca_PM_coord[is.na(pca_PM_coord)] <- 0

set.seed(123)
k_mean_PM <- kmeans(as.matrix(pca_PM_coord[, 1:2]), 7, nstart=25, iter.max=1000)

# adds colums vector of cluster categorizations to both seasons totals and pca data
seasons_PM_data <- cbind(seasons_totals_data, k_mean_PM$cluster)
pca_PM_coord <- cbind(pca_PM_coord, k_mean_PM$cluster)
pca_PM_coord <- as.data.frame(pca_PM_coord)

# Another visual representation using factoextra package
fviz_cluster(
  k_mean_PM,
  data = pca_PM_coord[, 1:2],
  main = ''
)


### PER MINUTE DATA (SCALED) ##
seasons_PM_data_scaled <- scale(seasons_PM_data[,2:17])

#PCA on seasons totals
set.seed(235)
pca_PM_scaled <- PCA(seasons_PM_data_scaled)
plot(pca_PM_scaled, type = 'l')

#Extracting PCA Data Points
pca_PM_coord_scaled <- pca_PM_scaled$ind$coord
pca_PM_coord_scaled[is.na(pca_PM_coord_scaled)] <- 0

set.seed(123)
k_mean_PM_scaled <- kmeans(as.matrix(pca_PM_coord_scaled[, 1:2]), 7, nstart=25, iter.max=1000)

# adds colums vector of cluster categorizations to both seasons totals and pca data
seasons_PM_data_scaled <- cbind(seasons_PM_data_scaled, k_mean_PM_scaled$cluster)
pca_PM_coord_scaled <- cbind(pca_PM_coord_scaled, k_mean_PM_scaled$cluster)
pca_PM_coord_scaled <- as.data.frame(pca_PM_coord_scaled)

# Another visual representation using factoextra package
fviz_cluster(
  k_mean_PM_scaled,
  data = pca_PM_coord_scaled[, 1:2],
  main = ''
)

