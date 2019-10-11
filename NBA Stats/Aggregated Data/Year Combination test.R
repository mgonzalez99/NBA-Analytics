library(readxl)
library(dplyr)
setwd("~/Desktop/NBA Stats")

#Imports 2016-2017 NBA individual stats, removes null column
X2016_2017_Players <- read_excel("2016-2017/2016-2017 Nba players.xls")
X2016_2017_Players <- X2016_2017_Players[-c(1), ]

#Removes non-numeric numbers and percentage values
X2016_2017_Players <- select(X2016_2017_Players, -Rk, -(Pos:GS), -`FG%`, -`3P%`, -`2P%`, -`eFG%`, -`FT%`)

#Arranges Players by alphabetic order
X2016_2017_Players <- arrange(X2016_2017_Players, Player)


#Imports 2017-2018 NBA individual stats, removes null column
X2017_2018_Players <- read_excel("2017-2018/2017-2018 Players.xls")
X2017_2018_Players <- X2017_2018_Players[-c(1), ]

#Removes non-numeric numbers
X2017_2018_Players <- select(X2017_2018_Players, -Rk, -(Pos:GS), -`FG%`, -`3P%`, -`2P%`, -`eFG%`, -`FT%`)

#Arranges Players by alphabetic order
X2017_2018_Players <- arrange(X2017_2018_Players, Player)

# Player name corrections for consistency across seasons
X2017_2018_Players[11,][1] <- "Alex Abrines"
X2017_2018_Players[40,][1] <- "Boban Marjanovic"
X2017_2018_Players[44,][1] <- "Bojan Bogdanovic"
X2017_2018_Players[74,][1] <- "Cristiano Felicio"
X2017_2018_Players[90,][1] <- "Dario Saric"
X2017_2018_Players[97,][1] <- "Davis Bertans"
X2017_2018_Players[109,][1] <- "Dennis Schroder"
X2017_2018_Players[145,][1] <- "Ersan Ilyasova"
X2017_2018_Players[162,][1] <- "Goran Dragic"
X2017_2018_Players[236,][1] <- "Jonas Valanciunas"
X2017_2018_Players[243,][1] <- "Jose Calderon"
X2017_2018_Players[251,][1] <- "Juan Hernangomez"
X2017_2018_Players[258,][1] <- "Jusuf Nurkic"
X2017_2018_Players[278,][1] <- "Kristaps Porzingis"
X2017_2018_Players[310,][1] <- "Manu Ginobili"
X2017_2018_Players[342,][1] <- "Mirza Teletovic"
X2017_2018_Players[347,][1] <- "Nene Hilario"
X2017_2018_Players[354,][1] <- "Nikola Jokic"
X2017_2018_Players[355,][1] <- "Nikola Mirotic"
X2017_2018_Players[356,][1] <- "Nikola Vucevic"
X2017_2018_Players[362,][1] <- "Omer Asik"
X2017_2018_Players[414,][1] <- "Skal Labissiere"
X2017_2018_Players[437,][1] <- "Timothe Luwawu-Cabarrot"
X2017_2018_Players[439,][1] <- "Tomas Satoransky"
X2017_2018_Players[475,][1] <- "Willy Hernangomez"

#Imports 2018-2019 Individual data
X2018_2019_NBA_Players <- read_excel("2018-2019/2018-2019 NBA Individuals.xls")

#Removes non-numeric numbers and percentage values
X2018_2019_NBA_Players <- select(X2018_2019_NBA_Players, -Rk, -(Pos:GS), -`FG%`, -`3P%`, -`2P%`, -`eFG%`, -`FT%`)

#Arranges Players by alphabetic order
X2018_2019_NBA_Players <- arrange(X2018_2019_NBA_Players, Player)

# Player name corrections for consistency across seasons
X2018_2019_NBA_Players[36,][1] <- "Boban Marjanovic"
X2018_2019_NBA_Players[39,][1] <- "Bojan Bogdanovic"
X2018_2019_NBA_Players[92,][1] <- "Dario Saric"
X2018_2019_NBA_Players[97,][1] <- "Davis Bertans"
X2018_2019_NBA_Players[266,][1] <- "Jusuf Nurkic"
X2018_2019_NBA_Players[416,][1] <- "Skal Labissiere"
X2018_2019_NBA_Players[439,][1] <- "Timothe Luwawu-Cabarrot"

### Total Stats Aggregation ###
# This line combines the total data from the 2016-2017 season, the 2017-2018 season, and the 2018-2019 season
seasons_aggregate_data <- rbind(X2016_2017_Players, X2017_2018_Players, X2018_2019_NBA_Players)

#arranges player names in alphabetical order
seasons_aggregate_data <- seasons_aggregate_data[order(seasons_aggregate_data$Player),]

# Adds numeric totals for multiple seasons
for (i in 1:(dim(seasons_aggregate_data)[1]-1)){
  
  if (seasons_aggregate_data[i,] == seasons_aggregate_data[(i+1),] & seasons_aggregate_data[i,] == seasons_aggregate_data[(i+2),]) {
    seasons_aggregate_data[i,] <- cbind(seasons_aggregate_data[i,][,c(1)], (seasons_aggregate_data[i,][,c(2:19)] + seasons_aggregate_data[(i+1),][,c(2:19)] + seasons_aggregate_data[(i+2),][,c(2:19)]))
    seasons_aggregate_data <- seasons_aggregate_data[-c((i+1):(i+2)), ]
  }
  
  if (seasons_aggregate_data[i,] == seasons_aggregate_data[(i+1),]) {
    seasons_aggregate_data[i,] <- cbind(seasons_aggregate_data[i,][,c(1)], (seasons_aggregate_data[i,][,c(2:19)] + seasons_aggregate_data[(i+1),][,c(2:19)]))
    seasons_aggregate_data <- seasons_aggregate_data[-c((i+1):(i+2)), ]
  }
}

write.csv(seasons_aggregate_data, file = 'season_totals_data.csv')

#Converts Total Stats to Per Minute Stats
seasons_PM_data <- dplyr::mutate(seasons_aggregate_data, 
                                    PPM = seasons_aggregate_data$PTS/seasons_aggregate_data$`MP▼`,
                                    PFPM = seasons_aggregate_data$PF/seasons_aggregate_data$`MP▼`,
                                    TOVPM = seasons_aggregate_data$TOV/seasons_aggregate_data$`MP▼`,
                                    BLKPM = seasons_aggregate_data$BLK/seasons_aggregate_data$`MP▼`,
                                    STLPM = seasons_aggregate_data$STL/seasons_aggregate_data$`MP▼`,
                                    ASTPM = seasons_aggregate_data$AST/seasons_aggregate_data$`MP▼`,
                                    TRBPM = seasons_aggregate_data$TRB/seasons_aggregate_data$`MP▼`,
                                    DRBPM = seasons_aggregate_data$DRB/seasons_aggregate_data$`MP▼`,
                                    ORBPM = seasons_aggregate_data$ORB/seasons_aggregate_data$`MP▼`,
                                    FGPM = seasons_aggregate_data$FG/seasons_aggregate_data$`MP▼`,
                                    FGAPM = seasons_aggregate_data$FGA/seasons_aggregate_data$`MP▼`,
                                    PM3P = seasons_aggregate_data$`3P`/seasons_aggregate_data$`MP▼`,
                                    PM3PA = seasons_aggregate_data$`3PA`/seasons_aggregate_data$`MP▼`,
                                    PM2P = seasons_aggregate_data$`2P`/seasons_aggregate_data$`MP▼`,
                                    PM2PA = seasons_aggregate_data$`2PA`/seasons_aggregate_data$`MP▼`)

seasons_PM_data <- dplyr::select(seasons_PM_data, -(FG:`2PA`), -(ORB:PTS))
seasons_PM_data <- dplyr::mutate(seasons_PM_data, `FT%` = FT/FTA)
seasons_PM_data <- dplyr::select(seasons_PM_data, -(FT:FTA))
seasons_PM_data[is.na(seasons_PM_data)] <- 0

write.csv(seasons_PM_data, file = 'seasons_PM_data.csv')



