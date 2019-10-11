library(readxl)
library(dplyr)
setwd("~/Desktop/NBA Stats")

#Imports 2016-2017 lineup data
Lineups_NBA_Website_2016_2017 <- read_excel("2016-2017/Lineups_NBA_Website_2016-2017.xlsx")
Lineups_NBA_Website_2016_2017 <- select(Lineups_NBA_Website_2016_2017, -TEAM,-GP, -`FG%`, -`3P%`,-`FT%`)
Lineups_NBA_Website_2016_2017 <- Lineups_NBA_Website_2016_2017 [-c(357), ]

#Imports 2017-2018 lineup data
Lineup_NBA_Website_2017_2018 <- read_excel("2017-2018/Lineup_NBA_Website_2017_2018.xlsx")
Lineup_NBA_Website_2017_2018 <- select(Lineup_NBA_Website_2017_2018, -TEAM,-GP, -`FG%`, -`3P%`,-`FT%`)
Lineup_NBA_Website_2017_2018 <- Lineup_NBA_Website_2017_2018[-c(335), ]

#Imports 2018-2019 lineup data
Lineup_NBA_Website_2018_2019 <- read_excel("2018-2019/Lineup_NBA_Website_2018_2019.xlsx")
Lineup_NBA_Website_2018_2019 <- select(Lineup_NBA_Website_2018_2019, -TEAM,-GP, -`FG%`, -`3P%`,-`FT%`)
Lineup_NBA_Website_2018_2019 <- Lineup_NBA_Website_2018_2019 [-c(337), ]

#Combines 2017-2018 and 2018-2019 seasons
seasons_totals_lineup <- rbind(Lineups_NBA_Website_2016_2017, Lineup_NBA_Website_2017_2018, Lineup_NBA_Website_2018_2019)

#puts data set in alphabetical order
seasons_totals_lineup <- arrange(seasons_totals_lineup, LINEUPS)

# Combines totals data for equivalent lineups
for (i in 1:(dim(seasons_totals_lineup)[1]-1)){

  if (seasons_totals_lineup[i,] == seasons_totals_lineup[(i+1),] & seasons_totals_lineup[i,] == seasons_totals_lineup[(i+2),]) {
    seasons_totals_lineup[i,] <- cbind(seasons_totals_lineup[i,][,c(1)], (seasons_totals_lineup[i,][,c(2:20)] + seasons_totals_lineup[(i+1),][,c(2:20)] + seasons_totals_lineup[(i+2),][,c(2:20)]))
    seasons_totals_lineup <- seasons_totals_lineup[-c((i+1):(i+2)), ]
  }

  if (seasons_totals_lineup[i,] == seasons_totals_lineup[(i+1),]) {
    seasons_totals_lineup[i,] <- cbind(seasons_totals_lineup[i,][,c(1)], (seasons_totals_lineup[i,][,c(2:20)] + seasons_totals_lineup[(i+1),][,c(2:20)]))
    seasons_totals_lineup <- seasons_totals_lineup[-c(i+1), ]
  }
}

write.csv(seasons_totals_lineup, file = 'seasons_totals_lineup.csv')


#Converts Total Stats to Per Minute Stats
seasons_PM_data <- dplyr::mutate(seasons_totals_lineup, 
                                 PPM = seasons_totals_lineup$PTS/seasons_totals_lineup$MIN,
                                 PFPM = seasons_totals_lineup$PF/seasons_totals_lineup$MIN,
                                 TOVPM = seasons_totals_lineup$TOV/seasons_totals_lineup$MIN,
                                 BLKPM = seasons_totals_lineup$BLK/seasons_totals_lineup$MIN,
                                 STLPM = seasons_totals_lineup$STL/seasons_totals_lineup$MIN,
                                 ASTPM = seasons_totals_lineup$AST/seasons_totals_lineup$MIN,
                                 REBPM = seasons_totals_lineup$REB/seasons_totals_lineup$MIN,
                                 DRBPM = seasons_totals_lineup$DREB/seasons_totals_lineup$MIN,
                                 ORBPM = seasons_totals_lineup$OREB/seasons_totals_lineup$MIN,
                                 FGPM = seasons_totals_lineup$FGM/seasons_totals_lineup$MIN,
                                 FGAPM = seasons_totals_lineup$FGA/seasons_totals_lineup$MIN,
                                 PM3P = seasons_totals_lineup$`3PM`/seasons_totals_lineup$MIN,
                                 PM3PA = seasons_totals_lineup$`3PA`/seasons_totals_lineup$MIN,
                                 PFDPM = seasons_totals_lineup$PFD/seasons_totals_lineup$MIN,
                                 plus_minusPM = seasons_totals_lineup$`+/-`/seasons_totals_lineup$MIN)

seasons_PM_data <- dplyr::select(seasons_PM_data, -(MIN:`3PA`), -(OREB:`+/-`))
seasons_PM_data <- dplyr::mutate(seasons_PM_data, `FT%` = FTM/FTA)
seasons_PM_data <- dplyr::select(seasons_PM_data, -(FTM:FTA))
seasons_PM_data[is.na(seasons_PM_data)] <- 0

write.csv(seasons_PM_data, file = 'seasons_PM_lineup_data.csv')
