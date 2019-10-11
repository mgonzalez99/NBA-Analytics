library(readxl)
library(dplyr)
setwd("~/Desktop/NBA Stats")

#Imports 2015-2016 lineup data
Lineup_NBA_Website_2015_2016 <- read_excel("2015-2016/Lineup_NBA_Website_2015_2016.xlsx")
Lineup_NBA_Website_2015_2016 <- select(Lineup_NBA_Website_2015_2016, -TEAM,-GP, -`FG%`, -`3P%`,-`FT%`)

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
complete_seasons_totals_lineup <- rbind(Lineup_NBA_Website_2015_2016, Lineups_NBA_Website_2016_2017, Lineup_NBA_Website_2017_2018, Lineup_NBA_Website_2018_2019)

#puts data set in alphabetical order
complete_seasons_totals_lineup <- arrange(complete_seasons_totals_lineup, LINEUPS)

# Combines totals data for equivalent lineups
for (i in 1:(dim(complete_seasons_totals_lineup)[1]-1)){
  
  if (complete_seasons_totals_lineup[i,] == complete_seasons_totals_lineup[(i+1),] & complete_seasons_totals_lineup[i,] == complete_seasons_totals_lineup[(i+2),]) {
    complete_seasons_totals_lineup[i,] <- cbind(complete_seasons_totals_lineup[i,][,c(1)], (complete_seasons_totals_lineup[i,][,c(2:20)] + complete_seasons_totals_lineup[(i+1),][,c(2:20)] + complete_seasons_totals_lineup[(i+2),][,c(2:20)]))
    complete_seasons_totals_lineup <- complete_seasons_totals_lineup[-c((i+1):(i+2)), ]
  }
  
  if (complete_seasons_totals_lineup[i,] == complete_seasons_totals_lineup[(i+1),] & complete_seasons_totals_lineup[i,] == complete_seasons_totals_lineup[(i+2),] & complete_seasons_totals_lineup[i,] == complete_seasons_totals_lineup[(i+3),]) {
    complete_seasons_totals_lineup[i,] <- cbind(complete_seasons_totals_lineup[i,][,c(1)], (complete_seasons_totals_lineup[i,][,c(2:20)] + complete_seasons_totals_lineup[(i+1),][,c(2:20)] + complete_seasons_totals_lineup[(i+2),][,c(2:20)] + complete_seasons_totals_lineup[(i+3),][,c(2:20)]))
    complete_seasons_totals_lineup <- complete_seasons_totals_lineup[-c((i+1):(i+3)), ]
  }
  
  if (complete_seasons_totals_lineup[i,] == complete_seasons_totals_lineup[(i+1),]) {
    complete_seasons_totals_lineup[i,] <- cbind(complete_seasons_totals_lineup[i,][,c(1)], (complete_seasons_totals_lineup[i,][,c(2:20)] + complete_seasons_totals_lineup[(i+1),][,c(2:20)]))
    complete_seasons_totals_lineup <- complete_seasons_totals_lineup[-c(i+1), ]
  }
}

write.csv(complete_seasons_totals_lineup, file = 'complete_seasons_totals_lineup.csv')


#Converts Total Stats to Per Minute Stats
complete_seasons_PM_lineup <- dplyr::mutate(complete_seasons_totals_lineup, 
                                 PPM = complete_seasons_totals_lineup$PTS/complete_seasons_totals_lineup$MIN,
                                 PFPM = complete_seasons_totals_lineup$PF/complete_seasons_totals_lineup$MIN,
                                 TOVPM = complete_seasons_totals_lineup$TOV/complete_seasons_totals_lineup$MIN,
                                 BLKPM = complete_seasons_totals_lineup$BLK/complete_seasons_totals_lineup$MIN,
                                 STLPM = complete_seasons_totals_lineup$STL/complete_seasons_totals_lineup$MIN,
                                 ASTPM = complete_seasons_totals_lineup$AST/complete_seasons_totals_lineup$MIN,
                                 REBPM = complete_seasons_totals_lineup$REB/complete_seasons_totals_lineup$MIN,
                                 DRBPM = complete_seasons_totals_lineup$DREB/complete_seasons_totals_lineup$MIN,
                                 ORBPM = complete_seasons_totals_lineup$OREB/complete_seasons_totals_lineup$MIN,
                                 FGPM = complete_seasons_totals_lineup$FGM/complete_seasons_totals_lineup$MIN,
                                 FGAPM = complete_seasons_totals_lineup$FGA/complete_seasons_totals_lineup$MIN,
                                 PM3P = complete_seasons_totals_lineup$`3PM`/complete_seasons_totals_lineup$MIN,
                                 PM3PA = complete_seasons_totals_lineup$`3PA`/complete_seasons_totals_lineup$MIN,
                                 PFDPM = complete_seasons_totals_lineup$PFD/complete_seasons_totals_lineup$MIN,
                                 plus_minusPM = complete_seasons_totals_lineup$`+/-`/complete_seasons_totals_lineup$MIN)

complete_seasons_PM_lineup <- dplyr::select(complete_seasons_PM_lineup, -(MIN:`3PA`), -(OREB:`+/-`))
complete_seasons_PM_lineup <- dplyr::mutate(complete_seasons_PM_lineup, `FT%` = FTM/FTA)
complete_seasons_PM_lineup <- dplyr::select(complete_seasons_PM_lineup, -(FTM:FTA))
complete_seasons_PM_lineup[is.na(complete_seasons_PM_lineup)] <- 0

write.csv(complete_seasons_PM_lineup, file = 'complete_seasons_PM_lineup_data.csv')
