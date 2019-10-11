library(readxl)
library(dplyr)

#Imports data frame from directory
X2017_2018_Players <- read_excel("2017-2018/2017-2018 Players.xls")
View(X2017_2018_Players)

#Deletes a Row
X2017_2018_Players <- X2017_2018_Players[-c(1), ]

#Checks if any player names are duplicated
mean(duplicated(X2017_2018_Players$Player))

#Converts Total Stats to Per Minute Stats
X2017_2018_Players <- dplyr::mutate(X2017_2018_Players, 
                                    PPM = X2017_2018_Players$PTS/X2017_2018_Players$`MP▼`,
                                    PFPM = X2017_2018_Players$PF/X2017_2018_Players$`MP▼`,
                                    TOVPM = X2017_2018_Players$TOV/X2017_2018_Players$`MP▼`,
                                    BLKPM = X2017_2018_Players$BLK/X2017_2018_Players$`MP▼`,
                                    STLPM = X2017_2018_Players$STL/X2017_2018_Players$`MP▼`,
                                    ASTPM = X2017_2018_Players$AST/X2017_2018_Players$`MP▼`,
                                    TRBPM = X2017_2018_Players$TRB/X2017_2018_Players$`MP▼`,
                                    DRBPM = X2017_2018_Players$DRB/X2017_2018_Players$`MP▼`,
                                    ORBPM = X2017_2018_Players$ORB/X2017_2018_Players$`MP▼`,
                                    FGBPM = X2017_2018_Players$FG/X2017_2018_Players$`MP▼`)





