library(dplyr)
library(readr)
library(stringr)
setwd("~/Desktop/NBA Stats")

#### LINEUP DATA SETUP ###
# load the full lineup stats (training)
test_for_lineups_full <- read_csv("Regression/test_for_lineups_full.csv")
train_for_lineups_full <- read_csv("Regression/train_for_lineups_full.csv")

# Loads individual PM data
seasons_PM_data <- read_csv("Aggregated Data/seasons_PM_data.csv")

# Splits the PM individual data from  
player_names <- strsplit(seasons_PM_data$Player, " ")


#Creates data frame 'corrected_names' so that player names matches 
corrected_names <- data.frame("name" = 1:(dim(seasons_PM_data)[1]))
for (i in 1:dim(seasons_PM_data)[1]) {
  corrected_names$name[i]  <- paste0( ".", str_sub(player_names[[i]][1], 1,1),".", " ", player_names[[i]][2])

}


corrected_names[19,] <- '.A. Jefferson_1'
corrected_names[68,] <- '.B. Brown_1'
corrected_names[121,] <- '.D. Lee_1'
corrected_names[143,] <- '.D. Jones_1'
corrected_names[161,] <- '.D. Green_1'
corrected_names[163,] <- '.D. Robinson_1'
corrected_names[243,] <- '.J. Jones_1'
corrected_names[259,] <- '.J. Smith_1'
corrected_names[262,] <- '.J. Evans_1'
corrected_names[267,] <- '.J. Green_1'
corrected_names[269,] <- '.J. Jones_2'
corrected_names[273,] <- '.J. Grant_1'
corrected_names[282,] <- '.J. Johnson_1'
corrected_names[283,] <- '.J. Young_1'
corrected_names[302,] <- '.J. Crawford_1'
corrected_names[308,] <- '.J. Jackson_1'
corrected_names[319,] <- '.J. Holiday_1'
corrected_names[320,] <- '.J. Jackson_2'
corrected_names[397,] <- '.M. Morris_1'
corrected_names[403,] <- '.M. Plumlee_1'
corrected_names[410,] <- '.M. Beasley_1'
corrected_names[417,] <- '.M. Miller_1'
corrected_names[420,] <- '.M. Bridges_1'
corrected_names[421,] <- '.M. Plumlee_2'
corrected_names[427,] <- '.M. Morris_2'
corrected_names[523,] <- '.S. Curry_1'
corrected_names[555,] <- '.T. Young_1'

View(cbind(seasons_PM_data$Player, corrected_names))


# adds adjusted player names to original player data frame, removes X1 
seasons_PM_data <- cbind(corrected_names, seasons_PM_data)
seasons_PM_data <- seasons_PM_data %>% select(-X1)

#Divides lineup into individuals
player_from_lineup <- strsplit(train_for_lineups_full$LINEUPS, ",")

#
lineup_1 <- rbind(seasons_PM_data %>% filter(name == player_from_lineup[[1]][1]),
                 seasons_PM_data %>% filter(name == str_sub(player_from_lineup[[1]][2], 2, nchar(player_from_lineup[[1]][2]))),
                 seasons_PM_data %>% filter(name == str_sub(player_from_lineup[[1]][3], 2, nchar(player_from_lineup[[1]][3]))),
                 seasons_PM_data %>% filter(name == str_sub(player_from_lineup[[1]][4], 2, nchar(player_from_lineup[[1]][4]))),
                 seasons_PM_data %>% filter(name == str_sub(player_from_lineup[[1]][5], 2, nchar(player_from_lineup[[1]][5]))))

lineup_1 <- lineup_1 %>% select(-name, -Player, -`MP▼`)

lineup_1_test <- cbind(lineup_1[1] %>% arrange(PPM) %>% t,
                      lineup_1[2] %>% arrange(PFPM) %>% t,
                      lineup_1[3] %>% arrange(TOVPM) %>% t,
                      lineup_1[4] %>% arrange(BLKPM) %>% t,
                      lineup_1[5] %>% arrange(STLPM) %>% t,
                      lineup_1[6] %>% arrange(ASTPM) %>% t,
                      lineup_1[7] %>% arrange(TRBPM) %>% t,
                      lineup_1[8] %>% arrange(DRBPM) %>% t,
                      lineup_1[9] %>% arrange(ORBPM) %>% t,
                      lineup_1[10] %>% arrange(FGPM) %>% t,
                      lineup_1[11] %>% arrange(FGAPM) %>% t,
                      lineup_1[12] %>% arrange(PM3P) %>% t,
                      lineup_1[13] %>% arrange(PM3PA) %>% t,
                      lineup_1[14] %>% arrange(PM2P) %>% t,
                      lineup_1[15] %>% arrange(PM2PA) %>% t,
                      lineup_1[16] %>% arrange(lineup_1$`FT%`) %>% t)

colnames(lineup_1_test) <- c("PPM_1", "PPM_2", "PPM_3", "PPM_4", "PPM_5",
                            "PFPM_1", "PFPM_2", "PFPM_3", "PFPM_4", "PFPM_5",
                            "TOVPM_1", "TOVPM_2", "TOVPM_3", "TOVPM_4", "TOVPM_5",
                            "BLKPM_1", "BLKPM_2", "BLKPM_3", "BLKPM_4", "BLKPM_5",
                            "STLPM_1", "STLPM_2", "STLPM_3", "STLPM_4", "STLPM_5",
                            "ASTPM_1", "ASTPM_2", "ASTPM_3", "ASTPM_4", "ASTPM_5",
                            "TRBPM_1", "TRBPM_2", "TRBPM_3", "TRBPM_4", "TRBPM_5",
                            "DRBPM_1", "DRBPM_2", "DRBPM_3", "DRBPM_4", "DRBPM_5",
                            "ORBPM_1", "ORBPM_2", "ORBPM_3", "ORBPM_4", "ORBPM_5",
                            "FGPM_1", "FGPM_2", "FGPM_3", "FGPM_4", "FGPM_5",
                            "FGAPM_1", "FGAPM_2", "FGAPM_3", "FGAPM_4", "FGAPM_5",
                            "PM3P_1", "PM3P_2", "PM3P_3", "PM3P_4", "PM3P_5",
                            "PM3PA_1", "PM3PA_2", "PM3PA_3", "PM3PA_4", "PM3PA_5",
                            "PM2P_1", "PM2P_2", "PM2P_3", "PM2P_4", "PM2P_5",
                            "PM2PA_1", "PM2PA_2", "PM2PA_3", "PM2PA_4", "PM2PA_5",
                            "FT%_1", "FT%_2", "FT%_3", "FT%_4", "FT%_5")

# for (i in 1: dim(train_for_lineups_full)[1]) {
lineup_run <- rbind(seasons_PM_data %>% filter(name == player_from_lineup[[i]][1]),
                  seasons_PM_data %>% filter(name == str_sub(player_from_lineup[[i]][2], 2, nchar(player_from_lineup[[i]][2]))),
                  seasons_PM_data %>% filter(name == str_sub(player_from_lineup[[i]][3], 2, nchar(player_from_lineup[[i]][3]))),
                  seasons_PM_data %>% filter(name == str_sub(player_from_lineup[[i]][4], 2, nchar(player_from_lineup[[i]][4]))),
                  seasons_PM_data %>% filter(name == str_sub(player_from_lineup[[i]][5], 2, nchar(player_from_lineup[[i]][5]))))

lineup_run <- lineup_run %>% select(-name, -Player, -`MP▼`)

lineup_run_test <- cbind(lineup_run[1] %>% arrange(PPM) %>% t,
                       lineup_run[2] %>% arrange(PFPM) %>% t,
                       lineup_run[3] %>% arrange(TOVPM) %>% t,
                       lineup_run[4] %>% arrange(BLKPM) %>% t,
                       lineup_run[5] %>% arrange(STLPM) %>% t,
                       lineup_run[6] %>% arrange(ASTPM) %>% t,
                       lineup_run[7] %>% arrange(TRBPM) %>% t,
                       lineup_run[8] %>% arrange(DRBPM) %>% t,
                       lineup_run[9] %>% arrange(ORBPM) %>% t,
                       lineup_run[10] %>% arrange(FGPM) %>% t,
                       lineup_run[11] %>% arrange(FGAPM) %>% t,
                       lineup_run[12] %>% arrange(PM3P) %>% t,
                       lineup_run[13] %>% arrange(PM3PA) %>% t,
                       lineup_run[14] %>% arrange(PM2P) %>% t,
                       lineup_run[15] %>% arrange(PM2PA) %>% t,
                       lineup_run[16] %>% arrange(lineup_run$`FT%`) %>% t)


# colnames(lineup_run_test) <- c("PPM_1", "PPM_2", "PPM_3", "PPM_4", "PPM_5",
#                              "PFPM_1", "PFPM_2", "PFPM_3", "PFPM_4", "PFPM_5",
#                              "TOVPM_1", "TOVPM_2", "TOVPM_3", "TOVPM_4", "TOVPM_5",
#                              "BLKPM_1", "BLKPM_2", "BLKPM_3", "BLKPM_4", "BLKPM_5",
#                              "STLPM_1", "STLPM_2", "STLPM_3", "STLPM_4", "STLPM_5",
#                              "ASTPM_1", "ASTPM_2", "ASTPM_3", "ASTPM_4", "ASTPM_5",
#                              "TRBPM_1", "TRBPM_2", "TRBPM_3", "TRBPM_4", "TRBPM_5",
#                              "DRBPM_1", "DRBPM_2", "DRBPM_3", "DRBPM_4", "DRBPM_5",
#                              "ORBPM_1", "ORBPM_2", "ORBPM_3", "ORBPM_4", "ORBPM_5",
#                              "FGPM_1", "FGPM_2", "FGPM_3", "FGPM_4", "FGPM_5",
#                              "FGAPM_1", "FGAPM_2", "FGAPM_3", "FGAPM_4", "FGAPM_5",
#                              "PM3P_1", "PM3P_2", "PM3P_3", "PM3P_4", "PM3P_5",
#                              "PM3PA_1", "PM3PA_2", "PM3PA_3", "PM3PA_4", "PM3PA_5",
#                              "PM2P_1", "PM2P_2", "PM2P_3", "PM2P_4", "PM2P_5",
#                              "PM2PA_1", "PM2PA_2", "PM2PA_3", "PM2PA_4", "PM2PA_5",
#                              "FT%_1", "FT%_2", "FT%_3", "FT%_4", "FT%_5")

# lineup_1_test <- rbind(lineup_1_test, lineup_run_test)

}
