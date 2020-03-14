# setting working directory
setwd('C:/Users/Juan Solorio/Desktop/DataScience/Grad School/Classes/Data 598C/Data-598C-wi2020/')

# Including necessary libraries 
library(tidyverse)
library(rvest)
library(survival)
library(survminer)
library(caTools)
library(ROCR)
library(ggfortify)
library(ggExtra)

#   Loading the files into individual data frames
S2014 <- read.csv('pbp-2014.csv')
S2015 <- read.csv('pbp-2015.csv')
S2016 <- read.csv('pbp-2016.csv')
S2017 <- read.csv('pbp-2017.csv')
S2018 <- read.csv('pbp-2018.csv')
S2019 <- read.csv('pbp-2019.csv')

# Joining data to make the preparation process quicker
data_train <- rbind(S2014, S2015, S2016, S2017, S2018, S2019)

# columns to drop 'X', 'X.1', 'X,2', 'X.3', 'Challenger'
data_train <- data_train[, -c(11, 13, 17, 18, 31)]

## Getting the ids for games
gameids_train <- unique(data_train$GameId)

teams_in_game <- function(df, column1, defense, offense, gameids) {
  # Function to replace the NAs of OffenseTeams for the offense team opposite to Defense team in a game
  for (gameid in gameids) {
    #print(gameid)
    teams <- unique(df[df[column1] == gameid,][,defense])
    #print(teams)
    tempdf <- df[(df[column1] == gameid & is.na(df[offense])),]
    #print(nrow(tempdf[column1]))
    for (i in 1:nrow(df[(df[column1] == gameid & is.na(df[offense])),][column1])) {
      ifelse(df[(df[column1] == gameid & is.na(df[offense])),][i,defense] == teams[1],df[(df[column1] == gameid & is.na(df[offense])),][i,offense] <- teams[2],df[(df[column1] == gameid & is.na(df[offense])),][i,offense] <- teams[1])
      #df[(df[column1] == gameid & is.na(df[offense])),] <- tempdf
    }
    
  }
  return(df)
  
}


data_train <- teams_in_game(data_train,column1 = 'GameId',
                            defense = 'DefenseTeam', 
                            offense = 'OffenseTeam',
                            gameids = gameids_test)

# I will first get he column number for each of the columns to convert to a factor, and make to a list
# I've gather that the columns to remain as factors are by number
# 1 , 2, 3, 6, 7, 8, 11, 13, 17 : 40
factor_columns <- c(1 , 2, 3, 6, 7, 8, 11, 13, 17 : 40)

data_test[factor_columns] <- lapply(data_test[factor_columns], factor)

# Now to convert the 'Description' from factor to character and Gamedate to time
data_train$Description <- as.character(data_train$Description)
data_train$GameDate <- as.Date(data_train$GameDate, format = '%Y-%m-%d')

# Ordering the tables by Ascending 'GameId', 'Quarter' and decensing 'Minute', 'Second' 
data_test <- data_test[order(data_test$GameId, data_test$Quarter, -data_test$Minute, -data_test$Second),]

#####################################################################
add.week.of.season <- function(df_in) {
  #------------------------------------------------------------
  # Funtion takes in either data_train or data_test,
  # looks at the individual season and adds a week for each game
  # Returns a dataframe with same dimensions as df provided plus added 'Week' col
  #------------------------------------------------------------
  tempdf1 <- df_in[,]     #temporary df copying the df provided
  seasons <- unique(df_in$SeasonYear)     # getting the individual season in df
  tempdf1$Week <- NA      #adding in the 'Week' column to the temp df
  tempdf2 <- data.frame() #setting 2nd tempdf for appending through rbind
  
  #   3 level loop to go through seasons, days in season, and day in group 
  #   of 3 days composing the NFL week (Thu, Sun, Mon) 
  for(season in seasons){
    dfSeason <- tempdf1[tempdf1$SeasonYear == season,]
    dates <- unique(dfSeason$GameDate)
    week <- 1
    
    # NFL season is composed of 17 weeks, each of 3 different game days
    # Divided the number of game days by 3 to account for the 3 game days
    # added 1 to capture the last week of the season, as I pass 'n' as an int
    # to get the 3 day vector from dates
    for(n in 1:(length(dates)/3+1)){
      # contiditional to break the loop if we reach week 18
      if (n == 18) {
        break
      }
      # subtract 1 to reindex for the purpose of the sequence  of dates
      n <- n-1
      days <- c(dates[(3*n+1):(3*n+3)])
      days <- days[!is.na(days)]
      
      # Loop through the 3 indivual week days to give the correct week to each
      for(i in days){
        df1 <- tempdf1[tempdf1$GameDate == i,]
        df1$Week <- week
        tempdf2 <- rbind(tempdf2, df1)
      }
      week <- week +1
    }
  }
  return(tempdf2)
}

data_train_week_added <- add.week.of.season(data_train)

###########################################################3333
# Copy of most current data_train df to add new column
data_train_playsuccess_add <- data_train_week_added[,]

# added column to describe if play gained yards by checking for trues in the 
# case that column 'Yards' >0  and that PenaltyTeam equals DefenseTeam
data_train_playsuccess_add$PlaySuccess <- as.numeric(data_train_playsuccess_add$Yards > 0 | (data_train_playsuccess_add$DefenseTeam == as.character(data_train_playsuccess_add$PenaltyTeam) & !is.na(data_train_playsuccess_add$PenaltyTeam)))

# Copy of most current data_train df to add new column
data_train_yrstotd_add <- data_train_playsuccess_add[,]
# Column to count how far play is from touchdown in yards by subtracting the 
# 'Yards' column from 100
data_train_yrstotd_add$YarsToTD <- 100 - as.numeric(data_train_yrstotd_add$YardLineFixed)

# Adding columns for field goal and extra point success
data_train_yrstotd_add$IsFieldGoalGood <- as.numeric(grepl('FIELD GOAL IS GOOD',data_train_yrstotd_add$Description, fixed = TRUE))

data_train_yrstotd_add$IsExtraPointGood <- as.numeric(grepl('EXTRA POINT IS GOOD',data_train_yrstotd_add$Description, fixed = TRUE))

data_train_yrstotd_add$matchid <- paste(data_train_yrstotd_add$GameDate, data_train_yrstotd_add$DefenseTeam, sep = '')
#####################################################
# Adding the number of plays in the drive
data_train_df <- data_train_yrstotd_add[,]

# Adding columns to df
data_train_df$NewDrive <- NA
data_train_df$PlayNumInDrive <- NA
data_train_df$YardsInDriveOff <- NA

# Check that OffenseTeam is NA from TIMEOUT plays
data_train_df %>% filter(PlayType == 'TIMEOUT') %>% select(GameId,OffenseTeam, DefenseTeam, Down, ToGo, PlayType)

# Dropping the Timeout and NA PlayTypes
data_train_dropTO <- data_train_df[(data_train_df$PlayType != 'TIMEOUT' & !is.na(data_train_df$PlayType)), ]
check_df_nulls(data_train_dropTO)

fill_DrivesCol <- function(df_in) {
  games <- unique(df_in$GameId)
  tempFilldf <- data.frame()
  for (game in games) {
    tempgamedf <- df_in[df_in$GameId == game,]
    tempgamedf$NewDrive <- as.numeric((lag(x=tempgamedf$DefenseTeam, n=1) != tempgamedf$DefenseTeam))
    teams <- unique(tempgamedf$DefenseTeam)
    if(is.na(tempgamedf$OffenseTeam)){
      tempgamedf[tempgamedf$DefenseTeam == teams[1], "OffenseTeam"] <- teams[2]
      tempgamedf[tempgamedf$DefenseTeam == teams[2], "OffenseTeam"] <- teams[1]
    }
    for (play in 1:dim(tempgamedf)[1]) {
      
      if(is.na(tempgamedf$NewDrive[play])){
        next
      }
      else if((tempgamedf$NewDrive[play] == 1)){
        tempgamedf$PlayNumInDrive[play] <- 1
        tempgamedf$YardsInDriveOff[play] <- 0
      }
      else{
        tempgamedf$PlayNumInDrive[play] <- tempgamedf$PlayNumInDrive[play - 1] + 1
        tempgamedf$YardsInDriveOff[play] <- tempgamedf$YarsToTD[play - 1] -  tempgamedf$YarsToTD[play]
      }
    }
    tempFilldf <- rbind(tempFilldf, tempgamedf)
    
  }
  return(tempFilldf) 
}

data_train_drives_df <- suppressMessages(fill_DrivesCol(data_train_dropTO))

###################################################################
# team list connecting short name to long name of teams
teamstable <- list( sname= c('ARI', 'ATL', 'BAL', 'BUF', 'CAR', 'CHI', 'CIN', 'CLE',
                             'DAL', 'DEN', 'DET', 'GB',  'HOU', 'IND', 'JAX', 'KC',
                             'LAC', 'LA', 'MIA',  'MIN', 'NE', 'NO', 'NYG', 'NYJ', 'OAK',
                             'PHI', 'PIT', 'SD',  'SF', 'SEA', 'LA', 'TB', 'TEN', 'WAS'),
                    LongName = c( 'Arizona Cardinals',    'Atlanta Falcons',      'Baltimore Ravens',    
                                  'Buffalo Bills',        'Carolina Panthers',    'Chicago Bears',       
                                  'Cincinnati Bengals',   'Cleveland Browns',     'Dallas Cowboys',      
                                  'Denver Broncos',       'Detroit Lions',        'Green Bay Packers',   
                                  'Houston Texans',       'Indianapolis Colts',   'Jacksonville Jaguars',
                                  'Kansas City Chiefs',   'Los Angeles Chargers', 'Los Angeles Rams',    
                                  'Miami Dolphins',       'Minnesota Vikings',    'New England Patriots',
                                  'New Orleans Saints',   'New York Giants',      'New York Jets',       
                                  'Oakland Raiders',      'Philadelphia Eagles',  'Pittsburgh Steelers', 
                                  'San Diego Chargers',   'San Francisco 49ers',  'Seattle Seahawks',    
                                  'St. Louis Rams',       'Tampa Bay Buccaneers', 'Tennessee Titans',    
                                  'Washington Redskins' ))
# setting up the needed requirements to scrap pro-football website to obtain the season info for each game [the season, the week, home vs away team, scores] using the library rvest
baseurl <- "https://www.pro-football-reference.com/years/"
seasontable <- data.frame()
GameWeek <- c()
GameDate <- c()
HomeTeam <- c()
AwayTeam <- c()
HomeScore <- c()
AwayScore <- c()
Season <- c()
GameNum <- c()
HTSName <- c()
ATSName <- c()
for (year in c(2014:2018)) {
  for(week in c(1:17)){
    url <- paste(baseurl,year,'/week_',week,'.htm', sep = '')
    dataraw <- url %>%
      html() %>%
      html_nodes(xpath = '//table[@class="teams"]') %>%
      html_table()
    for (game in 1:length(dataraw)) {
      Season <- c(Season, year)
      GameWeek <- c(GameWeek, week)
      GameNum <- c(GameNum, game)
      GameDate <- c(GameDate, dataraw[[game]][[1]][[1]])
      HomeTeam <- c(HomeTeam, dataraw[[game]][[1]][[3]])
      AwayTeam <- c(AwayTeam, dataraw[[game]][[1]][[2]])
      HomeScore <- c(HomeScore, dataraw[[game]][[2]][[3]])
      AwayScore <- c(AwayScore, dataraw[[game]][[2]][[2]])
      HTSName <- c(HTSName, teamstable$sname[teamstable$LongName == dataraw[[game]][[1]][[3]]])
      ATSName <- c(ATSName, teamstable$sname[teamstable$LongName == dataraw[[game]][[1]][[2]]])
    }
    seasontable <- data.frame(Season,GameDate, GameNum, GameWeek,AwayTeam, HomeTeam, ATSName, HTSName, AwayScore, HomeScore)
  }
}

# data frame for the over all games and season data to later join
seasontable$GameDate <- as.Date(as.character(seasontable$GameDate), format = "%b %d, %Y")
seasontable$matchid1 <- paste(seasontable$GameDate,seasontable$ATSName, sep = '')
seasontable$matchid2 <- paste(seasontable$GameDate,seasontable$HTSName, sep = '')
seasontable$AwayScore <- as.numeric(seasontable$AwayScore)
seasontable$HomeScore <- as.numeric(seasontable$HomeScore)

# looping through the main dataframe to add season table info to the games
add_season_info_fx <- function(df_in1, df_in2) {
  df1 <- df_in1[,]
  dfseasons <- df_in2[,]
  tempfilldf <- data.frame()
  
  matchids <- unique(dfseasons$matchid1)
  for (match in matchids) {
    dfseasons1 <- dfseasons[dfseasons$matchid1 == match,c('HTSName', 'AwayScore', 'HomeScore')]
    tempgameid <- unique(df1[df1$matchid == match,]$GameId)
    
    df2 <- df1[df1$GameId == tempgameid,]
    df2$HomeTeam <- rep(dfseasons1$HTSName,nrow(df2))
    df2$AwayTeamScore <- rep(dfseasons1$AwayScore,nrow(df2))
    df2$HomeTeamScore <- rep(dfseasons1$HomeScore,nrow(df2))
    
    tempfilldf <- rbind(tempfilldf, df2)
    
  }
  
  return(tempfilldf)
}

data_train1 <- data_train_drives_df[,]
tempdf <- add_season_info_fx(data_train1, seasontable)

data_train_add_win <- tempdf[,]

data_train_add_win$IsOffHomeTeam <- (ifelse((data_train_add_win$OffenseTeam == data_train_add_win$HomeTeam),'Home','Away'))

data_train_add_win$GameVictory <- (ifelse(data_train_add_win$HomeTeamScore >= data_train_add_win$AwayTeamScore, 'Home', 'Away'))

data_train_add_win$OffDriveOver <- as.numeric(lead(data_train_add_win$NewDrive == 1, n = 1))

data_train_model <- data_train_add_win[,]

data_train.notna <- data_train_model[!is.na(data_train_model$PlayNumInDrive),]

save(data_train.notna, file = "data_train_FeatEng.RData")
