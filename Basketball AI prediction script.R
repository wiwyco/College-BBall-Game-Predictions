install.packages("devtools")
library(devtools)
install_github("meysubb/collegeballR")
library(collegeballR)
require(neuralnet)
require(caret)

ref.sport <- sport_df

test <- team_stats(721, 2018, "MBB", by = "SEASON")

test2 <- team_schedule(813, 2019, "MBB")

years <- c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019)

ref.team <- team_mapping(2010, "Men's Basketball")


#team df instantiation
{
  teams <- data.frame(Name = as.character(ref.team[,2]),
                      ID = as.numeric(ref.team[,1]),
                      H.G = rep(c(0), each = nrow(ref.team)),
                      H.MP = rep(c(0), each = nrow(ref.team)),
                      H.FGM = rep(c(0), each = nrow(ref.team)),
                      H.FGA = rep(c(0), each = nrow(ref.team)),
                      H.FGP = rep(c(0), each = nrow(ref.team)),
                      H.3FG = rep(c(0), each = nrow(ref.team)),
                      H.3FGA = rep(c(0), each = nrow(ref.team)),
                      H.3FGP = rep(c(0), each = nrow(ref.team)),
                      H.FT = rep(c(0), each = nrow(ref.team)),
                      H.FTA = rep(c(0), each = nrow(ref.team)),
                      H.FTP = rep(c(0), each = nrow(ref.team)),
                      H.PTS = rep(c(0), each = nrow(ref.team)),
                      H.Avg = rep(c(0), each = nrow(ref.team)),
                      H.ORebs = rep(c(0), each = nrow(ref.team)),
                      H.DRebs = rep(c(0), each = nrow(ref.team)),
                      H.TotReb = rep(c(0), each = nrow(ref.team)),
                      H.RAvg = rep(c(0), each = nrow(ref.team)),
                      H.AST = rep(c(0), each = nrow(ref.team)),
                      H.TO = rep(c(0), each = nrow(ref.team)),
                      H.STL = rep(c(0), each = nrow(ref.team)),
                      H.BLK = rep(c(0), each = nrow(ref.team)),
                      H.Fouls = rep(c(0), each = nrow(ref.team)),
                      H.DblDbl = rep(c(0), each = nrow(ref.team)),
                      H.TrpDbl = rep(c(0), each = nrow(ref.team)),
                      H.DQ = rep(c(0), each = nrow(ref.team)),
                      H.TFouls = rep(c(0), each = nrow(ref.team)),
                      A.G = rep(c(0), each = nrow(ref.team)),
                      A.MP = rep(c(0), each = nrow(ref.team)),
                      A.FGM = rep(c(0), each = nrow(ref.team)),
                      A.FGA = rep(c(0), each = nrow(ref.team)),
                      A.FGP = rep(c(0), each = nrow(ref.team)),
                      A.3FG = rep(c(0), each = nrow(ref.team)),
                      A.3FGA = rep(c(0), each = nrow(ref.team)),
                      A.3FGP = rep(c(0), each = nrow(ref.team)),
                      A.FT = rep(c(0), each = nrow(ref.team)),
                      A.FTA = rep(c(0), each = nrow(ref.team)),
                      A.FTP = rep(c(0), each = nrow(ref.team)),
                      A.PTS = rep(c(0), each = nrow(ref.team)),
                      A.Avg = rep(c(0), each = nrow(ref.team)),
                      A.ORebs = rep(c(0), each = nrow(ref.team)),
                      A.DRebs = rep(c(0), each = nrow(ref.team)),
                      A.TotReb = rep(c(0), each = nrow(ref.team)),
                      A.RAvg = rep(c(0), each = nrow(ref.team)),
                      A.AST = rep(c(0), each = nrow(ref.team)),
                      A.TO = rep(c(0), each = nrow(ref.team)),
                      A.STL = rep(c(0), each = nrow(ref.team)),
                      A.BLK = rep(c(0), each = nrow(ref.team)),
                      A.Fouls = rep(c(0), each = nrow(ref.team)),
                      A.DblDbl = rep(c(0), each = nrow(ref.team)),
                      A.TrpDbl = rep(c(0), each = nrow(ref.team)),
                      A.DQ = rep(c(0), each = nrow(ref.team)),
                      A.TFouls = rep(c(0), each = nrow(ref.team)),
                      Year = ref.team[,3])
}


#2010 population
{
  for(i in 1:nrow(teams)) {
    print(i)
    team.vals <- team_stats(teams[i,2], 2010, "MBB", by = "SEASON")
    
    teams[i,3] <- 0
    teams[i,4] <- as.numeric(gsub(":", "", team.vals[1,3]))
    teams[i,5] <- team.vals[1,4]
    teams[i,6] <- as.numeric(gsub(",", "", team.vals[1,5]))
    teams[i,7:13] <- team.vals[1,6:12]
    teams[i,14] <- as.numeric(gsub(",", "", team.vals[1,13]))
    teams[i,15:17] <- team.vals[1,14:16]
    teams[i,18] <- as.numeric(gsub(",", "", team.vals[1,17]))
    teams[i,19:26] <- team.vals[1,18:25]
    teams[i,27] <- 0
    teams[i,28] <- 0
    
    teams[i,29] <- 0
    teams[i,30] <- as.numeric(gsub(":", "", team.vals[2,3]))
    teams[i,31] <- team.vals[2,4]
    teams[i,32] <- as.numeric(gsub(",", "", team.vals[2,5]))
    teams[i,33:39] <- team.vals[2,6:12]
    teams[i,40] <- as.numeric(gsub(",", "", team.vals[2,13]))
    teams[i,41:43] <- team.vals[2,14:16]
    teams[i,44] <- as.numeric(gsub(",", "", team.vals[2,17]))
    teams[i,45:52] <- team.vals[2,18:25]
    teams[i,53] <- 0
    teams[i,54] <- 0
    
    teams[i,55] <- team.vals[1,26]
  }
}

#2011 population
{
  ref.team <- team_mapping(2011, "Men's Basketball")
  
  #team df instantiation
  {
    team <- data.frame(Name = as.character(ref.team[,2]),
                       ID = as.numeric(ref.team[,1]),
                       H.G = rep(c(0), each = nrow(ref.team)),
                       H.MP = rep(c(0), each = nrow(ref.team)),
                       H.FGM = rep(c(0), each = nrow(ref.team)),
                       H.FGA = rep(c(0), each = nrow(ref.team)),
                       H.FGP = rep(c(0), each = nrow(ref.team)),
                       H.3FG = rep(c(0), each = nrow(ref.team)),
                       H.3FGA = rep(c(0), each = nrow(ref.team)),
                       H.3FGP = rep(c(0), each = nrow(ref.team)),
                       H.FT = rep(c(0), each = nrow(ref.team)),
                       H.FTA = rep(c(0), each = nrow(ref.team)),
                       H.FTP = rep(c(0), each = nrow(ref.team)),
                       H.PTS = rep(c(0), each = nrow(ref.team)),
                       H.Avg = rep(c(0), each = nrow(ref.team)),
                       H.ORebs = rep(c(0), each = nrow(ref.team)),
                       H.DRebs = rep(c(0), each = nrow(ref.team)),
                       H.TotReb = rep(c(0), each = nrow(ref.team)),
                       H.RAvg = rep(c(0), each = nrow(ref.team)),
                       H.AST = rep(c(0), each = nrow(ref.team)),
                       H.TO = rep(c(0), each = nrow(ref.team)),
                       H.STL = rep(c(0), each = nrow(ref.team)),
                       H.BLK = rep(c(0), each = nrow(ref.team)),
                       H.Fouls = rep(c(0), each = nrow(ref.team)),
                       H.DblDbl = rep(c(0), each = nrow(ref.team)),
                       H.TrpDbl = rep(c(0), each = nrow(ref.team)),
                       H.DQ = rep(c(0), each = nrow(ref.team)),
                       H.TFouls = rep(c(0), each = nrow(ref.team)),
                       A.G = rep(c(0), each = nrow(ref.team)),
                       A.MP = rep(c(0), each = nrow(ref.team)),
                       A.FGM = rep(c(0), each = nrow(ref.team)),
                       A.FGA = rep(c(0), each = nrow(ref.team)),
                       A.FGP = rep(c(0), each = nrow(ref.team)),
                       A.3FG = rep(c(0), each = nrow(ref.team)),
                       A.3FGA = rep(c(0), each = nrow(ref.team)),
                       A.3FGP = rep(c(0), each = nrow(ref.team)),
                       A.FT = rep(c(0), each = nrow(ref.team)),
                       A.FTA = rep(c(0), each = nrow(ref.team)),
                       A.FTP = rep(c(0), each = nrow(ref.team)),
                       A.PTS = rep(c(0), each = nrow(ref.team)),
                       A.Avg = rep(c(0), each = nrow(ref.team)),
                       A.ORebs = rep(c(0), each = nrow(ref.team)),
                       A.DRebs = rep(c(0), each = nrow(ref.team)),
                       A.TotReb = rep(c(0), each = nrow(ref.team)),
                       A.RAvg = rep(c(0), each = nrow(ref.team)),
                       A.AST = rep(c(0), each = nrow(ref.team)),
                       A.TO = rep(c(0), each = nrow(ref.team)),
                       A.STL = rep(c(0), each = nrow(ref.team)),
                       A.BLK = rep(c(0), each = nrow(ref.team)),
                       A.Fouls = rep(c(0), each = nrow(ref.team)),
                       A.DblDbl = rep(c(0), each = nrow(ref.team)),
                       A.TrpDbl = rep(c(0), each = nrow(ref.team)),
                       A.DQ = rep(c(0), each = nrow(ref.team)),
                       A.TFouls = rep(c(0), each = nrow(ref.team)),
                       Year = ref.team[,3])
  }
  
  for(i in 1:nrow(ref.team)) {
    print(i)
    team.vals <- team_stats(team[i,2], 2011, "MBB", by = "SEASON")
    
    team[i,3] <- 0
    team[i,4] <- as.numeric(gsub(":", "", team.vals[1,3]))
    team[i,5] <- team.vals[1,4]
    team[i,6] <- as.numeric(gsub(",", "", team.vals[1,5]))
    team[i,7:13] <- team.vals[1,6:12]
    team[i,14] <- as.numeric(gsub(",", "", team.vals[1,13]))
    team[i,15:17] <- team.vals[1,14:16]
    team[i,18] <- as.numeric(gsub(",", "", team.vals[1,17]))
    team[i,19:26] <- team.vals[1,18:25]
    team[i,27] <- 0
    team[i,28] <- 0
    
    team[i,29] <- 0
    team[i,30] <- as.numeric(gsub(":", "", team.vals[2,3]))
    team[i,31] <- team.vals[2,4]
    team[i,32] <- as.numeric(gsub(",", "", team.vals[2,5]))
    team[i,33:39] <- team.vals[2,6:12]
    team[i,40] <- as.numeric(gsub(",", "", team.vals[2,13]))
    team[i,41:43] <- team.vals[2,14:16]
    team[i,44] <- as.numeric(gsub(",", "", team.vals[2,17]))
    team[i,45:52] <- team.vals[2,18:25]
    team[i,53] <- 0
    team[i,54] <- 0
    
    team[i,55] <- team.vals[1,26]
  }
  
  teams <- rbind(teams, team)
}

#2012 population
{
  ref.team <- team_mapping(2012, "Men's Basketball")
  
  #team df instantiation
  {
    team <- data.frame(Name = as.character(ref.team[,2]),
                       ID = as.numeric(ref.team[,1]),
                       H.G = rep(c(0), each = nrow(ref.team)),
                       H.MP = rep(c(0), each = nrow(ref.team)),
                       H.FGM = rep(c(0), each = nrow(ref.team)),
                       H.FGA = rep(c(0), each = nrow(ref.team)),
                       H.FGP = rep(c(0), each = nrow(ref.team)),
                       H.3FG = rep(c(0), each = nrow(ref.team)),
                       H.3FGA = rep(c(0), each = nrow(ref.team)),
                       H.3FGP = rep(c(0), each = nrow(ref.team)),
                       H.FT = rep(c(0), each = nrow(ref.team)),
                       H.FTA = rep(c(0), each = nrow(ref.team)),
                       H.FTP = rep(c(0), each = nrow(ref.team)),
                       H.PTS = rep(c(0), each = nrow(ref.team)),
                       H.Avg = rep(c(0), each = nrow(ref.team)),
                       H.ORebs = rep(c(0), each = nrow(ref.team)),
                       H.DRebs = rep(c(0), each = nrow(ref.team)),
                       H.TotReb = rep(c(0), each = nrow(ref.team)),
                       H.RAvg = rep(c(0), each = nrow(ref.team)),
                       H.AST = rep(c(0), each = nrow(ref.team)),
                       H.TO = rep(c(0), each = nrow(ref.team)),
                       H.STL = rep(c(0), each = nrow(ref.team)),
                       H.BLK = rep(c(0), each = nrow(ref.team)),
                       H.Fouls = rep(c(0), each = nrow(ref.team)),
                       H.DblDbl = rep(c(0), each = nrow(ref.team)),
                       H.TrpDbl = rep(c(0), each = nrow(ref.team)),
                       H.DQ = rep(c(0), each = nrow(ref.team)),
                       H.TFouls = rep(c(0), each = nrow(ref.team)),
                       A.G = rep(c(0), each = nrow(ref.team)),
                       A.MP = rep(c(0), each = nrow(ref.team)),
                       A.FGM = rep(c(0), each = nrow(ref.team)),
                       A.FGA = rep(c(0), each = nrow(ref.team)),
                       A.FGP = rep(c(0), each = nrow(ref.team)),
                       A.3FG = rep(c(0), each = nrow(ref.team)),
                       A.3FGA = rep(c(0), each = nrow(ref.team)),
                       A.3FGP = rep(c(0), each = nrow(ref.team)),
                       A.FT = rep(c(0), each = nrow(ref.team)),
                       A.FTA = rep(c(0), each = nrow(ref.team)),
                       A.FTP = rep(c(0), each = nrow(ref.team)),
                       A.PTS = rep(c(0), each = nrow(ref.team)),
                       A.Avg = rep(c(0), each = nrow(ref.team)),
                       A.ORebs = rep(c(0), each = nrow(ref.team)),
                       A.DRebs = rep(c(0), each = nrow(ref.team)),
                       A.TotReb = rep(c(0), each = nrow(ref.team)),
                       A.RAvg = rep(c(0), each = nrow(ref.team)),
                       A.AST = rep(c(0), each = nrow(ref.team)),
                       A.TO = rep(c(0), each = nrow(ref.team)),
                       A.STL = rep(c(0), each = nrow(ref.team)),
                       A.BLK = rep(c(0), each = nrow(ref.team)),
                       A.Fouls = rep(c(0), each = nrow(ref.team)),
                       A.DblDbl = rep(c(0), each = nrow(ref.team)),
                       A.TrpDbl = rep(c(0), each = nrow(ref.team)),
                       A.DQ = rep(c(0), each = nrow(ref.team)),
                       A.TFouls = rep(c(0), each = nrow(ref.team)),
                       Year = ref.team[,3])
  }
  
  for(i in 1:nrow(ref.team)) {
    print(i)
    team.vals <- team_stats(team[i,2], 2012, "MBB", by = "SEASON")
    
    team[i,3] <- 0
    team[i,4] <- as.numeric(gsub(":", "", team.vals[1,3]))
    team[i,5] <- team.vals[1,4]
    team[i,6] <- as.numeric(gsub(",", "", team.vals[1,5]))
    team[i,7:13] <- team.vals[1,6:12]
    team[i,14] <- as.numeric(gsub(",", "", team.vals[1,13]))
    team[i,15:17] <- team.vals[1,14:16]
    team[i,18] <- as.numeric(gsub(",", "", team.vals[1,17]))
    team[i,19:26] <- team.vals[1,18:25]
    team[i,27] <- 0
    team[i,28] <- 0
    
    team[i,29] <- 0
    team[i,30] <- as.numeric(gsub(":", "", team.vals[2,3]))
    team[i,31] <- team.vals[2,4]
    team[i,32] <- as.numeric(gsub(",", "", team.vals[2,5]))
    team[i,33:39] <- team.vals[2,6:12]
    team[i,40] <- as.numeric(gsub(",", "", team.vals[2,13]))
    team[i,41:43] <- team.vals[2,14:16]
    team[i,44] <- as.numeric(gsub(",", "", team.vals[2,17]))
    team[i,45:52] <- team.vals[2,18:25]
    team[i,53] <- 0
    team[i,54] <- 0
    
    team[i,55] <- team.vals[1,26]
  }
  
  teams <- rbind(teams, team)
}

#2013 population
{
  ref.team <- team_mapping(2013, "Men's Basketball")
  
  #team df instantiation
  {
    team <- data.frame(Name = as.character(ref.team[,2]),
                       ID = as.numeric(ref.team[,1]),
                       H.G = rep(c(0), each = nrow(ref.team)),
                       H.MP = rep(c(0), each = nrow(ref.team)),
                       H.FGM = rep(c(0), each = nrow(ref.team)),
                       H.FGA = rep(c(0), each = nrow(ref.team)),
                       H.FGP = rep(c(0), each = nrow(ref.team)),
                       H.3FG = rep(c(0), each = nrow(ref.team)),
                       H.3FGA = rep(c(0), each = nrow(ref.team)),
                       H.3FGP = rep(c(0), each = nrow(ref.team)),
                       H.FT = rep(c(0), each = nrow(ref.team)),
                       H.FTA = rep(c(0), each = nrow(ref.team)),
                       H.FTP = rep(c(0), each = nrow(ref.team)),
                       H.PTS = rep(c(0), each = nrow(ref.team)),
                       H.Avg = rep(c(0), each = nrow(ref.team)),
                       H.ORebs = rep(c(0), each = nrow(ref.team)),
                       H.DRebs = rep(c(0), each = nrow(ref.team)),
                       H.TotReb = rep(c(0), each = nrow(ref.team)),
                       H.RAvg = rep(c(0), each = nrow(ref.team)),
                       H.AST = rep(c(0), each = nrow(ref.team)),
                       H.TO = rep(c(0), each = nrow(ref.team)),
                       H.STL = rep(c(0), each = nrow(ref.team)),
                       H.BLK = rep(c(0), each = nrow(ref.team)),
                       H.Fouls = rep(c(0), each = nrow(ref.team)),
                       H.DblDbl = rep(c(0), each = nrow(ref.team)),
                       H.TrpDbl = rep(c(0), each = nrow(ref.team)),
                       H.DQ = rep(c(0), each = nrow(ref.team)),
                       H.TFouls = rep(c(0), each = nrow(ref.team)),
                       A.G = rep(c(0), each = nrow(ref.team)),
                       A.MP = rep(c(0), each = nrow(ref.team)),
                       A.FGM = rep(c(0), each = nrow(ref.team)),
                       A.FGA = rep(c(0), each = nrow(ref.team)),
                       A.FGP = rep(c(0), each = nrow(ref.team)),
                       A.3FG = rep(c(0), each = nrow(ref.team)),
                       A.3FGA = rep(c(0), each = nrow(ref.team)),
                       A.3FGP = rep(c(0), each = nrow(ref.team)),
                       A.FT = rep(c(0), each = nrow(ref.team)),
                       A.FTA = rep(c(0), each = nrow(ref.team)),
                       A.FTP = rep(c(0), each = nrow(ref.team)),
                       A.PTS = rep(c(0), each = nrow(ref.team)),
                       A.Avg = rep(c(0), each = nrow(ref.team)),
                       A.ORebs = rep(c(0), each = nrow(ref.team)),
                       A.DRebs = rep(c(0), each = nrow(ref.team)),
                       A.TotReb = rep(c(0), each = nrow(ref.team)),
                       A.RAvg = rep(c(0), each = nrow(ref.team)),
                       A.AST = rep(c(0), each = nrow(ref.team)),
                       A.TO = rep(c(0), each = nrow(ref.team)),
                       A.STL = rep(c(0), each = nrow(ref.team)),
                       A.BLK = rep(c(0), each = nrow(ref.team)),
                       A.Fouls = rep(c(0), each = nrow(ref.team)),
                       A.DblDbl = rep(c(0), each = nrow(ref.team)),
                       A.TrpDbl = rep(c(0), each = nrow(ref.team)),
                       A.DQ = rep(c(0), each = nrow(ref.team)),
                       A.TFouls = rep(c(0), each = nrow(ref.team)),
                       Year = ref.team[,3])
  }
  
  for(i in 1:nrow(ref.team)) {
    print(i)
    team.vals <- team_stats(team[i,2], 2013, "MBB", by = "SEASON")
    
    team[i,3] <- 0
    team[i,4] <- as.numeric(gsub(":", "", team.vals[1,3]))
    team[i,5] <- team.vals[1,4]
    team[i,6] <- as.numeric(gsub(",", "", team.vals[1,5]))
    team[i,7:13] <- team.vals[1,6:12]
    team[i,14] <- as.numeric(gsub(",", "", team.vals[1,13]))
    team[i,15:17] <- team.vals[1,14:16]
    team[i,18] <- as.numeric(gsub(",", "", team.vals[1,17]))
    team[i,19:26] <- team.vals[1,18:25]
    team[i,27] <- 0
    team[i,28] <- 0
    
    team[i,29] <- 0
    team[i,30] <- as.numeric(gsub(":", "", team.vals[2,3]))
    team[i,31] <- team.vals[2,4]
    team[i,32] <- as.numeric(gsub(",", "", team.vals[2,5]))
    team[i,33:39] <- team.vals[2,6:12]
    team[i,40] <- as.numeric(gsub(",", "", team.vals[2,13]))
    team[i,41:43] <- team.vals[2,14:16]
    team[i,44] <- as.numeric(gsub(",", "", team.vals[2,17]))
    team[i,45:52] <- team.vals[2,18:25]
    team[i,53] <- 0
    team[i,54] <- 0
    
    team[i,55] <- team.vals[1,26]
  }
  
  teams <- rbind(teams, team)
}

#2014 population
{
  ref.team <- team_mapping(2014, "Men's Basketball")
  
  #team df instantiation
  {
    team <- data.frame(Name = as.character(ref.team[,2]),
                       ID = as.numeric(ref.team[,1]),
                       H.G = rep(c(0), each = nrow(ref.team)),
                       H.MP = rep(c(0), each = nrow(ref.team)),
                       H.FGM = rep(c(0), each = nrow(ref.team)),
                       H.FGA = rep(c(0), each = nrow(ref.team)),
                       H.FGP = rep(c(0), each = nrow(ref.team)),
                       H.3FG = rep(c(0), each = nrow(ref.team)),
                       H.3FGA = rep(c(0), each = nrow(ref.team)),
                       H.3FGP = rep(c(0), each = nrow(ref.team)),
                       H.FT = rep(c(0), each = nrow(ref.team)),
                       H.FTA = rep(c(0), each = nrow(ref.team)),
                       H.FTP = rep(c(0), each = nrow(ref.team)),
                       H.PTS = rep(c(0), each = nrow(ref.team)),
                       H.Avg = rep(c(0), each = nrow(ref.team)),
                       H.ORebs = rep(c(0), each = nrow(ref.team)),
                       H.DRebs = rep(c(0), each = nrow(ref.team)),
                       H.TotReb = rep(c(0), each = nrow(ref.team)),
                       H.RAvg = rep(c(0), each = nrow(ref.team)),
                       H.AST = rep(c(0), each = nrow(ref.team)),
                       H.TO = rep(c(0), each = nrow(ref.team)),
                       H.STL = rep(c(0), each = nrow(ref.team)),
                       H.BLK = rep(c(0), each = nrow(ref.team)),
                       H.Fouls = rep(c(0), each = nrow(ref.team)),
                       H.DblDbl = rep(c(0), each = nrow(ref.team)),
                       H.TrpDbl = rep(c(0), each = nrow(ref.team)),
                       H.DQ = rep(c(0), each = nrow(ref.team)),
                       H.TFouls = rep(c(0), each = nrow(ref.team)),
                       A.G = rep(c(0), each = nrow(ref.team)),
                       A.MP = rep(c(0), each = nrow(ref.team)),
                       A.FGM = rep(c(0), each = nrow(ref.team)),
                       A.FGA = rep(c(0), each = nrow(ref.team)),
                       A.FGP = rep(c(0), each = nrow(ref.team)),
                       A.3FG = rep(c(0), each = nrow(ref.team)),
                       A.3FGA = rep(c(0), each = nrow(ref.team)),
                       A.3FGP = rep(c(0), each = nrow(ref.team)),
                       A.FT = rep(c(0), each = nrow(ref.team)),
                       A.FTA = rep(c(0), each = nrow(ref.team)),
                       A.FTP = rep(c(0), each = nrow(ref.team)),
                       A.PTS = rep(c(0), each = nrow(ref.team)),
                       A.Avg = rep(c(0), each = nrow(ref.team)),
                       A.ORebs = rep(c(0), each = nrow(ref.team)),
                       A.DRebs = rep(c(0), each = nrow(ref.team)),
                       A.TotReb = rep(c(0), each = nrow(ref.team)),
                       A.RAvg = rep(c(0), each = nrow(ref.team)),
                       A.AST = rep(c(0), each = nrow(ref.team)),
                       A.TO = rep(c(0), each = nrow(ref.team)),
                       A.STL = rep(c(0), each = nrow(ref.team)),
                       A.BLK = rep(c(0), each = nrow(ref.team)),
                       A.Fouls = rep(c(0), each = nrow(ref.team)),
                       A.DblDbl = rep(c(0), each = nrow(ref.team)),
                       A.TrpDbl = rep(c(0), each = nrow(ref.team)),
                       A.DQ = rep(c(0), each = nrow(ref.team)),
                       A.TFouls = rep(c(0), each = nrow(ref.team)),
                       Year = ref.team[,3])
  }
  
  for(i in 1:nrow(ref.team)) {
    print(i)
    team.vals <- team_stats(team[i,2], 2014, "MBB", by = "SEASON")
    
    team[i,3] <- 0
    team[i,4] <- as.numeric(gsub(":", "", team.vals[1,3]))
    team[i,5] <- team.vals[1,4]
    team[i,6] <- as.numeric(gsub(",", "", team.vals[1,5]))
    team[i,7:13] <- team.vals[1,6:12]
    team[i,14] <- as.numeric(gsub(",", "", team.vals[1,13]))
    team[i,15:17] <- team.vals[1,14:16]
    team[i,18] <- as.numeric(gsub(",", "", team.vals[1,17]))
    team[i,19:26] <- team.vals[1,18:25]
    team[i,27] <- 0
    team[i,28] <- 0
    
    team[i,29] <- 0
    team[i,30] <- as.numeric(gsub(":", "", team.vals[2,3]))
    team[i,31] <- team.vals[2,4]
    team[i,32] <- as.numeric(gsub(",", "", team.vals[2,5]))
    team[i,33:39] <- team.vals[2,6:12]
    team[i,40] <- as.numeric(gsub(",", "", team.vals[2,13]))
    team[i,41:43] <- team.vals[2,14:16]
    team[i,44] <- as.numeric(gsub(",", "", team.vals[2,17]))
    team[i,45:52] <- team.vals[2,18:25]
    team[i,53] <- 0
    team[i,54] <- 0
    
    team[i,55] <- team.vals[1,26]
  }
  
  teams <- rbind(teams, team)
}

#2015 population
{
  ref.team <- team_mapping(2015, "Men's Basketball")
  
  #team df instantiation
  {
    team <- data.frame(Name = as.character(ref.team[,2]),
                       ID = as.numeric(ref.team[,1]),
                       H.G = rep(c(0), each = nrow(ref.team)),
                       H.MP = rep(c(0), each = nrow(ref.team)),
                       H.FGM = rep(c(0), each = nrow(ref.team)),
                       H.FGA = rep(c(0), each = nrow(ref.team)),
                       H.FGP = rep(c(0), each = nrow(ref.team)),
                       H.3FG = rep(c(0), each = nrow(ref.team)),
                       H.3FGA = rep(c(0), each = nrow(ref.team)),
                       H.3FGP = rep(c(0), each = nrow(ref.team)),
                       H.FT = rep(c(0), each = nrow(ref.team)),
                       H.FTA = rep(c(0), each = nrow(ref.team)),
                       H.FTP = rep(c(0), each = nrow(ref.team)),
                       H.PTS = rep(c(0), each = nrow(ref.team)),
                       H.Avg = rep(c(0), each = nrow(ref.team)),
                       H.ORebs = rep(c(0), each = nrow(ref.team)),
                       H.DRebs = rep(c(0), each = nrow(ref.team)),
                       H.TotReb = rep(c(0), each = nrow(ref.team)),
                       H.RAvg = rep(c(0), each = nrow(ref.team)),
                       H.AST = rep(c(0), each = nrow(ref.team)),
                       H.TO = rep(c(0), each = nrow(ref.team)),
                       H.STL = rep(c(0), each = nrow(ref.team)),
                       H.BLK = rep(c(0), each = nrow(ref.team)),
                       H.Fouls = rep(c(0), each = nrow(ref.team)),
                       H.DblDbl = rep(c(0), each = nrow(ref.team)),
                       H.TrpDbl = rep(c(0), each = nrow(ref.team)),
                       H.DQ = rep(c(0), each = nrow(ref.team)),
                       H.TFouls = rep(c(0), each = nrow(ref.team)),
                       A.G = rep(c(0), each = nrow(ref.team)),
                       A.MP = rep(c(0), each = nrow(ref.team)),
                       A.FGM = rep(c(0), each = nrow(ref.team)),
                       A.FGA = rep(c(0), each = nrow(ref.team)),
                       A.FGP = rep(c(0), each = nrow(ref.team)),
                       A.3FG = rep(c(0), each = nrow(ref.team)),
                       A.3FGA = rep(c(0), each = nrow(ref.team)),
                       A.3FGP = rep(c(0), each = nrow(ref.team)),
                       A.FT = rep(c(0), each = nrow(ref.team)),
                       A.FTA = rep(c(0), each = nrow(ref.team)),
                       A.FTP = rep(c(0), each = nrow(ref.team)),
                       A.PTS = rep(c(0), each = nrow(ref.team)),
                       A.Avg = rep(c(0), each = nrow(ref.team)),
                       A.ORebs = rep(c(0), each = nrow(ref.team)),
                       A.DRebs = rep(c(0), each = nrow(ref.team)),
                       A.TotReb = rep(c(0), each = nrow(ref.team)),
                       A.RAvg = rep(c(0), each = nrow(ref.team)),
                       A.AST = rep(c(0), each = nrow(ref.team)),
                       A.TO = rep(c(0), each = nrow(ref.team)),
                       A.STL = rep(c(0), each = nrow(ref.team)),
                       A.BLK = rep(c(0), each = nrow(ref.team)),
                       A.Fouls = rep(c(0), each = nrow(ref.team)),
                       A.DblDbl = rep(c(0), each = nrow(ref.team)),
                       A.TrpDbl = rep(c(0), each = nrow(ref.team)),
                       A.DQ = rep(c(0), each = nrow(ref.team)),
                       A.TFouls = rep(c(0), each = nrow(ref.team)),
                       Year = ref.team[,3])
  }
  
  for(i in 1:nrow(ref.team)) {
    print(i)
    team.vals <- team_stats(team[i,2], 2015, "MBB", by = "SEASON")
    
    team[i,3] <- 0
    team[i,4] <- as.numeric(gsub(":", "", team.vals[1,3]))
    team[i,5] <- team.vals[1,4]
    team[i,6] <- as.numeric(gsub(",", "", team.vals[1,5]))
    team[i,7:13] <- team.vals[1,6:12]
    team[i,14] <- as.numeric(gsub(",", "", team.vals[1,13]))
    team[i,15:17] <- team.vals[1,14:16]
    team[i,18] <- as.numeric(gsub(",", "", team.vals[1,17]))
    team[i,19:26] <- team.vals[1,18:25]
    team[i,27] <- team.vals[1,26]
    team[i,28] <- 0
    
    team[i,29] <- 0
    team[i,30] <- as.numeric(gsub(":", "", team.vals[2,3]))
    team[i,31] <- team.vals[2,4]
    team[i,32] <- as.numeric(gsub(",", "", team.vals[2,5]))
    team[i,33:39] <- team.vals[2,6:12]
    team[i,40] <- as.numeric(gsub(",", "", team.vals[2,13]))
    team[i,41:43] <- team.vals[2,14:16]
    team[i,44] <- as.numeric(gsub(",", "", team.vals[2,17]))
    team[i,45:52] <- team.vals[2,18:25]
    team[i,53] <- team.vals[2,26]
    team[i,54] <- 0
    
    team[i,55] <- team.vals[1,27]
  }
  
  teams <- rbind(teams, team)
}

#2016 population
{
  ref.team <- team_mapping(2016, "Men's Basketball")
  
  #team df instantiation
  {
    team <- data.frame(Name = as.character(ref.team[,2]),
                       ID = as.numeric(ref.team[,1]),
                       H.G = rep(c(0), each = nrow(ref.team)),
                       H.MP = rep(c(0), each = nrow(ref.team)),
                       H.FGM = rep(c(0), each = nrow(ref.team)),
                       H.FGA = rep(c(0), each = nrow(ref.team)),
                       H.FGP = rep(c(0), each = nrow(ref.team)),
                       H.3FG = rep(c(0), each = nrow(ref.team)),
                       H.3FGA = rep(c(0), each = nrow(ref.team)),
                       H.3FGP = rep(c(0), each = nrow(ref.team)),
                       H.FT = rep(c(0), each = nrow(ref.team)),
                       H.FTA = rep(c(0), each = nrow(ref.team)),
                       H.FTP = rep(c(0), each = nrow(ref.team)),
                       H.PTS = rep(c(0), each = nrow(ref.team)),
                       H.Avg = rep(c(0), each = nrow(ref.team)),
                       H.ORebs = rep(c(0), each = nrow(ref.team)),
                       H.DRebs = rep(c(0), each = nrow(ref.team)),
                       H.TotReb = rep(c(0), each = nrow(ref.team)),
                       H.RAvg = rep(c(0), each = nrow(ref.team)),
                       H.AST = rep(c(0), each = nrow(ref.team)),
                       H.TO = rep(c(0), each = nrow(ref.team)),
                       H.STL = rep(c(0), each = nrow(ref.team)),
                       H.BLK = rep(c(0), each = nrow(ref.team)),
                       H.Fouls = rep(c(0), each = nrow(ref.team)),
                       H.DblDbl = rep(c(0), each = nrow(ref.team)),
                       H.TrpDbl = rep(c(0), each = nrow(ref.team)),
                       H.DQ = rep(c(0), each = nrow(ref.team)),
                       H.TFouls = rep(c(0), each = nrow(ref.team)),
                       A.G = rep(c(0), each = nrow(ref.team)),
                       A.MP = rep(c(0), each = nrow(ref.team)),
                       A.FGM = rep(c(0), each = nrow(ref.team)),
                       A.FGA = rep(c(0), each = nrow(ref.team)),
                       A.FGP = rep(c(0), each = nrow(ref.team)),
                       A.3FG = rep(c(0), each = nrow(ref.team)),
                       A.3FGA = rep(c(0), each = nrow(ref.team)),
                       A.3FGP = rep(c(0), each = nrow(ref.team)),
                       A.FT = rep(c(0), each = nrow(ref.team)),
                       A.FTA = rep(c(0), each = nrow(ref.team)),
                       A.FTP = rep(c(0), each = nrow(ref.team)),
                       A.PTS = rep(c(0), each = nrow(ref.team)),
                       A.Avg = rep(c(0), each = nrow(ref.team)),
                       A.ORebs = rep(c(0), each = nrow(ref.team)),
                       A.DRebs = rep(c(0), each = nrow(ref.team)),
                       A.TotReb = rep(c(0), each = nrow(ref.team)),
                       A.RAvg = rep(c(0), each = nrow(ref.team)),
                       A.AST = rep(c(0), each = nrow(ref.team)),
                       A.TO = rep(c(0), each = nrow(ref.team)),
                       A.STL = rep(c(0), each = nrow(ref.team)),
                       A.BLK = rep(c(0), each = nrow(ref.team)),
                       A.Fouls = rep(c(0), each = nrow(ref.team)),
                       A.DblDbl = rep(c(0), each = nrow(ref.team)),
                       A.TrpDbl = rep(c(0), each = nrow(ref.team)),
                       A.DQ = rep(c(0), each = nrow(ref.team)),
                       A.TFouls = rep(c(0), each = nrow(ref.team)),
                       Year = ref.team[,3])
  }
  
  for(i in 1:nrow(ref.team)) {
    print(i)
    team.vals <- team_stats(team[i,2], 2016, "MBB", by = "SEASON")
    
    team[i,3] <- 0
    team[i,4] <- as.numeric(gsub(":", "", team.vals[1,3]))
    team[i,5] <- team.vals[1,4]
    team[i,6] <- as.numeric(gsub(",", "", team.vals[1,5]))
    team[i,7:13] <- team.vals[1,6:12]
    team[i,14] <- as.numeric(gsub(",", "", team.vals[1,13]))
    team[i,15:17] <- team.vals[1,14:16]
    team[i,18] <- as.numeric(gsub(",", "", team.vals[1,17]))
    team[i,19:26] <- team.vals[1,18:25]
    team[i,27] <- team.vals[1,26]
    team[i,28] <- team.vals[1,27]
    
    team[i,29] <- 0
    team[i,30] <- as.numeric(gsub(":", "", team.vals[2,3]))
    team[i,31] <- team.vals[2,4]
    team[i,32] <- as.numeric(gsub(",", "", team.vals[2,5]))
    team[i,33:39] <- team.vals[2,6:12]
    team[i,40] <- as.numeric(gsub(",", "", team.vals[2,13]))
    team[i,41:43] <- team.vals[2,14:16]
    team[i,44] <- as.numeric(gsub(",", "", team.vals[2,17]))
    team[i,45:52] <- team.vals[2,18:25]
    team[i,53] <- team.vals[2,26]
    team[i,54] <- team.vals[2,27]
    
    team[i,55] <- team.vals[1,28]
  }
  
  teams <- rbind(teams, team)
}

#2017 population
{
  ref.team <- team_mapping(2017, "Men's Basketball")
  
  #team df instantiation
  {
    team <- data.frame(Name = as.character(ref.team[,2]),
                       ID = as.numeric(ref.team[,1]),
                       H.G = rep(c(0), each = nrow(ref.team)),
                       H.MP = rep(c(0), each = nrow(ref.team)),
                       H.FGM = rep(c(0), each = nrow(ref.team)),
                       H.FGA = rep(c(0), each = nrow(ref.team)),
                       H.FGP = rep(c(0), each = nrow(ref.team)),
                       H.3FG = rep(c(0), each = nrow(ref.team)),
                       H.3FGA = rep(c(0), each = nrow(ref.team)),
                       H.3FGP = rep(c(0), each = nrow(ref.team)),
                       H.FT = rep(c(0), each = nrow(ref.team)),
                       H.FTA = rep(c(0), each = nrow(ref.team)),
                       H.FTP = rep(c(0), each = nrow(ref.team)),
                       H.PTS = rep(c(0), each = nrow(ref.team)),
                       H.Avg = rep(c(0), each = nrow(ref.team)),
                       H.ORebs = rep(c(0), each = nrow(ref.team)),
                       H.DRebs = rep(c(0), each = nrow(ref.team)),
                       H.TotReb = rep(c(0), each = nrow(ref.team)),
                       H.RAvg = rep(c(0), each = nrow(ref.team)),
                       H.AST = rep(c(0), each = nrow(ref.team)),
                       H.TO = rep(c(0), each = nrow(ref.team)),
                       H.STL = rep(c(0), each = nrow(ref.team)),
                       H.BLK = rep(c(0), each = nrow(ref.team)),
                       H.Fouls = rep(c(0), each = nrow(ref.team)),
                       H.DblDbl = rep(c(0), each = nrow(ref.team)),
                       H.TrpDbl = rep(c(0), each = nrow(ref.team)),
                       H.DQ = rep(c(0), each = nrow(ref.team)),
                       H.TFouls = rep(c(0), each = nrow(ref.team)),
                       A.G = rep(c(0), each = nrow(ref.team)),
                       A.MP = rep(c(0), each = nrow(ref.team)),
                       A.FGM = rep(c(0), each = nrow(ref.team)),
                       A.FGA = rep(c(0), each = nrow(ref.team)),
                       A.FGP = rep(c(0), each = nrow(ref.team)),
                       A.3FG = rep(c(0), each = nrow(ref.team)),
                       A.3FGA = rep(c(0), each = nrow(ref.team)),
                       A.3FGP = rep(c(0), each = nrow(ref.team)),
                       A.FT = rep(c(0), each = nrow(ref.team)),
                       A.FTA = rep(c(0), each = nrow(ref.team)),
                       A.FTP = rep(c(0), each = nrow(ref.team)),
                       A.PTS = rep(c(0), each = nrow(ref.team)),
                       A.Avg = rep(c(0), each = nrow(ref.team)),
                       A.ORebs = rep(c(0), each = nrow(ref.team)),
                       A.DRebs = rep(c(0), each = nrow(ref.team)),
                       A.TotReb = rep(c(0), each = nrow(ref.team)),
                       A.RAvg = rep(c(0), each = nrow(ref.team)),
                       A.AST = rep(c(0), each = nrow(ref.team)),
                       A.TO = rep(c(0), each = nrow(ref.team)),
                       A.STL = rep(c(0), each = nrow(ref.team)),
                       A.BLK = rep(c(0), each = nrow(ref.team)),
                       A.Fouls = rep(c(0), each = nrow(ref.team)),
                       A.DblDbl = rep(c(0), each = nrow(ref.team)),
                       A.TrpDbl = rep(c(0), each = nrow(ref.team)),
                       A.DQ = rep(c(0), each = nrow(ref.team)),
                       A.TFouls = rep(c(0), each = nrow(ref.team)),
                       Year = ref.team[,3])
  }
  
  for(i in 1:nrow(ref.team)) {
    print(i)
    team.vals <- team_stats(team[i,2], 2017, "MBB", by = "SEASON")
    
    team[i,3] <- 0
    team[i,4] <- as.numeric(gsub(":", "", team.vals[1,3]))
    team[i,5] <- team.vals[1,4]
    team[i,6] <- as.numeric(gsub(",", "", team.vals[1,5]))
    team[i,7:13] <- team.vals[1,6:12]
    team[i,14] <- as.numeric(gsub(",", "", team.vals[1,13]))
    team[i,15:17] <- team.vals[1,14:16]
    team[i,18] <- as.numeric(gsub(",", "", team.vals[1,17]))
    team[i,19:26] <- team.vals[1,18:25]
    team[i,27] <- team.vals[1,26]
    team[i,28] <- team.vals[1,27]
    
    team[i,29] <- 0
    team[i,30] <- as.numeric(gsub(":", "", team.vals[2,3]))
    team[i,31] <- team.vals[2,4]
    team[i,32] <- as.numeric(gsub(",", "", team.vals[2,5]))
    team[i,33:39] <- team.vals[2,6:12]
    team[i,40] <- as.numeric(gsub(",", "", team.vals[2,13]))
    team[i,41:43] <- team.vals[2,14:16]
    team[i,44] <- as.numeric(gsub(",", "", team.vals[2,17]))
    team[i,45:52] <- team.vals[2,18:25]
    team[i,53] <- team.vals[2,26]
    team[i,54] <- team.vals[2,27]
    
    team[i,55] <- team.vals[1,28]
  }
  
  teams <- rbind(teams, team)
}

#2018 population
{
  ref.team <- team_mapping(2018, "Men's Basketball")
  
  #team df instantiation
  {
    team <- data.frame(Name = as.character(ref.team[,2]),
                       ID = as.numeric(ref.team[,1]),
                       H.G = rep(c(0), each = nrow(ref.team)),
                       H.MP = rep(c(0), each = nrow(ref.team)),
                       H.FGM = rep(c(0), each = nrow(ref.team)),
                       H.FGA = rep(c(0), each = nrow(ref.team)),
                       H.FGP = rep(c(0), each = nrow(ref.team)),
                       H.3FG = rep(c(0), each = nrow(ref.team)),
                       H.3FGA = rep(c(0), each = nrow(ref.team)),
                       H.3FGP = rep(c(0), each = nrow(ref.team)),
                       H.FT = rep(c(0), each = nrow(ref.team)),
                       H.FTA = rep(c(0), each = nrow(ref.team)),
                       H.FTP = rep(c(0), each = nrow(ref.team)),
                       H.PTS = rep(c(0), each = nrow(ref.team)),
                       H.Avg = rep(c(0), each = nrow(ref.team)),
                       H.ORebs = rep(c(0), each = nrow(ref.team)),
                       H.DRebs = rep(c(0), each = nrow(ref.team)),
                       H.TotReb = rep(c(0), each = nrow(ref.team)),
                       H.RAvg = rep(c(0), each = nrow(ref.team)),
                       H.AST = rep(c(0), each = nrow(ref.team)),
                       H.TO = rep(c(0), each = nrow(ref.team)),
                       H.STL = rep(c(0), each = nrow(ref.team)),
                       H.BLK = rep(c(0), each = nrow(ref.team)),
                       H.Fouls = rep(c(0), each = nrow(ref.team)),
                       H.DblDbl = rep(c(0), each = nrow(ref.team)),
                       H.TrpDbl = rep(c(0), each = nrow(ref.team)),
                       H.DQ = rep(c(0), each = nrow(ref.team)),
                       H.TFouls = rep(c(0), each = nrow(ref.team)),
                       A.G = rep(c(0), each = nrow(ref.team)),
                       A.MP = rep(c(0), each = nrow(ref.team)),
                       A.FGM = rep(c(0), each = nrow(ref.team)),
                       A.FGA = rep(c(0), each = nrow(ref.team)),
                       A.FGP = rep(c(0), each = nrow(ref.team)),
                       A.3FG = rep(c(0), each = nrow(ref.team)),
                       A.3FGA = rep(c(0), each = nrow(ref.team)),
                       A.3FGP = rep(c(0), each = nrow(ref.team)),
                       A.FT = rep(c(0), each = nrow(ref.team)),
                       A.FTA = rep(c(0), each = nrow(ref.team)),
                       A.FTP = rep(c(0), each = nrow(ref.team)),
                       A.PTS = rep(c(0), each = nrow(ref.team)),
                       A.Avg = rep(c(0), each = nrow(ref.team)),
                       A.ORebs = rep(c(0), each = nrow(ref.team)),
                       A.DRebs = rep(c(0), each = nrow(ref.team)),
                       A.TotReb = rep(c(0), each = nrow(ref.team)),
                       A.RAvg = rep(c(0), each = nrow(ref.team)),
                       A.AST = rep(c(0), each = nrow(ref.team)),
                       A.TO = rep(c(0), each = nrow(ref.team)),
                       A.STL = rep(c(0), each = nrow(ref.team)),
                       A.BLK = rep(c(0), each = nrow(ref.team)),
                       A.Fouls = rep(c(0), each = nrow(ref.team)),
                       A.DblDbl = rep(c(0), each = nrow(ref.team)),
                       A.TrpDbl = rep(c(0), each = nrow(ref.team)),
                       A.DQ = rep(c(0), each = nrow(ref.team)),
                       A.TFouls = rep(c(0), each = nrow(ref.team)),
                       Year = ref.team[,3])
  }
  
  for(i in 1:nrow(ref.team)) {
    print(i)
    team.vals <- team_stats(team[i,2], 2018, "MBB", by = "SEASON")
    
    team[i,3] <- team.vals[1,3]
    team[i,4] <- as.numeric(gsub(":", "", team.vals[1,4]))
    team[i,5] <- team.vals[1,5]
    team[i,6] <- as.numeric(gsub(",", "", team.vals[1,6]))
    team[i,7:13] <- team.vals[1,7:13]
    team[i,14] <- as.numeric(gsub(",", "", team.vals[1,14]))
    team[i,15:17] <- team.vals[1,15:17]
    team[i,18] <- as.numeric(gsub(",", "", team.vals[1,18]))
    team[i,19:26] <- team.vals[1,19:26]
    team[i,27] <- team.vals[1,27]
    team[i,28] <- team.vals[1,28]
    
    team[i,29] <- team.vals[2,3]
    team[i,30] <- as.numeric(gsub(":", "", team.vals[2,4]))
    team[i,31] <- team.vals[2,5]
    team[i,32] <- as.numeric(gsub(",", "", team.vals[2,6]))
    team[i,33:39] <- team.vals[2,7:13]
    team[i,40] <- as.numeric(gsub(",", "", team.vals[2,14]))
    team[i,41:43] <- team.vals[2,15:17]
    team[i,44] <- as.numeric(gsub(",", "", team.vals[2,18]))
    team[i,45:52] <- team.vals[2,19:26]
    team[i,53] <- team.vals[2,27]
    team[i,54] <- team.vals[2,28]
    
    team[i,55] <- team.vals[1,29]
  }
  
  teams <- rbind(teams, team)
}

#2019 population
{
  ref.team <- team_mapping(2019, "Men's Basketball")
  
  #team df instantiation
  {
    team <- data.frame(Name = as.character(ref.team[,2]),
                       ID = as.numeric(ref.team[,1]),
                       H.G = rep(c(0), each = nrow(ref.team)),
                       H.MP = rep(c(0), each = nrow(ref.team)),
                       H.FGM = rep(c(0), each = nrow(ref.team)),
                       H.FGA = rep(c(0), each = nrow(ref.team)),
                       H.FGP = rep(c(0), each = nrow(ref.team)),
                       H.3FG = rep(c(0), each = nrow(ref.team)),
                       H.3FGA = rep(c(0), each = nrow(ref.team)),
                       H.3FGP = rep(c(0), each = nrow(ref.team)),
                       H.FT = rep(c(0), each = nrow(ref.team)),
                       H.FTA = rep(c(0), each = nrow(ref.team)),
                       H.FTP = rep(c(0), each = nrow(ref.team)),
                       H.PTS = rep(c(0), each = nrow(ref.team)),
                       H.Avg = rep(c(0), each = nrow(ref.team)),
                       H.ORebs = rep(c(0), each = nrow(ref.team)),
                       H.DRebs = rep(c(0), each = nrow(ref.team)),
                       H.TotReb = rep(c(0), each = nrow(ref.team)),
                       H.RAvg = rep(c(0), each = nrow(ref.team)),
                       H.AST = rep(c(0), each = nrow(ref.team)),
                       H.TO = rep(c(0), each = nrow(ref.team)),
                       H.STL = rep(c(0), each = nrow(ref.team)),
                       H.BLK = rep(c(0), each = nrow(ref.team)),
                       H.Fouls = rep(c(0), each = nrow(ref.team)),
                       H.DblDbl = rep(c(0), each = nrow(ref.team)),
                       H.TrpDbl = rep(c(0), each = nrow(ref.team)),
                       H.DQ = rep(c(0), each = nrow(ref.team)),
                       H.TFouls = rep(c(0), each = nrow(ref.team)),
                       A.G = rep(c(0), each = nrow(ref.team)),
                       A.MP = rep(c(0), each = nrow(ref.team)),
                       A.FGM = rep(c(0), each = nrow(ref.team)),
                       A.FGA = rep(c(0), each = nrow(ref.team)),
                       A.FGP = rep(c(0), each = nrow(ref.team)),
                       A.3FG = rep(c(0), each = nrow(ref.team)),
                       A.3FGA = rep(c(0), each = nrow(ref.team)),
                       A.3FGP = rep(c(0), each = nrow(ref.team)),
                       A.FT = rep(c(0), each = nrow(ref.team)),
                       A.FTA = rep(c(0), each = nrow(ref.team)),
                       A.FTP = rep(c(0), each = nrow(ref.team)),
                       A.PTS = rep(c(0), each = nrow(ref.team)),
                       A.Avg = rep(c(0), each = nrow(ref.team)),
                       A.ORebs = rep(c(0), each = nrow(ref.team)),
                       A.DRebs = rep(c(0), each = nrow(ref.team)),
                       A.TotReb = rep(c(0), each = nrow(ref.team)),
                       A.RAvg = rep(c(0), each = nrow(ref.team)),
                       A.AST = rep(c(0), each = nrow(ref.team)),
                       A.TO = rep(c(0), each = nrow(ref.team)),
                       A.STL = rep(c(0), each = nrow(ref.team)),
                       A.BLK = rep(c(0), each = nrow(ref.team)),
                       A.Fouls = rep(c(0), each = nrow(ref.team)),
                       A.DblDbl = rep(c(0), each = nrow(ref.team)),
                       A.TrpDbl = rep(c(0), each = nrow(ref.team)),
                       A.DQ = rep(c(0), each = nrow(ref.team)),
                       A.TFouls = rep(c(0), each = nrow(ref.team)),
                       Year = ref.team[,3])
  }
  
  for(i in 1:nrow(ref.team)) {
    print(i)
    team.vals <- team_stats(team[i,2], 2019, "MBB", by = "SEASON")
    
    team[i,3] <- team.vals[1,3]
    team[i,4] <- as.numeric(gsub(":", "", team.vals[1,4]))
    team[i,5] <- team.vals[1,5]
    team[i,6] <- as.numeric(gsub(",", "", team.vals[1,6]))
    team[i,7:13] <- team.vals[1,7:13]
    team[i,14] <- as.numeric(gsub(",", "", team.vals[1,14]))
    team[i,15:17] <- team.vals[1,15:17]
    team[i,18] <- as.numeric(gsub(",", "", team.vals[1,18]))
    team[i,19:26] <- team.vals[1,19:26]
    team[i,27] <- team.vals[1,27]
    team[i,28] <- team.vals[1,28]
    
    team[i,29] <- team.vals[2,3]
    team[i,30] <- as.numeric(gsub(":", "", team.vals[2,4]))
    team[i,31] <- team.vals[2,5]
    team[i,32] <- as.numeric(gsub(",", "", team.vals[2,6]))
    team[i,33:39] <- team.vals[2,7:13]
    team[i,40] <- as.numeric(gsub(",", "", team.vals[2,14]))
    team[i,41:43] <- team.vals[2,15:17]
    team[i,44] <- as.numeric(gsub(",", "", team.vals[2,18]))
    team[i,45:52] <- team.vals[2,19:26]
    team[i,53] <- team.vals[2,27]
    team[i,54] <- team.vals[2,28]
    
    team[i,55] <- team.vals[1,29]
  }
  
  teams <- rbind(teams, team)
}

#Four Factors Instantiation
{
  #Effective Turnover Percentage
  teams$eFGP <- (as.numeric(gsub(",", "", teams$H.FGM)) + (teams$H.3FG * 0.5)) / (teams$H.FGA)
  
  #Turnover Percentage
  teams$TOP <- (teams$H.TO) / ((teams$H.FGA) + (as.numeric(gsub(",", "", teams$H.FTA)) * 0.44) + (teams$H.TO))
  
  #Offensive Rebounding Percentage
  teams$ORBP <- (teams$H.ORebs) / ((teams$H.ORebs) + (as.numeric(gsub(",", "", teams$A.DRebs))))
  
  #Defensive Rebounding Percentage
  teams$DRBP <- (as.numeric(gsub(",", "", teams$H.DRebs))) / ((as.numeric(gsub(",", "", teams$H.DRebs))) + teams$A.ORebs)
  
  #Free Throw Percentage
  teams$FTP <- (teams$H.FT) / (teams$H.FGA)
}

#Desired Features: Four Factors, Binary home/away, numwins, numloss

#input instantiation
{
  inputs <- data.frame(H.name = "",
                       H.id = 0,
                       A.name = "",
                       H.eFGP = 0,
                       H.TOP = 0,
                       H.ORBP = 0,
                       H.DRBP = 0,
                       H.FTP = 0,
                       Home = 0,
                       H.WL = 0,
                       A.eFGP = 0,
                       A.TOP = 0,
                       A.ORBP = 0,
                       A.DRBP = 0,
                       A.FTP = 0,
                       Wins = 0)
  
  for(i in 305:3493) {
    
    x <- 3493 - i
    
    sched <- team_schedule(teams[x,2], teams[x,55], "MBB")
    
    print(i)
    print(x)
    
    for(j in 1:nrow(sched)) {
      
      away <- ""
      
      if(substr(sched[j,3], 1, 1) == "@") {
        away <- substr(sched[j,3], 3, nchar(sched[j,3]))
      }
      else if (grepl("@", sched[j,3])) {
        strt <- unlist(gregexpr('@', sched[j,3]))[1]
        away <- substr(sched[j,3], 1, strt - 2)
      }
      else {
        away <- sched[j,3]
      }
      
      loc <- 0
      
      if(sched[j,4] == "Home") {
        loc <- 1
      }
      
      position <- -1
      
      for(k in 1:3493) {
        if(teams[k,1] == away && teams[k,55] == teams[x,55]) {
          position <- k
        }
      }
      
      if(position != -1) {
        
        win <- 0
        
        
        if(sched[j,6] == "W") {
          win <- 1
        }
        
        input <- data.frame(H.name = sched[j,1],
                             H.id = teams[x,3],
                             A.name = sched[x,3],
                             H.eFGP = teams[x,56],
                             H.TOP = teams[x,57],
                             H.ORBP = teams[x,58],
                             H.DRBP = teams[x,59],
                             H.FTP = teams[x,60],
                             Home = loc,
                             H.WL = sched[j,10]/(sched[j,10] + sched[j,11]),
                             A.eFGP = teams[position,56],
                             A.TOP = teams[position, 57],
                             A.ORBP = teams[position, 58],
                             A.DRBP = teams[position, 59],
                             A.FTP = teams[position, 60],
                             Wins = win)
        
        inputs <- rbind(inputs, input)
      }
    }
  }
  
  inputs <- inputs[2:12837,]
  
  dataBackup <- inputs
}

#Input Normalization
{
  #Not necessary for current inputs
}

#Neural Net Construction
{
  # train <- inputs[1:10269,]
  # test <- inputs[10270:12836,]
  # 
  # nn <- neuralnet(Wins~H.eFGP+H.TOP+H.ORBP+H.DRBP+H.FTP+H.WL+A.eFGP+A.TOP+A.ORBP+A.DRBP+A.FTP,
  #                 data=train, hidden=7, learningrate=0.01, algorithm="backprop", act.fct = "logistic", linear.output = FALSE)
  # 
  # Predict=predict(nn,test)
  # 
  # #implements a step function activated at 0.5
  # pred <- ifelse(Predict>0.5, 1, 0)
  # pred
  # 
  # #counts errors in the test
  # sum <- 0
  # for(i in 1:2567) {
  #   if(pred[i] != test[i,16]) {
  #     sum <- sum + 1
  #   }
  # }
  # 
  # #computes accuracy of model on testing set
  # testAccuracy <- 100 - ((sum*100)/2567)
  
  for(i in 1:12836) {
  }
  
  model <- train(inputs[,c(4:8, 10:15)], factor(inputs[,16]), 'nb', trControl=trainControl(method = 'cv', number = 10))
}

#Neural Net Testing
{
  mmTest <- teams[3141:3493, c(1, 2, 56:60)]
  
  matchups <- list()

  matchups <- append(matchups, c("Duke", "North Dakota St."))
  matchups <- append(matchups, c("VCU", "UCF"))
  matchups <- append(matchups, c("Mississippi St.", "Liberty"))
  matchups <- append(matchups, c("Saint Louis", "Virginia Tech"))
  matchups <- append(matchups, c("Belmont", "Maryland"))
  matchups <- append(matchups, c("LSU", "Yale"))
  matchups <- append(matchups, c("Louisville", "Minnesota"))
  matchups <- append(matchups, c("Michigan St.", "Bradley"))
  matchups <- append(matchups, c("Gonzaga", "Fairleigh Dickinson"))
  matchups <- append(matchups, c("Baylor", "Syracuse"))
  matchups <- append(matchups, c("Marquette", "Murray St."))
  matchups <- append(matchups, c("Vermont", "Florida St."))
  matchups <- append(matchups, c("Buffalo", "Arizona St."))
  matchups <- append(matchups, c("Texas Tech", "Northern Ky."))
  matchups <- append(matchups, c("Nevada", "Florida"))
  matchups <- append(matchups, c("Michigan", "Montana"))
  matchups <- append(matchups, c("Duke", "UCF"))
  matchups <- append(matchups, c("Liberty", "Virginia Tech"))
  matchups <- append(matchups, c("Maryland", "LSU"))
  matchups <- append(matchups, c("Minnesota", "Michigan St."))
  matchups <- append(matchups, c("Gonzaga", "Baylor"))
  matchups <- append(matchups, c("Murray St.", "Florida St."))
  matchups <- append(matchups, c("Buffalo", "Texas Tech"))
  matchups <- append(matchups, c("Florida", "Michigan"))
  matchups <- append(matchups, c("Duke", "Virginia Tech"))
  matchups <- append(matchups, c("LSU", "Michigan St."))
  matchups <- append(matchups, c("Gonzaga", "Florida St."))
  matchups <- append(matchups, c("Texas Tech", "Michigan"))
  matchups <- append(matchups, c("Michigan St.", "Duke"))
  matchups <- append(matchups, c("Gonzaga", "Texas Tech"))
  matchups <- append(matchups, c("Michigan St.", "Texas Tech"))
  matchups <- append(matchups, c("Virginia", "Gardner-Webb"))
  matchups <- append(matchups, c("Mississippi St.", "Oklahoma"))
  matchups <- append(matchups, c("Wisconsin", "Oregon"))
  matchups <- append(matchups, c("Kansas St.", "UC Irvine"))
  matchups <- append(matchups, c("Villanova", "Saint Mary's (CA)"))
  matchups <- append(matchups, c("Purdue", "Old Dominion"))
  matchups <- append(matchups, c("Cincinnati", "Iowa"))
  matchups <- append(matchups, c("Tennessee", "Colgate"))
  matchups <- append(matchups, c("North Carolina", "Iona"))
  matchups <- append(matchups, c("Utah St.", "Washington"))
  matchups <- append(matchups, c("Auburn", "New Mexico St."))
  matchups <- append(matchups, c("Kansas", "Northeastern"))
  matchups <- append(matchups, c("Iowa St.", "Ohio St."))
  matchups <- append(matchups, c("Houston", "Georgia St."))
  matchups <- append(matchups, c("Wofford", "Seton Hall"))
  matchups <- append(matchups, c("Kentucky", "Abilene Christian"))
  matchups <- append(matchups, c("Virginia", "Oklahoma"))
  matchups <- append(matchups, c("Oregon", "UC Irvine"))
  matchups <- append(matchups, c("Villanova", "Purdue"))
  matchups <- append(matchups, c("Iowa", "Tennessee"))
  matchups <- append(matchups, c("North Carolina", "Washington"))
  matchups <- append(matchups, c("Auburn", "Kansas"))
  matchups <- append(matchups, c("Ohio St.", "Houston"))
  matchups <- append(matchups, c("Wofford", "Kentucky"))
  matchups <- append(matchups, c("Virginia", "Oregon"))
  matchups <- append(matchups, c("Purdue", "Tennessee"))
  matchups <- append(matchups, c("North Carolina", "Auburn"))
  matchups <- append(matchups, c("Houston", "Kentucky"))
  matchups <- append(matchups, c("Virginia", "Purdue"))
  matchups <- append(matchups, c("Auburn", "Kentucky"))
  matchups <- append(matchups, c("Virginia", "Auburn"))
  matchups <- append(matchups, c("Texas Tech", "Virginia"))
  
  mm2019 <- data.frame(H.name = "",
                       H.id = 0,
                       A.name = "",
                       H.eFGP = 0,
                       H.TOP = 0,
                       H.ORBP = 0,
                       H.DRBP = 0,
                       H.FTP = 0,
                       Home = 0,
                       H.WL = 0,
                       A.eFGP = 0,
                       A.TOP = 0,
                       A.ORBP = 0,
                       A.DRBP = 0,
                       A.FTP = 0,
                       Wins = 0)
  
  for(i in 1:63) {
    positionH <- 0
    positionA <- 0
    
    for(j in 1:353) {
      if(matchups[2*i] == mmTest[j,1]) {
        positionA <- j
      }
      if(matchups[(2*i)-1] == mmTest[j,1]) {
        positionH <- j
      }
    }
    
    # if(positionH == 0) {
    #   print(matchups[(2*i)-1])
    # }
    # if(positionA == 0) {
    #   print(matchups[(2*i)])
    # }
    
    game <- data.frame(H.name = mmTest[positionH, 1],
                         H.id = 0,
                         A.name = mmTest[positionA, 1],
                         H.eFGP =  mmTest[positionH, 3],
                         H.TOP =  mmTest[positionH, 4],
                         H.ORBP =  mmTest[positionH, 5],
                         H.DRBP =  mmTest[positionH, 6],
                         H.FTP =  mmTest[positionH, 7],
                         Home = 0,
                         H.WL = 0,
                         A.eFGP = 0,
                         A.TOP = 0,
                         A.ORBP = 0,
                         A.DRBP = 0,
                         A.FTP = 0,
                         Wins = 0)
  }
}
























