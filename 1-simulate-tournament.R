
##to run, can change number of sims, year, name of file to save tourneysims
#then run sim brackets file, where you can change numBrackets and file name to save brackets
#then run optimize brackets file where you can optimize brackets

##SET PARAMETERS/READ DATA######
year<-2023
sims<-5000
name<-paste0(year,"/TourneySims_", sims,"sims.Rda")
backtest<-ifelse(year==2023, F, T)

load("data/game-data.RData")
source("functions.R", encoding = "UTF-8")
samplesubmission<-read.csv(paste0(year, "/Kaggle Submission.csv"), stringsAsFactors = F) 

#VCU R1 Forfeit
NCAATourneyDetailedResults<-rbind.fill(NCAATourneyDetailedResults, data.frame(Season=2021, WTeamID=1332,LTeamID=1433, Tournament=1))


#if you use kaggle data files, this is where you could put your own projections...id like to add this to the shiny app but it's just not gonna happen
colnames(samplesubmission)[colnames(samplesubmission)%in% c("id", "Id")]<-"ID"
colnames(samplesubmission)[colnames(samplesubmission)%in% c("pred", "PRED")]<-"Pred"
samplesubmission$Team<-as.numeric(sapply(strsplit(samplesubmission$ID, "_"), `[[`, 2))
samplesubmission$OPP<-as.numeric(sapply(strsplit(samplesubmission$ID, "_"), `[[`, 3))
samplesubmission$Team_Full<-Teams$Team_Full[match(samplesubmission$Team, Teams$TeamID)]
samplesubmission$OPP_Full<-Teams$Team_Full[match(samplesubmission$OPP, Teams$TeamID)]
head(samplesubmission)



#need to get simulate winners in chronological order
TourneySlots$Round<-ifelse(substring(TourneySlots$Slot, 1, 1)=="R", substring(TourneySlots$Slot, 2, 2), 0)
TourneySlots<-TourneySlots[order(TourneySlots$Round, TourneySlots$Season, decreasing = F), ]
TourneySlots<-TourneySlots[TourneySlots$Season==year,]


#handling play-in games, impute actual into simulation prediction if game has occured. Team will still be listed as Temple/Asu though
losing_teams<-c()
if(year==2018){
  losing_teams<-c("Ucla", "Long Island", "Arizona State", "North Carolina Central")  #
} else if (year==2019){
  losing_teams<-c("Prairie View A&m", "Temple", "St Johns", "North Carolina Central")  #
} else if (year==2021){
  losing_teams<-c("Wichita State","Mount St Marys", "Appalachian State", "Michigan State")
}else if (year==2022){
  losing_teams<-c("Texas A&m Corpus Christi","Wyoming", "Rutgers","Bryant")
  # load("~/Documents/NCAA/ncaa-preseason/game-prediction/Saved-Files/Game Predictions Injury.Rda")
  # losing_teams<-c("Texas A&m Corpus Christi","Wyoming", "Rutgers","Bryant", fulldf$Team[which(fulldf$Type=="NCAA"&fulldf$Year==2022&fulldf$ScoreDiff<0)]) %>% unique()
}else if (year==2023){
  losing_teams<-c("Mississippi State","Southeast Missouri State")
}

if(length(losing_teams)>=1){
  samplesubmission$Pred[samplesubmission$Team_Full%in%losing_teams]<-0
  samplesubmission$Pred[samplesubmission$OPP_Full%in%losing_teams]<-1
}


###GET-WINNER FUNCTIONS####

getSimResult<-function(row){
  # slot<-TourneySlots$Slot[1]
  # row<-TourneySlots[TourneySlots$Season==year,][5,]
  #  newdata<-data.frame(Slot=TourneySeeds$Seed[TourneySeeds$Season==year], Team=as.numeric(TourneySeeds$Team[TourneySeeds$Season==year]), Payout=0)
  
  
  team1<-as.numeric(resultDF$Team[resultDF$Slot==TourneySlots$StrongSeed[row]]) #as.numeric(gsub("\\D", "", row$Strongseed)) 
  team2<-as.numeric(resultDF$Team[resultDF$Slot==TourneySlots$WeakSeed[row]])  #as.numeric(gsub("\\D", "", row$Weakseed)) 
  
  
  #simulate winner--samplesubmission stores data so that team1<team2
  if(team1<team2) {
    prob<-samplesubmission$Pred[samplesubmission$Team==team1 & samplesubmission$OPP==team2]
    if(runif(1)>prob){
      winner<-team2
      loser<-team1
    } else{
      winner<-team1
      loser<-team2
    }
  } else {
    prob<-samplesubmission$Pred[samplesubmission$Team==team2 & samplesubmission$OPP==team1]
    if(runif(1)>prob){
      winner<-team1
      loser<-team2
    } else{
      
      winner<-team2
      loser<-team1
    }
  }
  
  # advances-to, winner,loser
  list(TourneySlots$Slot[row], winner,loser)
}
getActualResult<-function(row){
  
  team1<-as.numeric(resultDF$Team[resultDF$Slot==TourneySlots$StrongSeed[row]])
  team2<-as.numeric(resultDF$Team[resultDF$Slot==TourneySlots$WeakSeed[row]]) 
  
  actual<-NCAATourneyDetailedResults[NCAATourneyDetailedResults$WTeamID%in% c(team1, team2)& 
                                       NCAATourneyDetailedResults$LTeamID%in% c(team1, team2) & NCAATourneyDetailedResults$Season==year,]
  
  if(actual$WTeamID==team1){
    winner<-team1
    loser<-team2
  } else if(actual$WTeamID==team2){
    winner<-team2
    loser<-team1
  }
  
  # advances-to, winner,loser
  list(TourneySlots$Slot[row], winner,loser)
  
}


##SIMULATE TOURNAMENT#####

tourneySims<-list();length(tourneySims)<-sims
for(j in 1:sims){  
  #go through matchups sequentially and store results to resultDF
  
  resultDF<-data.frame(Slot=TourneySeeds$Seed[TourneySeeds$Season==year], 
                       Team=TourneySeeds$Team[TourneySeeds$Season==year],
                       Loser=NA, Payout=NA)
  
  for(row in 1:nrow(TourneySlots)){
    
    result<-getSimResult(row)
    resultDF[nrow(resultDF)+1,1:3]<-c(result[[1]], result[[2]], result[[3]])
  }
  resultDF$Sim<-j
  print(j)
  tourneySims[[j]]<-resultDF
}

if(backtest==T){
  
  resultDF<-data.frame(Slot=TourneySeeds$Seed[TourneySeeds$Season==year], 
                       Team=TourneySeeds$Team[TourneySeeds$Season==year],
                       Loser=NA, Payout=NA)
  
  for(row in 1:nrow(TourneySlots)){
    
    result<-getActualResult(row)
    resultDF[nrow(resultDF)+1,1:3]<-c(result[[1]], result[[2]], result[[3]])
  }
  resultDF$Sim<-sims+1
  tourneySims[[length(tourneySims)+1]]<-resultDF 
}

######SAVE DATA###########

tourneySims<-ldply(tourneySims, data.frame)
tourneySims$team_seed<-as.numeric(gsub("\\D", "",TourneySeeds$Seed[TourneySeeds$Season==year][match(tourneySims$Team,TourneySeeds$TeamID[TourneySeeds$Season==year])] ))
tourneySims$loser_seed<-as.numeric(gsub("\\D", "",TourneySeeds$Seed[TourneySeeds$Season==year][match(tourneySims$Loser,TourneySeeds$TeamID[TourneySeeds$Season==year])] ))
tourneySims$Round<-substr(tourneySims$Slot, 1, 2)
tourneySims$Round[grepl("W|X|Y|Z", tourneySims$Round)]<-0
tourneySims<-tourneySims[as.numeric(gsub("R", "",tourneySims$Round))>=1,]
tourneySims$Team_Full<-Teams$Team_Full[match(tourneySims$Team, Teams$TeamID)]


#line up with espn ownership data
if( year==2018){
  tourneySims$Team_Full[tourneySims$Team_Full%in% c("Arizona State", "Syracuse")]<-"Asu/sy"
  tourneySims$Team_Full[tourneySims$Team_Full%in% c("St Bonaventure", "Ucla")]<-"Bon/la"
  tourneySims$Team_Full[tourneySims$Team_Full%in% c("Long Island", "Radford")]<-"Liu/rad"
  tourneySims$Team_Full[tourneySims$Team_Full%in% c("North Carolina Central", "Texas Southern")]<-"Ncc/ts"
} else if (year==2019){
  tourneySims$Team_Full[tourneySims$Team_Full%in% c("Arizona State", "St Johns")]<-"Asu/sju"
  tourneySims$Team_Full[tourneySims$Team_Full%in% c("Belmont", "Temple")]<-"Bel/tem"
  tourneySims$Team_Full[tourneySims$Team_Full%in% c("Fairleigh Dickinson", "Prairie View A&m")]<-"Fdu/pv"
  tourneySims$Team_Full[tourneySims$Team_Full%in% c("North Dakota State", "North Carolina Central")]<-"Nds/ncc"
}else if (year==2021){
  tourneySims$Team_Full[tourneySims$Team_Full%in% c("Michigan State", "Ucla")]<-"Msu/ucla"
  tourneySims$Team_Full[tourneySims$Team_Full%in% c("Norfolk State", "Appalachian State")]<-"Norf/app"
  tourneySims$Team_Full[tourneySims$Team_Full%in% c("Wichita State", "Drake")]<-"Wich/drke"
  tourneySims$Team_Full[tourneySims$Team_Full%in% c("Mount St Marys", "Texas Southern")]<-"Msm/txso"
}else if (year==2022){
  tourneySims$Team_Full[tourneySims$Team_Full%in% c("Texas Southern", "Texas A&m Corpus Christi")]<-"Txso/tcc"
  tourneySims$Team_Full[tourneySims$Team_Full%in% c("Wright State", "Bryant")]<-"Wrst/bry"
  tourneySims$Team_Full[tourneySims$Team_Full%in% c("Rutgers", "Notre Dame")]<-"Rutg/nd"
  tourneySims$Team_Full[tourneySims$Team_Full%in% c("Wyoming", "Indiana")]<-"Wyo/iu"
}else if (year==2023){
  tourneySims$Team_Full[tourneySims$Team_Full%in% c("Southeast Missouri State", "Texas A&m Corpus Christi")]<-"Amcc/smo"
  tourneySims$Team_Full[tourneySims$Team_Full%in% c("Texas Southern", "Fairleigh Dickinson")]<-"Txso/fdu"
  tourneySims$Team_Full[tourneySims$Team_Full%in% c("Arizona State", "Nevada")]<-"Asu/nev"
  tourneySims$Team_Full[tourneySims$Team_Full%in% c("Mississippi State", "Pittsburgh")]<-"Msst/pitt"
}

save(tourneySims, file=name)

##inspect simulation results
#name<-paste0(year,"/TourneySims_", sims,"sims.Rda")
load(name)
inspect<-as.data.frame.matrix(table(tourneySims$Team_Full[tourneySims$Sim<=sims], tourneySims$Round[tourneySims$Sim<=sims])/sims) 


analyze<-TourneySeeds[TourneySeeds$Season==year, ]
analyze$Team_Full<-Teams$Team_Full[match(analyze$Team, Teams$TeamID)]
if(year==2018){
  # analyze$Team_Full[analyze$Team_Full%in% c("Arizona State", "Syracuse")]<-"Asu/sy"
  # analyze$Team_Full[analyze$Team_Full%in% c("St Bonaventure", "Ucla")]<-"Bon/la"
  # analyze$Team_Full[analyze$Team_Full%in% c("Long Island", "Radford")]<-"Liu/rad"
  # analyze$Team_Full[analyze$Team_Full%in% c("North Carolina Central", "Texas Southern")]<-"Ncc/ts"
} else if (year==2019){
  analyze$Team_Full[analyze$Team_Full%in% c("Arizona State", "St Johns")]<-"Asu/sju"
  analyze$Team_Full[analyze$Team_Full%in% c("Belmont", "Temple")]<-"Bel/tem"
  analyze$Team_Full[analyze$Team_Full%in% c("Fairleigh Dickinson", "Prairie View A&m")]<-"Fdu/pv"
  analyze$Team_Full[analyze$Team_Full%in% c("North Dakota State", "North Carolina Central")]<-"Nds/ncc"
} else if(year==2021){
  analyze$Team_Full[analyze$Team_Full%in% c("Michigan State", "Ucla")]<-"Msu/ucla"
  analyze$Team_Full[analyze$Team_Full%in% c("Norfolk State", "Appalachian State")]<-"Norf/app"
  analyze$Team_Full[analyze$Team_Full%in% c("Wichita State", "Drake")]<-"Wich/drke"
  analyze$Team_Full[analyze$Team_Full%in% c("Mount St Marys", "Texas Southern")]<-"Msm/txso"
}else if (year==2022){
  analyze$Team_Full[analyze$Team_Full%in% c("Texas Southern", "Texas A&m Corpus Christi")]<-"Txso/tcc"
  analyze$Team_Full[analyze$Team_Full%in% c("Wright State", "Bryant")]<-"Wrst/bry"
  analyze$Team_Full[analyze$Team_Full%in% c("Rutgers", "Notre Dame")]<-"Rutg/nd"
  analyze$Team_Full[analyze$Team_Full%in% c("Wyoming", "Indiana")]<-"Wyo/iu"
}else if (year==2023){
  analyze$Team_Full[analyze$Team_Full%in% c("Southeast Missouri State", "Texas A&m Corpus Christi")]<-"Amcc/smo"
  analyze$Team_Full[analyze$Team_Full%in% c("Texas Southern", "Fairleigh Dickinson")]<-"Txso/fdu"
  analyze$Team_Full[analyze$Team_Full%in% c("Arizona State", "Nevada")]<-"Asu/nev"
  analyze$Team_Full[analyze$Team_Full%in% c("Mississippi State", "Pittsburgh")]<-"Msst/pitt"
}
# 


names<-unique(analyze[, c("Team_Full", "Seed")])
names$Seed<-as.numeric(substring(names$Seed, 2, 3))
pasteSeed<-function(teams){
  paste(names$Seed[match( teams, names$Team_Full)], teams, sep=" ")
}    
inspect<-as.data.frame.matrix(table(tourneySims$Team_Full[tourneySims$Sim<=sims], tourneySims$Round[tourneySims$Sim<=sims])/sims) 
row.names(inspect)<-pasteSeed( row.names(inspect))
inspect[order(inspect$R6+inspect$R5,inspect$R4,inspect$R3, inspect$R2,  decreasing = T), ]#[1:20,]
