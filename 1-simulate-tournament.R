
##to run, can change number of sims, year, name of file to save tourneysims
#then run sim brackets file, where you can change numBrackets and file name to save brackets
#then run optimize brackets file where you can optimize brackets

##SET PARAMETERS/READ DATA######

year<-2019
sims<-1000
name<-paste0(year,"/TourneySims_", sims,"sims.Rda")
backtest<-ifelse(year==2019, F, T)
playInTbd<-T  

load("data/game-data.RData")
source("functions.R", encoding = "UTF-8")

#if you use kaggle data files, this is where you could put your own projections...id like to add this to the shiny app but it's just not gonna happen
samplesubmission<-read.csv(paste0(year, "/Kaggle Submission.csv"), stringsAsFactors = F) 
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


#handling play-in games, impute actual into simulation prediction iff game has occured
losing_teams<-c()
if(year==2018){
  losing_teams<-c("Ucla", "Long Island", "Arizona State", "North Carolina Central")  #
} else if (year==2019){
  losing_teams<-c("Prairie View A&m", "Temple", "North Carolina Central", "St Johns")
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
  if(j%%100==0){
    print(j)
  }
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
if(playInTbd==T & year==2018){
  tourneySims$Team_Full[tourneySims$Team_Full%in% c("Arizona State", "Syracuse")]<-"Asu/sy"
  tourneySims$Team_Full[tourneySims$Team_Full%in% c("St Bonaventure", "Ucla")]<-"Bon/la"
  tourneySims$Team_Full[tourneySims$Team_Full%in% c("Long Island", "Radford")]<-"Liu/rad"
  tourneySims$Team_Full[tourneySims$Team_Full%in% c("North Carolina Central", "Texas Southern")]<-"Ncc/ts"
} else if (playInTbd==T & year==2019){
  tourneySims$Team_Full[tourneySims$Team_Full%in% c("Arizona State", "St Johns")]<-"Asu/sju"
  tourneySims$Team_Full[tourneySims$Team_Full%in% c("Belmont", "Temple")]<-"Bel/tem"
  tourneySims$Team_Full[tourneySims$Team_Full%in% c("Fairleigh Dickinson", "Prairie View A&m")]<-"Fdu/pv"
  tourneySims$Team_Full[tourneySims$Team_Full%in% c("North Dakota State", "North Carolina Central")]<-"Nds/ncc"
}

save(tourneySims, file=name)


##inspect results
inspect<-as.data.frame.matrix(table(tourneySims$Team_Full[tourneySims$Sim<=sims], tourneySims$Round[tourneySims$Sim<=sims])/sims) 
inspect[order(inspect$R6,inspect$R5,inspect$R4,inspect$R3, inspect$R2,  decreasing = T), ]




