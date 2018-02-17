year<-2017
backtest<-T
sims<-5000
playInTbd<-F
name<-"TourneySims.Rda"

setwd("~/Kaggle/NCAA/march-madness")
# projDir<-getwd()
# 
load("data/game data.RData")
source("functions.R")
source("model fitting 2018.R")
setwd(paste0(c( year, "/"), sep="", collapse=""))
list.files()

TourneySlots[TourneySlots$Season==year,]


#handling play-in games, impute actual into simulation prediction iff game has occured
losing_teams<-fulldf$Team_Full[grepl("a|b", fulldf$TeamSeed)& fulldf$Season==year & fulldf$Tournament==1 & grepl("a|b", fulldf$OPPSeed)& fulldf$Win==0]
if(length(losing_teams)>=1){
  samplesubmission$Pred[samplesubmission$Team_Full%in%losing_teams]<-0
  samplesubmission$Pred[samplesubmission$OPP_Full%in%losing_teams]<-1
}


###RUN SIMULATIONS####

advance<-function(row){
  # row<-TourneySlots[TourneySlots$Season==year,][5,]
  #  newdata<-data.frame(Slot=TourneySeeds$Seed[TourneySeeds$Season==year], Team=as.numeric(TourneySeeds$Team[TourneySeeds$Season==year]), Payout=0)
  
  team1<-as.numeric(newdata$Team[newdata$Slot==row$Strongseed]) #as.numeric(gsub("\\D", "", row$Strongseed)) 
  team2<-as.numeric(newdata$Team[newdata$Slot==row$Weakseed])  #as.numeric(gsub("\\D", "", row$Weakseed)) 
  
  
  #simulate winner
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
  list(row$Slot, winner,loser)
  
}
advanceActual<-function(row){
  # row<-TourneySlots[TourneySlots$Season==year,][5,]
  #  newdata<-data.frame(Slot=TourneySeeds$Seed[TourneySeeds$Season==year], Team=as.numeric(TourneySeeds$Team[TourneySeeds$Season==year]), Payout=0)
  
  team1<-as.numeric(newdata$Team[newdata$Slot==row$Strongseed]) #as.numeric(gsub("\\D", "", row$Strongseed)) 
  team2<-as.numeric(newdata$Team[newdata$Slot==row$Weakseed])  #as.numeric(gsub("\\D", "", row$Weakseed)) 
  
  #simulate winner
  #find result with team1=Team and team2=OPP
  if(fulldf$Win[which(fulldf$Team==team1& fulldf$OPP==team2 & fulldf$Season==year &fulldf$Tournament==1)]==1){
    winner<-team1
    loser<-team2
  } else if(fulldf$Win[which(fulldf$Team==team1& fulldf$OPP==team2 & fulldf$Season==year &fulldf$Tournament==1)]==0){
    winner<-team2
    loser<-team1
  }
  
  # advances-to, winner,loser
  list(row$Slot, winner,loser)
  
}


tourneySims<-list();length(tourneySims)<-sims
for(j in 1:(sims)) {
  # set.seed(j)
  newdata<-data.frame(Slot=TourneySeeds$Seed[TourneySeeds$Season==year], 
                      Team=as.numeric(TourneySeeds$Team[TourneySeeds$Season==year]),
                      Loser=NA, Payout=NA)
  
  
  
  for(i in 1:nrow(TourneySlots[TourneySlots$Season==year,])){
    # i<-i+1
    row<-TourneySlots[TourneySlots$Season==year,][i,]
    result<-advance(row)
    newdata[nrow(newdata)+1,1:3]<-c(result[[1]], result[[2]], result[[3]])
  }
  newdata$Sim<-j
  print(j)
  # newdata
  tourneySims[[j]]<-newdata
}
if(backtest==T){
  j<-j+1
  newdata<-data.frame(Slot=TourneySeeds$Seed[TourneySeeds$Season==year], 
                      Team=as.numeric(TourneySeeds$Team[TourneySeeds$Season==year]),
                      Loser=NA, Payout=NA)
  
  for(i in 1:nrow(TourneySlots[TourneySlots$Season==year,])){
    # i<-i+1
    row<-TourneySlots[TourneySlots$Season==year,][i,]
    result<-advanceActual(row)
    newdata[nrow(newdata)+1,1:3]<-c(result[[1]], result[[2]], result[[3]])
  }
  newdata$Sim<-j
  print(j)
  # newdata
  tourneySims[[j]]<-newdata
  
  
}

tourneySims<-ldply(tourneySims, data.frame)


######SAVE DATA###########
save(tourneySims, file=name)

save(samplesubmission, file="samplesubmission.Rda")





