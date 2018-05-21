###SENSITIVITY ANALYSIS-COMPARE PROJECTIONS#####

#compare model 1 and model2
setwd(projDir)
readFile<-function(year, version=""){
  data<-read.csv(paste(c(year ,"/Kaggle Submission", version,".csv"), sep="", collapse=""))
  data$Year<-year
  if(version==" v2"){
    colnames(data)<-gsub("pred", "pred2", colnames(data))
  }
  data
}
submissions<-ldply(lapply(2010:2017, readFile), data.frame)
submissions2<-ldply(lapply(2010:2017, function(x) readFile(x, version=" v2")), data.frame)
compare<-merge(submissions, submissions2, by=c("id", "Year"))
cor(compare$pred, compare$pred2)
plot(compare$pred~compare$pred2)


setwd(projDir)
readFile<-function(year){
  load(paste(c(year ,"/TourneySims", version,".Rda"), sep="", collapse=""))
  load(paste(c(year ,"/alldata.RData"), sep="", collapse=""))
  tourneySims$Round<-substr(tourneySims$Slot, 1, 2)
  tourneySims$Round[grepl("W|X|Y|Z", tourneySims$Round)]<-0
  tourneySims<-tourneySims[as.numeric(gsub("R", "",tourneySims$Round))>=1,]
  tourneySims$Team_Full<-id_df$Team_Full[match(tourneySims$Team, id_df$TeamID)]
  numSims<-max(tourneySims$Sim)-1
  inspect<-as.data.frame.matrix(table(tourneySims$Team_Full[tourneySims$Sim<=numSims], tourneySims$Round[tourneySims$Sim<=numSims])/numSims) 
  inspect[order(inspect$R6,inspect$R5,inspect$R4,inspect$R3, inspect$R2,  decreasing = T), ]
  winner<-row.names(inspect)[which.max(inspect$R6)]
  actualWinner<-tourneySims[tourneySims$Sim==max(tourneySims$Sim) & tourneySims$Slot=="R6CH", "Team_Full"]
  
  load(paste(c(year ,"/TourneySims_v2.Rda"), sep="", collapse=""))
  tourneySims$Round<-substr(tourneySims$Slot, 1, 2)
  tourneySims$Round[grepl("W|X|Y|Z", tourneySims$Round)]<-0
  tourneySims<-tourneySims[as.numeric(gsub("R", "",tourneySims$Round))>=1,]
  tourneySims$Team_Full<-id_df$Team_Full[match(tourneySims$Team, id_df$TeamID)]
  numSims<-max(tourneySims$Sim)-1
  inspect<-as.data.frame.matrix(table(tourneySims$Team_Full[tourneySims$Sim<=numSims], tourneySims$Round[tourneySims$Sim<=numSims])/numSims) 
  inspect[order(inspect$R6,inspect$R5,inspect$R4,inspect$R3, inspect$R2,  decreasing = T), ]
  winner2<-row.names(inspect)[which.max(inspect$R6)]
  
  analyze<-TourneySeeds[TourneySeeds$Season==year, ]
  analyze$Team_Full<-id_df$Team_Full[match(analyze$Team, id_df$TeamID)]
  names<-unique(analyze[, c("Team_Full", "Seed")])
  names$Seed<-as.numeric(substring(names$Seed, 2, 3))
  pasteSeed<-function(teams){
    paste(names$Seed[match( teams, names$Team_Full)], teams, sep=" ")
  }
  data.frame(Year=year,projectedWinner_Model1=pasteSeed(winner), projectedWinner_Model2=pasteSeed(winner2),actualWinner=pasteSeed(actualWinner))
}
submissions<-ldply(lapply(2010:2017, readFile), data.frame)
submissions
save(submissions, file="Backtest files/compare.Rda")

#Pool Size and March Madness: Backtesting March Madness Strategies


###RUN BACKTEST, SPECIFYING SIMS AND BRACKETS########
testGrid<-expand.grid(year=2010:2017, percentile=c(.9, .95, .99), numBrackets=c(1, 3, 6))

backtest<-lapply(1:nrow(testGrid), function(x){
  year<-testGrid$year[x]
  percentile<-testGrid$percentile[x];numBrackets<-testGrid$numBrackets[x]
  
  
  setwd(paste0(c(projDir, "/", year, "/"), sep="", collapse=""))
  load("alldata.RData")
  load("BracketResults_FullTournament_500sims_v2.Rda")
  load("TourneySims_500sims_v2.Rda")
  
  source(paste0(c(projDir, "/Shiny/optimize brackets.R"), sep="", collapse=""), local = T)
  
  allResults<-brackets$Percentile501_Actual[which(result$x[1:ncol(percentiles)]==1)][order(brackets$Percentile501_Actual[which(result$x[1:ncol(percentiles)]==1)], decreasing = T)]
  
  data.frame(year=year,percentile=percentile, numBrackets=numBrackets,
             probWin=sum(result$x[(ncol(percentiles)+1):(ncol(percentiles)+numSims)])/numSims, 
             result=allResults[1], #1st place
             result2=allResults[2], #2nd place
             result3=allResults[3], 
             result4=allResults[4], 
             result5=allResults[5], 
             result6=allResults[6]  )
}
)

backtest<-ldply(backtest, data.frame)
save(backtest, file=paste0(projDir,"/Backtest files/backtest results v2.Rda"))


###RUN BACKTEST WITH DIFFERENT SCORING SYSTEMS####
testGrid<-expand.grid(year=2010:2017, percentile=c(.9, .95, .99), numBrackets=c(1, 3, 6))


library(Rsymphony)
input<-list(r1=10, r2=20, r3=30, r4=40, r5=50, r6=60, upset1_mult=1, upset2_mult=1, upset3_mult=1,
            r1_seed_mult=0, r2_seed_mult=0,r3_seed_mult=0, r4_seed_mult=0,r5_seed_mult=0, r6_seed_mult=0,
            r1_seed_bonus=0, r2_seed_bonus=0,r3_seed_bonus=0, r4_seed_bonus=0,r5_seed_bonus=0, r6_seed_bonus=0)

backtest<-lapply(1:nrow(testGrid), function(x){
  year<-testGrid$year[x]
  percentile<-testGrid$percentile[x];numBrackets<-testGrid$numBrackets[x]
  
  
  setwd(paste0(c(projDir, "/", year, "/"), sep="", collapse=""))
  load("alldata.RData")
  load("BracketResults_FullTournament_500sims.Rda")
  load("TourneySims_500sims.Rda")
  
  source(paste0(c(projDir, "/Shiny/simulate calc payouts_merge.R"), sep="", collapse=""), local = T)
  
  
  
  source(paste0(c(projDir, "/Shiny/optimize brackets.R"), sep="", collapse=""), local = T)
  
  allResults<-brackets$Percentile501_Actual[which(result$x[1:ncol(percentiles)]==1)][order(brackets$Percentile501_Actual[which(result$x[1:ncol(percentiles)]==1)], decreasing = T)]
  
  #second place and third place--calculate probabilities
  second<-1-2*(1-percentile)
  third<-1-3*(1-percentile)
  probWin<-sum(result$x[(ncol(percentiles)+1):(ncol(percentiles)+numSims)])/numSims
  probSecond<-sum(apply(brackets[which(result$x[1:500]==1),grepl("Percentile", colnames(brackets)) & !grepl("Actual", colnames(brackets))], 2, function(x) max(x>second)))/numSims-probWin
  probThird<-sum(apply(brackets[which(result$x[1:500]==1),grepl("Percentile", colnames(brackets)) & !grepl("Actual", colnames(brackets))], 2, function(x) max(x>third)))/numSims-probWin-probSecond
  
  data.frame(year=year,percentile=percentile, numBrackets=numBrackets,
             probWin=probWin, 
             probSecond=probSecond,
             probThird=probThird,
             result=allResults[1], #1st place
             result2=allResults[2], #2nd place
             result3=allResults[3], 
             result4=allResults[4], 
             result5=allResults[5], 
             result6=allResults[6]  )
}
)

backtest<-ldply(backtest, data.frame)
save(backtest, file=paste0(projDir,"/Backtest files/backtest results Arithmetic AddSeedR1.Rda"))


