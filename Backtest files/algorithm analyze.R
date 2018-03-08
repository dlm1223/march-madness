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



#Pool Size and March Madness: Backtesting March Madness Strategies


###run backtest########
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
