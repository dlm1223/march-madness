#Pool Size and March Madness: Backtesting March Madness Strategies


###run backtest########
testGrid<-expand.grid(year=2010:2017, percentile=c(.9, .95, .99), numBrackets=c(1, 3, 6))

backtest<-lapply(1:nrow(testGrid), function(x){
  year<-testGrid$year[x]
  percentile<-testGrid$percentile[x];numBrackets<-testGrid$numBrackets[x]
  
  
  setwd(paste0(c(projDir, "/Shiny/", year, "/"), sep="", collapse=""))
  load("alldata.RData")
  load("BracketResults_FullTournament_500sims.Rda")
  load("TourneySims_500sims.Rda")
  
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
save(backtest, file=paste0(projDir,"/Backtest files/backtest results.Rda"))
