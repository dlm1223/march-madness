##functions
library(plyr);library(dplyr);library(ggplot2);library(ggrepel)

makePlot<-function(means, title="ROI by Optimization Percentile and numBrackets"){
  a<-ggplot(data=means, aes(fill=numBrackets, y=ROI, x=percentile)) +  
    geom_bar(position="dodge", stat="identity"    ,
             colour="black", # Use black outlines,
             size=.3) +      # Thinner lines
    geom_errorbar(aes(ymin = ROI - seROI, ymax = ROI + seROI),                
                  size=.3,    # Thinner lines
                  width=.2,
                  position=position_dodge(.9)) + 
    ylab("ROI (%)") +
    ggtitle(title)
  print(a)
}
groupedLine<-function(test, title="Cumulative Profit"){
  test[, c("percentile", "numBrackets")]<-sapply(test[, c("percentile", "numBrackets")], as.factor)
  
  a<-test%>% mutate(label = if_else(year == max(year), paste0(winningYears, "/8 Winning"), ifelse(year==min(year), numBrackets, NA_character_))) %>%
    ggplot( aes(x=year, y=CumProfit, group = numBrackets, colour = numBrackets)) +
    geom_line()+
    scale_colour_discrete(guide = 'none') +
    geom_label_repel(aes(label = label),
                     nudge_x = 1,
                     na.rm = TRUE)+
    ggtitle(title)
  print(a)
}


#winner take all####
load("backtest results AltScoring.Rda")
head(backtest)
backtest$numWinning<-rowSums(apply(backtest[, grepl("result", colnames(backtest))],2, function(x) x>=backtest$percentile  ), na.rm=T)
backtest$numSecond<-rowSums(apply(backtest[, grepl("result", colnames(backtest))],2, function(x) x<backtest$percentile & x>=backtest$percentile-(1-backtest$percentile) ), na.rm=T)
backtest$Prize<-(1/(1-backtest$percentile))*backtest$numWinning
backtest$Prize_same<-(1/(1-backtest$percentile))*(backtest$numWinning>=1)
backtest$Entry<-1*backtest$numBrackets


means<-ddply(backtest, .(percentile, numBrackets),
             summarize,ROI=100* sum(Prize_same-Entry)/sum(Entry), seROI=100*sd((Prize_same-Entry)/Entry)/length(year))
means[, c("percentile", "numBrackets")]<-sapply(means[, c("percentile", "numBrackets")], as.factor)
makePlot(means)

backtest<-ddply(backtest, .(numBrackets, percentile),mutate,
                winningYears=sum(Prize_same>Entry),
                CumProfit=cumsum(Prize_same)-cumsum(Entry))
test<-backtest[backtest$percentile==.9 ,]
groupedLine(test, title="Cumulative Profit, 90th-percentile parameter")

##custom payout structure###

#75% to first, 25% to second

backtest$Prize<-(.75/(1-backtest$percentile))*backtest$numWinning+(.25/(1-backtest$percentile))*backtest$numSecond
backtest$Prize_same<-(.75/(1-backtest$percentile))*(backtest$numWinning>=1)+(.25/(1-backtest$percentile))*(backtest$numSecond>=1 | backtest$numWinning>=2)
backtest$Entry<-1*backtest$numBrackets


means<-ddply(backtest, .(percentile, numBrackets), summarize,ROI=100* sum(Prize_same-Entry)/sum(Entry), seROI=100*sd((Prize_same-Entry)/Entry)/length(year))
means[, c("percentile", "numBrackets")]<-sapply(means[, c("percentile", "numBrackets")], as.factor)

makePlot(means)

backtest<-ddply(backtest, .(numBrackets, percentile),mutate,
                winningYears=sum(Prize_same>Entry),
                CumProfit=cumsum(Prize_same)-cumsum(Entry))
test<-backtest[backtest$percentile==.99,]
groupedLine(test, title="Cumulative Profit, 90th-percentile parameter")




##apply medium pool scoring to all, winner take all###
percentile<-.95

backtest$numWinning<-rowSums(apply(backtest[, grepl("result", colnames(backtest))],2, function(x) x>=percentile  ), na.rm=T)
backtest$numSecond<-rowSums(apply(backtest[, grepl("result", colnames(backtest))],2, function(x) x<percentile & x>=percentile-(1-percentile) ), na.rm=T)
backtest$Prize<-(1/(1-percentile))*backtest$numWinning
backtest$Prize_same<-(1/(1-percentile))*(backtest$numWinning>=1)


means<-ddply(backtest, .(percentile, numBrackets), summarize,ROI=100*sum(Prize_same-Entry)/sum(Entry), seROI=100*sd((Prize_same-Entry)/Entry)/length(year))
means[, c("percentile", "numBrackets")]<-sapply(means[, c("percentile", "numBrackets")], as.factor)
makePlot(means, title="ROI, Medium Pool Scoring")

backtest<-ddply(backtest, .(numBrackets, percentile),mutate,
                winningYears=sum(Prize_same>Entry),
                CumProfit=cumsum(Prize_same)-cumsum(Entry))
test<-backtest[backtest$percentile==.95 ,]
groupedLine(test, title="Cumulative Profit, 95th-percentile parameter")



##medium-pool ,custom scoring###

backtest$Prize<-(.75/(1-percentile))*backtest$numWinning+(.25/(1-percentile))*backtest$numSecond
backtest$Prize_same<-(.75/(1-percentile))*(backtest$numWinning>=1)+(.25/(1-percentile))*(backtest$numSecond>=1 | backtest$numWinning>=2)

means<-ddply(backtest, .(percentile, numBrackets), summarize,ROI=100* sum(Prize_same-Entry)/sum(Entry), seROI=100*sd((Prize_same-Entry)/Entry)/length(year))
means[, c("percentile", "numBrackets")]<-sapply(means[, c("percentile", "numBrackets")], as.factor)

makePlot(means, title="ROI, Medium Pool Custom Scoring")


backtest<-ddply(backtest, .(numBrackets, percentile),mutate,
                winningYears=sum(Prize_same>Entry),
                CumProfit=cumsum(Prize_same)-cumsum(Entry))
test<-backtest[backtest$percentile==.95,]
groupedLine(test, title="Cumulative Profit, 95th-percentile parameter, Custom Scoring")

unique(backtest$Type)
# file<-"backtest results AltScoring UpsetsDoubled.Rda"
files<-list.files(pattern="backtest results")
files<-files[!grepl("v2", files)]

getFiles<-function(file){
  load(file)
  backtest$Type<-gsub("backtest results |[.]Rda","", file)
  backtest$Type
  backtest
}

backtest<-ldply(lapply(files, getFiles), data.frame)
backtest$Type[backtest$Type=="UpsetsDoubled"]<-"Exponential UpsetsDoubled"
backtest$Type[backtest$Type=="backtest results"]<-"Exponential"
backtest$Type<-gsub("Exponential", "Exp", backtest$Type)
backtest$Type<-gsub("Arithmetic", "Arith", backtest$Type)
backtest$numWinning<-rowSums(apply(backtest[, grepl("result", colnames(backtest))],2, function(x) x>=percentile  ), na.rm=T)
backtest$numSecond<-rowSums(apply(backtest[, grepl("result", colnames(backtest))],2, function(x) x<percentile & x>=percentile-(1-percentile) ), na.rm=T)
backtest$Prize<-(1/(1-backtest$percentile))*backtest$numWinning
backtest$Prize_same<-(1/(1-backtest$percentile))*(backtest$numWinning>=1)


backtest<-backtest[backtest$numBrackets==1,]

means<-ddply(backtest, .(Type, percentile, numBrackets),summarize,  expectedNumLosses=sum(1-probWin),expectedNumWins=sum(probWin),
             expectedNumWins=sum(probWin),expectedProfit=sum(probWin*1/(1-percentile)))
means$numBrackets<-as.factor(means$numBrackets)
means$percentile<-as.factor(means$percentile)
ggplot(data=means, aes(fill=percentile, y=expectedNumWins, x=Type)) +  
  geom_bar(position="dodge", stat="identity"    ,
           colour="black", # Use black outlines,
           size=.3) +      # Thinner lines
  ggtitle("Expected Wins by Scoring, Winner Take All")

means<-ddply(backtest, .(Type, percentile, numBrackets),summarize,expectedNumLosses=sum((1-probWin)*(1-probSecond)), 
             expectedNumWins=sum(1-(1-probWin)*(1-probSecond)) 
             ,expectedProfit=sum(probWin*.75/(1-percentile)+probSecond*.25/(1-percentile)-1))
means$numBrackets<-as.factor(means$numBrackets)
means$percentile<-as.factor(means$percentile)
ggplot(data=means, aes(fill=percentile, y=expectedNumWins, x=Type)) +  
  geom_bar(position="dodge", stat="identity"    ,
           colour="black", # Use black outlines,
           size=.3) +      # Thinner lines
  ggtitle("Expected Wins By Scoring, 75% First & 25% Second")


means<-ddply(backtest, .(Type, percentile, numBrackets),summarize,expectedNumLosses=sum((1-probWin)*(1-probSecond)*(1-probThird)), 
             expectedNumWins=sum(1-(1-probWin)*(1-probSecond)*(1-probThird)), 
             expectedProfit=sum(probWin*.5/(1-percentile)+probSecond*.3/(1-percentile)+probThird*.2/(1-percentile)-1))
means$numBrackets<-as.factor(means$numBrackets)
means$percentile<-as.factor(means$percentile)
ggplot(data=means, aes(fill=percentile, y=expectedNumWins, x=Type)) +  
  geom_bar(position="dodge", stat="identity"    ,
           colour="black", # Use black outlines,
           size=.3) +      # Thinner lines
  ggtitle("Expected Wins By Pool Size, 50% 1st, 30% 2nd,20% 3rd")


