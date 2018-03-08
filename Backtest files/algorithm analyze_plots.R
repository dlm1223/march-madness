
##backtest results- regular winner take all######
head(backtest)
backtest$numWinning<-rowSums(apply(backtest[, grepl("result", colnames(backtest))],2, function(x) x>=backtest$percentile  ), na.rm=T)
backtest$Prize<-(1/(1-backtest$percentile))*backtest$numWinning
backtest$Prize_same<-(1/(1-backtest$percentile))*(backtest$numWinning>=1)
backtest$Entry<-1*backtest$numBrackets

backtest[, c("percentile", "numBrackets")]<-sapply(backtest[, c("percentile", "numBrackets")], as.factor)

means<-ddply(backtest, .(percentile, numBrackets), summarize,ROI=100* sum(Prize_same-Entry)/sum(Entry), seROI=100*sd((Prize_same-Entry)/Entry)/length(year))

makePlot(means)

#+ scale_fill_discrete(name = "New Legend Title")
backtest<-ddply(backtest, .(numBrackets, percentile),mutate,
                winningYears=sum(Prize_same>0),
                CumProfit=cumsum(Prize_same)-cumsum(Entry))
test<-backtest[backtest$percentile==.9,]
groupedLine(test)

# bool<-backtest$numBrackets==3& backtest$percentile==.9



##custom payout structure###

#75% to first, 25% to second
backtest$numSecond<-rowSums(apply(backtest[, grepl("result", colnames(backtest))],2, function(x) x<backtest$percentile & x>=backtest$percentile-(1-backtest$percentile) ), na.rm=T)

backtest$Prize<-(.75/(1-backtest$percentile))*backtest$numWinning+(.25/(1-backtest$percentile))*backtest$numSecond
backtest$Prize_same<-(.75/(1-backtest$percentile))*(backtest$numWinning>=1)+(.25/(1-backtest$percentile))*(backtest$numSecond>=1 | backtest$numWinning>=2)
backtest$Entry<-1*backtest$numBrackets


means<-ddply(backtest, .(percentile, numBrackets), summarize,ROI=100* sum(Prize_same-Entry)/sum(Entry), seROI=100*sd((Prize_same-Entry)/Entry)/length(year))
means[, c("percentile", "numBrackets")]<-sapply(means[, c("percentile", "numBrackets")], as.factor)

makePlot(means)
bool<-backtest$numBrackets==1& backtest$percentile==.9

makeLine(bool, backtest)

##apply medium pool scoring to all, winner take all###
percentile<-.95

backtest$numWinning<-rowSums(apply(backtest[, grepl("result", colnames(backtest))],2, function(x) x>=percentile  ), na.rm=T)
backtest$numSecond<-rowSums(apply(backtest[, grepl("result", colnames(backtest))],2, function(x) x<percentile & x>=percentile-(1-percentile) ), na.rm=T)
backtest$Prize<-(1/(1-percentile))*backtest$numWinning
backtest$Prize_same<-(1/(1-percentile))*(backtest$numWinning>=1)


means<-ddply(backtest, .(percentile, numBrackets), summarize,ROI=100*sum(Prize_same-Entry)/sum(Entry), seROI=100*sd((Prize_same-Entry)/Entry)/length(year))
means[, c("percentile", "numBrackets")]<-sapply(means[, c("percentile", "numBrackets")], as.factor)
makePlot(means, title="ROI, Small Pool Scoring")

backtest<-ddply(backtest, .(numBrackets, percentile),mutate,
                winningYears=sum(Prize_same>0),
                CumProfit=cumsum(Prize_same)-cumsum(Entry))
test<-backtest[backtest$percentile==.9,]
groupedLine(test)

# bool<-test$numBrackets==3& test$percentile==.9
# makeLine(bool, test)


##medium-pool ,custom scoring###
backtest$Prize<-(.75/(1-percentile))*backtest$numWinning+(.25/(1-percentile))*backtest$numSecond
backtest$Prize_same<-(.75/(1-percentile))*(backtest$numWinning>=1)+(.25/(1-percentile))*(backtest$numSecond>=1 | backtest$numWinning>=2)

means<-ddply(backtest, .(percentile, numBrackets), summarize,ROI=100* sum(Prize_same-Entry)/sum(Entry), seROI=100*sd((Prize_same-Entry)/Entry)/length(year))
means[, c("percentile", "numBrackets")]<-sapply(means[, c("percentile", "numBrackets")], as.factor)

makePlot(means, title="ROI, Medium Pool Custom Scoring")

bool<-test$numBrackets==1& test$percentile==.9
makeLine(bool, test)

