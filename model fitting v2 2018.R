# load("~/Kaggle/NCAA/game data.RData")
# year<-2016
# source("~/Kaggle/NCAA/functions.R")


cor(fulldf$Rank.POM, fulldf[, grepl("Rank[.]|med|min|max|mean", colnames(fulldf))], use="pairwise.complete.obs")
fulldf$meanRank_alt<-rowMeans(fulldf[, c("Rank.POM", "Rank.NOL","Rank.RTP", "Rank.DOL" )], na.rm=T)


fulldf$OPPmeanRank_alt<-rowMeans(fulldf[, c("OPP.POM", "OPP.NOL","OPP.RTP", "OPP.DOL" )], na.rm=T)

table(fulldf$Season, fulldf$Tournament)
table(fulldf$Season[!is.na(fulldf$Spread)], fulldf$Tournament[!is.na(fulldf$Spread)])

test<-fulldf[fulldf$Tournament==1 & fulldf$Season%in%year& fulldf$Team<fulldf$OPP&
               fulldf$DATE!=fulldf$Rank_DATE , ]

train<-fulldf[(fulldf$Season<min(year) | (fulldf$Tournament==0 & fulldf$Season==min(year))) & 
                fulldf$Rank_DATE!=fulldf$DATE & !is.na(rowSums(fulldf[, c("OPP.POM", "Rank.POM", "Spread")])), ]

###model 1####
fit<-glm(Win_factor~I(Rank.MOR-OPP.MOR)#+I(Rank.POM-OPP.POM)+Spread+I(Spread^2)+I(sign(Spread))
         ,  data=train, family="binomial");summary(fit)
test$predWin<-predict(fit, newdata=test, type="response")# type="prob")[, 2]
train$predWin<-predict(fit, newdata=train, type="response")#,type="prob")[, 2]
logLoss(train$Win, train$predWin)

###model 2###
fit2<-glm(Win_factor~Spread+I(Spread^2)+I(sign(Spread)),
          family="binomial",
          data=train);summary(fit2)
test$predWin2<-predict(fit2, newdata=test, type="response")
train$predWin2<-predict(fit2, newdata=train, type="response")

#model 3##
fit3<-glm(Win_factor~I(log(TeamSeed_num/OPPSeed_num))+I((Rank.MOR-OPP.MOR)),
         data=train[train$Tournament==1,], family="binomial");summary(fit3)
train$predWin3<-predict(fit3, newdata=train, type="response")

test$predWin3<-predict(fit3, newdata=test, type="response")
logLoss(test$Win, test$predWin3)

##model 4####
fit4<-glm(Win_factor~I((Rank.MOR-OPP.MOR))+Teamloc+ #+#^3)+
            I(log(Rank.MOR/OPP.MOR)),
          family="binomial", data=train);summary(fit4)
test$predWin4<-predict(fit4, newdata=test, type="response")
train$predWin4<-predict(fit4, newdata=train, type="response")
logLoss(test$Win, test$predWin4)

logLoss(test$Win, .5)
logLoss(test$Win, test$predWin4)  #Rank2
logLoss(test$Win, test$predWin3)  # Rank1
logLoss(test$Win, test$predWin2)  #Spread
logLoss(test$Win, test$predWin)  #Spread +Rank

logLoss(train$Win[train$Tournament==1], rowMeans(train[train$Tournament==1, c("predWin3", "predWin4")]))
logLoss(train$Win[train$Tournament==1], rowMeans(train[train$Tournament==1, c("predWin4", "predWin2", "predWin")]))

train$Pred<-ifelse(train$Round==1, rowMeans(train[, c("predWin", "predWin2", "predWin4")], na.rm=T), rowMeans(train[, c("predWin3", "predWin4")], na.rm=T))
logLoss(train$Win[train$Round>=1& train$Tournament==1], train$Pred[train$Round>=1& train$Tournament==1])

test$Pred<-ifelse(test$Round==1, rowMeans(test[, c("predWin", "predWin2")], na.rm=T), rowMeans(test[, c("predWin3", "predWin4")], na.rm=T))
logLoss(test$Win[test$Round>=1& test$Team<test$OPP], test$Pred[test$Round>=1& test$Team<test$OPP])

# test$Pred2<-ifelse(abs(test$OPPSeed_num-test$TeamSeed_num)==16, round(test$Pred), test$Pred)
# logLoss(test$Win[test$Round>=1& test$Team<test$OPP], test$Pred2[test$Round>=1& test$Team<test$OPP])

test$Pred3<-ifelse(test$Round>=6, test$Win,  test$Pred)
logLoss(test$Win[test$Round>=1& test$Team<test$OPP], test$Pred3[test$Round>=1& test$Team<test$OPP])




######write predictions########
dates<-c(seq(as.Date("2014-03-18"), as.Date("2014-03-21"), 1), 
         seq(as.Date("2015-03-17"), as.Date("2015-03-20"), 1),
         seq(as.Date("2016-03-15"), as.Date("2016-03-18"), 1), 
         seq(as.Date("2017-03-14"), as.Date("2017-03-17"), 1))


if(year==2017){
  samplesubmission<-read.csv("data/SampleSubmission.csv")
  samplesubmission$Team<-as.numeric(sapply(strsplit(samplesubmission$Id, "_"), `[[`, 2))
  samplesubmission$OPP<-as.numeric(sapply(strsplit(samplesubmission$Id, "_"), `[[`, 3))
  samplesubmission$Season<-as.numeric(sapply(strsplit(samplesubmission$Id, "_"), `[[`,1))
  
} else{
  bool<-fulldf$Season==year& fulldf$Tournament==1
  samplesubmission<-data.frame(Pred=.5, Team=rep(unique(fulldf$Team[bool]), 
                                                 each=length(unique(fulldf$Team[bool]))), 
                               OPP=rep(unique(fulldf$Team[bool]), 
                                       times=length(unique(fulldf$Team[bool]))), 
                               Season=year)
  samplesubmission<-samplesubmission[samplesubmission$Team<samplesubmission$OPP, ]
  samplesubmission$Id<-apply(samplesubmission[, c("Season", "Team", "OPP")], 1, paste, sep="", collapse="_")
}
odds2<-oddsDF[oddsDF$DATE%in% dates & as.numeric(substring(as.character(oddsDF$DATE), 1, 4))==year, ] #first round games
odds2$team_id<-id_df$team_id[match(odds2$Team, id_df$Team_Full)]
odds2$opp_id<-id_df$team_id[match(odds2$OPP, id_df$Team_Full)]
samplesubmission<-merge(samplesubmission, odds2[,c("team_id", "opp_id", "Spread")], 
                        by.x=c("Team", "OPP"), by.y=c("team_id", "opp_id"), all.x=T  )

fulldf<-fulldf[order(fulldf$DATE, decreasing = F), ]
team_stats<-ddply(fulldf[fulldf$Tournament==1, ], .(Team,Team_Full, Season), summarize,
                  meanRank=meanRank[1], Rank.MOR=Rank.MOR[1], Teamloc="N", TeamSeed=TeamSeed[1], TeamSeed_num=TeamSeed_num[1]
)
samplesubmission<-merge(samplesubmission,team_stats, by=c("Team", "Season"))
colnames(team_stats)<-c("OPP", "OPP_Full", "Season", "OPPmeanRank", "OPP.MOR", "OPPloc", "OPPSeed", "OPPSeed_num")
samplesubmission<-merge(samplesubmission,team_stats, by=c("OPP", "Season"))
samplesubmission$Round<-sapply(1:nrow(samplesubmission), function(x)getRound(samplesubmission$TeamSeed[x], samplesubmission$OPPSeed[x], samplesubmission$Season[x]))
samplesubmission$predWin<-predict(fit, newdata=samplesubmission, type="response")
samplesubmission$predWin2<-predict(fit2, newdata=samplesubmission, type="response")
samplesubmission$predWin3<-predict(fit3, newdata=samplesubmission, type="response")
samplesubmission$predWin4<-predict(fit4, newdata=samplesubmission, type="response")
samplesubmission$Pred<-ifelse(!is.na(samplesubmission$Spread), rowMeans(samplesubmission[, c("predWin", "predWin2")], na.rm=T),
                              rowMeans(samplesubmission[, c("predWin3", "predWin4")], na.rm=T))


cor(samplesubmission[, grepl("pred", colnames(samplesubmission))], use="pairwise.complete.obs")
hist(samplesubmission$predWin)
hist(samplesubmission$Pred)

#write to a csv file

seed.lm.submission <- data.frame(id=samplesubmission$Id, pred=samplesubmission$Pred)
head(seed.lm.submission)

# write.csv(seed.lm.submission, file="Kaggle Submission.csv", row.names=FALSE)







