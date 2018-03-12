# load("game data.RData")
# year<-2018
# source("functions.R")


###PREPROCESS#####

#inspect correlations
bool<-fulldf$Season==2017& !is.na(fulldf$Rank.DOK)
cors<-sapply(sources, function(x) cor(fulldf$Win[bool], 
                                      fulldf[bool, paste0("Rank.", x)]-fulldf[bool, paste0("OPP.", x)], use="pairwise.complete.obs"))
mor<-sapply(sources, function(x) cor(fulldf$Rank.MOR[bool],    fulldf[bool, paste0("Rank.", x)], use="pairwise.complete.obs"))
all<-data.frame(Cor=cors, Mor=mor)
all[order(all$Cor, decreasing = F), ]
#DOK,TRP, JNG, SEL, MCL, RTP, RPI, 7OT, LOG
summary(fulldf$Rank.TRP[bool])


#DOK,TRP, JNG, SEL, MCL, RTP, RPI, 7OT, LOG
#DOK,TRP, JNG, SEL, MCL, RTP, RPI, 7OT, LOG

fulldf$meanRank_alt<-rowMeans(fulldf[, c("Rank.DOK", "Rank.SEL", "Rank.7OT", "Rank.LOG", "Rank.TRP", "Rank.YAG" )], na.rm=T)
fulldf$OPPmeanRank_alt<-rowMeans(fulldf[, c("OPP.DOK", "OPP.SEL", "OPP.7OT", "OPP.LOG",  "OPP.TRP", "OPP.YAG" )], na.rm=T)
fulldf$Fav<-ifelse(fulldf$Rank.POM<fulldf$OPP.POM, fulldf$Team, fulldf$OPP ) #if team is not favorite
fulldf$Under<-ifelse(fulldf$Rank.POM<fulldf$OPP.POM, fulldf$OPP, fulldf$Team ) #if team is not favorite

cor(fulldf$Score.TR[!is.na(rowSums(fulldf[, c("Rank.MOR", "Score.TR","Rank.PGH")]))], 
    fulldf[!is.na(rowSums(fulldf[, c("Rank.MOR", "Score.TR","Rank.PGH")])), grepl("Rank[.]|med|min|max|mean", colnames(fulldf))], use="pairwise.complete.obs")


table(fulldf$Season, fulldf$Tournament)
table(fulldf$Season[!is.na(fulldf$Spread)], fulldf$Tournament[!is.na(fulldf$Spread)])

test<-fulldf[which(fulldf$Tournament==1 & fulldf$Season%in%year& fulldf$Team<fulldf$OPP), ]

train<-fulldf[which((fulldf$Season<min(year) | (fulldf$Tournament==0 & fulldf$Season==min(year)))), ]

summary(glm(Win_factor~TeamscoreMARGIN, data=train, family="binomial"))
scores2prob <- function(margin){
  margin<-(-0.150504)+0.058840*margin
  probs <- exp(margin) / (1 + exp(margin))
  probs
}

###MODEL FITTING#####



###model 1##
fit<-glm(Win_factor~Spread
         ,  data=train, family="binomial");summary(fit)
test$predWin<-predict(fit, newdata=test, type="response")# type="prob")[, 2]
train$predWin<-predict(fit, newdata=train, type="response")#,type="prob")[, 2]
logLoss(train$Win, train$predWin)

###model 2###
fit2<-glm(Win_factor~Spread, #+I(sign(Spread)*sqrt(abs(Spread)))
          family="binomial",
          data=train);summary(fit2)
test$predWin2<-predict(fit2, newdata=test, type="response")
train$predWin2<-predict(fit2, newdata=train, type="response")


logLoss(test$Win, test$predWin)
logLoss(test$Win, test$predWin2)
logLoss(test$Win, rowMeans(test[,c("predWin", "predWin2")]))

# cor(train$predWin[train$Tournament==1], train$predWin2[train$Tournament==1], use = "pairwise.complete.obs")
cor(test$predWin[test$Tournament==1], test$predWin2[test$Tournament==1], use = "pairwise.complete.obs")


#model 3##
fit3<-glm(Win_factor~I((Score.TR-OPPScore.TR))+I(Rank.MOR-OPP.MOR)++I(Dist^.25-OPPDist^.25),
          data=train, family="binomial");summary(fit3)
train$predWin3<-predict(fit3, newdata=train, type="response")
test$predWin3<-predict(fit3, newdata=test, type="response")



##model 4##
fit4<-glm(Win_factor~I(log(meanRank_alt/OPPmeanRank_alt))++I((Rank.MOR-OPP.MOR))+I(Dist^.25-OPPDist^.25),
          family="binomial", data=train);summary(fit4)
test$predWin4<-predict(fit4, newdata=test, type="response")
train$predWin4<-predict(fit4, newdata=train, type="response")
logLoss(test$Win, test$predWin4)


logLoss(test$Win, test$predWin3)
logLoss(test$Win, test$predWin4)
cor(train$predWin3[train$Tournament==1], train$predWin4[train$Tournament==1], use = "pairwise.complete.obs")
cor(train$Win[!is.na(train$predWin3) & !is.na(train$predWin4)], train$predWin3[!is.na(train$predWin3) & !is.na(train$predWin4)])


##inspect accuracy##

logLoss(test$Win, .5)
logLoss(test$Win, test$predWin4)  # rank-model
logLoss(test$Win, test$predWin3)  # tournamant rank-model
logLoss(test$Win, test$predWin2)  #Spread+Rank alt
logLoss(test$Win, test$predWin)  #Spread +Rank

logLoss(train$Win[train$Tournament==1], rowMeans(train[train$Tournament==1, c("predWin3", "predWin4")], na.rm=T))
logLoss(train$Win[train$Tournament==1], rowMeans(train[train$Tournament==1, c("predWin4", "predWin3", "predWin2", "predWin")], na.rm = T))

train$Pred<-ifelse(train$Round==1, rowMeans(train[, c("predWin",  "predWin2")], na.rm=T), rowMeans(train[, c("predWin3", "predWin4")], na.rm=T))
logLoss(train$Win[train$Round>=1& train$Tournament==1], train$Pred[train$Round>=1& train$Tournament==1])

test$Pred<-ifelse(test$Round==1, rowMeans(test[, c("predWin" ,"predWin2","predWin")], na.rm=T), 
                  rowMeans(test[, c("predWin4", "predWin3")], na.rm=T))

#align test set predictions so that Team<OPP
test$Win[test$Team>test$OPP]<-1-test$Win[test$Team>test$OPP]
test$Pred[test$Team>test$OPP]<-1-test$Pred[test$Team>test$OPP]
test[test$Team>test$OPP, c("Team", "OPP")]<-test[test$Team>test$OPP, c("OPP", "Team")]

logLoss(test$Win[test$Round>=1& test$Team<test$OPP], test$Pred[test$Round>=1& test$Team<test$OPP])

# test$Pred2<-ifelse(abs(test$OPPSeed_num-test$TeamSeed_num)==16, round(test$Pred), test$Pred)
# logLoss(test$Win[test$Round>=1& test$Team<test$OPP], test$Pred2[test$Round>=1& test$Team<test$OPP])
table(test$Round)

test$Pred3<-ifelse(test$Round>=6, test$Win,  test$Pred)
logLoss(test$Win[test$Round>=1& test$Team<test$OPP], test$Pred3[test$Round>=1& test$Team<test$OPP])

test$Pred4<-ifelse(test$Round==5& grepl("W|X", test$Slot), test$Win,  test$Pred)
logLoss(test$Win[test$Round>=1& test$Team<test$OPP], test$Pred4[test$Round>=1& test$Team<test$OPP])

test$Pred4<-ifelse(test$Round==5& grepl("Y|Z", test$Slot), test$Win,  test$Pred)
logLoss(test$Win[test$Round>=1& test$Team<test$OPP], test$Pred4[test$Round>=1& test$Team<test$OPP])

test$Pred5<-test$Pred
test$Pred5[which(test$Round==1)][which.min(abs(.5-test$Pred[test$Round==1]))]<-
  test$Win[which(test$Round==1)][which.min(abs(.5-test$Pred[test$Round==1]))]
logLoss(test$Win[test$Round>=1& test$Team<test$OPP], test$Pred5[test$Round>=1& test$Team<test$OPP])


######write predictions########
dates<-unique(fulldf$DATE[which(fulldf$Round<=1)])
dates<-c(dates, seq(as.Date("2018-03-13"), as.Date("2018-03-16"), 1))
dates<-dates[order(dates)]

if(year==2018){
  samplesubmission<-read.csv("data/SampleSubmission.csv")
  samplesubmission$Team<-as.numeric(sapply(strsplit(samplesubmission$ID, "_"), `[[`, 2))
  samplesubmission$OPP<-as.numeric(sapply(strsplit(samplesubmission$ID, "_"), `[[`, 3))
  samplesubmission$Season<-as.numeric(sapply(strsplit(samplesubmission$ID, "_"), `[[`,1))
  colnames(samplesubmission)<-gsub("ID", "Id", colnames(samplesubmission))
  
  
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

###create test-set for prediction###

odds2<-oddsDF[oddsDF$DATE%in% dates & as.numeric(substring(as.character(oddsDF$DATE), 1, 4))==year, ] #first round games
odds2$TeamID<-id_df$TeamID[match(odds2$Team, id_df$Team_Full)]
odds2$OPPID<-id_df$TeamID[match(odds2$OPP, id_df$Team_Full)]
samplesubmission<-merge(samplesubmission, odds2[,c("TeamID", "OPPID", "Spread")], 
                        by.x=c("Team", "OPP"), by.y=c("TeamID", "OPPID"), all.x=T  )

fulldf<-fulldf[order(fulldf$DATE, decreasing = F), ]
team_stats<-ddply(fulldf[which(fulldf$Season==year& (fulldf$Tournament==1| is.na(fulldf$Tournament))), ], .(Team,Team_Full, Season), summarize,
                  meanRank=meanRank[1],meanRank_alt=meanRank_alt[1],
                  Score.SAG.pre=Score.SAG.pre[1],
                   Rank.POM=Rank.POM[1],
                  Rank.MOR=Rank.MOR[1],
                  Rank.SAG=Rank.SAG[1], 
                  Score.TR=Score.TR[1],
                  Teamloc="N", TeamSeed=TeamSeed[1], TeamSeed_num=TeamSeed_num[1], 
                  TeamOwnership_R3=TeamOwnership_R3[1])


samplesubmission<-merge(samplesubmission,team_stats, by=c("Team", "Season"))
colnames(team_stats)<-c("OPP", "OPP_Full", "Season", "OPPmeanRank", "OPPmeanRank_alt","OPPScore.SAG.pre",
                        "OPP.POM", "OPP.MOR", "OPP.SAG", "OPPScore.TR",
                        "OPPloc", "OPPSeed", "OPPSeed_num", "OPPOwnership_R3")
samplesubmission<-merge(samplesubmission,team_stats, by=c("OPP", "Season"))


getSlot<-function(seed1, seed2, season){
  #seed1<-"W16a";seed2<-"W16b";season<-2017
  numCommon<-intersect(TourneyRounds$Slot[TourneyRounds$Seed==seed1 & TourneyRounds$Season==season], 
                       TourneyRounds$Slot[TourneyRounds$Seed==seed2 & TourneyRounds$Season==season]
  )[1]
}
samplesubmission$Slot<-sapply(1:nrow(samplesubmission),    function(x)getSlot(samplesubmission$TeamSeed[x], 
                                                                              samplesubmission$OPPSeed[x], samplesubmission$Season[x]) )
samplesubmission$Round<-as.numeric(sapply(1:nrow(samplesubmission), function(x)getRound(samplesubmission$TeamSeed[x], samplesubmission$OPPSeed[x], samplesubmission$Season[x])))

samplesubmission<-merge(samplesubmission, TourneyGeog[TourneyGeog$Season==year, c("Slot", "Lat", "Lng")], by.x=c("Slot"), all.x=T)
samplesubmission<-merge(samplesubmission, TeamGeog[, c("team_id", "TeamLat", "TeamLng")], by.x=c("Team"), by.y=("team_id"), all.x=T)
samplesubmission$Dist<-NA
samplesubmission$Dist[!is.na(samplesubmission$Lat)]<-distHaversine(samplesubmission[!is.na(samplesubmission$Lat), c( "Lng", "Lat")], samplesubmission[!is.na(samplesubmission$Lat), c( "TeamLng", "TeamLat")])/1000


#opponent distance
oppDist<- TeamGeog[, c("team_id", "TeamLat", "TeamLng")]
colnames(oppDist)[colnames(oppDist)%in% c("TeamLat", "TeamLng")]<-c("OPPLat", "OPPLng")
samplesubmission<-merge(samplesubmission, oppDist, by.x="OPP", by.y="team_id", all.x=T)
samplesubmission$OPPDist[!is.na(samplesubmission$OPPLat)]<-distHaversine(samplesubmission[!is.na(samplesubmission$OPPLat), c( "Lng", "Lat")], samplesubmission[!is.na(samplesubmission$Lat), c( "OPPLng", "OPPLat")])/1000


###predictions####

samplesubmission$Teamloc_num<-0
samplesubmission$predWin<-predict(fit, newdata=samplesubmission, type="response")
samplesubmission$predWin2<-predict(fit2, newdata=samplesubmission, type="response")
samplesubmission$predWin3<-predict(fit3, newdata=samplesubmission, type="response")
samplesubmission$predWin4<-predict(fit4, newdata=samplesubmission, type="response")



#ENSEMBLE
samplesubmission$Pred<-ifelse(!is.na(samplesubmission$Spread), rowMeans(samplesubmission[, c("predWin", "predWin2")], na.rm=T),
                              rowMeans(samplesubmission[, c("predWin4", "predWin3")], na.rm=T))


cor(samplesubmission[, grepl("pred", colnames(samplesubmission))], use="pairwise.complete.obs")
hist(samplesubmission$predWin)
hist(samplesubmission$Pred)

#write to a csv file
head(samplesubmission[, 1:10], 20)
seed.lm.submission <- data.frame(id=samplesubmission$Id, pred=samplesubmission$Pred)
head(seed.lm.submission)

write.csv(seed.lm.submission, file=paste0(year, "/Kaggle Submission.csv"), row.names=FALSE)







