######CALCULATE PAYOUTS###########

tourneySims$Payout<-ifelse(grepl("R1", tourneySims$Slot), input$r1,
                           ifelse(grepl("R2", tourneySims$Slot), input$r2,
                                  ifelse(grepl("R3", tourneySims$Slot),input$r3,
                                         ifelse(grepl("R4", tourneySims$Slot),input$r4,
                                                ifelse(grepl("R5", tourneySims$Slot), input$r5,
                                                       ifelse(grepl("R6", tourneySims$Slot), input$r6, 0))))))

tourneySims$team_seed<-as.numeric(gsub("\\D", "",TourneySeeds$Seed[TourneySeeds$Season==year][match(tourneySims$Team,TourneySeeds$Team[TourneySeeds$Season==year])] ))
tourneySims$loser_seed<-as.numeric(gsub("\\D", "",TourneySeeds$Seed[TourneySeeds$Season==year][match(tourneySims$Loser,TourneySeeds$Team[TourneySeeds$Season==year])] ))
tourneySims$Payout<-ifelse(tourneySims$team_seed>=tourneySims$loser_seed+10, tourneySims$Payout*input$upset3_mult,
                           ifelse(tourneySims$team_seed>=tourneySims$loser_seed+5, tourneySims$Payout*input$upset2_mult,
                                  ifelse(tourneySims$team_seed>tourneySims$loser_seed, tourneySims$Payout*input$upset1_mult, tourneySims$Payout)))
tourneySims$Payout<-ifelse(tourneySims$team_seed>=tourneySims$loser_seed+10 & tourneySims$Payout>0, tourneySims$Payout+input$upset3_add,
                           ifelse(tourneySims$team_seed>=tourneySims$loser_seed+5& tourneySims$Payout>0, tourneySims$Payout+input$upset2_add,
                                  ifelse(tourneySims$team_seed>tourneySims$loser_seed& tourneySims$Payout>0, tourneySims$Payout+input$upset1_add, tourneySims$Payout)))


save(tourneySims, file="TourneySims_withPayouts.Rda")

# load("TourneySims_withPayouts.Rda")
tourneySims$Round<-substr(tourneySims$Slot, 1, 2)
tourneySims$Round[grepl("W|X|Y|Z", tourneySims$Round)]<-0
tourneySims$Payout<-as.numeric(tourneySims$Payout)


load("brackets_temp")
head(brackets)


prop.table(table(brackets[,"R2W3"]))*100
# whoPicked[whoPicked$Round=="R1" & whoPicked$Team=="North Carolina",]

#####APPLY EACH TOURNAMENT SIMULATION TO EACH BRACKET####
tourneySims<-tourneySims[as.numeric(gsub("R", "",tourneySims$Round))>=1,]
names<-unique(analyze[, c("Team_Full", "Team")])
tourneySims$Team_Full<-names$Team_Full[match(tourneySims$Team, names$Team)]

# tourneySims<-ddply(tourneySims, .(Sim), mutate, Times=length(Team))
# table(tourneySims$Times)  #check again for errors, might have to recheck Team_Fulls

numSims<-max(tourneySims$Sim)-backtest
inspect<-as.data.frame.matrix(table(tourneySims$Team_Full[tourneySims$Sim<=numSims], tourneySims$Round[tourneySims$Sim<=numSims])/numSims) 
inspect[order(inspect$R6,inspect$R5,inspect$R4,inspect$R3, inspect$R2,  decreasing = T), ]

all<-data.frame(Slot=rep(colnames(brackets)[!grepl("Sim", colnames(brackets))], times=nrow(brackets)), 
                Team=unlist(lapply(1:nrow(brackets),function(x)brackets[x, !grepl("Sim", colnames(brackets))] )), 
                Bracket=rep(1:nrow(brackets), each=63))
all<-all[order(all$Bracket,all$Slot), ]
tourneySims<-tourneySims[order(tourneySims$Sim, tourneySims$Slot), ]


#need to get payout of each pick for each bracket.
#merging is quicker but runs into memory issues for large numSims, numBrackets

test<-data.frame(Bracket=1:max(all$Bracket))
test[, paste("Sim", 1:max(tourneySims$Sim), sep="")]<-NA

for(i in 1:max(tourneySims$Sim)){
  print(i)
  
  #returns the payout of each bracket pick for all brackets
  payouts <-tourneySims$Payout[tourneySims$Sim==i]*  (all$Team==tourneySims$Team_Full[tourneySims$Sim==i])
  
  #calculate sum of every 63 elements
  payouts<-unname(tapply(payouts, (seq_along(payouts)-1) %/% 63, sum))
  
  #store in dataframe which holds sim-total for each bracket
  test[, paste0("Sim",i)]<-payouts
}
all<-test

# test<-data.table(all, key=c("Team", "Slot"))[
#   data.table(tourneySims[, c("Sim", "Slot", "Payout", "Team_Full")], key=c("Team_Full", "Slot")),
#   allow.cartesian=TRUE  ]
# test<-test[,  list(Points=sum(Payout)),by=c("Sim", "Bracket")]
# test<-data.frame(test)
# test<-reshape(test,timevar="Sim",idvar="Bracket",direction="wide")
# colnames(test)<-gsub("Points.", "Sim", colnames(test))
# test<-test[!is.na(test$Bracket), ]
# test<-test[order(test$Bracket, decreasing = F), ]
#all<-test

brackets<-cbind(brackets, all)

# colnames(brackets)[grepl(numSims+1, colnames(brackets))]<-gsub("Sim", "Actual", colnames(brackets)[grepl(numSims+1, colnames(brackets))])

###ANALYZE RESULTS#####

hist(as.numeric(brackets[45, 65:ncol(brackets)]))
brackets<-brackets[, !grepl("Percentile|Prob", colnames(brackets))]
for(i in 1:(numSims+1) ){
  brackets[, paste0("Percentile", i)]<-ecdf(brackets[, paste0("Sim", i)])(brackets[,  paste0("Sim", i)])
}
if(backtest==T){
  colnames(brackets)[grepl(numSims+1, colnames(brackets))]<-paste(colnames(brackets)[grepl(numSims+1, colnames(brackets))], "Actual", sep="_")
  
}
bool<-grepl("Percentile", colnames(brackets)) & !grepl("Actual", colnames(brackets))
brackets$Prob90<-apply(brackets[, bool], 1, function(x) sum(x>.90)/numSims)
brackets$Prob95<-apply(brackets[,bool], 1, function(x) sum(x>.95)/numSims)
brackets$Prob97<-apply(brackets[, bool], 1, function(x) sum(x>.97)/numSims)
brackets$Prob99<-apply(brackets[, bool], 1, function(x) sum(x>.99)/numSims)
head(brackets[,c("R4W1", "R4X1", "R4Y1", "R4Z1", "R5WX", "R5YZ", "R6CH", "Prob95") ][order(brackets$Prob95, decreasing=T), ])

save(brackets, file="BracketResults_FullTournament.Rda")
