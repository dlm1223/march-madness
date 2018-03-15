######CALCULATE PAYOUTS###########
# input<-list(r1=10, r2=20, r3=30, r4=40, r5=80, r6=160, upset1_mult=2, upset2_mult=3, upset3_mult=4,
#             r1_seed_mult=1, r2_seed_mult=0,r3_seed_mult=0, r4_seed_mult=0,r5_seed_mult=0, r6_seed_mult=0,
#             r1_seed_bonus=0, r2_seed_bonus=0,r3_seed_bonus=0, r4_seed_bonus=0,r5_seed_bonus=0, r6_seed_bonus=0)

tourneySims$Payout<-ifelse(grepl("R1", tourneySims$Slot), input$r1,
                           ifelse(grepl("R2", tourneySims$Slot), input$r2,
                                  ifelse(grepl("R3", tourneySims$Slot),input$r3,
                                         ifelse(grepl("R4", tourneySims$Slot),input$r4,
                                                ifelse(grepl("R5", tourneySims$Slot), input$r5,
                                                       ifelse(grepl("R6", tourneySims$Slot), input$r6, 0))))))

tourneySims$Payout<-ifelse(tourneySims$team_seed>=tourneySims$loser_seed+10, tourneySims$Payout*input$upset3_mult,
                           ifelse(tourneySims$team_seed>=tourneySims$loser_seed+5, tourneySims$Payout*input$upset2_mult,
                                  ifelse(tourneySims$team_seed>tourneySims$loser_seed, tourneySims$Payout*input$upset1_mult, tourneySims$Payout)))
# tourneySims$Payout<-ifelse(tourneySims$team_seed>=tourneySims$loser_seed+10 & tourneySims$Payout>0, tourneySims$Payout+input$upset3_add,
#                            ifelse(tourneySims$team_seed>=tourneySims$loser_seed+5& tourneySims$Payout>0, tourneySims$Payout+input$upset2_add,
#                                   ifelse(tourneySims$team_seed>tourneySims$loser_seed& tourneySims$Payout>0, tourneySims$Payout+input$upset1_add, tourneySims$Payout)))
tourneySims$Payout<-ifelse(grepl("R1", tourneySims$Slot), tourneySims$Payout*(1+(tourneySims$team_seed-1)*as.numeric(input$r1_seed_mult)),
                           ifelse(grepl("R2", tourneySims$Slot), tourneySims$Payout*(1+(tourneySims$team_seed-1)*as.numeric(input$r2_seed_mult)),
                                  ifelse(grepl("R3", tourneySims$Slot), tourneySims$Payout*(1+(tourneySims$team_seed-1)*as.numeric(input$r3_seed_mult)),
                                         ifelse(grepl("R4", tourneySims$Slot),tourneySims$Payout*(1+(tourneySims$team_seed-1)*as.numeric(input$r4_seed_mult)),
                                                ifelse(grepl("R5", tourneySims$Slot), tourneySims$Payout*(1+(tourneySims$team_seed-1)*as.numeric(input$r5_seed_mult)),
                                                       ifelse(grepl("R6", tourneySims$Slot), tourneySims$Payout*(1+(tourneySims$team_seed-1)*as.numeric(input$r6_seed_mult)),
                                                              tourneySims$Payout))))))
# tourneySims$seed_diff<-floor(sapply(tourneySims$team_seed-tourneySims$loser_seed,function(x) max(x, 0)/2))

tourneySims$Payout<-ifelse(grepl("R1", tourneySims$Slot), tourneySims$Payout+tourneySims$team_seed*as.numeric(input$r1_seed_bonus),
                           ifelse(grepl("R2", tourneySims$Slot), tourneySims$Payout+tourneySims$team_seed*as.numeric(input$r2_seed_bonus),
                                  ifelse(grepl("R3", tourneySims$Slot), tourneySims$Payout+tourneySims$team_seed*as.numeric(input$r3_seed_bonus),
                                         ifelse(grepl("R4", tourneySims$Slot),tourneySims$Payout+tourneySims$team_seed*as.numeric(input$r4_seed_bonus),
                                                ifelse(grepl("R5", tourneySims$Slot), tourneySims$Payout+tourneySims$team_seed*as.numeric(input$r5_seed_bonus),
                                                       ifelse(grepl("R6", tourneySims$Slot), tourneySims$Payout+tourneySims$team_seed*as.numeric(input$r6_seed_bonus),tourneySims$Payout))))))
tourneySims$Payout<-as.numeric(tourneySims$Payout)


#####APPLY EACH TOURNAMENT SIMULATION TO EACH BRACKET####


brackets<-brackets[, 1:63]

all<-data.frame(Slot=rep(colnames(brackets)[!grepl("Sim", colnames(brackets))], times=nrow(brackets)), 
                Team=unlist(lapply(1:nrow(brackets),function(x)brackets[x, !grepl("Sim", colnames(brackets))] )), 
                Bracket=rep(1:nrow(brackets), each=63))
#get points for each pick
#left inner join
test<-data.table(all, key=c("Team", "Slot"))[
  data.table(tourneySims[, c("Sim", "Slot", "Payout", "Team_Full")], key=c("Team_Full", "Slot")),
  allow.cartesian=TRUE , nomatch=0 ]

# incProgress(1/3)


test<-test[,  list(Points=sum(Payout)),by=c("Sim", "Bracket")]
test<-data.frame(test)
test<-reshape(test,timevar="Sim",idvar="Bracket",direction="wide")
colnames(test)<-gsub("Points.", "Sim", colnames(test))
test<-test[!is.na(test$Bracket), ]
test<-test[order(test$Bracket, decreasing = F), ]
brackets<-cbind(brackets, test)

###ANALYZE RESULTS#####

# hist(as.numeric(brackets[14, 65:ncol(brackets)]))
brackets<-brackets[, !grepl("Percentile|Prob", colnames(brackets))]
numSims<-ncol(brackets[, grepl("Sim", colnames(brackets))])
for(i in 1:numSims ){
  brackets[, paste0("Percentile", i)]<-ecdf(brackets[, paste0("Sim", i)])(brackets[,  paste0("Sim", i)])
}

if(backtest==T){
  colnames(brackets)[grepl(numSims, colnames(brackets))]<-paste(colnames(brackets)[grepl(numSims, colnames(brackets))], "Actual", sep="_")
  numSims<-numSims-1
}
