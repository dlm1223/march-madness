#calculates bracket-payouts--need to uncomment files/scoring in order to run
# year<-2018;backtest<-ifelse(year==2019, F, T);load("2018/TourneySims_500sims.Rda");load("2018/BracketResults_FullTournament_500sims.Rda")
# input<-list(r1=10, r2=20, r3=40, r4=80, r5=160, r6=320, upset1_mult=1, upset2_mult=1, upset3_mult=1, upset1_add=0, upset2_add=0, upset3_add=0)






######CALCULATE PAYOUTS###########
#what is payout for getting correct team--will be different depending on upset scoring and if team upset someone in the given simulation

tourneySims$Payout<-ifelse(grepl("R1", tourneySims$Slot), input$r1,
                           ifelse(grepl("R2", tourneySims$Slot), input$r2,
                                  ifelse(grepl("R3", tourneySims$Slot),input$r3,
                                         ifelse(grepl("R4", tourneySims$Slot),input$r4,
                                                ifelse(grepl("R5", tourneySims$Slot), input$r5,
                                                       ifelse(grepl("R6", tourneySims$Slot), input$r6, 0))))))
tourneySims$Payout<-ifelse(tourneySims$team_seed>=tourneySims$loser_seed+10, tourneySims$Payout*input$upset3_mult,
                           ifelse(tourneySims$team_seed>=tourneySims$loser_seed+5, tourneySims$Payout*input$upset2_mult,
                                  ifelse(tourneySims$team_seed>tourneySims$loser_seed, tourneySims$Payout*input$upset1_mult, tourneySims$Payout)))
tourneySims$Payout<-ifelse(tourneySims$team_seed>=tourneySims$loser_seed+10 & tourneySims$Payout>0, tourneySims$Payout+input$upset3_add,
                           ifelse(tourneySims$team_seed>=tourneySims$loser_seed+5& tourneySims$Payout>0, tourneySims$Payout+input$upset2_add,
                                  ifelse(tourneySims$team_seed>tourneySims$loser_seed& tourneySims$Payout>0, tourneySims$Payout+input$upset1_add, tourneySims$Payout)))
tourneySims$Payout<-as.numeric(tourneySims$Payout)

brackets<-brackets[, 1:63]
head(brackets)


#####APPLY EACH TOURNAMENT SIMULATION TO EACH BRACKET####

sims<-max(tourneySims$Sim)-backtest
# inspect<-as.data.frame.matrix(table(tourneySims$Team_Full[tourneySims$Sim<=sims], tourneySims$Round[tourneySims$Sim<=sims])/sims) 
# inspect[order(inspect$R6,inspect$R5,inspect$R4,inspect$R3, inspect$R2,  decreasing = T), ]


bracket.payouts<-data.frame(Slot=rep(colnames(brackets)[!grepl("Sim", colnames(brackets))], times=nrow(brackets)), 
                            Team=unlist(lapply(1:nrow(brackets),function(x)brackets[x, !grepl("Sim", colnames(brackets))] )), 
                            Bracket=rep(1:nrow(brackets), each=63))
bracket.payouts<-bracket.payouts[order(bracket.payouts$Bracket,bracket.payouts$Slot), ]
tourneySims<-tourneySims[order(tourneySims$Sim, tourneySims$Slot), ]


#need to get payout of each pick for each bracket.
#merging is quicker but runs into memory issues for large numSims, numBrackets
if(sims>2000){
  test<-data.frame(Bracket=1:max(bracket.payouts$Bracket))
  test[, paste("Sim", 1:max(tourneySims$Sim), sep="")]<-NA
  for(i in 1:max(tourneySims$Sim)){
    print(i)
    
    #returns the payout of each bracket pick for bracket.payouts brackets
    payouts <-tourneySims$Payout[tourneySims$Sim==i]*  (bracket.payouts$Team==tourneySims$Team_Full[tourneySims$Sim==i])
    
    #calculate sum of every 63 elements
    payouts<-unname(tapply(payouts, (seq_along(payouts)-1) %/% 63, sum))
    
    #store in dataframe which holds sim-total for each bracket
    test[, paste0("Sim",i)]<-payouts
  }
  bracket.payouts<-test
} else{
  
  #want a column for each sim result to cbind to the brackets
  reshaped.sims<-reshape(tourneySims[, c("Team_Full", "Slot", "Sim", "Payout")], idvar = c("Team_Full", "Slot"), timevar="Sim",direction = "wide")
  
  bracket.payouts<-data.table(bracket.payouts, key=c("Team", "Slot"))[
    data.table(reshaped.sims, key=c("Team_Full", "Slot")),
    allow.cartesian=TRUE, nomatch=0  ]
  bracket.payouts<-bracket.payouts[,  lapply(.SD, sum, na.rm=TRUE),by=c( "Bracket"), .SDcols=colnames(bracket.payouts)[grepl("Payout", colnames(bracket.payouts))]]
  colnames(bracket.payouts)<-gsub("Payout.", "Sim", colnames(bracket.payouts))
  
  bracket.payouts<-bracket.payouts[!is.na(bracket.payouts$Bracket), ]
  bracket.payouts<-bracket.payouts[order(bracket.payouts$Bracket, decreasing = F), ]
}
brackets<-cbind(brackets, bracket.payouts)

# colnames(brackets)[grepl(numSims+1, colnames(brackets))]<-gsub("Sim", "Actual", colnames(brackets)[grepl(numSims+1, colnames(brackets))])
colnames(brackets)[grepl(sims+1, colnames(brackets))& grepl("Sim", colnames(brackets))]<-"Score.Actual"

###ANALYZE RESULTS#####

# hist(as.numeric(brackets[45,grepl("Sim", colnames(brackets))]))
# brackets<-brackets[, !grepl("Percentile|Prob", colnames(brackets))]
# for(i in 1:(sims+backtest) ){
#   brackets[, paste0("Percentile", i)]<-ecdf(brackets[, paste0("Sim", i)])(brackets[,  paste0("Sim", i)])
# }
# if(backtest==T){
#   colnames(brackets)[grepl(sims+1, colnames(brackets))& grepl("Percentile", colnames(brackets))]<-"Percentile.Actual"
# }
# bool<-grepl("Percentile", colnames(brackets)) & !grepl("Actual", colnames(brackets))
# brackets$Prob90<-apply(brackets[, bool], 1, function(x) sum(x>.90)/sims)
# brackets$Prob95<-apply(brackets[,bool], 1, function(x) sum(x>.95)/sims)
# brackets$Prob97<-apply(brackets[, bool], 1, function(x) sum(x>.97)/sims)
# brackets$Prob99<-apply(brackets[, bool], 1, function(x) sum(x>.99)/sims)
# head(brackets[,c("R4W1", "R4X1", "R4Y1", "R4Z1", "R5WX", "R5YZ", "R6CH", "Prob95", "Prob90", "Percentile.Actual") ][order(brackets$Prob99, decreasing=T), ])




###SAVE DATA#####

save(list=ls()[ls()%in% c( "backtest", "playInTbd", "Teams",   "year", "TourneySeeds","TourneyRounds", "brackets" )], file=paste0(year, "/bracketpayouts.RData"))

