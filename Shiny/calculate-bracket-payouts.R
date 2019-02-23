#calculates bracket-payouts--need to uncomment files/scoring in order to run
# year<-2018;backtest<-ifelse(year==2019, F, T);load("2018/TourneySims_500sims.Rda");load("2018/BracketResults_FullTournament_500sims.Rda")
# input<-list(r1=5, r2=10, r3=15, r4=20, r5=30, r6=40, upset1_mult=2, upset2_mult=3, upset3_mult=5, 
            # r1_seed_mult=0, r2_seed_mult=0, r3_seed_mult=0, r4_seed_mult=0, r5_seed_mult=0, r6_seed_mult=0,
            # r1_seed_bonus=0, r2_seed_bonus=0, r3_seed_bonus=0, r4_seed_bonus=0, r5_seed_bonus=0, r6_seed_bonus=0,
#             year=2018)


######CALCULATE PAYOUTS###########
#what is payout for getting correct team--will be different depending on upset scoring and if team upset someone in the given simulation
tourneySims$team_seed<-as.numeric(tourneySims$team_seed)
tourneySims$Payout<-ifelse(grepl("R1", tourneySims$Slot), input$r1,
                           ifelse(grepl("R2", tourneySims$Slot), input$r2,
                                  ifelse(grepl("R3", tourneySims$Slot),input$r3,
                                         ifelse(grepl("R4", tourneySims$Slot),input$r4,
                                                ifelse(grepl("R5", tourneySims$Slot), input$r5,
                                                       ifelse(grepl("R6", tourneySims$Slot), input$r6, 0))))))
tourneySims$Payout<-ifelse(grepl("R1", tourneySims$Slot), tourneySims$Payout+input$r1*as.numeric(input$r1_seed_mult)*(tourneySims$team_seed-1)+as.numeric(input$r1_seed_bonus)*(tourneySims$team_seed),
                           ifelse(grepl("R2", tourneySims$Slot), tourneySims$Payout+input$r2*as.numeric(input$r2_seed_mult)*(tourneySims$team_seed-1)+as.numeric(input$r2_seed_bonus)*(tourneySims$team_seed),
                                  ifelse(grepl("R3", tourneySims$Slot),tourneySims$Payout+input$r3*as.numeric(input$r3_seed_mult)*(tourneySims$team_seed-1)+as.numeric(input$r3_seed_bonus)*(tourneySims$team_seed),
                                         ifelse(grepl("R4", tourneySims$Slot),tourneySims$Payout+input$r4*as.numeric(input$r4_seed_mult)*(tourneySims$team_seed-1)+as.numeric(input$r4_seed_bonus)*(tourneySims$team_seed),
                                                ifelse(grepl("R5", tourneySims$Slot), tourneySims$Payout+input$r5*as.numeric(input$r5_seed_mult)*(tourneySims$team_seed-1)+as.numeric(input$r5_seed_bonus)*(tourneySims$team_seed),
                                                       ifelse(grepl("R6", tourneySims$Slot), tourneySims$Payout+input$r6*as.numeric(input$r6_seed_mult)*(tourneySims$team_seed-1)+as.numeric(input$r6_seed_bonus)*(tourneySims$team_seed), 0))))))

tourneySims$Payout<-ifelse(tourneySims$team_seed>=tourneySims$loser_seed+10, tourneySims$Payout*input$upset3_mult,
                           ifelse(tourneySims$team_seed>=tourneySims$loser_seed+5, tourneySims$Payout*input$upset2_mult,
                                  ifelse(tourneySims$team_seed>tourneySims$loser_seed, tourneySims$Payout*input$upset1_mult, tourneySims$Payout)))

brackets<-brackets[, 1:63]
# head(brackets)


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

if(backtest==T){
  colnames(brackets)[grepl(sims+1, colnames(brackets))& grepl("Sim", colnames(brackets))]<-"Score.Actual"
}

