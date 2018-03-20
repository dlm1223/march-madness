# input<-list(r1=5, r2=10, r3=15, r4=25, r5=30, r6=40, upset1_mult=2, upset2_mult=3, upset3_mult=4,
#             r1_seed_mult=0, r2_seed_mult=0,r3_seed_mult=0, r4_seed_mult=0,r5_seed_mult=0, r6_seed_mult=0,
#             r1_seed_bonus=0, r2_seed_bonus=0,r3_seed_bonus=0, r4_seed_bonus=0,r5_seed_bonus=0, r6_seed_bonus=0)
# input<-list(r1=10, r2=20, r3=40, r4=80, r5=160, r6=320, upset1_mult=1, upset2_mult=1, upset3_mult=1,
#             r1_seed_mult=0, r2_seed_mult=0,r3_seed_mult=0, r4_seed_mult=0,r5_seed_mult=0, r6_seed_mult=0,
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
tourneySims$Payout<-ifelse(grepl("R1", tourneySims$Slot), tourneySims$Payout*(1+(tourneySims$team_seed-1)*as.numeric(input$r1_seed_mult)),
                           ifelse(grepl("R2", tourneySims$Slot), tourneySims$Payout*(1+(tourneySims$team_seed-1)*as.numeric(input$r2_seed_mult)),
                                  ifelse(grepl("R3", tourneySims$Slot), tourneySims$Payout*(1+(tourneySims$team_seed-1)*as.numeric(input$r3_seed_mult)),
                                         ifelse(grepl("R4", tourneySims$Slot),tourneySims$Payout*(1+(tourneySims$team_seed-1)*as.numeric(input$r4_seed_mult)),
                                                ifelse(grepl("R5", tourneySims$Slot), tourneySims$Payout*(1+(tourneySims$team_seed-1)*as.numeric(input$r5_seed_mult)),
                                                       ifelse(grepl("R6", tourneySims$Slot), tourneySims$Payout*(1+(tourneySims$team_seed-1)*as.numeric(input$r6_seed_mult)),
                                                              tourneySims$Payout))))))
tourneySims$Payout<-as.numeric(tourneySims$Payout)
expected<-data.table(tourneySims)
expected<-expected[, list(Expected=sum(Payout)/max(Sim)), by=c("Team_Full", "Round")]
expected<-data.frame(expected)
expected<-merge(expected, analyze[, c("Team_Full", "Round", "Slot")], by=c("Team_Full", "Round"), all=T)
expected[is.na(expected)]<-0

#maximize round=Round to maximize i.e. "R1" or "R2", given round=next round i.e. "R2" or ""R3", row is a row of brackets--63 elements length
optimizeRounds<-function(maximize_round, given_round="NA", row, optmode="Rsymphony"){
  
  #maximize_round<-"R1";given_round="R2"
  maximize_round<-colnames(row)[grepl(paste(maximize_round,collapse="|"), colnames(row))]
  
  #given round-> constrains maximize_round so that it must contain teams listed in given round. defauly is "NA" i.e. no constraint
  given_round<-as.character(row[, grepl(given_round, colnames(row))])
  
  #maximize_round must contain given_round. the rest of slots, chose to mazimize EV
  given_slot<-expected[expected$Slot%in% maximize_round& expected$Team_Full%in% given_round, ] 
  
  must_fill<-expected[expected$Slot%in% maximize_round& !expected$Slot%in% given_slot$Slot, ] 
  
  
  nvars<- nrow(must_fill)
  numSlots<-length(unique(must_fill$Slot))
  model <- list()
  A<-matrix(0, ncol=nvars, nrow=numSlots+sum(must_fill$Round!="R1"))
  model$obj<-must_fill$Expected
  model$modelsense <- "max"
  
  q<-1
  for(i in unique(must_fill$Slot)) {
    A[q, which(must_fill$Slot==i)]<-1; model$sense[q]<-"="; model$rhs[q]<-1;q<-q+1
  } 
  
  must_fill$Round_num<-as.numeric(gsub("R", "", must_fill$Round))
  
  #for rounds being filled that are >=2
  for(i in which(must_fill$Round_num>=2)){
    
    team<-must_fill$Team_Full[i];rd<-must_fill$Round_num[i]
    
    #if a team is selected in a round, they must be selected in the previous round
    A[q, i]<-1;
    A[q, which(must_fill$Team_Full==team& must_fill$Round_num==rd-1)]<-(-1)
    model$sense[q]<-"<="; model$rhs[q]<-0;q<-q+1
    
  }
  must_fill$Round_num<-NULL
  
  model$vtype   <- 'B'
  params <- list(OutputFlag=0)
  model$A<-A[1:(q-1),]
  
  if(optmode=="RCplex"){
    model$sense[model$sense=="="]<-"E"
    model$sense[model$sense=="<="]<-"L"
    model$sense[model$sense==">="]<-"G"
    result<-Rcplex(cvec=model$obj,Amat=A[1:(q-1),],  bvec=model$rhs,sense = model$sense,
                   objsense = model$modelsense,vtype="B" ,control = list(trace=0) )
    result$x<- round(result$xopt, 1)
  } else if (optmode=="Rsymphony"){
    model$sense[model$sense=="="]<-"=="
    result<-Rsymphony_solve_LP(max=T, obj=model$obj, mat=A[1:(q-1),], types=rep("B",ncol(A)),
                               rhs=model$rhs, dir = model$sense, time_limit = 30, gap_limit = .03 )
    result$x<-result$solution
    
  }
  must_fill<-must_fill[as.logical(result$x),]
  
  return_vec<-rbind(must_fill, given_slot)
  return_vec<-return_vec[match(maximize_round, return_vec$Slot), ]
  return_vec$Team_Full
}


maximizeRound<-function(maximize_round, given_round="NA", row){
  
  
  #maximize_round<-"R1";given_round="R2"
  maximize_round<-colnames(row)[grepl(maximize_round, colnames(row))]
  
  #given round-> constrains maximize_round so that it must contain teams listed in given round. defauly is "NA" i.e. no constraint
  given_round<-as.character(row[, grepl(given_round, colnames(row))])
  
  #maximize_round must contain given_round. the rest of slots, chose to mazimize EV
  given_slot<-expected[expected$Slot%in% maximize_round& expected$Team_Full%in% given_round, ] 
  must_fill<-expected[expected$Slot%in% maximize_round& !expected$Slot%in% given_slot$Slot, ] 
  must_fill<-must_fill[order(must_fill$Expected, decreasing = T), ]
  must_fill<-must_fill[!duplicated(must_fill$Slot), ]
  
  #return teams for maximize_round
  return_vec<-rbind(must_fill, given_slot)
  
  return_vec<-return_vec[match(maximize_round, return_vec$Slot), ]
  return_vec$Team_Full
}
calcBrackets<-function(customBracket, brackets){
  customBracket<-customBracket[, 1:63]
  
  
  
  all<-data.frame(Slot=rep(colnames(customBracket)[!grepl("Sim", colnames(customBracket))], times=nrow(customBracket)), 
                  Team=unlist(lapply(1:nrow(customBracket),function(x)customBracket[x, !grepl("Sim", colnames(customBracket))] )), 
                  Bracket=rep(10000:(10000+ nrow(customBracket)-1),each=63 ))
  test<-data.table(all, key=c("Team", "Slot"))[
    data.table(tourneySims[, c("Sim", "Slot", "Payout", "Team_Full")], key=c("Team_Full", "Slot")),
    allow.cartesian=TRUE , nomatch=0 ]
  test<-test[,  list(Points=sum(Payout)),by=c("Sim", "Bracket")]
  test<-data.frame(test)
  
  # test<-reshape(test,timevar="Sim",idvar="Bracket",direction="wide")
  
  #faster than doing reshape since using 1 bracket:
  if(nrow(customBracket)==1){
    test<-test[order(test$Sim, decreasing = F),]
    test<-data.frame(t(test$Points))
    colnames(test)<-paste("Points.", 1:max(tourneySims$Sim), sep="")
    test$Bracket<-10000
  } else{
    #faster than doing reshape since using 1 bracket:
    test<-reshape(test,timevar="Sim",idvar="Bracket",direction="wide")
    colnames(test)<-gsub("Points.", "Sim", colnames(test))
    test<-test[!is.na(test$Bracket), ]
    test<-test[order(test$Bracket, decreasing = F), ]
    test$Bracket<-seq(10000,10000+ nrow(customBracket)-1)
    
  }
  colnames(test)<-gsub("Points.", "Sim", colnames(test))
  if(backtest){
    colnames(test)[grepl(max(tourneySims$Sim), colnames(test))]<-paste(colnames(test)[grepl(max(tourneySims$Sim), colnames(test))], "Actual", sep="_")
  }
  
  test<-rbind.fill(test, brackets[, grepl("Sim", colnames(brackets))])
  test$Bracket[is.na(test$Bracket)]<-seq(1, sum(is.na(test$Bracket)))
  numSims<-sum(grepl("Sim", colnames(test)))-backtest
  
  
  colnames(test)<-gsub("_Actual", "", colnames(test))
  test[, paste0("Percentile", (1:numSims+backtest))]<-NA
  for(i in 1:(numSims+backtest) ){
    test[test$Bracket>=10000, paste0("Percentile", i)]<-ecdf(test[test$Bracket<10000, paste0("Sim", i)])(test[test$Bracket>=10000,  paste0("Sim", i)])
  }
  colnames(test)[grepl(numSims+1, colnames(test))]<-paste(colnames(test)[grepl(numSims+1, colnames(test))], "Actual", sep="_")
  
  test<-test[which(test$Bracket>=10000), ]
  cbind(customBracket, test)
}
numSims<-sum(grepl("Sim", colnames(brackets)))
customBracket3<-brackets[, 1:63]
customBracket3[,49:56 ]<-t(sapply(1:nrow(customBracket3), function(x) maximizeRound(maximize_round = "R3",given_round = "R4", row=customBracket3[x, ] )))
customBracket3[,33:48 ]<-t(sapply(1:nrow(customBracket3), function(x) maximizeRound(maximize_round = "R2",given_round = "R3", row=customBracket3[x, ] )))
customBracket3[,1:32 ]<-t(sapply(1:nrow(customBracket3), function(x) maximizeRound(maximize_round = "R1",given_round = "R2", row=customBracket3[x, ] )))

customBracket4<-brackets[, 1:63]
customBracket4[,57:60 ]<-t(sapply(1:nrow(customBracket4), function(x) maximizeRound(maximize_round = "R4",given_round = "R5", row=customBracket4[x, ] )))
customBracket4[,49:56 ]<-t(sapply(1:nrow(customBracket4), function(x) maximizeRound(maximize_round = "R3",given_round = "R4", row=customBracket4[x, ] )))
customBracket4[,33:48 ]<-t(sapply(1:nrow(customBracket4), function(x) maximizeRound(maximize_round = "R2",given_round = "R3", row=customBracket4[x, ] )))
customBracket4[,1:32 ]<-t(sapply(1:nrow(customBracket4), function(x) maximizeRound(maximize_round = "R1",given_round = "R2", row=customBracket4[x, ] )))
