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

getOptimal<-function(brackets, percentile, numBrackets, speedUp=T){
  if(numBrackets==1){
    
    percentiles<-brackets[, grepl("Percentile", colnames(brackets)) & !grepl("Actual", colnames(brackets))]
    percentiles<-sapply(percentiles, function(x) as.numeric(x>percentile))
    percentiles<-t(percentiles)
    numSims<-nrow(percentiles)
    
    bool<-grepl("Percentile", colnames(brackets)) & !grepl("Actual", colnames(brackets))
    brackets[,paste("Prob", percentile*100, sep="", collapse="") ]<-apply(brackets[, bool], 1, function(x) sum(x>percentile)/numSims)
    
    result<-list()
    opt<-which.max(brackets[,paste("Prob", percentile*100, sep="", collapse="") ])
    result$x<-rep(0,nrow(percentiles)+numSims)
    result$x[opt]<-1
    result$x[(ncol(percentiles)+1):(ncol(percentiles)+numSims)]<-t(percentiles[,opt ])
    
    
  } else{
    
    #choose x lineups that give you maximum 
    optmode<-"Rsymphony"
    
    
    #each row is a simulation, each column is a lineup. goal:maximize sum of 
    percentiles<-brackets[, grepl("Percentile", colnames(brackets)) & !grepl("Actual", colnames(brackets))]
    percentiles<-sapply(percentiles, function(x) as.numeric(x>percentile))
    percentiles<-t(percentiles)
    # head(percentiles)
    
    numSims<-nrow(percentiles)
    bool<-grepl("Percentile", colnames(brackets)) & !grepl("Actual", colnames(brackets))
    brackets[,paste("Prob", percentile*100, sep="", collapse="") ]<-apply(brackets[, bool], 1, function(x) sum(x>percentile)/numSims)
    brackets$Index<-nrow(brackets)+1-rank(brackets[,paste("Prob", percentile*100, sep="", collapse="") ])
    
    
    nvars<- ncol(percentiles)+numSims
    model <- list()
    A<-matrix(0, ncol=nvars, nrow=numSims+2)
    model$obj<-c(rep(0, ncol(percentiles)), rep(1, numSims) )
    model$modelsense <- "max"
    
    q<-1
    for(i in 1:numSims) {
      
      #indicator is 9 if simulation wasn't passed 
      A[q, which(percentiles[i,]==1)]<-1;A[q, ncol(percentiles)+i]<-(-.5)#  indicator =0 unlesss person chosen
      model$sense[q]<-">="; model$rhs[q]<-0;q<-q+1 
      
    } 
    A[q, 1:ncol(percentiles)]<-1; model$sense[q]<-"="; model$rhs[q]<-numBrackets;q<-q+1
    
    if(speedUp){
      A[q, which(brackets$Index>=200)]<-1; model$sense[q]<-"="; model$rhs[q]<-0;q<-q+1  #set some to zero--speed up simulation?
    }
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
    } else if(optmode=="gurobi"){
      result <- gurobi(model, params)
    } else if(optmode=="lpsolve") {
      result<-lp ("max", objective.in=model$obj, const.mat=A[1:(q-1),],
                  const.dir=model$sense, const.rhs=model$rhs, all.bin=TRUE )
      result$x<-result$solution
    }else if (optmode=="Rsymphony"){
      model$sense[model$sense=="="]<-"=="
      result<-Rsymphony_solve_LP(max=T, obj=model$obj, mat=A[1:(q-1),], types=rep("B",ncol(A)),
                                 rhs=model$rhs, dir = model$sense, time_limit = 30, gap_limit = .03 )
      result$x<-result$solution
      
    }
    # which(result$x==1)
  }
  result
}
plotBracket<-function(bracket){
  
  
  # plotBracket<-function(bracket){
  round32 <- bracket[,  grepl("R1", colnames(bracket))] %>% as.character() %>% pasteSeed()
  round16<-bracket[ , grepl("R2", colnames(bracket))]%>% as.character() %>% pasteSeed()
  round8<-bracket[ , grepl("R3", colnames(bracket))]%>% as.character() %>% pasteSeed()
  round4<-bracket[ , grepl("R4", colnames(bracket))]%>% as.character() %>% pasteSeed()
  round2<-bracket[ , grepl("R5", colnames(bracket))]%>% as.character() %>% pasteSeed()
  round1<-bracket[ , grepl("R6", colnames(bracket))]%>% as.character() %>% pasteSeed()
  
  x<-seq(0,220,(221/67))
  y<-0:66
  
  plot(x,y,type="l", col.axis="white", col.lab="white", bty="n", 
       axes=F, col="white")
  segments(0,c(seq(0,30,2),seq(34,64,2)),20,c(seq(0,30,2),seq(34,64,2))) 
  segments(20,c(seq(0,28,4),seq(34,62,4)),20,c(seq(2,30,4),seq(36,64,4)))
  segments(20,c(seq(1,29,4),seq(35,63,4)),40,c(seq(1,29,4),seq(35,63,4)))
  segments(40,c(seq(1,25,8),seq(35,59,8)),40,c(seq(5,29,8),seq(39,63,8)))
  segments(40,c(3,11,19,27,37,45,53,61),60,c(3,11,19,27,37,45,53,61))
  segments(60,c(3,19,37,53),60,c(11,27,45,61))
  segments(60,c(7,23,41,57),80,c(7,23,41,57))
  segments(80,c(7,41),80,c(23,57))
  segments(80,c(15,49),100,c(15,49))
  segments(100,c(27,37),120,c(27,37))
  segments(200,c(seq(0,30,2),seq(34,64,2)),220,c(seq(0,30,2),seq(34,64,2))) 
  segments(200,c(seq(0,28,4),seq(34,62,4)),200,c(seq(2,30,4),seq(36,64,4)))
  segments(180,c(seq(1,29,4),seq(35,63,4)),200,c(seq(1,29,4),seq(35,63,4)))
  segments(180,c(seq(1,25,8),seq(35,59,8)),180,c(seq(5,29,8),seq(39,63,8)))
  segments(160,c(3,11,19,27,37,45,53,61),180,c(3,11,19,27,37,45,53,61))
  segments(160,c(3,19,37,53),160,c(11,27,45,61))
  segments(140,c(7,23,41,57),160,c(7,23,41,57))
  segments(140,c(7,41),140,c(23,57))
  segments(120,c(15,49),140,c(15,49))
  
  #round64
  for(i in 1:16){
    text(9.8,66.5-2*i,teams[i],cex=.6)
  }
  for(i in 1:16){
    text(9.8,32.5-2*i,teams[i+16],cex=.6)
  }
  for(i in 1:16){
    text(209.8,66.5-2*i,teams[i+32],cex=.6)
  }
  for(i in 1:16){
    text(209.8,32.5-2*i,teams[i+48],cex=.6)
  }
  #round32
  for(i in 1:8){
    text(29.8,67.5-4*i,round32[i],cex=.6)
  }
  for(i in 1:8){
    text(29.8,33.5-4*i,round32[i+8],cex=.6)
  }
  for(i in 1:8){
    text(189.8,67.5-4*i,round32[i+16],cex=.6)
  }
  for(i in 1:8){
    text(189.8,33.5-4*i,round32[i+24],cex=.6)
  }
  
  #round16
  for(i in 1:4){
    text(49.8,69.5-8*i,round16[i],cex=.6)
  }
  for(i in 1:4){
    text(49.8,35.5-8*i,round16[i+4],cex=.6)
  }
  for(i in 1:4){
    text(169.8,69.5-8*i,round16[i+8],cex=.6)
  }
  for(i in 1:4){
    text(169.8,35.5-8*i,round16[i+12],cex=.6)
  }
  
  #round8
  for(i in 1:2){
    text(69.8,73.5-16*i,round8[i],cex=.6)
  }
  for(i in 1:2){
    text(69.8,39.5-16*i,round8[i+2],cex=.6)
  }
  for(i in 1:2){
    text(149.8,73.5-16*i,round8[i+4],cex=.6)
  }
  for(i in 1:2){
    text(149.8,39.5-16*i,round8[i+6],cex=.6)
  }
  
  
  #final4
  text(89.8,49.5,round4[1],cex=.6)
  text(89.8,15.5,round4[2],cex=.6)
  text(129.8,49.5,round4[3],cex=.6)
  text(129.8,15.5,round4[4],cex=.6)
  
  #final2
  text(109.8,37.5,round2[1],cex=.6)
  text(109.8,27.5,round2[2],cex=.6)
  #champ
  text(109.8,32.5,round1[1],cex=1.4)
  
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
  # numSims<-sum(grepl("Sim", colnames(test)))-backtest
  
  
  colnames(test)<-gsub("_Actual", "", colnames(test))
  test[, paste0("Percentile", (1:numSims+backtest))]<-NA
  for(i in 1:(numSims+backtest) ){
    test[test$Bracket>=10000, paste0("Percentile", i)]<-ecdf(test[test$Bracket<10000, paste0("Sim", i)])(test[test$Bracket>=10000,  paste0("Sim", i)])
  }
  colnames(test)[grepl(numSims+1, colnames(test))]<-paste(colnames(test)[grepl(numSims+1, colnames(test))], "Actual", sep="_")
  
  test<-test[which(test$Bracket>=10000), ]
  cbind(customBracket, test)
}
