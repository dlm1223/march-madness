optimizeRounds<-function(rounds, fixed.rounds="NA", bracket, optmode="Rsymphony"){
  #fixed.rounds<-"R4";rounds<-c("R1", "R2", "R3") 
  
  #rounds must contain fixed.rounds. the rest of slots, chose to mazimize EV
  fixed_slot<-expected[expected$Round%in% fixed.rounds& expected$Team_Full%in% as.character(bracket[, grepl(fixed.rounds, colnames(bracket))]), ] 
  
  must_fill<-expected[expected$Round%in% rounds& !expected$Slot%in% fixed_slot$Slot, ] 
  
  
  nvars<- nrow(must_fill)
  numSlots<-length(unique(must_fill$Slot))
  model <- list()
  A<-matrix(0, ncol=nvars, nrow=numSlots+sum(must_fill$Round!="R1")+(nrow(fixed_slot)>1))
  model$obj<-must_fill$Expected
  model$modelsense <- "max"
  
  #only 1 team per slot:
  q<-1
  for(i in unique(must_fill$Slot)) {
    A[q, which(must_fill$Slot==i)]<-1; model$sense[q]<-"="; model$rhs[q]<-1;q<-q+1
  } 
  
  must_fill$Round_num<-as.numeric(gsub("R", "", must_fill$Round))
  
  #for rounds being filled that are >1
  for(i in which(must_fill$Round_num>1)){
    
    team<-must_fill$Team_Full[i];rd<-must_fill$Round_num[i]
    
    #if a team is selected in a round, they must be selected in the previous roundss
    A[q, i]<-1;
    A[q, which(must_fill$Team_Full==team& must_fill$Round_num==rd-1)]<-(-1)
    model$sense[q]<-"<="; model$rhs[q]<-0;q<-q+1
    
  }
  #also need to account for fixed_slot
  if(nrow(fixed_slot)>1){
    A[q, which(must_fill$Team_Full%in% fixed_slot$Team_Full)]<-1
    model$sense[q]<-"="; model$rhs[q]<-sum(must_fill$Team_Full%in% fixed_slot$Team_Full);q<-q+1
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
  
  #return optimized results to bracket
  return_vec<-rbind(must_fill, fixed_slot)
  final_vec<-data.frame(t(return_vec$Team_Full))
  colnames(final_vec)<-return_vec$Slot
  cols<- colnames(bracket[, grepl(paste0(c(rounds, fixed.rounds),collapse="|"), colnames(bracket))])
  final_vec<-final_vec[,cols]
  bracket[, cols]<-final_vec
  bracket
}




maximizeRound<-function(rounds, fixed.rounds="NA", bracket){
  
  #rounds<-"R1";fixed.rounds="R2"
  fixed_slot<-expected[expected$Round%in% fixed.rounds& expected$Team_Full%in% as.character(bracket[, grepl(fixed.rounds, colnames(bracket))]), ] 
  
  must_fill<-expected[expected$Round%in% rounds& !expected$Slot%in% fixed_slot$Slot, ] 
  must_fill<-must_fill[order(must_fill$Expected, decreasing = T), ]
  must_fill<-must_fill[!duplicated(must_fill$Slot), ]
  
  #return teams for rounds
  return_vec<-rbind(must_fill, fixed_slot)
  final_vec<-data.frame(t(return_vec$Team_Full))
  colnames(final_vec)<-return_vec$Slot
  cols<- colnames(bracket[, grepl(paste0(c(rounds, fixed.rounds),collapse="|"), colnames(bracket))])
  final_vec<-final_vec[,cols]
  bracket[, cols]<-final_vec
  bracket
}


calcBrackets<-function(customBrackets, brackets, tourneySims){
  # customBrackets<-customBracket0[, 1:63]
  
  bracket.payouts<-data.frame(Slot=rep(colnames(customBrackets)[!grepl("Sim", colnames(customBrackets))], times=nrow(customBrackets)), 
                              Team=unlist(lapply(1:nrow(customBrackets),function(x)customBrackets[x, !grepl("Sim", colnames(customBrackets))] )), 
                              Bracket=rep(1:nrow(customBrackets)+10000, each=63),
                              CustomBracket=T
  )
  brackets$CustomBracket<-F
  bracket.payouts<-bracket.payouts[order(bracket.payouts$Bracket,bracket.payouts$Slot), ]
  tourneySims<-tourneySims[order(tourneySims$Sim, tourneySims$Slot), ]
  
  #want a column for each sim result to cbind to the brackets
  reshaped.sims<-reshape(tourneySims[, c("Team_Full", "Slot", "Sim", "Payout")], idvar = c("Team_Full", "Slot"), timevar="Sim",direction = "wide")
  
  bracket.payouts<-data.table(bracket.payouts, key=c("Team", "Slot"))[
    data.table(reshaped.sims, key=c("Team_Full", "Slot")),
    allow.cartesian=TRUE, nomatch=0  ]
  bracket.payouts<-bracket.payouts[,  lapply(.SD, sum, na.rm=TRUE),by=c( "Bracket", "CustomBracket"), .SDcols=colnames(bracket.payouts)[grepl("Payout", colnames(bracket.payouts))]]
  colnames(bracket.payouts)<-gsub("Payout.", "Sim", colnames(bracket.payouts))
  bracket.payouts<-bracket.payouts[order(bracket.payouts$Bracket, decreasing = F), ]
  
  
  sims<-max(tourneySims$Sim)-backtest
  colnames(bracket.payouts)<-gsub("Points.", "Sim", colnames(bracket.payouts))
  colnames(bracket.payouts)[grepl(sims+1, colnames(bracket.payouts))& grepl("Sim", colnames(bracket.payouts))]<-"Score.Actual"
  
  #rbind custompool scores to current brackets
  bracket.payouts<-rbind.fill(bracket.payouts, brackets[, grepl("Sim|Bracket|Score", colnames(brackets))])
  bracket.payouts[, paste0("Percentile", (1:sims))]<-NA
  
  #calculate percentiles for custombrackets based on the non-custom brackets
  for(i in 1:(sims) ){
    bracket.payouts[bracket.payouts$CustomBracket==T, paste0("Percentile", i)]<-ecdf(bracket.payouts[bracket.payouts$CustomBracket==F, paste0("Sim", i)])(bracket.payouts[bracket.payouts$CustomBracket==T,  paste0("Sim", i)])
  }
  if(backtest){
    bracket.payouts$Percentile.Actual[bracket.payouts$CustomBracket==T]<-ecdf(bracket.payouts[bracket.payouts$CustomBracket==F, "Score.Actual"])(bracket.payouts[bracket.payouts$CustomBracket==T,  "Score.Actual"])
    
  }
  bracket.payouts<-bracket.payouts[which(bracket.payouts$CustomBracket==T), ]
  cbind(customBrackets, bracket.payouts)
}

plotBracket<-function(bracket, text.size=.6){
  
  analyze<-TourneySeeds[TourneySeeds$Season==year, ]
  analyze$Team_Full<-Teams$Team_Full[match(analyze$Team, Teams$TeamID)]
  names<-unique(analyze[, c("Team_Full", "Seed")])
  names$Seed<-as.numeric(substring(names$Seed, 2, 3))
  pasteSeed<-function(teams){
    paste(names$Seed[match( teams, names$Team_Full)], teams, sep=" ")
  }
  
  round64 <- tourneySims[tourneySims$Round=="R1" & tourneySims$Sim==1,]
  round64<-round64[order(round64$Slot, decreasing = F), ]
  round64$Loser_Full<-Teams$Team_Full[match(round64$Loser, Teams$TeamID)]
  ords<-c(1, 8, 4, 5, 3, 6, 2, 7)
  round64<-round64[c(ords, ords+8, ords+16, ords+24), ]
  teams<-lapply(1:nrow(round64),
                function(x) if(round64$team_seed[x]< round64$loser_seed[x]) {
                  c(round64$Team_Full[x], round64$Loser_Full[x])
                }else{
                  c(round64$Loser_Full[x], round64$Team_Full[x])
                })
  teams<-unlist(teams) %>% pasteSeed()
  
  # plotBracket<-function(bracket){
  round32 <- bracket[,  grepl("R1", colnames(bracket))]%>% unlist() %>% as.character() %>% pasteSeed()
  round16<-bracket[ , grepl("R2", colnames(bracket))]%>% unlist()%>% as.character() %>% pasteSeed()
  round8<-bracket[ , grepl("R3", colnames(bracket))]%>% unlist()%>% as.character() %>% pasteSeed()
  round4<-bracket[ , grepl("R4", colnames(bracket))]%>% unlist()%>% as.character() %>% pasteSeed()
  round2<-bracket[ , grepl("R5", colnames(bracket))]%>% unlist()%>% as.character() %>% pasteSeed()
  round1<-bracket[ , grepl("R6", colnames(bracket))]%>% unlist()%>% as.character() %>% pasteSeed()
  
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
    text(9.8,66.5-2*i,teams[i],cex=text.size)
  }
  for(i in 1:16){
    text(9.8,32.5-2*i,teams[i+16],cex=text.size)
  }
  for(i in 1:16){
    text(209.8,66.5-2*i,teams[i+32],cex=text.size)
  }
  for(i in 1:16){
    text(209.8,32.5-2*i,teams[i+48],cex=text.size)
  }
  #round32
  for(i in 1:8){
    text(29.8,67.5-4*i,round32[i],cex=text.size)
  }
  for(i in 1:8){
    text(29.8,33.5-4*i,round32[i+8],cex=text.size)
  }
  for(i in 1:8){
    text(189.8,67.5-4*i,round32[i+16],cex=text.size)
  }
  for(i in 1:8){
    text(189.8,33.5-4*i,round32[i+24],cex=text.size)
  }
  
  #round16
  for(i in 1:4){
    text(49.8,69.5-8*i,round16[i],cex=text.size)
  }
  for(i in 1:4){
    text(49.8,35.5-8*i,round16[i+4],cex=text.size)
  }
  for(i in 1:4){
    text(169.8,69.5-8*i,round16[i+8],cex=text.size)
  }
  for(i in 1:4){
    text(169.8,35.5-8*i,round16[i+12],cex=text.size)
  }
  
  #round8
  for(i in 1:2){
    text(69.8,73.5-16*i,round8[i],cex=text.size)
  }
  for(i in 1:2){
    text(69.8,39.5-16*i,round8[i+2],cex=text.size)
  }
  for(i in 1:2){
    text(149.8,73.5-16*i,round8[i+4],cex=text.size)
  }
  for(i in 1:2){
    text(149.8,39.5-16*i,round8[i+6],cex=text.size)
  }
  
  
  #final4
  text(89.8,49.5,round4[1],cex=text.size)
  text(89.8,15.5,round4[2],cex=text.size)
  text(129.8,49.5,round4[3],cex=text.size)
  text(129.8,15.5,round4[4],cex=text.size)
  
  #final2
  text(109.8,37.5,round2[1],cex=text.size)
  text(109.8,27.5,round2[2],cex=text.size)
  #champ
  text(109.8,32.5,round1[1],cex=text.size*2)
  
}
getOptimal<-function(brackets, percentile, numBrackets, speedUp=T){
  
  
  if(numBrackets==1){
    
    percentiles<-brackets[, grepl("Percentile", colnames(brackets)) & !grepl("Actual", colnames(brackets))]
    percentiles<-sapply(percentiles, function(x) as.numeric(x>=percentile))
    percentiles<-t(percentiles)
    # numBrackets<-ncol(percentiles)
    sims<-nrow(percentiles)
    
    bool<-grepl("Percentile", colnames(brackets)) & !grepl("Actual", colnames(brackets))
    brackets[,paste("Prob", percentile*100, sep="", collapse="") ]<-apply(brackets[, bool], 1, function(x) sum(x>=percentile)/sims)
    
    result<-list()
    opt<-which.max(brackets[,paste("Prob", percentile*100, sep="", collapse="") ])
    result$x<-rep(0,nrow(percentiles)+ncol(percentiles))
    result$x[opt]<-1
    result$x[(ncol(percentiles)+1):(ncol(percentiles)+sims)]<-t(percentiles[,opt ])
    
    
  } else{
    
    #choose x lineups that give you maximum 
    optmode<-"Rsymphony"
    library(Rsymphony)
    
    
    #each row is a simulation, each column is a lineup. goal:maximize sum of 
    percentiles<-brackets[, grepl("Percentile", colnames(brackets)) & !grepl("Actual", colnames(brackets))]
    percentiles<-sapply(percentiles, function(x) as.numeric(x>=percentile))
    percentiles<-t(percentiles)
    # head(percentiles)
    
    numSims<-nrow(percentiles)
    bool<-grepl("Percentile", colnames(brackets)) & !grepl("Actual", colnames(brackets))
    brackets[,paste("Prob", percentile*100, sep="", collapse="") ]<-apply(brackets[, bool], 1, function(x) sum(x>=percentile)/numSims)
    brackets$Index<-nrow(brackets)+1-rank(brackets[,paste("Prob", percentile*100, sep="", collapse="") ])
    
    
    nvars<- ncol(percentiles)+numSims
    model <- list()
    A<-matrix(0, ncol=nvars, nrow=numSims+3)
    model$obj<-c(rep(0, ncol(percentiles)), rep(1, numSims) )
    model$modelsense <- "max"
    
    q<-1
    for(i in 1:numSims) {
      
      #indicator is 9 if simulation wasn't passed 
      A[q, which(percentiles[i,]==1)]<-1;A[q, ncol(percentiles)+i]<-(-.5)#  indicator =0 unlesss person chosen
      model$sense[q]<-">="; model$rhs[q]<-0;q<-q+1 
      
    } 
    A[q, 1:ncol(percentiles)]<-1; model$sense[q]<-"="; model$rhs[q]<-numBrackets;q<-q+1
    
    A[q, which(duplicated(brackets[, 1:63]))]<-1; model$sense[q]<-"="; model$rhs[q]<-0;q<-q+1  #if there are duplicated brackets, set to 0
    
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
