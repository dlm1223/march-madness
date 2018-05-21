year<-2018
# input<-list(r1=10, r2=20, r3=40, r4=80, r5=160, r6=320, upset1_mult=1, upset2_mult=1, upset3_mult=1,
#             r1_seed_mult=0, r2_seed_mult=0,r3_seed_mult=0, r4_seed_mult=0,r5_seed_mult=0, r6_seed_mult=0,
#             r1_seed_bonus=0, r2_seed_bonus=0,r3_seed_bonus=0, r4_seed_bonus=0,r5_seed_bonus=0, r6_seed_bonus=0)

name<-"BracketResults_FullTournament_500sims.Rda"

setwd("~/Kaggle/NCAA/march-madness")
backtest<-ifelse(year==2018, F, T)
load("data/game data.RData");
setwd(as.character(year));
load("TourneySims_500sims.Rda");
load("BracketResults_FullTournament_500sims.Rda")

input<-list(r1=1, r2=2, r3=4, r4=8, r5=16, r6=32, upset1_mult=1, upset2_mult=1, upset3_mult=1,
            r1_seed_mult=1, r2_seed_mult=1,r3_seed_mult=1, r4_seed_mult=1,r5_seed_mult=1, r6_seed_mult=1,
            r1_seed_bonus=0, r2_seed_bonus=0,r3_seed_bonus=0, r4_seed_bonus=0,r5_seed_bonus=0, r6_seed_bonus=0)
# input<-list(r1=5, r2=10, r3=15, r4=25, r5=30, r6=40, upset1_mult=2, upset2_mult=3, upset3_mult=4,
#             r1_seed_mult=0, r2_seed_mult=0,r3_seed_mult=0, r4_seed_mult=0,r5_seed_mult=0, r6_seed_mult=0,
#             r1_seed_bonus=0, r2_seed_bonus=0,r3_seed_bonus=0, r4_seed_bonus=0,r5_seed_bonus=0, r6_seed_bonus=0)


source('~/Kaggle/NCAA/march-madness/Shiny/simulate calc payouts_merge.R')

# inspect[order(inspect$R6,inspect$R5,inspect$R4,inspect$R3, inspect$R2,  decreasing = T), ][1:20,]


###SAVE DATA#####

# save(brackets, file=name)



## CUSTOM BRACKET####

# year<-2018

# 
# input<-list(r1=5, r2=10, r3=15, r4=25, r5=30, r6=40, upset1_mult=2, upset2_mult=3, upset3_mult=4,
#             r1_seed_mult=0, r2_seed_mult=0,r3_seed_mult=0, r4_seed_mult=0,r5_seed_mult=0, r6_seed_mult=0,
#             r1_seed_bonus=0, r2_seed_bonus=0,r3_seed_bonus=0, r4_seed_bonus=0,r5_seed_bonus=0, r6_seed_bonus=0)
# input<-list(r1=10, r2=20, r3=40, r4=80, r5=160, r6=320, upset1_mult=1, upset2_mult=1, upset3_mult=1,
#             r1_seed_mult=0, r2_seed_mult=0,r3_seed_mult=0, r4_seed_mult=0,r5_seed_mult=0, r6_seed_mult=0,
#             r1_seed_bonus=0, r2_seed_bonus=0,r3_seed_bonus=0, r4_seed_bonus=0,r5_seed_bonus=0, r6_seed_bonus=0)

setwd("~/Kaggle/NCAA/march-madness")
setwd(as.character(year));
# load("BracketResults_FullTournament_500sims.Rda")
load("TourneySims_500sims.Rda");
load("allData.RData");

###Tourneysims expected values####

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

numSims<-sum(grepl("Sim", colnames(brackets)))-backtest

expected<-data.table(tourneySims[tourneySims$Sim<=numSims,])
expected<-expected[, list(Expected=sum(Payout)/numSims), by=c("Team_Full", "Round")]  
expected<-data.frame(expected)
expected<-merge(expected, analyze[, c("Team_Full", "Round", "Slot")], by=c("Team_Full", "Round"), all=T)
expected[is.na(expected)]<-0

#bracket expected values##
# expectedPoints<-function(slots, teams){
#   teams<-as.character(teams)
#   sapply(1:length(slots), function(x) {
#     expected$Expected[expected$Team_Full==teams[x]& expected$Round==slots[x]]
#   })
# }
# cols<-colnames(brackets[, 1:63]) %>% substring(., 1, 2)
# expectedPoints(slots=cols,  teams=brackets[8, 1:63])
# brackets[, paste("Expected", colnames(brackets[1:63]), sep="")]<-NA
# brackets[, paste("Expected",colnames(brackets[1:63]), sep="")]<-
#   matrix(unlist(lapply(1:nrow(brackets), function(x) expectedPoints(cols,teams=brackets[x, 1:63]))), byrow=T, ncol=63)
# for(i in c("R1", "R2", "R3", "R4","R5")){
#   brackets[, paste0("Expected", i)]<-NULL
#   brackets[, paste0("Expected", i)]<-apply(brackets[, grepl(paste0("Expected", i), colnames(brackets))], 1, sum)
# }
# brackets$ExpectedR1234<-rowSums(brackets[, c("ExpectedR1", "ExpectedR2", "ExpectedR3", "ExpectedR4")])
# brackets$ExpectedR123<-rowSums(brackets[, c("ExpectedR1", "ExpectedR2", "ExpectedR3")])
# brackets$ExpectedR12<-rowSums(brackets[, c("ExpectedR1", "ExpectedR2")])
# brackets$Prob97<-apply(brackets[, bool], 1, function(x) sum(x>.97)/numSims)
# brackets<-brackets[order(brackets$Prob97, decreasing = T),]

#bracket expected values##

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
plotBracket<-function(bracket){
  
  analyze<-TourneySeeds[TourneySeeds$Season==year, ]
  analyze$Team_Full<-id_df$Team_Full[match(analyze$Team, id_df$TeamID)]
  names<-unique(analyze[, c("Team_Full", "Seed")])
  names$Seed<-as.numeric(substring(names$Seed, 2, 3))
  pasteSeed<-function(teams){
    paste(names$Seed[match( teams, names$Team_Full)], teams, sep=" ")
  }
  
  round64 <- tourneySims[tourneySims$Round=="R1" & tourneySims$Sim==1,]
  round64<-round64[order(round64$Slot, decreasing = F), ]
  round64$Loser_Full<-id_df$Team_Full[match(round64$Loser, id_df$TeamID)]
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
    library(Rsymphony)
    
    
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

#iterate through previous rounds and maximize EV s.t. bracket remains valid i.e. team can't leave and return
customBracket1<-brackets[, 1:63]
customBracket1[,1:48 ]<-t(sapply(1:nrow(customBracket1), function(x) optimizeRounds(maximize_round = c("R1", "R2"),given_round = "R3", row=customBracket1[x, ] )))

customBracket2<-brackets[, 1:63]
customBracket2[,1:56]<-t(sapply(1:nrow(customBracket2), function(x) optimizeRounds(maximize_round = c("R1", "R2", "R3"),given_round = "R4", row=customBracket2[x, ] )))

customBracket3<-brackets[, 1:63]
customBracket3[,1:60 ]<-t(sapply(1:nrow(customBracket3), function(x) optimizeRounds(maximize_round =  c("R1", "R2", "R3","R4"),given_round = "R5", row=customBracket3[x, ] )))

customBracket4<-brackets[, 1:63]
customBracket4[,1:62 ]<-t(sapply(1:nrow(customBracket4), function(x) optimizeRounds(maximize_round =  c("R1", "R2", "R3","R4", "R5"),given_round = "R6", row=customBracket4[x, ] )))


#maximize EV without constraining bracket i.e. allowing teams to lose and then return
customBracket5<-brackets[, 1:63]
customBracket5[,1:32 ]<-t(sapply(1:nrow(customBracket5), function(x) maximizeRound(maximize_round = "R1", row=customBracket5[x, ] )))
customBracket5[,33:48 ]<-t(sapply(1:nrow(customBracket5), function(x) maximizeRound(maximize_round = "R2", row=customBracket5[x, ] )))
customBracket5[,49:56 ]<-t(sapply(1:nrow(customBracket5), function(x) maximizeRound(maximize_round = "R3", row=customBracket5[x, ] )))

customBracket6<-brackets[, 1:63]
customBracket6[,1:32 ]<-t(sapply(1:nrow(customBracket6), function(x) maximizeRound(maximize_round = "R1", row=customBracket6[x, ] )))
customBracket6[,33:48 ]<-t(sapply(1:nrow(customBracket6), function(x) maximizeRound(maximize_round = "R2", row=customBracket6[x, ] )))
customBracket6[,49:56 ]<-t(sapply(1:nrow(customBracket6), function(x) maximizeRound(maximize_round = "R3", row=customBracket6[x, ] )))
customBracket6[,57:60 ]<-t(sapply(1:nrow(customBracket6), function(x) maximizeRound(maximize_round = "R4", row=customBracket6[x, ] )))

#calculate set of custom brackets against bracket pool
# cl<-makeCluster(2, type = "SOCK")
# registerDoSNOW(cl)
improved<- foreach(i=list(
  brackets, customBracket1, 
  customBracket2,
  customBracket3     ,customBracket4
  # , customBracket5, customBracket6
), .packages = c( "data.table", "reshape2", "plyr")) %dopar% {
  calcBrackets(i, brackets)
}
results<- foreach(i=improved, .packages = c( "Rsymphony","Rcplex")) %dopar% {
  getOptimal(i, percentile = percentile, numBrackets =numBrackets, speedUp=T)
}
sapply(1:length(improved), function(y) sum(results[[y]]$x[(nrow(improved[[y]])+1):(nrow(improved[[y]])+numSims)])/numSims)

# which.max(sapply(1:length(improved), function(y) sum(results[[y]]$x[(nrow(improved[[y]])+1):(nrow(improved[[y]])+numSims)])/numSims)[1:5])
# 
# source('~/Kaggle/NCAA/march-madness/improve brackets helper.R')
# 
# #get optimal results
x<-4
inspect<-improved[[x]]
result<-results[[x]]
# percentile<-.97
# bool<-grepl("Percentile", colnames(inspect)) & !grepl("Actual", colnames(inspect))
# inspect[,paste("Prob", percentile*100, sep="", collapse="") ]<-apply(inspect[, bool], 1, function(x) sum(x>percentile)/numSims)
# inspect$Index<-nrow(inspect)+1-rank(inspect[,paste("Prob", percentile*100, sep="", collapse="") ])
inspect[which(result$x[1:nrow(inspect)]==1),c(1:63, which(colnames(inspect)%in% c("Prob95", "Prob97", "Prob90", "Index") | grepl("Actual", colnames(inspect))) )]
sum(result$x[(nrow(inspect)+1):(nrow(inspect)+numSims)])/numSims
# plot(inspect$Index~inspect$Prob97)
# # 
# # #plotting function
plotBracket(inspect[which(result$x[1:nrow(inspect)]==1),c(1:63)][1, ])

# improved<-improved[[3]]
# save(improved, file=paste(c("~/Kaggle/NCAA/march-madness/Shiny/", year, "/Improved_Brackets.Rda"), sep='', collapse=""))
