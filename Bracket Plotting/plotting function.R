#payouts
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
# tourneySims$seed_diff<-floor(sapply(tourneySims$team_seed-tourneySims$loser_seed,function(x) max(x, 0)/2))

tourneySims$Payout<-ifelse(grepl("R1", tourneySims$Slot), tourneySims$Payout+tourneySims$team_seed*as.numeric(input$r1_seed_bonus),
                           ifelse(grepl("R2", tourneySims$Slot), tourneySims$Payout+tourneySims$team_seed*as.numeric(input$r2_seed_bonus),
                                  ifelse(grepl("R3", tourneySims$Slot), tourneySims$Payout+tourneySims$team_seed*as.numeric(input$r3_seed_bonus),
                                         ifelse(grepl("R4", tourneySims$Slot),tourneySims$Payout+tourneySims$team_seed*as.numeric(input$r4_seed_bonus),
                                                ifelse(grepl("R5", tourneySims$Slot), tourneySims$Payout+tourneySims$team_seed*as.numeric(input$r5_seed_bonus),
                                                       ifelse(grepl("R6", tourneySims$Slot), tourneySims$Payout+tourneySims$team_seed*as.numeric(input$r6_seed_bonus),tourneySims$Payout))))))
tourneySims$Payout<-as.numeric(tourneySims$Payout)


analyze<-TourneySeeds[TourneySeeds$Season==year, ]
analyze$Team_Full<-id_df$Team_Full[match(analyze$Team, id_df$TeamID)]
names<-unique(analyze[, c("Team_Full", "Seed")])
names$Seed<-as.numeric(substring(names$Seed, 2, 3))
pasteSeed<-function(teams){
  paste(names$Seed[match( teams, names$Team_Full)], teams, sep=" ")
}

bool<-grepl("Percentile", colnames(brackets)) & !grepl("Actual", colnames(brackets))
numSims<-sum(bool)

numBrackets<-nrow(brackets)
r1<-as.data.frame(table(as.vector(unlist(brackets[, grepl("R1", colnames(brackets))& !grepl("Exp", colnames(brackets)) ])))/numBrackets);colnames(r1)<-c("Team_Full", "R1")
r2<-as.data.frame(table(as.vector(unlist(brackets[, grepl("R2", colnames(brackets))& !grepl("Exp", colnames(brackets))])))/numBrackets);colnames(r2)<-c("Team_Full", "R2")
r3<-as.data.frame(table(as.vector(unlist(brackets[, grepl("R3", colnames(brackets))& !grepl("Exp", colnames(brackets))])))/numBrackets);colnames(r3)<-c("Team_Full", "R3")
r4<-as.data.frame(table(as.vector(unlist(brackets[, grepl("R4", colnames(brackets))& !grepl("Exp", colnames(brackets))])))/numBrackets);colnames(r4)<-c("Team_Full", "R4")
r5<-as.data.frame(table(as.vector(unlist(brackets[, grepl("R5", colnames(brackets))& !grepl("Exp", colnames(brackets))])))/numBrackets);colnames(r5)<-c("Team_Full", "R5")
r6<-as.data.frame(table(as.vector(unlist(brackets[, grepl("R6", colnames(brackets))& !grepl("Exp", colnames(brackets))])))/numBrackets);colnames(r6)<-c("Team_Full", "R6")
ownership<-Reduce(function(x, y) merge(x, y, all=TRUE), list(r1, r2, r3, r4, r5, r6))
ownership[is.na(ownership)]<-0

row.names(ownership)<-pasteSeed(ownership$Team_Full)
# ownership$Team_Full<-NULL
ownership[order(ownership$R6, ownership$R5, ownership$R4, ownership$R3, ownership$R2, ownership$R1, decreasing = T), -1]

inspect<-as.data.frame.matrix(table(tourneySims$Team_Full[tourneySims$Sim<=numSims], tourneySims$Round[tourneySims$Sim<=numSims])/numSims) 
inspect$Team_Full<-row.names(inspect)
row.names(inspect)<-pasteSeed(row.names(inspect))
inspect[order(inspect$R6,inspect$R5,inspect$R4,inspect$R3, inspect$R2,  decreasing = T), -7]




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
calcBracket<-function(customBracket, brackets=brackets){
  

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
  
  ##
  
  colnames(test)<-gsub("Points.", "Sim", colnames(test))
  colnames(test)[grepl(max(tourneySims$Sim), colnames(test))]<-paste(colnames(test)[grepl(max(tourneySims$Sim), colnames(test))], "Actual", sep="_")
  
  
  test<-rbind.fill(test, brackets[, grepl("Sim", colnames(brackets))])
  numSims<-max(tourneySims$Sim)-1
  
  
  colnames(test)<-gsub("_Actual", "", colnames(test))
  for(i in 1:(numSims+1) ){
    test[, paste0("Percentile", i)]<-ecdf(test[, paste0("Sim", i)])(test[,  paste0("Sim", i)])
  }
  colnames(test)[grepl(numSims+1, colnames(test))]<-paste(colnames(test)[grepl(numSims+1, colnames(test))], "Actual", sep="_")
  
  test<-test[which(test$Bracket>=10000), ]
  bool<-grepl("Percentile", colnames(test)) & !grepl("Actual", colnames(test))
  # test$Prob80<-apply(test[, bool], 1, function(x) sum(x>.80)/numSims)
  test$Prob90<-apply(test[, bool], 1, function(x) sum(x>.9)/numSims)
  test$Prob95<-apply(test[, bool], 1, function(x) sum(x>.95)/numSims)
  test$Prob97<-apply(test[, bool], 1, function(x) sum(x>.97)/numSims)
  test$Prob99<-apply(test[, bool], 1, function(x) sum(x>.99)/numSims)
  
  
  test$Prob90_all<-sum(apply(test[, bool], 2, max)>.9)/numSims
  test$Prob97_all<-sum(apply(test[, bool], 2, max)>.97)/numSims
  test$Prob99_all<-sum(apply(test[, bool], 2, max)>.99)/numSims
  
  # test$Prob995<-apply(test[, bool], 1, function(x) sum(x>.995)/numSims)
  test$SimMean<-apply(test[, bool],1, mean)
  test[, !grepl("Percent|Sim|Bracket", colnames(test)) | grepl("Actual", colnames(test)) ]  
  
}

