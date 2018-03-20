# analyze<-TourneySeeds[TourneySeeds$Season==year, ]
# analyze$Team_Full<-id_df$Team_Full[match(analyze$Team, id_df$TeamID)]


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
