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
    bool<-grepl("Percentile", colnames(brackets)) & !grepl("Actual", colnames(brackets))
    brackets[,paste("Prob", percentile*100, sep="", collapse="") ]<-apply(brackets[, bool], 1, function(x) sum(x>percentile)/numSims)
    brackets$Index<-nrow(brackets)+1-rank(brackets[,paste("Prob", percentile*100, sep="", collapse="") ])
    
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
