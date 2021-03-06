library(datasets)

# Define a server for the Shiny app
function(input, output, session) {
  
  variables<-reactiveValues(brackets= NULL,improved=NULL, year=2019)
  
  #helper functions to perform bracket calculations such as getOptimal, calcBracket, plotBracket
  
  # cl<-makeCluster(2, type = "SOCK")
  # registerDoSNOW(cl)
  
  observeEvent(input$restore_defaults, {
    updateNumericInput(session, "r1", value = 10) 
    updateNumericInput(session, "r2", value = 20) 
    updateNumericInput(session, "r3", value = 40) 
    updateNumericInput(session, "r4", value = 80) 
    updateNumericInput(session, "r5", value = 160) 
    updateNumericInput(session, "r6", value = 320) 
    updateNumericInput(session, "upset1_mult", value = 1)
    updateNumericInput(session, "upset2_mult", value = 1) 
    updateNumericInput(session, "upset3_mult", value = 1) 
    updateNumericInput(session, "r1_seed_mult", value = 0) 
    updateNumericInput(session, "r2_seed_mult", value = 0) 
    updateNumericInput(session, "r3_seed_mult", value = 0) 
    updateNumericInput(session, "r4_seed_mult", value = 0) 
    updateNumericInput(session, "r5_seed_mult", value = 0) 
    updateNumericInput(session, "r6_seed_mult", value = 0) 
    updateNumericInput(session, "r1_seed_bonus", value = 0) 
    updateNumericInput(session, "r2_seed_bonus", value = 0) 
    updateNumericInput(session, "r3_seed_bonus", value = 0) 
    updateNumericInput(session, "r4_seed_bonus", value = 0) 
    updateNumericInput(session, "r5_seed_bonus", value = 0) 
    updateNumericInput(session, "r6_seed_bonus", value = 0) 
    # updateSelectInput(session, "year", value=2017)
  })
  
  # current_input <- reactiveValues(current=list(r1=)
  progress_shiny <-function (progress, step = 1){
    list(
      init = function(n){},
      step = function() {
        progress$set( progress$getValue() + step )
      }, 
      term = function(){}
    )
  }
  
  observeEvent(input$apply_scoring,ignoreNULL = F, {
    
    
    #load data
    load(paste0(c( input$year, "/team-data.RData"), sep="", collapse=""))
    load(paste0(c( input$year, "/TourneySims_1000sims.Rda"), sep="", collapse=""))
    load(paste0(c(input$year, "/BracketResults_FullTournament_1000sims.Rda"), sep="", collapse=""))
    
    
    
    if(input$r1!=10| input$r2!=20| input$r3!=40| input$r4!=80 | input$r5!=160| input$r6!=320|
       input$upset1_mult!=1| input$upset2_mult!=1| input$upset3_mult!=1|
       input$r1_seed_mult!=0| input$r2_seed_mult!=0| input$r3_seed_mult!=0|
       input$r4_seed_mult!=0| input$r5_seed_mult!=0| input$r6_seed_mult!=0|
       input$r1_seed_bonus!=0| input$r2_seed_bonus!=0| input$r3_seed_bonus!=0|
       input$r4_seed_bonus!=0| input$r5_seed_bonus!=0| input$r6_seed_bonus!=0  ){
      
      scoring<-"custom"
      progress <- shiny::Progress$new(session, min=0, max=100)
      on.exit(progress$close())
      progress$set(message = 'Updating Bracket Payouts')
      progress$set( value = 1)
      
      #re-calculate bracket-scores using new scoring
      source("calculate-bracket-payouts.R", local=T)
      
      progress$set( value = 25)
      
      sims<-max(tourneySims$Sim)-backtest
      bracket.data<-TourneyRounds[grepl("R", TourneyRounds$Slot) & TourneyRounds$Season==year,]
      bracket.data$Team<-TourneySeeds$Team[TourneySeeds$Season==year][match(bracket.data$Seed,TourneySeeds$Seed[TourneySeeds$Season==year] )]
      bracket.data$Team_Full<-Teams$Team_Full[match(bracket.data$Team, Teams$TeamID)]
      bracket.data$Round<-substring(bracket.data$Slot, 1 ,2)
      
      bracket.data<-TourneyRounds[grepl("R", TourneyRounds$Slot) & TourneyRounds$Season==year,]
      bracket.data$Team<-TourneySeeds$Team[TourneySeeds$Season==year][match(bracket.data$Seed,TourneySeeds$Seed[TourneySeeds$Season==year] )]
      bracket.data$Team_Full<-Teams$Team_Full[match(bracket.data$Team, Teams$TeamID)]
      bracket.data$Round<-substring(bracket.data$Slot, 1 ,2)
      
      if(playInTbd==T & input$year==2018){
        bracket.data$Team_Full[bracket.data$Team_Full%in% c("Arizona State", "Syracuse")]<-"Asu/sy"
        bracket.data$Team_Full[bracket.data$Team_Full%in% c("St Bonaventure", "Ucla")]<-"Bon/la"
        bracket.data$Team_Full[bracket.data$Team_Full%in% c("Long Island", "Radford")]<-"Liu/rad"
        bracket.data$Team_Full[bracket.data$Team_Full%in% c("North Carolina Central", "Texas Southern")]<-"Ncc/ts"
      } else if (playInTbd==T & input$year==2019){
        bracket.data$Team_Full[bracket.data$Team_Full%in% c("Arizona State", "St Johns")]<-"Asu/sju"
        bracket.data$Team_Full[bracket.data$Team_Full%in% c("Belmont", "Temple")]<-"Bel/tem"
        bracket.data$Team_Full[bracket.data$Team_Full%in% c("Fairleigh Dickinson", "Prairie View A&m")]<-"Fdu/pv"
        bracket.data$Team_Full[bracket.data$Team_Full%in% c("North Dakota State", "North Carolina Central")]<-"Nds/ncc"
      }
      
      
      expected<-data.table(tourneySims[tourneySims$Sim<=sims,])
      expected<-expected[, list(Expected=sum(Payout)/sims), by=c("Team_Full", "Round")]  
      expected<-data.frame(expected)
      expected<-merge(expected, bracket.data[!duplicated(bracket.data[, c("Team_Full", "Round")]), c("Team_Full", "Round", "Slot")], by=c("Team_Full", "Round"), all=T)
      expected[is.na(expected)]<-0
      
      #functions for optimizing brackets
      source("optimize-brackets.R", local=T)
      
      customBracket3<-brackets[, 1:63]
      customBracket3<-data.frame(rbindlist(lapply(1:nrow(customBracket3), function(x) 
        optimizeRounds(rounds   = c("R1", "R2", "R3"),fixed.rounds  = "R4", bracket=customBracket3[x, ] ))))
      
      #delete duplicated brackets
      customBracket3<-customBracket3[!duplicated(customBracket3[,1:63]),]
      
      progress$set( value = 50)
      
      improved<-calcBrackets(customBrackets = customBracket3, brackets=brackets, tourneySims = tourneySims)
      
      
    } else{
      scoring<-'default'
      load(paste0(c(input$year, "/Improved-Brackets.Rda"), sep="", collapse=""))
      customBracket2<-customBracket2[!duplicated(customBracket2[,1:63]),]
      improved<-customBracket2
    }
    
    
    # progress$set( value = 100)
    
    # variables$brackets<-brackets
    variables$year<-input$year
    variables$improved<-improved
    variables$scoring<-scoring
    
  }
  )
  
  
  #optimization stores output of optimization, given brackets
  optimization <- eventReactive( input$refreshInputs, {
    
    progress2 <- shiny::Progress$new(session, min=0, max=100)
    on.exit(progress2$close())
    progress2$set(message = 'Running Optimization')
    progress2$set( value = 25)
    
    scoring<-variables$scoring
    year<-variables$year
    improved<-variables$improved
    
    percentile<-input$percentile
    numBrackets<-input$numBrackets
    backtest<-ifelse(year==2019, F,T)
    
    source("optimize-brackets.R", local=T)
    
    results<-getOptimal(improved, percentile, numBrackets)
    
    sims<-ncol(improved[, grepl("Sim", colnames(improved))])-backtest
    list(  result=results, backtest=backtest, brackets=improved, year=year, sims=sims, scoring=scoring, percentile=percentile)
    
  })
  
  
  
  output$summary <- renderTable({
    
    # withProgress(message="Calculating",value=0.5, {
    optimization <- optimization()
    
    brackets<-optimization$brackets
    year<-optimization$year
    result<-optimization$result
    percentile<-optimization$percentile
    backtest<-optimization$backtest
    sims<-optimization$sims
    
    bool<-grepl("Percentile", colnames(brackets)) & !grepl("Actual", colnames(brackets))
    brackets[,paste("Prob", 90, sep="", collapse="") ]<-apply(brackets[, bool], 1, function(x) sum(x>=.9)/sims)
    brackets[,paste("Prob", 95, sep="", collapse="") ]<-apply(brackets[, bool], 1, function(x) sum(x>=.95)/sims)
    brackets[,paste("Prob", 97, sep="", collapse="") ]<-apply(brackets[, bool], 1, function(x) sum(x>=.97)/sims)
    brackets[,paste("Prob", percentile*100, sep="", collapse="") ]<-apply(brackets[, bool], 1, function(x) sum(x>=percentile)/sims)
    
    #displaycols
    cols<- colnames(brackets)[grepl("R4|R5|R6|Actual|Prob90|Prob95|Prob97", colnames(brackets)) | colnames(brackets)%in% paste("Prob", percentile*100, sep="", collapse="")]
    cols<-c(cols[!grepl("Actual", cols)], cols[grepl("Actual", cols)])
    
    #ordercol
    col<-colnames(brackets)[grepl("Percentile", colnames(brackets)) & grepl("Actual", colnames(brackets))]
    if(backtest==F){
    col<- colnames(brackets)%in% paste("Prob", percentile*100, sep="", collapse="")
    }
    
    test<-brackets[which(result$x[1:nrow(brackets)]==1),cols][ order(brackets[which(result$x[1:nrow(brackets)]==1), col], decreasing = T),]
    
    colnames(test)[colnames(test)=="Sim.Actual"]<-"Score.Actual"
    
    test
    
  })
  output$summary2<-renderPrint({
    
    optimization <- optimization()
    brackets<-optimization$brackets
    result<-optimization$result
    percentile<-optimization$percentile
    sims<-optimization$sims
    
    est<-round(sum(result$x[(nrow(brackets)+1):(nrow(brackets)+sims)])/sims,3)
    suffix<-case_when(percentile%in% c(.91)~ "st",
                      percentile%in% c(.92)~ "nd",
                      percentile%in% c(.93)~ "rd",
                      percentile%in% c(.90, .94, .95, .96, .97, .98,.99)~"th"  )
    
    paste0(c("Estimated Probability of ", percentile*100, suffix," Percentile: ", est), collapse="", sep="")
    
  })
  
  output$summary3<-renderPrint({
    
    optimization <- optimization()
    brackets<-optimization$brackets
    year<-optimization$year
    result<-optimization$result
    percentile<-optimization$percentile
    
    if((year)==2019){
      ret<-"TBD"
    } else{
      top<-max(brackets[which(result$x[1:nrow(brackets)]==1), grepl("Percentile", colnames(brackets)) & grepl("Actual", colnames(brackets))])
      ret<-top>=percentile
    }
    paste0(c("Success? " ,ret), sep="", collapse="")
    
  })
  
  
  output$table1 <- renderTable({
    
    optimization <- optimization()
    year<-optimization$year
    
    load(paste0(c(year, "/BracketResults_FullTournament_1000sims.Rda"), sep="", collapse=""))
    load(paste0(c( year, "/team-data.RData"), sep="", collapse=""))
    load(paste0(c( year, "/TourneySims_1000sims.Rda"), sep="", collapse=""))
    sims<-optimization$sims
    
    analyze<-TourneySeeds[TourneySeeds$Season==year, ]
    analyze$Team_Full<-Teams$Team_Full[match(analyze$Team, Teams$TeamID)]
    if(playInTbd==T & year==2018){
      analyze$Team_Full[analyze$Team_Full%in% c("Arizona State", "Syracuse")]<-"Asu/sy"
      analyze$Team_Full[analyze$Team_Full%in% c("St Bonaventure", "Ucla")]<-"Bon/la"
      analyze$Team_Full[analyze$Team_Full%in% c("Long Island", "Radford")]<-"Liu/rad"
      analyze$Team_Full[analyze$Team_Full%in% c("North Carolina Central", "Texas Southern")]<-"Ncc/ts"
    } else if (playInTbd==T & year==2019){
      analyze$Team_Full[analyze$Team_Full%in% c("Arizona State", "St Johns")]<-"Asu/sju"
      analyze$Team_Full[analyze$Team_Full%in% c("Belmont", "Temple")]<-"Bel/tem"
      analyze$Team_Full[analyze$Team_Full%in% c("Fairleigh Dickinson", "Prairie View A&m")]<-"Fdu/pv"
      analyze$Team_Full[analyze$Team_Full%in% c("North Dakota State", "North Carolina Central")]<-"Nds/ncc"
    }
    # 
    
    
    names<-unique(analyze[, c("Team_Full", "Seed")])
    names$Seed<-as.numeric(substring(names$Seed, 2, 3))
    pasteSeed<-function(teams){
      paste(names$Seed[match( teams, names$Team_Full)], teams, sep=" ")
    }    
    inspect<-as.data.frame.matrix(table(tourneySims$Team_Full[tourneySims$Sim<=sims], tourneySims$Round[tourneySims$Sim<=sims])/sims) 
    row.names(inspect)<-pasteSeed( row.names(inspect))
    
    inspect[order(inspect$R6,inspect$R5,inspect$R4,inspect$R3, inspect$R2,  decreasing = T), ]
  },     include.rownames=T)
  
  output$table2 <- renderTable({   
    
    optimization <- optimization()
    year<-optimization$year
    
    load(paste0(c(year, "/BracketResults_FullTournament_1000sims.Rda"), sep="", collapse=""))
    load(paste0(c( year, "/team-data.RData"), sep="", collapse=""))
    load(paste0(c( year, "/TourneySims_1000sims.Rda"), sep="", collapse=""))
    
    
    bool<-grepl("Percentile", colnames(brackets)) & !grepl("Actual", colnames(brackets))
    
    sims<-optimization$sims
    numBrackets<-nrow(brackets)
    
    r1<-as.data.frame(table(as.vector(unlist(brackets[, grepl("R1", colnames(brackets))])))/numBrackets);colnames(r1)<-c("Team_Full", "R1")
    r2<-as.data.frame(table(as.vector(unlist(brackets[, grepl("R2", colnames(brackets))])))/numBrackets);colnames(r2)<-c("Team_Full", "R2")
    r3<-as.data.frame(table(as.vector(unlist(brackets[, grepl("R3", colnames(brackets))])))/numBrackets);colnames(r3)<-c("Team_Full", "R3")
    r4<-as.data.frame(table(as.vector(unlist(brackets[, grepl("R4", colnames(brackets))])))/numBrackets);colnames(r4)<-c("Team_Full", "R4")
    r5<-as.data.frame(table(as.vector(unlist(brackets[, grepl("R5", colnames(brackets))])))/numBrackets);colnames(r5)<-c("Team_Full", "R5")
    r6<-as.data.frame(table(as.vector(unlist(brackets[, grepl("R6", colnames(brackets))])))/numBrackets);colnames(r6)<-c("Team_Full", "R6")
    ownership<-Reduce(function(x, y) merge(x, y, all=TRUE), list(r1, r2, r3, r4, r5, r6))
    ownership[is.na(ownership)]<-0
    
    analyze<-TourneySeeds[TourneySeeds$Season==year, ]
    analyze$Team_Full<-Teams$Team_Full[match(analyze$Team, Teams$TeamID)]
    if(playInTbd==T & year==2018){
      analyze$Team_Full[analyze$Team_Full%in% c("Arizona State", "Syracuse")]<-"Asu/sy"
      analyze$Team_Full[analyze$Team_Full%in% c("St Bonaventure", "Ucla")]<-"Bon/la"
      analyze$Team_Full[analyze$Team_Full%in% c("Long Island", "Radford")]<-"Liu/rad"
      analyze$Team_Full[analyze$Team_Full%in% c("North Carolina Central", "Texas Southern")]<-"Ncc/ts"
    } else if (playInTbd==T & year==2019){
      analyze$Team_Full[analyze$Team_Full%in% c("Arizona State", "St Johns")]<-"Asu/sju"
      analyze$Team_Full[analyze$Team_Full%in% c("Belmont", "Temple")]<-"Bel/tem"
      analyze$Team_Full[analyze$Team_Full%in% c("Fairleigh Dickinson", "Prairie View A&m")]<-"Fdu/pv"
      analyze$Team_Full[analyze$Team_Full%in% c("North Dakota State", "North Carolina Central")]<-"Nds/ncc"
    }
    names<-unique(analyze[, c("Team_Full", "Seed")])
    names$Seed<-as.numeric(substring(names$Seed, 2, 3))
    pasteSeed<-function(teams){
      paste(names$Seed[match( teams, names$Team_Full)], teams, sep=" ")
    }
    
    ownership$Team_Full<-pasteSeed(ownership$Team_Full)
    ownership[order(ownership$R6, ownership$R5, ownership$R4, ownership$R3, ownership$R2, ownership$R1, decreasing = T), ]
  })
  output$selected_year <- renderText({
    
    optimization <- optimization()
    year<-optimization$year
    
    paste("Year: ", year)
  })
  
  output$selected_scoring <- renderText({
    optimization <- optimization()
    year<-optimization$year
    scoring<-optimization$scoring    
    paste0(c("Year: ",year ,". Scoring: ", scoring))
  })
  
  #bracket plotting/download
  
  output$bracketPlot1 <- renderPlot({
    optimization <- optimization()
    year<-optimization$year
    
    load(paste0(c( year, "/team-data.RData"), sep="", collapse=""))
    load(paste0(c( year, "/TourneySims_1000sims.Rda"), sep="", collapse=""))
    
    brackets<-optimization$brackets
    result<-optimization$result
    i<-which(result$x[1:nrow(brackets)]==1)[1]
    
    source("optimize-brackets.R", local=T)
    plotBracket(bracket = brackets[i, 1:63], text.size=.85)  
  })
  output$bracketPlot2 <- renderPlot({
    optimization <- optimization()
    year<-optimization$year
    
    load(paste0(c( year, "/team-data.RData"), sep="", collapse=""))
    load(paste0(c( year, "/TourneySims_1000sims.Rda"), sep="", collapse=""))
    
    brackets<-optimization$brackets
    result<-optimization$result
    i<-which(result$x[1:nrow(brackets)]==1)[2]
    
    source("optimize-brackets.R", local=T)
    if(!is.na(i)){
      plotBracket(bracket = brackets[i, 1:63], text.size=.85)  
    } else{
      plot(1, type="n", axes=F, xlab="", ylab="")
    }
  })
  output$bracketPlot3 <- renderPlot({
    optimization <- optimization()
    year<-optimization$year
    
    load(paste0(c( year, "/team-data.RData"), sep="", collapse=""))
    load(paste0(c( year, "/TourneySims_1000sims.Rda"), sep="", collapse=""))
    
    brackets<-optimization$brackets
    result<-optimization$result
    i<-which(result$x[1:nrow(brackets)]==1)[3]
    
    source("optimize-brackets.R", local=T)
    if(!is.na(i)){
      plotBracket(bracket = brackets[i, 1:63], text.size=.85)  
    } else{
      plot(1, type="n", axes=F, xlab="", ylab="")
    }
  })
  output$bracketPlot4 <- renderPlot({
    optimization <- optimization()
    year<-optimization$year
    
    load(paste0(c( year, "/team-data.RData"), sep="", collapse=""))
    load(paste0(c( year, "/TourneySims_1000sims.Rda"), sep="", collapse=""))
    
    brackets<-optimization$brackets
    result<-optimization$result
    i<-which(result$x[1:nrow(brackets)]==1)[4]
    
    source("optimize-brackets.R", local=T)
    
    if(!is.na(i)){
      plotBracket(bracket = brackets[i, 1:63], text.size=.85)  
    } else{
      plot(1, type="n", axes=F, xlab="", ylab="")
    }
  })
  output$bracketPlot5 <- renderPlot({
    optimization <- optimization()
    year<-optimization$year
    
    load(paste0(c( year, "/team-data.RData"), sep="", collapse=""))
    load(paste0(c( year, "/TourneySims_1000sims.Rda"), sep="", collapse=""))
    
    brackets<-optimization$brackets
    result<-optimization$result
    i<-which(result$x[1:nrow(brackets)]==1)[5]
    
    source("optimize-brackets.R", local=T)
    if(!is.na(i)){
      plotBracket(bracket = brackets[i, 1:63], text.size=.85)  
    } else{
      plot(1, type="n", axes=F, xlab="", ylab="")
    }
  })
  
  ##https://stackoverflow.com/questions/32012893/shiny-app-run-code-to-generate-a-pdf-then-offer-that-pdf-to-user-for-download
  
  output$downloadBrackets <- downloadHandler(
    
    # # This function returns a string which tells the client
    # # browser what name to use when saving the file.
    filename = "Optimal Brackets.pdf",
    
    # This function should write data to a file given to it by
    # the argument 'file'.
    content = function(file) {
      optimization <- optimization()
      year<-optimization$year
      
      load(paste0(c( year, "/team-data.RData"), sep="", collapse=""))
      load(paste0(c( year, "/TourneySims_1000sims.Rda"), sep="", collapse=""))
      
      brackets<-optimization$brackets
      result<-optimization$result
      
      
      # Write to a file specified by the 'file' argument
      pdf(file = "Optimal Brackets.pdf")
      
      #plotting-bracket function
      source("optimize-brackets.R", local=T) 
      
      for (i in which(result$x[1:nrow(brackets)]==1)){
        plotBracket(bracket = brackets[i, 1:63])  
      }
      dev.off()
      file.copy(from="Optimal Brackets.pdf", to=file)
      
    }
  )
}

