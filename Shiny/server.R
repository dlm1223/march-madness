library(datasets)

# Define a server for the Shiny app
function(input, output, session) {
  
  variables<-reactiveValues(brackets= NULL,improved=NULL, year=2018)
  
  source("improve brackets helper.R", local=T)
  
  
  
  
  
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
    load(paste0(c( input$year, "/alldata.RData"), sep="", collapse=""))
    load(paste0(c( input$year, "/TourneySims_500sims.Rda"), sep="", collapse=""))
    load(paste0(c(input$year, "/BracketResults_FullTournament_500sims.Rda"), sep="", collapse=""))
    
    
    
    if(input$r1!=10| input$r2!=20| input$r3!=40| input$r4!=80 | input$r5!=160| input$r6!=320|
       input$upset1_mult!=1| input$upset2_mult!=1| input$upset3_mult!=1|
       input$r1_seed_mult!=0| input$r2_seed_mult!=0| input$r3_seed_mult!=0|
       input$r4_seed_mult!=0| input$r5_seed_mult!=0| input$r6_seed_mult!=0|
       input$r1_seed_bonus!=0| input$r2_seed_bonus!=0| input$r3_seed_bonus!=0|
       input$r4_seed_bonus!=0| input$r5_seed_bonus!=0| input$r6_seed_bonus!=0  ){
      
      progress <- shiny::Progress$new(session, min=0, max=100)
      on.exit(progress$close())
      progress$set(message = 'Updating Brackets')
      progress$set( value = 1)
      
      source("simulate calc payouts_merge.R", local=T)
      
      progress$set( value = 25)
      
      source("improve brackets.R", local=T)
      
      
      progress$set( value = 50)
      
      improved<-list(customBracket3, customBracket4)
      improved<- llply(improved, function(i)  {calcBrackets(i, brackets)}, .progress = progress_shiny(progress, step=25))
      
    } else{
      
      load(paste0(c(input$year, "/Improved_Brackets.Rda"), sep="", collapse=""))
      
    }
    
    
    # progress$set( value = 100)
    
    variables$brackets<-brackets
    variables$year<-input$year
    
    variables$improved<-improved
    
  }
  )
  
  
  #optimization stores output of optimization, given brackets
  optimization <- eventReactive( input$refreshInputs, {
    
    progress2 <- shiny::Progress$new(session, min=0, max=100)
    on.exit(progress2$close())
    progress2$set(message = 'Running Optimization')
    progress2$set( value = 25)
    
    
    year<-variables$year
    improved<-variables$improved
    
    percentile<-input$percentile
    numBrackets<-input$numBrackets
    backtest<-ifelse(year==2018, F,T)
    
    # source("improve brackets.R", local=T )
    
    #apply optimization to each improved bracket set
    results<-llply(improved, function(x) getOptimal(x, percentile, numBrackets), .progress = progress_shiny(progress2, step=75/length(improved)))
    
    #store result of best bracket
    numSims<-ncol(improved[[1]][, grepl("Sim", colnames(improved[[1]]))])-backtest
    obj<-sapply(1:length(improved), function(y) sum(results[[y]]$x[(nrow(improved[[y]])+1):(nrow(improved[[y]])+numSims)])/numSims)
    result<-results[[which.max(obj)]]
    brackets<-improved[[which.max(obj)]]
    
    list(  result=result, backtest=backtest, brackets=brackets, year=year, numSims=numSims)
    
  })
  
  
  
  output$summary <- renderTable({
    
    # withProgress(message="Calculating",value=0.5, {
    optimization <- optimization()
    
    brackets<-optimization$brackets
    year<-optimization$year
    result<-optimization$result
    percentile<-isolate(input$percentile)
    backtest<-optimization$backtest
    numSims<-optimization$numSims
    
    bool<-grepl("Percentile", colnames(brackets)) & !grepl("Actual", colnames(brackets))
    brackets[,paste("Prob", percentile*100, sep="", collapse="") ]<-apply(brackets[, bool], 1, function(x) sum(x>percentile)/numSims)
    
    cols<- grepl("R4|R5|R6|_Actual", colnames(brackets)) | colnames(brackets)%in% paste("Prob", percentile*100, sep="", collapse="")
    col<-colnames(brackets)[grepl("Percentile", colnames(brackets)) & grepl("Actual", colnames(brackets))]
    if(backtest==F){
      col<- colnames(brackets)%in% paste("Prob", percentile*100, sep="", collapse="")
    }
    
    test<-brackets[which(result$x[1:nrow(brackets)]==1),cols][ order(brackets[which(result$x[1:nrow(brackets)]==1), col], decreasing = T),]
    
    colnames(test)<-gsub("501|1001", "", colnames(test))
    colnames(test)[colnames(test)=="Sim_Actual"]<-"Score_Actual"
    test
    
  })
  output$summary2<-renderPrint({
    
    optimization <- optimization()
    brackets<-optimization$brackets
    result<-optimization$result
    percentile<-isolate(input$percentile) 
    numSims<-optimization$numSims
    
    est<-sum(result$x[(nrow(brackets)+1):(nrow(brackets)+numSims)])/numSims
    paste0(c("Estimated Probability of ", percentile*100, "th Percentile: ", est), collapse="", sep="")
    
  })
  
  output$summary3<-renderPrint({
    
    optimization <- optimization()
    brackets<-optimization$brackets
    year<-optimization$year
    result<-optimization$result
    
    
    if((year)==2018){
      ret<-"TBD"
    } else{
      top<-max(brackets[which(result$x[1:nrow(brackets)]==1), grepl("Percentile", colnames(brackets)) & grepl("Actual", colnames(brackets))])
      ret<-top>=isolate(input$percentile)
    }
    paste0(c("Success? " ,ret), sep="", collapse="")
    
  })
  
  
  output$table1 <- renderTable({
    
    optimization <- optimization()
    year<-optimization$year
    
    
    load(paste0(c( year, "/alldata.RData"), sep="", collapse=""))
    load(paste0(c( year, "/TourneySims_500sims.Rda"), sep="", collapse=""))
    numSims<-optimization$numSims
    
    names<-unique(analyze[, c("Team_Full", "Seed")])
    names$Seed<-as.numeric(substring(names$Seed, 2, 3))
    pasteSeed<-function(teams){
      paste(names$Seed[match( teams, names$Team_Full)], teams, sep=" ")
    }
    
    inspect<-as.data.frame.matrix(table(tourneySims$Team_Full[tourneySims$Sim<=numSims], tourneySims$Round[tourneySims$Sim<=numSims])/numSims) 
    row.names(inspect)<-pasteSeed( row.names(inspect))
    
    inspect[order(inspect$R6,inspect$R5,inspect$R4,inspect$R3, inspect$R2,  decreasing = T), ]
  },     include.rownames=T)
  
  output$table2 <- renderTable({   
    
    optimization <- optimization()
    brackets<-optimization$brackets
    year<-optimization$year
    result<-optimization$result
    
    
    bool<-grepl("Percentile", colnames(brackets)) & !grepl("Actual", colnames(brackets))
    
    load(paste0(c( year, "/alldata.RData"), sep="", collapse=""))
    numSims<-optimization$numSims
    numBrackets<-nrow(brackets)
    
    r1<-as.data.frame(table(as.vector(unlist(brackets[, grepl("R1", colnames(brackets))])))/numBrackets);colnames(r1)<-c("Team_Full", "R1")
    r2<-as.data.frame(table(as.vector(unlist(brackets[, grepl("R2", colnames(brackets))])))/numBrackets);colnames(r2)<-c("Team_Full", "R2")
    r3<-as.data.frame(table(as.vector(unlist(brackets[, grepl("R3", colnames(brackets))])))/numBrackets);colnames(r3)<-c("Team_Full", "R3")
    r4<-as.data.frame(table(as.vector(unlist(brackets[, grepl("R4", colnames(brackets))])))/numBrackets);colnames(r4)<-c("Team_Full", "R4")
    r5<-as.data.frame(table(as.vector(unlist(brackets[, grepl("R5", colnames(brackets))])))/numBrackets);colnames(r5)<-c("Team_Full", "R5")
    r6<-as.data.frame(table(as.vector(unlist(brackets[, grepl("R6", colnames(brackets))])))/numBrackets);colnames(r6)<-c("Team_Full", "R6")
    ownership<-Reduce(function(x, y) merge(x, y, all=TRUE), list(r1, r2, r3, r4, r5, r6))
    ownership[is.na(ownership)]<-0
    
    names<-unique(analyze[, c("Team_Full", "Seed")])
    names$Seed<-as.numeric(substring(names$Seed, 2, 3))
    pasteSeed<-function(teams){
      paste(names$Seed[match( teams, names$Team_Full)], teams, sep=" ")
    }
    ownership$Team_Full<-pasteSeed(ownership$Team_Full)
    ownership[order(ownership$R6, ownership$R5, ownership$R4, ownership$R3, ownership$R2, ownership$R1, decreasing = T), ]
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
      brackets<-optimization$brackets
      year<-optimization$year
      result<-optimization$result
      
      
      load(paste0(c( year, "/alldata.RData"), sep="", collapse=""))
      load(paste0(c( year, "/TourneySims_500sims.Rda"), sep="", collapse=""))
      
      
      # Write to a file specified by the 'file' argument
      pdf(file = "Optimal Brackets.pdf")
      
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
      
      source("improve brackets helper.R", local=T)
      
      for (i in which(result$x[1:nrow(brackets)]==1)){
        plotBracket(bracket = brackets[i, 1:63])  
      }
      dev.off()
      file.copy(from="Optimal Brackets.pdf", to=file)
      
    }
  )
}

