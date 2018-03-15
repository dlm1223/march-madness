library(datasets)

# Define a server for the Shiny app
function(input, output, session) {
  
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
  
  brackets<-eventReactive(input$apply_scoring,ignoreNULL = F, {
    if(input$r1!=10| input$r2!=20| input$r3!=40| input$r4!=80 | input$r5!=160| input$r6!=320|
       input$upset1_mult!=1| input$upset2_mult!=1| input$upset3_mult!=1|
       input$r1_seed_mult!=0| input$r2_seed_mult!=0| input$r3_seed_mult!=0|
       input$r4_seed_mult!=0| input$r5_seed_mult!=0| input$r6_seed_mult!=0|
       input$r1_seed_bonus!=0| input$r2_seed_bonus!=0| input$r3_seed_bonus!=0|
       input$r4_seed_bonus!=0| input$r5_seed_bonus!=0| input$r6_seed_bonus!=0  ){
      
      
      #load data
      year<-input$year
      load(paste0(c( input$year, "/alldata.RData"), sep="", collapse=""))
      load(paste0(c( input$year, "/TourneySims_500sims.Rda"), sep="", collapse=""))
      load(paste0(c(input$year, "/BracketResults_FullTournament_500sims.Rda"), sep="", collapse=""))
      source("simulate calc payouts_merge.R", local=T)
      
    } else{
      # setwd()
      load(paste0(c(input$year, "/BracketResults_FullTournament_500sims.Rda"), sep="", collapse=""))
    }
    brackets
  }
  )
  
  optimization <- eventReactive( input$refreshInputs, {
    brackets<-brackets()
    
    percentile<-input$percentile
    numBrackets<-input$numBrackets
    year<-input$year
    backtest<-ifelse(year==2018, F,T)
    
    source("optimize brackets.R", local=T )
    list(brackets=brackets, percentiles=percentiles, result=result, year=year, backtest=backtest)
    
  })
  
  
  
  
  output$summary <- renderTable({
    
    # withProgress(message="Calculating",value=0.5, {
    optimization <- optimization()
    
    
    brackets<-optimization$brackets
    percentiles<-optimization$percentiles
    result<-optimization$result
    percentile<-isolate(input$percentile)
    backtest<-optimization$backtest
    
    cols<- grepl("R4|R5|R6|_Actual", colnames(brackets)) | colnames(brackets)%in% paste("Prob", percentile*100, sep="", collapse="")
    col<-colnames(brackets)[grepl("Percentile", colnames(brackets)) & grepl("Actual", colnames(brackets))]
    if(backtest==F){
      col<- colnames(brackets)%in% paste("Prob", percentile*100, sep="", collapse="")
    }
    
    test<-brackets[which(result$x[1:ncol(percentiles)]==1),cols][ order(brackets[which(result$x[1:ncol(percentiles)]==1), col], decreasing = T),]
    
    colnames(test)<-gsub("501|1001", "", colnames(test))
    colnames(test)[colnames(test)=="Sim_Actual"]<-"Score_Actual"
    test
    
  })
  output$summary2<-renderPrint({
    optimization <- optimization()
    brackets<-optimization$brackets
    percentiles<-optimization$percentiles
    result<-optimization$result
    
    est<-sum(result$x[(ncol(percentiles)+1):(ncol(percentiles)+500)])/500
    paste0(c("Estimated Probability of ", isolate(input$percentile)*100, "th Percentile: ", est), collapse="", sep="")
    
  })
  
  output$summary3<-renderPrint({
    optimization <- optimization()
    brackets<-optimization$brackets
    percentiles<-optimization$percentiles
    result<-optimization$result
    year<-optimization$year
    
    
    if((year)==2018){
      ret<-"TBD"
    } else{
      top<-max(brackets[which(result$x[1:ncol(percentiles)]==1), grepl("Percentile", colnames(brackets)) & grepl("Actual", colnames(brackets))])
      ret<-top>=isolate(input$percentile)
    }
    paste0(c("Success? " ,ret), sep="", collapse="")
    
  })
  
  
  output$table1 <- renderTable({
    optimization <- optimization()
    year<-isolate(optimization$year)
    load(paste0(c( year, "/alldata.RData"), sep="", collapse=""))
    load(paste0(c( year, "/TourneySims_500sims.Rda"), sep="", collapse=""))
    numSims<-max(tourneySims$Sim)-backtest
    
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
    percentiles<-optimization$percentiles
    result<-optimization$result
    year<-isolate(optimization$year)
    
    bool<-grepl("Percentile", colnames(brackets)) & !grepl("Actual", colnames(brackets))
    
    load(paste0(c( year, "/alldata.RData"), sep="", collapse=""))
    
    numSims<-sum(bool)
    r1<-as.data.frame(table(as.vector(unlist(brackets[, grepl("R1", colnames(brackets))])))/numSims);colnames(r1)<-c("Team_Full", "R1")
    r2<-as.data.frame(table(as.vector(unlist(brackets[, grepl("R2", colnames(brackets))])))/numSims);colnames(r2)<-c("Team_Full", "R2")
    r3<-as.data.frame(table(as.vector(unlist(brackets[, grepl("R3", colnames(brackets))])))/numSims);colnames(r3)<-c("Team_Full", "R3")
    r4<-as.data.frame(table(as.vector(unlist(brackets[, grepl("R4", colnames(brackets))])))/numSims);colnames(r4)<-c("Team_Full", "R4")
    r5<-as.data.frame(table(as.vector(unlist(brackets[, grepl("R5", colnames(brackets))])))/numSims);colnames(r5)<-c("Team_Full", "R5")
    r6<-as.data.frame(table(as.vector(unlist(brackets[, grepl("R6", colnames(brackets))])))/numSims);colnames(r6)<-c("Team_Full", "R6")
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
      percentiles<-optimization$percentiles
      result<-optimization$result
      year<-optimization$year
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
      round64$Loser_Full<-id_df$Team_Full[match(round64$Loser, id_df$TeamID)]
      ords<-c(1, 8, 4, 5, 3, 6, 2, 7)
      round64<-round64[order(round64$Slot, decreasing = F), ]
      round64<-round64[c(ords, ords+8, ords+16, ords+24), ]
      teams<-lapply(1:nrow(round64),
                    function(x) if(round64$team_seed[x]< round64$loser_seed[x]) {
                      c(round64$Team_Full[x], round64$Loser_Full[x])
                    }else{
                      c(round64$Loser_Full[x], round64$Team_Full[x])
                    })
      teams<-unlist(teams) %>% pasteSeed()
      
      plotBracket<-function(bracket){
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
      for (i in which(result$x[1:ncol(percentiles)]==1)){
        plotBracket(bracket = brackets[i, 1:63])  
      }
      dev.off()
      file.copy(from="Optimal Brackets.pdf", to=file)
      
    }
  )
}

