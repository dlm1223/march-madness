# library(datasets)
library(shiny)
library(devtools);library(shinydashboard);
library(rsconnect)
library(gridExtra);library(grid)
library(forecast);library(RSelenium);library(RCurl);library(slam);library(XML)
library(caret);library(doSNOW);library(rvest);library(robustbase);library(rpart);library(kernlab)
library(lpSolve);library(lpSolveAPI);library(data.table);library(xgboost);
library(reshape2);library(plyr);library(zoo);library(dplyr);library(gtools);library(RJSONIO)
library(ggplot2);library(glmnet);library(MASS);library(TTR) 
library(Metrics);library(Rsymphony)
# setwd("~/Documents/")
options(stringsAsFactors = F)

#https://shiny.rstudio.com/tutorial/written-tutorial/lesson3/  <- action buttons, etc.
# rsconnect::deployApp('~/Kaggle/NCAA/march-madness/Shiny')


# Use a fluid Bootstrap layout
navbarPage("March Madness Optimization",
           # header="'Get Simulation Results' gets brackets and their simulation scores. 'Run' takes optimal brackets based on simulations.",
           tabPanel("Scoring",
                    sidebarLayout(
                      
                      # Sidebar with a slider input
                      sidebarPanel(
                        actionButton("apply_scoring","Apply Scoring/Year" ),
                        h6("Note: Will take up to a minute to calculate if not using default scoring"),
                        hr(),
                        actionButton("restore_defaults","Restore Defaults" ),
                        h6("Note: Default is ESPN/Yahoo Scoring")
                        
                      ), 
                      mainPanel(
                        h3("Tournament Year:"),
                        selectInput("year", "", 
                                    # value=2017,
                                    selected=2017,width = "20%",
                                    choices=2013:2017)
                      )),
                    
                    hr(),
                    
                    fluidRow(
                      column(3,
                             h3("Scoring:"),
                             numericInput("r1", "Round 1:",   value=10, min=0, width="30%"),
                             numericInput("r2", "Round 2:",  value=20, min=0, width="30%"),
                             numericInput("r3", "Round 3:",  value=40, min=0, width="30%"),
                             numericInput("r4", "Round 4:",  value=80, min=0, width="30%"),
                             numericInput("r5", "Round 5:",  value=160, min=0, width="30%"),
                             numericInput("r6", "Round 6:",  value=320, min=0, width="30%")),
                      
                      column(3,
                             h3("Multiply Points for Upsets by (optional):"),
                             numericInput("upset1_mult", "1-4 Seed Difference:", value=1, min=1, width="30%"),
                             numericInput("upset2_mult", "5-9 Seed Difference:",  value=1, min=1, width="30%"),
                             numericInput("upset3_mult", "10+ Seed Difference:",  value=1, min=1, width="30%")),
                      
                      column(3,
                             # tags$head(tags$style("{font-size:67%}")),
                             h3("Multiply Team-Seed for Correct Picks (optional):"),
                             numericInput("upset1_add", "1-4 Seed Difference:", value=0, min=0, width="30%"),
                             numericInput("upset2_add", "5-9 Seed Difference:",  value=0, min=0, width="30%"),
                             numericInput("upset3_add", "10+ Seed Difference:",  value=0, min=0, width="30%")),
                      column(3,
                             # tags$head(tags$style("{font-size:67%}")),
                             h3("Add Team-Seed for Correct Picks (optional):"),
                             selectInput("r1_seed_bonus", "Add For R1", selected=0, choices=c(0, 1), width="30%"),
                             selectInput("r2_seed_bonus", "Add For R2",   selected=0, choices=c(0, 1), width="30%"),
                             selectInput("r3_seed_bonus", "Add For R3", selected=0, choices=c(0, 1),width="30%"),
                             selectInput("r4_seed_bonus", "Add For R4", selected=0, choices=c(0, 1),width="30%"),
                             selectInput("r5_seed_bonus", "Add For R5",  selected=0, choices=c(0, 1),width="30%"),
                             selectInput("r6_seed_bonus", "Add For R6", selected=0, choices=c(0, 1),width="30%"))
                             
                             # hr(),
                             
                      )  ),
           
           
           tabPanel("Optimization",
                    
                    # hr(),
                    # Generate a row with a sidebar
                    sidebarLayout(
                      
                      # Define the sidebar with one input
                      sidebarPanel(
                        titlePanel("Parameters:"),
                        
                        sliderInput("percentile", "Desired Percentile:",
                                    min = .9, max = .99,
                                    value = .97,
                                    step = 0.01),
                        sliderInput("numBrackets", "Number of Brackets:", 
                                    min = 1, max = 12,
                                    value = 3,
                                    step = 1),
                        
                        actionButton("refreshInputs","Run")
                        
                        
                      ),
                      # ,helpText("Data from AT&T (1961) The World's Telephones.")
                      
                      mainPanel(
                        titlePanel("Result:"),
                        
                        tags$head(tags$style("#summary {font-size:67%;width: 100%;}")),
                        tableOutput("summary"),
                        verbatimTextOutput("summary2"),
                        verbatimTextOutput("summary3")
                        ,downloadLink("downloadBrackets","Download Brackets")
                        
                      )
                    )
           ),
           tabPanel("Data",
                    column(6,
                           h3("Simulation Results:"),
                           tableOutput("table1")
                    ),
                    column(6,
                           h3("Bracket Pool Ownership:"),
                           tableOutput("table2")
                           
                    )
                    
           ),
           
           
           tabPanel("About",
                    tags$div(
                      tags$p(),
                      tags$h3("Directions:"),
                      tags$p("Use this app to find optimal brackets based on your pool's scoring system and size. 
To use, enter scoring system/leave defaults on Scoring page and click Apply Scoring, 
then go to Optimization page and enter desired optimization parameters."), 
                      tags$h3("How it Works:"),
                      
                      tags$p("The app works by using 500 simulations of the tournament generated from estimated win probabilities and a 
pool of 500 brackets that have been generated according to that year's pick percentages taken from ESPN. 
The app then calculates how the brackets finish relative to eachother across the simulations,
                             and finally the app can select multiple brackets in order to maximize the probability of a high finish. 
                             Data from the 500 simulations and 500 brackets can be viewed on Data page. 
                             There you can see how certain teams will end up in the optimal results because of having a high simulation success and low pool ownership."),
                      div(p(HTML(paste0('Download code/data to run yourself or to run with more simulations/brackets: ',a(href = 'https://github.com/dlm1223/march-madness', 'https://github.com/dlm1223', target="_blank")))))
                      # a(href = 'https://github.com/dlm1223', 'https://github.com/dlm1223')
                    )
                    
           )
)
