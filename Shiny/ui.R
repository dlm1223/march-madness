library(datasets)
library(shiny)
library(shinydashboard)
library(shinyBS)
library(devtools);
library(rsconnect)
library(gridExtra);library(grid)
library(data.table)
library(reshape2);library(plyr);library(zoo);library(dplyr)
library(Rsymphony)
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
                        h6("Note: Will take a few seconds to calculate if not using default scoring"),
                        hr(),
                        actionButton("restore_defaults","Restore Defaults" ),
                        h6("Note: Default is ESPN/Yahoo Scoring")
                        
                      ), 
                      mainPanel(
                        h3("Tournament Year:"),
                        selectInput("year", "", 
                                    # value=2017,
                                    selected=2018,width = "20%",
                                    choices=2013:2018)
                      )),
                    
                    hr(),
                    
                    fluidRow(
                      column(3,
                             h4("Scoring:"),
                             numericInput("r1", "Round 1:",   value=10, min=0, width="30%"),
                             numericInput("r2", "Round 2:",  value=20, min=0, width="30%"),
                             numericInput("r3", "Round 3:",  value=40, min=0, width="30%"),
                             numericInput("r4", "Round 4:",  value=80, min=0, width="30%"),
                             numericInput("r5", "Round 5:",  value=160, min=0, width="30%"),
                             numericInput("r6", "Round 6:",  value=320, min=0, width="30%")),
                      
                      column(3,
                             h4("Multiply Points for Upsets by:"),
                             h6("(ex: 1.5 means upset gets 10x1.5=15 points in round 1. deafult is 1 i.e. no bonus)"),
                             numericInput("upset1_mult", "1-4 Seed Difference:", value=1, min=1, width="30%"),
                             numericInput("upset2_mult", "5-9 Seed Difference:",  value=1, min=1, width="30%"),
                             numericInput("upset3_mult", "10+ Seed Difference:",  value=1, min=1, width="30%")),
                      
                      column(3,
                             # tags$head(tags$style("{font-size:67%}")),
                             h4("Multiply Team-Seed for Correct Picks:"),
                             h6("(ex: r1=1 means an 8 seed gets 10x8=80 points in rd 1)"),
                             selectInput("r1_seed_mult", "Multiply For R1", selected=0, choices=c(0, 1), width="30%"),
                             selectInput("r2_seed_mult", "Multiply For R2",   selected=0, choices=c(0, 1), width="30%"),
                             selectInput("r3_seed_mult", "Multiply For R3", selected=0, choices=c(0, 1),width="30%"),
                             selectInput("r4_seed_mult", "Multiply For R4", selected=0, choices=c(0, 1),width="30%"),
                             selectInput("r5_seed_mult", "Multiply For R5",  selected=0, choices=c(0, 1),width="30%"),
                             selectInput("r6_seed_mult", "Multiply For R6", selected=0, choices=c(0, 1),width="30%")),
                      column(3,
                             # tags$head(tags$style("{font-size:67%}")),
                             h4("Add Team-Seed for Correct Picks:"),
                             h6("(ex: r1=1 means an 8 seed gets 10+8=18 points in rd 1)"),
                             selectInput("r1_seed_bonus", "Add For R1", selected=0, choices=c(0, 1), width="30%"),
                             selectInput("r2_seed_bonus", "Add For R2",   selected=0, choices=c(0, 1), width="30%"),
                             selectInput("r3_seed_bonus", "Add For R3", selected=0, choices=c(0, 1),width="30%"),
                             selectInput("r4_seed_bonus", "Add For R4", selected=0, choices=c(0, 1),width="30%"),
                             selectInput("r5_seed_bonus", "Add For R5",  selected=0, choices=c(0, 1),width="30%"),
                             selectInput("r6_seed_bonus", "Add For R6", selected=0, choices=c(0, 1),width="30%"))
                      
                    )  ),
           
           
           tabPanel("Optimization",
                    mainPanel(
                      textOutput("selected_scoring")
                    ),
                    hr(),
                    # Generate a row with a sidebar
                    sidebarLayout(
                      # Define the sidebar with one input
                      sidebarPanel(
                        titlePanel("Parameters:"),
                        
                        sliderInput("percentile", "Desired Percentile:",
                                    min = .9, max = .99,
                                    value = .95,
                                    step = 0.01),
                        sliderInput("numBrackets", "Number of Brackets:", 
                                    min = 1, max = 5,
                                    value = 1,
                                    step = 1),
                        
                        actionButton("refreshInputs","Run")
                        
                        
                      ),
                      # ,helpText("Data from AT&T (1961) The World's Telephones.")
                      
                      mainPanel(
                        titlePanel("Result:"),
                        
                        tags$head(tags$style("#summary {font-size:67%;width: 100%;}")),
                        tableOutput("summary"),
                        verbatimTextOutput("summary2"),
                        verbatimTextOutput("summary3"),
                        downloadLink("downloadBrackets","Download Brackets"),
                        plotOutput("bracketPlot1", width = 700, height=600),
                        plotOutput("bracketPlot2", width = 700, height=600),
                        plotOutput("bracketPlot3", width = 700, height=600),
                        plotOutput("bracketPlot4", width = 700, height=600),
                        plotOutput("bracketPlot5", width = 700, height=600)
                        #^dont really know how to plot dynamic # of plots so bootlegging it..
                        
                      )
                    )
           ),
           tabPanel("Data",
                    mainPanel(
                      textOutput("selected_year"),
                      # hr(),
                      fluidRow(
                        column(6,
                               h3("Simulation Results:"),
                               tableOutput("table1")
                        ),
                        column(6,
                               h3("Bracket Pool Ownership:"),
                               tableOutput("table2")
                               
                        )
                        
                      ))),
           # tabPanel("Custom Bracket",
           #          h5("Fill out a custom bracket and click calculate to get percentiles based on inputted scoring")
           #          # fluidRow(
           #          #   column(3,
           #          #          h3("Scoring:"),
           #          #          numericInput("r1", "Round 1:",   value=10, min=0, width="30%"),
           #          #          
           #          ),
           tabPanel("About",
                    tags$div(
                      tags$p(),
                      tags$h3("Directions:"),
                      tags$p("Use this app to find optimal brackets based on your pool's scoring system and size. 
To use, enter scoring system/leave defaults on Scoring page and click Apply Scoring, 
then go to Optimization page and enter desired optimization parameters."), 
                      hr(),                      
                      tags$h3("How it Works:"),
                      
                      tags$p("First the app simulates the tournament 500 times to get estimates of team finishes. Next the app simulates a pool of 500 
brackets based on the ESPN data of who picked who that year. This data is shown on the data page. Finally, based on the entered scoring system, it 
calculates how each of the brackets expects to finish
relative to the others across the simulations. Using this data, I can say things like: take the bracket with the highest chance of getting a 90th percentile. 
                              On the Data page, you can how some teams are undervalued in the ownership relative to the simulations and so they will come up more in the optimal soluations"),
tags$p( "One change I made this year is trying to improve upon the optimal solutions. Because there are so many potential brackets, it is likely that there are good brackets that weren't part of the pool simulation. 
I could test out many other brackets against the pool of 500 such as a maximized-expected-points bracket, or a bracket that maximizes EV in the first 2 rounds only.
                             In terms of speed, it will take too long to calculate the bracket payouts of 2000+ potential brackets.
                              After testing out different ideas, the maximized-first 3 rounds EV seemed to perform well regardless of scoring/numlineups/percentile parameters and seems to always improve upon the projected finish.
                              What I do is maximize the EV in the first 3 rounds, given the scoring system and the teams chosen in the 4th round. In summary, it is just a way to slightly improve upon the projected bracket finish instead of just limiting myself to the 500 brackets in the pool simuation." ),
                      div(p(HTML(paste0('You can get the code for all of this here: ',a(href = 'https://github.com/dlm1223/march-madness', 'https://github.com/dlm1223', target="_blank")))))
                      # a(href = 'https://github.com/dlm1223', 'https://github.com/dlm1223')
                    )
                    
           )
)
