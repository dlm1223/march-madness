year<-2019
numBrackets<-1000
playInTbd<-T
load("data/game-data.RData"); 
load(paste0(year,"/TourneySims_1000sims.Rda")) #tourneysims file
name<-paste0(year,"/BracketResults_FullTournament_",numBrackets ,"sims.Rda") #name to save this file as 

###organize ownership data, check names********************


whoPicked<-whoPicked[whoPicked$Season==year, !colnames(whoPicked)%in% "Season"]



#prepare dataframe to be used for ownership-create-breackets
bracket.data<-TourneyRounds[grepl("R", TourneyRounds$Slot) & TourneyRounds$Season==year,]
bracket.data$Team<-TourneySeeds$Team[TourneySeeds$Season==year][match(bracket.data$Seed,TourneySeeds$Seed[TourneySeeds$Season==year] )]
bracket.data$Team_Full<-Teams$Team_Full[match(bracket.data$Team, Teams$TeamID)]
bracket.data$Round<-substring(bracket.data$Slot, 1 ,2)

#handle play-in games, if playINTBD=T then combine play-in games into one
bracket.data<-bracket.data[bracket.data$Team%in% c(tourneySims$Team[grepl("R1", tourneySims$Slot) ],
                                    tourneySims$Loser[grepl("R1", tourneySims$Slot) ]), ]

if(playInTbd==T & year==2018){
  bracket.data$Team_Full[bracket.data$Team_Full%in% c("Arizona State", "Syracuse")]<-"Asu/sy"
  bracket.data$Team_Full[bracket.data$Team_Full%in% c("St Bonaventure", "Ucla")]<-"Bon/la"
  bracket.data$Team_Full[bracket.data$Team_Full%in% c("Long Island", "Radford")]<-"Liu/rad"
  bracket.data$Team_Full[bracket.data$Team_Full%in% c("North Carolina Central", "Texas Southern")]<-"Ncc/ts"
  whoPicked$Team[whoPicked$Team%in% c("Arizona State", "Syracuse")]<-"Asu/sy"
  whoPicked$Team[whoPicked$Team%in% c("St Bonaventure", "Ucla")]<-"Bon/la"
  whoPicked$Team[whoPicked$Team%in% c("Long Island", "Radford")]<-"Liu/rad"
  whoPicked$Team[whoPicked$Team%in% c("North Carolina Central", "Texas Southern")]<-"Ncc/ts"
} else if (playInTbd==T & year==2019){
  bracket.data$Team_Full[bracket.data$Team_Full%in% c("Arizona State", "St Johns")]<-"Asu/sju"
  bracket.data$Team_Full[bracket.data$Team_Full%in% c("Belmont", "Temple")]<-"Bel/tem"
  bracket.data$Team_Full[bracket.data$Team_Full%in% c("Fairleigh Dickinson", "Prairie View A&m")]<-"Fdu/pv"
  bracket.data$Team_Full[bracket.data$Team_Full%in% c("North Dakota State", "North Carolina Central")]<-"Nds/ncc"
  whoPicked$Team[whoPicked$Team%in% c("Arizona State", "St Johns")]<-"Asu/sju"
  whoPicked$Team[whoPicked$Team%in% c("Belmont", "Temple")]<-"Bel/tem"
  whoPicked$Team[whoPicked$Team%in% c("Fairleigh Dickinson", "Prairie View A&m")]<-"Fdu/pv"
  whoPicked$Team[whoPicked$Team%in% c("North Dakota State", "North Carolina Central")]<-"Nds/ncc"
}
setdiff( whoPicked$Team, bracket.data$Team_Full)


bracket.data<-merge(bracket.data, whoPicked, by.x=c( "Team_Full","Round"), by.y=c("Team", "Round"), all.x=T)


####SIMULATE BRACKETS#####

whoPicked<-merge(whoPicked, unique(bracket.data[, c("Team_Full", "Seed")]), by.x=c("Team"), by.y=c("Team_Full"), all.x=T)
whoPicked[is.na(whoPicked$Seed),]

whoPicked<-whoPicked[!duplicated(whoPicked[, c("Team", "Round")]), ] ###play-in games get duplicated

whoPicked[whoPicked$Round=="R6", ][order(whoPicked$Ownership[whoPicked$Round=="R6"], decreasing = T), ]
setdiff( whoPicked$Team, bracket.data$Team_Full)
setdiff( bracket.data$Team_Full, whoPicked$Team)

bracket.data<-bracket.data[bracket.data$Seed%in% whoPicked$Seed, ]

setdiff( bracket.data$Team_Full, whoPicked$Team)


####CREATE BRACKETS######


r2_df<-data.frame(Slot=c("R2W1", "R2W4", "R2W3","R2W2", "R2X1", "R2X4",
                         "R2X3", "R2X2", "R2Y1", "R2Y4", "R2Y3", "R2Y2","R2Z1", "R2Z4", "R2Z3", "R2Z2" ), TeamPicked=NA)
r1_df<-data.frame(Slot=c(paste0("R1W", c(1, 8, 4,5, 3, 6, 2, 7)), paste0("R1X", c(1, 8, 4,5, 3, 6, 2, 7)), 
                         paste0("R1Y", c(1, 8, 4,5, 3, 6, 2, 7)), paste0("R1Z",c(1, 8, 4,5, 3, 6, 2, 7))), TeamPicked=NA)

table(bracket.data$Team_Full, bracket.data$Round)
# unique(bracket.data[bracket.data$Team_Full=="Vermont",1:4])

# unique(whoPicked[whoPicked$Team=="Vermont",1:4])


brackets<-list();length(brackets)<-numBrackets
for(j in 1:length(brackets)){
  
  
  #the bracket simulation procedure starts with the finals, 
  #then goes to earlier rounds, rejecting ecah 2-team sample if the winner of the next round is not in the 2 teams sampled
  #i cant figure out a better way to do this and the brackets get pretty close except for slightly under-sampling the underdog teams
  
  #sample 1 champion using pick-percentages as probabilities
  R6<-sample(whoPicked$Team[whoPicked$Round=="R6"] , 1, prob=c(whoPicked$Ownership[ whoPicked$Round=="R6"]))
  
  #sample 2-finalists using pick percentages, reject if champion not in selected pair
  R5_1<-c()
  R5_2<-c()
  while(!R6%in%c(R5_1, R5_2)) {
    R5_1<-sample(whoPicked$Team[grepl("W|X", whoPicked$Seed) & whoPicked$Round=="R5"], 1, 
                 prob=whoPicked$Ownership[grepl("W|X", whoPicked$Seed) & whoPicked$Round=="R5"])
    R5_2<-sample(whoPicked$Team[grepl("Y|Z", whoPicked$Seed) & whoPicked$Round=="R5"], 1, 
                 prob=whoPicked$Ownership[grepl("Y|Z", whoPicked$Seed) & whoPicked$Round=="R5"])
  }
  
  
  #sample final 4, make sure 2 finalists are in final 4
  R4_1<-c()
  R4_2<-c()
  R4_3<-c()
  R4_4<-c()
  while(!R5_1%in%c(R4_1, R4_2)) {
    R4_1<-sample(whoPicked$Team[grepl("W", whoPicked$Seed) & whoPicked$Round=="R4"], 1, 
                 prob=whoPicked$Ownership[grepl("W", whoPicked$Seed) & whoPicked$Round=="R4"])
    R4_2<-sample(whoPicked$Team[grepl("X", whoPicked$Seed) & whoPicked$Round=="R4"], 1, 
                 prob=whoPicked$Ownership[grepl("X", whoPicked$Seed) & whoPicked$Round=="R4"])
  }
  while(!R5_2%in%c(R4_3, R4_4)) {
    R4_3<-sample(whoPicked$Team[grepl("Y", whoPicked$Seed) & whoPicked$Round=="R4"], 1, 
                 prob=whoPicked$Ownership[grepl("Y", whoPicked$Seed) & whoPicked$Round=="R4"])
    R4_4<-sample(whoPicked$Team[grepl("Z", whoPicked$Seed) & whoPicked$Round=="R4"], 1, 
                 prob=whoPicked$Ownership[grepl("Z", whoPicked$Seed) & whoPicked$Round=="R4"])
  }
  
  #sample elite 8, make sure final 4 is in the selection
  R3_1<-c()
  R3_2<-c()
  R3_3<-c()
  R3_4<-c()
  R3_5<-c()
  R3_6<-c()
  R3_7<-c()
  R3_8<-c()
  
  while(!R4_1%in%c(R3_1, R3_2)) {
    R3_1<-sample(whoPicked$Team[whoPicked$Seed%in% unique(bracket.data$Seed[bracket.data$Slot=="R3W1"]) & whoPicked$Round=="R3"], 1, 
                 prob=whoPicked$Ownership[whoPicked$Seed%in% unique(bracket.data$Seed[bracket.data$Slot=="R3W1"]) & whoPicked$Round=="R3"])
    R3_2<-sample(whoPicked$Team[whoPicked$Seed%in% unique(bracket.data$Seed[bracket.data$Slot=="R3W2"]) & whoPicked$Round=="R3"], 1, 
                 prob=whoPicked$Ownership[whoPicked$Seed%in% unique(bracket.data$Seed[bracket.data$Slot=="R3W2"]) & whoPicked$Round=="R3"])
  }
  
  while(!R4_2%in%c(R3_3, R3_4)) {
    R3_3<-sample(whoPicked$Team[whoPicked$Seed%in% unique(bracket.data$Seed[bracket.data$Slot=="R3X1"]) & whoPicked$Round=="R3"], 1, 
                 prob=whoPicked$Ownership[whoPicked$Seed%in% unique(bracket.data$Seed[bracket.data$Slot=="R3X1"]) & whoPicked$Round=="R3"])
    R3_4<-sample(whoPicked$Team[whoPicked$Seed%in% unique(bracket.data$Seed[bracket.data$Slot=="R3X2"]) & whoPicked$Round=="R3"], 1, 
                 prob=whoPicked$Ownership[whoPicked$Seed%in% unique(bracket.data$Seed[bracket.data$Slot=="R3X2"]) & whoPicked$Round=="R3"])
  }
  while(!R4_3%in%c(R3_5, R3_6)) {
    R3_5<-sample(whoPicked$Team[whoPicked$Seed%in% unique(bracket.data$Seed[bracket.data$Slot=="R3Y1"]) & whoPicked$Round=="R3"], 1, 
                 prob=whoPicked$Ownership[whoPicked$Seed%in% unique(bracket.data$Seed[bracket.data$Slot=="R3Y1"]) & whoPicked$Round=="R3"])
    R3_6<-sample(whoPicked$Team[whoPicked$Seed%in% unique(bracket.data$Seed[bracket.data$Slot=="R3Y2"]) & whoPicked$Round=="R3"], 1, 
                 prob=whoPicked$Ownership[whoPicked$Seed%in% unique(bracket.data$Seed[bracket.data$Slot=="R3Y2"]) & whoPicked$Round=="R3"])
    
  }
  while(!R4_4%in%c(R3_7, R3_8)) {
    R3_7<-sample(whoPicked$Team[whoPicked$Seed%in% unique(bracket.data$Seed[bracket.data$Slot=="R3Z1"]) & whoPicked$Round=="R3"], 1, 
                 prob=whoPicked$Ownership[whoPicked$Seed%in% unique(bracket.data$Seed[bracket.data$Slot=="R3Z1"]) & whoPicked$Round=="R3"])
    R3_8<-sample(whoPicked$Team[whoPicked$Seed%in% unique(bracket.data$Seed[bracket.data$Slot=="R3Z2"]) & whoPicked$Round=="R3"], 1, 
                 prob=whoPicked$Ownership[whoPicked$Seed%in% unique(bracket.data$Seed[bracket.data$Slot=="R3Z2"]) & whoPicked$Round=="R3"])
  }
  
  #sample sweet 16
  r2_df$TeamPicked<-NA
  r2_df$NextRoundTeam<-rep(c(R3_1, R3_2, R3_3, R3_4, R3_5, R3_6, R3_7, R3_8), each=2)
  for(i in seq(1, nrow(r2_df), by=2)) {
    while(!r2_df$NextRoundTeam[i] %in% r2_df$TeamPicked[i:(i+1)]){
      r2_df$TeamPicked[i]<-sample(whoPicked$Team[whoPicked$Seed%in%  unique(bracket.data$Seed[bracket.data$Slot==r2_df$Slot[i]])  & whoPicked$Round=="R2"], 1, 
                                  prob=whoPicked$Ownership[whoPicked$Seed%in%unique(bracket.data$Seed[bracket.data$Slot==r2_df$Slot[i]]) & whoPicked$Round=="R2"])
      r2_df$TeamPicked[i+1]<-sample(whoPicked$Team[whoPicked$Seed%in%  unique(bracket.data$Seed[bracket.data$Slot==r2_df$Slot[i+1]])  & whoPicked$Round=="R2"], 1, 
                                    prob=whoPicked$Ownership[whoPicked$Seed%in%unique(bracket.data$Seed[bracket.data$Slot==r2_df$Slot[i+1]]) & whoPicked$Round=="R2"])
      
    }
  }
  
  #sample round of 32
  r1_df$TeamPicked<-NA
  r1_df$NextRoundTeam<-rep(r2_df$TeamPicked, each=2)
  
  for(i in seq(1, nrow(r1_df), by=2)) {
    while(!r1_df$NextRoundTeam[i] %in% r1_df$TeamPicked[i:(i+1)]){
      r1_df$TeamPicked[i]<-sample(whoPicked$Team[whoPicked$Seed%in%  unique(bracket.data$Seed[bracket.data$Slot==r1_df$Slot[i]])  & whoPicked$Round=="R1"], 1, 
                                  prob=whoPicked$Ownership[whoPicked$Seed%in%unique(bracket.data$Seed[bracket.data$Slot==r1_df$Slot[i]]) & whoPicked$Round=="R1"])
      r1_df$TeamPicked[i+1]<-sample(whoPicked$Team[whoPicked$Seed%in%  unique(bracket.data$Seed[bracket.data$Slot==r1_df$Slot[i+1]])  & whoPicked$Round=="R1"], 1, 
                                    prob=whoPicked$Ownership[whoPicked$Seed%in%unique(bracket.data$Seed[bracket.data$Slot==r1_df$Slot[i+1]]) & whoPicked$Round=="R1"])
      
      
    }
  }
  
  #save full sampled bracket
  brackets[[j]]<-c(r1_df$TeamPicked,r2_df$TeamPicked,
                   R3_1, R3_2, R3_3, R3_4, R3_5, R3_6, R3_7, R3_8,R4_1, R4_2, R4_3, R4_4, R5_1, R5_2, R6)
  if(j%%100==0){
    print(j)
  }
}

#combine and save
brackets<-data.frame(t(sapply(brackets, `[`)))
r2_df<-data.frame(Slot=c("R2W1", "R2W4", "R2W3","R2W2", "R2X1", "R2X4",
                         "R2X3", "R2X2", "R2Y1", "R2Y4", "R2Y3", "R2Y2","R2Z1", "R2Z4", "R2Z3", "R2Z2" ), TeamPicked=NA)
r1_df<-data.frame(Slot=c(paste0("R1W", c(1, 8, 4,5, 3, 6, 2, 7)), paste0("R1X", c(1, 8, 4,5, 3, 6, 2, 7)), 
                         paste0("R1Y", c(1, 8, 4,5, 3, 6, 2, 7)), paste0("R1Z",c(1, 8, 4,5, 3, 6, 2, 7))), TeamPicked=NA)
colnames(brackets)<-  c(r1_df$Slot, r2_df$Slot,
                        "R3W1", "R3W2", "R3X1", "R3X2", "R3Y1", "R3Y2"  , "R3Z1", "R3Z2", "R4W1", "R4X1", "R4Y1", "R4Z1", "R5WX", "R5YZ", "R6CH")


save(brackets, file=name)
