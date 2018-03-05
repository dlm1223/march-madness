#year<-2017;setwd(projDir);load("data/game data.RData");setwd(as.character(year));load("TourneySims_500sims.Rda");numBrackets<-500;name<-"BracketResults_FullTournament_500sims.Rda"


#optional: change bracketresults name as _v2 or _v3 if using different tourneysims version

###organize ownership data, check names********************
whoPicked<-whoPicked[whoPicked$Season==year, !colnames(whoPicked)%in% "Season"]

setdiff(fulldf$Team_Full[fulldf$Season==year & fulldf$Tournament==1], whoPicked$Team)
setdiff( whoPicked$Team, fulldf$Team_Full[fulldf$Season==year & fulldf$Tournament==1])

#play-in games--some of the ownership data years don't have playin updated so have to impute winner if playInTBD=F
if(playInTbd==T){
  whoPicked$Team[whoPicked$Team=="Pr / Sc"]<-"Providence / Usc"
  whoPicked$Team[whoPicked$Team=="North Carolina / Ud"]<-"North Carolina Central / Uc Davis"
} else{
  whoPicked$Team[whoPicked$Team=="Pr / Sc"]<-"Usc"
  whoPicked$Team[whoPicked$Team=="North Carolina / Ud"]<-"Uc Davis"
}


#prepare dataframe to be used for ownership-create-breackets
analyze<-TourneyRounds[grepl("R", TourneyRounds$Slot) & TourneyRounds$Season==year,]
analyze$Team<-TourneySeeds$Team[TourneySeeds$Season==year][match(analyze$Seed,TourneySeeds$Seed[TourneySeeds$Season==year] )]
analyze$Team_Full<-id_df$Team_Full[match(analyze$Team, id_df$TeamID)]
analyze$Team_Full<-coordName(analyze$Team_Full)
analyze$Round<-substring(analyze$Slot, 1 ,2)

#handle play-in games, if playINTBD=T then combine play-in games into one
analyze<-analyze[analyze$Team%in% c(tourneySims$Team[grepl("R1", tourneySims$Slot) ],
                                    tourneySims$Loser[grepl("R1", tourneySims$Slot) ]), ]
if(playInTbd==T & year==2017){
  analyze$Team_Full[analyze$Team_Full%in% c("Providence", "Usc")]<-"Providence / Usc"
  analyze$Team_Full[analyze$Team_Full%in% c("North Carolina Central", "Uc Davis")]<-"North Carolina Central / Uc Davis"
}

setdiff( analyze$Team_Full, whoPicked$Team)
setdiff( whoPicked$Team, analyze$Team_Full)
analyze<-merge(analyze, whoPicked, by.x=c( "Team_Full","Round"), by.y=c("Team", "Round"), all.x=T)


####SIMULATE BRACKETS#####

whoPicked<-merge(whoPicked, unique(analyze[, c("Team_Full", "Seed")]), by.x=c("Team"), by.y=c("Team_Full"), all.x=T)
whoPicked[is.na(whoPicked$Seed),]

whoPicked<-whoPicked[!duplicated(whoPicked[, c("Team", "Round")]), ] ###play-in games get duplicated
whoPicked[whoPicked$Round=="R6", ][order(whoPicked$Ownership[whoPicked$Round=="R6"], decreasing = T), ]
setdiff( whoPicked$Team, analyze$Team_Full)


#go to ownership debug
source(paste0(projDir, "/ownership create brackets.R"))


#can adjust calcPayout function in simulate calc payouts if desired
# input<-list(r1=5, r2=10, r3=15, r4=25, r5=30, r6=40, upset1_mult=2,
#             upset2_mult=3, upset3_mult=4, upset1_add=0, upset2_add=0, upset3_add=0)

###CHANGE SCORING#####

input<-list(r1=10, r2=20, r3=40, r4=80, r5=160, r6=320, upset1_mult=1,
            upset2_mult=1, upset3_mult=1, upset1_add=0, upset2_add=0, upset3_add=0)


source(paste0(projDir, "/simulate calc payouts.R"))

inspect[order(inspect$R6,inspect$R5,inspect$R4,inspect$R3, inspect$R2,  decreasing = T), ]

###SAVE DATA#####

save(brackets, file=name)

save(list=ls()[ls()%in% c( "backtest", "id_df",   "year", "TourneySeeds", "analyze" )], file="alldata.RData")

