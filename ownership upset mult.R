year<-2018;setwd(projDir);load("data/game data.RData");setwd(as.character(year));load("TourneySims_500sims.Rda");backtest<-T

numBrackets<-500;
name<-"BracketResults_FullTournament_upsetmult.Rda"


#optional: change bracketresults name as _v2 or _v3 if using different tourneysims version

###organize ownership data, check names********************
whoPicked<-whoPicked[whoPicked$Season==year, !colnames(whoPicked)%in% "Season"]



#play-in games--some of the ownership data years don't have playin updated so have to impute winner if playInTBD=F
if(year==2017){
  if(playInTbd==T){
    whoPicked$Team[whoPicked$Team=="Pr / Sc"]<-"Providence / Usc"
    whoPicked$Team[whoPicked$Team=="North Carolina / Ud"]<-"North Carolina Central / Uc Davis"
  } else{
    whoPicked$Team[whoPicked$Team=="Pr / Sc"]<-"Usc"
    whoPicked$Team[whoPicked$Team=="North Carolina / Ud"]<-"Uc Davis"
  }
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
} else if(playInTbd==T & year==2018){
  analyze$Team_Full[analyze$Team_Full%in% c("Arizona State", "Syracuse")]<-"Asu/sy"
  analyze$Team_Full[analyze$Team_Full%in% c("St Bonaventure", "Ucla")]<-"Bon/la"
  analyze$Team_Full[analyze$Team_Full%in% c("Long Island", "Radford")]<-"Liu/rad"
  analyze$Team_Full[analyze$Team_Full%in% c("North Carolina Central", "Texas Southern")]<-"Ncc/ts"
  whoPicked$Team[whoPicked$Team%in% c("Arizona State", "Syracuse")]<-"Asu/sy"
  whoPicked$Team[whoPicked$Team%in% c("St Bonaventure", "Ucla")]<-"Bon/la"
  whoPicked$Team[whoPicked$Team%in% c("Long Island", "Radford")]<-"Liu/rad"
  whoPicked$Team[whoPicked$Team%in% c("North Carolina Central", "Texas Southern")]<-"Ncc/ts"
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
setdiff( analyze$Team_Full, whoPicked$Team)

analyze<-analyze[analyze$Seed%in% whoPicked$Seed, ]

#go to ownership debug
source(paste0(projDir, "/ownership create brackets.R"))


#go to ownership debug
# load("BracketResults_FullTournament.Rda")

#can adjust calcPayout function in simulate calc payouts if desired
# input<-list(r1=5, r2=10, r3=15, r4=25, r5=30, r6=40, upset1_mult=2,
#             upset2_mult=3, upset3_mult=4, upset1_add=0, upset2_add=0, upset3_add=0)

###CHANGE SCORING#####
# 
input<-list(r1=5, r2=10, r3=15, r4=25, r5=30, r6=40, upset1_mult=2, upset2_mult=3, upset3_mult=4,
            r1_seed_mult=0, r2_seed_mult=0,r3_seed_mult=0, r4_seed_mult=0,r5_seed_mult=0, r6_seed_mult=0,
            r1_seed_bonus=0, r2_seed_bonus=0,r3_seed_bonus=0, r4_seed_bonus=0,r5_seed_bonus=0, r6_seed_bonus=0)
# input<-list(r1=10, r2=20, r3=40, r4=80, r5=160, r6=320, upset1_mult=1, upset2_mult=1, upset3_mult=1,
#             r1_seed_mult=0, r2_seed_mult=0,r3_seed_mult=0, r4_seed_mult=0,r5_seed_mult=0, r6_seed_mult=0,
#             r1_seed_bonus=0, r2_seed_bonus=0,r3_seed_bonus=0, r4_seed_bonus=0,r5_seed_bonus=0, r6_seed_bonus=0)


source('~/Kaggle/NCAA/march-madness/Shiny/simulate calc payouts_merge.R')

# inspect[order(inspect$R6,inspect$R5,inspect$R4,inspect$R3, inspect$R2,  decreasing = T), ][1:20,]


###SAVE DATA#####

save(brackets, file=name)

