library(gridExtra);library(grid)
library(forecast);library(RSelenium);library(RCurl);library(slam);library(XML)
library(caret);library(doSNOW);library(rvest);library(robustbase);library(rpart);library(kernlab)
library(lpSolve);library(lpSolveAPI);library(data.table);library(xgboost);library(gurobi);library(Rcplex)
library(reshape2);library(plyr);library(zoo);library(dplyr);library(gtools);library(RJSONIO)
library(ggplot2);library(glmnet);library(MASS);library(TTR) 
library(Metrics);library(Rsymphony)
options(stringsAsFactors=FALSE)
options(scipen=999)

# setwd("")
rm(list = ls())
source("functions.R")


###ORGANIZE TEAM RANKINGS#####

TourneySlots<-read.csv("data/TourneySlots.csv")
tail(TourneySlots, 35)

TourneySeeds<-read.csv("data/TourneySeeds.csv")

#all possible slots that each team can be in
getSlots<-function(seed, season){
  # slot<-"W11"
  slot<-TourneySlots$Slot[TourneySlots$Season==season & (TourneySlots$Strongseed==seed|TourneySlots$Weakseed==seed)]
  next_slot<-slot
  while(slot!="R6CH"){
    next_slot<-c(next_slot, 
                 TourneySlots$Slot[TourneySlots$Season==season & (TourneySlots$Strongseed==slot|TourneySlots$Weakseed==slot)]
    )
    slot<- TourneySlots$Slot[TourneySlots$Season==season & (TourneySlots$Strongseed==slot|TourneySlots$Weakseed==slot)]
  }
  data.frame(Seed=seed, Slot=next_slot, Season=season)
}
TourneyRounds<-lapply(1:nrow(TourneySeeds),function(x) getSlots(TourneySeeds$Seed[x], TourneySeeds$Season[x]))
TourneyRounds<-ldply(TourneyRounds,data.frame)

TourneyGeog<-read.csv("data/TourneyGeog.csv")
TourneyGeog$lat[TourneyGeog$host=="new_york"]<-40.6827
TourneyGeog$lng[TourneyGeog$host=="new_york"]<-(-73.9753)
colnames(TourneyGeog)<-sapply(colnames(TourneyGeog), simpleCap)

TeamGeog<-read.csv("data/TeamGeog.csv")
colnames(TeamGeog)[2:3]<-c("TeamLat", "TeamLng")


KenPom<-read.csv("data/PomeryRatings.csv")
KenPom$Team<-gsub("[.]", "", KenPom$Team)
KenPom$alternative_spelling<-gsub("[.]", "", KenPom$alternative_spelling)
KenPom$Team<-coordName(KenPom$Team)


getRound<-function(year, round){
  #year<-2012
  if(year==2012){
    data<-read_html(paste0("https://web.archive.org/web/20120316095713/http://tournament.fantasysports.yahoo.com:80/t1/group/all/pickdistribution?round=", round))
  } 
  # else if(year==2011){
  #   data<-read_html(paste0("https://web.archive.org/web/201105011334555/https://tournament.fantasysports.yahoo.com/t1/group/all/pickdistribution?round=", round))
  # }
  # else if(year==2009){
  #   data<-read_html(paste0("https://web.archive.org/web/20090322101538/https://tournament.fantasysports.yahoo.com/t1/group/all/pickdistribution?round=", round))
  # }else if(year==2008){
  #   data<-read_html(paste0("https://web.archive.org/web/20080321234943/https://tournament.fantasysports.yahoo.com/t1/group/all/pickdistribution?round=", round))
  # }
  names<-data%>% html_nodes("td")%>% html_text()
  if(names[1]!="1"){
    names<-names[-c(1:2)]
  }
  names<-data.frame(matrix(names, ncol=3, byrow=T))
  names<-names[, -1]
  colnames(names)<-c("Team", "Ownership")
  names$Team<-coordName(sapply(strsplit(names$Team, " \\("), `[[`, 1))
  names$Round<-paste0("R", round)
  names$Season<-year
  names$Ownership<-as.numeric(names$Ownership)/100
  names
}
getYear<-function(year){
  print(year)
  data.frame(rbindlist(lapply(1:6, function(x) getRound(year, x))))
}
whoPicked_yahoo<-getYear(2012)
whoPicked_yahoo$Team[whoPicked_yahoo$Team%in% c("Lamar/vermont")]<-"Vermont"
whoPicked_yahoo$Team[whoPicked_yahoo$Team%in% c("Mvsu/wku")]<-"Western Kentucky"
whoPicked_yahoo$Team[whoPicked_yahoo$Team%in% c("Byu/iona")]<-"Brigham Young"
whoPicked_yahoo$Team[whoPicked_yahoo$Team%in% c("Cal/usf")]<-"Usf"
whoPicked_yahoo<-ddply(whoPicked_yahoo, .(Team, Round, Season),summarize, Ownership=sum(Ownership))
table(whoPicked_yahoo$Round)
setdiff(whoPicked_yahoo$Team, id_df$Team_Full)

readFile<-function(year){
  string<-paste0(year, "/WhoPickedWhom.csv")
  
  whoPicked<-read.csv(string)
  colnames(whoPicked)<-c("R1", "R2", "R3", "R4", "R5", "R6")
  whoPicked[, 7:12]<-sapply(whoPicked, function(x) sapply( strsplit(x, "-"), function(x) x[length(x)]))
  whoPicked[, 1:6]<-sapply(whoPicked[, 1:6], function(x) sapply( strsplit(x, "-"), function(x) paste0(x[-length(x)], collapse="-", sep="")))
  whoPicked[, 1:6]<-sapply(whoPicked[, 1:6], function(x) gsub(paste0(1:16,collapse="|" ), "", x))
  whoPicked[,7:12]<-sapply(whoPicked[, 7:12], function(x) as.numeric(gsub("%", "", x))*.01)
  
  #reshape whoPicked data
  whoPicked2<-data.frame(Team=rep(NA, 64*6), Round=rep(NA, 64*6), Ownership=rep(NA, 64*6))
  row<-1;
  for(i in 1:6){
    for(team in whoPicked[, i]) {
      whoPicked2$Team[row]<-team
      whoPicked2$Round[row]<-colnames(whoPicked)[i]
      whoPicked2$Ownership[row]<-whoPicked[whoPicked[, i]==team,i+6]
      row<-row+1
    }
  }
  whoPicked2$Team<-coordName((whoPicked2$Team))
  whoPicked2$Season<-year
  whoPicked2
}
whoPicked<-ldply(lapply(c(2008:2011, 2013:2017), readFile), data.frame)
setdiff(whoPicked$Team, id_df$Team_Full)

whoPicked<-rbind(whoPicked, whoPicked_yahoo)

###****YOU NEED TO DOWNLOAD "cb2001-18" DATA FROM THE FOLLOWING LINK AND PLACE all files IN A FOLDER Massey/cb/   ******
#https://www.kaggle.com/masseyratings/rankings/data

# setwd("")
readFile<-function(file){
  Massey_All<-read.csv(paste0("Massey/cb/", file), header=F, strip.white = T)
  Massey_All
}
files<-list.files("Massey/cb/")
Massey_All<-ldply(lapply(files[-c(1:6)], readFile), data.frame)
colnames(Massey_All)<-c("File", "team_id2", "Team_Full", "Source", "Source_Full", "DATE", "Rank")
Massey_All$DATE<-as.Date(as.character(Massey_All$DATE), format="%Y%m%d")

Massey_All<-data.table(Massey_All)
Massey_means<-Massey_All[, list(  medRank=as.numeric(median(Rank)),
                                  maxRank=max(Rank), minRank=min(Rank), meanRank=mean(Rank)), by=c("team_id2", "Team_Full", "DATE")]
Massey_All<-data.frame(Massey_All)
Massey_means<-data.frame(Massey_means)
Massey_means$Team_Full<-coordName(Massey_means$Team_Full)

id_df<-unique(KenPom[, c("team_id", "Team")])
id2<-unique(Massey_means[, c("team_id2", "Team_Full")])
colnames(id2)<-c("team_id2", "Team_Full")
colnames(id_df)<-c("team_id", "Team_Full")
id_df<-merge(id_df, id2, by=c("Team_Full"), all.x=T)
head(id_df)

Massey_means$team_id<-id_df$team_id[match(Massey_means$Team_Full, id_df$Team_Full)]
Massey_All$team_id<-id_df$team_id[match(Massey_All$team_id2, id_df$team_id2)]

Massey_All<-Massey_All[!months(Massey_All$DATE)=="April",]
Massey_means<-Massey_means[!months(Massey_means$DATE)=="April",]
###ORGANIZE GAME DATA#####


dates<-seq(as.Date("2017-03-14"), as.Date("2017-04-03"), 1)
dates<-gsub( "-", "/",as.character(dates))
dates<-dates[!dates=="2017/04/02"]
importData<-function(date){
  print(date)
  site<-paste0("http://www.ncaa.com/scoreboard/basketball-men/d1/", date)
  site<-read_html(site)
  scores<-site  %>% 
    html_nodes(".score , .team a")%>% 
    html_text()
  scores<-data.frame(matrix(scores, ncol=2, byrow=T))
  colnames(scores)<-c("TeamFull", "Pts")
  scores$GameIndex<-rep(seq(1, nrow(scores)/2), each=2)
  scores$Away<-rep(c(1, 0), times=nrow(scores)/2)
  away<-scores[scores$Away==1,]
  colnames(away)[1:2]<-c("OPP_Full", "OPPscore")
  colnames(scores)[1:2]<-c("Team_Full", "Teamscore")
  scores<-merge(scores[scores$Away==0, ], away, by=c("GameIndex"))
  scores<-scores[, !grepl("Away|Index", colnames(scores))]
  scores$DATE<-date
  scores
}
tourney17<-ldply(lapply(dates, importData), data.frame)
tourney17<-tourney17[!is.na(tourney17$Teamscore), ]
tourney17[, c("Teamscore", "OPPscore")]<-sapply(tourney17[,  c("Teamscore", "OPPscore")], as.numeric)
tourney17[, c("Team_Full", "OPP_Full")]<-sapply(tourney17[, c("Team_Full", "OPP_Full")], coordName)
tourney17$Wteam<-ifelse(tourney17$Teamscore>tourney17$OPPscore, tourney17$Team_Full, tourney17$OPP_Full)
tourney17$Lteam<-ifelse(tourney17$Teamscore>tourney17$OPPscore, tourney17$OPP_Full, tourney17$Team_Full)
tourney17$Wteam<-id_df$team_id[match(tourney17$Wteam, id_df$Team_Full)]
tourney17$Lteam<-id_df$team_id[match(tourney17$Lteam, id_df$Team_Full)]
tourney17$DATE<-as.Date(tourney17$DATE, format="%Y/%m/%d")
tourney17$Wloc<-"N"
tourney17$Season<-2017


tourney<-read.csv("data/TourneyDetailedResults.csv")
tourney<-rbind.fill(tourney, tourney17[, c("Wteam", "Lteam", "DATE", "Wloc", "Season")])
tourney$Tournament<-1
tail(tourney)


regularSeason<-read.csv("data/RegularSeasonDetailedResults.csv")
regularSeason$Tournament<-0
fulldf<-rbind.fill(tourney, regularSeason)

seasons<-read.csv("data/Seasons.csv")
seasons$Dayzero<-as.Date(seasons$Dayzero, format="%m/%d/%Y")
fulldf$dayzero<-seasons$Dayzero[match(fulldf$Season, seasons$Season)]
fulldf$DATE[is.na(fulldf$DATE)]<-fulldf$dayzero[is.na(fulldf$DATE)]+fulldf$Daynum[is.na(fulldf$DATE)]


fulldf<-fulldf[fulldf$Season>=2008, ]
fulldf$Rank_DATE<-sapply(fulldf$DATE, function(x) max(Massey_means$DATE[Massey_means$DATE<=x]))
fulldf$Rank_DATE<-as.Date(fulldf$Rank_DATE)
winners<-fulldf
colnames(winners)<-gsub("W", "Team", colnames(winners))
colnames(winners)<-gsub("L", "OPP", colnames(winners))
colnames(winners)[colnames(winners)%in% c("Teamteam", "OPPteam")]<-c("Team", "OPP")

losers<-fulldf
colnames(losers)<-gsub("W", "OPP", colnames(losers))
colnames(losers)<-gsub("L", "Team", colnames(losers))
losers$OPPloc<-ifelse(losers$OPPloc=="A", "H", ifelse(losers$OPPloc=="H", "A", "N"))
colnames(losers)[colnames(losers)=="OPPloc"]<-"Teamloc"
colnames(losers)[colnames(losers)%in% c("OPPteam", "Teamteam")]<-c("OPP", "Team")

winners$Win<-1
losers$Win<-0
fulldf<-rbind(winners, losers)
fulldf<-merge(fulldf, Massey_means, by.x=c("Team", "Rank_DATE"), by.y = c("team_id", "DATE"), all.x=T)

OPP_means<-Massey_means
colnames(OPP_means)<-gsub("Team","OPP", colnames(OPP_means))
colnames(OPP_means)<-gsub("team","opp", colnames(OPP_means))
colnames(OPP_means)[grepl("Rank", colnames(OPP_means))]<-paste("OPP", colnames(OPP_means)[grepl("Rank", colnames(OPP_means))], sep="")
head(OPP_means)
fulldf<-merge(fulldf, OPP_means, by.x=c("OPP", "Rank_DATE"), by.y = c("opp_id", "DATE"), all.x=T)

sources<- c("MAS", "SAG", "POM", "MOR", "CMP", "PGH", "RTP",
            "WIL", "WLK", "TPR", "NOL", "PIG", "DOL", "BIH", "70T",
            "DII", "DOK", "RT", "TRP", "BIH", "KPK")
test<-Massey_All[ Massey_All$Source%in%sources,c("team_id", "Source", "DATE", "Rank") ]

test<-reshape(test, timevar = c("Source"), idvar = c("DATE", "team_id"), direction = "wide")
fulldf<-merge(fulldf[, !grepl("Rank[.]", colnames(fulldf))], 
              test, by.x=c("Team", "Rank_DATE"), by.y=c("team_id", "DATE"), all.x=T)
colnames(test)<-gsub("Rank", "OPP", colnames(test))
fulldf<-merge(fulldf[, !grepl("OPP[.]", colnames(fulldf))], 
              test, by.x=c("OPP", "Rank_DATE"), by.y=c("team_id", "DATE"), all.x=T)

##s-curve data####
getYear<-function(year){
  link<-read_html(paste(c("https://en.wikipedia.org/wiki/", year, "_NCAA_Division_I_Men%27s_Basketball_Tournament"), sep="", collapse=""))
  
  if(year<=2014){
    SCurve<-link%>% html_nodes("td .wikitable td") %>% html_text()
  }else{
    SCurve<-link%>% html_nodes("td, th") %>% html_text()
    SCurve<-SCurve[(which(SCurve=="Seed")[1]+6):(grep("East Region|West Region|South Region|Midwest Region", SCurve)[1]-2)]
    SCurve<-SCurve[!SCurve%in% c("Seed", "School", "Conference", "Record", "Berth type", "Overall rank")]
  }
  
  ind<-grep("[*]", SCurve)
  if(year%in%c(2012, 2015:2017)){
    SCurve<-SCurve[c(1:(ind[1]+5), ind[1], 
                     (ind[1]+6):(ind[2]+5), ind[2],
                     (ind[2]+6):(ind[3]+5), ind[3],
                     (ind[3]+6):(ind[4]+5), ind[4],
                     (ind[4]+6):length(SCurve))]  #play-in games have missing column
    SCurve<-data.frame(matrix(SCurve, ncol=6, byrow=T)) 
    colnames(SCurve)<-c("Seed", "Team", "Conference", "Record", "BidType", "SCurve")
  } else if(year%in%2013:2014){
    SCurve<-SCurve[c(1:(ind[1]+6), ind[1], 
                     (ind[1]+7):(ind[2]+6), ind[2],
                     (ind[2]+7):(ind[3]+6), ind[3],
                     (ind[3]+7):(ind[4]+6), ind[4],
                     (ind[4]+7):length(SCurve))]  #play-in games have missing column
    SCurve<-data.frame(matrix(SCurve, ncol=7, byrow=T)) 
    colnames(SCurve)<-c("Seed", "Team", "Conference", "Record", "Coach", "BidType", "SCurve")
    SCurve$Coach<-NULL
    
  }
  SCurve$Season<-year
  SCurve
}
SCurve<-ldply(lapply(2012:2017, getYear), data.frame)
SCurve$Team<-coordName(SCurve$Team)
SCurve$Team[SCurve$Team=="Louisville[a]"]<-"Louisville"

setdiff(SCurve$Team, id_df$Team_Full)


##scrape odds data####

if(!exists("oddsDF")){
  dates<-unique(fulldf$DATE)
} else{
  dates<-as.Date(setdiff(unique(fulldf$DATE), oddsDF$DATE))
  
}

importOdds<-function(date) {
  #   date<-dates[77]#Sys.Date()
  url<-read_html(paste0(c("http://www.sportsbookreview.com/betting-odds/ncaa-basketball/?date=",gsub("-", "", date )), collapse=""))
  odds<-url %>%
    html_nodes("b , .eventLine-value") %>%
    html_text()
  if(length(odds)>=1){
    odds<-odds[!grepl("Pitching|Batting", odds)]
    # odds<-iconv(odds, to='ASCII//TRANSLIT')
    odds<-gsub("=|?", ".5", odds)
    odds<-gsub("PK", "0 ", odds)
    odds<-data.frame(t(matrix(odds, nrow=22)))
    odds$OPP1<-odds[, 2]
    odds$OPP2<-odds[, 1]
    odds<-as.data.frame(mapply(c, odds[, c(TRUE, FALSE)], odds[, c(FALSE, TRUE)]))
    odds[odds==""]<-NA
    if(nrow(odds)>1){
      odds$DATE<-as.Date(date)
    }
  } else{
    odds<-data.frame()
  }
  print(date)
  odds
}
oddsList<-list();length(oddsList)<-length(dates)
for(i  in 1:length(dates)){
  oddsList[[i]]<-importOdds(dates[i])
}
odds<-ldply(oddsList, data.frame)
odds$Team<-coordName(tolower(odds[, 1]))
odds$OPP<-coordName(tolower(odds$OPP1))
odds[, 2:11]<-sapply(odds[, 2:11], function(x) sapply(strsplit(x, "\\s+"), `[[`, 1))
odds[, 2:11]<-sapply(odds[, 2:11], as.numeric)
odds$Spread<-apply(odds[, 2:11], 1, median, na.rm = TRUE)
odds<-odds[, c("Team","OPP", "Spread", "DATE")]
odds$Team<-coordName(sapply(strsplit(odds$Team, "\\)"), function(x) sub("^\\s+", "", tail(x, 1))))
odds$OPP<-coordName(sapply(strsplit(odds$OPP, "\\)"), function(x) sub("^\\s+", "", tail(x, 1))))
odds[, c("Team", "OPP")]<-sapply(odds[, c("Team", "OPP")],function(x) {gsub(paste0(0:9, collapse="|"), "", x)})
odds[, c("Team", "OPP")]<-sapply(odds[, c("Team", "OPP")],function(x) coordName(trimws(x)))

if(!exists("oddsDF")){
  oddsDF<-odds
} else{
  oddsDF<-rbind(odds, oddsDF)
}
oddsDF<-oddsDF[!is.na(oddsDF$Spread), ]
oddsDF<-oddsDF[!duplicated(oddsDF[, c("Team", "DATE")]), ]



oddsDF[oddsDF$DATE==as.Date("2017-03-17"), ]
setdiff( fulldf$Team_Full, oddsDF$Team)

fulldf<-merge(fulldf, oddsDF[, c("Team", "Spread", "DATE")], by.x=c("Team_Full","DATE"), by.y=c("Team", "DATE"),all.x=T)


cor(fulldf$Win, fulldf[, grepl("Rank[.]|med|min|max|mean", colnames(fulldf))], use="pairwise.complete.obs")
fulldf$Teamtr<-fulldf$Teamor+fulldf$Teamdr
fulldf$OPPtr<-fulldf$OPPor+fulldf$OPPdr

fulldf<-data.table(fulldf)
fulldf<-fulldf[order(fulldf$DATE, decreasing = T), ]
fulldf<-fulldf[,`:=`(
  DaysOff=as.integer(DATE-lead(DATE, 1)),
  TeamscoreMARGIN=moving(Teamscore-OPPscore,41,"median" ),
  Teamfga3MARGIN=moving(Teamfga3-OPPfga3,41,"median" ),
  TeamftaMARGIN=moving(Teamfta-OPPfta,41,"median" ),
  TeamtoMARGIN=moving(Teamto-OPPto,41,"median" ),
  TeamtrMARGIN=moving(Teamtr-OPPtr,41,"median" ),
  TeamblkMARGIN=moving(Teamblk-OPPblk,41,"median" ),
  TeamstlMARGIN=moving(Teamstl-OPPstl,41,"median" )
), by=c("Team","Season" ) ]
fulldf<-data.frame(fulldf)
head(fulldf)

head(fulldf)
fulldf$Win_factor<-as.factor(fulldf$Win)
levels(fulldf$Win_factor)<-c("Loss", "Win")

###ADD TOURNAMENT DATA TO FULLDF#####

files<-list.files("538/", pattern ="csv")
import538<-function(file) {
  #file<-files[2]
  comp<-read.csv(paste0("538/", file), header=F)
  comp<-unlist(sapply(1:nrow(comp),function(x) comp[x,]))
  comp<-comp[comp!="" & !is.na(comp) & !comp%in% c(as.character(1:16), paste(as.character(1:16), c("b"), sep=""),
                                                   paste(as.character(1:16), c("a"), sep="")) & !comp%in% c("v")]
  comp<-gsub("%|<", "",  comp)
  comp<-data.frame(t(matrix(comp, nrow=8)))
  comp$Season<-as.numeric(gsub(".csv", "", file))
  comp  
}
march538_1<-ldply(lapply(files[1:2], import538), data.frame)
march538_1[, 3:8]<-sapply(march538_1[, 3:8],  function(x) as.numeric(x)*.001)

colnames(march538_1)[3:8]<-c("rd2_win", "rd3_win", "rd4_win", "rd5_win", "rd6_win", "rd7_win")
colnames(march538_1)[1:2]<-c("Region", "team_name")

march538_2<-read.csv("538/2017.csv")
march538_2$Season<-2017
march538_2<-march538_2[march538_2$gender=="mens",]
march538_2<-march538_2[!duplicated(march538_2[, c("team_name")], fromLast=T), ]

march538_3<-read.csv("538/2016.csv")
march538_3$Season<-2016
march538_3<-march538_3[march538_3$gender=="mens" &as.Date(as.character(march538_3$forecast_date), format="%m/%d/%Y")<=as.Date("2016-03-19") ,]
march538_3<-march538_3[!duplicated(march538_3[, c("team_name")], fromLast=T), ]

march538<-data.frame(rbindlist(list(march538_1, march538_2, march538_3), fill=T))
march538<-march538[, c("team_name", "Season", "rd2_win", "rd3_win", "rd4_win", "rd5_win", "rd6_win", "rd7_win")]
march538$Team<-coordName((march538$team_name))

# load("game data.RData")

fulldf<-merge(fulldf, march538[, !colnames(march538)=="team_name"], by.x=c("Team_Full", "Season"), by.y=c("Team", "Season"), all.x=T)
march538_2<-march538
colnames(march538_2)<-gsub("rd", "OPPrd", colnames(march538_2))
fulldf<-merge(fulldf, march538_2[, !colnames(march538_2)=="team_name"], by.x=c("OPP_Full", "Season"), by.y=c("Team", "Season"), all.x=T)

fulldf<-merge(fulldf, TourneySeeds, by.x=c("Season", "Team"), by.y=c("Season", "Team"), all.x=T)
colnames(fulldf)[colnames(fulldf)=="Seed"]<-"TeamSeed"
fulldf<-merge(fulldf, TourneySeeds,by.x=c("OPP", "Season") ,by.y=c("Team", "Season"), all.x=T)
colnames(fulldf)[colnames(fulldf)=="Seed"]<-"OPPSeed"
fulldf$TeamSeed_num<-as.numeric(substring(fulldf$TeamSeed, 2, 3))
fulldf$OPPSeed_num<-as.numeric(substring(fulldf$OPPSeed, 2, 3))
fulldf<-fulldf[!(fulldf$Tournament==1 & is.na(fulldf$TeamSeed)), ] #NIT/CBI tournament


getRound<-function(seed1, seed2, season){
  #seed1<-"W16a";seed2<-"W16b";season<-2017
  numCommon<-intersect(TourneyRounds$Slot[TourneyRounds$Seed==seed1 & TourneyRounds$Season==season], 
                       TourneyRounds$Slot[TourneyRounds$Seed==seed2 & TourneyRounds$Season==season]
  )%>%length()
  7-numCommon
}
getSlot<-function(seed1, seed2, season){
  #seed1<-"W16a";seed2<-"W16b";season<-2017
  numCommon<-intersect(TourneyRounds$Slot[TourneyRounds$Seed==seed1 & TourneyRounds$Season==season], 
                       TourneyRounds$Slot[TourneyRounds$Seed==seed2 & TourneyRounds$Season==season]
  )[1]
}
fulldf$Slot<-NA
fulldf$Slot[fulldf$Tournament==1]<-sapply(which(fulldf$Tournament==1),    function(x)getSlot(fulldf$TeamSeed[x], fulldf$OPPSeed[x], fulldf$Season[x]) )
fulldf$Round[fulldf$Tournament==1]<-sapply(which(fulldf$Tournament==1),    function(x)getRound(fulldf$TeamSeed[x], fulldf$OPPSeed[x], fulldf$Season[x]) )
fulldf$Round<-as.numeric(fulldf$Round)

fulldf<-merge(fulldf, TourneyGeog[, c("Season", "Slot", "Host", "Lat", "Lng")], by=c("Season","Slot"), all.x=T)
fulldf<-merge(fulldf, TeamGeog[, c("team_id", "TeamLat", "TeamLng")], by.x=c("Team"), by.y=("team_id"), all.x=T)
fulldf$Dist<-NA
library(geosphere)
fulldf$Dist[!is.na(fulldf$Lat)]<-distHaversine(fulldf[!is.na(fulldf$Lat), c( "Lng", "Lat")], fulldf[!is.na(fulldf$Lat), c( "TeamLng", "TeamLat")])/1000

#opponent distance
oppDist<-unique(fulldf[fulldf$Tournament==1, c("Team_Full","DATE", "Dist" )])
colnames(oppDist)[colnames(oppDist)=="Dist"]<-"OPPDist"
fulldf<-merge(fulldf, oppDist, by.x=c("OPP_Full", "DATE"), by.y=c("Team_Full", "DATE"), all.x=T)

fulldf<-data.table(fulldf)
fulldf<-fulldf[order(fulldf$DATE, decreasing = T), ]
fulldf<-fulldf[,`:=`(
  R1_Spread=as.numeric(ifelse(1%in% Tournament, Spread[which(Tournament==1 & Round==1)], NA))
), by=c("Team","Season" ) ]
fulldf<-data.frame(fulldf)

fulldf<-fulldf[, !grepl("Ownership_", colnames(fulldf))]
for(i in c("R1", "R2", "R3", "R4", "R5", "R6")){
  fulldf<-merge(fulldf, whoPicked[whoPicked$Round==i,c("Team", "Season", "Ownership")  ], by.x=c("Team_Full", "Season"), by.y=c("Team", "Season"), all.x=T)
  colnames(fulldf)[colnames(fulldf)=="Ownership"]<-paste0("TeamOwnership_", i)
  
  fulldf<-merge(fulldf, whoPicked[whoPicked$Round==i, c("Team", "Season", "Ownership") ], by.x=c("OPP_Full", "Season"), by.y=c("Team", "Season"), all.x=T)
  colnames(fulldf)[colnames(fulldf)=="Ownership"]<-paste0("OPPOwnership_", i)
}

SCurve$SCurve<-as.numeric(SCurve$SCurve)
fulldf<-fulldf[, !grepl("SCurve", colnames(fulldf))]
fulldf<-merge(fulldf, SCurve[, c("Team", "Season", "SCurve")], by.x=c("Team_Full", "Season"), by.y=c("Team", "Season"),all.x=T)
OPPCurve<-SCurve
colnames(OPPCurve)[colnames(OPPCurve)=="SCurve"]<-"OPPSCurve"
fulldf<-merge(fulldf, OPPCurve[, c("Team", "Season", "OPPSCurve")], by.x=c("OPP_Full", "Season"), by.y=c("Team", "Season"),all.x=T)



fulldf[fulldf$Team_Full=='North Carolina'& fulldf$Season==2015& fulldf$Tournament==1, ]


save(list=ls()[ls()%in% c("fulldf", "KenPom", "Massey_All", "Massey_means","oddsDF", "id_df", "march538", #projections data
                          "seasons", "TourneySlots", "TourneySeeds","TourneyRounds","getRound" , "TourneyGeog", "TeamGeog", "whoPicked", "SCurve" #tourney specific data
)], file="data/game data.RData")

# load("data/game data.RData")
