options(stringsAsFactors=FALSE)
options(scipen=999)

# setwd("")
rm(list = ls())
source("functions.R")


###LOAD TOURNEY/TEAM DATA#####

id_df<-read.csv("data/Teams.csv")
id_df$TeamName<-coordName(id_df$TeamName)
colnames(id_df)[1:2]<-c("TeamID", "Team_Full")

id_df[duplicated(id_df$Team_Full), ]

TourneySlots<-read.csv("data/NCAATourneySlots.csv")

TourneySeeds<-read.csv("data/NCAATourneySeeds.csv")

seasons<-read.csv("data/Seasons.csv")
seasons$DayZero<-as.Date(seasons$DayZero, format="%m/%d/%Y")

#all possible slots that each team can be in
getSlots<-function(seed, season){
  # slot<-"W11"
  slot<-TourneySlots$Slot[TourneySlots$Season==season & (TourneySlots$StrongSeed==seed|TourneySlots$WeakSeed==seed)]
  next_slot<-slot
  while(slot!="R6CH"){
    next_slot<-c(next_slot, 
                 TourneySlots$Slot[TourneySlots$Season==season & (TourneySlots$StrongSeed==slot|TourneySlots$WeakSeed==slot)]
    )
    slot<- TourneySlots$Slot[TourneySlots$Season==season & (TourneySlots$StrongSeed==slot|TourneySlots$WeakSeed==slot)]
  }
  data.frame(Seed=seed, Slot=next_slot, Season=season)
}
TourneyRounds<-lapply(1:nrow(TourneySeeds),function(x) getSlots(TourneySeeds$Seed[x], TourneySeeds$Season[x]))
TourneyRounds<-ldply(TourneyRounds,data.frame)


TeamGeog<-read.csv("data/TeamGeog.csv")
colnames(TeamGeog)[2:3]<-c("TeamLat", "TeamLng")

CitiesEnriched<-read.csv("data/CitiesEnriched.csv")
head(CitiesEnriched)

GameGeog<-read.csv("data/GameCities.csv")
GameGeog<-merge(GameGeog, CitiesEnriched[, c("Lat", "Lng", "CityID", "City","State")], by="CityID")



TeamGeog$Team_Full<-id_df$Team_Full[match(TeamGeog$team_id, id_df$TeamID)]
write.csv(TeamGeog, file="data/TeamGeogEnriched.csv")

###*MASSEY DATA ******
#https://www.kaggle.com/masseyratings/rankings/data

Massey_All<-fread("data/MasseyOrdinals.csv")

colnames(Massey_All)<-c("Season", "DayNum", "Source", "TeamID", "Rank")
Massey_All$DayZero<-seasons$DayZero[match(Massey_All$Season, seasons$Season)]
Massey_All$DATE<-Massey_All$DayZero+Massey_All$DayNum
Massey_All$DATE<-as.Date(Massey_All$DATE)

Massey_All<-data.table(Massey_All)
Massey_means<-Massey_All[, list(  medRank=as.numeric(median(Rank)), 
                                  maxRank=max(Rank), minRank=min(Rank), meanRank=mean(Rank)), by=c("TeamID", "DATE", "Season")]
Massey_means<-Massey_means[order(Massey_means$DATE, decreasing = F), ]
Massey_means<-Massey_means[, `:=`(  preRank=meanRank[1]), by=c("TeamID", "Season")]
Massey_All<-data.frame(Massey_All)
Massey_means<-data.frame(Massey_means)


Massey_means<-Massey_means[month(Massey_means$DATE)!=4,]
Massey_All<-Massey_All[month(Massey_All$DATE) !=4,]

###SCRAPE EXTERNAL DATA#####


readKPOM<-function(year){
  if(year==2017){
    link<-"https://web.archive.org/web/20170315194730/https://kenpom.com/"
  } else if (year==2016){
    link<-"https://web.archive.org/web/20160317134436/https://kenpom.com/"
  } else if (year==2015){
    link<-"https://web.archive.org/web/20150319035724/https://kenpom.com/"
  } else if (year==2014){
    link<-"https://web.archive.org/web/20140320132217/https://kenpom.com"
  } else if (year==2013){
    link<-"https://web.archive.org/web/20130320175417/https://kenpom.com/"
  } else if (year==2012){
    link<-"https://web.archive.org/web/20120315142441/https://kenpom.com/"
  }
  page<-read_html(link)
  info<-page%>% html_nodes("td")%>% html_text()
  info<-data.frame(matrix(info, ncol=21, byrow=T))
  names<-page%>% html_nodes("th a")%>% html_text()
  names<-names[1:21]
  colnames(info)<-c("Rank", "Team", "Conf", "WL", "AdjEM", "AdjO", "AdjO_Rk", "AdjD", "AdjD_Rk",
                    "AdjT","AdjT_Rk" ,"Luck", "Luck_Rk", "SOS_AdjEM","SOS_AdjEM_Rk" , "SOS_OPPO", "SOS_OPPO_Rk", "SOS_OPPD","SOS_OPPD_Rk", 
                    "NCSOS_AdjEM", "NCSOS_AdjEM_Rk")
  info$Season<-year
  info
}
KenPom<-ldply(lapply(2012:2017, readKPOM), data.frame)
KenPom[, !colnames(KenPom)%in% c("Team", "Conf", "WL")]<-sapply(KenPom[, !colnames(KenPom)%in% c("Team", "Conf", "WL")], as.numeric)
KenPom$Team<-gsub(paste(1:20, collapse="|"), "",KenPom$Team)
KenPom$Team<-coordName(KenPom$Team)
setdiff(KenPom$Team, id_df$Team_Full)


readSAG<-function(year){
  if(year<=2013){
    sag<-read.csv(paste(c("SAG/SAG_POST/" ,year, ".csv"), collapse="", sep=""), header=F)
    sag<-sag[,c(1, 2, 3,14)]
  } else{
    sag<-read.csv(paste(c("SAG/" ,year, ".csv"), collapse="", sep=""), header=F)
    sag<-sag[,c(1, 2, 3,ncol(sag)-1)]
  }
  
  colnames(sag)<-c("Rank.SAG.pre", "Team_Full", "Score.SAG.pre", "Score2.SAG.pre")
  sag<-sag[!sag$Rank.SAG.pre%in%c("Colleg", "", " ", "FINAL", "HOME") & 
             !grepl("RATING", sag$Score.SAG.pre) & !grepl("HOME|Home", sag$Team_Full), ]
  sag$Season<-year
  sag
}
##clean up columns--put letters in second column into first column

SAG_Rank<-ldply(lapply(2008:2017, readSAG), data.frame)
SAG_Rank$Score.SAG.pre<-gsub(" |=|$|^\\s+|[*]|[~]|[$]", "", SAG_Rank$Score.SAG.pre)
SAG_Rank$Score.SAG.pre<-gsub("^\\s+|\\s+$", "", SAG_Rank$Score.SAG.pre)

#error in columns
strings<-sapply(strsplit(SAG_Rank$Score.SAG.pre,  "\\s+"), function(x) x[-length(x)])
strings[sapply(strings, length)==0|strings=="$"]<-""
SAG_Rank$Team_Full<-paste0(SAG_Rank$Team_Full,strings )

SAG_Rank$Rank.SAG.pre<-as.numeric(gsub("[;]", "", SAG_Rank$Rank.SAG.pre))
SAG_Rank$Score.SAG.pre<-as.numeric(sapply(strsplit(SAG_Rank$Score.SAG.pre,  "\\s+"), function(x) x[length(x)]))
SAG_Rank$Score2.SAG.pre<-NULL
SAG_Rank$Team_Full<-coordName(SAG_Rank$Team_Full)

setdiff(SAG_Rank$Team_Full, id_df$Team_Full)
head(SAG_Rank)


readMOR<-function(year){
  print(year)
  mor<-read.csv(paste(c("MOR/",year, ".csv"), collapse=""), header=F)
  mor<-mor[, c(2, ncol(mor))]
  colnames(mor)<-c( "Team_Full", "Score.MOR")
  mor$Season<-year
  
  mor
}
##clean up columns--put letters in second column into first column

MOR_Rank<-ldply(lapply(c(2006:2018), readMOR), data.frame)
MOR_Rank$Team_Full<-coordName(MOR_Rank$Team_Full)
MOR_Rank<-MOR_Rank[!duplicated(MOR_Rank[, c("Team_Full", "Season")]) & !MOR_Rank$Team_Full=="", ]
setdiff(MOR_Rank$Team_Full, id_df$Team_Full)


getYahoo<-function(year, round){
  #year<-2012
  if(year==2012){
    data<-read_html(paste0("https://web.archive.org/web/20120316095713/http://tournament.fantasysports.yahoo.com:80/t1/group/all/pickdistribution?round=", round))
  } 
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
  data.frame(rbindlist(lapply(1:6, function(x) getYahoo(year, x))))
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
whoPicked<-ldply(lapply(c(2008:2011, 2013:2018), readFile), data.frame)
setdiff(whoPicked$Team, id_df$Team_Full)

whoPicked<-rbind(whoPicked, whoPicked_yahoo)


##s-curve data#
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

##538 Data#

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

march538_4<-read.csv("538/2018.csv")
march538_4$Season<-2018
march538_4<-march538_4[march538_4$gender=="mens",]
march538_4<-march538_4[!duplicated(march538_4[, c("team_name")], fromLast=T), ]

march538_2<-read.csv("538/2017.csv")
march538_2$Season<-2017
march538_2<-march538_2[march538_2$gender=="mens",]
march538_2<-march538_2[!duplicated(march538_2[, c("team_name")], fromLast=T), ]

march538_3<-read.csv("538/2016.csv")
march538_3$Season<-2016
march538_3<-march538_3[march538_3$gender=="mens" &as.Date(as.character(march538_3$forecast_date), format="%m/%d/%Y")<=as.Date("2016-03-19") ,]
march538_3<-march538_3[!duplicated(march538_3[, c("team_name")], fromLast=T), ]

march538<-data.frame(rbindlist(list(march538_1, march538_2, march538_3, march538_4), fill=T))
march538<-march538[, c("team_name", "Season", "rd2_win", "rd3_win", "rd4_win", "rd5_win", "rd6_win", "rd7_win")]
march538$Team<-coordName((march538$team_name))

setdiff(march538$Team, id_df$Team_Full)




readTR<-function(date){
  Sys.sleep(1)
  print(date)
  link<-paste0("https://www.teamrankings.com/ncaa-basketball/ranking/predictive-by-other/?date=", as.character(date))
  
  #download since getting time-out error sometimes
  download.file(link, destfile = "scrapedpage.html", quiet=TRUE)
  link <- read_html("scrapedpage.html")
  
  data<-link%>% html_nodes("td, h2")%>% html_text()
  data<-data[1:(which(data=="About The New Ratings")-1)]
  data<-data.frame(matrix(data, ncol=9, byrow=T))
  if(nrow(data)>=1){
    data$DATE<-as.Date(date)
  }
  data
}
if(exists("TR_Rank")){
  dates<-unique(Massey_means$DATE[Massey_means$Season>=2007& !Massey_means$DATE%in% TR_Rank$DATE])
  dates<-c(dates, setdiff( as.Date(c("2014-11-14", "2015-11-13", "2016-11-11", "2017-11-10")), TR_Rank$DATE))
  
} else{
  dates<-unique(Massey_means$DATE[Massey_means$Season>=2007])
  dates<-c(dates, as.Date(c("2014-11-14", "2015-11-13", "2016-11-11", "2017-11-10")))
}
rankList<-list();length(rankList)<-length(dates)
for(i in 1:length(dates)){
  rankList[[i]]<-readTR(dates[i])
}
TR_temp<-ldply(rankList, data.frame)
TR_temp<-TR_temp[,c( 1:3, 10)]
colnames(TR_temp)[1:3]<-c("Rank.TR", "Team_Full", "Score.TR")
TR_temp<-TR_temp[TR_temp$Rank.TR!="About The New Ratings",]
TR_temp[,c("Rank.TR", "Score.TR")]<-sapply(TR_temp[,c("Rank.TR", "Score.TR")], as.numeric)
TR_temp$Team_Full<-sapply(strsplit(TR_temp$Team_Full, "\\("), function(x) paste(x[-length(x)], collapse="("))
TR_temp<-TR_temp[TR_temp$Team_Full!=" ",]
TR_temp$Team_Full<-coordName(TR_temp$Team_Full)

if(exists("TR_Rank")){
  TR_Rank<-rbind(TR_Rank, TR_temp)
  TR_Rank<-TR_Rank[!duplicated(TR_Rank[, c("Team_Full", "DATE")], fromLast=T), ]
} else{
  TR_Rank<-TR_temp
}


tail(TR_Rank[grepl("Duke", TR_Rank$Team_Full), ])

setdiff(id_df$Team_Full,TR_Rank$Team_Full )
setdiff( TR_Rank$Team_Full[TR_Rank$Rank.TR<=150], id_df$Team_Full )
unique(TR_Rank$Team_Full[grepl("App", TR_Rank$Team_Full) ])


##2018 tourney results###
tourney2<-read.csv("data/tourney2018.csv",header=F)
colnames(tourney2)<-c("DATE", "Team", "TeamScore", "OPP", "OPPScore")
tourney2$OPPScore<-sapply(strsplit(tourney2$OPPScore, " "), "[[", 1)
tourney2$DATE<-as.Date(tourney2$DATE, format="%m/%d/%Y")
tourney2[, c("TeamScore", "OPPScore")]<-sapply(tourney2[, c("TeamScore", "OPPScore")], as.numeric)
tourney2[, c("Team", "OPP")]<-sapply(tourney2[, c("Team", "OPP")], coordName)
tourney2$WTeam<-ifelse(tourney2$TeamScore>tourney2$OPPScore, tourney2$Team, tourney2$OPP)
tourney2$LTeam<-ifelse(tourney2$TeamScore>tourney2$OPPScore, tourney2$OPP, tourney2$Team)
tourney2$WTeamID<-id_df$TeamID[match(tourney2$WTeam, id_df$Team_Full)]
tourney2$LTeamID<-id_df$TeamID[match(tourney2$LTeam, id_df$Team_Full)]
tourney2$WScore<-ifelse(tourney2$TeamScore>tourney2$OPPScore, tourney2$TeamScore, tourney2$OPPScore)
tourney2$LScore<-ifelse(tourney2$TeamScore>tourney2$OPPScore, tourney2$OPPScore, tourney2$TeamScore)
tourney2$Season<-2018
tourney2$Tournament<-1
tourney2$WLoc<-"N"

###ORGANIZE DATASET#####

tourney<-read.csv("data/NCAATourneyDetailedResults.csv")
tourney$Tournament<-1
regularSeason<-read.csv("data/RegularSeasonDetailedResults.csv")
regularSeason$Tournament<-0

fulldf<-rbind.fill(tourney, regularSeason)
fulldf$DayZero<-seasons$DayZero[match(fulldf$Season, seasons$Season)]
fulldf$DATE<-fulldf$DayZero+fulldf$DayNum
fulldf$DATE<-as.Date(fulldf$DATE)


#add 1 place holder so that have a row with final 2018 Ratings.. only need if running pre-tournament
test<-TourneySeeds[TourneySeeds$Season==2018,c("TeamID", "Season") ]
test$DATE<-as.Date("2018-03-13")
colnames(test)[colnames(test)=="TeamID"]<-"WTeamID"
head(test)
# fulldf<-rbind.fill(fulldf, test)

#if running post tournament use this, if not use above ^^
tourney2<- tourney2[tourney2$DATE>=as.Date("2018-03-13") & (tourney2$WTeamID%in% test$WTeamID| tourney2$LTeam%in% test$WTeamID) ,]
fulldf<-rbind.fill(fulldf,tourney2[, colnames(tourney2)%in% colnames(fulldf)])


fulldf<-fulldf[fulldf$Season>=2005, ]
table(fulldf$Season, fulldf$Tournament)

dates<-unique(Massey_means$DATE)
fulldf$Rank_DATE<-sapply(fulldf$DATE, function(x) max(dates[dates<=x]))
fulldf$Rank_DATE<-as.Date(fulldf$Rank_DATE)

fulldf<-merge(fulldf, GameGeog[, c("Lat", "Lng", "WTeamID", "LTeamID", "DayNum", "Season")], 
              by=c("WTeamID","LTeamID", "DayNum", "Season"), all.x=T)

winners<-fulldf
colnames(winners)<-gsub("W", "Team", colnames(winners))
colnames(winners)<-gsub("L", "OPP", colnames(winners))
colnames(winners)<-gsub("OPPoc", "loc", colnames(winners))
colnames(winners)[colnames(winners)%in% c("TeamTeamID", "OPPTeamID")]<-c("Team", "OPP")
colnames(winners)[colnames(winners)%in% c("OPPat", "OPPng")]<-c("Lat", "Lng")

losers<-fulldf
colnames(losers)<-gsub("W", "OPP", colnames(losers))
colnames(losers)<-gsub("L", "Team", colnames(losers))
colnames(losers)[colnames(losers)=="OPPTeamoc"]<-"Teamloc"
losers$Teamloc<-ifelse(losers$Teamloc=="A", "H", ifelse(losers$Teamloc=="H", "A", "N"))
colnames(losers)[colnames(losers)%in% c( "OPPTeamID", "TeamTeamID")]<-c("OPP", "Team")
colnames(losers)[colnames(losers)%in% c("Teamat", "Teamng")]<-c("Lat", "Lng")

head(losers)

winners$Win<-1
losers$Win<-0
fulldf<-rbind(winners, losers)
fulldf$Team_Full<-id_df$Team_Full[match(fulldf$Team, id_df$TeamID)]
fulldf$OPP_Full<-id_df$Team_Full[match(fulldf$OPP, id_df$TeamID)]

fulldf<-merge(fulldf, Massey_means, by.x=c("Team", "Rank_DATE", "Season"), by.y = c("TeamID", "DATE", "Season"), all.x=T)

OPP_means<-Massey_means
colnames(OPP_means)<-gsub("Team","OPP", colnames(OPP_means))
colnames(OPP_means)<-gsub("team","opp", colnames(OPP_means))
colnames(OPP_means)[grepl("Rank", colnames(OPP_means))]<-paste("OPP", colnames(OPP_means)[grepl("Rank", colnames(OPP_means))], sep="")
head(OPP_means)
fulldf<-merge(fulldf, OPP_means, by.x=c("OPP", "Rank_DATE", "Season"), by.y = c("OPPID", "DATE", "Season"), all.x=T)

sources<- c("MAS", "SAG", "POM", "MOR", "CMP", "PGH", "RTP","BPI",
            "WIL", "WLK", "TPR", "NOL", "PIG", "DOL",  "70T",
            "DII", "DOK", "RT",  "BIH", "KPK")
sources<-unique(Massey_All$Source[Massey_All$Season==2017])
test<-Massey_All[ Massey_All$Source%in%sources,c("TeamID", "Source", "DATE", "Rank", "Season") ]

test<-reshape(test, timevar = c("Source"), idvar = c("DATE", "Season", "TeamID"), direction = "wide")

fulldf<-merge(fulldf[, !grepl("Rank[.]", colnames(fulldf))], 
              test, by.x=c("Team", "Rank_DATE", "Season"), by.y=c("TeamID", "DATE", "Season"), all.x=T)
colnames(test)<-gsub("Rank", "OPP", colnames(test))
fulldf<-merge(fulldf[, !grepl("OPP[.]", colnames(fulldf))], 
              test, by.x=c("OPP", "Rank_DATE", "Season"), by.y=c("TeamID", "DATE", "Season"), all.x=T)

sum(is.na(fulldf$Team_Full[fulldf$Season>=2011]))


##ODDS DATA####

if(!exists("moneyDF")){
  dates<-unique(fulldf$DATE[fulldf$Season>=2007])
} else{
  dates<-as.Date(setdiff(unique(fulldf$DATE[fulldf$DATE>as.Date("2006-11-17")]), moneyDF$DATE))
  
}
dates<-c(dates, seq(as.Date("2018-03-13"), as.Date("2018-03-16"), 1)) %>% unique()

importmoney<-function(date) {
  #   date<-dates[77]#Sys.Date()
  url<-read_html(paste0(c("https://www.sportsbookreview.com/betting-odds/ncaa-basketball/money-line/?date=",gsub("-", "", date )), collapse=""))
  money<-url %>%
    html_nodes("b , .eventLine-value") %>%
    html_text()
  if(length(money)>=1){
    money<-money[!grepl("Pitching|Batting", money)]
    # money<-iconv(money, to='ASCII//TRANSLIT')
    money<-gsub("[=]|[?]|[?]", ".5", money)
    money<-gsub("PK", "0 ", money)
    money<-data.frame(t(matrix(money, nrow=22)))
    money$OPP1<-money[, 2]
    money$OPP2<-money[, 1]
    money<-as.data.frame(mapply(c, money[, c(TRUE, FALSE)], money[, c(FALSE, TRUE)]))
    money[money==""]<-NA
    if(nrow(money)>1){
      money$DATE<-as.Date(date)
    }
  } else{
    money<-data.frame()
  }
  print(date)
  money
}
moneyList<-list();length(moneyList)<-length(dates)
for(i  in 1:length(dates)){
  moneyList[[i]]<-importmoney(dates[i])
}
money<-ldply(moneyList, data.frame)
money<-money[!is.na(money$X1) & !is.na(money$OPP1),]
money$Team<-coordName(tolower(money[, 1]))
money$OPP<-coordName(tolower(money$OPP1))
# money[, 2:11]<-sapply(money[, 2:11], function(x) sapply(strsplit(x, "\\s+"), `[[`, 1))
money[, 2:11]<-sapply(money[, 2:11], as.numeric)
money$MoneyLine<-apply(money[, 2:11], 1, median, na.rm = TRUE)
money<-money[, c("Team","OPP", "MoneyLine", "DATE")]
money$Team<-coordName(sapply(strsplit(money$Team, "\\)"), function(x) sub("^\\s+", "", tail(x, 1))))
money$OPP<-coordName(sapply(strsplit(money$OPP, "\\)"), function(x) sub("^\\s+", "", tail(x, 1))))
money[, c("Team", "OPP")]<-sapply(money[, c("Team", "OPP")],function(x) {gsub(paste0(0:9, collapse="|"), "", x)})
money[, c("Team", "OPP")]<-sapply(money[, c("Team", "OPP")], coordName)

if(!exists("moneyDF")){
  moneyDF<-money
} else{
  moneyDF<-rbind.fill( moneyDF, money)
}
moneyDF<-moneyDF[!is.na(moneyDF$MoneyLine), ]
moneyDF<-moneyDF[!duplicated(moneyDF[, c("Team", "DATE")]), ]
moneyDF$ImpProb<-ifelse(moneyDF$MoneyLine>0, 100/(moneyDF$MoneyLine+100), moneyDF$MoneyLine/(moneyDF$MoneyLine-100))


##scrape odds data#

if(!exists("oddsDF")){
  dates<-unique(fulldf$DATE[fulldf$Season>=2007])
} else{
  dates<-as.Date(setdiff(unique(fulldf$DATE[fulldf$DATE>as.Date("2006-11-17")]), oddsDF$DATE))
  
}
dates<-c(dates, seq(as.Date("2018-03-13"), as.Date("2018-03-16"), 1)) %>% unique()

importOdds<-function(date) {
  #   date<-dates[77]#Sys.Date()
  url<-read_html(paste0(c("http://www.sportsbookreview.com/betting-odds/ncaa-basketball/?date=",gsub("-", "", date )), collapse=""))
  odds<-url %>%
    html_nodes("b , .eventLine-value") %>%
    html_text()
  if(length(odds)>=1){
    odds<-odds[!grepl("Pitching|Batting", odds)]
    odds<-iconv(odds, to='ASCII//TRANSLIT')
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
odds<-odds[!is.na(odds$X1) & !is.na(odds$OPP1),]
odds$Team<-coordName(tolower(odds[, 1]))
odds$OPP<-coordName(tolower(odds$OPP1))
odds[, 2:11]<-sapply(odds[, 2:11], function(x) gsub("A[?]A ", ".5 ",x))
odds[, 2:11]<-sapply(odds[, 2:11], function(x) gsub("A ", " ",x))
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

##scrape odds data#

if(!exists("half1DF")){
  dates<-unique(fulldf$DATE[fulldf$Season>=2007])
} else{
  dates<-as.Date(setdiff(unique(fulldf$DATE[fulldf$DATE>as.Date("2006-11-17")]), half1DF$DATE))
  
}
dates<-c(dates, seq(as.Date("2018-03-13"), as.Date("2018-03-16"), 1)) %>% unique()

importhalf1<-function(date) {
  #   date<-dates[77]#Sys.Date()
  url<-read_html(paste0(c("http://www.sportsbookreview.com/betting-odds/ncaa-basketball/1st-half/?date=",gsub("-", "", date )), collapse=""))
  half1<-url %>%
    html_nodes("b , .eventLine-value") %>%
    html_text()
  if(length(half1)>=1){
    half1<-half1[!grepl("Pitching|Batting", half1)]
    half1<-iconv(half1, to='ASCII//TRANSLIT')
    half1<-gsub("PK", "0 ", half1)
    half1<-data.frame(t(matrix(half1, nrow=22)))
    half1$OPP1<-half1[, 2]
    half1$OPP2<-half1[, 1]
    half1<-as.data.frame(mapply(c, half1[, c(TRUE, FALSE)], half1[, c(FALSE, TRUE)]))
    half1[half1==""]<-NA
    if(nrow(half1)>1){
      half1$DATE<-as.Date(date)
    }
  } else{
    half1<-data.frame()
  }
  print(date)
  half1
}
half1List<-list();length(half1List)<-length(dates)
for(i  in 1:length(dates)){
  half1List[[i]]<-importhalf1(dates[i])
}
half1<-ldply(half1List, data.frame)
half1<-half1[!is.na(half1$X1) & !is.na(half1$OPP1),]
half1$Team<-coordName(tolower(half1[, 1]))
half1$OPP<-coordName(tolower(half1$OPP1))
half1[, 2:11]<-sapply(half1[, 2:11], function(x) gsub("A[?]A ", ".5 ",x))
half1[, 2:11]<-sapply(half1[, 2:11], function(x) gsub("A ", " ",x))
half1[, 2:11]<-sapply(half1[, 2:11], function(x) sapply(strsplit(x, "\\s+"), `[[`, 1))
half1[, 2:11]<-sapply(half1[, 2:11], as.numeric)
half1$Spread1<-apply(half1[, 2:11], 1, median, na.rm = TRUE)
half1<-half1[, c("Team","OPP", "Spread1", "DATE")]
half1$Team<-coordName(sapply(strsplit(half1$Team, "\\)"), function(x) sub("^\\s+", "", tail(x, 1))))
half1$OPP<-coordName(sapply(strsplit(half1$OPP, "\\)"), function(x) sub("^\\s+", "", tail(x, 1))))
half1[, c("Team", "OPP")]<-sapply(half1[, c("Team", "OPP")],function(x) {gsub(paste0(0:9, collapse="|"), "", x)})
half1[, c("Team", "OPP")]<-sapply(half1[, c("Team", "OPP")],function(x) coordName(trimws(x)))

if(!exists("half1DF")){
  half1DF<-half1
} else{
  half1DF<-rbind(half1, half1DF)
}
half1DF<-half1DF[!is.na(half1DF$Spread1), ]
half1DF<-half1DF[!duplicated(half1DF[, c("Team", "DATE")]), ]


##scrape odds data#

if(!exists("total1DF")){
  dates<-unique(fulldf$DATE[fulldf$Season>=2007])
} else{
  dates<-as.Date(setdiff(unique(fulldf$DATE[fulldf$DATE>as.Date("2006-11-17")]), total1DF$DATE))
  
}
dates<-c(dates, seq(as.Date("2018-03-13"), as.Date("2018-03-16"), 1)) %>% unique()

importtotal1<-function(date) {
  #   date<-dates[77]#Sys.Date()
  url<-read_html(paste0(c("http://www.sportsbookreview.com/betting-odds/ncaa-basketball/totals/1st-half/?date=",gsub("-", "", date )), collapse=""))
  total1<-url %>%
    html_nodes("b , .eventLine-value") %>%
    html_text()
  if(length(total1)>=1){
    total1<-total1[!grepl("Pitching|Batting", total1)]
    total1<-iconv(total1, to='ASCII//TRANSLIT')
    total1<-gsub("PK", "0 ", total1)
    total1<-data.frame(t(matrix(total1, nrow=22)))
    total1$OPP1<-total1[, 2]
    total1$OPP2<-total1[, 1]
    total1<-as.data.frame(mapply(c, total1[, c(TRUE, FALSE)], total1[, c(FALSE, TRUE)]))
    total1[total1==""]<-NA
    if(nrow(total1)>1){
      total1$DATE<-as.Date(date)
    }
  } else{
    total1<-data.frame()
  }
  print(date)
  total1
}
total1List<-list();length(total1List)<-length(dates)
for(i  in 1:length(dates)){
  total1List[[i]]<-importtotal1(dates[i])
}
total1<-ldply(total1List, data.frame)
total1<-total1[!is.na(total1$X1) & !is.na(total1$OPP1),]
total1$Team<-coordName(tolower(total1[, 1]))
total1$OPP<-coordName(tolower(total1$OPP1))
total1[, 2:11]<-sapply(total1[, 2:11], function(x) gsub("A[?]A ", ".5 ",x))
total1[, 2:11]<-sapply(total1[, 2:11], function(x) gsub("A ", " ",x))
total1[, 2:11]<-sapply(total1[, 2:11], function(x) sapply(strsplit(x, "\\s+"), `[[`, 1))
total1[, 2:11]<-sapply(total1[, 2:11], as.numeric)
total1$OverUnder1<-apply(total1[, 2:11], 1, median, na.rm = TRUE)
total1<-total1[, c("Team","OPP", "OverUnder1", "DATE")]
total1$Team<-coordName(sapply(strsplit(total1$Team, "\\)"), function(x) sub("^\\s+", "", tail(x, 1))))
total1$OPP<-coordName(sapply(strsplit(total1$OPP, "\\)"), function(x) sub("^\\s+", "", tail(x, 1))))
total1[, c("Team", "OPP")]<-sapply(total1[, c("Team", "OPP")],function(x) {gsub(paste0(0:9, collapse="|"), "", x)})
total1[, c("Team", "OPP")]<-sapply(total1[, c("Team", "OPP")],function(x) coordName(trimws(x)))


if(!exists("total1DF")){
  total1DF<-total1
} else{
  total1DF<-rbind(total1, total1DF)
}
total1DF<-total1DF[!is.na(total1DF$OverUnder1), ]
total1DF<-total1DF[!duplicated(total1DF[, c("Team", "DATE")]), ]



##scrape odds data#

if(!exists("half2DF")){
  dates<-unique(fulldf$DATE[fulldf$Season>=2007])
} else{
  dates<-as.Date(setdiff(unique(fulldf$DATE[fulldf$DATE>as.Date("2006-11-17")]), half2DF$DATE))
  
}
dates<-c(dates, seq(as.Date("2018-03-13"), as.Date("2018-03-16"), 1)) %>% unique()

importhalf2<-function(date) {
  #   date<-dates[77]#Sys.Date()
  url<-read_html(paste0(c("http://www.sportsbookreview.com/betting-odds/ncaa-basketball/2nd-half/?date=",gsub("-", "", date )), collapse=""))
  half2<-url %>%
    html_nodes("b , .eventLine-value") %>%
    html_text()
  if(length(half2)>=1){
    half2<-half2[!grepl("Pitching|Batting", half2)]
    half2<-iconv(half2, to='ASCII//TRANSLIT')
    half2<-gsub("PK", "0 ", half2)
    half2<-data.frame(t(matrix(half2, nrow=22)))
    half2$OPP1<-half2[, 2]
    half2$OPP2<-half2[, 1]
    half2<-as.data.frame(mapply(c, half2[, c(TRUE, FALSE)], half2[, c(FALSE, TRUE)]))
    half2[half2==""]<-NA
    if(nrow(half2)>1){
      half2$DATE<-as.Date(date)
    }
  } else{
    half2<-data.frame()
  }
  print(date)
  half2
}
half2List<-list();length(half2List)<-length(dates)
for(i  in 1:length(dates)){
  half2List[[i]]<-importhalf2(dates[i])
}
half2<-ldply(half2List, data.frame)
half2<-half2[!is.na(half2$X1) & !is.na(half2$OPP1),]
half2$Team<-coordName(tolower(half2[, 1]))
half2$OPP<-coordName(tolower(half2$OPP1))
half2[, 2:11]<-sapply(half2[, 2:11], function(x) gsub("A[?]A ", ".5 ",x))
half2[, 2:11]<-sapply(half2[, 2:11], function(x) gsub("A ", " ",x))
half2[, 2:11]<-sapply(half2[, 2:11], function(x) sapply(strsplit(x, "\\s+"), `[[`, 1))
half2[, 2:11]<-sapply(half2[, 2:11], as.numeric)
half2$Spread2<-apply(half2[, 2:11], 1, median, na.rm = TRUE)
half2<-half2[, c("Team","OPP", "Spread2", "DATE")]
half2$Team<-coordName(sapply(strsplit(half2$Team, "\\)"), function(x) sub("^\\s+", "", tail(x, 1))))
half2$OPP<-coordName(sapply(strsplit(half2$OPP, "\\)"), function(x) sub("^\\s+", "", tail(x, 1))))
half2[, c("Team", "OPP")]<-sapply(half2[, c("Team", "OPP")],function(x) {gsub(paste0(0:9, collapse="|"), "", x)})
half2[, c("Team", "OPP")]<-sapply(half2[, c("Team", "OPP")],function(x) coordName(trimws(x)))

if(!exists("half2DF")){
  half2DF<-half2
} else{
  half2DF<-rbind(half2, half2DF)
}
half2DF<-half2DF[!is.na(half2DF$Spread2), ]
half2DF<-half2DF[!duplicated(half2DF[, c("Team", "DATE")]), ]



##scrape odds data#

if(!exists("total2DF")){
  dates<-unique(fulldf$DATE[fulldf$Season>=2007])
} else{
  dates<-as.Date(setdiff(unique(fulldf$DATE[fulldf$DATE>as.Date("2006-11-17")]), total2DF$DATE))
  
}
dates<-c(dates, seq(as.Date("2018-03-13"), as.Date("2018-03-16"), 1)) %>% unique()

importtotal2<-function(date) {
  #   date<-dates[77]#Sys.Date()
  url<-read_html(paste0(c("http://www.sportsbookreview.com/betting-odds/ncaa-basketball/totals/2nd-half/?date=",gsub("-", "", date )), collapse=""))
  total2<-url %>%
    html_nodes("b , .eventLine-value") %>%
    html_text()
  if(length(total2)>=1){
    total2<-total2[!grepl("Pitching|Batting", total2)]
    total2<-iconv(total2, to='ASCII//TRANSLIT')
    total2<-gsub("PK", "0 ", total2)
    total2<-data.frame(t(matrix(total2, nrow=22)))
    total2$OPP1<-total2[, 2]
    total2$OPP2<-total2[, 1]
    total2<-as.data.frame(mapply(c, total2[, c(TRUE, FALSE)], total2[, c(FALSE, TRUE)]))
    total2[total2==""]<-NA
    if(nrow(total2)>1){
      total2$DATE<-as.Date(date)
    }
  } else{
    total2<-data.frame()
  }
  print(date)
  total2
}
total2List<-list();length(total2List)<-length(dates)
for(i  in 1:length(dates)){
  total2List[[i]]<-importtotal2(dates[i])
}
total2<-ldply(total2List, data.frame)
total2<-total2[!is.na(total2$X1) & !is.na(total2$OPP1),]
total2$Team<-coordName(tolower(total2[, 1]))
total2$OPP<-coordName(tolower(total2$OPP1))
total2[, 2:11]<-sapply(total2[, 2:11], function(x) gsub("A[?]A ", ".5 ",x))
total2[, 2:11]<-sapply(total2[, 2:11], function(x) gsub("A ", " ",x))
total2[, 2:11]<-sapply(total2[, 2:11], function(x) sapply(strsplit(x, "\\s+"), `[[`, 1))
total2[, 2:11]<-sapply(total2[, 2:11], as.numeric)
total2$OverUnder2<-apply(total2[, 2:11], 1, median, na.rm = TRUE)
total2<-total2[, c("Team","OPP", "OverUnder2", "DATE")]
total2$Team<-coordName(sapply(strsplit(total2$Team, "\\)"), function(x) sub("^\\s+", "", tail(x, 1))))
total2$OPP<-coordName(sapply(strsplit(total2$OPP, "\\)"), function(x) sub("^\\s+", "", tail(x, 1))))
total2[, c("Team", "OPP")]<-sapply(total2[, c("Team", "OPP")],function(x) {gsub(paste0(0:9, collapse="|"), "", x)})
total2[, c("Team", "OPP")]<-sapply(total2[, c("Team", "OPP")],function(x) coordName(trimws(x)))


if(!exists("total2DF")){
  total2DF<-total2
} else{
  total2DF<-rbind(total2, total2DF)
}
total2DF<-total2DF[!is.na(total2DF$OverUnder2), ]
total2DF<-total2DF[!duplicated(total2DF[, c("Team", "DATE")]), ]



##scrape totals data##

if(!exists("totalsDF")){
  dates<-unique(fulldf$DATE[fulldf$Season>=2007])
} else{
  dates<-as.Date(setdiff(unique(fulldf$DATE[fulldf$DATE>as.Date("2006-11-17")]), totalsDF$DATE))
  
}
dates<-c(dates, seq(as.Date("2018-03-13"), as.Date("2018-03-16"), 1)) %>% unique()

importtotals<-function(date) {
  #   date<-dates[77]#Sys.Date()
  url<-read_html(paste0(c("http://www.sportsbookreview.com/betting-odds/ncaa-basketball/totals/?date=",gsub("-", "", date )), collapse=""))
  totals<-url %>%
    html_nodes("b , .eventLine-value") %>%
    html_text()
  if(length(totals)>=1){
    totals<-totals[!grepl("Pitching|Batting", totals)]
    totals<-iconv(totals, to='ASCII//TRANSLIT')
    totals<-gsub("PK", "0 ", totals)
    totals<-data.frame(t(matrix(totals, nrow=22)))
    totals$OPP1<-totals[, 2]
    totals$OPP2<-totals[, 1]
    totals<-as.data.frame(mapply(c, totals[, c(TRUE, FALSE)], totals[, c(FALSE, TRUE)]))
    totals[totals==""]<-NA
    if(nrow(totals)>1){
      totals$DATE<-as.Date(date)
    }
  } else{
    totals<-data.frame()
  }
  print(date)
  totals
}
totalsList<-list();length(totalsList)<-length(dates)
for(i  in 1:length(dates)){
  totalsList[[i]]<-importtotals(dates[i])
}
totals<-ldply(totalsList, data.frame)
totals<-totals[!is.na(totals$X1) & !is.na(totals$OPP1),]
totals$Team<-coordName(tolower(totals[, 1]))
totals$OPP<-coordName(tolower(totals$OPP1))
totals[, 2:11]<-sapply(totals[, 2:11], function(x) gsub("A[?]A ", ".5 ",x))
totals[, 2:11]<-sapply(totals[, 2:11], function(x) gsub("A ", " ",x))
totals[, 2:11]<-sapply(totals[, 2:11], function(x) sapply(strsplit(x, "\\s+"), `[[`, 1))
totals[, 2:11]<-sapply(totals[, 2:11], as.numeric)
totals$OverUnder<-apply(totals[, 2:11], 1, median, na.rm = TRUE)
totals<-totals[, c("Team","OPP", "OverUnder", "DATE")]
totals$Team<-coordName(sapply(strsplit(totals$Team, "\\)"), function(x) sub("^\\s+", "", tail(x, 1))))
totals$OPP<-coordName(sapply(strsplit(totals$OPP, "\\)"), function(x) sub("^\\s+", "", tail(x, 1))))
totals[, c("Team", "OPP")]<-sapply(totals[, c("Team", "OPP")],function(x) {gsub(paste0(0:9, collapse="|"), "", x)})
totals[, c("Team", "OPP")]<-sapply(totals[, c("Team", "OPP")],function(x) coordName(trimws(x)))

if(!exists("totalsDF")){
  totalsDF<-totals
} else{
  totalsDF<-rbind(totals, totalsDF)
}
totalsDF<-totalsDF[!is.na(totalsDF$OverUnder), ]
totalsDF<-totalsDF[!duplicated(totalsDF[, c("Team", "DATE")]), ]


#vegas info such as % wagering on each side of spread
if(!exists("vegasDF")){
  dates<-unique(fulldf$DATE[fulldf$Season>=2009])
} else{
  dates<-as.Date(setdiff(unique(fulldf$DATE[fulldf$DATE>as.Date("2008-11-17")]), vegasDF$DATE))
  
}
dates<-c(dates, seq(as.Date("2018-03-13"), as.Date("2018-03-16"), 1)) %>% unique()


importVegas<-function(date){
  #date<-dates[2]
  
  string<-format.Date(date, format="%m-%d-%y")
  url<-paste0("http://www.vegasinsider.com/college-basketball/matchups/matchups.cfm/date/", string)
  
  url <- read_html(url)
  page<- url%>%html_nodes("td")%>% html_text()
  if("Teams"%in% page){
    
    page<-page[which(page=="Teams")[1]:length(page)]
    page<-gsub("[\r\n\t]", "", page)
    
    if(as.Date(date)<Sys.Date()){
      page<-page[unlist(lapply(which(page=="Teams"), function(x) seq(x, x+35, 1)))]
      page<-data.frame(matrix(page, ncol=12, byrow=T))
      names<-as.character(page[1,])
    } else{
      page<-page[unlist(lapply(which(page=="Teams"), function(x) seq(x, x+32, 1)))]
      page<-data.frame(matrix(page, ncol=11, byrow=T))
      names<-as.character(page[1,])
    }
    
    colnames(page)<-names
    page<-page[page$Teams!="Teams", ]
    page$DATE<-as.Date(date)
  } else{
    page<-data.frame()
  }
  
  print(date)
  page
}
vegasList<-list();length(vegasList)<-length(dates)
for(i  in 1:length(dates)){
  vegasList[[i]]<-importVegas(dates[i])
}
vegas<-data.frame(rbindlist(vegasList, fill=T))
vegas<-vegas[, 1:15]
vegas$X.Money.[is.na(vegas$X.Money.)]<-vegas$Money[is.na(vegas$X.Money.)]
vegas$X.Closing.[is.na(vegas$X.Closing.)]<-vegas$Current[is.na(vegas$X.Closing.)]
vegas<-vegas[, 1:13]
vegas$Team<-gsub(paste(0:9, collapse="|"), "", vegas$Teams)
vegas$Team<-gsub("????", "", vegas$Team)
vegas$Team<-trimws(vegas$Team)
vegas$Team<-coordName(vegas$Team)
vegas[, c("Open", "X.Closing.", "X1Half", "X2Half", "Side", "X.Money.", "O.U" )]<-
  sapply(vegas[, c("Open", "X.Closing.", "X1Half", "X2Half", "Side", "X.Money.", "O.U" )], function(x)  as.numeric(gsub("(^\\s+)|(\\s+$)|[%]", "", x)))
colnames(vegas)[colnames(vegas)%in% c("X.Money.")]<-"Money"

errors<-unique(vegas$DATE[vegas$Team%in% c("Western")])


if(!exists("vegasDF")){
  vegasDF<-vegas
} else{
  vegasDF<-rbind(vegasDF,vegas )
}
vegasDF<-vegasDF[!duplicated(vegasDF[, c("DATE", "Team")]), ]



fulldf<-merge(fulldf[, !colnames(fulldf)%in% "OverUnder"], totalsDF[, c("Team", "OverUnder", "DATE")], by.x=c("Team_Full","DATE"), by.y=c("Team", "DATE"),all.x=T)
fulldf<-merge(fulldf[, !colnames(fulldf)%in% "Spread"], oddsDF[, c("Team", "Spread", "DATE")], by.x=c("Team_Full","DATE"), by.y=c("Team", "DATE"),all.x=T)
fulldf<-merge(fulldf[, !colnames(fulldf)%in%c("MoneyLine", "ImpProb")], moneyDF[, c("Team", "DATE", "ImpProb", "MoneyLine")], by.x=c("Team_Full","DATE"), by.y=c("Team", "DATE"),all.x=T)
fulldf<-merge(fulldf[, !colnames(fulldf)%in% c("Side", "Money")], vegasDF[, c("Team", "Side", "Money", "DATE")], by.x=c("Team_Full","DATE"), by.y=c("Team", "DATE"),all.x=T)
fulldf<-merge(fulldf[, !colnames(fulldf)%in% "Spread1"], half1DF[, c("Team", "Spread1", "DATE")], by.x=c("Team_Full","DATE"), by.y=c("Team", "DATE"),all.x=T)
fulldf<-merge(fulldf[, !colnames(fulldf)%in% "Spread2"], half2DF[, c("Team", "Spread2", "DATE")], by.x=c("Team_Full","DATE"), by.y=c("Team", "DATE"),all.x=T)
fulldf<-merge(fulldf[, !colnames(fulldf)%in% "OverUnder1"], total1DF[, c("Team", "OverUnder1", "DATE")], by.x=c("Team_Full","DATE"), by.y=c("Team", "DATE"),all.x=T)
fulldf<-merge(fulldf[, !colnames(fulldf)%in% "OverUnder2"], total2DF[, c("Team", "OverUnder2", "DATE")], by.x=c("Team_Full","DATE"), by.y=c("Team", "DATE"),all.x=T)
fulldf[, c("Spread1", "Spread", "Spread2")][ abs(fulldf[, c("Spread1", "Spread", "Spread2")])>=50]<-NA #errors in oddsDF


#DOL, POM, BPI, SAG, 
cor(fulldf$Win[!is.na(rowSums(fulldf[,c("Rank.POM",  "Rank.KPK", "Rank.SAG", "Rank.DOL")]))], 
    fulldf[!is.na(rowSums(fulldf[,c("Rank.POM", "Rank.KPK", "Rank.SAG", "Rank.DOL")])), grepl("Rank[.]|med|min|max|mean", colnames(fulldf))], use="pairwise.complete.obs")

fulldf$TeamTR<-fulldf$TeamOR+fulldf$TeamDR
fulldf$OPPTR<-fulldf$OPPOR+fulldf$OPPDR


####FGA3 ANALYSIS#####
# fulldf<-data.table(fulldf)
# fulldf<-fulldf[order(fulldf$DATE, decreasing = T), ]
# fulldf<-fulldf[,`:=`(
#   DaysOff=as.integer(DATE-lead(DATE, 1)),
#   
#   SeasonScore=moving(TeamScore,41,"mean" ),
#   SeasonFGA3=moving(TeamFGA3,41,"mean" ),
#   SeasonFTA=moving(TeamFTA,41,"mean" ),
#   SeasonTO=moving(TeamTO,41,"mean" ),
#   SeasonTR=moving(TeamTR,41,"mean" ),
#   SeasonBlk=moving(TeamBlk,41,"mean" ),
#   SeasonStl=moving(TeamStl,41,"mean" ),
#   
#   
#   TeamscoreMARGIN=moving(TeamScore-OPPScore,41,"median" ),
#   TeamFGA3MARGIN=moving(TeamFGA3-OPPFGA3,41,"median" ),
#   TeamFTAMARGIN=moving(TeamFTA-OPPFTA,41,"median" ),
#   TeamTOMARGIN=moving(TeamTO-OPPTO,41,"median" ),
#   TeamTRMARGIN=moving(TeamTR-OPPTR,41,"median" ),
#   TeamBlkMARGIN=moving(TeamBlk-OPPBlk,41,"median" ),
#   TeamStlMARGIN=moving(TeamStl-OPPStl,41,"median" )
# ), by=c("Team","Season" ) ]
# fulldf<-data.table(fulldf)
# fulldf<-fulldf[order(fulldf$DATE, decreasing = T), ]
# fulldf<-fulldf[,`:=`(
#   Last30FGA3=moving(TeamFGA3,30,"mean" ),
#   Last10FGA3=moving(TeamFGA3,10,"mean" ),
#   Last5FGA3=moving(TeamFGA3,5,"mean" ),
#   Last3FGA3=moving(TeamFGA3,3,"mean" ),
#   LastFGA3=moving(TeamFGA3,1,"mean" )
# ), by=c("Team" ) ]
# fulldf<-fulldf[order(fulldf$DATE, decreasing = T), ]
# fulldf<-fulldf[,`:=`(
#   OPPSeasonScore=moving(OPPScore,41,"mean" ),
#   OPPSeasonFGA3=moving(OPPFGA3,41,"mean" ),
#   OPPSSeasonFTA=moving(OPPFTA,41,"mean" ),
#   OPPSSeasonTO=moving(OPPTO,41,"mean" ),
#   OPPSSeasonTR=moving(OPPTR,41,"mean" ),
#   OPPSSeasonBlk=moving(OPPBlk,41,"mean" ),
#   OPPSSeasonStl=moving(OPPStl,41,"mean" )
# ), by=c("OPP","Season" ) ]
# fulldf<-fulldf[order(fulldf$DATE, decreasing = T), ]
# fulldf<-fulldf[,`:=`(
#   OPPLast30FGA3=moving(OPPFGA3,30,"mean" ),
#   OPPLast10FGA3=moving(OPPFGA3,10,"mean" ),
#   OPPLast5FGA3=moving(OPPFGA3,5,"mean" ),
#   OPPLast3FGA3=moving(OPPFGA3,3,"mean" ),
#   OPPLastFGA3=moving(OPPFGA3,1,"mean" )
# ), by=c("OPP" ) ]
# 
# #opponent fga3 defense
# fulldf<-data.table(fulldf)
# fulldf<-fulldf[order(fulldf$DATE, decreasing = T), ]
# fulldf<-fulldf[,`:=`(
#   OPPSeasonFGA3_vs=moving(TeamFGA3,41,"mean" ),
#   OPPSeasonScore_vs=moving(TeamScore,41,"mean" )
# ), by=c("OPP","Season" ) ]
# fulldf<-fulldf[order(fulldf$DATE, decreasing = T), ]
# fulldf<-fulldf[,`:=`(
#   OPPLast30Score_vs=moving(TeamScore,30,"mean" ),
#   OPPLast30FGA3_vs=moving(TeamFGA3,30,"mean" ),
#   OPPLast10FGA3_vs=moving(TeamFGA3,10,"mean" )
# ), by=c("OPP" ) ]
# 
# fulldf<-data.table(fulldf)
# fulldf<-fulldf[order(fulldf$DATE, decreasing = T), ]
# fulldf<-fulldf[,`:=`(
#   SeasonFGA3_vs=moving(OPPFGA3,41,"mean" ),
#   SeasonScore_vs=moving(OPPScore,41,"mean" )
# ), by=c("Team","Season" ) ]
# fulldf<-fulldf[order(fulldf$DATE, decreasing = T), ]
# fulldf<-fulldf[,`:=`(
#   Last30Score_vs=moving(OPPScore,30,"mean" ),
#   Last30FGA3_vs=moving(OPPFGA3,30,"mean" ),
#   Last10FGA3_vs=moving(OPPFGA3,10,"mean" )
# ), by=c("Team" ) ]
# 
# fulldf<-data.frame(fulldf)
# # fulldf$TeamFGA3_est<-rowMeans(fulldf[, c("SeasonFGA3", "Last10FGA3", "Last30FGA3")], na.rm=T)
# # fulldf$OPPFGA3_est<-rowMeans(fulldf[, c("OPPSeasonFGA3", "OPPLast10FGA3", "OPPLast30FGA3")], na.rm=T)

# fit<-lm(TeamFGA3~OPPSeasonFGA3_vs+OPPLast30FGA3_vs+OPPLast10FGA3_vs+SeasonFGA3+Last10FGA3+Last30FGA3, data=fulldf);summary(fit)
# fulldf$TeamFGA3_est<-predict(fit, newdata=fulldf)
# 
# fit<-lm(OPPFGA3~SeasonFGA3_vs+Last30FGA3_vs+Last10FGA3_vs+OPPSeasonFGA3+OPPLast10FGA3+OPPLast30FGA3, data=fulldf);summary(fit)
# fulldf$OPPFGA3_est<-predict(fit, newdata=fulldf)


head(fulldf)
fulldf$Win_factor<-as.factor(fulldf$Win)
levels(fulldf$Win_factor)<-c("Loss", "Win")

###ADD MORE DATA TO DATASET#####


fulldf<-merge(fulldf, march538[, !colnames(march538)=="team_name"], by.x=c("Team_Full", "Season"), by.y=c("Team", "Season"), all.x=T)
march538_2<-march538
colnames(march538_2)<-gsub("rd", "OPPrd", colnames(march538_2))
fulldf<-merge(fulldf, march538_2[, !colnames(march538_2)=="team_name"], by.x=c("OPP_Full", "Season"), by.y=c("Team", "Season"), all.x=T)

fulldf<-merge(fulldf, TourneySeeds, by.x=c("Season", "Team"), by.y=c("Season", "TeamID"), all.x=T)
colnames(fulldf)[colnames(fulldf)=="Seed"]<-"TeamSeed"
fulldf<-merge(fulldf, TourneySeeds,by.x=c("OPP", "Season") ,by.y=c("TeamID", "Season"), all.x=T)
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
fulldf$Slot<-NA;fulldf$Round<-NA
fulldf$Slot[which(fulldf$Tournament==1)]<-sapply(which(fulldf$Tournament==1),    function(x)getSlot(fulldf$TeamSeed[x], fulldf$OPPSeed[x], fulldf$Season[x]) )
fulldf$Round[which(fulldf$Tournament==1)]<-sapply(which(fulldf$Tournament==1),    function(x)getRound(fulldf$TeamSeed[x], fulldf$OPPSeed[x], fulldf$Season[x]) )
fulldf$Round<-as.numeric(fulldf$Round)

fulldf<-merge(fulldf, TeamGeog[, c("team_id", "TeamLat", "TeamLng")], by.x=c("Team"), by.y=("team_id"), all.x=T)
fulldf$Dist<-NA

source("create datasets_tourney geog.R")
#if results are loaded
fulldf$Lat[fulldf$Season==2018& fulldf$Tournament==1]<-
  TourneyGeog$Lat[TourneyGeog$Season==2018][match(fulldf$Slot[fulldf$Season==2018& fulldf$Tournament==1],  TourneyGeog$Slot[TourneyGeog$Season==2018])]
fulldf$Lng[fulldf$Season==2018& fulldf$Tournament==1]<-
  TourneyGeog$Lng[TourneyGeog$Season==2018][match(fulldf$Slot[fulldf$Season==2018& fulldf$Tournament==1],  TourneyGeog$Slot[TourneyGeog$Season==2018])]


fulldf$Dist[!is.na(fulldf$Lat)]<-geosphere::distHaversine(fulldf[!is.na(fulldf$Lat), c( "Lng", "Lat")], fulldf[!is.na(fulldf$Lat), c( "TeamLng", "TeamLat")])/1000


#opponent distance
oppDist<-unique(fulldf[!is.na(fulldf$Dist), c("Team_Full","DATE", "Dist" )])
colnames(oppDist)[colnames(oppDist)=="Dist"]<-"OPPDist"
fulldf<-merge(fulldf, oppDist, by.x=c("OPP_Full", "DATE"), by.y=c("Team_Full", "DATE"), all.x=T)

fulldf<-data.table(fulldf)
fulldf<-fulldf[order(fulldf$DATE, decreasing = T), ]
fulldf<-fulldf[,`:=`(
  R1_Spread=as.numeric(ifelse(1%in% Tournament, Spread[which(Tournament==1 & Round==1)], NA))
), by=c("Team","Season" ) ]
fulldf<-data.frame(fulldf)

fulldf<-fulldf[, !grepl("Ownership_", colnames(fulldf))]
fulldf$TeamOwnership_RX<-NA
fulldf$OPPOwnership_RX<-NA

for(i in c("R1", "R2", "R3", "R4", "R5", "R6")){
  fulldf<-merge(fulldf, whoPicked[whoPicked$Round==i,c("Team", "Season", "Ownership")  ], by.x=c("Team_Full", "Season"), by.y=c("Team", "Season"), all.x=T)
  colnames(fulldf)[colnames(fulldf)=="Ownership"]<-paste0("TeamOwnership_", i)
  
  fulldf<-merge(fulldf, whoPicked[whoPicked$Round==i, c("Team", "Season", "Ownership") ], by.x=c("OPP_Full", "Season"), by.y=c("Team", "Season"), all.x=T)
  colnames(fulldf)[colnames(fulldf)=="Ownership"]<-paste0("OPPOwnership_", i)
  
  bool<-which(fulldf$Round==as.numeric(gsub("R", "", i)))
  fulldf$TeamOwnership_RX[bool]<-fulldf[bool,paste0("TeamOwnership_", i) ]
  fulldf$OPPOwnership_RX[bool]<-fulldf[bool,paste0("OPPOwnership_", i) ]
}



#sagrank

SCurve$SCurve<-as.numeric(SCurve$SCurve)
fulldf<-fulldf[, !grepl("SCurve", colnames(fulldf))]
fulldf<-merge(fulldf, SCurve[, c("Team", "Season", "SCurve")], by.x=c("Team_Full", "Season"), by.y=c("Team", "Season"),all.x=T)
OPPCurve<-SCurve
colnames(OPPCurve)[colnames(OPPCurve)=="SCurve"]<-"OPPSCurve"
fulldf<-merge(fulldf, OPPCurve[, c("Team", "Season", "OPPSCurve")], by.x=c("OPP_Full", "Season"), by.y=c("Team", "Season"),all.x=T)

#kenpom

fulldf<-merge(fulldf, KenPom[, !colnames(KenPom)%in% c("Rank", "Conf", "WL")], by.x=c("Team_Full","Season"), by.y=c("Team", "Season"), all.x=T)
OppPom<-KenPom
colnames(OppPom)[ !colnames(OppPom)%in% c("Rank", "Team", "Conf", "WL", "Season")]<-
  paste("OPP", colnames(OppPom)[ !colnames(OppPom)%in% c("Rank", "Team", "Conf", "WL", "Season")], sep="")
fulldf<-merge(fulldf, OppPom[, !colnames(KenPom)%in% c("Rank", "Conf", "WL")], by.x=c("OPP_Full","Season"), by.y=c("Team", "Season"), all.x=T)

#sag-pre tournament

fulldf<-fulldf[, !grepl("SAG[.]pre", colnames(fulldf))]
fulldf<-merge(fulldf, SAG_Rank, by=c("Team_Full","Season"), all.x=T)
OPP_Rank<-SAG_Rank
colnames(OPP_Rank)[ !colnames(OPP_Rank)%in% c("Team_Full", "Season")]<-
  paste( "OPP", colnames(OPP_Rank)[ !colnames(OPP_Rank)%in% c("Team_Full", "Season")], sep="")
fulldf<-merge(fulldf, OPP_Rank, by.x=c("OPP_Full","Season"), by.y=c("Team_Full", "Season"), all.x=T)

#mor-pre/post tournament (pre for 2012, 13, 14, 17)

fulldf<-fulldf[, !grepl("Score[.]MOR", colnames(fulldf))]
fulldf<-merge(fulldf, MOR_Rank, by=c("Team_Full","Season"), all.x=T)
OPP_Rank<-MOR_Rank
colnames(OPP_Rank)[ !colnames(OPP_Rank)%in% c("Team_Full", "Season")]<-
  paste( "OPP", colnames(OPP_Rank)[ !colnames(OPP_Rank)%in% c("Team_Full", "Season")], sep="")
fulldf<-merge(fulldf, OPP_Rank, by.x=c("OPP_Full","Season"), by.y=c("Team_Full", "Season"), all.x=T)

#team-rankings

fulldf<-fulldf[, !colnames(fulldf)%in% c("Rank.TR", "OPP.TR", "Score.TR", "OPPScore.TR")]
fulldf<-merge(fulldf, TR_Rank, by.x=c("Team_Full","Rank_DATE"), by.y=c("Team_Full", "DATE"), all.x=T)
OPP_Rank<-TR_Rank
colnames(OPP_Rank)[c(1, 3)]<-c("OPP.TR", "OPPScore.TR")
fulldf<-merge(fulldf, OPP_Rank, by.x=c("OPP_Full","Rank_DATE"), by.y=c("Team_Full", "DATE"), all.x=T)


fulldf$Teamloc_num<-ifelse(fulldf$Teamloc=="H", 1, ifelse(fulldf$Teamloc=="N", 0, -1))
fulldf$OPPloc_num<-ifelse(fulldf$Teamloc=="H", -1, ifelse(fulldf$Teamloc=="N", 0, 1))


fulldf<-fulldf[!is.na(fulldf$Rank_DATE), ] #need to fix this
fulldf[which(fulldf$Team_Full=='North Carolina'& fulldf$Season==2018), ][order(fulldf$DATE[which(fulldf$Team_Full=='North Carolina'& fulldf$Season==2018)], decreasing = T), ][1:5, ]



save(list=ls()[ls()%in% c("fulldf",           #dataset
                          
                          "KenPom", "SAG_Rank", "TR_Rank","whoPicked", "SCurve" , 
                          "oddsDF","moneyDF", "vegasDF","totalsDF",  "march538","half1DF","total1DF","half2DF", "total2DF",       #scraped data
                          
                          "seasons","id_df", "TourneySlots", "TourneySeeds","TourneyRounds","getRound" ,
                          "TourneyGeog", "TeamGeog","CitiesEnriched","Massey_means", "Massey_All",  "GameGeog"    #team/tourney data
)], file="data/game data.RData")


# load("data/game data.RData")
