options(stringsAsFactors=FALSE)
options(scipen=999)

# setwd("")
rm(list = ls())
source("functions.R")


###ORGANIZE TEAM RANKINGS#####

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

###SCRAPE DATA#####


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

MOR_Rank<-ldply(lapply(c(2005:2017), readMOR), data.frame)
MOR_Rank$Team_Full<-coordName(MOR_Rank$Team_Full)
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

##538 Data####

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
} else{
  dates<-unique(Massey_means$DATE[Massey_means$Season>=2007])
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


tail(TR_Rank[grepl("Wilm", TR_Rank$Team_Full), ])

setdiff(id_df$Team_Full,TR_Rank$Team_Full )
setdiff( TR_Rank$Team_Full[TR_Rank$Rank.TR<=150], id_df$Team_Full )
unique(TR_Rank$Team_Full[grepl("App", TR_Rank$Team_Full) ])



###ORGANIZE GAME DATA#####

tourney<-read.csv("data/NCAATourneyDetailedResults.csv")
tourney$Tournament<-1

#add 1 row so that have a row with final 2018 Massey Ratings

test<-TourneySeeds[TourneySeeds$Season==2018,c("TeamID", "Season") ]
test$DATE<-as.Date("2018-03-13")
colnames(test)[colnames(test)=="TeamID"]<-"WTeamID"
head(test)

regularSeason<-read.csv("data/RegularSeasonDetailedResults.csv")
regularSeason$Tournament<-0

fulldf<-rbind.fill(tourney, regularSeason)
fulldf$DayZero<-seasons$DayZero[match(fulldf$Season, seasons$Season)]
fulldf$DATE<-fulldf$DayZero+fulldf$DayNum
fulldf$DATE<-as.Date(fulldf$DATE)

fulldf<-rbind.fill(fulldf, test)

fulldf<-fulldf[fulldf$Season>=2005, ]

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
##scrape odds data####

if(!exists("moneyDF")){
  dates<-unique(fulldf$DATE[fulldf$Season>=2007])
} else{
  dates<-as.Date(setdiff(unique(fulldf$DATE[fulldf$DATE>as.Date("2006-11-17")]), moneyDF$DATE))
  
}
dates<-c(dates, seq(as.Date("2018-03-13"), as.Date("2018-03-16"), 1))

importmoney<-function(date) {
  #   date<-dates[77]#Sys.Date()
  url<-read_html(paste0(c("https://www.sportsbookreview.com/betting-odds/ncaa-basketball/money-line/?date=",gsub("-", "", date )), collapse=""))
  money<-url %>%
    html_nodes("b , .eventLine-value") %>%
    html_text()
  if(length(money)>=1){
    money<-money[!grepl("Pitching|Batting", money)]
    # money<-iconv(money, to='ASCII//TRANSLIT')
    money<-gsub("[=]|[?]|[½]", ".5", money)
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


##scrape odds data####

if(!exists("oddsDF")){
  dates<-unique(fulldf$DATE[fulldf$Season>=2007])
} else{
  dates<-as.Date(setdiff(unique(fulldf$DATE[fulldf$DATE>as.Date("2006-11-17")]), oddsDF$DATE))
  
}
dates<-c(dates, seq(as.Date("2018-03-13"), as.Date("2018-03-16"), 1))

importOdds<-function(date) {
  #   date<-dates[77]#Sys.Date()
  url<-read_html(paste0(c("http://www.sportsbookreview.com/betting-odds/ncaa-basketball/?date=",gsub("-", "", date )), collapse=""))
  odds<-url %>%
    html_nodes("b , .eventLine-value") %>%
    html_text()
  if(length(odds)>=1){
    odds<-odds[!grepl("Pitching|Batting", odds)]
    # odds<-iconv(odds, to='ASCII//TRANSLIT')
    odds<-gsub("[=]|[?]|[½]", ".5", odds)
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
# oddsDF<-rbind( data.frame(Team=c("Syracuse", "Tcu", "Xavier", "Texas Southern"), DATE=as.Date("2018-03-16"),
#                           OPP=c("Tcu", "Syracuse", "Texas Southern", "Xavier"), Spread=c(3, -3, -21.5, 21.5)), oddsDF)
oddsDF<-oddsDF[!duplicated(oddsDF[, c("Team", "DATE")]), ]




if(!exists("vegasDF")){
  dates<-unique(fulldf$DATE[fulldf$Season>=2009])
} else{
  dates<-as.Date(setdiff(unique(fulldf$DATE[fulldf$DATE>as.Date("2008-11-17")]), vegasDF$DATE))
  
}
dates<-c(dates, seq(as.Date("2018-03-13"), as.Date("2018-03-16"), 1))

fulldf<-merge(fulldf[, !colnames(fulldf)%in% "Spread"], oddsDF[, c("Team", "Spread", "DATE")], by.x=c("Team_Full","DATE"), by.y=c("Team", "DATE"),all.x=T)
fulldf$Spread[ abs(fulldf$Spread)>=50]<-NA #errors in oddsDF

fulldf<-merge(fulldf[, !colnames(fulldf)%in%c("MoneyLine", "ImpProb")], moneyDF[, c("Team", "DATE", "ImpProb", "MoneyLine")], 
              by.x=c("Team_Full","DATE"), by.y=c("Team", "DATE"),all.x=T)

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
vegas$Team<-gsub("Â«", "", vegas$Team)
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

fulldf<-merge(fulldf[, !colnames(fulldf)%in% c("Side", "Money")], vegasDF[, c("Team", "Side", "Money", "DATE")], by.x=c("Team_Full","DATE"), by.y=c("Team", "DATE"),all.x=T)


#DOL, POM, BPI, SAG, 
cor(fulldf$Win[!is.na(rowSums(fulldf[,c("Rank.POM",  "Rank.KPK", "Rank.SAG", "Rank.DOL")]))], 
    fulldf[!is.na(rowSums(fulldf[,c("Rank.POM", "Rank.KPK", "Rank.SAG", "Rank.DOL")])), grepl("Rank[.]|med|min|max|mean", colnames(fulldf))], use="pairwise.complete.obs")

fulldf$TeamTR<-fulldf$TeamOR+fulldf$TeamDR
fulldf$OPPTR<-fulldf$OPPOR+fulldf$OPPDR

fulldf<-data.table(fulldf)
fulldf<-fulldf[order(fulldf$DATE, decreasing = T), ]
fulldf<-fulldf[,`:=`(
  DaysOff=as.integer(DATE-lead(DATE, 1)),
  TeamscoreMARGIN=moving(TeamScore-OPPScore,41,"median" ),
  TeamFGA3MARGIN=moving(TeamFGA3-OPPFGA3,41,"median" ),
  TeamFTAMARGIN=moving(TeamFTA-OPPFTA,41,"median" ),
  TeamTOMARGIN=moving(TeamTO-OPPTO,41,"median" ),
  TeamTRMARGIN=moving(TeamTR-OPPTR,41,"median" ),
  TeamBlkMARGIN=moving(TeamBlk-OPPBlk,41,"median" ),
  TeamStlMARGIN=moving(TeamStl-OPPStl,41,"median" )
), by=c("Team","Season" ) ]
fulldf<-data.frame(fulldf)
head(fulldf)

head(fulldf)
fulldf$Win_factor<-as.factor(fulldf$Win)
levels(fulldf$Win_factor)<-c("Loss", "Win")

###ADD TOURNAMENT DATA TO FULLDF#####


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
library(geosphere)
fulldf$Dist[!is.na(fulldf$Lat)]<-distHaversine(fulldf[!is.na(fulldf$Lat), c( "Lng", "Lat")], fulldf[!is.na(fulldf$Lat), c( "TeamLng", "TeamLat")])/1000

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

fulldf<-fulldf[, !colnames(fulldf)%in% c("Rank.TR", "OPP.TR", "Score.TR", "OPPScore.TR")]
fulldf<-merge(fulldf, TR_Rank, by.x=c("Team_Full","Rank_DATE"), by.y=c("Team_Full", "DATE"), all.x=T)
OPP_Rank<-TR_Rank
colnames(OPP_Rank)[c(1, 3)]<-c("OPP.TR", "OPPScore.TR")
fulldf<-merge(fulldf, OPP_Rank, by.x=c("OPP_Full","Rank_DATE"), by.y=c("Team_Full", "DATE"), all.x=T)


fulldf$Teamloc_num<-ifelse(fulldf$Teamloc=="H", 1, ifelse(fulldf$Teamloc=="N", 0, -1))
fulldf$OPPloc_num<-ifelse(fulldf$Teamloc=="H", -1, ifelse(fulldf$Teamloc=="N", 0, 1))


fulldf<-fulldf[!is.na(fulldf$Rank_DATE), ] #need to fix this
fulldf[which(fulldf$Team_Full=='North Carolina'& fulldf$Season==2018), ][order(fulldf$DATE[which(fulldf$Team_Full=='North Carolina'& fulldf$Season==2018)], decreasing = T), ][1:5, ]

source("TourneyGeog.R")
tail(TourneyGeog)


save(list=ls()[ls()%in% c("fulldf", "KenPom", "SAG_Rank", "TR_Rank", "Massey_All", "Massey_means","oddsDF","moneyDF", "vegasDF", "id_df", "march538", #projections data
                          "seasons", "TourneySlots", "TourneySeeds","TourneyRounds","getRound" ,
                          "TourneyGeog", "TeamGeog","CitiesEnriched", "GameGeog", "whoPicked", "SCurve" #tourney specific data
)], file="data/game data.RData")

# load("data/game data.RData")
