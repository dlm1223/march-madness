load("data/game-data.RData")
source("functions.R", encoding = "UTF-8")


###LOAD TOURNEY/TEAM DATA#####

NCAATourneyDetailedResults<-read.csv("data/MNCAATourneyDetailedResults.csv")
NCAATourneyDetailedResults$Tournament<-1
RegularSeasonDetailedResult<-read.csv("data/MRegularSeasonDetailedResults.csv")
RegularSeasonDetailedResult$Tournament<-0
SecondaryTourneyDetailedResults<-read.csv("data/MSecondaryTourneyCompactResults.csv")
SecondaryTourneyDetailedResults$Tournament<-1

Teams<-read.csv("data/MTeams.csv")
Teams$TeamName<-coordTeam(Teams$TeamName)
colnames(Teams)[1:2]<-c("TeamID", "Team_Full")

Teams[duplicated(Teams$Team_Full), ]

# source("misc-create-tourney-geog.R")
TourneySeeds<-read.csv("data/MNCAATourneySeeds.csv")
TourneySeeds_temp<-read.csv("data/MNCAATourneySeeds_temp.csv")
TourneySeeds_temp$TeamID[TourneySeeds_temp$Season==2022]<-coordTeam(TourneySeeds_temp$TeamID[TourneySeeds_temp$Season==2022])
TourneySeeds_temp$TeamID[TourneySeeds_temp$Season==2022]<-Teams$TeamID[match(TourneySeeds_temp$TeamID[TourneySeeds_temp$Season==2022], Teams$Team_Full)]
TourneySeeds_temp$TeamID[TourneySeeds_temp$Season==2023]<-coordTeam(TourneySeeds_temp$TeamID[TourneySeeds_temp$Season==2023])
TourneySeeds_temp$TeamID[TourneySeeds_temp$Season==2023]<-Teams$TeamID[match(TourneySeeds_temp$TeamID[TourneySeeds_temp$Season==2023], Teams$Team_Full)]
TourneySeeds_temp$TeamID<-as.numeric(TourneySeeds_temp$TeamID)
tail(TourneySeeds_temp, 100)
TourneySeeds<-TourneySeeds_temp
TourneySeeds[duplicated(TourneySeeds[, c("TeamID", "Season")]),]

TourneySlots<-read.csv("data/MNCAATourneySlots.csv")

TourneySlots<-read.csv("data/MNCAATourneySlots_temp.csv") #Update playin-games seeds which may change year to year
TourneySlots[, c("Slot", "StrongSeed", "WeakSeed")]<-sapply(TourneySlots[, c("Slot", "StrongSeed", "WeakSeed")], as.character)

Seasons<-read.csv("data/MSeasons.csv")
Seasons$DayZero<-as.Date(sapply(strsplit(Seasons$DayZero, " "),`[[`,1))
table(Seasons$Season)
head(Seasons)

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
table(TourneyRounds$Season)


#manually add current season,can delete this once kaggle data released


# TourneySlots2<-TourneySlots[TourneySlots$Season==2021,]
# TourneySlots2$Season<-2022
# TourneySlots<-rbind.fill(TourneySlots, TourneySlots2)
# table(TourneySlots$Season)
# setdiff(TourneySeeds$Seed[TourneySeeds$Season==2022], TourneySlots$StrongSeed[TourneySlots$Season==2022])


# 
# TeamGeog<-read.csv("data/TeamGeog.csv")
# colnames(TeamGeog)[2:3]<-c("TeamLat", "TeamLng")
# 
# CitiesEnriched<-read.csv("data/CitiesEnriched.csv")
# Cities2<-data.frame(CityID=4444:4449, 
#                     City=c("Florence", "Georgetown", "Seaside", "Kissimmee", "Oakland", "St. Petersburg"),
#                     State=c("AL", "KY", "CA", "FL", "MI", "FL"), 
#                     Lat=c(34.7998,38.2098,36.6149,28.2920,  37.8044,27.7676), 
#                     Lng=c(-87.6773,-84.5588,-121.8221 ,-81.4076,-122.2711,-82.6403))
# CitiesEnriched<-rbind.fill(CitiesEnriched, Cities2[!Cities2$CityID%in% CitiesEnriched$CityID,])
# 
# GameGeog<-read.csv("data/MGameCities.csv")
# GameGeog<-merge(GameGeog, CitiesEnriched[, c("Lat", "Lng", "CityID", "City","State")], by="CityID", all.x = T)
# GameGeog$WTeam<-Teams$Team_Full[match(GameGeog$WTeamID, Teams$TeamID)]
# GameGeog$LTeam<-Teams$Team_Full[match(GameGeog$LTeamID, Teams$TeamID)]
# GameGeog$DayZero<-Seasons$DayZero[match(GameGeog$Season, Seasons$Season)]
# GameGeog$DATE<-GameGeog$DayZero+GameGeog$DayNum
# GameGeog<-melt(GameGeog,id.vars =setdiff(colnames(GameGeog), c("WTeam", "LTeam")), value.name = "Team")
# GameGeog<-GameGeog[, !colnames(GameGeog)%in%c("variable", "WTeamID", "LTeamID")]
# head(GameGeog)

#add more game-distances
# load("data/merged_locs.Rda") #scraped-realgm data
# merged_locs<-merged_locs[,  c("Team", "Date", "City", "State.Abb", "Lat", "Lng")]
# colnames(merged_locs)[colnames(merged_locs)%in% c("Date", "State.Abb")]<-c("DATE", "State")
# merged_locs$Season<-ifelse(month(merged_locs$DATE)%in% 10:12, year(merged_locs$DATE)+1, year(merged_locs$DATE))
# GameGeog<-rbind.fill(GameGeog, merged_locs[merged_locs$Season<=2009,])
# table(GameGeog$Season)
# 
# write.csv(GameGeog, file="data/GameGeogEnriched.csv") #use theses for my other project
# 
# TeamGeog$Team_Full<-Teams$Team_Full[match(TeamGeog$team_id, Teams$TeamID)]
# write.csv(TeamGeog, file="data/TeamGeogEnriched.csv") #usethese for other project
# 
# TourneyGeog<-read.csv("data/TourneyGeog.csv")
# TourneyGeog<-TourneyGeog[, c("season", "slot", "lat", "lng")]
# colnames(TourneyGeog)<-c("Season", "Slot", "Lat", "Lng")
# table(TourneyGeog$Season)
# source("misc-create-tourney-geog.R")
# 
# TourneyGeog<-rbind(TourneyGeog, slots2[,c("Season", "Slot", "Lat", "Lng")])
# TourneyGeog<-rbind(TourneyGeog, slots[,c("Season", "Slot", "Lat", "Lng")])
# table(TourneyGeog$Season)


# TeamCoaches<-read.csv("data/MTeamCoaches.csv")
# TeamCoaches$DayZero<-Seasons$DayZero[match(TeamCoaches$Season, Seasons$Season)]
# TeamCoaches$FirstDATE<-TeamCoaches$DayZero+TeamCoaches$FirstDayNum
# TeamCoaches$LastDATE<-TeamCoaches$DayZero+TeamCoaches$LastDayNum



###MASSEY DATA####
#https://www.kaggle.com/masseyratings/rankings/data

Massey_All<-fread("data/MMasseyOrdinals.csv")

colnames(Massey_All)<-c("Season", "DayNum", "Source", "TeamID", "Rank")
Massey_All$DayZero<-Seasons$DayZero[match(Massey_All$Season, Seasons$Season)]
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



Massey_temp<-Massey_All[Massey_All$DATE==as.Date("2019-03-13"),]
Massey_temp$DATE<-as.Date("2019-03-18")
Massey_All<-rbind(Massey_All, Massey_temp)
unique(Massey_All$DATE[Massey_All$Season==2019])

###SCRAPE EXTERNAL DATA#####

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
  names$Team<-coordTeam(sapply(strsplit(names$Team, " \\("), `[[`, 1))
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
setdiff(whoPicked_yahoo$Team, Teams$Team_Full)

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
  whoPicked2$Team<-coordTeam((whoPicked2$Team))
  whoPicked2$Season<-year
  whoPicked2
}
whoPicked<-ldply(lapply(c(2008:2011, 2013:2019), readFile), data.frame)
whoPicked<-rbind(whoPicked, whoPicked_yahoo)
whoPicked$Team[whoPicked$Team=='Pr / Sc']<-'Providence'
whoPicked$Team[whoPicked$Team=="North Carolina / Ud"]<-'Uc Davis'
whoPicked$Team[whoPicked$Team=="Abil Christian"]<-'Abilene Christian'

whoPicked2<-ldply(lapply(c(2021:2023), readFile), data.frame)
whoPicked2$Team[whoPicked2$Team=="Iu"]<-'Indiana'
whoPicked<-rbind(whoPicked[!whoPicked$Season%in% whoPicked2$Season,], whoPicked2)
setdiff(whoPicked$Team, Teams$Team_Full)



readPage<-function(year){
  print(year)
  page<-read_html(paste0("https://www.sportsoddshistory.com/cbb-div/?y=",year-1 ,"-", year ,"&sa=cbb&a=tour&o=r1"))
  result<-page%>% html_table(fill=T, header=T)
  result[[2]]$Region<-"A"
  result[[3]]$Region<-"B"
  result[[4]]$Region<-"C"
  result[[5]]$Region<-"D"
  result<-rbindlist(result[2:5])
  colnames(result)[2]<-"Final.Four.Odds"
  result<-result[, c("Team", "Region", "Final.Four.Odds", "Result")]
  result<-result[!result$Team=="Team"&!is.na(result$Team)]
  result$Final.Four.Odds<-as.numeric(result$Final.Four.Odds)
  result$Final.Four.Odds.dec<-ifelse(result$Final.Four.Odds>0, result$Final.Four.Odds/100+1, -100/result$Final.Four.Odds+1)
  result$ImpliedProb<-1/result$Final.Four.Odds.dec
  result[, ImpliedProb.sum:=sum(ImpliedProb), by="Region"]
  result[, ImpliedProb:=ImpliedProb/ImpliedProb.sum]
  result$Year<-year
  result
}
final.four.odds<-ldply(lapply(c(2013:2019, 2021), readPage), data.frame)
final.four.odds$Team<-coordTeam(final.four.odds$Team)
final.four.odds$Team[final.four.odds$Team=="Stjohns"]<-"St Johns"
setdiff(final.four.odds$Team, Teams$Team_Full)


save(list=ls()[ls()%in% c( "TR_Rank","whoPicked", "final.four.odds",# "SCurve" ,  "KenPom", "SAG_Rank","march538",     #scraped  data
                          
                          "Seasons","Teams", "TourneySlots","TeamCoaches","TourneySeeds_temp",
                          "TourneySeeds","TourneyRounds","NCAATourneyDetailedResults","RegularSeasonDetailedResult","SecondaryTourneyDetailedResults",
                          "TourneyGeog", "TeamGeog",  "GameGeog"  ,"CitiesEnriched","Massey_means", "Massey_All" #team/tourney data
)], file="data/game-data.RData")


# load("data/game-data.RData")