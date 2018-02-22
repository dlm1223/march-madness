#some random functions

library(gridExtra)
library(grid)
library(forecast);library(RSelenium);library(RCurl);library(slam);library(XML)
library(caret);library(doSNOW);library(rvest);library(robustbase);library(rpart);library(kernlab)
library(lpSolve);library(lpSolveAPI);library(data.table);library(xgboost);#library(gurobi);library(Rcplex)
library(reshape2);library(plyr);library(zoo);library(dplyr);library(gtools);library(RJSONIO)
library(ggplot2);library(glmnet);library(MASS);library(TTR) 
library(Metrics)
options(stringsAsFactors=FALSE)
options(scipen=999)


coordName<-function(x){
  x<-tolower(x)
  x<-sub("^\\s+", "",x)
  x[substr(x, 1, 2)=="s "]<-gsub("s ", "south ", x[substr(x, 1, 2)=="s "])
  x[substr(x, 1, 2)=="n "]<-gsub("n ", "north ", x[substr(x, 1, 2)=="n "])
  x[substr(x, 1, 2)=="e "]<-gsub("e ", "eastern ", x[substr(x, 1, 2)=="e "])
  x[substr(x, 1, 2)=="w "]<-gsub("w ", "western ", x[substr(x, 1, 2)=="w "])
  x[substr(x, 1, 2)=="c "]<-gsub("c ", "central ", x[substr(x, 1, 2)=="c "])
  x[substr(x, 1, 3)=="cs "]<-gsub("cs ", "cal st ", x[substr(x, 1, 3)=="cs "])
  x[substr(x, 1, 3)=="nc "]<-gsub("nc ", "north carolina ", x[substr(x, 1, 3)=="nc "])
  x[substr(x, 1, 3)=="st "]<-gsub("st ", "saint ", x[substr(x, 1, 3)=="st "])
  x[substr(x, 1, 3)=="wi "]<-gsub("wi ", "wisconsin ", x[substr(x, 1, 3)=="wi "])
  x[substr(x, 1, 3)=="sc "]<-gsub("sc ", "south carolina ", x[substr(x, 1, 3)=="sc "])
  x[x=="va commonwealth"|x=="virginia commonwealth"]<-"vcu"
  x[x=="brigham young"]<-"byu"
  x[x=="eastern tennesseeastern st"|x=="east tenn. st."]<-"east tennessee st"
  x[x=="unc charlotte"]<-"charlotte"
  x[x=="indiana purdue"]<-"iupui"
  x[x=="la lafayette"]<-"louisiana lafayette"
  x[x=="south utah"]<-"southern utah"
  x[x=="troy st"]<-"troy"
  x[x=="md baltimore co"]<-"umbc"
  x[x=="texas-el paso"]<-"utep"
  x[x=="tx san antonio"]<-"utsa"
  x[x=="tx pan american"]<-"texas pan american"
  x[x=="middle tenn st"]<-"middle tennessee"
  x[x=="saint mary's ca"]<-"saint mary's"
  x[x=="kent" ]<-"kent st"
  x[x=="rmu" |x=="r morris"]<-"robert morris"
  x[x=="north kentucky" ]<-"northern kentucky"
  x[x=="nc central" ]<-"north carolina central"
  x[x=="wku"]<-"western kentucky"
  x[x=="etsu"]<-"east tennessee st"
  x[x=="mtsu"]<-"middle tennessee"
  x[x=="fl gulf coast"|x=="fgcu"|x=="gulf coast"|x=="Gulf Coast"|x=="florida gulf"]<-"florida gulf coast"
  x[x=="tx southern"|x=="texas so"]<-"texas southern"
  x[x=="mt st mary's"|x=="msm"|x=="mt. st. mary's"]<-"mount st mary's"
  x[x=="il chicago"]<-"illinois chicago"
  x[x=="north iowa"|x=="n iowa"]<-"northern iowa"
  x[x=="south illinois"]<-"southern illinois"
  x[x=="american univ"|x=="american university"]<-"american"
  x[x=="north colorado"|x=="No Colorado"|x=="no colorado"]<-"northern colorado"
  x[x=="boston univ"]<-"boston university"
  x[x=="sf austin"]<-"stephen f austin"
  x[x=="southern univ"|x=="southern university"]<-"southern"
  x[x=="g washington"]<-"george washington"
  x[x=="ark pine bluff"]<-"arkansas pine bluff"
  x[x=="coastal car"]<-"coastal carolina"
  x[x=="central conn"]<-"central connecticut"
  x[x=="albany ny"]<-"albany"
  x[x=="santa barbara"]<-"uc santa barbara"
  x[x=="wisconsin milwaukee"]<-"milwaukee"
  x[x=="wisconsin green bay"]<-"green bay"
  x[x=="cal poly slo"]<-"cal poly"
  x[x=="ksu"]<-"kansas state"
  x[x=="f dickinson"]<-"fairleigh dickinson"
  x[x=="saint john's"]<-"st john's"
  x[x=="fl atlantic"|x=="fla atlantic"|x=="Fla Atlantic"]<-"florida atlantic"
  x[x=="ull"|x=="louisiana-lafayette"]<-"louisiana lafayette"
  x[x=="long island"]<-"liu brooklyn"
  x[x=="ark little rock"|x=="arkansas-little rock"]<-"arkansas little rock"
  x[x=="ms valley st"]<-"mississippi valley st"
  x[x=="arkansas-pine bluff"]<-"arkansas pine bluff"
  x[x=="ms valley st"]<-"mississippi valley st"
  x[x=="ole miss"]<-"mississippi"
  x[x=="texas-arlington"]<-"ut arlington"
  x[x=="ms valley st"]<-"mississippi valley st"
  x[x=="central connecticut st"]<-"central connecticut"
  x[x=="north carolina-wilmington"]<-"unc wilmington"
  x[x=="pennsylvania"]<-"penn"
  x[x=="ms valley st"]<-"mississippi valley st"
  x[x=="northwestern la"|x=="nw state"]<-"northwestern st"
  x[x=="north carolina state"]<-"north carolina st"
  x[x=="monmouth nj"]<-"monmouth"
  x[x=="wku"]<-"western kentucky"
  x[x=="ut san antonio"]<-"utsa"
  x[x=="se louisiana"]<-"southeastern louisiana"
  x[x=="saint joseph's pa"]<-"saint joseph's"
  x[x=="saint bonaventure"]<-"st bonaventure"
  x[x=="col charleston"]<-"college of charleston"
  x[x=="charleston so"]<-"charleston southern"
  x[x=="se missouri st"]<-"southeast missouri st"
  x[x=="north illinois"]<-"northern illinois"
  x[x=="florida intl"]<-"fiu"
  x[x=="tam c. christi"|x=="texas a&m-cc"| x=="tam c christi"| x=="texas a&m corpus chris"]<-"texas a&m corpus christi"
  x<-gsub(" state"," st", x)
  x<-gsub("\\(","", x)
  x<-gsub("\\)","", x)
  x<-gsub("[.]", "", x)
  x<-gsub("[;]", "", x)
  x[x=="north iowa"|x=="n iowa"]<-"northern iowa"
  x[x=="prairie view"]<-"prairie view a&m"
  x[x=="ulm"]<-"louisiana monroe"
  x[x== "southern methodist" ]<-"smu"
  x[x== "louisiana st"  ]<-"lsu"
  x[x=="saint mary's ca" ]<-"saint mary's"
  x[x=="albany ny"]<-"albany"
  x[x=="southern california"]<-"usc"
  x[x=="unf"]<-"north florida"
  x[x=="north carolina-asheville"|x=="unca"]<-"unc asheville"
  x[x=="uc-davis"]<-"uc davis"
  x[x=="unc"]<-"north carolina"
  x[x=="uva"]<-"virginia"
  x[x=="fsu"]<-"florida st"
  x[x=="mid tennessee"]<-"middle tennessee"
  x[x=="uri"]<-"rhode island"
  x[x=="miami"|x=="miami fla"|x=="miami (fla.)"]<-"miami fl"
  x[x=="uc-davis"]<-"uc davis"
  x<-sapply(x, simpleCap)
  
  
  x[x=="NC State"|x=="North Carolina St."|x=="North Carolina St" |x=="North Carolina State"]<-"N.C. State"
  x[x=="St. Joseph's"|x=="St Joseph's"]<-"Saint Joseph's"
  x[x=="Middle Tennessee"|x=="Middle Tenn"]<-"Middle Tennessee State"
  x[x=="Eku"]<-"Eastern Kentucky"
  x[x=="UNC"|x=="Unc"]<-"North Carolina"
  x[x=="SE Louisiana"]<-"Southeastern Louisiana"
  x[x=="Presbyterian College"]<-"Presbyterian"
  x[x=="UConn"|x=="Uconn"]<-"Connecticut"
  x[x=="North Carolina-Asheville"|x=="NC Asheville"]<-"Unc Asheville"
  x[x=="North Carolina-Greensboro"|x=="NC Greensboro"]<-"Unc Greensboro"
  x[x=="North Carolina-Wilmington"|x=="NC Wilmington"]<-"Unc Wilmington"
  x[x=="NC Central"]<-"North Carolina Central"
  x[grepl("East Tennessee St", x)]<-"East Tennessee State"
  x[x=="Virginia Commonwealth"]<-"Vcu"
  x[x=="U Mass"]<-"Massachusetts"
  x[x=="San Jos?? State"]<-"San Jose State"
  x[x=="Texas Christian"]<-"TCU"
  x[x=="Southern California"|x=="Southern Cal"]<-"Usc"
  x[x=="Nevada-Las Vegas"]<-"UNLV"
  x[x=="Saint Peters"]<-"St Peters"
  x[x=="Southern Methodist"]<-"SMU"
  x[x=="Prairie View"]<-"Prairie View A&m"
  x[x=="Albany (NY)"]<-"Albany"
  x[x=="Central Florida"]<-"Ucf"
  x[x=="Alabama-Birmingham"]<-"Uab"
  x[x=="Bowling Green State"|x=="Bowling Green St"]<-"Bowling Green"
  x[x=="Louisiana State"]<-"Lsu"
  x[x=="Southern Methodist"]<-"SMU"
  x[x=="Centenary (LA)"]<-"Centenary"
  x[x=="Maryland-Baltimore County"]<-"Umbc"
  x[x=="Southern Ill"]<-"Southern Illinois"
  x[x=="Troy State"|x=="Troy St"]<-"Troy"
  x[x=="Charleston So"]<-"Charleston Southern"
  x[x=="St Louis"|x=="St. Louis"]<-"Saint Louis"
  x[x=="Mt St Marys"|x=="Mt. St. Mary's"]<-"Mount St Marys"
  x[x=="SF Austin"|x=="Sf Austin"]<-"Stephen F Austin"
  x[x=="South Florida"]<-"USF"
  x[x=="Saint Marys Ca"|x=="Saint Mary's (CA)"]<-"St Marys"
  x[x=="Miss Valley State"]<-"Mississippi Valley State"
  x[x=="Cal"|x=="University of California"]<-"California"
  x[x=="Birm Southern"]<-"Birmingham Southern"
  x[x=="Ccu"|x=="Coastal Car"]<-"Coastal Carolina"
  x[x=="Ucr"]<-"Uc Riverside"
  x[x=="Uci"]<-"Uc Irvine"
  x[x=="Ucd"]<-"Uc Davis"
  x[x=="Nccu"]<-"North Carolina Central"
  x[x=="Csun"|x=="CS Northridge"]<-"Cal State Northridge"
  x[x=="Famu"]<-"Florida A&m"
  x[x=="Ualr"|x=="Arkansas-Little Rock"|x=="Arkansas LR"]<-"Little Rock"
  x[x=="No Colorado"]<-"Northern Colorado"
  x[x=="Ucsb"]<-"Uc Santa Barbara"
  x[x=="Csub"|x=="CS Bakersfield"]<-"Cal State Bakersfield"
  x[x=="Csuf"|x=="CS Fullerton"|x=="Csu Fullerton"]<-"Cal State Fullerton"
  x[x=="Gw"|x=="G Wash"]<-"George Washington"
  x[x=="Gmu"]<-"George Mason"
  x[x=="Ecu"]<-"East Carolina"
  x[x=="Uni"]<-"Northern Iowa"
  x[x=="Nku"]<-"Northern Kentucky"
  x[x=="Tenn Martin"|x=="Tn Martin"|x=="Ut Martin"]<-"Tennessee Martin"
  x[x=="Florida International"|x=="Florida Intl"]<-"FIU"
  x[x=="Uncw"|x=="North Carolina Wilmington"]<-"UNC-Wilmington"
  x[x=="Birmingham So"]<-"Birmingham Southern"
  x[x=="Utah Val State"|x=="Utah Val St"|x=="Utah Valley State"|x=="Utah Valley St"]<-"Utah Valley"
  x[x=="Kennesaw"]<-"Kennesaw State"
  x[x=="Sacred Ht"]<-"Sacred Heart"
  x[x=="Ut Chattanooga"]<-"Chattanooga"
  x[x=="Cent Arkansas"]<-"Central Arkansas"
  x[x=="LaSalle"|x=="Lasalle"]<-"La Salle"
  x[x=="Abilene Chr"]<-"Abilene Christian"
  x[x=="Western Salem State"|x=="Western Salem St"|x=="Winston Salem"|x=="Winston-Salem"]<-"Winston Salem State"
  x[x=="Md E Shore"|x=="MD Eastern Shore"]<-"Maryland Eastern Shore"
  x[x=="Ma Lowell"|x=="Massachusetts-Lowell"]<-"Umass Lowell"
  x[x=="South Florida"]<-"USF"
  x[x=="Miami (FL)"|x=="Miami FL"|x=="Miami Fl"|x=="Miami Florida"]<-"Miami"
  x[x=="Southern Miss"]<-"Southern Mississippi"
  x[x=="Charleston"|x=="Col Charleston"|x=="Col Of Charleston"]<-"College of Charleston"
  x[x=="Texas-Arlington"|x=="Tx Arlington"|x=="Texas Arlington"]<-"UT-Arlington"
  x[x=="Texas A&M-CC"|x=="Texas A&M Corpus Chris"|x=="Texas A&M CC"]<-"Texas A&M Corpus Christi"
  x[x=="Loyola (IL)"|x=="Loyola Il"]<-"Loyola Chicago"
  x[x=="Saint Mary's"]<-"St. Mary's"
  x[x=="American University"]<-"American"
  x[x=="Green Bay"|x=="Wisconsin Gb"]<-"Wisconsin-Green Bay"
  x[x=="Milwaukee"|x=="Wisc Milwaukee"]<-"Wisconsin-Milwaukee"
  x[x=="Uncg"]<-"Unc Greensboro"
  x[x=="Saint Peter's"]<-"St. Peter's"
  x[x=="Illinois-Chicago"|x=="Uic"|x=="Ill Chicago"]<-"Illinois Chicago"
  x[x=="Fort Wayne"]<-"IPFW"
  x[x=="Cal State Sacramento"|x=="Cal St Sacramento"]<-"Sacramento State"
  x[x=="F Dickinson"|x=="Farleigh Dickinson"]<-"Fairleigh Dickinson"
  x[x=="Penn"]<-"Pennsylvania"
  x[x=="La Monroe"]<-"Louisiana Monroe"
  x[x=="Loyola Marymt"|x=="Loy Marymount"|x=="Lmu"]<-"Loyola Marymount"
  x[x=="Miami OH"| x=="Miami Oh"|x=="Miami Ohio"]<-"Miami (OH)"
  x[x=="Loyola MD"|x=="Loyola Md"|x=="Loyola Maryland"|x=="Loyola"]<-"Loyola (MD)"
  x[x=="UMKC"|x=="Missouri Kc"|x=="Umkc"]<-"Missouri-Kansas City"
  x[x=="UTSA"]<-"Texas-San Antonio"
  x[x=="Savannah St"]<-"Savannah State"
  x[x=="Southeast Mo State"|x=="Southeast Mo St"|x=="SE Missouri St."]<-"Southeast Missouri State"
  x[x=="Southwest Missouri State"|x=="Sw Missouri State"]<-"Missouri State"
  x[x=="Southwest Texas State"]<-"Texas State"
  x[x=="Mississippi"]<-"Ole Miss"
  x[x=="Ul Lafayette"|x=="Ull"]<-"Louisiana-Lafayette"
  x[x=="Central Connecticut"]<-"Central Connecticut State"
  x[x=="VMI"|x=="Virginia Military Institute"|x=="Vmi"]<-"Virginia Military"
  x[x=="BYU"|x=="Byu"]<-"Brigham Young"
  x[x=="LIU Brooklyn"|x=="Liu Brooklyn"|x=="Long Island University"|x=="Brooklyn"]<-"Long Island"
  x[x=="Ga Southern"]<-"Georgia Southern"
  x[x=="Lbsu"]<-"Long Beach State"
  x[x=="North Arizona"]<-"Northern Arizona"
  x[x=="The Citadel"]<-"Citadel"
  x[x=="Grambling"]<-"Grambling State"
  x[x=="Edwardsville"|x=="Siue"|x=="Southern Illinois-Edwardsville"|x=="Siu Edwardsvle"]<-"Siu Edwardsville"
  x[x=="Houston Bap"]<-"Houston Baptist"
  x[x=="Nebraska Omaha"|x=="Nebraska-Omaha" |x=="Ne Omaha"]<-"Omaha"
  x[x=="UT Rio Grande Valley"|x=="Utrgv"| tolower(x)=="texas rio grande valley"|x=="UTRGV"]<-"Texas RGV"
  x[x=="Miss Valley"]<-"Mississippi Valley State"
  x[x=="St. Francis NY"|x=="St Francis NY"|x=="Saint Francis Ny"|x=="St Francis Brooklyn"|x=="Saint Francis Brooklyn"|x=="St Francis (BKN)"]<-"St Francis (NY)"
  x[x=="Bryant University"]<-"Bryant"
  x[x=="St Johns (ny)"|x=="ST Johns Ny"|x=="St. John's (NY)"]<-"St Johns"
  x[x=="South Carolina Upstate"|x=="Usc Upstate"|x=="Sc Upstate"]<-"USC Upstate"
  x[x=="Southern University"|x=="Southern U"]<-"Southern"
  x[x=="New Jersey Tech"]<-"NJIT"
  x[x=="Arkansas Little Rock"|x=="Ular"]<-"Little Rock"
  x[x=="Utsa"]<-"Texas San Antonio"
  x[x=="Texas-El Paso"]<-"Utep"
  x[x=="Detroit Mercy"]<-"Detroit"
  x[x=="Saint Johns Ny"]<-"St Johns"
  x[x=="Cal Riverside"]<-"UC Riverside"
  x[x=="Sw Texas State"|x=="Sw Texas St"]<-"Texas State"
  
  
  #get rid of symobols
  x<-gsub(", Jr.", " Jr.", x)
  x<-gsub("[.]|[']|[,]", "", x)
  x<-gsub(" Jr", "", x)
  x<-gsub(" III", "", x)
  x<-gsub(" IV", "", x)
  x<-gsub(" II", "", x)
  x<-gsub("-", " ", x)
  x<-gsub("  ", " ", x)

  x[which(endsWith(x, " St"))]<-gsub(" St", " State", x[which(endsWith(x, " St"))])
  x<-gsub("Cal St. |Cal St ", "Cal State ", x)
  x[x=="St Francis PA"|x=="Saint Francis Pa"|x=="St. Francis Pa"|x=="Saint Francis (PA)"]<-"St Francis (pa)"
  
  #redo
  x<-gsub(", Jr.", " Jr.", x)
  x<-gsub("[.]|[']|[,]", "", x)
  x<-gsub(" Jr", "", x)
  x<-gsub(" III", "", x)
  x<-gsub(" IV", "", x)
  x<-gsub(" II", "", x)
  x<-gsub("-", " ", x)
  x<-gsub("  ", " ", x)
  
  
  x<-sapply(x, simpleCap)
  x<-gsub("T J", "Tj", x)
  x[x=="NANA"]<-NA
  x[x=="Texas Rio Grande Valley"]<-"Texas Rgv"
  x[x=="St Francis Pa"]<-"St Francis (pa)"
  x[x=="Drekalo Clayton"]<-"Dre Clayton"
  x[x=="Tony Mitch"]<-"Tony Mitchell"
  x[x=="North Colorado"]<-"Northern Colorado"
  x[x=="Savannah St"]<-"Savannah State"
  x[x=="Saint Marys"|x=="St Marys Cal"]<-"St Marys"
  x[x=="Csu Bakersfield"]<-"Cal State Bakersfield"
  x[x=="Ar Little Rock"]<-"Little Rock"
  x[x=="Pitt"]<-"Pittsburgh"
  x[x=="Saint Joes"]<-"Saint Josephs"
  x[x=="Geo Washington"]<-"George Washington"
  x[x=="Coastal Caro"]<-"Coastal Carolina"
  x[x=="Texas A&m Cc"]<-"Texas A&m Corpus Christi"
  x[x=="Eastern Wash"]<-"Eastern Washington"
  x[x=="Steph F Austin"]<-"Stephen F Austin"
  x[x=="Nc Central"]<-"North Carolina Central"
  x[x=="Mich State"]<-"Michigan State"
  x[x=="Lamar University"]<-"Lamar"
  x[x=="Ut Rio Grande Valley"]<-"Texas Rgv"
  x[x=="Saint Johns"]<-"St Johns"
  x[x=="Saint Josephs Pa"]<-"Saint Josephs"
  x[x=="Saint Francis Bkn"]<-"St Francis (ny)"
  x[x=="Massachusetts Lowell"]<-"Umass Lowell"
  x[x=="Jmu"]<-"James Madison"
  x[x=="Nd State"]<-"North Dakota State"
  x[x=="Nm State"|x=="North Mexico State"]<-"New Mexico State"
  x[x=="La Lafayette"]<-"Louisiana Lafayette"
  x[x=="R Morris"]<-"Robert Morris"
  x[x=="Sd State"]<-"South Dakota State"
  x[x=="Maryland Baltimore County"]<-"Umbc"
  unname(x)
  
  
}

# 
# Massey$Team2<-tolower(Massey$Team)
# Massey$Team2<-coordName(Massey$Team2)
# setdiff(Massey$Team2[Massey$Year==2017], KenPom$Team)
# 
# setdiff(Massey$Team2[Massey$Year>=min(KenPom$Season)], KenPom$Team)
# 
# table(KenPom$Team[grepl("saint j", KenPom$Team)])
simpleCap <- function(x) {
  x<-tolower(x)
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}
Metrics<-function(y, x) {
  
  Rsq<-round(1-sum((y-x)^2)/sum((y-mean(y))^2), 4) #Rsq
  RMSE<-round(sqrt(mean((y-x)^2)), 4) #RMSE
  MAE<-round(mean(abs(y-x)), 4) #RMSE
  MAPE<-round(mean(abs((y[y!=0]-x[y!=0])/y[y!=0])*100), 4)
  bias<-round(mean(y-x), 4) #RMSE
  
  paste(c(paste("RSq:", Rsq), paste("RMSE:", RMSE),paste("bias:", bias), paste("MAE:", MAE),paste("MAPE:", MAPE) ))
}

moving<-function(x, length, operation="mean", include.current=F) { 
  if(include.current){
    x<-c(NA, x) 
  }
  
  #x<-c(2, 4, 1,2,4,1,2,4 ,4,4, 10, 1, NA, 1, 1, 1, 2 , NA,3, NA, 2);length<-10
  #x<-c(NA, NA)
  #if(operation%in% c())
  
  x2<-x
  if(operation%in% c("harmonic", "geometric")) {
    x[x<=0]<-NA
  }
  
  if(operation=="mean" & length(x)>=1) {
    y<-rep(NA, length(x))
    
    if(is.na(x[1])) {
      y[1]<-sum(x[!is.na(x)][1:min(length, length(x[!is.na(x)]))])/
        length(x[!is.na(x)][1:min(length, length(x[!is.na(x)]))])
    }
    
    
    if(length(x[!is.na(x)])>length) {
      
      #rolling mean of previous values
      y[!is.na(x)]<-append(rollmean(x[!is.na(x)], length,na.pad=TRUE, align="left" )[-1], NA) 
      
      #fill in first few games with cumulative moving ave
      y[is.na(y) & !is.na(x)]<-rev(cumsum(rev(x[is.na(y) & !is.na(x)]))-rev(x[is.na(y) & !is.na(x)]))/
        rev(seq_along(x[is.na(y) & !is.na(x)])-1) 
      
      #fill in missing games with previous game
      y<-na.locf(y, fromLast=FALSE,na.rm=FALSE) 
      
      
    } else{
      #fill in first few games with cumulative moving ave
      y[is.na(y) & !is.na(x)]<-rev(cumsum(rev(x[is.na(y) & !is.na(x)]))-rev(x[is.na(y) & !is.na(x)]))/
        rev(seq_along(x[is.na(y) & !is.na(x)])-1) 
      
      #fill in missing games with previous game
      y<-na.locf(y, fromLast=FALSE,na.rm=FALSE) 
      
    }
  } else if (operation=="median"  & length(x)>=1) {
    y<-rep(NA, length(x))
    
    if(is.na(x[1])) {
      y[1]<-median(x[!is.na(x)][1:min(length, length(x[!is.na(x)]))])
    }
    if(length(x[!is.na(x)])>length) {
      
      #rolling median of previous values
      y[!is.na(x)]<-append(rollmedian(x[!is.na(x)], length,na.pad=TRUE, align="left" )[-1], NA) 
      
      #fill in first few games with cumulative moving ave
      y[is.na(y) & !is.na(x)]<-rev(cumsum(rev(x[is.na(y) & !is.na(x)]))-rev(x[is.na(y) & !is.na(x)]))/
        rev(seq_along(x[is.na(y) & !is.na(x)])-1) 
      
      #fill in missing games with previous game
      y<-na.locf(y, fromLast=FALSE,na.rm=FALSE) 
      
      
    } else{
      #fill in first few games with cumulative moving ave
      y[is.na(y) & !is.na(x)]<-rev(cumsum(rev(x[is.na(y) & !is.na(x)]))-rev(x[is.na(y) & !is.na(x)]))/
        rev(seq_along(x[is.na(y) & !is.na(x)])-1) 
      
      #fill in missing games with previous game
      y<-na.locf(y, fromLast=FALSE,na.rm=FALSE) 
      
      
    }
  } else if (operation=="max"  & length(x)>=1) {
    #x<-c(2, 4, 1,2,4,1,2,4 ,4,4, 10, 1, NA, 1, 1, 1, 2 , NA,3, NA, 2);length<-4
    y<-rep(NA, length(x))
    if(is.na(x[1])) {
      y[1]<-max(x[!is.na(x)][1:min(length, length(x[!is.na(x)]))])
    }
    if(length(x[!is.na(x)])>length) {
      
      #rolling median of previous values
      y[!is.na(x)]<-append(rollmax(x[!is.na(x)], length,na.pad=TRUE, align="left" )[-1], NA) 
      
      #fill in first few games with cumulative moving ave
      y[is.na(y) & !is.na(x)][-length( y[is.na(y) & !is.na(x)])]<-   
        rev(cummax(rev(x[is.na(y) & !is.na(x)]))[-length(cummax(rev(x[is.na(y) & !is.na(x)])))])
      
      #fill in missing games with previous game
      y<-na.locf(y, fromLast=FALSE,na.rm=FALSE) 
      
      
    } else{
      #fill in first few games with cumulative moving ave
      y[is.na(y) & !is.na(x)][-length( y[is.na(y) & !is.na(x)])]<-   
        rev(cummax(rev(x[is.na(y) & !is.na(x)]))[-length(cummax(rev(x[is.na(y) & !is.na(x)])))])
      
      #fill in missing games with previous game
      y<-na.locf(y, fromLast=FALSE,na.rm=FALSE) 
      
      
    }
  } else if (operation=="min"  & length(x)>=1) {
    x<-(-x)
    y<-rep(NA, length(x))
    
    if(is.na(x[1])) {
      y[1]<-max(x[!is.na(x)][1:min(length, length(x[!is.na(x)]))])
    }
    if(length(x[!is.na(x)])>length) {
      
      #rolling median of previous values
      y[!is.na(x)]<-append(rollmax(x[!is.na(x)], length,na.pad=TRUE, align="left" )[-1], NA) 
      
      #fill in first few games with cumulative moving ave
      y[is.na(y) & !is.na(x)][-length( y[is.na(y) & !is.na(x)])]<-   
        rev(cummax(rev(x[is.na(y) & !is.na(x)]))[-length(cummax(rev(x[is.na(y) & !is.na(x)])))])
      
      #fill in missing games with previous game
      y<-na.locf(y, fromLast=FALSE,na.rm=FALSE) 
      
      
    } else{
      #fill in first few games with cumulative moving ave
      y[is.na(y) & !is.na(x)][-length( y[is.na(y) & !is.na(x)])]<-   
        rev(cummax(rev(x[is.na(y) & !is.na(x)]))[-length(cummax(rev(x[is.na(y) & !is.na(x)])))])
      
      #fill in missing games with previous game
      y<-na.locf(y, fromLast=FALSE,na.rm=FALSE) 
      
      
    }
    y<-(-y)
  } else if (operation =="sum"  & length(x)>=1)   { 
    y<-rep(NA, length(x))
    
    if(is.na(x[1])) {
      y[1]<-sum(x[!is.na(x)][1:min(length, length(x[!is.na(x)]))])
    }
    
    if(length(x[!is.na(x)])>length) {
      
      #rolling sum of previous values
      y[!is.na(x)]<-append(rollsum(x[!is.na(x)], length,na.pad=TRUE, align="left" )[-1], NA) 
      
      #fill in first few games with cumulative moving sum
      y[is.na(y) & !is.na(x)]<-rev(cumsum(rev(x[is.na(y) & !is.na(x)]))-rev(x[is.na(y) & !is.na(x)]))
      
      
      #fill in missing games with previous game
      y<-na.locf(y, fromLast=FALSE,na.rm=FALSE) 
      
      #fill in first game with next game if NA
      y<-na.locf(y, fromLast=TRUE,na.rm=FALSE)
      
      
    } else {
      
      #rolling mean of previous values
      #fill in first few games with cumulative moving sum
      y[is.na(y) & !is.na(x)]<-rev(cumsum(rev(x[is.na(y) & !is.na(x)]))-rev(x[is.na(y) & !is.na(x)]))
      
      
      #fill in missing games with previous game
      y<-na.locf(y, fromLast=FALSE,na.rm=FALSE) 
      
      #fill in first game with next game 
      y<-na.locf(y, fromLast=TRUE,na.rm=FALSE)
      
      
    } 
  }else if (operation =="lag"  & length(x)>=1)   { 
    y<-rep(NA, length(x))
    
    
    if(length(x[!is.na(x)])>length) {
      
      #rolling sum of previous values
      y[!is.na(x)]<-lead(x[!is.na(x)], length)
      
      
      #fill in missing games with previous game
      y<-na.locf(y, fromLast=FALSE,na.rm=FALSE) 
      
      toNA<-which(!is.na(x))[length(which(!is.na(x)))-length+1]
      y[toNA:length(y)]<-NA
      
    } 
  } else if( length(x)>=1) {
    
    y<-rep(NA, length(x))
    operationFunc<-get(operation)
    
    if(is.na(x[1])) {
      y[1]<-operationFunc(x[!is.na(x)][1:min(length, length(x[!is.na(x)]))])
    }
    if(length(x[!is.na(x)])>length) {
      
      #rolling median of previous values
      y[!is.na(x)]<-c(rollapply(x[!is.na(x)], width=length,FUN=operationFunc, fill=NA, align="left" )[-1], NA) 
      
      #fill in first few games with cumulative moving ave, rollapplyr is slower than cumsum, cummean, etc
      y[is.na(y) & !is.na(x)][-length( y[is.na(y) & !is.na(x)])]<- 
        rollapplyr(x[is.na(y) & !is.na(x)], rev(seq_along(x[is.na(y) & !is.na(x)])), operationFunc, align="left")[-1]
      
      #fill in missing games with previous game
      y<-na.locf(y, fromLast=FALSE,na.rm=FALSE) 
      
      
    } else if(!all(is.na(x))){
      #fill in first few games with cumulative moving ave
      y[is.na(y) & !is.na(x)][-length( y[is.na(y) & !is.na(x)])]<- 
        rollapplyr(x[is.na(y) & !is.na(x)], rev(seq_along(x[is.na(y) & !is.na(x)])), operationFunc, align="left")[-1]
      
      #fill in missing games with previous game
      y<-na.locf(y, fromLast=FALSE,na.rm=FALSE) 
      
      
    }
    
    y[is.na(y) & !is.na(x2)]<-0
    
  }
  if(length(x)<1) {
    y<-NA
  }
  
  #adjust beginning games, sometimes errors if multiple NAs to start off
  NA_vec<-which(is.na(x2));
  if(operation%in% c("nonZero", "geometric", "harmonic")){
    NA_vec<-which(is.na(x2)| x2==0);
  }
  still_NA<-TRUE;
  y[length(x2)]<-NA
  adjust<-length(x2)
  
  #first 2 should be NA for var-variables
  if(operation %in% c("var", "sd", "kurtosis", "skewness")){
    y[length(x2)-1]<-NA
    adjust<-length(x2)-1
  }
  while(still_NA) {
    y[adjust]<-NA
    if((adjust) %in% NA_vec) {adjust<-adjust-1} else{ still_NA<-FALSE}
    
  }
  
  if(include.current){
    y<-y[-length(y)] 
    y[length(y)]<-x[length(x)]
    if(is.na(x[1])){
      y[1]<-NA
    }
    
  }
  
  y
  
}
harmonic<-function(x) {
  harm<-1/mean(1/x[x>0], na.rm=TRUE)
  if(is.nan(harm)){harm<-NA}
  harm
}
geometric<- function(x, na.rm=TRUE){
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}
trimmed<-function(x) {
  mean(x[-c(which.min(x),which.max(x))], na.rm=T)
}
trimmed2<-function(x) {
  mean(x[-c(which.min(x))], na.rm=T)
}
quartile75<-function(x) {
  unname(quantile(x, .75, na.rm=TRUE))
}
quartile25<-function(x) {
  unname(quantile(x, .25, na.rm=TRUE))
}
nonZero<-function(x) {
  mean(x[x!=0], na.rm=TRUE)
}

