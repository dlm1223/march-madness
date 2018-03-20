#some random functions

library(geosphere)
library(gridExtra)
library(grid)
library(Rsymphony)
library(forecast);library(RSelenium);library(RCurl);library(slam);library(XML)
library(caret);library(doSNOW);library(rvest);library(robustbase);library(rpart);library(kernlab)
library(lpSolve);library(lpSolveAPI);library(data.table);library(xgboost);#library(gurobi);library(Rcplex)
library(reshape2);library(plyr);library(zoo);library(dplyr);library(gtools);library(RJSONIO)
library(ggplot2);library(glmnet);library(MASS);library(TTR) 
library(Metrics)
options(stringsAsFactors=FALSE)
options(scipen=999)


#this function is a mess..but it works..sorry!

coordName<-function(x){
  x<-trimws(x)
  x<-tolower(x)
  x<-gsub("(^\\s+)|(\\s+$)", "", x)
  x<-gsub("fla[.]", "florida", x)
  x<-gsub("ky[.]", "kentucky", x)
  x<-gsub("ala[.]", "alabama", x)
  x<-gsub("la[.]", "louisiana", x)
  x<-gsub("ariz[.]", "arizona", x)
  x<-gsub("mich[.]", "michigan", x)
  x<-gsub("ill[.]", "illinois", x)
  x<-gsub("ark[.]", "arkansas", x)
  x<-gsub("val[.]", "valley", x)
  x<-gsub("u[.]", "university", x)
  x<-gsub("tex[.]", "texas", x)
  x<-gsub("conn[.]", "connecticut", x)
  x<-gsub("colo[.]", "colorado", x)
  x<-gsub("caro[.]", "carolina", x)
  x<-gsub("n[.]c[.]", "nc", x)
  
  
  #get rid of symobols
  x<-gsub(", Jr.", " Jr.", x)
  x<-gsub("[.]|[']|[,]", "", x)
  x<-gsub(" Jr", "", x)
  x<-gsub(" III", "", x)
  x<-gsub(" IV", "", x)
  x<-gsub(" II", "", x)
  x<-gsub("-", " ", x)
  x<-gsub("  ", " ", x)
  x<-gsub("\\(","", x)
  x<-gsub("\\)","", x)
  x<-gsub("[.]", "", x)
  x<-gsub("[;]", "", x)
  x<-gsub("-", " ", x)
  x<-gsub("  ", " ", x)
  
  
  
  x[substr(x, 1, 2)=="s "]<-gsub("s ", "south ", x[substr(x, 1, 2)=="s "])
  x[substr(x, 1, 2)=="n "]<-gsub("n ", "north ", x[substr(x, 1, 2)=="n "])
  x[substr(x, 1, 2)=="e "]<-gsub("e ", "eastern ", x[substr(x, 1, 2)=="e "])
  x[substr(x, 1, 2)=="w "]<-gsub("w ", "western ", x[substr(x, 1, 2)=="w "])
  x[substr(x, 1, 2)=="c "]<-gsub("c ", "central ", x[substr(x, 1, 2)=="c "])
  x[substr(x, 1, 3)=="cs "]<-gsub("cs ", "cal st ", x[substr(x, 1, 3)=="cs "])
  x[substr(x, 1, 3)=="nc "]<-gsub("nc ", "north carolina ", x[substr(x, 1, 3)=="nc "])
  x[substr(x, 1, 3)=="st "]<-gsub("st ", "saint ", x[substr(x, 1, 3)=="st "])
  x[substr(x, 1, 3)=="wi "]<-gsub("wi ", "wisconsin ", x[substr(x, 1, 3)=="wi "])
  x[substr(x, 1, 3)=="va "]<-gsub("va ", "virginia ", x[substr(x, 1, 3)=="va "])
  x[substr(x, 1, 3)=="ga "]<-gsub("ga ", "georgia ", x[substr(x, 1, 3)=="ga "])
  x[substr(x, 1, 3)=="la "]<-gsub("la ", "louisiana ", x[substr(x, 1, 3)=="la "])
  x[substr(x, 1, 3)=="tx "]<-gsub("tx ", "texas ", x[substr(x, 1, 3)=="tx "])
  x[substr(x, 1, 3)=="tn "]<-gsub("tn ", "tennessee ", x[substr(x, 1, 3)=="tn "])
  
  x[substr(x, 1, 4)=="ark "]<-gsub("ark ", "arkansas ", x[substr(x, 1, 4)=="ark "])
  x[substr(x, 1, 5)=="alab "]<-gsub("alab ", "alabama ", x[substr(x, 1, 5)=="alab "])
  x[substr(x, 1, 5)=="wash "]<-gsub("wash ", "washington ", x[substr(x, 1, 5)=="wash "])
  
  x<-gsub("wshgtn|washingtn", "washington", x)
  x<-gsub("grn", "green", x)
  
  x[x=="va commonwealth"|x=="virginia commonwealth"]<-"vcu"
  x[x=="brigham young"]<-"byu"
  x[x=="mcneese"]<-"mcneese st"
  x[x=="seattle u"|x=="seattle university"]<-"seattle"
  x[x=="eastern tennesseeastern st"|x=="east tenn st"|x=="eastern tenn st"|x=="eastern tenn state"]<-"east tennessee st"
  x[x=="unc charlotte"|x=="north carolina charlotte"]<-"charlotte"
  x[x=="indiana purdue"]<-"iupui"
  x[x=="la lafayette"]<-"louisiana lafayette"
  x[x=="south utah"]<-"southern utah"
  x[x=="troy st"]<-"troy"
  x[x=="md baltimore co"]<-"umbc"
  x[x=="texas-el paso"]<-"utep"
  x[x=="tx san antonio"]<-"utsa"
  x[x=="tx pan american"|x=="texas pan american"|x=="texas pan am"]<-"texas pan american"
  x[x=="middle tenn st"|x=="m tenn"]<-"middle tennessee"
  x[x=="saint mary's ca"]<-"saint mary's"
  x[x=="kent" ]<-"kent st"
  x[x=="rmu" |x=="r morris"|x=="rob morris"]<-"robert morris"
  x[x=="north kentucky"|x=="northern kent" ]<-"northern kentucky"
  x[x=="nc central" ]<-"north carolina central"
  x[x=="wku"]<-"western kentucky"
  x[x=="etsu"]<-"east tennessee st"
  x[x=="mtsu"]<-"middle tennessee"
  x[x=="fl gulf coast"|x=="fgcu"|x=="gulf coast"|x=="Gulf Coast"|x=="florida gulf"|x=="fla gulf cst"]<-"florida gulf coast"
  x[x=="tx southern"|x=="texas so"]<-"texas southern"
  x[x=="mt st mary's"|x=="msm"|x=="mt. st mary's"|x=="mt st mary"|x=="mt st marys md"]<-"mount st mary's"
  x[x=="il chicago"]<-"illinois chicago"
  x[x=="north iowa"|x=="n iowa"|x=="no iowa"]<-"northern iowa"
  x[x=="south illinois"]<-"southern illinois"
  x[x=="american univ"|x=="american university"]<-"american"
  x[x=="north colorado"|x=="No Colorado"|x=="no colorado"]<-"northern colorado"
  x[x=="boston univ"]<-"boston university"
  x[x=="sf austin"|x=="sfa"|x=="ste f austin"|x=="south f austin"]<-"stephen f austin"
  x[x=="southern univ"|x=="southern university"]<-"southern"
  x[x=="g washington"]<-"george washington"
  x[x=="ark pine bluff"|x=="ar pine bluff"|x=="arkansas pine bl"]<-"arkansas pine bluff"
  x[x=="coastal car"]<-"coastal carolina"
  x[x=="central conn"]<-"central connecticut"
  x[x=="albany ny"]<-"albany"
  x[x=="santa barbara"]<-"uc santa barbara"
  x[x=="wisconsin milwaukee"|x=="wi milwkee"|x=="wisconsin milwkee"]<-"milwaukee"
  x[x=="wisconsin green bay"|x=="uw green bay"|x=="Wi Grn Bay"]<-"green bay"
  x[x=="cal poly slo"]<-"cal poly"
  x[x=="ksu"]<-"kansas state"
  x[x=="f dickinson"|x=="fair dickinson"]<-"fairleigh dickinson"
  x[x=="saint john's"]<-"st john's"
  x[x=="fl atlantic"|x=="fla atlantic"|x=="Fla Atlantic"]<-"florida atlantic"
  x[x=="ull"|x=="louisiana-lafayette"]<-"louisiana lafayette"
  x[x=="long island"]<-"liu brooklyn"
  x[x=="ark little rock"|x=="arkansas-little rock"]<-"arkansas little rock"
  x[x=="ms valley st"|x=="mississippi valley"|x=="Miss Val State"]<-"mississippi valley st"
  x[x=="arkansas-pine bluff"]<-"arkansas pine bluff"
  x[x=="ms valley st"]<-"mississippi valley st"
  x[x=="ole miss"]<-"mississippi"
  x[x=="texas-arlington"|x=="texas arlington"]<-"ut arlington"
  x[x=="ms valley st"]<-"mississippi valley st"
  x[x=="central connecticut st"]<-"central connecticut"
  x[x=="north carolina-wilmington"|x=="unc willmington"|x=="unc wilmgton"|x=="north carolina wilmgton"|x=="north carolina wilm"]<-"unc wilmington"
  x[x=="pennsylvania"|x=="u penn"]<-"penn"
  x[x=="ms valley st"]<-"mississippi valley st"
  x[x=="northwestern la"|x=="nw state"|x=="nw state"|x=="nw st"]<-"northwestern st"
  x[x=="north carolina state"]<-"north carolina st"
  x[x=="monmouth nj"]<-"monmouth"
  x[x=="western virginia"]<-"west virginia"
  x[x=="wku"]<-"western kentucky"
  x[x=="ut san antonio"]<-"utsa"
  x[x=="se louisiana"]<-"southeastern louisiana"
  x[x=="saint joseph's pa"|x=="sju pa"|x== "st joes"]<-"saint joseph's"
  x[x=="saint bonaventure"|x=="saint bonavent"]<-"st bonaventure"
  x[x=="col charleston"]<-"college of charleston"
  x[x=="charleston so"]<-"charleston southern"
  x[x=="se missouri st"]<-"southeast missouri st"
  x[x=="north illinois"]<-"northern illinois"
  x[x=="florida intl"|x=="Florida Int"]<-"fiu"
  x[x=="charl south"]<-"charleston southern"
  x[x=="grd canyon"]<-"grand canyon"
  x[x=="northeastrn"|x=="neastern"]<-"northeastern"
  x[x=="sam hous state"|x=="sam hous st"]<-"sam houston state"
  
  x[x=="xavier ohio"]<-"xavier"
  x[x=="fort wayneipfw"|x=="iupu ft wayne"]<-"ipfw"
  x[x=="saint marys cal"]<-"saint marys"
  x[x=="central floridaucf"|x=="central fl"]<-"ucf"
  x[x=="omahaneb omaha"|x=="neb omaha"]<-"omaha"
  x[x=="stony brook ny"]<-"stony brook"
  x[x=="oakland michigan"|x=="oakland mi"]<-"oakland"
  x[x=="north floridaunf"]<-"north florida"
  x[x=="kansas cityumkc"]<-"umkc"
  x[x=="njitnew jersey tech"|x=="nj tech"]<-"njit"
  x[x=="binghamton ny"]<-"binghamton"
  x[x=="umbcmd balt"]<-"umbc"
  x[x=="wis milwaukee"]<-"wisconsin milwaukee"
  x[x=="central connecticutst"]<-"central connecticut"
  x[x=="wis green bay"]<-"wisconsin green bay"
  x[x=="long island universityliu"]<-"long island"
  x[x=="ohio university"]<-"Ohio"
  x[x=="louisville9619"]<-"louisville"
  x[x=="texas san antonioutsa"]<-"texas san antonio"
  x[x=="arkansas little rockualr"]<-"arkansas little rock"
  x[x=="mvsumiss valleyst"|x=="mvsumiss valley state"|x=="mvsumis valley st"|x=="miss val st"|x=="miss val state"]<-"mississippi valley state"
  x[x=="umesmd easternshore"|x=="md eastern shoreumes"|x=="maryland es"]<-"maryland eastern shore"
  
  
  
  x[x=="tam c. christi"|x=="texas a&m-cc"| x=="tam c christi"|x=="texas a&m corpuschristi"|
      x=="texas a&m corpus chris"|x=="a&m-corpus; chris"|x=="texas am cc"]<-"texas a&m corpus christi"
  x[x=="north iowa"|x=="n iowa"]<-"northern iowa"
  x[x=="prairie view"|x=="prarie view am"]<-"prairie view a&m"
  x[x=="ulm"|x=="ul monroe"]<-"louisiana monroe"
  x[x== "southern methodist" |x=="south methodist"]<-"smu"
  x[x== "louisiana st" |x=="Louisiana state" ]<-"lsu"
  x[x=="saint mary's ca" |x=="st mary's ca"]<-"saint mary's"
  x[x=="albany ny"]<-"albany"
  x[x=="southern california"]<-"usc"
  x[x=="unf"]<-"north florida"
  x[x=="texas am"]<-"texas a&m"
  x[x=="north carolina-asheville"|x=="unca"|x=="north carolina asheville"|x=="north carolina ashev"]<-"unc asheville"
  x[x=="uc-davis"|x=="california davis"]<-"uc davis"
  x[x=="unc"]<-"north carolina"
  x[x=="uva"]<-"virginia"
  x[x=="fsu"]<-"florida st"
  x[x=="mid tennessee"]<-"middle tennessee"
  x[x=="uri"]<-"rhode island"
  x[x=="miami"|x=="miami fla"|x=="miami (fla.)"|x=="miami fl"]<-"miami"
  x[x=="uc-davis"]<-"uc davis"
  x[x=="dartmouth college"]<-"dartmouth"
  x[x=="william mary"]<-"william & mary"
  x[x=="siena college"]<-"siena"
  x[x=="north carolina at"]<-"north carolina a&t"
  
  
  x<-sapply(x, simpleCap)
  x[x=="NC State"|x=="North Carolina st"|x=="North Carolina St" |x=="North Carolina State"]<-"Nc State"
  x[x=="st Joseph's"|x=="St Joseph's"]<-"Saint Joseph's"
  x[x=="Middle Tennessee"|x=="Middle Tenn"]<-"Middle Tennessee State"
  x[x=="Eku"]<-"Eastern Kentucky"
  x[x=="UNC"|x=="Unc"]<-"North Carolina"
  x[x=="SE Louisiana"]<-"Southeastern Louisiana"
  x[x=="Presbyterian College"]<-"Presbyterian"
  x[x=="UConn"|x=="Uconn"]<-"Connecticut"
  x[x=="North Carolina-Asheville"|x=="NC Asheville"|x=="North Carolina Asheville"|x=="Asheville"]<-"Unc Asheville"
  x[x=="North Carolina-Greensboro"|x=="NC Greensboro"|x=="North Carolina Greensboro"|x=="Greensboro"|x=="Nc Grnsboro"]<-"Unc Greensboro"
  x[x=="North Carolina-Wilmington"|x=="NC Wilmington"|x=="Wilmington"]<-"Unc Wilmington"
  x[x=="NC Central"]<-"North Carolina Central"
  x[grepl("East Tennessee St", x)]<-"East Tennessee State"
  x[x=="Virginia Commonwealth"|x=="Vcuva Commonwealth"]<-"Vcu"
  x[x=="U Mass"]<-"Massachusetts"
  x[x=="San Jos?? State"]<-"San Jose State"
  x[x=="Texas Christian"]<-"Tcu"
  x[x=="Southern California"|x=="Southern Cal"]<-"Usc"
  x[x=="Nevada-Las Vegas"|x=="Nevada Las Vegas"]<-"Unlv"
  x[x=="Saint Peters"]<-"St Peters"
  x[x=="Southern Methodist"]<-"Smu"
  x[x=="Prairie View"]<-"Prairie View A&m"
  x[x=="Albany (NY)"]<-"Albany"
  x[x=="Central Florida"]<-"Ucf"
  x[x=="Alabama-Birmingham"|x=="Alabama Birmingham"]<-"Uab"
  x[x=="Bowling Green State"|x=="Bowling Green St"|x=="Bowling Grn"]<-"Bowling Green"
  x[x=="Louisiana State"]<-"Lsu"
  x[x=="Southern Methodist"]<-"SMU"
  x[x=="Centenary (LA)"|x=="Centenary La"]<-"Centenary"
  x[x=="Maryland-Baltimore County"|x=="Md Baltimore County"]<-"Umbc"
  x[x=="Southern Ill"]<-"Southern Illinois"
  x[x=="Troy State"|x=="Troy St"]<-"Troy"
  x[x=="Charleston So"]<-"Charleston Southern"
  x[x=="St Louis"|x=="st Louis"]<-"Saint Louis"
  x[x=="Mt St Marys"|x=="Mt. st Mary's"]<-"Mount St Marys"
  x[x=="SF Austin"|x=="Sf Austin"]<-"Stephen F Austin"
  x[x=="South Florida"]<-"Usf"
  x[x=="Saint Marys Ca"|x=="Saint Mary's (CA)"]<-"St Marys"
  x[x=="Miss Valley State"]<-"Mississippi Valley State"
  x[x=="Cal"|x=="University of California"]<-"California"
  x[x=="Birm Southern"|x=="Bham Southern"]<-"Birmingham Southern"
  x[x=="Ccu"|x=="Coastal Car"]<-"Coastal Carolina"
  x[x=="Ucr"|x=="California Riverside"]<-"Uc Riverside"
  x[x=="Uci"|x=="California Irvine"]<-"Uc Irvine"
  x[x=="Ucd"]<-"Uc Davis"
  x[x=="Nccu"]<-"North Carolina Central"
  x[x=="Csun"|x=="CS Northridge"|x=="Cal State Nrdge"|x=="Cal St Nrdge"]<-"Cal State Northridge"
  x[x=="Famu"|x=="florida am"]<-"Florida A&m"
  x[x=="Ualr"|x=="Arkansas-Little Rock"|x=="Arkansas LR"]<-"Little Rock"
  x[x=="No Colorado"]<-"Northern Colorado"
  x[x=="Ucsb"|x=="California Santa Barbara"]<-"Uc Santa Barbara"
  x[x=="Csub"|x=="CS Bakersfield"|x=="Bakersfield"|x=="Cal State Bakersfld"|x=="Cal St Bakersfld"|x=="cal state bak"]<-"Cal State Bakersfield"
  x[x=="Csuf"|x=="CS Fullerton"|x=="Csu Fullerton"]<-"Cal State Fullerton"
  x[x=="Gw"|x=="G Wash"|x=="Geo Washington"|x=="Geo Wshgtn"]<-"George Washington"
  x[x=="Gmu"|x=="Geo Mason"]<-"George Mason"
  x[x=="Ecu"|x=="Eastern Carolina"]<-"East Carolina"
  x[x=="Uni"]<-"Northern Iowa"
  x[x=="Nku"]<-"Northern Kentucky"
  x[x=="Suny Buffalo"]<-"Buffalo"
  x[x=="Tenn Martin"|x=="Tn Martin"|x=="Ut Martin"]<-"Tennessee Martin"
  x[x=="Florida International"|x=="Florida Intl"|x=="Florida Int"]<-"FIU"
  x[x=="Uncw"|x=="North Carolina Wilmington"]<-"UNC Wilmington"
  x[x=="Birmingham So"]<-"Birmingham Southern"
  x[x=="Utah Val State"|x=="Utah Val St"|x=="Utah Valley State"|x=="Utah Valley St"]<-"Utah Valley"
  x[x=="Kennesaw"]<-"Kennesaw State"
  x[x=="Austin Peay State"|x=="Austin Peay St"]<-"Austin Peay"
  x[x=="Sacred Ht"]<-"Sacred Heart"
  x[x=="Youngs State"|x=="Youngs St"]<-"Youngstown State"
  x[x=="Ut Chattanooga"|x=="Ut Chatt"|x=="Tennessee Chattanooga"]<-"Chattanooga"
  x[x=="Cent Arkansas"|x=="Central Ark"]<-"Central Arkansas"
  x[x=="LaSalle"|x=="Lasalle"|x=="Louisiana Salle"]<-"La Salle"
  x[x=="Abilene Chr"|x=="Abl Christian"]<-"Abilene Christian"
  x[x=="Western Salem State"|x=="Western Salem St"|x=="Winston Salem"|x=="Winston-Salem"|x=="Wins Salem"]<-"Winston Salem State"
  x[x=="Md E Shore"|x=="Md Eastern Shore"|x=="Umes"|x=="Md East Shore"]<-"Maryland Eastern Shore"
  x[x=="Ma Lowell"|x=="Massachusetts-Lowell"|x=="Mass Lowell"]<-"Umass Lowell"
  x[x=="Alcorn"]<-"Alcorn State"
  x[x=="Towson State"|x=="Towson St"]<-"Towson"
  x[x=="Umass"]<-"Massachusetts"
  x[x=="George Wash"]<-"George Washington"
  x[x=="Miami (FL)"|x=="Miami FL"|x=="Miami Fl"|x=="Miami Florida"]<-"Miami"
  x[x=="Southern Miss"|x=="South Mississippi"]<-"Southern Mississippi"
  x[x=="Charleston"|x=="Col Charleston"|x=="Col Of Charleston"|x=="Col Charlestn"]<-"College Of Charleston"
  x[x=="Texas-Arlington"|x=="Tx Arlington"|x=="Texas Arlington"]<-"UT-Arlington"
  x[x=="Texas A&M-CC"|x=="Texas A&M Corpus Chris"|x=="Texas A&M CC"|x=="A&M Corpus Christi"|x=="A&m Corpus Chris"]<-"Texas A&M Corpus Christi"
  x[x=="Loyola (IL)"|x=="Loyola Il"|x=="Loyola Chi"|x=="Loyola Illinois"]<-"Loyola Chicago"
  x[x=="Saint Mary's"|x=="St Marys Ca"]<-"st Marys"
  x[x=="American University"]<-"American"
  x[x=="Central Mich"]<-"Central Michigan"
  x[x=="Green Bay"|x=="Wisconsin Gb"]<-"Wisconsin-Green Bay"
  x[x=="Milwaukee"|x=="Wisc Milwaukee"|x=="Uw Milwaukee"]<-"Wisconsin-Milwaukee"
  x[x=="Uncg"]<-"Unc Greensboro"
  x[x=="Saint Peter's"]<-"st Peter's"
  x[x=="Illinois-Chicago"|x=="Uic"|x=="Ill Chicago"]<-"Illinois Chicago"
  x[x=="Fort Wayne"|x=="Iupu Fort Wayne"]<-"IPFW"
  x[x=="Cal State Sacramento"|x=="Cal St Sacramento"|x=="Sac State"|x=="California State Sacramento"]<-"Sacramento State"
  x[x=="F Dickinson"|x=="Farleigh Dickinson"]<-"Fairleigh Dickinson"
  x[x=="Penn"]<-"Pennsylvania"
  x[x=="La Monroe"]<-"Louisiana Monroe"
  x[x=="Loyola Marymt"|x=="Loy Marymount"|x=="Lmu"|x=="Loyola Mymt"]<-"Loyola Marymount"
  x[x=="Miami OH"| x=="Miami Oh"|x=="Miami Ohio"]<-"Miami Oh"
  x[x=="Loyola MD"|x=="Loyola Md"|x=="Loyola Maryland"|x=="Loyola"]<-"Loyola (MD)"
  x[x=="UMKC"|x=="Missouri Kc"|x=="Umkc"]<-"Missouri Kansas City"
  x[x=="UTSA"]<-"Texas-San Antonio"
  x[x=="Savannah St"]<-"Savannah State"
  x[x=="Southeast Mo State"|x=="Southeast Mo St"|x=="SE Missouri st"| x=="Se Missouri Statesemo"|
      x=="Se Missouri"|x=="Se Missouri St"|x=="Se Missouri State"]<-"Southeast Missouri State"
  x[x=="Southwest Missouri State"|x=="Sw Missouri State"]<-"Missouri State"
  x[x=="Southwest Texas State"]<-"Texas State"
  x[x=="Mississippi"]<-"Ole Miss"
  x[x=="Ul Lafayette"|x=="Ull"]<-"Louisiana-Lafayette"
  x[x=="Central Connecticut"|x=="Cent Connecticut State"|x=="Cent Connecticut St"]<-"Central Connecticut State"
  x[x=="VMI"|x=="Virginia Military Institute"|x=="Vmi"|x=="Virginia Military Inst"]<-"Virginia Military"
  x[x=="BYU"|x=="Byu"]<-"Brigham Young"
  x[x=="LIU Brooklyn"|x=="Liu Brooklyn"|x=="Long Island University"]<-"Long Island"
  x[x=="Ga Southern"]<-"Georgia Southern"
  x[x=="Lbsu"|x=="Lg Beach State"|x=="Long Beach St"|x=="Lg Beach St"|x=="Cal State Long Beach"|x=="California St Long Beach"]<-"Long Beach State"
  x[x=="North Arizona"]<-"Northern Arizona"
  x[x=="Army West Point"]<-"Army"
  x[x=="Rice University"]<-"Rice"
  x[x=="The Citadel"]<-"Citadel"
  x[x=="North Hampshire"]<-"New Hampshire"
  x[x=="Grambling"]<-"Grambling State"
  x[x=="Edwardsville"|x=="Siue"|x=="Southern Illinois-Edwardsville"|x=="Siu Edwardsvle"|x=="Siu Edward"]<-"Siu Edwardsville"
  x[x=="Houston Bap"]<-"Houston Baptist"
  x[x=="Nebraska Omaha"|x=="Nebraska-Omaha" |x=="Ne Omaha"]<-"Omaha"
  x[x=="UT Rio Grande Valley"|x=="Utrgv"| tolower(x)=="texas rio grande valley"|x=="UTRGV"|x=="Rio Grande"]<-"Texas RGV"
  x[x=="Miss Valley"]<-"Mississippi Valley State"
  x[x=="st Francis NY"|x=="St Francis NY"|x=="Saint Francis Ny"|x=="St Francis Ny"|x=="Saint Fran Ny"|
      x=="St Francis Brooklyn"|x=="Saint Francis Brooklyn"|x=="St Francis (BKN)"]<-"St Francis (NY)"
  x[x=="Bryant University"]<-"Bryant"
  x[x=="St Johns (ny)"|x=="ST Johns Ny"|x=="st John's (NY)"]<-"St Johns"
  x[x=="South Carolina Upstate"|x=="Usc Upstate"|x=="Sc Upstate"]<-"USC Upstate"
  x[x=="Southern University"|x=="Southern U"]<-"Southern"
  x[x=="New Jersey Tech"|x=="Nj Inst of Technology"]<-"NJIT"
  x[x=="Arkansas Little Rock"|x=="Ular"|x=="Ar Lit Rock"]<-"Little Rock"
  x[x=="Utsa"|x=="Texas San Ant"]<-"Texas San Antonio"
  x[x=="Texas-El Paso"|x=="Texas El Paso"|x=="Tx El Paso"]<-"Utep"
  x[x=="Detroit Mercy"]<-"Detroit"
  x[x=="Saint Johns Ny"]<-"St Johns"
  x[x=="Cal Riverside"]<-"UC Riverside"
  x[x=="Sw Texas State"|x=="Sw Texas St"]<-"Texas State"
  x[x=="Beth Cook"]<-"Bethune Cookman"
  x[x=="Sacred Hrt"]<-"Sacred Heart"
  x[x=="Jksnville State"|x=="Jksnville St"]<-"Jacksonville State"
  
  
  x[which(endsWith(x, " St"))]<-gsub(" St", " State", x[which(endsWith(x, " St"))])
  x[which(endsWith(x, " U"))]<-gsub(" U", " University", x[which(endsWith(x, " U"))])
  x[which(endsWith(x, " Col"))]<-gsub(" Col", " College", x[which(endsWith(x, " Col"))])
  x<-gsub("Cal st |Cal St ", "Cal State ", x)
  x[x=="St Francis PA"|x=="Saint Francis Pa"|x=="st Francis Pa"|x=="Saint Francis (PA)"|x=="Saint Fran Pa"]<-"St Francis (pa)"
  
  
  x[x=="NANA"]<-NA
  x[x=="Sc State"|x=="South Car State"]<-"South Carolina State"
  x[x=="Texas Rio Grande Valley"]<-"Texas Rgv"
  x[x=="St Francis Pa"]<-"St Francis (pa)"
  x[x=="North Colorado"]<-"Northern Colorado"
  x[x=="Savannah St"]<-"Savannah State"
  x[x=="Saint Marys"|x=="St Marys Cal"|x=="Saint Marys College California"]<-"St Marys"
  x[x=="Csu Bakersfield"|x=="Cal State Bak"]<-"Cal State Bakersfield"
  x[x=="Ar Little Rock"]<-"Little Rock"
  x[x=="Pitt"]<-"Pittsburgh"
  x[x=="Saint Joes"|x=="Saint Josephs Pennsylvania"]<-"Saint Josephs"
  x[x=="Geo Washington"]<-"George Washington"
  x[x=="Coastal Caro"]<-"Coastal Carolina"
  x[x=="Texas A&m Cc"|x=="Texas A&m Corpus Chris"]<-"Texas A&m Corpus Christi"
  x[x=="Eastern Wash"]<-"Eastern Washington"
  x[x=="Steph F Austin"|x=="Stephen Faustin"]<-"Stephen F Austin"
  x[x=="Nc Central"]<-"North Carolina Central"
  x[x=="Mich State"]<-"Michigan State"
  x[x=="Lamar University"]<-"Lamar"
  x[x=="Ut Rio Grande Valley"]<-"Texas Rgv"
  x[x=="Wm & Mary"]<-"William & Mary"
  x[x=="Saint Johns"]<-"St Johns"
  x[x=="Saint Josephs Pa"]<-"Saint Josephs"
  x[x=="Saint Francis Bkn"|x=="st francis brk"]<-"St Francis (ny)"
  x[x=="Massachusetts Lowell"]<-"Umass Lowell"
  x[x=="Jmu"|x=="James Mad"]<-"James Madison"
  x[x=="App State"]<-"Appalachian State"
  x[x=="So Carolina"]<-"South Carolina"
  x[x=="No Carolina"]<-"North Carolina"
  x[x=="Nwestern"]<-"Northwestern"
  x[x=="Fla Gulf"]<-"Florida Gulf Coast"
  x[x=="No Dakota"]<-"North Dakota"
  x[x=="Rhode Isl"]<-"Rhode Island"
  x[x=="Okla State"]<-"Oklahoma State"
  x[x=="Jax State"]<-"Jacksonville State"
  x[x=="Gard Webb"]<-"Gardner Webb"
  x[x=="Incar Word"]<-"Incarnate Word"
  x[x=="Nd State"]<-"North Dakota State"
  x[x=="Nm State"|x=="North Mexico State"|x=="North Mex St"|x=="North Mex State"]<-"New Mexico State"
  x[x=="La Lafayette"]<-"Louisiana Lafayette"
  x[x=="R Morris"]<-"Robert Morris"
  x[x=="South Diego State"]<-"San Diego State"
  x[x=="Sd State"]<-"South Dakota State"
  x[x=="Maryland Baltimore County"|x=="Maryland Bc"]<-"Umbc"
  x[x=="Tx San Antonio"]<-"Texas San Antonio"
  x[x=="Texas Arlington"]<-"Ut Arlington"
  x[x=="Ar Pine Bluff"|x=="Uapb"]<-"Arkansas Pine Bluff"
  x[x=="New Jersey Tech"|x=="Nj Inst Of Technology"]<-"Njit"
  x[x=="St Johns Ny"]<-"St Johns"
  x[x=="Louisiana"]<-"Louisiana Lafayette"
  x[x=="Va Commonwealth"]<-"Vcu"
  x[x=="A&m Corpus Christi"]<-"Texas A&m Corpus Christi"
  x[x=="Tenn Martin"]<-"Tennessee Martin"
  x[x=="Csu Northridge"]<-"Cal State Northridge"
  x[x=="Miss State"]<-"Mississippi State"
  x[x=="Arkansas Little Rock"|x=="Arkansas Lr"]<-"Little Rock"
  x[x=="Osu"]<-"Ohio State"
  x[x=="St Francis Brk"]<-"St Francis (NY)"
  x<-gsub("California State", "Cal State", x)
  x[x=="Nw State"]<-"Northwestern State"
  x[x=="St Josephs"]<-"Saint Josephs"
  x[x=="San Fransco"]<-"San Francisco"
  x[x=="Mvsumiss Valley State"]<-"Mississippi Valley State"
  x[x=="Cal State Long Beach"]<-"Long Beach State"
  x[x=="Texas Rio Grande"]<-"Texas Rgv"
  
  #redo get rid of symobols
  x<-gsub(", Jr.", " Jr.", x)
  x<-gsub("[.]|[']|[,]", "", x)
  x<-gsub(" Jr", "", x)
  x<-gsub(" III", "", x)
  x<-gsub(" IV", "", x)
  x<-gsub(" II", "", x)
  x<-gsub("-", " ", x)
  x<-gsub("  ", " ", x)
  x<-gsub("\\(","", x)
  x<-gsub("\\)","", x)
  x<-gsub("[.]", "", x)
  x<-gsub("[;]", "", x)
  x<-gsub("-", " ", x)
  x<-gsub("  ", " ", x)
  x<-unname(x)
  sapply(x, simpleCap)
  
}
# 
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

