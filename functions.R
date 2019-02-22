
library(lubridate)
library(geosphere)
library(Rsymphony)
library(forecast);library(RCurl);library(slam);library(XML);library(RSelenium);
library(caret);library(doSNOW);library(rvest);library(robustbase);library(rpart);library(kernlab)
library(lpSolve);library(data.table);
library(reshape2);library(plyr);library(zoo);library(dplyr);library(RJSONIO)
library(ggplot2);library(glmnet);library(MASS);library(TTR) 
library(Metrics)
options(stringsAsFactors=FALSE)
options(scipen=999)

#https://stackoverflow.com/questions/20495598/replace-accented-characters-in-r-with-non-accented-counterpart-utf-8-encoding

unwanted_array <- list(    'Š'='S', 'š'='s', 'Ž'='Z', 'ž'='z', 'À'='A', 'Á'='A', 'Â'='A', 'Ã'='A', 'Ä'='A', 'Å'='A', 'Æ'='A', 'Ç'='C', 'È'='E', 'É'='E',
                           'Ê'='E', 'Ë'='E', 'Ì'='I', 'Í'='I', 'Î'='I', 'Ï'='I', 'Ñ'='N', 'Ò'='O', 'Ó'='O', 'Ô'='O', 'Õ'='O', 'Ö'='O', 'Ø'='O', 'Ù'='U',
                           'Ú'='U', 'Û'='U', 'Ü'='U', 'Ý'='Y', 'Þ'='B', 'ß'='Ss', 'à'='a', 'á'='a', 'â'='a', 'ã'='a', 'ä'='a', 'å'='a', 'æ'='a', 'ç'='c',
                           'è'='e', 'é'='e', 'ê'='e', 'ë'='e', 'ì'='i', '??'='i', 'î'='i', 'ï'='i', 'ð'='o', 'ñ'='n', 'ò'='o', 'ó'='o', 'ô'='o', 'õ'='o',
                           'ö'='o', 'ø'='o', 'ù'='u', 'ú'='u', 'û'='u', 'ý'='y', 'ý'='y', 'é'='e')
coordName<-function(x){
  
  x<-chartr(paste(names(unwanted_array), collapse=''),
            paste(unwanted_array, collapse=''),
            x)
  
  #get rid of symobols
  x<-gsub("\\s+$|^\\s+", "", x)
  x<-gsub(", Jr.", " Jr.", x)
  x<-gsub("[.]|[']|[,]|[’]|[?]", "", x)
  x<-gsub(" Jr", "", x)
  x<-gsub(" III", "", x)
  x<-gsub(" IV", "", x)
  x<-gsub(" II", "", x)
  x<-gsub("-", " ", x)
  x<-gsub("  ", " ", x)
  x<-gsub("[*]", "", x)
  
  x<-gsub("R J ", "RJ ", x)
  x<-gsub("J J ", "JJ ", x)
  x<-gsub("C J ", "CJ ", x)
  x<-gsub("B J ", "BJ ", x)
  x<-gsub("O J ", "OJ ", x)
  x<-gsub("D J ", "DJ ", x)
  x<-gsub("J P ", "JP ", x)
  
  x[x=="Jacob Wiley"]<-"Jake Wiley"
  x[grepl("Adnan Hodzi", x)]<-"Adnan Hodzic"
  x[grepl("Stefan Jankovi", x)]<-"Stefan Jankovic"
  x[x=="Josy Juan Barea"]<-"JJ Barea"
  x[x=="Attarius Norwood"]<-"Attarrius Norwood"
  x[x=="Orlando Mendez Valdez"]<-"Orlando Mendez"
  x[x=="Keydren Clark"]<-"Kee Kee Clark"
  x[x=="Norm Richardson"]<-"Norman Richardson"
  x[x=="Abdul-Malik Abu"]<-"Abdul Malik Abu"
  x[x=="Byron Mullens"]<-"BJ Mullens"
  x[x=="James McAdoo"]<-"James Michael McAdoo"
  x[x=="Kelly Oubre, Jr."]<-"Kelly Oubre"
  x[x=="Le'Bryan Nash"]<-"LeBryan Nash"
  x[x=="Tiny Gallon"]<-"Keith Gallon"
  x[x=="Fabricio de Melo"|x=="Fabricio Melo"]<-"Fab Melo"
  x[x=="Jose Manuel Calderon"]<-"Jose Calderon"
  x[x=="Alexsander Vujacic"]<-"Sasha Vujacic"
  x[grepl("l Gelabale", x)]<-"Mickael Gelabale"
  x[x=="J J Hickson"]<-"JJ Hickson"
  x[x=="C J Miles"]<-"CJ Miles"
  x[x=="Sagana Diop"]<-"DeSagana Diop"
  x[x=="Mengke Bateer"]<-"Bateer Mengke"
  x[x=="Erik Jay Murphy"]<-"Erik Murphy"
  x[x=="Sheldon Mac"]<-"Sheldon Mcclellan"
  x[x=="Cliff Robinson"]<-"Clifford Robinson"
  x[x=="Flip Murray"]<-"Ronald Murray"
  x[x=="Efthimi Rentzias"]<-"Efthimios Rentzias"
  x[x=="Ruben Boumtje"]<-"Ruben Boumtje-Boumtje"
  x[x=="Maybyner Nene"]<-"Nene Hilario"
  x[grepl("Maybyner", x)& grepl("Nene", x)]<-"Nene Hilario"
  x[x=="Predrag Drobnjak"]<-"Peja Drobnjak"
  x[x=="Anfernee Hardaway"]<-"Penny Hardaway"
  x[x=="Slava Medvedenko"]<-"Stanislav Medvedenko"
  x[x=="Charles Cornelius Smith"]<-"Charles Smith"
  x[x=="Alek Radojevic"]<-"Aleksandar Radojevic"
  x[x=="Wang Zhizhi"]<-"Zhizhi Wang"
  x[x=="DJ Mbenga"]<-"Didier Ilunga-Mbenga"
  x[x=="Frankie Williams"]<-"Frank Williams"
  x[x=="Pooh Jeter"]<-"Eugene Jeter"
  x[x=="Luigi Datome"]<-"Gigi Datome"
  x[x=="Marcelo Huertas"]<-"Marcelinho Huertas"
  x[x=="Ognjen Kuzmic"]<-"Ognen Kuzmic"
  x[x=="Taurean Waller Prince"]<-"Taurean Prince"
  x[x=="Randy Holcomb"]<-"Raed Farid Elhamali"
  x[x=="Anthony Miller"]<-"Pig Miller"
  x[x=="Carldell Johnson"]<-"Squeaky Johnson"
  x[x=="Ibrahim Kutluay"]<-"Ibo Kutluay"
  x[x=="Randy Holcomb"]<-"Raed Farid Elhamali"
  
  x[x=="J R Giddens"]<-"JR Giddens"
  x[x=="Alexsander Vujacic"]<-"Sasha Vujacic"
  x[x=="Oluwafemi Ibikunle"]<-"Femi Ibikunle"
  x[x=="Carlos Lopez Sosa"]<-"Carlos Lopez"
  x[x=="Youssoupha Mbao"]<-"Yous Mbao"
  x[x=="Adrien Coleman"]<-"Adrian Coleman"
  x[x=="Adonis Delarosa"|x=="Joe De La Rosa"]<-"Joey De La Rosa"
  x[x=="Svi Mykhailiuk"]<-"Sviatoslav Mykhailiuk"
  x[x=="Sheldon Mclellan"]<-"Sheldon Mcclellan"
  x[x=="Trevon Blueitt"]<-"Trevon Bluiett"
  x<-gsub("Joshua ", "Josh ", x)
  x<-gsub("Boubecar ", "Boubacar ", x)
  x<-gsub("Gabe ", "Gabriel ", x)
  x<-gsub("Scottie |Scotty ", "Scott ", x)
  x<-gsub("Gabe ", "Gabriel ", x)
  x<-gsub("Rayshawn |Raymond |Rayes ", "Ray ", x)
  x<-gsub("SirDominic |Dominic |Dominitrix |Dominique ", "Dom ", x)
  x<-gsub("Langendoerfer", "Langendoefer", x)
  x<-gsub("Mohamed", "Mohammed", x)
  x<-gsub("JaQuan", "Jaquan", x)
  
  x<-gsub("Demarcus |DaMarcus |Damarcus ", "DeMarcus ", x)
  x<-gsub("Dwan ", "Dwon ", x)
  x<-gsub("Jamal |Jaamal ", "Jamaal ", x)
  x<-gsub("Octavious ", "Octavius ", x)
  x<-gsub("Malcom", "Malcolm", x)
  x<-gsub("Darrington ", "Darington ", x)
  x<-gsub("Kenechukwu ", "Kene ", x)
  x<-gsub("Gene |Geno ", "Eugene ", x)
  x<-gsub("Rasheen ", "Rasheem ", x)
  x<-gsub("Lew ", "Lewis ", x)
  x<-gsub("Hasaan ", "Hassan ", x)
  
  x[x=="Kahlil Ahmad"]<-"Khalil Ahmad"
  x[x=="Derek Funderburk"]<-"Dj Funderburk"
  x[x=="Rechon Black"]<-"Leaky Black"
  x[x=="Tamell Pearson"]<-"Tamell Peason"
  x[x=="Deshang Williams Weaver"| grepl("g Williams Weaver", x)]<-"Dj Weaver"
  x[x=="Ty Mosely"]<-"Tyronn Mosley"
  x[x=="Aristide Boya"]<-'Ari Boya'
  x[x=="Rodgerick Brown"]<-"Rod Brown"
  x[x=="Ezekiel Richards"]<-"Zeke Richards"
  x[x=="Shaun Williams"]<-"Shaun Neal Williams"
  x[x=="Jermaine Couisnard"]<-"Jermaine Cousinard"
  x[x=="Sam Froling"]<-"Samson Froling"
  x[x=="Torrence Watson"]<-"Torrance Watson"
  x[x=="Jake Forrester"]<-"Jakob Forrester"
  x[x=="Immanuel Bates"]<-"Manny Bates"
  x[x=="Luguentz Dort"]<-"Lugentz Dort"
  x[x=="Nazreon Reid"]<-"Naz Reid"
  x[x=="Simisola Shittu"]<-"Simi Shittu"
  x[x=="Reggie Garnder"]<-"Reggie Gardner"
  x[x=="Karlis Silis"]<-"Karlis Silins"
  x[x=="Domen Omladi"]<-"Domen Omladic"
  x[x=="Abdulsalam Sarki"]<-"Abdul Sarki"
  x[x=="Elijah Gonzalez"]<-"Elijah Gonzales"
  x[x=="Anthony Perez Cortesia"]<-"Anthony Perez"
  x[x=="Johan Van Zegeren"]<-"Joey van Zegeren"
  x[x=="Jamie Crockett"]<-"Jamee Crockett"
  x[x=="Charles Gavin"]<-"Chuck Gavin"
  x[x=="Achraf Yacoubou"]<-"Ash Yacoubou"
  x[x=="Darwin Davis"]<-"Dee Davis"
  x[x=="Sergey Lishchuk"]<-"Sergei Lishouk"
  x[x=="Sergey Monya"]<-"Sergei Monia"
  x[x=="Louis Amundson"]<-"Lou Amundson"
  x[x=="Denis Alibegovi"]<-"Denis Alibegovic"
  x[x=="Deonte Burton Nv"]<-"Deonte Burton"
  x[x=="Gary Payton Ii"]<-"Gary Payton"
  x[x=="Robert Hubbs III"]<-"Robert Hubbs"
  x[x=="Glenn Robinson Iii"]<-"Glenn Robinson"
  x[x=="Derek Semanas"]<-"Derek Semenas"
  x[x=="Collin Reddick"]<-"Colin Reddick"
  x[x=="Jmison Morgan"]<-"JMison Morgan"
  x[x=="Isaac Austin"]<-"Ike Austin"
  x[x=="Isaac Fontaine"]<-"Ike Fontaine"
  x[x=="Mo Bamba"]<-"Mohammed Bamba"
  x[x=="Moe Wagner"]<-"Moritz Wagner"
  x[x=="Sheldon Mclellan"]<-"Sheldon Mac"
  x[x=="Isaiah Rider"]<-"Jr Rider"
  x[x=="Robert Hubbs III"]<-"Robert Hubbs"
  x[x=="Bawa Muniri"| x=="Bawa Muniru"|x=="Muniri Bawa"]<-"Muniru Bawa"
  x[x=="Rich McBride"]<-"Richard McBride"
  x[x=="Cat Barber"]<-"Anthony Barber"
  x[x=="Alter Gilbert"]<-"Alterique Gilbert"
  x[x=="Tiny Gallon"]<-"Keith Gallon"
  x[x=="Mike Dunigan"]<-"Michael Dunigan"
  x[grepl("Corperryale", x) &grepl("Harris", x)]<-"Manny Harris"
  x[x=="Mike Dunigan"]<-"Michael Dunigan"
  x[x=="Cameron Biedscheid"]<-"Cam Biedscheid"
  x[x=="Wink Adams"]<-"Jovan Adams"
  x[x=="Ky Madden"]<-"Rashad Madden"
  x[x=="Marquise Kately"]<-"Marquis Kately"
  x[x=="Drew Lavender"]<-"Andrew Lavender"
  x[x=="Chris Douglas-Roberts"|grepl("Douglas Roberts", x)]<-"Chris Douglas Roberts"
  x[x=="Brenton Petway"]<-"Brent Petway"
  x[x=="Andrew White III"]<-"Andrew White"
  x[x=="M Kidd Gilchrist"|x=="Michael Gilchrist"]<-"Michael Kidd Gilchrist"
  x[x=="Ricardo Ledo"]<-"Ricky Ledo"
  x[x=="K Caldwell Pope"]<-"Kentavious Caldwell Pope"
  x[x=="C Douglas Roberts"]<-"Chris Douglas Roberts"
  x[x=="M Carter Williams"]<-"Michael Carter Williams"
  x[x=="R Hollis Jefferson"]<-"Rondae Hollis Jefferson"
  x[x=="Nick Stauskas"]<-"Nik Stauskas"
  x[x=="Pierre Niles"]<-"Pierre Henderson"
  x[x=="Bill Walker"]<-"Henry Walker"
  x[x=="Jalyn Pennie"]<-"Jayln Pennie"
  x[x=="Augustus Gilchrist"]<-"Gus Gilchrist"
  x[x=="Augustin Rubit"]<-"Augustine Rubit"
  x[x=="Juan Toscano"]<-"Juan Anderson"
  x[x=="Akini Akini"]<-"Akini Adkins"
  x[x=="Kevin Sam Hunt"]<-"Sam Hunt"
  x[x=="Isaiah Grayson"]<-"Issiah Grayson"
  x[x=="JT Miller"]<-"James Miller"
  x[grepl("Marko Pirovi", x)]<-"Marko Pirovic"
  x[x=="Chukwudubem Duby Okeke"]<-"Duby Okeke"
  x[x=="Kendell Ramlal"]<-"Kendall Ramlal"
  x[x=="Andre Gillette"]<-"Scooter Gillette"
  x[x=="Rahlir Jefferson"]<-"Rahlir Hollis Jefferson"
  x[x=="Jontaveous Sulton"]<-"JT Sulton"
  x[x=="CJ Jackson"|x=="Cj Jackson"]<-"Charles Jackson"
  x[x=="Joseph Young"]<-"Joe Young"
  x[x=="Alejandro Carmona Sanchez"]<-"Alex Carmona"
  x[x=="Will Tchiengang"|x=="William Tchiengang"]<-"William Tchiengang Tanko"
  x[x=="Nuwriyl Williams"]<-"New Williams"
  x[x=="Snap Peters"]<-"Brandon Peters"
  x[x=="Royce O'Neal"|x=="Royce ONeal"]<-"Royce O'Neale"
  x[grepl("Kissoonlal", x)]<-"Reginald Kissoonlal"
  x[grepl("Lukas Berg", x)]<-"Lukas Bergang"
  x[x=="Karl Towns"]<-"Karl Anthony Towns"
  x[x=="Demond Carter"]<-"Tweety Carter"
  x[x=="Ping Shang"]<-"Shang Ping"
  x[x=="Paul Jorgensen"]<-"Paul Jorgenson"
  x[x=="Michael Milligan"]<-"Boo Milligan"
  x[x=="Kristiyan Stavrev"]<-"Kiko Stavrev"
  x[x=="Harrison Mackenzie Smith"]<-"Harrison Smith"
  x[x=="Vasilije Pusica"]<-"Vasa Pusica"
  x[x=="Bryant Searcy"]<-"Junior Searcy"
  x[x=="Cleveland Thomas"]<-"Pancake Thomas"
  x[x=="Shaun Willett"]<-"Shawn Willett"
  x[x=="Nikola Scekic"]<-"Nikola Scekic"
  x[x=="Elvar Friiriksson"]<-"Elvar Fridriksson"
  x[x=="Joell Hopkins"]<-"Joel Hopkins"
  x[x=="Antonio Toplyn"]<-"Tony Toplyn"
  x[x=="Jere Vucica"]<-"Jere Vucica"
  x[x=="Alex Shepard"]<-"Alex Sheppard"
  x[x=="Babajide Aina"]<-"Babajidne Aina"
  x[x=="Cory Arentsen"]<-"Corey Arentsen"
  x[x=="Quinten Bastian"]<-"Quentin Bastian"
  x[x=="Ude Ifeanyichukwu"]<-"Ude Ifeanyichukwa"
  x[x=="Deonta Jethroe"]<-"Dj Jethroe"
  
  x[x=="Daouda Berete"]<-"David Berete"
  x[x=="Aaryngston Bibens"]<-"Aary Bibens"
  x[x=="Burroughs Cook"]<-"Johnathan Cook"
  x[x=="Kermal Dincer"]<-"Kemal Dincer"
  x[x=="Terence Johnson"]<-"Terrance Johnson"
  x[x=="Andrew Smeathers"]<-"Andy Smeathers"
  x[x=="Torlof Thomas"]<-"Torlorf Thomas"
  x[x=="Hidde Vos"]<-"Hidde Arjans Vos"
  x[x=="Prince Arceneaux"]<-"Prince Arceneux"
  x[x=="Clayton Cowell"]<-"Dom Cowell"
  x[x=="Ifeanyi Eke"]<-"Ife Eke"
  x[x=="Simuel Frazier"]<-"Sim Frazier"
  x[x=="Rayner Fredrick"]<-"Ray Fredrick"
  x[x=="Isaac Freeman"]<-"Izzy Freeman"
  x[x=="Volodymyr Gerun"]<-"Volodymyr Herun"
  x[x=="Jarrell Lane"]<-"Jarrel Lane"
  
  x[which(startsWith(x, "Mike "))]<-gsub("Mike ", "Michael ", x[which(startsWith(x, "Mike "))])
  x[which(startsWith(x, "Mo "))]<-gsub("Mo ", "Maurice ", x[which(startsWith(x, "Mo "))])
  x[which(startsWith(x, "Joseph "))]<-gsub("Joseph ", "Joe ", x[which(startsWith(x, "Joseph "))])
  x[which(startsWith(x, "Joey "))]<-gsub("Joey ", "Joe ", x[which(startsWith(x, "Joey "))])
  
  x[which(startsWith(x, "Erkam K"))]<-"Erkam Kiris"
  x<-gsub("Gregory |Gregg ", "Greg ", x)
  x<-gsub("Mitchell ", "Mitch ", x)
  x<-gsub("Micheal ", "Michael ", x)
  x<-gsub("Rashad ", "Rashaad ", x)
  x<-gsub("Enrico ", "Rico ", x)
  x<-gsub("Giovanni ", "Gio ", x)
  x<-gsub("Khalill ", "Khalil ", x)
  x<-gsub("Nicholas |Nic |Nicolas |Nickolas ", "Nick ", x)
  x<-gsub("Dame |Damien ", "Damian ", x)
  x<-gsub("William ", "Will ", x)
  x<-gsub("Timothy ", "Tim ", x)
  x<-gsub("Herb ", "Herbert ", x)
  x<-gsub("Christian |Christopher ", "Chris ", x)
  x<-gsub("Phillip |Philip ", "Phil ", x)
  x<-gsub("Wesley ", "Wes ", x)
  x<-gsub("Qise ", "Marqise ", x)
  x<-gsub("Vic ", "Victor ", x)
  x<-gsub("Rodrick ", "Rod ", x)
  x<-gsub("Denzell ", "Denzel ", x)
  x<-gsub("Edward |Eddy |Eddie ", "Ed ", x)
  x<-gsub("Zach |Zack |Zachery |Zakarie |Zak |Zac ", "Zachary ", x)
  x<-gsub("Sam |Samuel |Sammie |Sammy ", "Sam ", x)
  x<-gsub("Steve |Stevie ", "Steven ", x)
  x<-gsub("Dan |Danny ", "Daniel ", x)
  x<-gsub("Dave ", "David ", x)
  x<-gsub("Ayodele ", "Dele ", x)
  x<-gsub("Brad ", "Bradley ", x)
  x<-gsub("Tyrone |Tyquone ", "Ty ", x)
  x<-gsub("Bobby ", "Bob ", x)
  x<-gsub("Cedrick ", "Cedric ", x)
  x<-gsub("Gabiriel ", "Gabe ", x)
  x<-gsub("Kenny |Kenneth ", "Ken ", x)
  x<-gsub("Joshua |Joshia ", "Josh ", x)
  x<-gsub("Ishmael |Ishmail ", "Ish ", x)
  x<-gsub("Cameron ", "Cam ", x)
  x<-gsub("Douglas ", "Doug ", x)
  x<-gsub("Kameron ", "Kam ", x)
  x<-gsub("Braiden ", "Braden ", x)
  x<-gsub("Leonidas ", "Leo ", x)
  x<-gsub("Matthew |Matth ", "Matt ", x)
  x<-gsub("Nathaniel |Nathan ", "Nate ", x)
  x<-gsub("Jeffrey |Jeffery ", "Jeff ", x)
  x<-gsub("Franklin ", "Frank ", x)
  x<-gsub("Pat ", "Patrick ", x)
  x<-gsub("Alexander |Alexandru ", "Alex ", x)
  x<-gsub("Ron |Ronnie ", "Ronald ", x)
  x<-gsub("Walter ", "Walt ", x)
  x<-gsub("A J ", "AJ ", x)
  x<-gsub("Shaq |Shaquil |Shaquiell |Shaqueille |Shaqil |Shaquile ", "Shaquille ", x)
  x<-gsub("Tywon ", "Ty ", x)
  x<-gsub("Peter ", "Pete ", x)
  x<-gsub("Vincent ", "Vince ", x)
  x<-gsub("Issac |Isacc ", "Isaac ", x)
  x<-gsub("Roderick |Rodney ", "Rod ", x)
  x<-gsub("Bennie |Ben ", "Benjamin ", x)
  x<-gsub("Robbie |Robby |Rob ", "Robert ", x)
  x<-gsub("^Jonathan |^Jon |^Johnny |^John |^Johnathan |^Jonathon ", "Jonathan ", x)
  x<-gsub("Darris |Darrius ", "Darius ", x)
  x<-gsub("Darquavis ", "Dar ", x)
  x<-gsub("Eneil ", "Eniel ", x)
  x<-gsub("Kashiff ", "Kashif ", x)
  x<-gsub("Max ", "Maxwell ", x)
  x<-gsub("Terrence |Terance |Terence ", "Terrance ", x)
  x<-gsub("Kalvin ", "Kal ", x)
  x<-gsub("Markieth ", "Markeith ", x)
  x<-gsub("Thomas ", "Tom ", x)
  x<-gsub("Kurtis ", "Kurt ", x)
  x<-gsub("Javi ", "Javier ", x)
  x<-gsub("Joey ", "Joe ", x)
  x<-gsub("Jontavious ", "Johntavius ", x)
  x[grepl("Brandis Raley", x)]<-"Brandis Raley Ross"
  x[x=="Lakendric Longmier"]<-"Lakendric Longmire"
  x[x=="Antonio Lang"]<-"Tj Lang"
  x[x=="Ricardo Brown"]<-"Josh Brown"
  x[x=="Terrell Holloway"]<-"Tu Holloway"
  x[x=="Krystopher Faber"]<-"Krys Faber"
  x[x=="Jin Soo Kim"]<-"Jin Soo Choi"
  x[x=="Sean Carter"]<-"Sean Roosevelt Carter"
  x[x=="Antiquan Beckham"]<-"Twany Beckham"
  x[x=="Gaby Ngoundgo"]<-"Gaby Ngoundjo"
  x[x=="Robo Kreps"]<-"Robert Kreps"
  x[x=="Elliot Engelmann"]<-"Elliot Englemann"
  x[x=="Andrew Ogilvy"]<-"AJ Ogilvy"
  x[x=="Bassy Inameti"]<-"Bassey Inameti"
  x[x=="Gerald Inman"]<-"JR Inman"
  x[x=="Trae Goldstan"]<-"Trae Goldston"
  x[x=="Derrick Rivera"]<-"DJ Rivera"
  x[x=="George Fotso"]<-"Georges Fotso"
  x[grepl("Szymon Szewczyk", x)]<-"Szymon Szewczyk"
  x[x=="George Fotso"]<-"Georges Fotso"
  x[x=="George Massoda"]<-"Georges Massoda"
  x[x=="Dashan Harris"]<-"Dash Harris"
  x[x=="Darren Govens"]<-"Darrin Govens"
  x[x=="Desean Butler"|x=="DeSean Butler"]<-"Dasean Butler"
  x[x=="Cory Pflieger"]<-"Cory Pfleiger"
  x[x=="Colby Santos"]<-"Colbey Santos"
  x[x=="Shamaine Dukes"]<-"Joe Dukes"
  x[x=="Terrance Woodberry"]<-"Terrance Woodbury"
  x[x=="Sharaud Curry"]<-"Morris Curry"
  x[x=="Kent Tribbett"]<-"Kenny Tribbett"
  x[x=="Jamar Thorpe"]<-"Jahmar Thorpe"
  x[x=="Donald Rashaad Singleton"]<-"Rashaad Singleton"
  x[x=="Arminus Urbutis"]<-"Arminas Urbutis"
  x[grepl("Benas Gric", x)]<-"Benas Griciunas"
  x[x=="Miguel Millien"]<-"Slim Millien"
  x[x=="Chamberlain Oguchi"]<-"Champ Oguchi"
  x[x=="Wil Fameni"]<-"Wilfried Fameni"
  x[iconv(x, to="ASCII//TRANSLIT")=="Marko A?uze"]<-"Marko Cuze"
  x[iconv(x, to="ASCII//TRANSLIT")=="Nikola SA?ekiA?"]<-"Nikola Scekic"
  x[x=="Weyinmi Efejuku"]<-"Weyinmi Efejuku Rose"
  x[x=="Stevie Thompson"]<-"Stephen Thompson"
  x[x=="Elbert Robinson III"]<-"Elbert Robinson"
  x[x=="Marcus E Williams"]<-"Marcus Williams"
  x[x=="Steven Thompson"| x=="Stevie Thompson"|x=="Stephen Thompson"]<-"Stephen Thompson"
  x[x=="Mitch Worchester"]<-"Mitch Worcester"
  x[x=="Jarod Sam"]<-"Jared Sam"
  x[x=="Steven Albrecht"]<-"Stephen Albrecht"
  x[x=="Mycheal Henry"]<-"Myke Henry"
  x[x=="Mykhael Lattimore"]<-"Myke Lattimore"
  x[x=="Kevin Ferrell"]<-"Yogi Ferrell"
  x[x=="Reginald Becton"]<-"Reginald Buckner"
  x[x=="E C Matthews"]<-"Ec Matthews"
  x[x=="Rod Simean"]<-"Rod Simeon"
  x[x=="Ike Smith"]<-"Ikeon Smith"
  x[x=="Reginal Johnson"]<-"Reginald Johnson"
  x[x=="Domas Sabonis"]<-"Domantas Sabonis"
  x[x=="Arnaud William Adala Moto"|x=="Arnaud Adala Moto"]<-"Arnaud Moto"
  x[x=="Tum Tum Nairn"]<-"Lourawls Nairn"
  x[x=="Montaque Gill Ceasar"]<-"Montaque Gill Caesar"
  x[x=="Chicken Knowles"]<-"Danrad Knowles"
  x[x=="Pierre Henderson Niles"]<-"Pierre Henderson"
  x[x=="Marquez Haynes"]<-"Keith Haynes"
  x[x=="Moe Harkless"]<-"Maurice Harkless"
  x[x=="Donnovan Kirk"]<-"Donnavan Kirk"
  x[x=="Lorenzo Mata Real"]<-"Lorenzo Mata"
  x[x=="Michael Cage"]<-"Mj Cage"
  x[x=="Baden Jaxen"]<-"Dexter Strickland"
  x[x=="Perry Dozier"]<-"PJ Dozier"
  x[x=="Antonio Wilson"]<-"Tony Wilson"
  x[x=="Jinski Grigsby"]<-"Trajinski Grigsby"
  x[x=="Papa Samba Ndao"]<-"Papa Ndao"
  x[x=="Daryus Quarles"]<-"Darius Quarles"
  x[x=="Devonte Drinkard"]<-"Davante Drinkard"
  x[grepl("Von", x) & grepl("Wafer", x)]<-"Von Wafer"
  x[x=="Faisal Aden"]<-"Faisel Aden"
  x[x=="Trevan Abraham"]<-"Travon Abraham"
  x[x=="Darell Haley"]<-"DJ Haley"
  x[x=="Tony Parker (ga)"|x=="Tony Parker (GA)"]<-"Tony Parker"
  x[x=="Justin Jackson (canada)"|x=="Justin Jackson (Canada)"]<-"Justin Jackson"
  x[x=="Craig Sealy"]<-"Craig Sealey"
  x[x=="Demarquise Johnson"]<-"Que Johnson"
  x[x=="Anrio Adams"]<-"Rio Adams"
  x[x=="Andre Applewhite"]<-"Dre Applewhite"
  x[x=="DeQuavious Wagner"]<-"Dee Wagner"
  x[x=="Maurice Watson"]<-"Mo Watson"
  x[x=="Nkereuwem Okoro"]<-"Kerwin Okoro"
  x[x=="Nazareth Long"]<-"Naz Long"
  x[x=="Kendal Yancy"]<-"Kendal Yancy Harris"
  x[x=="Wannah Bail"]<-"Wanaah Bail"
  x[x=="Bryce Dejean Jones"]<-"Bryce Jones"
  x[x=="Ikenna Iroegbu"]<-"Ike Iroegbu"
  x[x=="Byron Zeigler"|x=="Byron Bo Zeigler"]<-"Bo Zeigler"
  x[x=="Jarius Lyles"]<-"Jairus Lyles"
  x[x=="Rashawn Powell"|x=="RaShawn Powell"]<-"Pookie Powell"
  x[x=="Soma Edo"]<-"Karachi Edo"
  x[x=="Jalen Coleman"]<-"Jalen Coleman Lands"
  x[x=="Luke Garza"]<-"Luka Garza"
  x[x=="Ahmed Fields"]<-"Ahmad Fields"
  x[x=="Manu Lecomte"]<-"Emmanuel Lecomte"
  x[x=="Lucas Meikle"]<-"Luke Meikle"
  x[x=="Wadley Mompremeir"]<-"Wadly Mompremier"
  x[x=="Janari Joesarr"]<-"Janari Joesaar"
  x[x=="Idrissa Diallo"]<-"Idy Diallo"
  x[x=="Nick Babb"]<-"Nick Weiler Babb"
  x[x=="Jeremiah Paige"]<-"JD Paige"
  x[x=="Joseph Toye"]<-"Joe Toye"
  x[x=="Bakari Evelyn"]<-"Baker Evelyn"
  x[x=="Jonathan Laurent"]<-"Jonathan Jean Laurent"
  x[x=="Joseph Strugg"|x=="Joseph Struggs"]<-"Joe Srugg"
  x[x=="Christ Koumadje"]<-"Jean Marc Christ Koumadje"
  x[x=="Ahmad Carver"]<-"Ahmad Caver"
  x[x=="Enrico Tucker"]<-"Rico Tucker"
  x[x=="Richard Dorsey"]<-"Joe Dorsey"
  x[x=="Aleksander Maric"]<-"Aleks Maric"
  x[x=="Gordan Watt"]<-"Gordon Watt"
  x[x=="Jamall Edmonson"]<-"Jamall Edmondson"
  x[x=="Artavis Fisher"]<-"Tay Fisher"
  x[x=="Tangueray Beavers"]<-"Tank Beavers"
  x[x=="Artavis Fisher"]<-"Tay Fisher"
  x[x=="Laimonas Kisielius"]<-"Laimis Kisielius"
  x[x=="Bret Loscalzo"]<-"Brett Loscalzo"
  x[x=="Juan Pablo Silveira"]<-"Juan Pablo"
  x[x=="Gyno Pomare"]<-"Leonardo Pomare"
  x[grepl("Abu", x)& grepl("Arisha", x)]<-"Mohammed Abuarisha"
  x[x=="Erik Flato"]<-"Eric Flato"
  x[x=="Abdul Herrera"]<-"Abdul Hererra"
  x[x=="Micka?l Pietrus"]<-"Mickael Pietrus"
  x[x=="Anderson Varej?o"]<-"Anderson Varejao"
  x[x=="Zaur Pachulia"]<-"Zaza Pachulia"
  x[x=="Leandro Barbosa"]<-"Leandrinho Barbosa"
  x[x=="Rafael Araujo"]<-"Babby Araujo"
  x[x=="Pavel Podkolzine"]<-"Pavel Podkolzin"
  x[x=="Xue Yuyang"]<-"Yuyang Xue"
  x[x=="Pete Ramos"]<-"Pete Jonathan Ramos"
  x[x=="Rickey Paulding"]<-"Ricky Paulding"
  x[x=="Lou Williams"]<-"Louis Williams"
  x[x=="M Andriuskevicius"]<-"Martynas Andriuskevicius"
  x[x=="Micka?l Gelabale"]<-"Mickael Gelabale"
  x[x=="Cheik Samb"|x=="Cheick Samb"|x=="Cheikh Samb"]<-"Cheickh Samb"
  x[x=="Marcus Vinicus"]<-"Marquinhos Vinicius"
  x[x=="Deitrich Cole"]<-"Dietrich Cole"
  x[x=="Olexsiy Pecherov"]<-"Oleksiy Pecherov"
  x[x=="Anderson Varej?o"]<-"Anderson Varejao"
  x[x=="Tre Hansbrough"]<-"Tre Hansborough"
  x[x=="Aaron Afflalo"]<-"Arron Afflalo"
  x[x=="Giorgos Printezis"]<-"Georgios Printezis"
  x[x=="Yi Jianlian"]<-"Jianlian Yi"
  x[x=="Sun Yue"]<-"Yue Sun"
  x[x=="D J Strawberry"]<-"DJ Strawberry"
  x[grepl("Mbah aMou| A Moute| a Moute", x)|x=="LR Mbah"]<-"Luc Mbah a Moute"
  x[x=="Danilo Pinnock"]<-"JR Pinnock"
  x[x=="Chukwudiebere Maduabum"]<-"Chu Maduabum"
  x[x=="AlFarouq Aminu"]<-"Al-Farouq Aminu"
  x[x=="Patrick Mills"]<-"Patty Mills"
  x[x=="Sergi Llull"]<-"Sergio Llull"
  x[x=="Sergiy Gladyr"|x=="Sergey Gladyr"]<-"Sergii Gladyr"
  x[x=="Alexy Shved"]<-"Alexey Shved"
  x[x=="Christiano Felicio"]<-"Cristiano Felicio"
  x[x=="Ifeanyi Umezurike"]<-"Ifyeani Umezurike"
  x[x=="Roy Devyn Marble"]<-"Devyn Marble"
  x[x=="Sergiy Gladyr"]<-"Sergii Gladyr"
  x[x=="Avery Ugba"]<-"Averyl Ugba"
  x[x=="Vyacheslav Kravtsov"|x=="Slava Kravtsov"]<-"Viacheslav Kravtsov"
  x[x=="Nando DeColo"]<-"Nando De Colo"
  x[x=="Darius JohnsonOdom"]<-"Darius Johnson-Odom"
  x[x=="Atar Majok"]<-"Ater Majok"
  x[x=="Michael KiddGilchrist"]<-"Michael Kidd-Gilchrist"
  x[x=="Targuy Ngombo"]<-"Tanguy Ngombo"
  x[x=="Dennis Schroeder"]<-"Dennis Schroder"
  x[x=="Giannis Adetokunbo"|x=="Giannis Adetokoubo"]<-"Giannis Antetokounmpo"
  x[x=="Kentavious CaldwellPope"]<-"Kentavious Caldwell-Pope"
  x[x=="Michael CarterWilliams"]<-"Michael Carter-Williams"
  x[x=="Livio JeanCharles"]<-"Livio Jean-Charles"
  x[x=="Walter Tavares"]<-"Edy Tavares"
  x[x=="Nemanja Zubac"]<-"Nemanja Zubec"
  x[x=="Raul Neto"]<-"Raulzinho Neto"
  x[x=="Vasilije Micic"]<-"Vasilje Micic"
  x[x=="Willie CauleyStein"]<-"Willie Cauley-Stein"
  x[x=="Rondae HollisJefferson"]<-"Rondae Hollis-Jefferson"
  x[x=="Oliver Hanlan"]<-"Olivier Hanlan"
  x[x=="Juan Hernangomez"]<-"Juancho Hernangomez"
  x[x=="Guillermo Hernangomez"]<-"Willy Hernangomez"
  x[x=="Sasha Pavlovic"]<-"Aleksandar Pavlovic"
  x[x=="Marcus Vinicius"]<-"Marquinhos Vinicius"
  x[x=="Georgios Papagiannis"]<-"George Papagiannis"
  x[x=="Mouhamed SaerSene"|x=="Mouhamed Sene"]<-"Saer Sene"
  x[x=="Jaun Vaulet"]<-"Juan Vaulet"
  x[x=="Kahlil Felder"]<-"Kay Felder"
  x[x=="Timothe Luwawu"]<-"Timothe Luwawu-Cabarrot"
  x[x=="Edrice Adebayo"]<-"Bam Adebayo"
  x[x=="Jeff Pendergraph"]<-"Jeff Ayres"
  x[x=="Juan Jose  Barea"|x=="Jose Barea"]<-"JJ Barea"
  x[x=="Dorian FinneySmith"]<-"Dorian Finney-Smith"
  x[x=="OG Anunoby"|x=="Og Anunoby"]<-"Ogugua Anunoby"
  x[x=="Abdul Herrera"]<-"Abdul Hererra"
  x[x=="Miles Guidry"]<-"Myles Guidry"
  x[x=="Adam Walther"]<-"Bubba Walther"
  x[x=="Emmanuel Willis"]<-"Emanuel Willis"
  x[x=="Walter Waters"]<-"Walt Waters"
  x[x=="Stefan Jackson"]<-"Stefon Jackson"
  x[x=="Nayal Koshwal"]<-"Mac Koshwal"
  x[x=="Antonio Jardine"]<-"Scoop Jardine"
  x[x=="Shai Gilgeous Alexander"]<-"Shai Alexander"
  x[x=="Howard Thompkins"]<-"Trey Thompkins"
  x[x=="Demarquis Bost"]<-"Dee Bost"
  x[x=="Jajuan Johnson Purdue"|x=="JaJuan Johnson (Purdue)"]<-"Jajuan Johnson"
  x[x=="James Mcadoo"]<-"James Michael Mcadoo"
  x[x=="Arron "]<-"Arron Afflalo"
  x[x=="Demarquis Bost"]<-"Dee Bost"
  x[x=="Dezmine Wells"]<-"Dez Wells"
  x[x=="Elijah Carter"]<-"Eli Carter"
  x[x=="Ricardo Gathers"]<-"Rico Gathers"
  x[x=="Zam Fredrick"]<-"Zam Frederick"
  x[x=="Hanner Perea"]<-"Hanner Mosquera-Perea"
  x[x=="Jethro Tshisumpa Mbiya"]<-"Jethro Tshisumpa"
  x[x=="Marcus Georges Hunt"]<-"Marcus Hunt"
  x[x=="Fred Van Vleet"]<-"Fred VanVleet"
  x[x=="Dayshon Smith"]<-"Scoochie Smith"
  x[x=="Romelo Trimble"]<-"Melo Trimble"
  x[x=="Keelon Lawson"]<-"KJ Lawson"
  x[x=="Marcus D Williams"]<-"Marcus Williams"
  x[x=="Tj Gibbs"|x=="TJ Gibbs"]<-"Temple Gibbs"
  x[x=="Levan Alston"]<-"Shizz Alston"
  x[x=="Edwin Rios"]<-"Ed Rios"
  x[x=="Tyrell Reed"]<-"Tyrel Reed"
  x[x=="Carlton Scott"]<-"Carleton Scott"
  x[x=="James Hickson"]<-"JJ Hickson"
  x[x=="Clinton Chapman"]<-"Clint Chapman"
  x[x=="Martavious Adams"]<-"Martavius Adams"
  x[x=="Elijah Holman"]<-"Eli Holman"
  x[x=="Robin Sikes"]<-"Rob Sikes"
  x[x=="Kenechukwu Obi"]<-"Kene Obi"
  x[x=="Rayford Shipman"]<-"Ray Shipman"
  x[x=="Wallace Judge"]<-"Wally Judge"
  x[x=="Johnnie Williams"]<-"Johnnny Williams"
  x[x=="Thomas Knight"]<-"Tom Knight"
  x[x=="Andre Horne"]<-"Andrew Horne"
  x[x=="Eric Hutchinson"]<-"Eric Hutchison"
  x[x=="Gorgui Sy Dieng"]<-"Gorgui Dieng"
  x[x=="Jan Maehlan"]<-"Jan Maehlen"
  x[x=="Daniel Sapp"]<-"TJ Sapp"
  x[x=="Jarrell Eddie"]<-"Jarell Eddie"
  x[x=="Devonte Abron"]<-"Devonta Abron"
  x[x=="Kyle Caudill"]<-"KC Caudill"
  x[x=="Geoffrey Groselle"]<-"Geoff Groselle"
  x[x=="Isaac Nielson"]<-"Isaac Neilson"
  x[x=="Ray Lee"]<-"Raven Lee"
  x[x=="Willie Cauley"]<-"Willie Cauley Stein"
  x[x=="Ifeany Onyekaba"]<-"Ifeanyi Onyekaba"
  x[x=="Boris Bojanovsky"]<-"Boris Bojanovski"
  x[x=="Rashawn Powell"]<-"Pookie Powell"
  x[x=="Jarrell Martin"]<-"Jarell Martin"
  x[x=="Tony Trocha"]<-"Tonny Trocha-Morelos"
  x[x=="Emmanuel Ezechinoso"]<-"Emmanuel Ezechinonso"
  x[x=="Clay Custer"]<-"Clayton Custer"
  x[x=="Davon Chuck Ester"]<- "Chuck Ester"
  x[x=="Jalen Benton"]<- "Jaylen Benton"
  x[x=="Anas Osama Mahmoud"|x=="Anas Osama"]<-"Anas Mahmoud"
  x[x=="Jacob Hammond"]<-"Jake Hammond"
  x[x=="Austin Gillman"]<-"Austin Gillmann"
  x[x=="Derek Pardon"]<-"Dererk Pardon"
  x[x=="Semir Sehic"]<-"Samir Sehic"
  x[x=="Jailon Miller"|x=="Jalon Miller"|x=="Jalion Miller"]<-"JD Miller"
  x[x=="Cheickna Dembele"]<-"Cheikna Dembele"
  x[x=="Jacob Hammond"]<-"Jake Hammond"
  
  x[grepl("Trae", x)& grepl(" Jefferson", x)]<-"Demontrae Jefferson"
  
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
  x<-as.character(x)
  x<-  unname(x)
  x<-gsub("T J", "Tj", x)
  x[x=="NANA"]<-NA
  x[grepl("Owens V", x)]<-"Clemmye Owens"
  x[x=="Drekalo Clayton"]<-"Dre Clayton"
  x[grepl("Fitzpatrick Dorsey" ,x)]<-"Donte Fitzpatrick"
  x[x=="Tony Mitch"]<-"Tony Mitchell"
  x[x=="Sam Mccracken"]<-"Sam Mccraken"
  x[x=="Ravonn Posey"]<-"Pj Posey"
  x[grepl("Francis Ramirez", x)]<-"Brandone Francis"
  x[grepl("Strong Moore", x)]<-"Darrion Strong"
  x[grepl("Nye Redding", x)]<-"Ny Redding"
  x[x=="Devante Fitzgerald"|x=="Davante Fitzgerald"]<-"Davonte Fitzgerald"
  x[grepl("Hamdy Mohammed", x)]<-"Ahmed Hamdy"
  x[x=="Dadrian Allen"]<-"Darian Allen"
  x[x=="Reginald Gee"]<-"Reggie Gee"
  x[grepl("Cj Williamson", x)]<-"Cj Williamson"
  x[x=="Trevon Seymore"]<-"Trey Seymour"
  x[x=="Jabarr Singleton"]<-"Jabbar Singleton"
  x[grepl("marnier Cunningham", x)]<-"Dmarnier Cunningham"
  x[grepl("Bueno Leite", x)]<-"Rico Bueno"
  x[grepl("urroughs Cook", x)]<-"Johnathan Cook"
  x[grepl("Jere Vu", x)]<-"Jere Vucica"
  x[grepl("Cartagena Reyes", x)]<-"Miguel Cartagena"
  x[grepl("Walt Lawson", x)]<-"J Walker Lawson"
  x[x=="Mcwisdom Badejo"]<-"Mcwisdon Badejo"
  x[x=="Chiekh Mbacke Diong"]<-"Cheikh Mbacke Diong"
  x[x=="Dantiel Daniels"]<-"Tiel Daniels"
  x[x=="Justin Phillipe"]<-"Justin Philippe"
  x[x=="Cj Turman"]<-"Cedric Turman"
  x[x=="Jarell Lane"]<-"Jarrel Lane"
  x[x=="Darryln Johnson"]<-"Darrvin Johnson"
  x[x=="Nicola Djogo"]<-"Nikola Djogo"
  x[x=="Trevon James"]<-"Trey James"
  x[x=="Denzel Mcdaniel"]<-"Denaell Mcdaniel"
  x[x=="Shayok Shayok"]<-"Shayok M Shayok"
  x[x=="Ngijol Songolo"]<-"Robert Songolo Ngijol"
  x[x=="Darius Gardner"]<-"Pee Wee Gardner"
  x[x=="Mohammed Fall"]<-"Mohammed Tamsir Fall"
  x[x=="Rakim Brown"]<-"Rocky Brown"
  x[x=="Michael Harrell"]<-"Michael Harrel"
  x[x=="Rashad James"]<-"Rashaad James"
  x[x=="Jevon Lyle"]<-"Jr Lyle"
  x[x=="Stan Wier"]<-"Stan Weir"
  x[x=="Igor Nujic"]<-"Iggy Nujic"
  x[x=="Xavier Simpson"]<-"Zavier Simpson"
  x[x=="Jason Rich"]<-"Jason Richardson"
  x[x=="Kezie Okpala"]<-"Kz Okpala"
  x[x=="N Alex Walker"]<-"Nickeil Alex Walker"
  x[x=="S Gilgeous Alexander"]<-"Shai Alexander"
  x[x=="M Ashton Langford"]<-"Makai Ashton Langford"
  x[x=="P Jackson Cartwright"]<-"Parker Jackson Cartwright"
  x[x=="Ikey Obiagu"]<-"Ike Obiagu"
  x[x=="Marcos Santos Silva"]<-"Marcus Santos Silva"
  x[x=="Chris Sodom"]<-"Chris Yannick Sodom"
  x[x=="D Cosby Roundtree"]<-"Dhamir Cosby Roundtree"
  x[x=="Ty Shon Alexander"]<-"Tyshon Alexander"
  x[x=="Tejonathan Lucas"]<-"Tejon Lucas"
  x[x=="Johnnie Vasser"]<-"Johnnie Vassar"
  x[x=="Deaundre Ballard"]<-"Deaundrae Ballard"
  x[x=="Tony Anderson"]<-"Anthony Anderson"
  x[x=="Abdulhakim Ado"]<-"Abdul Ado"
  x[x=="Chris Darrington"]<-"Chris Darington"
  x[x=="Q Weatherspoon"]<-"Quinndary Weatherspoon"
  x[x=="D Smith Rivera"]<-"Dvauntes Smith Rivera"
  x[x=="D Washington"]<-"Darius Washington"
  x[x=="Jamorko Pickett"]<-"Jamarko Pickett"
  x[x=="J Williams"]<-"Johnathan Williams"
  x[x=="L Morris Walker"]<-"Langston Morris Walker"
  x[x=="J Coombs Mcdaniel"]<-"Jamaal Coombs Mcdaniel"
  x[x=="M Bryan Amaning"]<-"Matt Bryan Amaning"
  x[x=="Harold Baruti"]<-"Bitumba Baruti"
  x[x=="S Dorsey Walker"]<-"Sherron Dorsey Walker"
  x[x=="Davonte Lacey"]<-"Davonte Lacy"
  x[x=="Imara Ready"]<-"Ij Ready"
  x[x%in% c("Charles Obannon", "Charles Obannon Jr", "Chuck Obannon", "Chuck Obannon Jr")]<-"Chuck Obannon"
  x[x=="Timmel Eggleston"]<-"Melo Eggleston"
  x[x=="Anthony Jp Cortesia"]<-"Anthony Cortesia"
  x[x=="Leo Criswell"]<-"Leo Lyons"
  x[x=="Jonathan Reyes"]<-"Jonathan Reyes"
  x[x=="Josh Fortney"]<-"Josh Forney"
  x[x=="Malik Price Martin"]<-"Malik Martin"
  x[x=="Jay Jay Chandler"]<-"Jj Chandler"
  x[x=="Gary E Johnson"]<-"Gary Johnson"
  x[x=="Gary E Johnson"]<-"Gary Johnson"
  x[grepl("Tugs Bowen", x)]<-"Brian Bowen"
  x[x=="Jovontae Milner"]<-"Jovontae Millner"
  x[x=="Jagan Mosely"]<-"Jagan Mosley"
  x[x=="Jonathan Riek"]<-"Jonathan Reik"
  x[x=="Antonio Graves Davis"]<-"Antonio Graves"
  x[x=="E Victor Nickerson"]<-"Victor Nickerson"
  x[x=="Tory Miller"]<-"Tory Miller Stewart"
  x[x=="Jerald Butler"]<-"Jerald Gillens Butler"
  x[x=="Jonathan Reyes"|x=="Jonathan Carlos Reyes"]<-"Johncarlos Reyes"
  x[x=="Aljami Durham"]<-"Aj Durham"
  x[x=="Ibrahim Famouke Doumbia"]<-"Ibrahim Doumbia"
  x[x=="Rashond Salnave"]<-"Ray Salnave"
  x[x=="Quindarius Brown"]<-"Tookie Brown"
  x[x=="Jonathan Mayhanne"]<-"Jonathan Mayhane"
  x[x=="St Pierre"]<-"Joe St Pierre"
  x[x=="Ronshad Shabazz"]<-"Ronshad Allen Shabazz"
  x[x=="Tulio Henrique"]<-"Tulio Da Silva"
  x[x=="Al Eichelberger"]<-"Algevon Eichelberger"
  x[x=="Donovan Love"]<-"Donavan Theme Love"
  x[x=="Dante Hayes Williams"|x=="Dante Hales Williams"]<-"Dante Williams"
  x[x=="Jean Marc Christ Koumadje"]<-"Jean Marc Koumadje"
  x[x=="Juby Johnson"]<-"Julius Johnson"
  x[x=="Ky Bowman"]<-"Kyran Bowman"
  x[x=="Norbertas Giga"]<-"Norbertas Gigas"
  x[x=="Xavier Blount Johnson"]<-"Xavier Blount"
  x[x=="Colin Goss"]<-"Collin Goss"
  x[x=="Maxwell Hoetzel"]<-"Maxwell Montana"
  x[x=="Therren Szmidt"]<-"Therren Shelton Szmidt"
  x[x=="Guy Landry"]<-"Guy Edi"
  x[x=="Cheikh Mbodji"]<-"Cheikh Mbodj"
  x[x=="Marquis Godwin"]<-"Marquise Godwin"
  x[x=="Mario Haskett"]<-"Rio Haskett"
  x[x=="Nate Butler"]<-"Nate Butler Lind"
  x[x=="Stacey Wilson"]<-"Stacy Wilson"
  x[x=="Immanuel Quickly"]<-"Immanuel Quickley"
  x[x=="Demario Hines"]<-"Mario Hines"
  x[x=="Duvuaghn Maxwell"]<-"Duvaughn Maxwell"
  x[x=="Shareif Adumu"]<-"Shareif Adamu"
  x[x=="Trantell Knight"]<-"Tweety Knight"
  x[x=="Ashauhn Dixon Tatum"]<-"Asauhn Dixon Tatum"
  x[x=="Tymel Murphy"]<-"Tymell Murphy"
  x[x=="Dre Connor"]<-"Dre Conner"
  x[x=="Cheikh Fall"]<-"Ali Fall"
  x[x=="Ken Cherry"]<-"Ken Chery"
  x[x=="Joe Ucehbo"]<-"Joe Uchebo"
  x[x=="Remi Dibo"]<-"Remi N E Dibo"
  x[x=="Dre Mathieu"]<-"Deandre Mathieu"
  x[x=="Trevuaghn White"]<-"Corey Allen"
  x[x=="Cory Allen"]<-"Travaughn White"
  x[x=="Isahiah Williams"]<-"Isaiah Williams"
  x[x=="Ocatvious Green"]<-"Octavius Green"
  x[x=="Dejuan Newton"]<-"Desjuan Newton"
  x[x=="Teigbe Bamba"]<-"Tiegbe Bamba"
  x[x=="Aaron Adoye"]<-"Aaron Adeoye"
  x[x=="Trashon Burrell"]<-"Trahson Burrell"
  x[x=="Dinjyl Walker"]<-"Dinjiyl Walker"
  x[x=="Correante Deberry"]<-"Coreontae Deberry"
  x[x=="Devaughn Purcell"]<-"Devaughn Akoon Purcell"
  x[x=="Ivan Uceda"]<-"Ivan Cruz"
  x[x=="Devnota Pollard"]<-"Devonta Pollard"
  x[x=="Joe Thomassen"]<-"Joe Thomasson"
  x[x=="Bush Wamukota"]<-"Tom Wamukota"
  x[x=="Devuangtah Williams"]<-"Devaugntah Williams"
  x[x=="Fred Dure"]<-"Frederic Dure"
  x[x=="Betrand Nkali"]<-"Bertrand Nkali"
  x[x=="Rayanthony Sanders"]<-"Ray Sanders"
  x[x=="Jusitn Roberson"]<-"Justin Roberson"
  x[x=="Billydee Williams"]<-"Billy Dee Williams"
  x[x=="Ty Criswell"]<-"Tyron Criswell"
  x[x=="Joe Acuil"]<-"Jo Acuil"
  x[x=="Fredrick Edmond"]<-"Frederick Edmond"
  x[x=="Ahmed Mohammed"]<-"Ahmed Hamdy"
  x[x=="Buay Tauch"]<-"Buay Tuach"
  x[x=="Willie Connors"]<-"Willie Conner"
  x[x=="Jarell Marshall"]<-"Jarrel Marshall"
  x[grepl("Ethan", x)& grepl("Telfair", x)]<-"Ethan Telfair"
  x[x=="Semajae Haynes Jones"]<-"Samajae Haynes Jones"
  x[x=="Josh Webstera"]<-"Josh Webster"
  x[x=="Dejuan Morrero"]<-"Dejuan Marrero"
  x[x=="Cam Reedus"]<-"Camron Reedus"
  x[x=="Zachary Cuthbertson"]<-"Zac Cuthbertson"
  x[x=="Leroy Buchanan"]<-"Shaquille Buchanan"
  x[x=="Benjamin Nakwaasaha"]<-"Benjamin Nakwaasah"
  x[x=="Kj Scott"]<-"Kevin Scott"
  x[x=="Jamar Sandifer"]<-"Jamar Sandfer"
  x[x=="Temetrius Morant"]<-"Ja Morant"
  x[x=="Nate Nahirny"]<-"Nate Nahimy"
  x[x=="Ibrahima Sankary"]<-"Ibrahima Sankare"
  x[x=="Cyzanne Carson"]<-"Cezanne Carson"
  x[x=="Tray Bryant"]<-"Trae Bryant"
  x[x=="Jordan Varnado"]<-"Jordon Varnado"
  x[x=="Aj Mosby"]<-"Anthony Mosby"
  x[x=="Isaiah Blackmon"]<-"Isiah Blackmon"
  x[x=="Temidayo Yussuf"]<-"Temidayo Yussef"
  x[x=="Francis Alonso"]<-"Francisco Alonso"
  x[x=="Eli Pemberton"]<-"Elijah Pemberton"
  x[x=="Eli Cain"]<-"Elijah Cain"
  x[x=="Ryan Swan"]<-"Ryan Swan Ford"
  x[x=="Maurice Greenwood"]<-"Moses Greenwood"
  x[x=="Jakub Niziol"]<-"Kuba Niziol"
  x[x=="Brandonn Kamga"]<-"Brandon Kamga"
  x[x=="Artur Labinowicz"]<-"Art Labinowicz"
  x[x=="Nyaires Redding"]<-"Ny Redding"
  x[x=="Eli Scott"]<-"Elizjah Scott"
  x[x=="Sabastian Townes"]<-"Sebastian Townes"
  x[x=="Frederick Scott"]<-"Fred Scott"
  x[x=="Deederick Petty"]<-"Deedrick Petty"
  x[x=="Alex Dargenten"]<-"Alexis Dargentin"
  x[x=="Anthony Gatson"]<-"Anthony Gaston"
  x[x=="Georgie Pacheco Ortiz"]<-"Jorge Pacheco Ortiz"
  x[x=="Khari Jabriel Allen"]<-"Jabriel Allen"
  x[x=="Arnaldo Toro"]<-"Arnaldo Toro Barea"
  x[x=="Von Julien"]<-"Charvon Julien"
  x[x=="Wajid Aminu"]<-"Al Wajid Aminu"
  x[x=="Rene Castro"|x=="Rene Cannedy"| grepl("Rene Castro", x)]<-"Rene Castro Caneddy"
  x[x=="Leantwaun Luckett"]<-"Leantwan Luckett"
  x[x=="Aj Astroph"]<-"Aj Astroth"
  x[x=="Will Green"]<-"Willie Green"
  x[x=="Roy Marble"|x=="Roy Devyn Marble"]<-"Devyn Marble"
  x[x=="Donavan Theme Love"]<-"Donovan Theme Love"
  
  #names like DaiJon or Dai Jon that accidentally got changed to jonathan
  x<-gsub("jonathan |john |johnathan ", "jon ", x)
  x<-gsub(" Jonathan", " Jon", x)
  
  x[grepl("Branislav Terz", x)]<-"Branislav Terzic"
  x[grepl("Saint Gelais", x)]<-"Allan Saint Gelais"
  x[grepl("Muepo Kelly", x)]<-"Dj Muepo Kelly"
  x[grepl("Obiagu", x) & grepl("Ike", x)]<-"Ike Obiagu"
  x<-gsub("Vils", "Vila", x)
  x<-gsub("Enique", "Enrique", x)
  x
  
}

simpleCap <- function(x) {
  x<-tolower(x)
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

moving<-function(x, length, operation="mean") { 
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
  if(length(x)<=1) {
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
  if(!operation=="lag"){
    as.numeric(y)
  } else{
    y
  }
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
  mean(x, na.rm=TRUE, trim=.05)
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
GLMgrid<-expand.grid(alpha=c(0, .5, 1), lambda=seq(.00002, .15, .033))
fitControl1 <- trainControl(
  method = "cv", 
  number=5)
glmWrap<-function(modelFormula, data){
  train(modelFormula,
        data=data,#data[complete.cases(data[, all.vars(modelFormula)]), all.vars(modelFormula)],
        method = "glmnet",
        preProcess=c("scale"),
        trControl = fitControl1,
        tuneGrid=GLMgrid) 
}
XGBgrid <- expand.grid(  eta = .05,
                         max_depth =1:3,
                         nrounds = (1:4)*100,
                         gamma = 0,               #default=0
                         colsample_bytree = c(.5),    #default=1
                         min_child_weight = 5    #default=1
)
xgbWrap<-function(modelFormula, data){
  train(modelFormula,
        data=data[complete.cases(data[, all.vars(modelFormula)]), all.vars(modelFormula)],
        method = "xgbTree",
        preProcess=c("scale"),
        trControl = fitControl1,
        tuneGrid=XGBgrid) 
} 
Metrics<-function(y, x) {
  
  Rsq<-round(1-sum((y-x)^2)/sum((y-mean(y))^2), 4) #Rsq
  RMSE<-round(sqrt(mean((y-x)^2)), 4) #RMSE
  MAE<-round(mean(abs(y-x)), 4) #RMSE
  # MAPE<-round(mean(abs((y[y!=0]-x[y!=0])/y[y!=0])*100), 4)
  bias<-round(mean(y-x), 4) #RMSE
  
  paste(c(paste("RSq:", Rsq), paste("RMSE:", RMSE),paste("bias:", bias), paste("MAE:", MAE) )) #,paste("MAPE:", MAPE)
}
# function is a mess..but it works..sorry!

coordTeam<-function(x){
  x<-gsub(paste0(0:9,collapse="|"), "", x)
  x<-trimws(x)
  x<-gsub("Signed|signed", "", x)
  
  
  #TO LOWER####
  
  
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
  x<-gsub("-|-|[-]", " ", x, fixed = TRUE)
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
  x[substr(x, 1, 2)=="a "]<-gsub("a ", "", x[substr(x, 1, 2)=="a "])
  
  x<-gsub("wshgtn|washingtn", "washington", x)
  x<-gsub("grn", "green", x)
  
  
  #vegas line movements data uses weird names
  x[x=="valpar"]<-"valparaiso"
  x[x=="col"]<-"colorado"
  x[x=="sacto"]<-"sacramento state"
  x[x=="noco"]<-'northern colorado'
  x[x=="steph"]<-"stephen f austin"
  x[x=="can"]<-"canisius"
  x[x=="fresno"]<-"fresno state"
  x[x=="arklr"]<-"arkansas little rock"
  x[x=='loymry']<-"loyola marymount"
  x[x=="ncashv"]<-"unc asheville"
  x[x=="flrint"]<-"fiu"
  x[x=="calbap"]<-"california baptist"
  x[x=="libert"]<-"liberty"
  x[x=="gramb"]<-"grambling"
  x[x=="prince"]<-'princeton'
  x[x=="geoso"]<-"georgia southern"
  x[x=="ark"]<-"arkansas"
  x[x=="duq"]<-"duquesne"
  x[x=='sodak']<-"south dakota"
  x[x=="stbon"]<-"st bonaventure"
  x[x=='asu']<-"arizona state"
  x[x=='minn']<-"minnesota"
  x[x=="wash"]<-"washington"
  x[x=="stjos"]<-"st josephs"
  x[x=="auspy"]<-"austin peay"
  x[x=="pennst"]<-"penn state"
  x[x=='kanst']<-"kansas state"
  x[x=="belmon"]<-'belmont'
  x[x=='sfla']<-"usf"
  x[x=='okla']<-"oklahoma"
  x[x=="kanst"]<-'kansas state'
  x[x=='samfrd']<-"samford"
  x[x=='nwest']<-"Northwestern"
  x[x=="txamcc"]<-"texas a&m cc"
  x[x=='iowast']<-"iowa state"
  x[x=='grand']<-"grand canyon"
  x[x=='kan']<-'kansas'
  x[x=="ala"]<-"alabama"
  x[x=="marsh"]<-'marshall'
  x[x=="va"]<-"virginia"
  x[x=='centfl']<-"central florida"
  x[x=="rutg"]<-"rutgers"
  x[x=="ariz"]<-"arizona"
  x[x=="ntex"]<-'north texas'
  x[x=='creigh']<-"creighton"
  x[x=='ndakst']<-"north dakota state"
  x[x=='ohiost']<-"ohio state"
  x[x=='bc']<-"boston college"
  x[x=="corn"]<-"cornell"
  x[x=='njtech']<-"njit"
  x[x=="swmo"]<-"southwest missouri state"
  x[x=="ecar"]<-"eastern carolina"
  x[x=="neb"]<-"nebraska"
  x[x=='conn']<-"connecticut"
  x[x=="latech"]<-"louisiana tech"
  x[x=="missst"]<-"mississippi state"
  x[x=="drtmth"]<-"dartmouth"
  x[x=="michst"]<-"michigan state"
  x[x=="washst"]<-"washington state"
  x[x=="maryca"]<-"st marys"
  x[x=="seattl"]<-"seattle"
  x[x=='miss']<-"mississippi"
  x[x=="stlou"]<-"st louis"
  x[x=='okst']<-"oklahoma state"
  x[x=="niowa"]<-"northern iowa"
  x[x=='oregst']<-"oregon state"
  x[x=='pepper']<-"pepperdine"
  x[x=="akr"]<-"akron"
  x[x=="syr"]<-"syracuse"
  x[x=="charsc"]<-"charleston"
  x[x=="brad"]<-'bradley'
  x[x=='wkty']<-"western kentucky"
  x[x=='stpete']<-"st peters"
  x[x=='tencha']<-"chattanooga"
  x[x=='wiscmi']<-"wisconsin milwaukee"
  x[x=='boise']<-"boise state"
  x[x=='vill']<-"villanova"
  x[x=='murray']<-"murray state"
  x[x=="sutah"]<-"southern utah"
  x[x=="portst"]<-'portland state'
  x[x=='ncst']<-'nc state'
  x[x=="woff"]<-"wofford"
  x[x=='wake']<-"wake forest"
  x[x=="hou"]<-"houston"
  x[x=="lasall"]<-"lasalle"
  x[x=="marqet"]<-"marquette"
  x[x=="tentec"]<-"tennessee tech"
  x[x=='day']<-'dayton'
  x[x=='buck']<-"bucknell"
  x[x=='mich']<-"michigan"
  x[x=="winthr"]<-"winthrop"
  x[x=='ford']<-"fordham"
  x[x=='gonzag']<-"gonzaga"
  x[x=='emich']<-'eastern michigan'
  x[x=="indist"]<-"indiana state"
  x[x=='wiscgb']<-"wisconsin green bay"
  x[x=="stjohn"]<-"st johns"
  x[x=='david']<-"davidson"
  x[x=='harv']<-"harvard"
  x[x=="ken"]<-"kentucky"
  x[x=='stan']<-"stanford"
  x[x=="temp"]<-"temple"
  x[x=='tenn']<-'tennessee'
  x[x=='wva']<-"west virginia"
  x[x=="jmad"]<-"james madison"
  x[x=="aub"]<-"auburn"
  x[x=='nkty']<-"northern kentucky"
  x[x=="texsta"]<-'texas state'
  x[x=='txam']<-"texas a&m"
  x[x=="nev"]<-"nevada"
  x[x=='prov']<-"providence"
  x[x=="loychi"]<-"loyola chicago"
  x[x=="ind"]<-"indiana"
  x[x=="jkvlst"]<-"jacksonville state"
  x[x=='montan']<-'montana'
  x[x=="oreg"]<-"oregon"
  x[x=="mass"]<-"umass"
  x[x=="cmich"]<-'central michigan'
  x[x=="scar"]<-"south carolina"
  x[x=="fla"]<-"florida"
  x[x=="colg"]<-"colgate"
  x[x=="richmd"]<-"richmond"
  x[x=='franpa']<-'st francis pa'
  x[x=="vermon"]<-"vermont"
  x[x=='geomas']<-'george mason'
  x[x=="vatech"]<-"virginia tech"
  x[x=="clem"]<-'clemson'
  x[x=='hfstra']<-"hofstra"
  x[x=="liu"]<-"long island"
  x[x=="camp"]<-"campbell"
  x[x=="gatech"]<-"georgia tech"
  x[x=='binghm']<-'binghamton'
  x[x=='abil']<-"abilene christian"
  x[x=="ga"]<-'georgia'
  x[x=='radfrd']<-"radford"
  x[x=='txtec']<-"texas tech"
  x[x=="nmst"]<-"new mexico state"
  x[x=="utahst"]<-'utah state'
  x[x=='snclra']<-'santa clara'
  x[x=="ill"]<-'illinois'
  x[x=='geowas']<-"george washington"
  x[x=="louis"]<-"louisville"
  x[x=="george"]<-"georgetown"
  x[x=="sodaks"]<-"south dakota state"
  x[x=="cinci"]<-"cincinnati"
  x[x=='wrigst']<-"wright state"
  x[x=="wmich"]<-"western michigan"
  x[x=='ball']<-"ball state"
  x[x=="wich"]<-"wichita state"
  x[x=="nmex"]<-"new mexico"
  x[x=="davis"]<-"uc davis"
  x[x=="wyo"]<-"wyoming"
  x[x=='charlo']<-"charlotte"
  x[x=='geogst']<-'georgia state'
  x[x=='memphs']<-'memphis'
  x[x=='oldom']<-"old dominion"
  x[x=='seton']<-"seton hall"
  x[x=="rhode"]<-"rhode island"
  x[x=="lamon"]<-"louisiana monroe"
  x[x=="hartfd"]<-"hartford"
  x[x=='ilst']<-"illinois state"
  x[x=='holycr']<-"holy cross"
  x[x=='grensb']<-"unc greensboro"
  x[x=='wisc']<-"wisconsin"
  x[x=="missou"]<-"missouri"
  x[x=="van"]<-"vanderbilt"
  x[x=="neast"]<-"northeastern"
  x[x=='monmth']<-'monmouth'
  x[x=='sdsu']<-'san diego state'
  x[x=="mdbal"]<-"umbc"
  x[x=='bg']<-"bowling green"
  x[x=='sill']<-"southern illinois"
  x[x=='illchi']<-"illinois chicago"
  x[x=="lbst"]<-'long beach state'
  x[x=='niag']<-"niagara"
  x[x=='midten']<-'middle tennessee'
  x[x=='manh']<-'manhattan'
  x[x=='ncarat']<-"north carolina a&t"
  x[x=="kensaw"]<-'kennesaw state'
  x[x=='amer']<-"american"
  x[x=='alaam']<-"alabama a&m"
  x[x=="txso"]<-"texas southern"
  x[x=='mary']<-"maryland"
  x[x=='naz']<-'northern arizona'
  x[x=="prarvw"]<-"prairie view a&m"
  x[x=='morgan']<-'morgan state'
  x[x=='appal']<-"appalachian state"
  x[x=="drexl"]<-'drexel'
  x[x=='jkvl']<-"jacksonville"
  x[x=="smiss"]<-"southern miss"
  x[x=='etenst']<-'east tennessee state'
  x[x=='ifw']<-"ipfw"
  x[x=="dalip"]<-"lipscomb"
  x[x=='usd']<-"san diego"
  x[x=='oral']<-"oral roberts"
  x[x=="stetsn"]<-"stetson"
  x[x=="selou"]<-"southeast louisiana"
  x[x=="mokc"]<-"missouri kansas city"
  x[x=='loymd']<-"loyola maryland"  
  x[x=='nichol']<-"nicholls state"
  x[x=='utvlst']<-"utah valley state"
  x[x=='wm']<-"western michigan"
  x[x=='evan']<-"evansville"
  x[x=="txarl"]<-"ut arlington"
  x[x=='port']<-"portland"
  x[x=='clevst']<-"cleveland state"
  x[x=="morest"]<-"morehead state"
  x[x=="wcar"]<-"western carolina"
  x[x=="eill"]<-'eastern illinois'
  x[x=='tenmar']<-'ut martin'
  x[x=="del"]<-"delaware"
  x[x=="colu"]<-'columbia'
  x[x=="ekty"]<-"eastern kentucky"
  x[x=='nodak']<-"north dakota"
  x[x=='idast']<-"idaho st"
  x[x=='quinni']<-'quinnipiac'
  x[x=='fair']<-'fairfield'
  x[x=='sjsu']<-"san jose state"
  x[x=='umassl']<-'umass lowell'
  x[x=='hpoint']<-'high point'
  x[x=="jackst"]<-"jackson state"
  x[x=="lafay"]<-"lafayette"
  x[x=='tenst']<-"tennessee state"
  x[x=="bethun"]<-'bethune cookman'
  x[x=="scst"]<-"south carolina state"
  x[x=="centct"]<-'central connecticut'
  x[x=='sothrn']<-'southern'
  x[x=='presby']<-"presbyterian"
  x[x=='coppst']<-'coppin state'
  x[x=="siu"]<-"siu edwardsville"
  x[x=="uop"]<-"pacific"
  x[x=='nd']<-"notre dame"
  x[x=='detmer']<-'detroit'
  x[x=="cark"]<-'central arkansas'
  x[x=="flatln"]<-"florida atlantic"
  x[x=="una"]<-'north alabama'
  x[x=="calslo"]<-"cal poly"
  x[x=="scups"]<-"usc upstate"
  x[x=="miaoh"]<-'miami oh'
  x[x=="nordge"]<-'cal state northridge'
  x[x=="young"]<-"youngstown state"
  x[x=="semist"]<-"southeast missouri state"
  x[x=="swla"]<-"louisiana lafayette"
  x[x=="air"]<-"air force"
  x[x=="nowest"]<-"northwestern state"
  x[x=="salab"]<-"south alabama"
  x[x=="full"]<-"cal state fullerton"
  x[x=="will"]<-'western illinois'
  x[x=='saj']<-"sacred heart"
  x[x=="franny"]<-"st francis ny"
  x[x=='shore']<-"maryland eastern shore"
  x[x=="flagu"]<-"fgcu"
  x[x=="samst"]<-"sam houston state"
  x[x=="alabir"]<-"uab"
  x[x=="alast"]<-"alabama state"
  x[x=="texpa"]<-"texas rgv"
  x[x=='colst']<-"colorado state"
  x[x=="delst"]<-"delaware state"
  x[x=='ncwilm']<-"unc wilmington"
  x[x=='oaklnd']<-'oakland'
  x[x=="noill"]<-"northern illinois"
  x[x=="montst"]<-'montana st'
  x[x=='ewash']<-'eastern washington'
  x[x=="nwhamp"]<-"new hampshire"
  x[x=="hamptn"]<-'hampton'
  x[x=="chist"]<-'chicago state'
  x[x=="stony"]<-"stony brook"
  x[x=='charso']<-"charleston southern"
  x[x=="frdick"]<-"farleigh dickinson"
  x[x=="long"]<-"longwood"
  x[x=='bu']<-"boston university"
  x[x=='gardwb']<-'gardner webb'
  x[x=="nccent"]<-"north carolina central"
  x[x=="stmary"]<-"st marys"
  x[x=="nfla"]<-'north florida'
  x[x=='cstcar']<-"coastal carolina"
  x[x=='rmoris']<-"robert morris"
  x[x=='arkst']<-"arkansas state"
  x[x=='savan']<-"savannah st"
  x[x=='missvl']<-"mississippi valley state"
  x[x=="txelp"]<-"texas el paso"
  x[x=='mcnees']<-'mcneese state'
  x[x=="norl"]<-"new orleans"  
  x[x=="houbap"]<-'houston baptist'
  x[x=="csbak"]<-'cal state bakersfield'
  x[x=="citadl"]<-'citadel'
  x[x=="weber"]<-'weber state'
  x[x=="flaam"]<-"florida a&m"
  x[x=="incarnate"|x%in% c("Uiw", "UIW", "uiw")]<-"incarnate word"
  x[x=='norflk']<-'norfolk state'
  x[x=="snanto"]<-'texas san antonio'
  x[x=="arkpb"]<-'arkansas pine bluff'
  
  
  
  
  
  
  
  x[x=="va commonwealth"|x=="virginia commonwealth"]<-"vcu"
  x[x=="brigham young"]<-"byu"
  x[x=="mcneese"]<-"mcneese st"
  x[x=="seattle u"|x=="seattle university"]<-"seattle"
  x[x=="eastern tennesseeastern st"|x=="east tenn st"|x=="eastern tenn st"|x=="eastern tenn state"|x=="eastern tennesseeastern state"]<-"east tennessee st"
  x[x=="unc charlotte"|x=="north carolina charlotte"]<-"charlotte"
  x[x=="indiana purdue"|x=="iu purdue indianapolis"]<-"iupui"
  x[x=="la lafayette"]<-"louisiana lafayette"
  x[x=="south utah"]<-"southern utah"
  x[x=="troy st"]<-"troy"
  x[x=="md baltimore co"]<-"umbc"
  x[x=="texas-el paso"]<-"utep"
  x[x=="tx san antonio"]<-"utsa"
  x[x=="tx pan american"|x=="texas pan american"|x=="texas pan am"]<-"texas rgv"
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
  x[x=="boston univ"|x=="boston"]<-"boston university"
  x[x=="sf austin"|x=="sfa"|x=="ste f austin"|x=="south f austin"]<-"stephen f austin"
  x[x=="southern univ"|x=="southern university"]<-"southern"
  x[x=="g washington"]<-"george washington"
  x[x=="ark pine bluff"|x=="ar pine bluff"|x=="arkansas pine bl"|x=="arkansas-pine bluff"]<-"arkansas pine bluff"
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
  x[x=="texas-arlington"|x=="texas arlington"|(grepl("texas", x)& grepl("lington", x))]<-"ut arlington"
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
  x[x=="fort wayneipfw"|x=="iupu ft wayne"|x=="indiana purdue ft wayne"|x=="purdue ft wayne"|x=="purdue fort wayne"]<-"ipfw"
  x[x=="saint marys cal"|(grepl("saint", x)&grepl("mary", x))]<-"saint marys"
  x[x=="central floridaucf"|x=="central fl"]<-"ucf"
  x[x=="omahaneb omaha"|x=="neb omaha"]<-"omaha"
  x[x=="stony brook ny"]<-"stony brook"
  x[x=="oakland michigan"|x=="oakland mi"]<-"oakland"
  x[x=="north floridaunf"]<-"north florida"
  x[x=="kansas cityumkc"]<-"umkc"
  x[x=="njitnew jersey tech"|x=="nj tech"]<-"njit"
  x[x=="binghamton ny"]<-"binghamton"
  x[x=="umbcmd balt"]<-"umbc"
  x[x=="wis milwaukee"|(grepl("wisco", x)& grepl("milwauk", x))]<-"wisconsin milwaukee"
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
  x[x=="unc"|x=="ncar"]<-"north carolina"
  x[x=="uva"]<-"virginia"
  x[x=="fsu"]<-"florida st"
  x[x=="mid tennessee"]<-"middle tennessee"
  x[x=="uri"]<-"rhode island"
  x[x=="miami"|x=="miami fla"|x=="miami (fla.)"|x=="miami fl"]<-"miami"
  x[x=="uc-davis"]<-"uc davis"
  x[x=="dartmouth college"]<-"dartmouth"
  x[x=="william mary"|x=="william and mary"]<-"william & mary"
  x[x=="siena college"]<-"siena"
  x[x=="north carolina at"]<-"north carolina a&t"
  x[x=="california santa cruz"]<-"uc santa cruz"
  
  #SIMPLE CAP####
  
  x<-sapply(x, simpleCap)
  x[x=="NC State"|x=="North Carolina st"|x=="North Carolina St" |x=="North Carolina State"]<-"Nc State"
  x[x=="st Joseph's"|x=="St Joseph's"]<-"Saint Joseph's"
  x[x=="Middle Tennessee"|x=="Middle Tenn"]<-"Middle Tennessee State"
  x[x=="Eku"]<-"Eastern Kentucky"
  x[x=="UNC"|x=="Unc"]<-"North Carolina"
  x[x=="SE Louisiana"|x=="Southeast Louisiana"]<-"Southeastern Louisiana"
  x[x=="Presbyterian College"]<-"Presbyterian"
  x[x=="UConn"|x=="Uconn"]<-"Connecticut"
  x[x=="North Carolina-Asheville"|x=="NC Asheville"|x=="North Carolina Asheville"|x=="Asheville"]<-"Unc Asheville"
  x[x=="North Carolina-Greensboro"|x=="NC Greensboro"|x=="North Carolina Greensboro"|x=="Greensboro"|x=="Nc Grnsboro"]<-"Unc Greensboro"
  x[x=="North Carolina-Wilmington"|x=="NC Wilmington"|x=="Wilmington"]<-"Unc Wilmington"
  x[x=="NC Central"]<-"North Carolina Central"
  x[grepl("East Tennessee St", x)]<-"East Tennessee State"
  x[x=="Virginia Commonwealth"|x=="Vcuva Commonwealth"|x=="Vacomm"]<-"Vcu"
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
  x[x=="Valpo"|x=="Valpar"]<-"Valparaiso"
  x[x=="Saint Marys Ca"|x=="Saint Mary's (CA)"]<-"St Marys"
  x[x=="Miss Valley State"]<-"Mississippi Valley State"
  x[x=="Cal"|x=="University of California"]<-"California"
  x[x=="Birm Southern"|x=="Bham Southern"]<-"Birmingham Southern"
  x[x=="Ccu"|x=="Coastal Car"]<-"Coastal Carolina"
  x[x=="Ucr"|x=="California Riverside"]<-"Uc Riverside"
  x[x=="Uci"|x=="California Irvine"]<-"Uc Irvine"
  x[x=="Ucd"]<-"Uc Davis"
  x[x=="Nccu"]<-"North Carolina Central"
  x[x=="Csun"|x=="CS Northridge"|x=="Cal State Nrdge"|x=="Cal St Nrdge"|x=="Northridge"]<-"Cal State Northridge"
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
  x[x=="Suny Buffalo"|x=='Bufflo']<-"Buffalo"
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
  x[x=="Charleston"|x=="Col Charleston"|x=="Col Of Charleston"|x=="Col Charlestn"|
      x=="University Of Charleston"|x=="U Of Charleston"]<-"College Of Charleston"
  x[x=="Texas-Arlington"|x=="Tx Arlington"|x=="Texas Arlington"]<-"UT-Arlington"
  x[x=="Texas A&M-CC"|x=="Texas A&M Corpus Chris"|x=="Texas A&M CC"|x=="A&M Corpus Christi"|x=="A&m Corpus Chris"]<-"Texas A&M Corpus Christi"
  x[x=="Loyola (IL)"|x=="Loyola Il"|x=="Loyola Chi"|x=="Loyola Illinois"|x=="Loyola Ill"]<-"Loyola Chicago"
  x[x=="Saint Mary's"|x=="St Marys Ca"|x=="St Mary"|x=="Saint Mary"]<-"st Marys"
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
  x[x=="Loyola Marymt"|x=="Loy Marymount"|x=="Lmu"|x=="Loyola Mymt"|x=="Loymry"]<-"Loyola Marymount"
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
  x[x=="Ul Lafayette"|x=="Ull"|(grepl("ouisiana", x)& grepl("afayette", x))]<-"Louisiana-Lafayette"
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
  x[x=="Edwardsville"|x=="Siue"|x=="Southern Illinois-Edwardsville"|x=="Siu Edwardsvle"|x=="Siu Edward"| grepl("Edwardsville", x)]<-"Siu Edwardsville"
  x[x=="Houston Bap"]<-"Houston Baptist"
  x[x=="Nebraska Omaha"|x=="Nebraska-Omaha" |x=="Ne Omaha"]<-"Omaha"
  x[x=="UT Rio Grande Valley"|x=="Utrgv"| tolower(x)=="texas rio grande valley"|x=="UTRGV"|x=="Rio Grande"|x== "Texas Pan American"]<-"Texas RGV"
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
  x[x=="Sw Texas State"|x=="Sw Texas St"|x=="Texas State San Marcos"|x=="Texas St San Marcos"]<-"Texas State"
  x[x=="Beth Cook"|grepl("cookman|Cookman", x)]<-"Bethune Cookman"
  x[x=="Sacred Hrt"]<-"Sacred Heart"
  x[x=="Jksnville State"|x=="Jksnville St"]<-"Jacksonville State"
  
  x[grepl("San Jo", x)& grepl("St", x)]<-"San Jose State"
  x[x=="Ucside"]<-"Uc Riverside"
  x[x=="Bowling"]<-"Bowling Green"
  x[x=="Maryland Eastern Shore Shore"]<-"Maryland Eastern Shore"
  x[x=="Uncsboro"]<-"Unc Greensboro"
  x[x=="Georgia St Georgia"]<-"Georgia St"
  x[x=="Miss Valley State"|x=="Miss Valley St"]<-"Mississippi Valley State"
  
  x[which(endsWith(x, " St"))]<-gsub(" St", " State", x[which(endsWith(x, " St"))])
  x[which(endsWith(x, " U"))]<-gsub(" U", " University", x[which(endsWith(x, " U"))])
  x[which(endsWith(x, " Col"))]<-gsub(" Col", " College", x[which(endsWith(x, " Col"))])
  x<-gsub("Cal st |Cal St ", "Cal State ", x)
  x[x=="St Francis PA"|x=="Saint Francis Pa"|x=="Saint Francis University"|
      x=="st Francis Pa"|x=="Saint Francis (PA)"|x=="Saint Fran Pa"|x=="Saint Francis Penn"|x=="Saint Francis"]<-"St Francis (pa)"
  
  
  x[x=="NANA"]<-NA
  x[x=="Sc State"|x=="South Car State"]<-"South Carolina State"
  x[x=="Texas Rio Grande Valley"|x=="Texas Pan Am"|x=="Texas Pan American"]<-"Texas Rgv"
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
  x[x=="Texas A&m Cc"|x=="Texas A&m Corpus Chris"|(grepl("Texas", x)& grepl("cc", x))]<-"Texas A&m Corpus Christi"
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
  x[x=="No Alabama"|x=="University of North Alabama"]<-"North Alabama"
  x[x=="California Baptist"|x=="California Baptist University"|x=="Cal Baptist"|x=="Calbap"]<-"California Baptist"
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
  x[x=="R Morris"|x=="Robert Morris Il"]<-"Robert Morris"
  x[x=="South Diego State"]<-"San Diego State"
  x[x=="Sd State"]<-"South Dakota State"
  x[x=="Maryland Baltimore County"|x=="Maryland Bc"]<-"Umbc"
  x[x=="Tx San Antonio"|(grepl("Texas", x)& grepl("Antonio", x))]<-"Texas San Antonio"
  x[x=="Texas Arlington"]<-"Ut Arlington"
  x[x=="Ar Pine Bluff"|x=="Uapb"|grepl("Bluff", x)]<-"Arkansas Pine Bluff"
  x[x=="New Jersey Tech"|x=="Nj Inst Of Technology"]<-"Njit"
  x[x=="St Johns Ny"]<-"St Johns"
  x[x=="Louisiana"]<-"Louisiana Lafayette"
  x[x=="Va Commonwealth"]<-"Vcu"
  x[x=="A&m Corpus Christi"]<-"Texas A&m Corpus Christi"
  x[x=="Tenn Martin"]<-"Tennessee Martin"
  x[x=="Csu Northridge"]<-"Cal State Northridge"
  x[x=="Miss State"]<-"Mississippi State"
  x[x=="Arkansas Little Rock"|x=="Arkansas Lr"|x=="Arklr"]<-"Little Rock"
  x[x=="Osu"]<-"Ohio State"
  x[x=="St Francis Brk"]<-"St Francis (NY)"
  x<-gsub("California State", "Cal State", x)
  x[x=="Nw State"]<-"Northwestern State"
  x[x=="St Josephs"]<-"Saint Josephs"
  x[x=="San Fransco"]<-"San Francisco"
  x[x=="Mvsumiss Valley State"]<-"Mississippi Valley State"
  x[x=="Cal State Long Beach"]<-"Long Beach State"
  x[x=="Texas Rio Grande"]<-"Texas Rgv"
  x[x=="Nicholls"]<-"Nicholls State"
  x[x=="Gcu"]<-"Grand Canyon"
  x[grepl("us Christi", x)]<-"Texas A&m Corpus Christi"
  x[grepl("Grande Valley", x)|grepl("Pan Am", x)]<-"Texas Rgv"
  x[x=="Will & Mary"]<-"William & Mary"
  x[x=="Southwest Missouri State"]<-"Missouri State"
  x[x=="Southwest Texas State"]<-"Texas State"
  x[x=="University Of California"]<-"California"
  x[x=="Florida Am"]<-"Florida A&m"
  x[x=="Alabama Am"]<-"Alabama A&m"
  x[x=="Xavierohio"]<-"Xavier"
  x[x=="Miamiflorida"]<-"Miami"
  x[x=='Loyolachicago']<-"Loyola Chicago"
  x[x=='Saint Josephspa']<-"Saint Josephs"
  x[x=="Stony Brookny"]<-"Stony Brook"
  x[x=='Omahanebomaha']<-"Omaha"
  x[x=="Fort Waynepfw"]<-"Ipfw"
  x[x=="American University"]<-"American"
  x[x=='Illinoischicago']<-"Illinois Chicago"
  x[x=="Oaklandmich"]<-"Oakland"
  x[x=='Fla International']<-"Fiu"
  x[x=='Gardnerwebb']<-"Gardner Webb"
  x[x=="Iona College"]<-"Iona"
  x[x=="Albanyny"]<-"Albany"
  x[x=="Saint Francispa"]<-"St Francis Pa"
  x[x=="Texas Amcorpuschristi"]<-"Texas A&m Corpus Christi"
  x[x=="Saint Francisny"]<-"St Francis Ny"
  x[x=='Monmouthnj']<-"Monmouth"
  x[x=='Prairie View Am']<-"Prairie View A&m"
  x[x=='Long Island Uliu']<-"Long Island"
  x[x=='Loyolamaryland']<-"Loyola Md"
  x[x=="Tennesseemartin"]<-"Tennessee Martin"
  x[x=="Cal Polyslo"]<-"Cal Poly"
  x[x=="Siuedwardsville"]<-"Siu Edwardsville"
  x[x=="Binghamtonny"]<-"Binghamton"
  x[x=="Mdeastern Shoreumes"]<-"Maryland Eastern Shore"
  x[x=='Miamiohio']<-"Miami Oh"
  x[x=="Lsuniversity"]<-"Lsu"
  x[x=="Tcuniversity"]<-"Tcu"
  x[x=="Byuniversity"]<-"Brigham Young"
  x[x=='Universityab']<-"Uab"
  x[x=="Georgia South"]<-'Georgia Southern'
  x[x=='Loyola Mary']<-"Loyola Marymount"
  x[x=='Cent Michigan']<-"Central Michigan"
  x[x=="Coast Carolina"]<-"Coastal Carolina"
  x[x=="Florida Atl"]<-"Florida Atlantic"
  x[x=='Ut Rio Grande']<-"Texas Rgv"
  x[x=="Fla Gulf Coast"]<-"Florida Gulf Coast"
  x[x=="Cent Conn State"]<-"Central Connecticut State"
  x[x=="Bethune Cook"]<-"Bethune Cookman"
  x[x=="North Carolina Cent"]<-"North Carolina Central"
  x[x=='Bryant University']<-"Bryant"
  x[x=="Csu"]<-"Charleston Southern"
  x[x=="Georgetown Dc"]<-"Georgetown"
  x[x=='Providence Ri']<-"Providence"
  x[x=="Xavier Oh"]<-'Xavier'
  x[x=="Saint Louis University"]<-"Saint Louis"
  x[x=='Coll Of Charleston']<-"College of Charleston"
  x[x=="Hawai`i"]<-'Hawaii'
  x[x=="Texas Rio Grande Va"]<-"Texas Rgv"
  x[x=='Pacific Ca']<-"Pacific"
  x[x=="Holy Cross Ma"]<-"Holy Cross"
  x[x=="Cornell Ny"]<-"Cornell"
  x[x=="Robert Morris Pa"]<-"Robert Morris"
  x[x=='Arkansas Little Roc']<-"Little Rock"
  x[x=="Jacksonville Fl"]<-"Jacksonville"
  x[x=="Niagara Ny"]<-"Niagara"
  x[x=="Columbia Ny"]<-"Columbia"
  x[x=="Virginitech"]<-"Virginia Tech"
  x[x=="Floridstate"]<-"Florida State"
  x[x=="Arizonstate"]<-"Arizona State"
  x[x=="North Carolinstate"]<-"North Carolina State"
  x[x=="Iowstate"]<-"Iowa State"
  
  
  
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
  x<-sapply(x, simpleCap)
  x<-unname(x)
  
  x
  
}
