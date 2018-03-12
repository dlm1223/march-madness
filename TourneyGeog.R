test1<-unique(fulldf[!is.na(fulldf$Slot), c("Slot", "Lat", "Lng", "Season")])


slots<-TourneySlots[TourneySlots$Season==2018, ]
locs<-c("Dayton", "Dayton", "Dayton", "Dayton",  #R0
        "Pittsburgh", "Detroit", "Dallas", "San Diego", "San Diego", "Dallas","Detroit", "Pittsburgh", #R1W East
        "Wichita", "Pittsburgh", "Detroit", "San Diego", "San Diego", "Detroit","Pittsburgh", "Wichita", #R1X Midwest
        "Charlotte", "Nashville", "Dallas", "Boise", "Boise", "Dallas","Nashville", "Charlotte", #R1Y South
        "Nashville", "Charlotte", "Wichita", "Boise", "Boise", "Wichita","Charlotte", "Nashville", #R1Z West
        
        "Pittsburgh", "Detroit", "Dallas", "San Diego",  #R2W East
        "Wichita", "Pittsburgh", "Detroit", "San Diego",  #R2X Midwest
        "Charlotte", "Nashville", "Dallas", "Boise",#R2Y South
        "Nashville", "Charlotte", "Wichita", "Boise",  #R2Z West
        
        "Boston", "Boston", "Omaha", "Omaha", "Atlanta", "Atlanta","Los Angeles", "Los Angeles",#R3
        
        "Boston", "Omaha", "Atlanta", "Los Angeles", #R4
        
        "San Antonio", "San Antonio", "San Antonio" #R5, R6
        )
slots$City<-locs
length(unique(locs))
nrow(CitiesEnriched[CitiesEnriched$City%in% locs,])
setdiff(locs, CitiesEnriched$City)

slots<-merge(slots, CitiesEnriched[, c("City", "State", "Lat", "Lng")], by="City")
test1<-rbind(test1, slots[, c("Slot", "Lat", "Lng", "Season")])
TourneyGeog<-test1
table(TourneyGeog$Season)
