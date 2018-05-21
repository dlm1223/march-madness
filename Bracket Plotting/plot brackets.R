##LOAD DATA####
#only the default bracketresults are uploaded on github
#to get other cases i.e. _v2 _v3, need to rerun sims and ownership using model-fitting_v2 and model-fitting_v3 as save file names as _v2, _v3.
input<-list(r1=10, r2=20, r3=40, r4=80, r5=160, r6=320, upset1_mult=1, upset2_mult=1, upset3_mult=1,
            r1_seed_mult=0, r2_seed_mult=0,r3_seed_mult=0, r4_seed_mult=0,r5_seed_mult=0, r6_seed_mult=0,
            r1_seed_bonus=0, r2_seed_bonus=0,r3_seed_bonus=0, r4_seed_bonus=0,r5_seed_bonus=0, r6_seed_bonus=0)



setwd("~/Kaggle/NCAA/march-madness")
load("data/game data.RData")
projDir<-getwd()

year<-2018
setwd(paste0(c(projDir, "/", year, "/"), sep="", collapse=""))

load("BracketResults_FullTournament_500sims.Rda")
load("TourneySims_500sims.Rda")

source(paste0(projDir, "/Bracket Plotting/plotting function.R"))

load("alldata.RData")


numSims<-sum(grepl("Sim", colnames(brackets)))-backtest
expected<-data.table(tourneySims[tourneySims$Sim<=numSims,])
expected<-expected[, list(Expected=sum(Payout)/numSims), by=c("Team_Full", "Round")]  
expected<-data.frame(expected)
expected<-merge(expected, analyze[, c("Team_Full", "Round", "Slot")], by=c("Team_Full", "Round"), all=T)
expected[is.na(expected)]<-0

inspect[order(inspect$R6,inspect$R5,inspect$R4,inspect$R3, inspect$R2,  decreasing = T), -7]

ownership[order(ownership$R6, ownership$R5, ownership$R4, ownership$R3, ownership$R2, ownership$R1, decreasing = T), -1]


###CALCULATE VARIABLES####
#calculate values for brackets like prob90, expectedvalue by round, etc

brackets$SimMean<-apply(brackets[, grepl("Sim", colnames(brackets)) & !grepl("Mean|SD", colnames(brackets))], 1, mean)
bool<-grepl("Percentile", colnames(brackets)) & !grepl("Actual", colnames(brackets))
brackets$Prob80<-apply(brackets[, bool], 1, function(x) sum(x>.80)/numSims)
brackets$Prob995<-apply(brackets[, bool], 1, function(x) sum(x>.995)/numSims)
brackets$Prob998<-apply(brackets[, bool], 1, function(x) sum(x>.998)/numSims)
brackets$SimSD<-apply(brackets[, grepl("Sim", colnames(brackets))], 1, sd)
expectedPoints<-function(slots, teams){
  teams<-as.character(teams)
  sapply(1:length(slots), function(x) expected$Expected[expected$Team_Full==teams[x]& expected$Round==slots[x]])
}
cols<-colnames(brackets[, 1:63]) %>% substring(., 1, 2)
expectedPoints(slots=cols,  teams=brackets[8, 1:63])
brackets[, paste("Expected", colnames(brackets[1:63]), sep="")]<-NA
brackets[, paste("Expected",colnames(brackets[1:63]), sep="")]<-
  matrix(unlist(lapply(1:nrow(brackets), function(x) expectedPoints(cols,teams=brackets[x, 1:63]))), byrow=T, ncol=63)
brackets[, c("ExpectedR1234", "ExpectedR123", "ExpectedR12")]<-NULL
for(i in c("R1", "R2", "R3", "R4","R5")){
  brackets[, paste0("Expected", i)]<-NULL
  brackets[, paste0("Expected", i)]<-apply(brackets[, grepl(paste0("Expected", i), colnames(brackets))], 1, sum)
}
hist(brackets$ExpectedR1[order(brackets$Prob995, decreasing = T)][1:50])
brackets$ExpectedR1234<-rowSums(brackets[,c("ExpectedR1", "ExpectedR2", "ExpectedR3", "ExpectedR4")])
brackets$ExpectedR123<-rowSums(brackets[,c("ExpectedR1", "ExpectedR2", "ExpectedR3")])
brackets$ExpectedR12<-rowSums(brackets[,c("ExpectedR1", "ExpectedR2")])
brackets$ExpectedR3456<-rowSums(brackets[,c( "ExpectedR3", "ExpectedR4", "ExpectedR5", "ExpectedR6CH")])
library(plot3D)
scatter3D(brackets$ExpectedR6, brackets$SimMean, brackets$Prob95, xlab="ExpectedR1", ylab="SimMean",zlab="Prob95")



###PLOTTING CHAMP BY GROUP####

# plot(brackets$SimSD~brackets$SimMean, main="500 Brackets, 500 Simulations", xlab="Mean Points", ylab="SD Points")
# text(brackets$Prob90~brackets$SimMean, labels =substring(brackets$R6CH, 1, 4), col=ife, cex=.75, pos=4)

yvar<-"Prob90"
plot(brackets[, yvar]~brackets$SimMean, main="5000 Brackets, 5000 Simulations", xlab="Mean Points", ylab=yvar)
brackets$Champ<-as.factor(ifelse(brackets$R6CH%in% c("Villanova", "Gonzaga","Louisville","West Virginia","Purdue",
                                                     "Kansas", "North Carolina"), brackets$R6CH, "Other"))
# brackets$Champ<-as.factor(ifelse(brackets$R6CH%in% c("Michigan State", "Oklahoma","West Virginia","Kentucky",
#                                                      "North Carolina","Virginia",
#                                                      "Kansas", "Villanova"), brackets$R6CH, "Other"))
brackets$RunnerUp<-ifelse(brackets$R6CH==brackets$R5WX, brackets$R5YZ,brackets$R5WX )

brackets$RunnerUp<-as.factor(ifelse(brackets$RunnerUp%in% c("Villanova", "Gonzaga","Louisville","West Virginia","Purdue","Kentucky",
                                                            "Kansas", "North Carolina"), brackets$RunnerUp, "Other"))
# brackets$RunnerUp<-as.factor(ifelse(brackets$RunnerUp%in% c("Michigan State", "Oklahoma","West Virginia","Kentucky",
#                                                      "North Carolina","Virginia",
#                                                      "Kansas", "Villanova"), brackets$RunnerUp, "Other"))

brackets<-brackets[order(brackets$Prob97, decreasing = T), ]
test<-brackets[1:round(nrow(brackets)/5), ]
ggplot(test, aes(x=SimMean, y=Prob97, group=(Champ))) +  
  geom_point(aes(color=Champ, shape=Champ)) + scale_color_brewer(palette="Set1")+
  scale_shape_manual(values=1:nlevels(brackets$Champ)) 
ggplot(test[test$Champ%in% "Gonzaga",], aes(x=SimMean, y=Prob97, group=(RunnerUp)))+
  geom_point(aes(color=RunnerUp, shape=RunnerUp)) + scale_color_brewer(palette="Set1")+
  scale_shape_manual(values=1:nlevels(as.factor(test$RunnerUp)) )



###PLOTTING TOP 20 BRACKETS BY OPTIMIZATION GROUP####

cols<-c(colnames(brackets)[57:63], c("ExpectedR123", "ExpectedR1", "ExpectedR2", "ExpectedR3","ExpectedR4", "ExpectedR5", "ExpectedR6CH", 
                                     # "OwnershipR1" , "OwnershipR2", "OwnershipR3", "OwnershipR4", "OwnershipR5", "OwnershipR6CH", 
                                     # "OwnershipR123",
                                     "SimMean", "Prob90","Prob995" ))
organize<-function(var){
  if(var=="ALL"){
    test<-brackets
    
  } else{
    test<-brackets[order(brackets[, var], decreasing = T), ][1:20, cols]
    test[, 1:7]<-sapply(test[, 1:7], pasteSeed)
  }
  test$Type<-var
  test
}
test<-ldply(lapply(c("SimMean", "Prob90", "Prob97", "Prob995", "ALL"), organize), data.frame)

# test<-data
test<-ddply(test, .(Type), summarize,
            # OwnershipR123=mean(OwnershipR123),   ExpectedR123=mean(ExpectedR123), 
            ExpectedR1=mean(ExpectedR1), 
            ExpectedR2=mean(ExpectedR2), 
            ExpectedR3=mean(ExpectedR3), 
            ExpectedR4=mean(ExpectedR4), 
            ExpectedR5=mean(ExpectedR5),
            ExpectedR6CH=mean(ExpectedR6CH))
test$Type<- ordered(test$Type, levels = c("ALL", "SimMean", "Prob90", "Prob97", "Prob995"))
test<-melt(test, id.vars = "Type")
test<-test[order(test$Type), ]

ggplot(test[grepl("Expected", test$variable), ], aes(fill=Type, y=value, x=variable)) + 
  geom_bar(position="dodge", stat="identity") +ggtitle("Summarizing the top 20 Brackets for each Optimization Type")
