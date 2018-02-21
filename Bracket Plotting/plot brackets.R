##LOAD DATA####
#only the default bracketresults are uploaded on github
#to get other cases i.e. _v2 _v3, need to rerun sims and ownership using model-fitting_v2 and model-fitting_v3 as save file names as _v2, _v3.

setwd("~/Kaggle/NCAA/march-madness")
projDir<-getwd()

year<-2016
setwd(paste0(c(projDir, "/", year, "/"), sep="", collapse=""))
load("alldata.RData")
load("BracketResults_FullTournament_v2.Rda")
load("TourneySims_v2.Rda")

##plotting function--calcBracket(), plotBracket() and more
source(paste0(projDir, "/Bracket Plotting/plotting function.R"))

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
  sapply(1:length(slots), function(x) inspect[inspect$Team_Full==teams[x],slots[x]])
}
cols<-colnames(brackets[, 1:63]) %>% substring(., 1, 2)
expectedPoints(slots=cols,  teams=brackets[1, 1:63])
brackets[, paste("Expected", colnames(brackets[1:63]), sep="")]<-NA
brackets[, paste("Expected",colnames(brackets[1:63]), sep="")]<-
  matrix(unlist(lapply(1:nrow(brackets), function(x) expectedPoints(cols,teams=brackets[x, 1:63]))), byrow=T, ncol=63)
brackets[, c("ExpectedR1234", "ExpectedR123", "ExpectedR12")]<-NULL
for(i in c("R1", "R2", "R3", "R4","R5")){
  brackets[, paste0("Expected", i)]<-NULL
  brackets[, paste0("Expected", i)]<-apply(brackets[, grepl(paste0("Expected", i), colnames(brackets))], 1, sum)
}
brackets$ExpectedR1234<-round(brackets$ExpectedR1*10+brackets$ExpectedR2*20+brackets$ExpectedR3*40+brackets$ExpectedR4*80, 1)
brackets$ExpectedR123<-round(brackets$ExpectedR1*10+brackets$ExpectedR2*20+brackets$ExpectedR3*40, 1)
brackets$ExpectedR12<-round(brackets$ExpectedR1*10+brackets$ExpectedR2*20, 1)
hist(brackets$ExpectedR1[order(brackets$Prob995, decreasing = T)][1:50])

ownershipFun<-function(slots, teams){
  teams<-as.character(teams)
  sapply(1:length(slots), function(x) ownership[ownership$Team_Full==teams[x],slots[x]])
}
cols<-colnames(brackets[, 1:63]) %>% substring(., 1, 2)
ownershipFun(slots=cols,  teams=brackets[1, 1:63])
brackets[, paste("Ownership", colnames(brackets[ 1:63]), sep="")]<-NA
brackets[, paste("Ownership",colnames(brackets[ 1:63]), sep="")]<-
  matrix(unlist(lapply(1:nrow(brackets), function(x) ownershipFun(cols,teams=brackets[x, 1:63]))), byrow=T, ncol=63)
brackets$OwnershipR123<-NULL
for(i in c("R1", "R2", "R3", "R4","R5")){
  brackets[, paste0("Ownership", i)]<-NULL
  
  cols<-colnames(brackets)[grepl(paste0("Ownership", i), colnames(brackets))]
  test<-rowSums(sapply(cols, function(x) brackets[, gsub("Ownership", "Expected",x)]/brackets[, x]))
  brackets[, paste0("ExpectedScaled", i)]<-test
  
  brackets[, paste0("Ownership", i)]<-apply(brackets[,cols], 1, sum)
  
}
brackets$OwnershipR123<-brackets$OwnershipR1*10+brackets$OwnershipR2*20+brackets$OwnershipR3*40

summary(lm(Prob995~ ExpectedR123+OwnershipR123+
             ExpectedR4+OwnershipR4+
             OwnershipR5*ExpectedR5+
             I(ExpectedR6CH/OwnershipR6CH)+ExpectedR6CH, data=brackets))


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


brackets<-brackets[order(brackets$SimMean, decreasing = T), ]
# opt<-which(brackets$Prob995==brackets$Prob995[order(brackets$Prob995, decreasing = T)][1])
opt<-which.max(brackets$Prob99)
# opt<-which(result$x[1:ncol(percentiles)]==1)

bracket<-brackets[opt[1], 1:63]


###CUSTOM BRACKET CALCULATION######
# bracket
opt<-which.max(brackets$Prob99)
plotBracket(brackets[which.max(brackets$ExpectedR12), 1:63])
customBracket<-cbind(brackets[which.max(brackets$ExpectedR12), 1:48], brackets[opt, 49:63])
plotBracket(brackets[opt, 1:63])

#argument1= brackets, argument2=all brackets. returns point scared & 
prob99<-brackets[ opt,colnames(brackets)%in% c("ExpectedR12", "Prob99")]
max12<-brackets[ which.max(brackets$ExpectedR12),colnames(brackets)%in% c("ExpectedR12", "Prob99")]
custom<-calcBracket(customBracket, brackets=brackets[-opt, ])
custom$ExpectedR12<-max12$ExpectedR12

test<-rbindlist(list(prob99, max12, custom[, c("Prob99", "ExpectedR12")]), fill=T)
test$Bracket<-c("Prob99", "MaxR12", "Custom")
test
# test<-brackets[order(brackets$Prob995, decreasing = T), ][1:10, cols]
# test[, 1:7]<-sapply(test[, 1:7], pasteSeed)
# test

###PLOTTING TOP 20 BRACKETS BY OPTIMIZATION GROUP####

cols<-c(colnames(brackets)[57:63], c("ExpectedR123", "ExpectedR1", "ExpectedR2", "ExpectedR3","ExpectedR4", "ExpectedR5", "ExpectedR6CH", 
                                     "OwnershipR1" , "OwnershipR2", "OwnershipR3", "OwnershipR4", "OwnershipR5", "OwnershipR6CH", 
                                     "OwnershipR123",
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
            OwnershipR1=mean(OwnershipR1)*10,  ExpectedR1=mean(ExpectedR1)*10, 
            OwnershipR2=mean(OwnershipR2)*20,  ExpectedR2=mean(ExpectedR2)*20, 
            OwnershipR3=mean(OwnershipR3)*40,  ExpectedR3=mean(ExpectedR3)*40, 
            OwnershipR4=mean(OwnershipR4)*80,  ExpectedR4=mean(ExpectedR4)*80, 
            OwnershipR5=mean(OwnershipR5)*160,  ExpectedR5=mean(ExpectedR5)*160,
            OwnershipR6CH=mean(OwnershipR6CH)*320,ExpectedR6CH=mean(ExpectedR6CH)*320)
test$Type<- ordered(test$Type, levels = c("ALL", "SimMean", "Prob90", "Prob97", "Prob995"))
test<-melt(test, id.vars = "Type")
test<-test[order(test$Type), ]

ggplot(test[grepl("Expected", test$variable), ], aes(fill=Type, y=value, x=variable)) + 
  geom_bar(position="dodge", stat="identity") +ggtitle("Summarizing the top 20 Brackets for each Optimization Type")
ggplot(test[grepl("Ownership", test$variable), ], aes(fill=Type, y=value, x=variable)) + 
  geom_bar(position="dodge", stat="identity")+ggtitle("Top 20 Brackets by Optimization Percentile")
head(test)
#   
# }
# pdf("~/myBrackets.pdf")
# for (i in which(result$x[1:ncol(percentiles)]==1)){
#   plotBracket(bracket = brackets[i, 1:63])  
# }
# dev.off()
# 
# 
