### Eva, July 31, 2015 ###

##### PACKAGING ##########
rm(list=ls())
library(doBy)
library(gmodels)
library(MASS)
library(ggplot2)
library(Hmisc)
library(lme4)
library(car)
library(languageR)
library(car)
library(xtable)
#read data in
getwd()
data <- read.csv("FormattedBatchSimilarity.csv")
str(data)

#kick out NAs
#kick out fillers & irrelevant items
#rename stuff
criticals <-subset(data,  ! eventCat %in% c("filler") & ! pair %in% c("#VALUE!"))
length(complete.cases(criticals))
completecases <- na.omit(criticals)
str(completecases)
completecases$eventCat[completecases$event=="thanking"] <- "kindness"
length(complete.cases(completecases))
completecases <- na.omit(completecases)
str(completecases)
completecases <-droplevels(subset(completecases, eventCat!="filler" & event!="respecting" & event!="condoling" & event!="punching" & event!="sermoning"  & event!="approving" & event!="helping"))
completecases$eventCat <- as.character(completecases$eventCat)
completecases$eventCat[completecases$eventCat=="kindness"] <- "durative mass\n(advise - to give advice)"
completecases$eventCat[completecases$eventCat=="utterance"] <- "durative count\n(talk - to give a talk)"
completecases$eventCat[completecases$eventCat=="contact"] <- "punctive count\n(kiss - to give a kiss)"
completecases$eventCat <- as.factor(completecases$eventCat)
print(levels(completecases$eventCat)) 
completecases$eventCat = factor(completecases$eventCat,levels(completecases$eventCat)[c(3,1,2)])
completecases$event <- droplevels(completecases$event)


#means by condition
conditiononlymeans <- summaryBy(similarity ~ eventCat , FUN=c(mean,sd), data= completecases)
conditiononlymeans

#means by condition and pair
pairmeans <- summaryBy(similarity ~  eventCat + event , FUN=c(mean,sd), data= completecases)
pairmeans
write.table(pairmeans,"pairmeans_N=40_Similar", col.names=NA)

##### GRAPHING #########

bar <- ggplot(completecases, aes(x=eventCat,y=similarity, fill = eventCat, ))
bar + theme_bw() + stat_summary(fun.y= mean, geom = "bar", colour="black", position = "dodge") + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=.9), width= 0.4) +
  scale_fill_manual(values=c("#de2d26", "#ffeda0", "#fc8d59"))  + 
  scale_colour_manual(values=c("grey50", "grey80", "black")) + 
  theme(axis.text.y = element_text(size=16), 
        axis.text.x = element_text(size=16),
        strip.text.x = element_text(size=24),
        axis.title.y = element_text(size=24),
        legend.title = element_text(size=14),
        legend.text = element_text(size=14),
        legend.position="none",
        axis.title.x = element_text(size=24))+
  labs(x="", y="Similarity Rating", fill="Event \nCategory")
ggsave("EventSimilarityBar.pdf", width=12, height=9, unit='in')

barTALK <- ggplot(completecases, aes(x=eventCat,y=similarity, fill = eventCat, ))
barTALK + theme_bw() + stat_summary(fun.y= mean, geom = "bar", colour="black", position = "dodge") + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=.9), width= 0.4) +
  scale_fill_manual(values=c("#de2d26", "#ffeda0", "#fc8d59"))  + 
  scale_colour_manual(values=c("grey50", "grey80", "black")) + 
  theme(axis.text.y = element_text(size=26), 
        axis.text.x = element_text(size=26),
        strip.text.x = element_text(size=30),
        axis.title.y = element_text(size=30),
        legend.title = element_text(size=24),
        legend.text = element_text(size=24),
        axis.title.x = element_text(size=30),
        legend.position="none")+
  scale_y_continuous(breaks=c(1,1.5,2))+
  labs(x="", y="Similarity Rating", fill="Event \nCategory")
ggsave("EventSimilarityBarTALK.pdf", width=12.5, height=9, unit='in')
##### STATS ##############
#### REGRESSION MODELS ####
#TEST FOR MAIN EFFECT -- ROGER's PAPER
completecases$eventCat <- as.factor(completecases$eventCat)
eventCat.numeric <- sapply(completecases$eventCat,function(i) contr.helmert(3)[i,])
#eventCat.numeric
completecases$eventCat1 <- eventCat.numeric[1,]
completecases$eventCat2 <- eventCat.numeric[2,]

#model with full effect structure
system.time(m.full <- lmer(similarity ~ 1 + eventCat + (1|workerId) +(0+eventCat1|workerId) + (0+eventCat2|workerId)  + (1 | event), data=completecases,REML=F))
system.time(m.noEventCat <- lmer(similarity ~ 1 + (1|workerId) +(0+eventCat1|workerId) + (0+eventCat2|workerId)  + (1 | event), data=completecases,REML=F))
anova(m.noEventCat,m.full)

#### Separate Data for eventCat #####
noPC = subset(completecases, ! eventCat %in% c('punctive count\n(kiss - to give a kiss)'))
noDC = subset(completecases, ! eventCat %in% c('durative count\n(talk - to give a talk)'))
noDM = subset(completecases, ! eventCat %in% c('durative mass\n(advise - to give advice)'))

system.time(noPC.full <- lmer(similarity ~ 1 + eventCat + (1|workerId) +(0+eventCat1|workerId) + (0+eventCat2|workerId)  + (1 | event), data=noPC,REML=F))
system.time(noPC.noEventCat <- lmer(similarity ~ 1 + (1|workerId) +(0+eventCat1|workerId) + (0+eventCat2|workerId)  + (1 | event), data=noPC,REML=F))
anova(noPC.full,noPC.noEventCat)

system.time(noDC.full <- lmer(similarity ~ 1 + eventCat + (1|workerId) +(0+eventCat1|workerId) + (0+eventCat2|workerId)  + (1 | event), data=noDC,REML=F))
system.time(noDC.noEventCat <- lmer(similarity ~ 1 + (1|workerId) +(0+eventCat1|workerId) + (0+eventCat2|workerId)  + (1 | event), data=noDC,REML=F))
xtabs(anova(noDC.full,noDC.noEventCat))

system.time(noDM.full <- lmer(similarity ~ 1 + eventCat + (1|workerId) +(0+eventCat1|workerId) + (0+eventCat2|workerId)  + (1 | event), data=noDM,REML=F))
system.time(noDM.noEventCat <- lmer(similarity ~ 1 + (1|workerId) +(0+eventCat1|workerId) + (0+eventCat2|workerId)  + (1 | event), data=noDM,REML=F))
anova(noDM.full,noDM.noEventCat)

save.image()
#### FILLERS ####
fillers = subset(data, category='filler')
fillers <-na.omit(fillers)

fillertable <- summaryBy(similarity ~ eventCat , FUN=c(mean,sd), data= fillers)
fillertable
max(fillers$similarity)
min(fillers$similarity)
histogram(fillers$similarity)


