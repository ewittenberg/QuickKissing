# Eva, 7/19/2015 #

rm(list=ls())
library(doBy)
library(ggplot2)
library(car)
library(lme4)
library(languageR)
library(xtable)


#read data in
rts <- read.csv("FormattedData.csv")
#drop columns
str(rts)
#kick out NAs
length(complete.cases(rts))
completecases <- na.omit(rts)
str(completecases)

length(unique(completecases$workerId))
#kick out fillers
pairs <-completecases[completecases$pair %in% c(1:50), ]
length(complete.cases(pairs))
completecases <- na.omit(pairs)
str(completecases)
completecases$eventCat[pairs$event=="thanking"] <- "kindness"
completecases <-subset(completecases, category!="filler" & event!="respecting" & event!="condoling" & event!="punching" & event!="sermoning"  & event!="approving" & event!="helping")
completecases$eventCat <- as.character(completecases$eventCat)
completecases$eventCat[completecases$eventCat=="kindness"] <- "durative mass\n(advise - to give advice)"
completecases$eventCat[completecases$eventCat=="utterance"] <- "durative count\n(talk - to give a talk)"
completecases$eventCat[completecases$eventCat=="contact"] <- "punctive count\n(kiss - to give a kiss)"
completecases$category <- as.character(completecases$category)
completecases$category[completecases$category=="BV"] <- "transitive verb"
completecases$category[completecases$category=="LVC"] <- "ditransitive\nlight verb"

#look at how many zero-second answers there are:
with(completecases, tapply(seconds==0, list(eventCat, category), sum))
with(completecases, tapply(seconds==0, list(eventCat, category), mean))
with(completecases, tapply(seconds==0, list(eventCat, category), function(x) sd(x)/sqrt(length(x)-1)))
completecases <- droplevels(subset(completecases, seconds!=0))


#log-transform RTs 
completecases$logseconds <- log(completecases$seconds)
#means by condition
conditiononlymeans <- summaryBy(logseconds ~ eventCat + category , FUN=c(mean,sd), data= completecases)
conditiononlymeans

#recoding
completecases$eventCat <- as.factor(completecases$eventCat)
print(levels(completecases$eventCat)) 
completecases$eventCat = factor(completecases$eventCat,levels(completecases$eventCat)[c(3,1,2)])
print(levels(completecases$eventCat)) 


## check how many trials of each condition each subject got
with(completecases,tapply(logseconds,list(workerId,category,eventCat),length)) 
with(completecases, xtabs(~eventCat+event))

## condition means, specified as means and standard errors of condition-specific subject means
completecases.bysubj <- with(completecases,aggregate(list(logseconds=logseconds),list(category=category,eventCat=eventCat,workerId=workerId),mean))
with(completecases.bysubj,tapply(logseconds,list(category,eventCat),length))  ## mean # hours, for our understanding

bysubj.means <- with(completecases.bysubj,tapply(logseconds,list(category,eventCat),mean))  ## mean # hours, for our understanding
bysubj.ses <- with(completecases.bysubj,tapply(logseconds,list(category,eventCat),function(x) sd(x)/sqrt(length(x)-1)))  ## mean # hours, for our understanding

exp(bysubj.means)/60  ## mean # hours, for our understanding
exp(bysubj.means+bysubj.ses)/60 - exp(bysubj.means)/60
exp(bysubj.means)/60 - exp(bysubj.means-bysubj.ses)/60


#library(lattice)
#with(completecases.bysubj,densityplot(~logseconds | factor(paste(category,eventCat)),completecases.bysubj,layout=c(3,2)))
y.ticks <- c(1,10, 60,600,3600)
Category <- factor(completecases.bysubj$category)
Category <- relevel(Category,levels(Category)[2])

completecases.bysubj$category[completecases.bysubj$category=="transitive verb"] <- "transitive verb \n"
bar <- ggplot(completecases.bysubj, aes(x=eventCat,y=logseconds, fill = Category, ))
bar + theme_bw() + stat_summary(fun.y= mean, geom = "bar", colour="black", position = "dodge") + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=.9), width= 0.4) +
  scale_fill_manual(values=c("#253494", "#2ca25f"),name="Construction")  + 
  scale_colour_manual(values=c("grey50", "grey80", "black")) + 
  theme(axis.text.y = element_text(size=16), 
        axis.text.x = element_text(size=16),
        strip.text.x = element_text(size=20),
        axis.title.y = element_text(size=20),
        legend.title = element_text(size=14),
        legend.text = element_text(size=14),
        axis.title.x = element_text(size=20),
        legend.justification=c(0,1), legend.position=c(0,1))+
  scale_y_continuous(breaks=log(y.ticks), labels = c("1 second","10 seconds", "1 minute","10 minutes","1 hour"))+
  labs(x="", y="Estimated Event Duration in Seconds, log scale", fill="Construction")
ggsave("AllEventsBar.pdf", height=8, width=12,units="in")

barTALK <- ggplot(completecases.bysubj, aes(x=eventCat,y=logseconds, fill = Category, ))
barTALK + theme_bw() + stat_summary(fun.y= mean, geom = "bar", colour="black", position = "dodge") + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=.9), width= 0.4) +
  scale_fill_manual(values=c("#253494", "#2ca25f"),name="Construction")  + 
  scale_colour_manual(values=c("grey50", "grey80", "black")) + 
  theme(axis.text.y = element_text(size=26), 
        axis.text.x = element_text(size=26),
        strip.text.x = element_text(size=30),
        axis.title.y = element_text(size=30),
        legend.title = element_text(size=24),
        legend.text = element_text(size=24),
        axis.title.x = element_text(size=30),
        legend.position="bottom")+
  scale_y_continuous(breaks=log(y.ticks), labels = c("1 second","10 seconds", "1 minute","10 minutes","1 hour"))+
  labs(x="", y="Estimated Event Duration in Seconds, \nlog scale", fill="Construction")
ggsave("AllEventsBarTALK.pdf", height=9, width=18,units="in")

#TEST FOR MAIN EFFECT -- ROGER's PAPER

completecases$category <- as.factor(completecases$category)
completecases$category.numeric <- sapply(completecases$category,function(i) contr.sum(2)[i,])

completecases$eventCat <- as.factor(completecases$eventCat)
eventCat.numeric <- sapply(completecases$eventCat,function(i) contr.helmert(3)[i,])
#eventCat.numeric
completecases$eventCat1 <- eventCat.numeric[1,]
completecases$eventCat2 <- eventCat.numeric[2,]

#use these results 
library(nlme)
system.time(m.full <- lmer(logseconds ~ 1 + category.numeric*(eventCat1+eventCat2) + (1|workerId) + (0+ category.numeric|workerId) +(0+eventCat1|workerId) + (0+eventCat2|workerId) + (0 + category.numeric:eventCat1|workerId) + (0+category.numeric:eventCat2|workerId) + (1 | event) + (0+ category.numeric|event), data=completecases,REML=F))
system.time(m.noEventCat <- lmer(logseconds ~ 1 + category.numeric + category.numeric:(eventCat1+eventCat2) + (1|workerId) + (0+ category.numeric|workerId) +(0+eventCat1|workerId) + (0+eventCat2|workerId) + (0 + category.numeric:eventCat1|workerId) + (0+category.numeric:eventCat2|workerId) + (1 | event) + (0+ category.numeric|event), data=completecases,REML=F))
anova(m.noEventCat,m.full)

system.time(m.noCategory <- lmer(logseconds ~ 1 + eventCat1+eventCat2 + category.numeric:(eventCat1+eventCat2) + (1|workerId) + (0+ category.numeric|workerId) +(0+eventCat1|workerId) + (0+eventCat2|workerId) + (0 + category.numeric:eventCat1|workerId) + (0+category.numeric:eventCat2|workerId) + (1 | event) + (0+ category.numeric|event), data=completecases,REML=F))
anova(m.noCategory,m.full)

system.time(m.noInteraction <- lmer(logseconds ~ 1 + eventCat1+eventCat2 + category.numeric + (1|workerId) + (0+ category.numeric|workerId) +(0+eventCat1|workerId) + (0+eventCat2|workerId) + (0 + category.numeric:eventCat1|workerId) + (0+category.numeric:eventCat2|workerId) + (1 | event) + (0+ category.numeric|event), data=completecases,REML=F))
anova(m.noInteraction,m.full)


#Separate Data for estDur
#### subset #####
PC = subset(completecases, eventCat %in% c('punctive count\n(kiss - to give a kiss)'))
DC = subset(completecases, eventCat %in% c('durative count\n(talk - to give a talk)'))
DM = subset(completecases, eventCat %in% c('durative mass\n(advise - to give advice)'))

PC_m <- lmer(logseconds ~ 1 + category + (category|workerId) + (category|event), data=PC,REML=F)
PC_m0 <- lmer(logseconds ~ 1 + (category|workerId) + (category|event), data=PC,REML=F)
anova(PC_m0,PC_m)

DC_m <- lmer(logseconds ~ 1 + category + (category|workerId) + (category|event), data=DC,REML=F)
DC_m0 <- lmer(logseconds ~ 1 + (category|workerId) + (category|event), data=DC,REML=F)
anova(DC_m0,DC_m)

DM_m <- lmer(logseconds ~ 1 + category + (category|workerId) + (category|event), data=DM,REML=F)
DM_m0 <- lmer(logseconds ~ 1 + (category|workerId) + (category|event), data=DM,REML=F)
anova(DM_m0,DM_m)




