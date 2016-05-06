# Eva, 7/19/2015 #

rm(list=ls())
library(doBy)
library(ggplot2)
library(car)
library(lme4)
library(languageR)

#read data in
data <- read.csv("hashed_1b_EstimatesBetwSubj.csv")
head(data)

#drop columns
str(data)
#kick out NAs
length(complete.cases(data))
completecases <- na.omit(data)
str(completecases)
#kick out fillers
pairs = droplevels(subset(completecases, eventCat!='filler' & english=='yes'))
str(pairs)

completecases$eventCat <- as.character(completecases$eventCat)
completecases$eventCat[completecases$eventCat=="DM"] <- "durative mass\n(advise - to give advice)"
completecases$eventCat[completecases$eventCat=="DC"] <- "durative count\n(talk - to give a talk)"
completecases$eventCat[completecases$eventCat=="PC"] <- "punctive count\n(kiss - to give a kiss)"
completecases <- droplevels(subset(completecases, construction!='filler' & english=='yes'))
completecases$category <- as.character(completecases$construction)
completecases$category[completecases$construction=="BV"] <- "transitive verb"
completecases$category[completecases$construction=="LVC"] <- "ditransitive\nlight verb"
str(completecases)

with(completecases, tapply(time==0, list(eventCat, category), sum))
with(completecases, tapply(time==0, list(eventCat, category), mean))
with(completecases, tapply(time==0, list(eventCat, category), function(x) sd(x)/sqrt(length(x)-1)))
completecases <- droplevels(subset(completecases, time!=0))

#log-transform estimates
completecases$logseconds <- log(completecases$time)

#means by condition
conditiononlymeans <- summaryBy(logseconds ~ eventCat + category , FUN=c(mean,sd), data= completecases)
conditiononlymeans


completecases$eventCat <- as.factor(completecases$eventCat)
print(levels(completecases$eventCat)) 
completecases$eventCat = factor(completecases$eventCat,levels(completecases$eventCat)[c(3,1,2)])

## condition means, specified as means and standard errors of condition-specific subject means
completecases.bysubj <- with(completecases,aggregate(list(logseconds=logseconds),list(category=category,eventCat=eventCat,workerId=workerId),mean))
with(completecases.bysubj,tapply(logseconds,list(category,eventCat),length))  ## per participant

bysubj.means <- with(completecases.bysubj,tapply(logseconds,list(category,eventCat),mean))  ## mean # hours, for our understanding
bysubj.ses <- with(completecases.bysubj,tapply(logseconds,list(category,eventCat),function(x) sd(x)/sqrt(length(x)-1)))  ## mean # hours, for our understanding

exp(bysubj.means)/60  ## mean # minutes, for our understanding
exp(bysubj.means+bysubj.ses)/60 - exp(bysubj.means)/60
exp(bysubj.means)/60 - exp(bysubj.means-bysubj.ses)/60


library(lattice)
with(completecases.bysubj,densityplot(~logseconds | factor(paste(category,eventCat)),completecases.bysubj,layout=c(3,2)))

## check how many trials of each condition each subject got
with(completecases,tapply(logseconds,list(workerId,category,eventCat),length)) 
with(completecases,tapply(logseconds,list(workerId,category,eventCat),length))[1,,] 
with(completecases, xtabs(~eventCat+event))


y.ticks <- c(1,10, 60,600,3600)
Category <- factor(completecases$category)
Category <- relevel(Category,levels(Category)[2])

bar <- ggplot(completecases, aes(x=eventCat,y=logseconds, fill = Category, ))
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
#  facet_wrap(~eventCat, scales ="free")
ggsave("AllEventsBar.pdf", height=8, width=12,units="in")

y.ticks <- c(1,10, 60,600,3600)
Category <- factor(completecases.bysubj$category)
Category <- relevel(Category,levels(Category)[2])

bysubjbar <- ggplot(completecases.bysubj, aes(x=eventCat,y=logseconds, fill = Category, ))
bysubjbar + theme_bw() + stat_summary(fun.y= mean, geom = "bar", colour="black", position = "dodge") + 
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
#  facet_wrap(~eventCat, scales ="free")
ggsave("AllEventsBarBySubjects.pdf", height=8, width=12,units="in")

bar <- ggplot(completecases, aes(x=eventCat,y=logseconds, fill = Category, ))
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
        axis.title.x = element_text(size=20))+
  scale_y_continuous(breaks=log(y.ticks), labels = c("1 second","10 seconds", "1 minute","10 minutes","1 hour"))+
  labs(x="", y="Estimated Event Duration in Seconds, log scale", fill="Construction")
#  facet_wrap(~eventCat, scales ="free")
ggsave("AllEventsBar.pdf", height=8, width=12,units="in")



#######CONTINUE WITH STATS##############
#TEST FOR MAIN EFFECT -- ROGER's PAPER

completecases$category <- as.factor(completecases$category)
completecases$category.numeric <- sapply(completecases$category,function(i) contr.sum(2)[i,])

completecases$eventCat <- as.factor(completecases$eventCat)
eventCat.numeric <- sapply(completecases$eventCat,function(i) contr.helmert(3)[i,])
#eventCat.numeric
completecases$eventCat1 <- eventCat.numeric[1,]
completecases$eventCat2 <- eventCat.numeric[2,]

#use these results 
system.time(m.full.roger <- lmer(logseconds ~ 1 + category.numeric*(eventCat1+eventCat2) + (1|workerId) + (0+ category.numeric|workerId) + (1 | event) + (0+ category.numeric|event), data=completecases,REML=F))
system.time(m.noEventCat.roger <- lmer(logseconds ~ 1 + category.numeric*(eventCat1+eventCat2) - (eventCat1+eventCat2) + (1|workerId) + (0+ category.numeric|workerId) + (1 | event) + (0+ category.numeric|event), data=completecases,REML=F))
anova(m.noEventCat.roger,m.full.roger)

system.time(m.noCategory.roger <- lmer(logseconds ~ 1 + category.numeric*(eventCat1+eventCat2) - category.numeric + (1|workerId) + (0+ category.numeric|workerId) + (1 | event) + (0+ category.numeric|event), data=completecases,REML=F))
anova(m.noCategory.roger,m.full.roger)

system.time(m.noInteraction.roger <- lmer(logseconds ~ 1 + category.numeric+(eventCat1+eventCat2) + (1|workerId) + (0+ category.numeric|workerId) + (1 | event) + (0+ category.numeric|event), data=completecases,REML=F))
anova(m.noInteraction.roger,m.full.roger)



system.time(m.full <- lmer(logseconds ~ 1 + category.numeric*(eventCat1+eventCat2) + (1|workerId) + (0+ category.numeric|workerId) +(0+eventCat1|workerId) + (0+eventCat2|workerId) + (0 + category.numeric:eventCat1|workerId) + (0+category.numeric:eventCat2|workerId) + (1 | event) + (0+ category.numeric|event), data=completecases,REML=F))
system.time(m.noEventCat <- lmer(logseconds ~ 1 + category.numeric + category.numeric:(eventCat1+eventCat2) + (1|workerId) + (0+ category.numeric|workerId) +(0+eventCat1|workerId) + (0+eventCat2|workerId) + (0 + category.numeric:eventCat1|workerId) + (0+category.numeric:eventCat2|workerId) + (1 | event) + (0+ category.numeric|event), data=completecases,REML=F))
xtable(anova(m.noEventCat,m.full), digits=3)

system.time(m.noCategory <- lmer(logseconds ~ 1 + eventCat1+eventCat2 + category.numeric:(eventCat1+eventCat2) + (1|workerId) + (0+ category.numeric|workerId) +(0+eventCat1|workerId) + (0+eventCat2|workerId) + (0 + category.numeric:eventCat1|workerId) + (0+category.numeric:eventCat2|workerId) + (1 | event) + (0+ category.numeric|event), data=completecases,REML=F))
anova(m.noCategory,m.full)

system.time(m.noInteraction <- lmer(logseconds ~ 1 + eventCat1+eventCat2 + category.numeric + (1|workerId) + (0+ category.numeric|workerId) +(0+eventCat1|workerId) + (0+eventCat2|workerId) + (0 + category.numeric:eventCat1|workerId) + (0+category.numeric:eventCat2|workerId) + (1 | event) + (0+ category.numeric|event), data=completecases,REML=F))
anova(m.noInteraction,m.full)


# Roger's analyses with "maximal but no more" random effects structure
system.time(m.full.roger <- lmer(logseconds ~ 1 + category.numeric*(eventCat1+eventCat2) + (1|workerId) + (0+ category.numeric|workerId) + (1 | event) + (0+ category.numeric|event), data=completecases,REML=F))
system.time(m.noEventCat.roger <- lmer(logseconds ~ 1 + category.numeric*(eventCat1+eventCat2) - (eventCat1+eventCat2) + (1|workerId) + (0+ category.numeric|workerId) + (1 | event) + (0+ category.numeric|event), data=completecases,REML=F))
anova(m.noEventCat.roger,m.full.roger)

system.time(m.noCategory.roger <- lmer(logseconds ~ 1 + category.numeric*(eventCat1+eventCat2) - category.numeric + (1|workerId) + (0+ category.numeric|workerId) + (1 | event) + (0+ category.numeric|event), data=completecases,REML=F))
anova(m.noCategory.roger,m.full.roger)

system.time(m.noInteraction.roger <- lmer(logseconds ~ 1 + category.numeric+(eventCat1+eventCat2) + (1|workerId) + (0+ category.numeric|workerId) + (1 | event) + (0+ category.numeric|event), data=completecases,REML=F))
anova(m.noInteraction.roger,m.full.roger)


#Separate Data for estDur
#### subset to targets only #####
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





