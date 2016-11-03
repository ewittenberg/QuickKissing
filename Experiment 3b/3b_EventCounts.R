library(doBy)
library(gmodels)
library(MASS)
library(ggplot2)
library(Hmisc)
library(lme4)
library(car)
library(languageR)
library(car)
library(ordinal)
library(dplyr)
rm(list=ls(all=TRUE))  
#read data in
getwd()
data <- read.csv("hashed_FormattedBatchHowMany.csv")
length(unique(data$workerId))
#this is the dataset in which we asked for specific events -- i.e., how many kisses
####CLEANING DATA ####
head(data)
#drop columns
str(data)
#kick out NAs
length(complete.cases(data))
completecases <- na.omit(data)
str(completecases)
head(completecases)

#kick out fillers
pairs <-completecases[completecases$pair %in% c(1:30), ]
pairs = subset(pairs, category!='filler')
pairs$eventCat[pairs$event=="thanking"] <- "kindness"

length(complete.cases(pairs))
completecases <- na.omit(pairs)
str(completecases)
completecases <-subset(completecases, category!="filler" & event!="respecting" & event!="condoling" & event!="punching" & event!="sermoning"  & event!="approving" & event!="helping")
completecases$eventCat <- as.character(completecases$eventCat)
completecases$eventCat[completecases$eventCat=="kindness"] <- "durative mass\n(advise - to give advice)"
completecases$eventCat[completecases$eventCat=="utterance"] <- "durative count\n(talk - to give a talk)"
completecases$eventCat[completecases$eventCat=="contact"] <- "punctive count\n(kiss - to give a kiss)"
completecases$category <- as.character(completecases$category)
completecases$category[completecases$category=="BV"] <- "transitive verb"
completecases$category[completecases$category=="LVC"] <- "ditransitive\nlight verb"
completecases$event <- droplevels(completecases$event)


with(completecases, tapply(how.many==0, list(eventCat, category), sum))
with(completecases, tapply(how.many==0, list(eventCat, category), mean))
with(completecases, tapply(how.many==0, list(eventCat, category), function(x) sd(x)/sqrt(length(x)-1)))
completecases <- droplevels(subset(completecases, how.many!=0))

completecases$log.how.many <- log(completecases$how.many)
completecases$eventCat <- as.factor(completecases$eventCat)
print(levels(completecases$eventCat)) 
completecases$eventCat = factor(completecases$eventCat,levels(completecases$eventCat)[c(3,1,2)])
completecases$exp.how.many <- exp(completecases$log.how.many)


#grand mean (by subjects and then by conditions)
se <- function(x,na.rm=F) {
  if(na.rm) x <- x[!is.na(x)]
  sd(x)/sqrt(length(x))
}
completecases.bysubj <- summarise(group_by(completecases,eventCat,workerId,category),subj.mean.log=mean(log.how.many))
print(completecases.summaryStats <- summarise(group_by(completecases.bysubj,eventCat,category),
                                              grand.mean.log=mean(subj.mean.log),
                                              SE=se(subj.mean.log),
                                              grand.mean=exp(mean(subj.mean.log))))


#### GRAPHING ####
y.ticks <- c(1,1.5,2,2.5,3)
Category <- factor(completecases$category)
Category <- relevel(Category,levels(Category)[2])
bar <- ggplot(completecases, aes(x=eventCat,y=log(how.many), fill = Category, ))
bar + theme_bw() + stat_summary(fun.y= mean, geom = "bar", colour="black", position = "dodge") + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=.9), width= 0.4) +
  scale_fill_manual(values=c("#253494", "#2ca25f"),name="Construction")  + 
  scale_colour_manual(values=c("grey50", "grey80", "black")) + 
  theme(axis.text.y = element_text(size=16), 
        axis.text.x = element_text(size=16),
        strip.text.x = element_text(size=24),
        axis.title.y = element_text(size=24),
        legend.title = element_text(size=14),
        legend.text = element_text(size=14),
        axis.title.x = element_text(size=24),
        legend.justification=c(0,1), legend.position=c(0,1))+
  labs(x="", y="Event Counts, on log scale", fill="Construction")+
  #scale_y_continuous(breaks=c(0.0,.3,.6,.9), labels = c("0","~1.3","~1.8","~2.5"))+
  scale_y_continuous(breaks=log(y.ticks), labels = y.ticks)
#  facet_wrap(~eventCat, scales ="free")
ggsave("EventCountsBar.pdf", width=12, height=8, unit='in')


#######CONTINUE WITH STATS##############
#TEST FOR MAIN EFFECT -- ROGER's PAPER
completecases$category <- as.factor(completecases$category)
completecases$category.numeric <- sapply(completecases$category,function(i) contr.sum(2)[i,])

completecases$eventCat <- as.factor(completecases$eventCat)
eventCat.numeric <- sapply(completecases$eventCat,function(i) contr.helmert(3)[i,])
#eventCat.numeric
completecases$eventCat1 <- eventCat.numeric[1,]
completecases$eventCat2 <- eventCat.numeric[2,]

#model with full effect structure
system.time(m.full <- lmer(log(how.many) ~ 1 + category.numeric*(eventCat1+eventCat2) + (1|workerId) + 
                             (0+ category.numeric|workerId) +(0+eventCat1|workerId) + 
                             (0+eventCat2|workerId) + (0 + category.numeric:eventCat1|workerId) + 
                             (0+category.numeric:eventCat2|workerId) + (1 | event) + (0+ category.numeric|event), 
                              data=completecases,REML=F))
system.time(m.noEventCat <- lmer(log(how.many) ~ 1 + category.numeric + category.numeric:(eventCat1+eventCat2) + (1|workerId) + (0+ category.numeric|workerId) +(0+eventCat1|workerId) + (0+eventCat2|workerId) + (0 + category.numeric:eventCat1|workerId) + (0+category.numeric:eventCat2|workerId) + (1 | event) + (0+ category.numeric|event), data=completecases,REML=F))
anova(m.noEventCat,m.full)
system.time(m.noCategory <- lmer(log(how.many) ~ 1 + eventCat1+eventCat2 + category.numeric:(eventCat1+eventCat2) + (1|workerId) + (0+ category.numeric|workerId) +(0+eventCat1|workerId) + (0+eventCat2|workerId) + (0 + category.numeric:eventCat1|workerId) + (0+category.numeric:eventCat2|workerId) + (1 | event) + (0+ category.numeric|event), data=completecases,REML=F))
anova(m.noCategory,m.full)
system.time(m.noInteraction <- lmer(log(how.many) ~ 1 + eventCat1+eventCat2 + category.numeric + (1|workerId) + (0+ category.numeric|workerId) +(0+eventCat1|workerId) + (0+eventCat2|workerId) + (0 + category.numeric:eventCat1|workerId) + (0+category.numeric:eventCat2|workerId) + (1 | event) + (0+ category.numeric|event), data=completecases,REML=F))
anova(m.noInteraction,m.full)

#### Separate Data for estDur #####
PC = subset(completecases, eventCat %in% c('punctive count\n(kiss - to give a kiss)'))
DC = subset(completecases, eventCat %in% c('durative count\n(talk - to give a talk)'))
DM = subset(completecases, eventCat %in% c('durative mass\n(advise - to give advice)'))

PC_m <- lmer(log(how.many) ~ 1 + category + (category|workerId) + (category|event), data=PC,REML=F)
PC_m0 <- lmer(log(how.many) ~ 1 + (category|workerId) + (category|event), data=PC,REML=F)
anova(PC_m0,PC_m)

DC_m <- lmer(log(how.many) ~ 1 + category + (category|workerId) + (category|event), data=DC,REML=F)
DC_m0 <- lmer(log(how.many) ~ 1 + (category|workerId) + (category|event), data=DC,REML=F)
anova(DC_m0,DC_m)

DM_m <- lmer(log(how.many) ~ 1 + category + (category|workerId) + (category|event), data=DM,REML=F)
DM_m0 <- lmer(log(how.many) ~ 1 + (category|workerId) + (category|event), data=DM,REML=F)
anova(DM_m0,DM_m)
