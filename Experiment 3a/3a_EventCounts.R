### EVA JULY 2015 ####
#### EXPERIMENT 3 QuickKissing ###


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

#this is the dataset in which we asked for general events -- i.e., how many events
rm(list=ls())
#read data in
getwd()
data <- read.csv("hashed_FormattedBatch.csv")
head(data)
#drop columns
str(data)
#kick out NAs
length(complete.cases(data))
completecases <- na.omit(data)

#kick out fillers
pairs <-completecases[completecases$pair %in% c(1:30), ]
pairs = subset(pairs, category!='filler')
pairs$eventCat[pairs$event=="thanking"] <- "kindness"
pairs$event <- droplevels(pairs$event)
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


#means by condition
conditiononlymeans <- summaryBy(count ~ eventCat + category , FUN=c(mean,sd), data= completecases)
conditiononlymeans
xtable(conditiononlymeans)

#means by item
itemmeans <- summaryBy(count ~ event + category , FUN=c(mean,sd), data= completecases)



## densityplots
library(lattice)
with(subset(completecases,log10(count)<20), densityplot(~log10(count) | factor(paste(category,eventCat)),layout=c(3,2)))

with(subset(completecases,event=="recognizing"), densityplot(~log10(count) | factor(paste(category,event)),layout=c(1,2)))
with(subset(completecases,event=="advising"), densityplot(~log10(count) | factor(paste(category,event)),layout=c(1,2)))


##OK here is the explanation of why the mean(count) table looks so different from the graph:
test <- c(1,7,43859,542)
mean(test) #this corresponds to what happens to the mean(count) table
#log-transforming means that every datapoint is first log-transformed, and then everything is averaged, resulting in MUCH lower means:
logtest <- log(c(1,7,43859,542))
mean(logtest) #this corresponds to the graph
#this would be the log transformation of the means:
log(mean(test)) 

with(completecases, tapply(count==0, list(eventCat, category), sum))
with(completecases, tapply(count==0, list(eventCat, category), mean))
with(completecases, tapply(count==0, list(eventCat, category), function(x) sd(x)/sqrt(length(x)-1)))
completecases <- droplevels(subset(completecases, count!=0))



#means by condition and pair
pairmeans <- summaryBy(count ~  eventCat + event +category , FUN=c(mean,sd), data= completecases)
pairmeans
write.table(pairmeans,"pairmeans _N=80_HowMany", col.names=NA)

completecases$log.how.many <- log(completecases$count)
max(completecases$log.how.many)
#means by condition
conditiononlymeans <- summaryBy(log.how.many ~ eventCat + category , FUN=c(mean,sd), data= completecases)
conditiononlymeans
xtable(conditiononlymeans)

completecases$eventCat <- as.factor(completecases$eventCat)
print(levels(completecases$eventCat)) 
completecases$eventCat = factor(completecases$eventCat,levels(completecases$eventCat)[c(3,1,2)])

#### GRAPHING ####
y.ticks <- c(1,1.5,2,2.5,3, 3.5,4)
Category <- factor(completecases$category)
Category <- relevel(Category,levels(Category)[2])

bar <- ggplot(completecases, aes(x=eventCat,y=log(count+1), fill = Category, ))
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
  #scale_y_continuous(breaks=c(0.0,.5,1,1.5), labels = c("0","~1.6","~2.7","~4.5"))+
  scale_y_continuous(breaks=log(y.ticks), labels = y.ticks)
#  facet_wrap(~eventCat, scales ="free")
  ggsave("EventCountsBar.pdf", width=12, height=8, unit='in')



barTALK <- ggplot(completecases, aes(x=eventCat,y=log(count+1), fill = Category, ))
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
  labs(x="", y="Event Counts, on log scale", fill="Construction")+
  #scale_y_continuous(breaks=c(0.0,.5,1,1.5), labels = c("0","~1.6","~2.7","~4.5"))+
  scale_y_continuous(breaks=log(y.ticks), labels = y.ticks)+
  #  facet_wrap(~eventCat, scales ="free")
  ggsave("EventCountsBarTALK.pdf", width=12, height=8, unit='in')

pairbar <- ggplot(completecases, aes(x=event,y=log(count+1), fill = factor(category)))
pairbar + 
  stat_summary(fun.y= mean, geom = "bar", colour="black", position = "dodge") + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=.9), width= 0.4) + 
  scale_fill_manual(values=c("#9ecae1", "#3182bd", "#08519c"),name="Construction")  + 
  scale_colour_manual(values=c("grey50", "grey80", "black")) + 
  labs(x="Events", y="log(Event Counts)", fill="Construction") +
  names(completecases$event)+
  theme(axis.text.x = element_text(angle = 90,size=14), 
  axis.text.y = element_text(size=14))+
  facet_wrap(~eventCat,scales="free_x")
ggsave("PairEventsCountBar.pdf", width=18, height=9)

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
system.time(m.full <- lmer(log(count) ~ 1 + category.numeric*(eventCat1+eventCat2) + (1|workerId) + (0+ category.numeric|workerId) +(0+eventCat1|workerId) + (0+eventCat2|workerId) + (0 + category.numeric:eventCat1|workerId) + (0+category.numeric:eventCat2|workerId) + (1 | event) + (0+ category.numeric|event), data=completecases,REML=F))
system.time(m.noEventCat <- lmer(log(count) ~ 1 + category.numeric + category.numeric:(eventCat1+eventCat2) + (1|workerId) + (0+ category.numeric|workerId) +(0+eventCat1|workerId) + (0+eventCat2|workerId) + (0 + category.numeric:eventCat1|workerId) + (0+category.numeric:eventCat2|workerId) + (1 | event) + (0+ category.numeric|event), data=completecases,REML=F))
anova(m.noEventCat,m.full)

#extract betas for full model
Vcov <- vcov(m.full, useScale = FALSE)
betas <- fixef(m.full)
se <- sqrt(diag(Vcov))
zval <- betas / se
pval <- 2 * pnorm(abs(zval), lower.tail = FALSE)
cbind(betas, se, zval, pval)

system.time(m.noCategory <- lmer(log(count) ~ 1 + eventCat1+eventCat2 + category.numeric:(eventCat1+eventCat2) + (1|workerId) + (0+ category.numeric|workerId) +(0+eventCat1|workerId) + (0+eventCat2|workerId) + (0 + category.numeric:eventCat1|workerId) + (0+category.numeric:eventCat2|workerId) + (1 | event) + (0+ category.numeric|event), data=completecases,REML=F))
anova(m.noCategory,m.full)
system.time(m.noInteraction <- lmer(log(count) ~ 1 + eventCat1+eventCat2 + category.numeric + (1|workerId) + (0+ category.numeric|workerId) +(0+eventCat1|workerId) + (0+eventCat2|workerId) + (0 + category.numeric:eventCat1|workerId) + (0+category.numeric:eventCat2|workerId) + (1 | event) + (0+ category.numeric|event), data=completecases,REML=F))
anova(m.noInteraction,m.full)

#### Separate Data for estDur #####
PC = subset(completecases, eventCat %in% c('punctive count\n(kiss - to give a kiss)'))
DC = subset(completecases, eventCat %in% c('durative count\n(talk - to give a talk)'))
DM = subset(completecases, eventCat %in% c('durative mass\n(advise - to give advice)'))

PC_m <- lmer(log(count) ~ 1 + category + (category|workerId) + (category|event), data=PC,REML=F)
PC_m0 <- lmer(log(count) ~ 1 + (category|workerId) + (category|event), data=PC,REML=F)
anova(PC_m0,PC_m)


DC_m <- lmer(log(count) ~ 1 + category + (category|workerId) + (category|event), data=DC,REML=F)
DC_m0 <- lmer(log(count) ~ 1 + (category|workerId) + (category|event), data=DC,REML=F)
anova(DC_m0,DC_m)

DM_m <- lmer(log(count) ~ 1 + category + (category|workerId) + (category|event), data=DM,REML=F)
DM_m0 <- lmer(log(count) ~ 1 + (category|workerId) + (category|event), data=DM,REML=F)
anova(DM_m0,DM_m)

