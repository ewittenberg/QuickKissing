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
library(xtable)
rm(list=ls(all=TRUE))  
ALL = read.csv("hashed_ShortAnswerOptions.csv", header = TRUE)
ALL <- na.omit(ALL)
#make value an ordered factor
ALL$value <- ordered(ALL$numvalue)
str(ALL)
#make new column with verbs; replace values in "event" by verbs used
ALL$event <- ALL$sentID
ALL$event <- recode(ALL$event, "c('BV_11','LVC_11') = 'kissing' ; 
                                c('BV_12','LVC_12') = 'kicking' ; 
                                c('BV_13','LVC_13') = 'hugging' ; 
                                c('BV_14','LVC_14') = 'punching' ; 
                                c('BV_15','LVC_15') = 'embracing' ;
                                c('BV_16','LVC_16') = 'cuddling';
                                c('BV_17','LVC_17') = 'poking' ;
                                c('BV_18','LVC_18') = 'shaking';
                                c('BV_21','LVC_21') = 'sermoning';
                                c('BV_22','LVC_22') = 'scolding';
                                c('BV_23','LVC_23') = 'thanking';
                                c('BV_24','LVC_24') = 'lecturing';
                                c('BV_25','LVC_25') = 'talking';
                                c('BV_26','LVC_26') = 'speaking';
                                c('BV_27','LVC_27') = 'presenting';
                                c('BV_28','LVC_28') = 'addressing';
                                c('BV_31','LVC_31') = 'advising';
                                c('BV_32','LVC_32') = 'assisting';
                                c('BV_33','LVC_33') = 'assuring';
                                c('BV_34','LVC_34') = 'encouraging';
                                c('BV_35','LVC_35') = 'condoling';
                                c('BV_36','LVC_36') = 'recognizing';
                                c('BV_37','LVC_37') = 'supporting';
                                c('BV_38','LVC_38') = 'respecting';
                    ") 


#delete rows with fillers and NA
#make new columns with Category and Event
criticals <-subset(ALL, category!="filler" & event!="respecting" & event!="condoling" & event!="punching" & event!="sermoning")
str(criticals)
criticals$event <- droplevels(criticals$event)

criticals$EventCat <- criticals$event
criticals$EventCat <- recode(criticals$EventCat, "c('addressing','lecturing','presenting','speaking','talking','scolding') = 'durative count' ; 
                             c('advising','assisting','assuring','encouraging','recognizing','respecting','supporting','thanking') = 'durative mass' ; 
                             c('cuddling','embracing','hugging','kicking','kissing','poking','shaking') = 'punctive count' ") 
criticals$category <- recode(criticals$category, "'BV' = 'transitive verb' ; 'LVC' = 'ditransitive\nlight verb' ")
criticals$EventCat[criticals$Event=="thanking"] <- "durative mass"

print(levels(criticals$EventCat)) 
criticals$EventCat = factor(criticals$EventCat,levels(criticals$EventCat)[c(3,1,2)])
print(levels(criticals$EventCat)) 

#tables
table <- ftable(xtabs(~ EventCat + category + value, data = criticals))
table

print(
  xtable(format(table)),file="OrdinalData.tex"
)

table2 <- ftable(prop.table(xtabs(~ EventCat + category + value, data = criticals),1))
table2
print(
  xtable(format(table2, digits=1)),file="OrdinalData.tex"
)


str(criticals)
# data in frequency form
library(vcd)
doubledecker(value ~ EventCat + category, data=criticals)
#contrast-coding: intercept is average for all conditions (unlike treatment coding, which is standard)
contrasts(criticals$category)<- contr.sdif(2)


#no Helmert coding; ordered logit model with package ordinal, interaction & random effects
system.time(fm2 <- clmm(value ~ category * EventCat + (category*EventCat|workerID) + (category|event), data=criticals))
fm2
summary(fm2)
save(fm2, file="fm2.RData")
alarm()

ctable2 <- coef(summary(fm2))
xtable(ctable2, digits=3)
ctable2
#helmert coding: vergleicht zwei bedingungen miteinander, und dann die dritte gegen zwei 
#(in diesem fall richtig da keine baseline; dc verhaelt sich in allen anderen studien anders, deshalb gegen pc und dm)
contrasts(criticals$EventCat) <- contr.helmert(3)[c(1,3,2),]
contrasts(criticals$EventCat)

#ordered logit model with package ordinal, interaction & random effects
system.time(fm3 <- clmm(value ~ category * EventCat + (category*EventCat|workerID) + (category|event), data=criticals))
fm3
summary(fm3)
ctable3 <- coef(summary(fm3))
ctable3
save(fm3, file="fm3.RData")


system.time(fm4 <- clmm(value ~ category * EventCat + (1|workerID) + (1|event), data=criticals))
fm4
summary(fm4)
save(fm4, file="fm4.RData")

anova(fm3,fm2) ## 
anova(fm3,fm4) ## 

ctable4 <- coef(summary(fm4))
xtable(ctable4, digits=3)

#### subset to targets only #####
PC = subset(criticals, EventCat %in% c('punctive count'))
DC = subset(criticals, EventCat %in% c('durative count'))
DM = subset(criticals, EventCat %in% c('durative mass'))

PCM <- clmm(value ~ category + (category|workerID) + (category|event), data=PC)
summary(PCM)

DCM <- clmm(value ~ category + (category|workerID) + (category|event), data=DC)
summary(DCM)

DMM <- clmm(value ~ category + (category|workerID) + (category|event), data=DM)
summary(DMM)

#trying Roger's method on the ordered regression model
criticals$Category <- as.factor(criticals$Category)
criticals$Category.numeric <- sapply(criticals$Category,function(i) contr.sum(2)[i,])
m1 <- clmm(value ~ Category.numeric + Category.numeric:EventCat + (Category*EventCat|WorkerId) + (1|Event), data=criticals)
summary(m1)

anova(fm3,m1)

criticals$EventCat <- as.factor(criticals$EventCat)
eventCat.numeric <- sapply(criticals$EventCat,function(i) contr.sum(3)[i,])
eventCat.numeric
criticals$eventCat1 <- eventCat.numeric[1,]
criticals$eventCat2 <- eventCat.numeric[2,]


m2 <- clmm(value ~ eventCat1+ Category:eventCat1 + eventCat2+ Category:eventCat2 + (Category*EventCat|WorkerId) + (1|Event), data=criticals)
summary(m2)

anova(fm3,m2)



library(ggplot2)
library(ordinal)



#Data visualization

criticals$value <- recode(criticals$value, "'1'='short'"); 
criticals$value <- recode(criticals$value, "'2'='medium'"); 
criticals$value <- recode(criticals$value, "'3'='long'")

print(levels(criticals$value)) 
criticals$value = factor(criticals$value,levels(criticals$value)[c(3,2,1)])
print(levels(criticals$category)) 


bar <- ggplot(criticals, aes(value, fill = category, ))
bar   +  geom_bar(position = "dodge",colour="black") +
  scale_fill_manual(values=c("#08519c",  "#2ca25f"))  + 
  theme_bw()+ 
  theme(axis.text.y = element_text(size=16), 
        axis.text.x = element_text(size=16, vjust=1),
        strip.text.x = element_text(size=16),
        axis.title.y = element_text(size=16), 
        axis.title.x = element_text(size=16))+
  labs(x="", y="Count of choice of duration", fill="Construction")+ coord_flip()+
  facet_wrap(~EventCat) 
ggsave("AspectCategorizationCategory.pdf", width=9.5, height=8.2)




