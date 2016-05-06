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
rm(list=ls(all=TRUE))  
ALL = read.csv("hashed_2_Categorizations.csv", header = TRUE)

#make value an ordered factor
ALL$value <- ordered(ALL$value)
str(ALL)

#delete rows with fillers and NA
criticals <- ALL[-grep("_filler_filler",ALL$variable),]
criticals <- na.omit(criticals)
str(criticals)

#make new columns with Category and Event
criticals$variable <- as.character(criticals$variable)
criticals$Answer <- sapply(strsplit(criticals$variable, split=("_"), fixed=TRUE), function(x) (x[1]))
criticals$Category <- sapply(strsplit(criticals$variable, split=("_"), fixed=TRUE), function(x) (x[2]))
criticals$Event <- sapply(strsplit(criticals$variable, split=("_"), fixed=TRUE), function(x) (x[3]))
criticals$Event <- as.factor(criticals$Event)
criticals$Category <- as.factor(criticals$Category)
criticals <-droplevels(subset(criticals, Category!="filler" & Event!="respecting" & Event!="condoling" & Event!="punching" & Event!="sermoning"  & Event!="approving" & Event!="helping"))
str(criticals)

criticals$EventCat <- criticals$Event
criticals$EventCat <- recode(criticals$EventCat, "c('addressing','lecturing','presenting','speaking','talking','scolding') = 'durative count' ; c('advising','assisting','assuring','encouraging','recognizing','respecting','supporting','thanking') = 'durative mass' ; c('cuddling','embracing','hugging','kicking','kissing','poking','shaking') = 'punctive count' ") 
criticals$Category <- recode(criticals$Category, "'BV' = 'transitive verb' ; 'LVC' = 'ditransitive\nlight verb' ")
criticals$EventCat[criticals$Event=="thanking"] <- "durative mass"

print(levels(criticals$EventCat)) 
criticals$EventCat = factor(criticals$EventCat,levels(criticals$EventCat)[c(3,1,2)])
print(levels(criticals$EventCat)) 

#### subset to targets only #####
PC = subset(criticals, EventCat %in% c('punctive count'))
DC = subset(criticals, EventCat %in% c('durative count'))
DM = subset(criticals, EventCat %in% c('durative mass'))

tablePC <- ftable(prop.table(xtabs(~ value + Category, data = PC)))
tablePC

tableDC <- ftable(prop.table(xtabs(~ value + Category, data = DC)))
tableDC

tableDM <- ftable(prop.table(xtabs(~ value + Category, data = DM)))
tableDM
####REGRESSION MODELS####
#coding
criticals$Category <- as.factor(criticals$Category)
criticals$Category.numeric <- sapply(criticals$Category,function(i) contr.sum(2)[i,])
xtabs(~Category+Category.numeric,criticals)

criticals$EventCat <- as.factor(criticals$EventCat)
eventCat.numeric <- sapply(criticals$EventCat,function(i) contr.helmert(3)[i,])
#eventCat.numeric
criticals$EventCat1 <- eventCat.numeric[1,]
criticals$EventCat2 <- eventCat.numeric[2,]
load('fm3.RData')
system.time(fm.full <- clmm(value ~ Category * EventCat + (Category*EventCat|WorkerId) + (Category|Event), data=criticals))
system.time(fm.full.numeric <- clmm(value ~ Category.numeric*(EventCat1+EventCat2) + (Category*EventCat|WorkerId) + (Category|Event), data=criticals))
system.time(m.noEventCat <- clmm(value ~ Category.numeric + Category.numeric:(EventCat1+EventCat2) + (Category*EventCat|WorkerId) + (Category|Event), data=criticals))
anova(m.noEventCat,fm.full)
anova(m.noEventCat,fm3)
system.time(m.noCategory <- clmm(value ~ EventCat1 + EventCat2 + Category.numeric:(EventCat1 + EventCat2) + (Category*EventCat|WorkerId) + (Category|Event), data=criticals))
anova(m.noCategory,fm.full)
save.image()
load(".RData")
system.time(m.noInteraction <- clmm(value ~ EventCat1 + EventCat2 + Category.numeric + (Category*EventCat|WorkerId) + (Category|Event), data=criticals))
anova(m.noInteraction,fm.full)

summary(fm3)
#### pairwise comparisons #####

PCM <- clmm(value ~ Category + (Category|WorkerId) + (Category|Event), data=PC)
PC0 <- clmm(value ~ 1 + (Category|WorkerId) + (Category|Event), data=PC)
anova(PCM,PC0)

DCM <- clmm(value ~ Category + (Category|WorkerId) + (Category|Event), data=DC)
DC0 <- clmm(value ~ 1 + (Category|WorkerId) + (Category|Event), data=DC)
summary(DCM)
anova(DCM,DC0)

DMM <- clmm(value ~ Category + (Category|WorkerId) + (Category|Event), data=DM)
DM0 <- clmm(value ~ 1 + (Category|WorkerId) + (Category|Event), data=DM)
anova(DMM,DM0)
summary(DMM)
summary(PCM)

library(ggplot2)
library(ordinal)



####Data visualization####

criticals$value <- recode(criticals$value, "'1'='shortest'"); 
criticals$value <- recode(criticals$value, "'2'='short'"); 
criticals$value <- recode(criticals$value, "'3'='long'"); 
criticals$value <- recode(criticals$value, "'4'='longest'") 

print(levels(criticals$value)) 
criticals$value = factor(criticals$value,levels(criticals$value)[c(4,3,1,2)])
print(levels(criticals$value)) 

criticals.counts <- with(criticals,aggregate(list(Count=value),list(ResponseCategory=value,Category=Category,EventCat=EventCat),length))
criticals.sums <- with(criticals.counts,tapply(Count,list(Category=Category,EventCat=EventCat),sum))
criticals.counts$Proportion <- with(criticals.counts,Count/criticals.sums[cbind(Category,EventCat)])
category <- factor(criticals.counts$Category)
category <- relevel(category,levels(category)[2])

criticals.counts$Category[criticals.counts$Category=="transitive verb"] <- "transitive verb \n"


bar <- ggplot(criticals.counts, aes(x=ResponseCategory,y=Proportion, fill = category))

annotation_text <- 
  with(criticals.counts,data.frame(ResponseCategory=ResponseCategory,EventCat=EventCat,Category=Category,TextLocation=Proportion+0.23,lab=paste("(",round(Proportion,2),")",sep="")))

dodge <- position_dodge(width=0.9)
bar + geom_bar(stat="identity",position=dodge) + 
  scale_fill_manual(values=c("#08519c",  "#2ca25f"))  + 
  facet_wrap(~EventCat) +
  theme_bw()+ 
  scale_y_continuous(limits=c(0,1),breaks=c(0,0.5,1),labels=c("0","0.5","1")) +
  theme(axis.text.y = element_text(size=16), 
        axis.text.x = element_text(size=16),
        strip.text.x = element_text(size=20),
        axis.title.y = element_text(size=20),
        legend.title = element_text(size=14),
        legend.text = element_text(size=14),
        axis.title.x = element_text(size=20),
        legend.justification=c(1,1), legend.position=c(1,1))+
  labs(x="", y="Proportion of duration category chosen", fill="Constructions")+ coord_flip()+
  geom_text(data=annotation_text,aes(x=ResponseCategory,y=TextLocation,label=lab),position=dodge) +
  coord_flip()
ggsave("AspectCategorizationCategory.pdf", width=12, height=8, unit="in")

barTALK <- ggplot(criticals.counts, aes(x=ResponseCategory,y=Proportion, fill = category))

annotation_text <- 
  with(criticals.counts,data.frame(ResponseCategory=ResponseCategory,EventCat=EventCat,Category=Category,TextLocation=Proportion+0.23,lab=paste("(",round(Proportion,2),")",sep="")))

dodge <- position_dodge(width=0.9)
barTALK + geom_bar(stat="identity",position=dodge) + 
  scale_fill_manual(values=c("#08519c",  "#2ca25f"))  + 
  facet_wrap(~EventCat) +
  theme_bw()+ 
  scale_y_continuous(limits=c(0,1),breaks=c(0,0.5,1),labels=c("0","0.5","1")) +
  theme(axis.text.y = element_text(size=26), 
        axis.text.x = element_text(size=26),
        strip.text.x = element_text(size=30),
        axis.title.y = element_text(size=30),
        legend.title = element_text(size=24),
        legend.text = element_text(size=24),
        axis.title.x = element_text(size=30),
        legend.position="bottom")+
  labs(x="", y="Proportion of duration category chosen", fill="Construction")+ coord_flip()+
  geom_text(data=annotation_text,aes(x=ResponseCategory,y=TextLocation,label=lab),position=dodge,size=8) +
  coord_flip()
ggsave("AspectCategorizationCategoryTALK.pdf", width=12, height=8, unit="in")


# bar   +  geom_bar(position = "dodge",colour="black") +
#   #scale_fill_brewer("Blues")+
#   scale_fill_manual(values=c("#08519c",  "#2ca25f"))  + 
#   #scale_colour_manual(values=c("black","black")) + 
#   theme_bw()+ 
#   scale_x_discrete(breaks=c("longest", "long", "short", "shortest"))+
#   theme(axis.text.y = element_text(size=16), 
#         axis.text.x = element_text(size=16, vjust=1),
#         strip.text.x = element_text(size=16),
#         axis.title.y = element_text(size=16), 
#         axis.title.x = element_text(size=16))+
#   labs(x="", y="Count of choice of duration", fill="Durations")+ coord_flip()+
#   facet_wrap(~EventCat) 
# ggsave("AspectCategorizationCategory.pdf", width=9.5, height=8.2)

save.image("NewCat_image.RData")
