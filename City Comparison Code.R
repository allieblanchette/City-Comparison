library(tidyverse)
library(codyn)
library(dplyr)
library(reshape)
library(ggplot2)
library(vegan)
library(naniar)
library(cowplot)

library(scales)


#lab computer 245
#setwd("C:\\Users\\ablanch4\\Dropbox\\Time V Money\\SLC Comparison\\")

#Allie's laptop
setwd("C:\\Users\\Allie\\Dropbox\\Time V Money\\SLC Comparison\\")

#remove gray background in plots
theme_set(theme_bw(12))

#Baltimore Data read in
B_nbinfo<-read.csv("BAL_NB_Codes_200407.csv") %>% 
  mutate(Nb=as.character(Nb),
         Med_Inc=med_inc,
         Nb=recode(Nb, "34"="0034")) %>% 
  select(Nb, Style, med_age, nb_inc, Med_Inc, size, yard_area)

BAL_Surveys<-read.csv("BAL_Surveys.csv")

All_Diversity_House_Balt<-read.csv("All_Diversity_House_Balt.csv") 

BAL_Direct<-read.csv("BAL_Direct.csv")

B_F_data1<-read.csv("B_F_data1.csv")

BAL_Trees_raw<-read.csv("BAL_Trees_raw.csv")

BAL_Lawns_raw<-read.csv("BAL_Lawns_raw.csv")



#Salt Lake data read in and merging
S_nbinfo<-read.csv("SLC_NB_details_200407.csv") %>% 
  select(-X, -Type) %>% 
  mutate(Nb=as.character(Nb))

SLC_Surveys<-read.csv("SLC_Surveys.csv")

SLC_Direct<-read.csv("SLC_Direct_house_200407.csv") %>% 
  mutate(City="SLC",
         Flower_Color=as.numeric(as.character(A902)),
         Biodiversity=as.numeric(as.character(A8)))

S_F_data1<-read.csv("S_F_data1.csv")

SLC_Lawns_raw<-read.csv("SLC_Lawns_raw.csv")

SLC_Trees_raw<-read.csv("SLC_Trees_raw.csv")


#Average preferences data read in 
S_Prefs_avg<-read.csv("S_Prefs_avg.csv")

B_Prefs_avg<-read.csv("B_Prefs_avg.csv")

CC_Prefs_avg<-read.csv("CC_Prefs_avg.csv")

CC_nbinfo<-read.csv("CC_nbinfo.csv")

#Preferences by home value data set up (can add in parcel_area here)
S_Prefs_parcel<-read.csv("S_Prefs_parcel.csv")
  
#NEED TO ADD IN PARCEL_AREA
B_Prefs_parcel<-read.csv("B_Prefs_parcel.csv")
  
CC_Prefs_parcel<-read.csv("CC_Prefs_parcel.csv")

#Preferences by nb median income data set up
S_Prefs_inc<-read.csv("S_Prefs_inc.csv")

B_Prefs_inc<-read.csv("B_Prefs_inc.csv")

CC_Prefs_inc<-read.csv("CC_Prefs_inc.csv")

#Richness/Abundance data frames
CC_rich_abund<-read.csv("CC_rich_abund.csv")

CC_ra_MedInc<-read.csv("CC_ra_MedInc.csv")

#Section A: Landscaping preferences
summary(m1<-lm(Importance~City*Med_Inc, data= CC_Prefs_inc, subset=(variable=="Leaf Texture")))
anova(m1)

#A1 by city: How satisfied are you with your current landscaping?
ggplot(subset(CC_Prefs_avg, variable%in%c("Satisfaction")), aes(x=City, y=Importance)) + 
  geom_bar(stat="identity", aes(fill=City))+ 
  scale_fill_manual(values=c("steelblue3","goldenrod2"))+
  geom_errorbar(aes(ymin=Importance-se, ymax=Importance+se), width=0.2)+
  ylim(-2,2)+
  ylab("Satisfaction")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(), 
        axis.title.y = element_text(color="black", size=15),
        axis.text.x = element_text(color="black",size=15), 
        axis.text.y = element_text(color="black",size=15),
        legend.title = element_blank(),
        legend.text = element_blank(),
        legend.position="none")

#A1 by home value (only have home_value for 96 of the Balt houses)
ggplot(subset(CC_Prefs_parcel, variable%in%c("Satisfaction")), aes(x=Home_Value, y=value2, group=City)) + 
  geom_point(position=position_jitter(0.55), size=2, aes(color=City))+
  scale_color_manual(values=c("steelblue3","goldenrod2"))+
  xlab("Home Value")+
  ylab("Satisfaction with Yard")+
  ylim(-2.5, 2.5)+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(color="black",size=15), 
        axis.title.y = element_text(color="black", size=15),
        axis.text.x = element_text(color="black",size=15), 
        axis.text.y = element_text(color="black",size=15),
        legend.text=element_text(color="black", size=13),
        legend.title=element_blank())

#Data isn't normal, not sure if pearson is the correct analysis for likert type data anyway
# cor.test(formula = ~ value2+Home_Value,
#          data = B_Prefs_parcel,
#          subset=variable=="Satisfaction")

#A1 by Median Income (IS normally distributed)
ggplot(subset(CC_Prefs_inc, variable%in%c("Satisfaction")), aes(x=Med_Inc, y=Importance, group=City)) + 
  geom_point(size=3, aes(color=City))+
  scale_color_manual(values=c("steelblue3","goldenrod2"))+
  geom_errorbar(aes(ymin=Importance-se, ymax=Importance+se), width=0.2)+
  xlab("Median Neighborhood Income")+
  ylab("Satisfaction with Yard")+
  ylim(-2.5, 2.5)+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(color="black",size=15), 
        axis.title.y = element_text(color="black", size=15),
        axis.text.x = element_text(color="black",size=15), 
        axis.text.y = element_text(color="black",size=15),
        legend.text=element_text(color="black", size=13),
        legend.title=element_blank())

cor.test(formula = ~ Importance+Med_Inc,
         data = B_Prefs_inc,
         subset=variable=="Satisfaction")

CC_Satis_nbinc<-CC_Prefs_inc %>% 
  filter(variable=="Satisfaction") %>% 
  group_by(City, nb_inc) %>% 
  summarize(mean.Satisfaction=mean(Importance),
            n=length(Importance),
            sd=sd(Importance)) %>% 
  mutate(se=sd/sqrt(n))

ggplot(CC_Satis_nbinc, aes(x=nb_inc, y=mean.Satisfaction, fill=City)) + 
  geom_bar(stat="identity", aes(fill=City), position="dodge")+
  scale_fill_manual(values=c("steelblue3","goldenrod2"))+
  xlab("Income")+
  ylab("Satisfaction with Yard")+
  ylim(-2.5, 2.5)+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(color="black",size=15), 
        axis.title.y = element_text(color="black", size=15),
        axis.text.x = element_text(color="black",size=15), 
        axis.text.y = element_text(color="black",size=15),
        legend.text=element_text(color="black", size=13),
        legend.title=element_blank())

summary(m1<-lm(Importance~nb_inc, data= S_Prefs_inc, subset=(variable=="Satisfaction")))
anova(m1)

#A2 by city: If you are disatisfied with your yard, why?
ggplot(subset(B_Prefs_avg, variable%in%c("Unattractive","Plant types", "Effort", "Time", "Money", "Too small", "Too large")), aes(x= reorder(variable, Importance), y=Importance)) + 
  coord_flip()+
  geom_bar(stat="identity",fill="steelblue3" )+ 
  geom_errorbar(aes(ymin=Importance-se, ymax=Importance+se), width=0.2)+
  ylim(0,1)+
  xlab("Reason for dissatisfaction")+
  ylab("Proportion responses")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(), 
        axis.title.y = element_text(color="black", size=15),
        axis.text.x = element_text(color="black",size=15), 
        axis.text.y = element_text(color="black",size=15))+
  geom_hline(yintercept = 0.25, linetype="dotted")+
  ggtitle("Baltimore")

ggplot(subset(S_Prefs_avg, variable%in%c("Unattractive","Plant types", "Effort", "Time", "Money", "Too small", "Too large")), aes(x= reorder(variable, Importance), y=Importance)) + 
  coord_flip()+
  geom_bar(stat="identity",fill="goldenrod2" )+ 
  geom_errorbar(aes(ymin=Importance-se, ymax=Importance+se), width=0.2)+
  ylim(0,1)+
  xlab("Reason for dissatisfaction")+
  ylab("Proportion responses")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(), 
        axis.title.y = element_text(color="black", size=15),
        axis.text.x = element_text(color="black",size=15), 
        axis.text.y = element_text(color="black",size=15))+
  geom_hline(yintercept = 0.25, linetype="dotted")+
  ggtitle("Salt Lake City")

#A2 by nb income
cor.test(formula = ~ Importance+Med_Inc,
         data = B_Prefs_inc,
         subset=variable=="Plant types")

#reasons correlated with income: Time (SLC), Too large (SLC marginal)
CC_Time_inc<-CC_Prefs_inc %>% 
  filter(variable=="Time")

ggplot(subset(CC_Prefs_inc, variable%in%c("Time")), aes(x=Med_Inc, y=Importance, group=City)) + 
  geom_point(size=3, aes(color=City))+
  scale_color_manual(values=c("steelblue3","goldenrod2"))+
  geom_smooth(data=subset(CC_Time_inc, City=="Salt Lake City"), method="lm", se=F, color="goldenrod2")+
  geom_errorbar(aes(ymin=Importance-se, ymax=Importance+se), width=0.2)+
  xlab("Median Neighborhood Income")+
  ylab("My yard takes too much time Y/N")+
  scale_y_continuous(limits=c(0,1.3), breaks=c(0,1))+ 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(color="black",size=15), 
        axis.title.y = element_text(color="black", size=15),
        axis.text.x = element_text(color="black",size=15), 
        axis.text.y = element_text(color="black",size=15),
        legend.text=element_text(color="black", size=13),
        legend.title=element_blank())+
  annotate("text", x=100000, y=0.5, label="BAL: r = -0.123, p = 0.702\nSLC: r = 0.728, p = 0.026", size=4)

CC_Planttypes_inc<-CC_Prefs_inc %>% 
  filter(variable=="Plant types")

ggplot(subset(CC_Prefs_inc, variable%in%c("Plant types")), aes(x=Med_Inc, y=Importance, group=City)) + 
  geom_point(size=3, aes(color=City))+
  scale_color_manual(values=c("steelblue3","goldenrod2"))+
  geom_smooth(data=subset(CC_Planttypes_inc, City=="Baltimore"), method="lm", se=F, color="steelblue3")+
  geom_errorbar(aes(ymin=Importance-se, ymax=Importance+se), width=0.2)+
  xlab("Median Neighborhood Income")+
  ylab("My yard does not have the types of\nplants or trees I prefer")+
  scale_y_continuous(limits=c(0,1.3), breaks=c(0,1))+ 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(color="black",size=15), 
        axis.title.y = element_text(color="black", size=15),
        axis.text.x = element_text(color="black",size=15), 
        axis.text.y = element_text(color="black",size=15),
        legend.text=element_text(color="black", size=13),
        legend.title=element_blank())+
  annotate("text", x=100000, y=0.5, label="BAL: r = 0.519, p = 0.084\nSLC: r = -0.287, p = 0.454", size=4)

#A3 by city: Do you prefer neighborhoods with houses that all look...
ggplot(subset(S_Prefs_avg, variable%in%c("Similar","Different", "Unsure/Neutral")), aes(x= reorder(variable, Importance), y=Importance)) + 
  coord_flip()+
  geom_bar(stat="identity",fill="goldenrod2" )+ 
  geom_errorbar(aes(ymin=Importance-se, ymax=Importance+se), width=0.2)+
  ylim(0,1)+
  ylab("Proportion responses")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        axis.text.x = element_text(color="black",size=15), 
        axis.text.y = element_text(color="black",size=15))+
  ggtitle("Salt Lake City")

ggplot(subset(B_Prefs_avg, variable%in%c("Similar","Different", "Unsure/Neutral")), aes(x= reorder(variable, Importance), y=Importance)) + 
  coord_flip()+
  geom_bar(stat="identity",fill="steelblue3" )+ 
  geom_errorbar(aes(ymin=Importance-se, ymax=Importance+se), width=0.2)+
  ylim(0,1)+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        axis.text.x = element_text(color="black",size=15), 
        axis.text.y = element_text(color="black",size=15))+
  ggtitle("Baltimore")

#A3 by income
cor.test(formula = ~ Importance+Med_Inc,
         data = S_Prefs_inc,
         subset=variable=="Unsure/Neutral")

#A4-6/A6-8 by city: How important is it to you to have plants that: create variety, have high biodiversity, are native?
ggplot(subset(B_Prefs_avg, variable%in%c("Natives","Variety","Biodiversity")), aes(x=reorder(variable, Importance), y=Importance)) + geom_bar(stat="identity", fill="steelblue3")+ 
  coord_flip() + 
  geom_errorbar(aes(ymin=Importance-se, ymax=Importance+se), width=0.2)+
  ylim(0,3)+
  xlab("")+
  ylab("Importance")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(color="black",size=15), 
        axis.text.x = element_text(color="black",size=20), 
        axis.text.y = element_text(color="black",size=13, angle=18))+
  geom_hline(yintercept = 2, linetype="dotted")+
  ggtitle("Baltimore")

ggplot(subset(S_Prefs_avg, variable%in%c("Natives","Variety","Biodiversity")), aes(x=reorder(variable, Importance), y=Importance)) + geom_bar(stat="identity", fill="goldenrod2")+ 
  coord_flip() + 
  geom_errorbar(aes(ymin=Importance-se, ymax=Importance+se), width=0.2)+
  ylim(0,3)+
  xlab("")+
  ylab("Importance")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(color="black",size=15), 
        axis.text.x = element_text(color="black",size=20), 
        axis.text.y = element_text(color="black",size=13, angle=18))+
  geom_hline(yintercept = 2, linetype="dotted")+
  ggtitle("Salt Lake City")

ggplot(subset(S_Prefs_avg, variable%in%c("Leaf Color","Flower Color","Flower Type","Plant Shape","Plant Height","Seasonal Color","Leaf Texture","Plant Type","Ornamental Species","Tree Species")), aes(x=reorder(variable, Importance), y=Importance)) + 
  geom_bar(stat="identity", fill="goldenrod2")+ 
  coord_flip() + 
  geom_errorbar(aes(ymin=Importance-se, ymax=Importance+se), width=0.2)+
  ylim(0,3)+
  xlab("")+
  ylab("Importance of Trait Variety")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(color="black",size=15), 
        axis.text.x = element_text(color="black",size=20), 
        axis.text.y = element_text(color="black",size=13, angle=+18))+
  ggtitle("Salt Lake City")

ggplot(subset(B_Prefs_avg, variable%in%c("Leaf Color","Flower Color","Flower Type","Plant Shape","Plant Height","Seasonal Color","Leaf Texture","Plant Type","Ornamental Species","Tree Species")), aes(x=reorder(variable, Importance), y=Importance)) + 
  geom_bar(stat="identity", fill="steelblue3")+ 
  coord_flip() + 
  geom_errorbar(aes(ymin=Importance-se, ymax=Importance+se), width=0.2)+
  ylim(0,3)+
  xlab("")+
  ylab("Importance of Trait Variety")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(color="black",size=15), 
        axis.text.x = element_text(color="black",size=20), 
        axis.text.y = element_text(color="black",size=13, angle=+18))+
  ggtitle("Baltimore")


#A4-6/A6-8 by income
cor.test(formula = ~ Importance+Med_Inc,
         data = S_Prefs_inc,
         subset=variable=="Natives")

CC_Variety_inc<-CC_Prefs_inc %>% 
  filter(variable=="Variety")

ggplot(subset(CC_Prefs_inc, variable%in%c("Variety")), aes(x=Med_Inc, y=Importance, group=City)) + 
  geom_point(size=3, aes(color=City))+
  scale_color_manual(values=c("steelblue3","goldenrod2"))+
  geom_smooth(data=subset(CC_Variety_inc, City=="Baltimore"), method="lm", se=F)+
  geom_errorbar(aes(ymin=Importance-se, ymax=Importance+se), width=0.2)+
  xlab("Median Neighborhood Income")+
  ylab("Importance of Variety")+
  ylim(0,3)+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(color="black",size=15), 
        axis.title.y = element_text(color="black", size=15),
        axis.text.x = element_text(color="black",size=15), 
        axis.text.y = element_text(color="black",size=15),
        legend.text=element_text(color="black", size=13),
        legend.title=element_blank())+
  annotate("text", x=100000, y=0.5, label="BAL: r = 0.725, p = 0.008**\nSLC: r = -0.404, p = 0.281", size=4)

ggplot(subset(CC_Prefs_inc, variable%in%c("Biodiversity")), aes(x=Med_Inc, y=Importance, group=City)) + 
  geom_point(size=3, aes(color=City))+
  scale_color_manual(values=c("steelblue3","goldenrod2"))+
  geom_errorbar(aes(ymin=Importance-se, ymax=Importance+se), width=0.2)+
  xlab("Median Neighborhood Income")+
  ylab("Importance of Biodiversity")+
  ylim(0,3)+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(color="black",size=15), 
        axis.title.y = element_text(color="black", size=15),
        axis.text.x = element_text(color="black",size=15), 
        axis.text.y = element_text(color="black",size=15),
        legend.text=element_text(color="black", size=13),
        legend.title=element_blank())

#A7/A9 by income
cor.test(formula = ~ Importance+Med_Inc,
         data = B_Prefs_inc,
         subset=variable=="Plant Shape")

CC_TreeSp_inc<-CC_Prefs_inc %>% 
  filter(variable=="Tree Species")

ggplot(subset(CC_Prefs_inc, variable%in%c("Tree Species")), aes(x=Med_Inc, y=Importance, group=City)) + 
  geom_point(size=3, aes(color=City))+
  scale_color_manual(values=c("steelblue3","goldenrod2"))+
  geom_smooth(data=subset(CC_TreeSp_inc, City=="Baltimore"), method="lm", se=F)+
  geom_errorbar(aes(ymin=Importance-se, ymax=Importance+se), width=0.2)+
  xlab("Median Neighborhood Income")+
  ylab("Importance of Tree Species Variety")+
  ylim(0,3)+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(color="black",size=15), 
        axis.title.y = element_text(color="black", size=15),
        axis.text.x = element_text(color="black",size=15), 
        axis.text.y = element_text(color="black",size=15),
        legend.text=element_text(color="black", size=13),
        legend.title=element_blank())+
  annotate("text", x=100000, y=0.5, label="BAL: r = 0.622, p = 0.031\nSLC: r = 0.571, p = 0.109", size=4)

ggplot(subset(CC_Prefs_inc, variable%in%c("Plant Shape")), aes(x=Med_Inc, y=Importance, group=City)) + 
  geom_point(size=3, aes(color=City))+
  scale_color_manual(values=c("steelblue3","goldenrod2"))+
  geom_errorbar(aes(ymin=Importance-se, ymax=Importance+se), width=0.2)+
  xlab("Median Neighborhood Income")+
  ylab("Importance of Plant Shape Variety")+
  ylim(0,3)+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(color="black",size=15), 
        axis.title.y = element_text(color="black", size=15),
        axis.text.x = element_text(color="black",size=15), 
        axis.text.y = element_text(color="black",size=15),
        legend.text=element_text(color="black", size=13),
        legend.title=element_blank())+
  annotate("text", x=100000, y=0.5, label="BAL: r = 0.158, p = 0.625\nSLC: r = -0.096, p = 0.807", size=4)

#Section B: Tree Preferences (average)
summary(m1<-lm(Importance~City*Med_Inc, data= CC_Prefs_inc, subset=(variable=="Reduce night vis")))
anova(m1)

#B1 by city: Tree attributes
ggplot(subset(S_Prefs_avg, variable%in%c("Shade","Fruit","Flowers","Play/relax","Wind break","Aroma","Beauty","Habitat")), aes(x=reorder(variable, Importance), y=Importance)) + 
  geom_bar(stat="identity", fill="goldenrod2")+ 
  coord_flip() + 
  geom_errorbar(aes(ymin=Importance-se, ymax=Importance+se), width=0.2)+
  ylim(0,3)+
  xlab("")+
  ylab("Importance of Tree Attribute")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(color="black",size=15), 
        axis.text.x = element_text(color="black",size=20), 
        axis.text.y = element_text(color="black",size=13, angle=+18))+
  geom_hline(yintercept = 2, linetype="dotted")+
  ggtitle("Salt Lake City")

ggplot(subset(B_Prefs_avg, variable%in%c("Shade","Fruit","Flowers","Play/relax","Wind break","Aroma","Beauty","Habitat")), aes(x=reorder(variable, Importance), y=Importance)) + 
  geom_bar(stat="identity", fill="steelblue3")+ 
  coord_flip() + 
  geom_errorbar(aes(ymin=Importance-se, ymax=Importance+se), width=0.2)+
  ylim(0,3)+
  xlab("")+
  ylab("Importance of Tree Attribute")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(color="black",size=15), 
        axis.text.x = element_text(color="black",size=20), 
        axis.text.y = element_text(color="black",size=13, angle=+18))+
  geom_hline(yintercept = 2, linetype="dotted")+
  ggtitle("Baltimore")

#B1 by income
cor.test(formula = ~ Importance+Med_Inc,
         data = B_Prefs_inc,
         subset=variable=="Fruit")

CC_Wb_inc<-CC_Prefs_inc %>% 
  filter(variable=="Wind break")

#for subsetting geom_smooth
#data=subset(CC_Wb_inc, City=="Salt Lake City"), color="goldenrod2",

ggplot(subset(CC_Prefs_inc, variable%in%c("Fruit")), aes(x=Med_Inc, y=Importance, group=City)) + 
  geom_point(size=3, aes(color=City))+
  scale_color_manual(values=c("steelblue3","goldenrod2"))+
  geom_smooth(method="lm", se=F, aes(color=City))+
  geom_errorbar(aes(ymin=Importance-se, ymax=Importance+se, color=City), width=0.2)+
  xlab("Median Neighborhood Income")+
  ylab("Provides Fruit")+
  ylim(0,3)+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(color="black",size=15), 
        axis.title.y = element_text(color="black", size=15),
        axis.text.x = element_text(color="black",size=15), 
        axis.text.y = element_text(color="black",size=15),
        legend.text=element_text(color="black", size=13),
        legend.title=element_blank())+
  annotate("text", x=100000, y=2.3, label="BAL: r = -0.677, p = 0.016\nSLC: r = -0.689, p = 0.04", size=4)

#Playing with plotting all 4 significant variables on one graph
# CC_Att_test<-CC_Prefs_inc %>% 
#   filter(variable %in%c("Shade","Play/relax")) %>% 
#   mutate(City_var=paste(City, variable, sep="_"))
# 
# ggplot(CC_Att_test, aes(x=Med_Inc, y=Importance, group=City_var)) + 
#   geom_point(size=3, aes(color=City, shape=variable))+
#   scale_color_manual(values=c("steelblue3","goldenrod2"))+
#   geom_smooth(method="lm", se=F, aes(color=City))+
#   xlab("Median Neighborhood Income")+
#   ylab("Importance of Tree Attributes")+
#   ylim(0,3)+
#   theme(panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(),
#         axis.title.x = element_text(color="black",size=15), 
#         axis.title.y = element_text(color="black", size=15),
#         axis.text.x = element_text(color="black",size=15), 
#         axis.text.y = element_text(color="black",size=15),
#         legend.text=element_text(color="black", size=13),
#         legend.title=element_blank())



#B2 by city: Tree costs/maintenance
ggplot(subset(B_Prefs_avg, variable%in%c("Debris", "Reduce night vis","Water use","Sidewalk damage","Blocks views","Pollen","Cost")), aes(x=reorder(variable, Importance), y=Importance)) + 
  geom_bar(stat="identity", fill="steelblue3")+ 
  coord_flip() + 
  geom_errorbar(aes(ymin=Importance-se, ymax=Importance+se), width=0.2)+
  ylim(0,3)+
  xlab("")+
  ylab("Importance of Tree Costs & Maintenance")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(color="black",size=15), 
        axis.text.x = element_text(color="black",size=20), 
        axis.text.y = element_text(color="black",size=13, angle=+18))+
  geom_hline(yintercept = 2, linetype="dotted")+
  ggtitle("Baltimore")

ggplot(subset(S_Prefs_avg, variable%in%c("Debris", "Reduce night vis","Water use","Sidewalk damage","Blocks views","Pollen","Cost")), aes(x=reorder(variable, Importance), y=Importance)) + 
  geom_bar(stat="identity", fill="goldenrod2")+ 
  coord_flip() + 
  geom_errorbar(aes(ymin=Importance-se, ymax=Importance+se), width=0.2)+
  ylim(0,3)+
  xlab("")+
  ylab("Importance of Tree Costs & Maintenance")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(color="black",size=15), 
        axis.text.x = element_text(color="black",size=20), 
        axis.text.y = element_text(color="black",size=13, angle=+18))+
  geom_hline(yintercept = 2, linetype="dotted")+
  ggtitle("Salt Lake City")

#B2 by income
cor.test(formula = ~ Importance+Med_Inc,
         data = S_Prefs_inc,
         subset=variable=="Reduce night vis")

CC_Night_inc<-CC_Prefs_inc %>% 
  filter(variable=="Reduce night vis")

ggplot(subset(CC_Prefs_inc, variable%in%c("Reduce night vis")), aes(x=Med_Inc, y=Importance, group=City)) + 
  geom_point(size=3, aes(color=City))+
  scale_color_manual(values=c("steelblue3","goldenrod2"))+
  geom_smooth(data=subset(CC_Night_inc, City=="Baltimore"), method="lm", se=F)+
  geom_errorbar(aes(ymin=Importance-se, ymax=Importance+se, color=City), width=0.2)+
  xlab("Median Neighborhood Income")+
  ylab("Concern about trees reducing\nnightime visibility")+
  ylim(0,3)+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(color="black",size=15), 
        axis.title.y = element_text(color="black", size=15),
        axis.text.x = element_text(color="black",size=15), 
        axis.text.y = element_text(color="black",size=15),
        legend.text=element_text(color="black", size=13),
        legend.title=element_blank())+
  annotate("text", x=100000, y=0.5, label="BAL: r = -0.793, p = 0.002\nSLC: r = -0.267, p = 0.487", size=4)

#B3: Have you ever removed a tree? Why?
CC_B3_nec<-read.csv("CC_B3_nec.csv")

summary(m1<-lm(per.Unn~City*Med_Inc, data= CC_B3_nec))
anova(m1)

cor.test(formula = ~ per.Unn+Med_Inc,
         data = CC_B3_nec,
         subset = City=="Salt Lake City",
         method = "pearson")

ggplot(CC_B3_nec, aes(x=Med_Inc, y=per.Unn, group=City)) + 
  geom_point(size=3, aes(color=City))+
  scale_color_manual(values=c("steelblue3","goldenrod2"))+
  geom_smooth(method="lm", se=F, aes(color=City))+
  xlab("Median Neighborhood Income")+
  ylab("Percent nb making 1+ unnecessary tree removals")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(color="black",size=15), 
        axis.title.y = element_text(color="black", size=15),
        axis.text.x = element_text(color="black",size=15), 
        axis.text.y = element_text(color="black",size=15),
        legend.text=element_text(color="black", size=13),
        legend.title=element_blank())

#B4: What's your favorite tree in your yard?
CC_B4<-SLC_Surveys %>%
  select(City, Nb, House_ID, PARCEL_ID, B401, B402) %>% 
  mutate(Nb=as.character(Nb)) %>% 
  full_join(BAL_Surveys) %>% 
  select(City, Nb, House_ID, PARCEL_ID, B401, B402)

#write.csv(CC_B4, file="CC_B4.csv", row.names = F)

#Fertilizer application: lawn co. or personal (C5-601 / D1-2)
#NOTE- this dataframe is slightly different from the SLC paper's. For some reason a couple houses got dropped from most nb's, slightlt altering the p-values but not the overall correlation
CC_LawnCo_F<-BAL_Surveys %>% 
  select(City, C5) %>% 
  mutate(D1=C5) %>% 
  select(-C5) %>% 
  full_join(SLC_Surveys) %>% 
  select(City, D1) %>% 
  mutate(Lawn_Co= as.character(D1)) %>% 
  group_by(City, Lawn_Co) %>% 
  summarize(n=length(Lawn_Co))

CC_LawnCo_F_inc<-BAL_Surveys %>% 
  select(City, Nb, C5) %>% 
  mutate(D1=C5) %>% 
  select(-C5) %>% 
  full_join(SLC_Surveys) %>% 
  left_join(CC_nbinfo) %>% 
  select(City, Nb, Med_Inc, D1) %>% 
  mutate(Lawn_Co= as.numeric(as.character(D1))) %>% 
  group_by(City, Med_Inc) %>% 
  summarize(avg.Lawn_Co=mean(Lawn_Co, na.rm=T),
            sd=sd(Lawn_Co, na.rm=T),
            n=length(Lawn_Co[!is.na(Lawn_Co)]))%>%
  mutate(se=sd/sqrt(n))

cor.test(formula = ~ avg.Lawn_Co+Med_Inc,
         data = CC_LawnCo_F_inc,
         subset = City=="Baltimore")

summary(m1<-lm(avg.Lawn_Co~City*Med_Inc, data= CC_LawnCo_F_inc))
anova(m1)

ggplot(CC_LawnCo_F_inc, aes(x=Med_Inc, y=avg.Lawn_Co, group=City)) + 
  geom_point(size=3, aes(color=City))+
  scale_color_manual(values=c("steelblue3","goldenrod2"))+
  geom_smooth(data=subset(CC_LawnCo_F_inc, City=="Salt Lake City"), method="lm", se=F, color="goldenrod2")+
  ylim(0,1)+
  xlab("Median Neighborhood Income")+
  ylab("Proportion nb hiring professionals\nto fertilize lawn")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(color="black",size=15), 
        axis.title.y = element_text(color="black", size=15),
        axis.text.x = element_text(color="black",size=15), 
        axis.text.y = element_text(color="black",size=15),
        legend.text=element_text(color="black", size=13),
        legend.title=element_blank())+
  annotate("text", x=100000, y=0.85, label="BAL: r = 0.436, p = 0.157\nSLC: r = 0.804, p = 0.009", size=4)

CC_Personal_F<-BAL_Surveys %>% 
  select(City, C601) %>% 
  mutate(D2=C601) %>% 
  select(-C601) %>% 
  full_join(SLC_Surveys) %>% 
  select(City, D2) %>% 
  group_by(City) %>% 
  mutate(Personally=as.character(D2),
         tot_responses=length(City)) %>% 
  group_by(City, tot_responses, Personally) %>% 
  summarize(num_resp=length(Personally)) %>% 
  mutate(per_resp=num_resp/tot_responses)

ggplot(subset(CC_Personal_F, Personally%in%c("1")), aes(x=City, y=per_resp)) + 
  geom_bar(stat="identity", aes(fill=City))+ 
  scale_fill_manual(values=c("steelblue3","goldenrod2"))+
  ylim(0,1)+
  ylab("Percent that fertilize personally")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(), 
        axis.title.y = element_text(color="black", size=15),
        axis.text.x = element_text(color="black",size=15), 
        axis.text.y = element_text(color="black",size=15),
        legend.title = element_blank(),
        legend.text = element_blank(),
        legend.position="none")

CC_Perso_F_inc<-BAL_Surveys %>% 
  select(City, Nb, C601) %>% 
  mutate(D2=C601) %>% 
  select(-C601) %>% 
  full_join(SLC_Surveys) %>% 
  left_join(CC_nbinfo) %>% 
  select(City, Nb, Med_Inc, D2) %>% 
  mutate(Personally= as.numeric(as.character(D2))) %>% 
  group_by(City, Med_Inc) %>% 
  summarize(avg.Personally=mean(Personally, na.rm=T),
            sd=sd(Personally, na.rm=T),
            n=length(Personally[!is.na(Personally)]))%>%
  mutate(se=sd/sqrt(n))

cor.test(formula = ~ avg.Personally+Med_Inc,
         data = CC_Perso_F_inc,
         subset = City=="Baltimore")

summary(m1<-lm(avg.Personally~City*Med_Inc, data= CC_Perso_F_inc))
anova(m1)

ggplot(CC_LawnCo_F_inc, aes(x=Med_Inc, y=avg.Lawn_Co, group=City)) + 
  geom_point(size=3, aes(color=City))+
  scale_color_manual(values=c("steelblue3","goldenrod2"))+
  geom_smooth(data=subset(CC_LawnCo_F_inc, City=="Salt Lake City"), method="lm", se=F, color="goldenrod2")+
  ylim(0,1)+
  xlab("Median Neighborhood Income")+
  ylab("Proportion nb hiring professionals\nto fertilize lawn")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(color="black",size=15), 
        axis.title.y = element_text(color="black", size=15),
        axis.text.x = element_text(color="black",size=15), 
        axis.text.y = element_text(color="black",size=15),
        legend.text=element_text(color="black", size=13),
        legend.title=element_blank())+
  annotate("text", x=100000, y=0.85, label="BAL: r = 0.436, p = 0.157\nSLC: r = 0.804, p = 0.009", size=4)

#CC_Both_F counts the number of people that said yes they fertilize personally plus pay for a lawn company (eeven though they wee uspposed to skip that question...it's a very small number of people aynway)
CC_Both_F<-BAL_Surveys %>% 
  select(City, C5, C601) %>% 
  mutate(D1=C5,
         D2=C601) %>% 
  select(-C601, -C5) %>% 
  full_join(SLC_Surveys) %>% 
  filter(D1==1) %>% 
  select(City, D2) %>% 
  mutate(Personally=as.character(D2)) %>% 
  group_by(City, Personally) %>% 
  summarize(n=length(Personally))


#Direct links between preferences and yard biodiversity
ggplot(BAL_Direct, aes(x=Flower_Color, y =c.rich))+
  geom_point(position=position_jitter(0.15),size=3, color="steelblue3")+
  geom_smooth(method="lm", se=F, color="steelblue3")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(color="black",size=15), 
        axis.title.y = element_text(color="black", size=15),
        axis.text.x = element_text(color="black",size=20), 
        axis.text.y = element_text(color="black",size=20))+
  xlab("Importance of Color Variety")+
  ylab("Number of Flower Colors")+
  ylim(0,21)+
  annotate("text", x=1.2, y=19, label="r = 0.202, p = 0.050", size=6)+
  ggtitle("Baltimore")

cor.test(formula = ~ Flower_Color+c.rich,
         data = BAL_Direct)

ggplot(BAL_Direct, aes(x=Biodiversity, y =f.rich))+
  geom_point(position=position_jitter(0.15), size=3, color="steelblue3")+
  geom_smooth(method="lm", se=F, color="steelblue3")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(color="black",size=15), 
        axis.title.y = element_text(color="black", size=15),
        axis.text.x = element_text(color="black",size=20), 
        axis.text.y = element_text(color="black",size=20))+
  xlab("Importance of Biodiversity")+
  ylab("Flower Genus Richness")+
  ylim(0,35)+
  annotate("text", x=1, y=34, label="r = 0.250, p = 0.015", size=6)+
  ggtitle("Baltimore")

cor.test(formula = ~ Biodiversity+f.rich,
         data = BAL_Direct)

ggplot(SLC_Direct, aes(x=Flower_Color, y =color.num_F))+
  geom_point(position=position_jitter(0.15),size=3, color="goldenrod2")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(color="black",size=15), 
        axis.title.y = element_text(color="black", size=15),
        axis.text.x = element_text(color="black",size=20), 
        axis.text.y = element_text(color="black",size=20))+
  xlab("Importance of Color Variety")+
  ylab("Number of Flower Colors")+
  ylim(0,21)+
  annotate("text", x=1.2, y=19, label="r = 0.240, p = 0.058", size=6)+
  ggtitle("Salt Lake City")

cor.test(formula = ~ Flower_Color+color.num_F,
         data = SLC_Direct)

ggplot(SLC_Direct, aes(x=Biodiversity, y =rich_Flowers))+
  geom_point(position=position_jitter(0.15), size=3, color="goldenrod2")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(color="black",size=15), 
        axis.title.y = element_text(color="black", size=15),
        axis.text.x = element_text(color="black",size=20), 
        axis.text.y = element_text(color="black",size=20))+
  xlab("Importance of Biodiversity")+
  ylab("Flower Genus Richness")+
  ylim(0,35)+
  annotate("text", x=1, y=34, label="r = 0.205, p = 0.108", size=6)+
  ggtitle("Salt Lake City")

cor.test(formula = ~ Biodiversity+rich_Flowers,
         data = SLC_Direct)


#NMDS Lawn
B_Lawns_nmds<-BAL_Lawns_raw %>% 
  left_join(B_nbinfo) %>%
  select(Nb, House_ID, City, Species, Type, nb_inc, F1, F2, B1, B2)

S_Lawns_nmds<-SLC_Lawns_raw %>% 
  left_join(S_nbinfo) %>% 
  select(Nb, House_ID, City, Species, nb_inc, F1, F2, B1, B2)

CC_Lawns_nmds<-S_Lawns_nmds %>% 
  full_join(B_Lawns_nmds) %>% 
  gather(plot, cover, F1:B2) %>% 
  group_by(Nb, City, nb_inc, Species) %>% 
  summarize(mcov=mean(cover, na.rm = T)) %>% 
  filter(Species!="No Lawn",
         Species!="NO LAWN",
         Species!="Dead grass") %>% 
  spread(Species, mcov, fill=0)

plots_L<-CC_Lawns_nmds[,1:3]
mds_L<-metaMDS(CC_Lawns_nmds[,4:141], autotransform=FALSE, shrink=FALSE, trymax = 1000)
plot(mds_L)
mds_L

scores_L <- data.frame(scores(mds_L, display="sites"))  # Extracts NMDS scores for each block
scores2_L<- plots_L %>% 
  bind_cols(scores_L) # binds the NMDS scores to NB info

theme_set(theme_bw(12))
ggplot(scores2_L, aes(x=NMDS1, y=NMDS2, color=City))+
  geom_point(size=5, aes(color=City))+
  scale_color_manual(values=c("steelblue3","goldenrod2"))+
  xlab("NMDS Axis 1")+
  ylab("NMDS Axis 2")+
  ggtitle("Lawn Community")+
  annotate("text", x=0.3, y=-0.8, label="stress = 0.131", size=5)+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.title=element_text(color = "black", size = 20, face = "bold"),
        axis.title.x = element_text(color="black",size=18), 
        axis.title.y = element_text(color="black", size=18),
        axis.text.x = element_text(color="black",size=18), 
        axis.text.y = element_text(color="black",size=18),
        legend.title=element_text(color="black",size=18),
        legend.text=element_text(color="black",size=15))

#NMDS Flowers
S_F_nmds_prep<-S_F_data1 %>% 
  left_join(S_nbinfo) %>%
  group_by(City, Nb, FaGe) %>% 
  summarize(numplants=sum(num_plants)) %>% 
  filter(numplants!=0)


B_F_nmds_prep<-B_F_data1 %>% 
  mutate(City="Baltimore",
         Nb=as.character(Nb),
         FaGe=paste(Family, Genus, sep="_"))%>% 
  left_join(B_nbinfo) %>%
  group_by(City, Nb, FaGe) %>% 
  summarize(numplants=sum(num_plants)) %>% 
  filter(numplants!=0)

CC_Flowers_nmds<-S_F_nmds_prep %>% 
  full_join(B_F_nmds_prep) %>% 
  spread(FaGe, numplants, fill=0)

plots_F<-CC_Flowers_nmds[,1:2]
mds_F<-metaMDS(CC_Flowers_nmds[,3:163], autotransform=FALSE, shrink=FALSE, trymax = 100)
plot(mds_F)
mds_F

scores_F <- data.frame(scores(mds_F, display="sites"))  # Extracts NMDS scores for Nb

scores2_F<- plots_F %>% 
  bind_cols(scores_F)

theme_set(theme_bw(12))
ggplot(scores2_F, aes(x=NMDS1, y=NMDS2, color=City))+
  geom_point(size=5, aes(color=City))+
  scale_color_manual(values=c("steelblue3","goldenrod2"))+
  xlab("NMDS Axis 1")+
  ylab("NMDS Axis 2")+
  ylim(-1.3,1)+
  ggtitle("Flower Community")+
  annotate("text", x=0.3, y=-1.2, label="stress = 0.172", size=5)+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.title=element_text(color = "black", size = 20, face = "bold"),
        axis.title.x = element_text(color="black",size=18), 
        axis.title.y = element_text(color="black", size=18),
        axis.text.x = element_text(color="black",size=18), 
        axis.text.y = element_text(color="black",size=18),
        legend.title=element_text(color="black",size=18),
        legend.text=element_text(color="black",size=15))

#NMDS Trees
#note- By filtering out the "Species!=""" , we're removing the 'no tree' houses
SLC_Trees_1<-SLC_Trees_raw%>% 
  filter(Species!="") %>% 
  group_by(City, Nb, House_ID, Species) %>% 
  summarize(num_trees=length(Species)) %>% 
  left_join(S_nbinfo) %>%
  group_by(City, Nb, Species) %>% 
  summarize(numtrees=sum(num_trees)) %>% 
  filter(numtrees!=0)

BAL_Trees_1<-BAL_Trees_raw%>% 
  filter(Species!="no trees") 
  group_by(City, Nb, House_ID, Species) %>% 
  summarize(num_trees=length(Species)) %>% 
  left_join(B_nbinfo) %>%
  group_by(City, Nb, Species) %>% 
  summarize(numtrees=sum(num_trees)) %>% 
  filter(numtrees!=0)

CC_Trees_nmds<-SLC_Trees_1 %>% 
  full_join(BAL_Trees_1) %>% 
  spread(Species, numtrees, fill=0)

plots_T<-CC_Trees_nmds[,1:2]
mds_T<-metaMDS(CC_Trees_nmds[,3:192], autotransform=FALSE, shrink=FALSE, trymax = 100)
plot(mds_T)
mds_T

scores_T <- data.frame(scores(mds_T, display="sites"))  # Extracts NMDS scores for Nb

scores2_T<- plots_T %>% 
  bind_cols(scores_T)

theme_set(theme_bw(12))
ggplot(scores2_T, aes(x=NMDS1, y=NMDS2, color=City))+
  geom_point(size=5, aes(color=City))+
  scale_color_manual(values=c("steelblue3","goldenrod2"))+
  xlab("NMDS Axis 1")+
  ylab("NMDS Axis 2")+
  ylim(-1.3,1)+
  ggtitle("Tree Community")+
  annotate("text", x=0.3, y=-1.2, label="stress = 0.140", size=5)+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.title=element_text(color = "black", size = 20, face = "bold"),
        axis.title.x = element_text(color="black",size=18), 
        axis.title.y = element_text(color="black", size=18),
        axis.text.x = element_text(color="black",size=18), 
        axis.text.y = element_text(color="black",size=18),
        legend.title=element_text(color="black",size=18),
        legend.text=element_text(color="black",size=15))

#Richness and abundance comparisons vs Income and yard area (per city)


#Richness by home value
ggplot(CC_rich_abund, aes(x=Home_Value, y=Lawn_rich, group=City)) + 
  geom_point(size=3, aes(color=City))+
  scale_color_manual(values=c("steelblue3","goldenrod2"))+
  xlab("Home Value")+
  ylab("Lawn Diversity")+
  geom_smooth(data=subset(CC_rich_abund, City=="Salt Lake City"), method="lm", se=F, color="goldenrod2")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(color="black",size=15), 
        axis.title.y = element_text(color="black", size=15),
        axis.text.x = element_text(color="black",size=15), 
        legend.position="none",
        axis.text.y = element_text(color="black",size=15),
        legend.title=element_blank())+
  annotate("text", x=780000, y=25, label="BAL: r = -0.177, p = 0.105\nSLC: r = -0.390, p < 0.001", size=3.5)

cor.test(formula = ~ Home_Value+Lawn_rich,
         data = CC_rich_abund,
         subset=City=="Salt Lake City")

ggplot(CC_rich_abund, aes(x=Home_Value, y=Flower_rich, group=City)) + 
  geom_point(size=3, aes(color=City))+
  scale_color_manual(values=c("steelblue3","goldenrod2"))+
  xlab("Home Value")+
  ylab("Flower Diversity")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(color="black",size=15), 
        axis.title.y = element_text(color="black", size=15),
        axis.text.x = element_text(color="black",size=15), 
        legend.position="none",
        axis.text.y = element_text(color="black",size=15),
        legend.title=element_blank())+
  geom_smooth(method="lm", se=F, aes(color=City))+
  annotate("text", x=780000, y=27, label="BAL: r = 0.323, p = 0.002\nSLC: r = 0.311, p = 0.006", size=3.5)

cor.test(formula = ~ Home_Value+Flower_rich,
         data = CC_rich_abund,
         subset=City=="Baltimore")

ggplot(CC_rich_abund, aes(x=Home_Value, y=Tree_rich, group=City)) + 
  geom_point(size=3, aes(color=City))+
  scale_color_manual(values=c("steelblue3","goldenrod2"))+
  xlab("Home Value")+
  ylab("Tree Diversity")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(color="black",size=15), 
        axis.title.y = element_text(color="black", size=15),
        axis.text.x = element_text(color="black",size=15), 
        axis.text.y = element_text(color="black",size=15),
        legend.title=element_blank(),
        legend.text=element_text(color="black", size=12))+
  geom_smooth(method="lm", se=F, aes(color=City))+
  annotate("text", x=780000, y=4, label="BAL: r = 0.530, p < 0.001\nSLC: r = 0.357, p = 0.001", size=3.5)

cor.test(formula = ~ Home_Value+Tree_rich,
         data = CC_rich_abund,
         subset=City=="Salt Lake City")

#Abundance by home value
ggplot(CC_rich_abund, aes(x=Home_Value, y=Flower_abund, group=City)) + 
  geom_point(size=3, aes(color=City))+
  scale_color_manual(values=c("steelblue3","goldenrod2"))+
  xlab("Home Value")+
  ylab("Flower Abundance")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(color="black",size=15), 
        axis.title.y = element_text(color="black", size=15),
        axis.text.x = element_text(color="black",size=15), 
        legend.position="none",
        axis.text.y = element_text(color="black",size=15),
        legend.title=element_blank())+
  geom_smooth(method="lm", se=F, aes(color=City))+
  annotate("text", x=780000, y=200, label="BAL: r = 0.355, p < 0.001\nSLC: r = 0.341, p = 0.002", size=3.5)

cor.test(formula = ~ Home_Value+Flower_abund,
         data = CC_rich_abund,
         subset=City=="Baltimore")

ggplot(CC_rich_abund, aes(x=Home_Value, y=Tree_abund, group=City)) + 
  geom_point(size=3, aes(color=City))+
  scale_color_manual(values=c("steelblue3","goldenrod2"))+
  xlab("Home Value")+
  ylab("Tree Abundance")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(color="black",size=15), 
        axis.title.y = element_text(color="black", size=15),
        axis.text.x = element_text(color="black",size=15), 
        axis.text.y = element_text(color="black",size=15),
        legend.title=element_blank(),
        legend.text=element_text(color="black", size=12))+
  geom_smooth(method="lm", se=F, aes(color=City))+
  annotate("text", x=780000, y=, label="BAL: r = 0.557, p < 0.001\nSLC: r = 0.361, p = 0.001", size=3.5)

cor.test(formula = ~ Home_Value+Tree_abund,
         data = CC_rich_abund,
         subset=City=="Baltimore")



#Richness by nb median income figures
ggplot(CC_ra_MedInc, aes(x=Med_Inc, y=Lawn_rich_avg, group=City)) + 
  geom_point(size=3, aes(color=City))+
  geom_errorbar(aes(ymin=Lawn_rich_avg-se_LRA, ymax=Lawn_rich_avg+se_LRA, color=City), width=0)+
  scale_color_manual(values=c("steelblue3","goldenrod2"))+
  xlab("Median Nieghborhood Income")+
  ylab("Lawn Diversity")+
  geom_smooth(data=subset(CC_ra_MedInc, City=="Salt Lake City"), method="lm", se=F, color="goldenrod2")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(color="black",size=15), 
        axis.title.y = element_text(color="black", size=15),
        axis.text.x = element_text(color="black",size=15), 
        legend.position="none",
        axis.text.y = element_text(color="black",size=15),
        legend.title=element_blank())+
  annotate("text", x=116000, y=18, label="BAL: r = -0.423, p = 0.170\nSLC: r = -0.860, p = 0.003", size=3.5)

cor.test(formula = ~ Med_Inc+Lawn_rich_avg,
         data = CC_ra_MedInc,
         subset=City=="Baltimore")

ggplot(CC_ra_MedInc, aes(x=Med_Inc, y=Flower_rich_avg, group=City)) + 
  geom_point(size=3, aes(color=City))+
  geom_errorbar(aes(ymin=Flower_rich_avg-se_FRA, ymax=Flower_rich_avg+se_FRA, color=City), width=0)+
  scale_color_manual(values=c("steelblue3","goldenrod2"))+
  xlab("Median Neighborhood Income")+
  ylab("Flower Diversity")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(color="black",size=15), 
        axis.title.y = element_text(color="black", size=15),
        axis.text.x = element_text(color="black",size=15), 
        legend.position="none",
        axis.text.y = element_text(color="black",size=15),
        legend.title=element_blank())+
  geom_smooth(data=subset(CC_ra_MedInc, City=="Salt Lake City"), method="lm", se=F, color="goldenrod2")+
  annotate("text", x=116000, y=3, label="BAL: r = 0.427, p = 0.166\nSLC: r = 0.676, p = 0.045", size=3.5)

cor.test(formula = ~ Med_Inc+Flower_rich_avg,
         data = CC_ra_MedInc,
         subset=City=="Baltimore")

ggplot(CC_ra_MedInc, aes(x=Med_Inc, y=Tree_rich_avg, group=City)) + 
  geom_point(size=3, aes(color=City))+
  geom_errorbar(aes(ymin=Tree_rich_avg-se_TRA, ymax=Tree_rich_avg+se_TRA, color=City), width=0)+
  scale_color_manual(values=c("steelblue3","goldenrod2"))+
  xlab("Median Neighborhood Income")+
  ylab("Tree Diversity")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(color="black",size=15), 
        axis.title.y = element_text(color="black", size=15),
        axis.text.x = element_text(color="black",size=15), 
        axis.text.y = element_text(color="black",size=15),
        legend.title=element_blank(),
        legend.text=element_text(color="black", size=12))+
  geom_smooth(data=subset(CC_ra_MedInc, City=="Salt Lake City"), method="lm", se=F, color="goldenrod2")+
  annotate("text", x=108000, y=1, label="BAL: r = 0.450, p = 0.142\nSLC: r = 0.742, p = 0.022", size=3.5)

cor.test(formula = ~ Med_Inc+Tree_rich_avg,
         data = CC_ra_MedInc,
         subset=City=="Baltimore")

#Abundance by Median Income figures
ggplot(CC_ra_MedInc, aes(x=Med_Inc, y=Flower_abund_avg, group=City)) + 
  geom_point(size=3, aes(color=City))+
  geom_errorbar(aes(ymin=Flower_abund_avg-se_FAA, ymax=Flower_abund_avg+se_FAA, color=City), width=0)+
  scale_color_manual(values=c("steelblue3","goldenrod2"))+
  xlab("Median Neighborhood Income")+
  ylab("Flower Abundance")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(color="black",size=15), 
        axis.title.y = element_text(color="black", size=15),
        axis.text.x = element_text(color="black",size=15), 
        legend.position="none",
        axis.text.y = element_text(color="black",size=15),
        legend.title=element_blank())+
  geom_smooth(data=subset(CC_ra_MedInc, City=="Salt Lake City"), method="lm", se=F, color="goldenrod2")+
  annotate("text", x=114000, y=8, label="BAL: r = 0.507, p = 0.092\nSLC: r = 0.818, p = 0.007", size=3.5)

cor.test(formula = ~ Med_Inc+Flower_abund_avg,
         data = CC_ra_MedInc,
         subset=City=="Baltimore")

ggplot(CC_ra_MedInc, aes(x=Med_Inc, y=Tree_abund_avg, group=City)) + 
  geom_point(size=3, aes(color=City))+
  geom_errorbar(aes(ymin=Tree_abund_avg-se_TAA, ymax=Tree_abund_avg+se_TAA, color=City), width=0)+
  scale_color_manual(values=c("steelblue3","goldenrod2"))+
  xlab("Median Neighborhood Income")+
  ylab("Tree Abundance")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(color="black",size=15), 
        axis.title.y = element_text(color="black", size=15),
        axis.text.x = element_text(color="black",size=15), 
        axis.text.y = element_text(color="black",size=15),
        legend.title=element_blank(),
        legend.text=element_text(color="black", size=12))+
  geom_smooth(data=subset(CC_ra_MedInc, City=="Salt Lake City"), method="lm", se=F, color="goldenrod2")+
  annotate("text", x=60000, y=24, label="BAL: r = 0.359, p = 0.252\nSLC: r = 0.712, p = 0.031", size=3.5)

cor.test(formula = ~ Med_Inc+Tree_abund_avg,
         data = CC_ra_MedInc,
         subset=City=="Baltimore")

#Flower and grass divergences (Figure 5 in SLC paper) 
Toplot.FL_Balt<-All_Diversity_House_Balt %>% 
  filter(l.rich!="NA") %>% 
  left_join(B_nbinfo) %>% 
  select(Nb, House_ID, Med_Inc,l.rich, f.rich)

PlotFL_melt.B<-melt(Toplot.FL_Balt, id=c("Nb","House_ID","Med_Inc"))%>%
  mutate(variable=recode(variable, l.rich="Lawn"),
         variable=recode(variable, f.rich="Flowers")) %>% 
  group_by(Nb,variable, Med_Inc)%>%
  summarize(Num.Species = mean(as.numeric(value, na.rm=T)),
            sd=sd(as.numeric(value, na.rm=T)),
            n=length(value[!is.na(value)]))%>%
  mutate(se=sd/sqrt(n))

ggplot(data = PlotFL_melt.B, aes(x=Med_Inc, y = Num.Species, group=variable))+
  geom_point(size=3, aes(color=variable))+
  geom_errorbar(aes(ymin=Num.Species-se, ymax=Num.Species+se, color=variable), width=0.2)+
  guides(color=guide_legend(reverse=TRUE))+
  geom_smooth(method="lm", se=F, aes(color=variable))+
  xlab("Median Neighborhood Income (Balt)")+
  ylab("Species/Genus Richness")+
  xlim(40000, 135000)+
  scale_color_manual(values=c("darkgreen","tomato3"))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(color="black",size=20), 
        axis.title.y = element_text(color="black", size=20),
        axis.text.x = element_text(color="black",size=20), 
        axis.text.y = element_text(color="black",size=20),
        legend.position = "none")

#Turf, and weed divergences (Figure 5 in SLC paper)
Toplot.TW_Balt<-BAL_Lawns_raw %>% 
  filter(Species!="No Lawn",
         Type!="") %>% 
  mutate(Type=recode(Type, seedling="Weed"),
         Type=recode(Type, lawn="Turf Grass"),
         Type=recode(Type, weed="Weed"))%>%
  group_by(Nb, House_ID, Type) %>% 
  summarize(Num.Species=length(unique(Species))) %>% 
  group_by(Nb, Type) %>% 
  summarize(Num.Species_avg = mean(Num.Species),
            sd=sd(Num.Species, na.rm=T),
            n=length(Num.Species[!is.na(Num.Species)]))%>%
  mutate(se=sd/sqrt(n)) %>% 
  left_join(B_nbinfo)

ggplot(data=Toplot.TW_Balt, aes(x=Med_Inc, y=Num.Species_avg, group=Type))+
  geom_point(size=3, aes(color=Type))+
  geom_errorbar(aes(ymin=Num.Species_avg-se, ymax=Num.Species_avg+se, color=Type), width=0.2)+
  geom_smooth(method="lm", se=F, aes(color=Type))+
  guides(color=guide_legend(reverse=TRUE))+
  xlab("Median Neighborhood Income (Balt)")+
  ylab("Species Richness")+
  xlim(40000,135000)+
  ylim(0,16)+
  scale_color_manual(values=c("green4","orange3"))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(color="black",size=20), 
        axis.title.y = element_text(color="black", size=20),
        axis.text.x = element_text(color="black",size=20), 
        axis.text.y = element_text(color="black",size=20),
        legend.position = "none")


#RANK ABUNDANCE CURVES
S_ranks<-read.csv("S_ranks.csv")
B_ranks<-read.csv("B_ranks.csv")

S_Frank<-S_ranks%>%
  select(City, type, Species, freq, Frank, Native_UT)%>%
  filter(Frank<11)

S_Arank<-S_ranks%>%
  select(City, type, Species, abund, Arank, Native_UT)%>%
  filter(Arank<11)

B_Frank<-B_ranks%>%
  select(City, type, Species, freq, Frank, Native_MD)%>%
  filter(Frank<11)

B_Arank<-B_ranks%>%
  select(City, type, Species, abund, Arank, Native_MD)%>%
  filter(Arank<11)

#Lawn RACs
B_freq_lawn <- 
  ggplot(data=subset(B_Frank, type=="lawn"), aes(x=Frank, y=freq))+
  geom_point(size=4, color="steelblue3")+
  theme()+
  xlab("Rank")+
  ylab("Number of Yards Present")+
  scale_x_continuous(limits=c(1,11), breaks = c(1:10))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.y = element_text(size=15, color="black"),
        axis.title.y = element_text(size=15, color="black"),
        axis.text.x = element_text(size=15, color="black"),
        axis.title.x = element_text(size=15, color="black"))+
    annotate("text", x =2.6, y = 79, label='Poa pratensis', size=4)+
    annotate("text", x =3.8, y = 72, label='Festuca arund.', size=4)+
    annotate("text", x =4.8, y = 66, label='Trifolium repens', size=4)+
    annotate("text", x =6, y = 60, label='Cynodon dactylon', size=4)+
    annotate("text", x =3.8, y = 55, label='Digitaria sang.', size=4)+
    annotate("text", x =7.5, y = 55, label='Taraxacum off.', size=4)+
    annotate("text", x =5.4, y = 53, label='Oxalis stricta', size=4)+
    annotate("text", x =9.2, y = 52, label='Viola pap.', size=4)+
    annotate("text", x =7.2, y = 47, label='Plantago major', size=4)+
    annotate("text", x =8.8, y = 42, label='F. rubra', size=4)

B_abund_lawn<-
  ggplot(data=subset(B_Arank, type=="lawn"), aes(x=Arank, y=abund))+
    geom_point(size=4, color="steelblue3")+
    theme()+
    xlab("Rank")+
    ylab("Abundance")+
    scale_x_continuous(limits=c(1,11), breaks = c(1:10))+
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.text.y = element_text(size=15, color="black"),
          axis.title.y = element_text(size=15, color="black"),
          axis.text.x = element_text(size=15, color="black"),
          axis.title.x = element_text(size=15, color="black"))+
    annotate("text", x =2.6, y = 2180, label='Poa pratensis', size=4)+
    annotate("text", x =3.8, y = 1286, label='Festuca arund.', size=4)+
    annotate("text", x =4.8, y = 1050, label='Zoysia japonica', size=4)+
    annotate("text", x =6.1, y = 910, label='Cynodon dactylon', size=4)+
    annotate("text", x =3, y = 770, label='Trifolium repens', size=4)+
    annotate("text", x =5, y = 660, label='F. rubra', size=4)+
    annotate("text", x =8.8, y = 620, label='Digitaria sang.', size=4)+
    annotate("text", x =5.5, y = 396, label='Glechoma hederacea', size=4)+
    annotate("text", x =10.4, y = 362, label='Viola pap.', size=4)+
    annotate("text", x =8, y = 247, label='Lolium perenne', size=4)

#note- 5 lawn species tied for rank #11 in frequency (9 yards)
S_freq_lawn <- 
  ggplot(data=subset(S_Frank, type=="lawn"), aes(x=Frank, y=freq))+
    geom_point(size=4, color="goldenrod2")+
    theme()+
    xlab("Rank")+
    ylab("Number of Yards Present")+
    scale_x_continuous(limits=c(1,11), breaks = c(1:10))+
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.text.y = element_text(size=15, color="black"),
          axis.title.y = element_text(size=15, color="black"),
          axis.text.x = element_text(size=15, color="black"),
          axis.title.x = element_text(size=15, color="black"))+
    annotate("text", x =2.6, y = 79, label='Poa pratensis', size=4)+
    annotate("text", x =3.8, y = 45, label='Lolium perenne', size=4)+
    annotate("text", x =4.7, y = 39, label='Taraxacum off.', size=4)+
    annotate("text", x =5.7, y = 32, label='Festuca rubra', size=4)+
    annotate("text", x =6.9, y = 27, label='Convolvulus arv.', size=4)+
    annotate("text", x =7.7, y = 21, label='Festuca arund.', size=4)+
    annotate("text", x =8.8, y = 17, label='Elymus repens', size=4)+
    annotate("text", x =9.7, y = 11, label='Medicago lup.', size=4)

S_abund_lawn<-
  ggplot(data=subset(S_Arank, type=="lawn"), aes(x=Arank, y=abund))+
    geom_point(size=4, color="goldenrod2")+
    theme()+
    xlab("Rank")+
    ylab("Abundance")+
    scale_x_continuous(limits=c(1,11), breaks = c(1:10))+
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.text.y = element_text(size=15, color="black"),
          axis.title.y = element_text(size=15, color="black"),
          axis.text.x = element_text(size=15, color="black"),
          axis.title.x = element_text(size=15, color="black"))+
    annotate("text", x =2.6, y = 5480, label='Poa pratensis', size=4)+
    annotate("text", x =3.2, y = 1600, label='Festuca rubra', size=4, angle=45)+
    annotate("text", x =4.2, y = 1570, label='Lolium perenne', size=4, angle=45)+
    annotate("text", x =5.2, y = 1350, label='Festuca arund.', size=4, angle=45)+
    annotate("text", x =6.2, y = 1300, label='Convolvulus arv.', size=4, angle = 45)+
    annotate("text", x =7.2, y = 1200, label='Elymus repens', size=4, angle=45)+
    annotate("text", x =8.2, y = 1200, label='Taraxacum off.', size=4, angle=45)+
    annotate("text", x =9.2, y = 1200, label='Trifolium repens', size=4, angle=45)+
    annotate("text", x =10.1, y = 1100, label='Cynodon dact.', size=4, angle=45)+
    annotate("text", x =10.5, y = 600, label='UNK', size=4, angle=45)

#lawn RAC
ggdraw()+
  draw_plot(B_freq_lawn, x=0.036, y=.5, width=.45, height=.5)+
  draw_plot(B_abund_lawn, x=.5, y=.5, width=.45, height=.5)+
  draw_plot(S_freq_lawn, x=0.04, y=0, width=.45, height=.5)+
  draw_plot(S_abund_lawn, x=.5, y=0, width=.45, height=.5)+
  draw_plot_label(label= c("A","B","C","D"), size=13,
                  x= c(0.01, 0.51, 0.01, 0.51), y = c(1, 1, 0.5,0.5))

#Tree RACS
B_freq_tree <- 
  ggplot(data=subset(B_Frank, type=="tree"), aes(x=Frank, y=freq))+
  geom_point(size=4, color="steelblue3")+
  theme()+
  xlab("Rank")+
  ylab("Number of Yards Present")+
  scale_x_continuous(limits=c(1,11), breaks = c(1:10))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.y = element_text(size=15, color="black"),
        axis.title.y = element_text(size=15, color="black"),
        axis.text.x = element_text(size=15, color="black"),
        axis.title.x = element_text(size=15, color="black"))+
  annotate("text", x =5.8, y = 24, label='Cornus florida, Lagerstroemia indica.', size=4)+
  annotate("text", x =5, y = 20, label='Acer palmatum', size=4)+
  annotate("text", x =7, y = 17.5, label='Acer saccharinum, Ilex Opaca,\nPrunus sp.', size=4)+
  annotate("text", x =9, y = 13, label='Acer rubrum', size=4)+
  annotate("text", x =9.4, y = 10, label='Morus alba', size=4)+
  annotate("text", x =5.5, y = 8, label='Magnolia grand., Syrina sp., Thuja occ.', size=4)

B_abund_tree<-
  ggplot(data=subset(B_Arank, type=="tree"), aes(x=Arank, y=abund))+
  geom_point(size=4, color="steelblue3")+
  theme()+
  xlab("Rank")+
  ylab("Abundance")+
  scale_x_continuous(limits=c(1,11), breaks = c(1:10))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.y = element_text(size=15, color="black"),
        axis.title.y = element_text(size=15, color="black"),
        axis.text.x = element_text(size=15, color="black"),
        axis.title.x = element_text(size=15, color="black"))+
  annotate("text", x =2.4, y = 35, label='Ilex Opaca', size=4)+
  annotate("text", x =3.8, y = 33.5, label='Cornus florida', size=4)+
  annotate("text", x =7.4, y = 32.2, label='Lagerstroemia indica, Thuja occ.', size=4)+
  annotate("text", x =7, y = 29, label='Acer palmatum', size=4)+
  annotate("text", x =4.2, y = 25, label='Ailanthis alt.,Prunus sp.,\nAcer saccharinum', size=4)+
  annotate("text", x =7.4, y = 18, label='Acer rubrum', size=4)+
  annotate("text", x =8.8, y = 16.2, label='Acer negundo', size=4)

S_freq_tree <- 
  ggplot(data=subset(S_Frank, type=="tree"), aes(x=Frank, y=freq))+
  geom_point(size=4, color="goldenrod2")+
  theme()+
  xlab("Rank")+
  ylab("Number of Yards Present")+
  scale_x_continuous(limits=c(1,11), breaks = c(1:10))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.y = element_text(size=15, color="black"),
        axis.title.y = element_text(size=15, color="black"),
        axis.text.x = element_text(size=15, color="black"),
        axis.title.x = element_text(size=15, color="black"))+
  annotate("text", x =3, y = 39, label='Acer platanoides', size=4)+
  annotate("text", x =4.5, y =31, label='Populus tremuloides', size=4)+
  annotate("text", x =4.8, y = 28.8, label='Acer palmatum', size=4)+
  annotate("text", x =6.1, y = 26.8, label='Malus domestica', size=4)+
  annotate("text", x =7.2, y = 25, label='Pyrus calleryana', size=4)+
  annotate("text", x =7.9, y = 22, label='Picea pungens', size=4)+
  annotate("text", x =8.8, y = 18.7, label='Ulmus pumila', size=4)+
  annotate("text", x =5.1, y = 17, label='Acer negundo, Picea glauca', size=4)+
  annotate("text", x =7.1, y = 15, label='Malus hybrid, Prunus cerasus', size=4)
  
    
S_abund_tree<-
  ggplot(data=subset(S_Arank, type=="tree"), aes(x=Arank, y=abund))+
  geom_point(size=4, color="goldenrod2")+
  theme()+
  xlab("Rank")+
  ylab("Abundance")+
  scale_y_continuous(limits=c(20,90), breaks = c(20,30,40,50,60,70,80,90))+
  scale_x_continuous(limits=c(1,14), breaks = c(1:10))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.y = element_text(size=15, color="black"),
        axis.title.y = element_text(size=15, color="black"),
        axis.text.x = element_text(size=15, color="black"),
        axis.title.x = element_text(size=15, color="black"))+
  annotate("text", x =4, y = 88, label='Populus tremuloides', size=4)+
  annotate("text", x =4.4, y = 67.5, label='Acer platanoides', size=4)+
  annotate("text", x =5.6, y = 62.5, label='Quercus gambelii', size=4)+
  annotate("text", x =2, y = 38, label='Acer palm.', size=4, angle=30)+
  annotate("text", x =7.8, y = 56, label='Platycladus orientalis', size=4, angle=30)+
  annotate("text", x =9.4, y = 56, label='Acer negundo,Pyrus cal.', size=4, angle=30)+
  annotate("text", x =9.5, y = 48, label='Pinus nigra', size=4, angle=30)+
  annotate("text", x =11, y = 45, label='Picea pungens', size=4, angle=30)+
  annotate("text", x =12.2, y = 29, label='Picea glauca,\nMalus domestica', size=4)
  
#tree RAC
ggdraw()+
  draw_plot(B_freq_tree, x=0.036, y=.5, width=.45, height=.5)+
  draw_plot(B_abund_tree, x=.5, y=.5, width=.45, height=.5)+
  draw_plot(S_freq_tree, x=0.04, y=0, width=.45, height=.5)+
  draw_plot(S_abund_tree, x=.5, y=0, width=.45, height=.5)+
  draw_plot_label(label= c("A","B","C","D"), size=13,
                  x= c(0.01, 0.51, 0.01, 0.51), y = c(1, 1, 0.5,0.5))

#Flower RACS
#note- there's a 4-way tie for 11.5 rank
B_freq_flow <- 
  ggplot(data=subset(B_Frank, type=="floral"), aes(x=Frank, y=freq))+
  geom_point(size=4, color="steelblue3")+
  theme()+
  xlab("Rank")+
  ylab("Number of Yards Present")+
  scale_x_continuous(limits=c(1,11), breaks = c(1:10))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.y = element_text(size=15, color="black"),
        axis.title.y = element_text(size=15, color="black"),
        axis.text.x = element_text(size=15, color="black"),
        axis.title.x = element_text(size=15, color="black"))+
  annotate("text", x =2.8, y = 37, label='Hydrangea', size=4)+
  annotate("text", x =3, y = 32, label='Rosa', size=4)+
  annotate("text", x =4.5, y = 26.5, label='Rudbeckia', size=4)+
  annotate("text", x =5.2, y = 24, label='Hosta', size=4)+
  annotate("text", x =6.6, y = 16, label='Echinacea', size=4)+
  annotate("text", x =4, y = 12, label='Hemerocallis, Tagetes', size=4)+
  annotate("text", x =9.3, y = 13.5, label='Impatiens', size=4)+
  annotate("text", x =10.2, y = 11, label='Salvia', size=4)

B_abund_flow<-
  ggplot(data=subset(B_Arank, type=="floral"), aes(x=Arank, y=abund))+
  geom_point(size=4, color="steelblue3")+
  theme()+
  xlab("Rank")+
  ylab("Abundance")+
  scale_x_continuous(limits=c(1,13), breaks = c(1:10))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.y = element_text(size=15, color="black"),
        axis.title.y = element_text(size=15, color="black"),
        axis.text.x = element_text(size=15, color="black"),
        axis.title.x = element_text(size=15, color="black"))+
  annotate("text", x =3.2, y = 223, label='Catharanthus', size=4)+
  annotate("text", x =3.3, y = 193, label='Hemerocallis', size=4)+
  annotate("text", x =4.9, y = 180, label='Rudbeckia', size=4)+
  annotate("text", x =5.2, y = 157, label='Hosta', size=4)+
  annotate("text", x =4.5, y = 127, label='Hydrangea', size=4)+
  annotate("text", x =7.5, y = 117, label='Impatiens', size=4)+
  annotate("text", x =8.8, y = 107, label='Echinacea', size=4)+
  annotate("text", x =9.2, y = 95, label='Rosa', size=4)+
  annotate("text", x =7, y = 78, label='Lysimachia', size=4)+
  annotate("text", x =11.5, y = 76, label='Begonia', size=4)

S_freq_flow <- 
  ggplot(data=subset(S_Frank, type=="floral"), aes(x=Frank, y=freq))+
  geom_point(size=4, color="goldenrod2")+
  theme()+
  xlab("Rank")+
  ylab("Number of Yards Present")+
  scale_x_continuous(limits=c(1,11), breaks = c(1:10))+
  scale_y_continuous(limits=c(7,45), breaks = c(10, 20, 30, 40))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.y = element_text(size=15, color="black"),
        axis.title.y = element_text(size=15, color="black"),
        axis.text.x = element_text(size=15, color="black"),
        axis.title.x = element_text(size=15, color="black"))+
  annotate("text", x =2.1, y = 43, label='Rosa', size=4)+
  annotate("text", x =3, y =31, label='Salvia', size=4)+
  annotate("text", x =4.7, y = 27, label='Hemerocallis', size=4)+
  annotate("text", x =5.5, y = 22, label='Lavandula', size=4)+
  annotate("text", x =6.3, y = 19, label='Hibiscus, Petunia, Sedum', size=4)+
  annotate("text", x =7.1, y = 14, label='Hosta', size=4)+
  annotate("text", x =9.7, y = 16, label='Perovskia', size=4)+
  annotate("text", x =8.8, y = 10, label='Dianthus, Echinacea', size=4)


S_abund_flow<-
  ggplot(data=subset(S_Arank, type=="floral"), aes(x=Arank, y=abund))+
  geom_point(size=4, color="goldenrod2")+
  theme()+
  xlab("Rank")+
  ylab("Abundance")+
  scale_x_continuous(limits=c(1,11.5), breaks = c(1:10))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.y = element_text(size=15, color="black"),
        axis.title.y = element_text(size=15, color="black"),
        axis.text.x = element_text(size=15, color="black"),
        axis.title.x = element_text(size=15, color="black"))+
  annotate("text", x =2.2, y = 438, label='Petunia', size=4)+
  annotate("text", x =3.2, y = 255, label='Tagetes', size=4)+
  annotate("text", x =4.1, y = 216, label='Rosa', size=4)+
  annotate("text", x =2.5, y = 110, label='Geranium,\nHemerocallis', size=4)+
  annotate("text", x =6.3, y = 115, label='Salvia', size=4, angle=30)+
  annotate("text", x =7.5, y = 115, label='Lobelia', size=4, angle=30)+
  annotate("text", x =8.8, y = 115, label='Alyssum', size=4, angle=30)+
  annotate("text", x =10.8, y = 95, label='Sedum,\nZinnia', size=4, angle=30)

#Flower RAC
ggdraw()+
  draw_plot(B_freq_flow, x=0.036, y=.5, width=.45, height=.5)+
  draw_plot(B_abund_flow, x=.5, y=.5, width=.45, height=.5)+
  draw_plot(S_freq_flow, x=0.04, y=0, width=.45, height=.5)+
  draw_plot(S_abund_flow, x=.5, y=0, width=.45, height=.5)+
  draw_plot_label(label= c("A","B","C","D"), size=13,
                  x= c(0.01, 0.51, 0.01, 0.51), y = c(1, 1, 0.5,0.5))


#Tree traits
CC_TT_avg<-read.csv("CC_TT_avg.csv")

CC_TT_inc<-read.csv("CC_TT_inc.csv")

CC_TT_char<-read.csv("CC_TT_char.csv")

CC_TT_nb<-read.csv("CC_TT_nb.csv")

#CC_TT_char
CC_TFam<-CC_TT_char %>% 
  group_by(City) %>% 
  mutate(tot_abund=length(City)) %>% 
  group_by(City, tot_abund, Family) %>% 
  summarize(abund=length(Family)) %>% 
  mutate(rank=rank(-abund),
         per.Fam=abund/tot_abund)

CC_Tdecid<-CC_TT_char %>% 
  mutate(decid=as.character(decid)) %>% 
  group_by(City) %>% 
  mutate(tot_abund=length(City)) %>% 
  group_by(City, tot_abund, decid) %>% 
  summarize(abund=length(decid)) %>% 
  mutate(per.decid=abund/tot_abund)

ggplot(subset(CC_Tdecid, decid%in%c("evergreen")), aes(x= City, y=abund)) + 
  geom_bar(stat="identity", aes(fill=City))+ 
  scale_fill_manual(values=c("steelblue3","goldenrod2"))+ 
  ylab("Abundance of evergreens")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(), 
        axis.title.y = element_text(color="black", size=15),
        axis.text.x = element_text(color="black",size=15), 
        axis.text.y = element_text(color="black",size=15))

CC_Tcont<-CC_TT_char %>% 
  group_by(City) %>% 
  mutate(tot_abund=length(City)) %>% 
  group_by(City, tot_abund, Continent) %>% 
  summarize(abund=length(Continent)) %>% 
  mutate(per.cont=abund/tot_abund)

ggplot(subset(CC_Tcont, Continent%in%c("North America")), aes(x= City, y=per.cont)) + 
  geom_bar(stat="identity", aes(fill=City))+ 
  scale_fill_manual(values=c("steelblue3","goldenrod2"))+ 
  ylab("% Trees from North America")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(), 
        axis.title.y = element_text(color="black", size=15),
        axis.text.x = element_text(color="black",size=15), 
        axis.text.y = element_text(color="black",size=15))

#CC_TT ANOVA
summary(m1<-lm(mean_score~City*Front_Back*Med_Inc, data= CC_TT_nb, subset=(trait=="water")))
anova(m1)

#CC_TT_avg
ggplot(subset(CC_TT_avg, City%in%c("Salt Lake City")), aes(x=reorder(trait, mean_score), y=mean_score)) + 
  geom_bar(stat="identity", fill="goldenrod2")+ 
  coord_flip() + 
  geom_errorbar(aes(ymin=mean_score-se, ymax=mean_score+se), width=0.2)+
  ylim(0,1)+
  xlab("")+
  ylab("Average Tree Trait")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(color="black",size=15), 
        axis.text.x = element_text(color="black",size=20), 
        axis.text.y = element_text(color="black",size=13, angle=+18))

ggplot(subset(CC_TT_avg, trait%in%c("Height")), aes(x=City, y=mean_score)) + 
  geom_bar(stat="identity", aes(fill=City))+ 
  scale_fill_manual(values=c("steelblue3","goldenrod2"))+
  geom_errorbar(aes(ymin=mean_score-se, ymax=mean_score+se), width=0.2)+
  ylim(0,1)+
  ylab("Height")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(), 
        axis.title.y = element_text(color="black", size=15),
        axis.text.x = element_text(color="black",size=15), 
        axis.text.y = element_text(color="black",size=15),
        legend.title = element_blank(),
        legend.text = element_blank(),
        legend.position="none")

#CC_TT_FB
CC_TT_FB1<-read.csv("CC_TT_FB1.csv")
CC_TT_FB2<-read.csv("CC_TT_FB2.csv")

S_TT_FB2<-CC_TT_FB2 %>% 
  filter(City=="Salt Lake City")

B_TT_FB2<-CC_TT_FB2 %>% 
  filter(City!="Salt Lake City")

SF<-ggplot(subset(S_TT_FB2, Front_Back%in%c("F")), aes(x=trait, y=mean_score)) + 
  geom_bar(stat="identity", fill="goldenrod2")+ 
  coord_flip() + 
  geom_errorbar(aes(ymin=mean_score-se, ymax=mean_score+se), width=0.2)+
  ylim(0,1)+
  xlab("")+
  ylab("Average Tree Trait")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(color="black",size=15), 
        axis.text.x = element_text(color="black",size=20), 
        axis.text.y = element_text(color="black",size=13, angle=+18))+
  ggtitle("SLC Front")+
  geom_hline(yintercept = 0.5, linetype="dotted")

SB<-ggplot(subset(S_TT_FB2, Front_Back%in%c("B")), aes(x=trait, y=mean_score)) + 
  geom_bar(stat="identity", fill="goldenrod2")+ 
  coord_flip() + 
  geom_errorbar(aes(ymin=mean_score-se, ymax=mean_score+se), width=0.2)+
  ylim(0,1)+
  xlab("")+
  ylab("Average Tree Trait")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(color="black",size=15), 
        axis.text.x = element_text(color="black",size=20), 
        axis.text.y = element_text(color="black",size=13, angle=+18))+
  ggtitle("SLC Back")+
  geom_hline(yintercept = 0.5, linetype="dotted")

BF<-ggplot(subset(B_TT_FB2, Front_Back%in%c("F")), aes(x=trait, y=mean_score)) + 
  geom_bar(stat="identity", fill="steelblue3")+ 
  coord_flip() + 
  geom_errorbar(aes(ymin=mean_score-se, ymax=mean_score+se), width=0.2)+
  ylim(0,1)+
  xlab("")+
  ylab("Average Tree Trait")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(color="black",size=15), 
        axis.text.x = element_text(color="black",size=20), 
        axis.text.y = element_text(color="black",size=13, angle=+18))+
  ggtitle("BAL Front")+
  geom_hline(yintercept = 0.5, linetype="dotted")

BB<-ggplot(subset(B_TT_FB2, Front_Back%in%c("B")), aes(x=trait, y=mean_score)) + 
  geom_bar(stat="identity", fill="steelblue3")+ 
  coord_flip() + 
  geom_errorbar(aes(ymin=mean_score-se, ymax=mean_score+se), width=0.2)+
  ylim(0,1)+
  xlab("")+
  ylab("Average Tree Trait")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(color="black",size=15), 
        axis.text.x = element_text(color="black",size=20), 
        axis.text.y = element_text(color="black",size=13, angle=+18))+
  ggtitle("BAL Back")+
  geom_hline(yintercept = 0.5, linetype="dotted")

ggdraw()+
  draw_plot(SF, x=0.036, y=.5, width=.45, height=.5)+
  draw_plot(SB, x=.5, y=.5, width=.45, height=.5)+
  draw_plot(BF, x=0.04, y=0, width=.45, height=.5)+
  draw_plot(BB, x=.5, y=0, width=.45, height=.5)+
  draw_plot_label(label= c("A","B","C","D"), size=13,
                  x= c(0.01, 0.51, 0.01, 0.51), y = c(1, 1, 0.5,0.5))

ggplot(subset(CC_TT_FB2, trait%in%c("Edible Fruit")), aes(x=City_Loc, y=mean_score)) + 
  geom_bar(stat="identity", aes(fill=City))+ 
  scale_fill_manual(values=c("steelblue3","goldenrod2"))+
  geom_errorbar(aes(ymin=mean_score-se, ymax=mean_score+se), width=0.2)+
  ylim(0,1)+
  ylab("Edible Fruit")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(), 
        axis.title.y = element_text(color="black", size=15),
        axis.text.x = element_text(color="black",size=12, angle=20), 
        axis.text.y = element_text(color="black",size=15),
        legend.title = element_blank(),
        legend.text = element_blank(),
        legend.position="none")
