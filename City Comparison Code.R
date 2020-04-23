library(tidyverse)
library(codyn)
library(dplyr)
library(reshape)
library(ggplot2)
library(vegan)
library(naniar)

library(scales)


#commit #2 on 4/23 (test)


#lab computer 245
setwd("C:\\Users\\ablanch4\\Dropbox\\Time V Money\\SLC Comparison\\")

#Allie's laptop
setwd("C:\\Users\\Allie\\Dropbox\\Time V Money\\SLC Comparison\\")

#remove gray background in plots
theme_set(theme_bw(12))

#Baltimore Data read in and merging
BAL_AllSurveys_raw<-read.csv("BAL_Surveys_200407.csv") %>% 
  select(-Clean, -Notes.Comments) %>% 
  mutate(House_ID1=House_ID) %>% 
  separate(House_ID1, c("Nb", "House"))

B_nbinfo<-read.csv("BAL_NB_Codes_200407.csv") %>% 
  mutate(Nb=as.character(Nb),
         Med_Inc=med_inc,
         Nb=recode(Nb, "34"="0034")) %>% 
  select(Nb, Style, med_age, nb_inc, Med_Inc, size, yard_area)

BAL_Surveys<-BAL_AllSurveys_raw %>% 
  mutate(City="Baltimore")%>% 
  select(City, House_ID, Nb, A1, A201, A202, A203, A204, A205, A206, A207, A301, A302, A303, A4, A5, A6, A701, A702, A703, A704, A705, A706, A707, A708, A709, A710, B101, B102, B103, B104, B105, B106, B107, B108, B201, B202, B203, B204, B205, B206, B207, B301, B302, B401, B402, C5, C601)

All_Diversity_House_Balt<-read.csv("BAL_All_Diversity_House_200407.csv") %>% 
  mutate(Nb=as.character(Nb),
         Nb=recode(Nb, "34"="0034"),
         House_ID=paste(Nb, House, sep="_"))

BAL_Direct<-All_Diversity_House_Balt %>% 
  left_join(BAL_Surveys) %>% 
  left_join(B_nbinfo) %>% 
  mutate(Flower_Color=as.numeric(as.character(A702)),
         Biodiversity=as.numeric(as.character(A6)))

B_F_data1<-read.csv("BAL_Floral_Data_Balt18_clean_AB_200410.csv") %>% 
  mutate(num_plants=X..F.plants)

BAL_Trees_raw<-read.csv("BAL_Trees_Balt18_FB_clean_200413.csv") %>% 
  mutate(Nb=as.character(NB),
         Nb=recode(Nb, "34"="0034"),
         House_ID=paste(Nb,House, sep="_"),
         Species=Tree.species)

BAL_Lawns_raw<-read.csv("BAL_Lawn Quadrats_Balt18_AB_200407.csv") %>% 
  mutate(City="Baltimore",
         House_ID=paste(NB, House, sep="_"),
         Species=Species.combined,
         Nb=as.character(NB),
         Nb=recode(Nb, "34"="0034"),
         House_ID=paste(Nb, House, sep="_"))

#Salt Lake data read in and merging
SLC_AllSurveys_raw<-read.csv("SLC_2014 Homeowner_Survey Data_Final_homeowner ID intact_200407.csv") %>% 
  select(PARCEL_ID, A1, A201, A202, A203, A204, A205, A206, A207, A208, A301, A302, A303, A6, A7, A8, A901, A902, A903, A904, A905, A906, A907, A908, A909, A910, B101, B102, B103, B104, B105, B106, B107, B108, B201, B202, B203, B204, B205, B206, B207, B301, B302, B401, B402, D1, D2, D301, D4, D601A, D601B)

S_nbinfo<-read.csv("SLC_NB_details_200407.csv") %>% 
  select(-X) %>% 
  mutate(Nb=as.character(Nb))
S_parcels<-read.csv("SLC_Parcel_Info_200407.csv")%>% 
  mutate(Nb=NB_ID) %>% 
  filter(Responded!="REPEAT") %>% 
  select(Nb, House_ID, PARCEL_ID, Home_Value) %>% 
  separate(House_ID, c("Nb1","Block","House")) %>% 
  mutate(House_ID=paste(Nb1, Block, House, sep = "_"),
         House_ID=as.character(House_ID)) %>% 
  select(-Nb1) %>% 
  replace_with_na(replace = list(House_ID = "_NA_NA"))

SLC_Surveys<-merge(S_parcels, SLC_AllSurveys_raw, by="PARCEL_ID", all=F) %>% 
  mutate(City="Salt Lake City")

SLC_Direct<-read.csv("SLC_Direct_house_200407.csv") %>% 
  mutate(City="SLC",
         Flower_Color=as.numeric(as.character(A902)),
         Biodiversity=as.numeric(as.character(A8)))

S_F_data<-read.csv("SLC_Floral_data_all_cleaning_AMB_200410.csv")%>%
  filter(Nb!="NA")%>%
  filter(House_ID!="387_11_4")#drop b/c extra sampling point

S_F_data1<-S_F_data%>%
  mutate(num_flowering_stem=as.numeric(X.F.stems),
         flower_width=as.numeric(as.character(Flower.Width..cm.)),
         flower_length=as.numeric(as.character(Flower.Length..cm.)),
         flower_size=(flower_width*flower_length),
         num_flowers=num_flowering_stem*ave_flower_perstem,
         num_plants = X..F.plants,
         TotalFlower_area=(flower_size*num_flowers)/10000,
         Native_bin=as.numeric(as.character(Native_man)),
         Native_plants=Native_bin*num_plants,
         Water_man=as.numeric(as.character(Water_man)),
         City="Salt Lake City",
         Nb=as.character(Nb),
         FaGe=paste(Family, Genus, sep="_"))%>%
  select(City, Nb, Block, House_ID, Family, Genus, FaGe, Front.Back, Water_man, Native_plants, Native_bin, num_flowers, num_plants, color1, color2, TotalFlower_area, flower_size, Inf.Type, Symmetry, flowertype, photo_ID, notes)

SLC_Trees_raw<-read.csv("SLC_ResTrees_200403.csv") %>% 
  mutate(City="Salt Lake City",
         Nb=as.character(Nb))

SLC_Lawns_raw<-read.csv("SLC_Lawns2014_200407.csv")%>%
  filter(House_ID!="387_11_4",
         House_ID!="651_1_4") %>% 
  mutate(City="Salt Lake City",
         Nb=as.character(Nb))

#Average preferences data set up
S_Prefs_avg<-melt(SLC_Surveys, id=c("PARCEL_ID","House_ID","Nb", "Block", "House", "City"))%>%
  mutate(value2=(as.numeric(as.character(value))))%>%
  group_by(City, variable)%>%
  summarize(Importance=mean(value2, na.rm=T),
            sd=sd(as.numeric(value2), na.rm=T),
            n=length(value2[!is.na(value2)]))%>%
  mutate(se=sd/sqrt(n))%>%
  mutate(variable=recode(variable, A1="Satisfaction"),
         variable=recode(variable, A201="Unattractive"),
         variable=recode(variable, A202="Plant types"),
         variable=recode(variable, A203="Effort"),
         variable=recode(variable, A204="Time"),
         variable=recode(variable, A206="Money"),
         variable=recode(variable, A207="Too small"),
         variable=recode(variable, A208="Too large"),
         variable=recode(variable, A301="Similar"),
         variable=recode(variable, A302="Different"),
         variable=recode(variable, A303="Unsure/Neutral"),
         variable=recode(variable, A6="Natives"),
         variable=recode(variable, A7="Variety"),
         variable=recode(variable, A8="Biodiversity"),
         variable=recode(variable, A901="Leaf Color"),
         variable=recode(variable, A902="Flower Color"),
         variable=recode(variable, A903="Flower Type"),
         variable=recode(variable, A904="Plant Shape"),
         variable=recode(variable, A905="Plant Height"),
         variable=recode(variable, A906="Seasonal Color"),
         variable=recode(variable, A907="Leaf Texture"),
         variable=recode(variable, A908="Plant Type"),
         variable=recode(variable, A909="Tree Species"),
         variable=recode(variable, A910="Ornamental Species"),
         variable=recode(variable, B101="Shade"),
         variable=recode(variable, B102="Fruit"),
         variable=recode(variable, B103="Flowers"),
         variable=recode(variable, B104="Play/relax"),
         variable=recode(variable, B105="Wind break"),
         variable=recode(variable, B106="Aroma"),
         variable=recode(variable, B107="Beauty"),
         variable=recode(variable, B108="Habitat"),
         variable=recode(variable, B201="Debris"),
         variable=recode(variable, B202="Reduce night vis"),
         variable=recode(variable, B203="Water use"),
         variable=recode(variable, B204="Sidewalk damage"),
         variable=recode(variable, B205="Blocks views"),
         variable=recode(variable, B206="Pollen"),
         variable=recode(variable, B207="Cost"),
         variable=recode(variable, D1="Lawn Co F"),
         variable=recode(variable, D2="Personally F"))


B_Prefs_avg<-melt(BAL_Surveys, id=c("House_ID","Nb", "City"))%>%
  mutate(value2=(as.numeric(as.character(value))))%>%
  group_by(City, variable)%>%
  summarize(Importance=mean(value2, na.rm=T),
            sd=sd(as.numeric(value2), na.rm=T),
            n=length(value2[!is.na(value2)]))%>%
  mutate(se=sd/sqrt(n))%>%
  mutate(variable=recode(variable, A1="Satisfaction"),
         variable=recode(variable, A201="Unattractive"),
         variable=recode(variable, A202="Plant types"),
         variable=recode(variable, A203="Effort"),
         variable=recode(variable, A204="Time"),
         variable=recode(variable, A205="Money"),
         variable=recode(variable, A206="Too small"),
         variable=recode(variable, A207="Too large"),
         variable=recode(variable, A301="Similar"),
         variable=recode(variable, A302="Different"),
         variable=recode(variable, A303="Unsure/Neutral"),
         variable=recode(variable, A4="Natives"),
         variable=recode(variable, A5="Variety"),
         variable=recode(variable, A6="Biodiversity"),
         variable=recode(variable, A701="Leaf Color"),
         variable=recode(variable, A702="Flower Color"),
         variable=recode(variable, A703="Flower Type"),
         variable=recode(variable, A704="Plant Shape"),
         variable=recode(variable, A705="Plant Height"),
         variable=recode(variable, A706="Seasonal Color"),
         variable=recode(variable, A707="Leaf Texture"),
         variable=recode(variable, A708="Plant Type"),
         variable=recode(variable, A709="Tree Species"),
         variable=recode(variable, A710="Ornamental Species"),
         variable=recode(variable, B101="Shade"),
         variable=recode(variable, B102="Fruit"),
         variable=recode(variable, B103="Flowers"),
         variable=recode(variable, B104="Play/relax"),
         variable=recode(variable, B105="Wind break"),
         variable=recode(variable, B106="Aroma"),
         variable=recode(variable, B107="Beauty"),
         variable=recode(variable, B108="Habitat"),
         variable=recode(variable, B201="Debris"),
         variable=recode(variable, B202="Reduce night vis"),
         variable=recode(variable, B203="Water use"),
         variable=recode(variable, B204="Sidewalk damage"),
         variable=recode(variable, B205="Blocks views"),
         variable=recode(variable, B206="Pollen"),
         variable=recode(variable, B207="Cost"),
         variable=recode(variable, C5="Lawn Co F"),
         variable=recode(variable, C601="Personally F"))

CC_Prefs_avg<-B_Prefs_avg %>% 
  full_join(S_Prefs_avg)

write.csv(CC_Prefs_avg, file = "CC_Prefs_avg.csv", row.names = F)

#Preferences by income data set up
S_Prefs_inc<-melt(SLC_Surveys, id=c("PARCEL_ID","House_ID","Nb", "Block", "House", "City"))%>%
  mutate(value2=(as.numeric(as.character(value))),
         Nb=as.character(Nb))%>%
  left_join(S_nbinfo) %>% 
  group_by(City, Med_Inc, variable)%>%
  summarize(Importance=mean(value2, na.rm=T),
            sd=sd(as.numeric(value2), na.rm=T),
            n=length(value2[!is.na(value2)]))%>%
  mutate(se=sd/sqrt(n))%>%
  mutate(variable=recode(variable, A1="Satisfaction"),
         variable=recode(variable, A201="Unattractive"),
         variable=recode(variable, A202="Plant types"),
         variable=recode(variable, A203="Effort"),
         variable=recode(variable, A204="Time"),
         variable=recode(variable, A206="Money"),
         variable=recode(variable, A207="Too small"),
         variable=recode(variable, A208="Too large"),
         variable=recode(variable, A301="Similar"),
         variable=recode(variable, A302="Different"),
         variable=recode(variable, A303="Unsure/Neutral"),
         variable=recode(variable, A6="Natives"),
         variable=recode(variable, A7="Variety"),
         variable=recode(variable, A8="Biodiversity"),
         variable=recode(variable, A901="Leaf Color"),
         variable=recode(variable, A902="Flower Color"),
         variable=recode(variable, A903="Flower Type"),
         variable=recode(variable, A904="Plant Shape"),
         variable=recode(variable, A905="Plant Height"),
         variable=recode(variable, A906="Seasonal Color"),
         variable=recode(variable, A907="Leaf Texture"),
         variable=recode(variable, A908="Plant Type"),
         variable=recode(variable, A909="Tree Species"),
         variable=recode(variable, A910="Ornamental Species"),
         variable=recode(variable, B101="Shade"),
         variable=recode(variable, B102="Fruit"),
         variable=recode(variable, B103="Flowers"),
         variable=recode(variable, B104="Play/relax"),
         variable=recode(variable, B105="Wind break"),
         variable=recode(variable, B106="Aroma"),
         variable=recode(variable, B107="Beauty"),
         variable=recode(variable, B108="Habitat"),
         variable=recode(variable, B201="Debris"),
         variable=recode(variable, B202="Reduce night vis"),
         variable=recode(variable, B203="Water use"),
         variable=recode(variable, B204="Sidewalk damage"),
         variable=recode(variable, B205="Blocks views"),
         variable=recode(variable, B206="Pollen"),
         variable=recode(variable, B207="Cost"),
         variable=recode(variable, D1="Lawn Co F"),
         variable=recode(variable, D2="Personally F"))

B_Prefs_inc<-melt(BAL_Surveys, id=c("House_ID","Nb", "City"))%>%
  mutate(value2=(as.numeric(as.character(value))))%>%
  left_join(B_nbinfo) %>% 
  group_by(City, Med_Inc, variable)%>%
  summarize(Importance=mean(value2, na.rm=T),
            sd=sd(as.numeric(value2), na.rm=T),
            n=length(value2[!is.na(value2)]))%>%
  mutate(se=sd/sqrt(n))%>%
  mutate(variable=recode(variable, A1="Satisfaction"),
         variable=recode(variable, A201="Unattractive"),
         variable=recode(variable, A202="Plant types"),
         variable=recode(variable, A203="Effort"),
         variable=recode(variable, A204="Time"),
         variable=recode(variable, A205="Money"),
         variable=recode(variable, A206="Too small"),
         variable=recode(variable, A207="Too large"),
         variable=recode(variable, A301="Similar"),
         variable=recode(variable, A302="Different"),
         variable=recode(variable, A303="Unsure/Neutral"),
         variable=recode(variable, A4="Natives"),
         variable=recode(variable, A5="Variety"),
         variable=recode(variable, A6="Biodiversity"),
         variable=recode(variable, A701="Leaf Color"),
         variable=recode(variable, A702="Flower Color"),
         variable=recode(variable, A703="Flower Type"),
         variable=recode(variable, A704="Plant Shape"),
         variable=recode(variable, A705="Plant Height"),
         variable=recode(variable, A706="Seasonal Color"),
         variable=recode(variable, A707="Leaf Texture"),
         variable=recode(variable, A708="Plant Type"),
         variable=recode(variable, A709="Tree Species"),
         variable=recode(variable, A710="Ornamental Species"),
         variable=recode(variable, B101="Shade"),
         variable=recode(variable, B102="Fruit"),
         variable=recode(variable, B103="Flowers"),
         variable=recode(variable, B104="Play/relax"),
         variable=recode(variable, B105="Wind break"),
         variable=recode(variable, B106="Aroma"),
         variable=recode(variable, B107="Beauty"),
         variable=recode(variable, B108="Habitat"),
         variable=recode(variable, B201="Debris"),
         variable=recode(variable, B202="Reduce night vis"),
         variable=recode(variable, B203="Water use"),
         variable=recode(variable, B204="Sidewalk damage"),
         variable=recode(variable, B205="Blocks views"),
         variable=recode(variable, B206="Pollen"),
         variable=recode(variable, B207="Cost"),
         variable=recode(variable, C5="Lawn Co F"),
         variable=recode(variable, C601="Personally F"))

CC_Prefs_inc<-B_Prefs_inc %>% 
  full_join(S_Prefs_inc)

#Section A: Landscaping preferences (average)
#A1: How satisfied are you with your current landscaping?
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

#A2: If you are disatisfied with your yard, why?
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

#A3: Do you prefer neighborhoods with houses that all look...
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

#A4-6/A6-8: How important is it to you to have plants that: create variety, have high biodiversity, are native?
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


#Section A: Landscaping preferences (by income)
ggplot(subset(CC_Prefs_inc, variable%in%c("Flower Color")), aes(x=Med_Inc, y=Importance, group=City)) + 
  geom_point(size=3, aes(color=City))+
  scale_color_manual(values=c("steelblue3","goldenrod2"))+
  geom_errorbar(aes(ymin=Importance-se, ymax=Importance+se), width=0.2)+
  xlab("Median Neighborhood Income")+
  ylab("Importance of Color Variety")+
  ylim(0,3)+
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
         subset=variable=="Flower Color")

CC_Variety_inc<-CC_Prefs_inc %>% 
  filter(variable=="Variety")

ggplot(subset(CC_Prefs_inc, variable%in%c("Flower Color")), aes(x=Med_Inc, y=Importance, group=City)) + 
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
  annotate("text", x=100000, y=0.5, label="BAL: r = 0.725, p = 0.008**\nSLC: r = -0.411, p = 0.271", size=4)

cor.test(formula = ~ Importance+Med_Inc,
         data = B_Prefs_inc,
         subset=variable=="Variety")

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

cor.test(formula = ~ Importance+Med_Inc,
         data = S_Prefs_inc,
         subset=variable=="Biodiversity")

#Section B: Tree Preferences (average)
#B1: Tree attributes
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

#B2: Tree costs/maintenance
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


#B3 & B4
CC_B3<-SLC_Surveys %>%
  select(City, Nb, House_ID, PARCEL_ID, B301, B302) %>% 
  mutate(Nb=as.character(Nb)) %>% 
  full_join(BAL_Surveys) %>% 
  select(City, Nb, House_ID, PARCEL_ID, B301, B302)

write.csv(CC_B3, file="CC_B3.csv", row.names = F)

CC_B4<-SLC_Surveys %>%
  select(City, Nb, House_ID, PARCEL_ID, B401, B402) %>% 
  mutate(Nb=as.character(Nb)) %>% 
  full_join(BAL_Surveys) %>% 
  select(City, Nb, House_ID, PARCEL_ID, B401, B402)

write.csv(CC_B4, file="CC_B4.csv", row.names = F)

#Fertilizer application: lawn co. or personal (C5-601 / D1-2)
CC_LawnCo_F<-BAL_Surveys %>% 
  select(City, C5) %>% 
  mutate(D1=C5) %>% 
  select(-C5) %>% 
  full_join(SLC_Surveys) %>% 
  select(City, D1) %>% 
  mutate(Lawn_Co= as.character(D1)) %>% 
  group_by(City, Lawn_Co) %>% 
  summarize(n=length(Lawn_Co))

CC_Personal_F<-BAL_Surveys %>% 
  select(City, C601) %>% 
  mutate(D2=C601) %>% 
  select(-C601) %>% 
  full_join(SLC_Surveys) %>% 
  select(City, D2) %>% 
  mutate(Personally=as.character(D2)) %>% 
  group_by(City, Personally) %>% 
  summarize(n=length(Personally))

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
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(color="black",size=15), 
        axis.title.y = element_text(color="black", size=15),
        axis.text.x = element_text(color="black",size=20), 
        axis.text.y = element_text(color="black",size=20))+
  xlab("Importance of Color Variety")+
  ylab("Number of Flower Colors")+
  ylim(0,21)+
  annotate("text", x=1.2, y=19, label="r = 0.123, p = 0.276", size=6)+
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
  annotate("text", x=1, y=34, label="r = 0.270, p = 0.015", size=6)+
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

S_Lawns_nmds<-SLC_Lawns_raw
select(-Type) %>% 
  left_join(S_nbinfo) %>% 
  select(Nb, House_ID, City, Species, nb_inc, F1, F2, B1, B2)

CC_Lawns_nmds<-S_Lawns_nmds %>% 
  full_join(B_Lawns_nmds) %>% 
  gather(plot, cover, F1:B2) %>% 
  group_by(Nb, City, nb_inc, Species) %>% 
  summarize(mcov=mean(cover, na.rm = T)) %>% 
  filter(Species!="No Lawn",
         Species!="NO LAWN") %>% 
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
S_F_data2<-S_F_data1 %>% 
  left_join(S_nbinfo) %>%
  group_by(City, Nb, FaGe) %>% 
  summarize(numplants=sum(num_plants)) %>% 
  filter(numplants!=0)


B_F_data2<-B_F_data1 %>% 
  mutate(City="Baltimore",
         Nb=as.character(Nb),
         FaGe=paste(Family, Genus, sep="_"))%>% 
  left_join(B_nbinfo) %>%
  group_by(City, Nb, FaGe) %>% 
  summarize(numplants=sum(num_plants)) %>% 
  filter(numplants!=0)

CC_Flowers_nmds<-S_F_data2 %>% 
  full_join(B_F_data2) %>% 
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
  filter(Species!="no trees") %>% 
  mutate(City="Baltimore") %>% 
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
#Richness count function
richcount<-function(x){
  x1<-x[x!=0]
  x2<-unique(x1)
  length(x2)
}

abundcount<-function(x){
  x1<-x[x!=0]
  length(x1)
}

#Prepping Baltimore data for richness/abundance
B_rich_abund<-All_Diversity_House_Balt %>% 
  mutate(City="Baltimore",
         Lawn_rich=l.rich,
         Flower_rich=f.rich,
         Flower_abund=nplants,
         Tree_rich=t.rich,
         Tree_abund=num.trees,
         Nb=as.character(Nb)) %>% 
  left_join(B_nbinfo) %>% 
  select(City, Nb, House_ID, Med_Inc, Lawn_rich, Flower_rich, Flower_abund,Tree_rich, Tree_abund, Med_Inc)

#Prepping SLC data for richness/abundance
S_Lawns_rich<-SLC_Lawns_raw%>% 
  mutate(Species=ifelse(Species=="No Lawn",0, as.character(Species)),
         House_ID=as.character(House_ID),
         Nb=as.character(Nb))%>%
  group_by(City, Nb, House_ID) %>% 
  summarize(Lawn_rich=ABcount(Species))

S_Trees_richabund<-SLC_Trees_raw %>% 
  mutate(Species=ifelse(Species=="",0, as.character(Species)),
         House_ID=as.character(House_ID))%>%
  group_by(City, Nb, House_ID) %>% 
  summarize(Tree_rich=richcount(Species),
            Tree_abund=abundcount(Species)) %>% 
  left_join(S_nbinfo) %>% 
  select(City, Nb, House_ID, Med_Inc,Tree_rich, Tree_abund)

S_Flowers_richabund<-S_F_data1 %>% 
  mutate(FaGe=ifelse(FaGe=="NoFlowers_NoFlowers",0, as.character(FaGe)),
         House_ID=as.character(House_ID)) %>% 
  group_by(City, Nb, House_ID) %>% 
  summarize(Flower_rich=ABcount(FaGe),
            Flower_abund=sum(num_plants)) %>% 
  select(City, Nb, House_ID, Flower_rich, Flower_abund)

S_rich_abund<-merge(S_Lawns_rich, S_Trees_richabund) %>% 
  full_join(S_Flowers_richabund)

#Both cities data prep/merge for richness/abundance
B_parcels<-read.csv("BAL_YardArea_200413.csv") %>% 
  mutate(Home_Value=HOME_VALUE,
         City="Baltimore") %>% 
  select(City, House_ID, Home_Value)

CC_HomeValue<-S_parcels %>% 
  mutate(City="Salt Lake City") %>% 
  full_join(B_parcels) %>% 
  select(City, House_ID, Home_Value)

CC_rich_abund<-B_rich_abund %>% 
  full_join(S_rich_abund) %>% 
  left_join(CC_HomeValue)

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
  annotate("text", x=780000, y=25, label="BAL: r = -0.173, p = 0.132\nSLC: r = -0.390, p < 0.001", size=3.5)

cor.test(formula = ~ Home_Value+Lawn_rich,
         data = CC_rich_abund,
         subset=City=="Baltimore")

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
  annotate("text", x=780000, y=27, label="BAL: r = 0.323, p = 0.002\nSLC: r = 0.306, p = 0.006", size=3.5)

cor.test(formula = ~ Home_Value+Flower_rich,
         data = CC_rich_abund,
         subset=City=="Salt Lake City")

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
  annotate("text", x=780000, y=4, label="BAL: r = 0.552, p < 0.001\nSLC: r = 0.357, p = 0.001", size=3.5)

cor.test(formula = ~ Home_Value+Tree_rich,
         data = CC_rich_abund,
         subset=City=="Baltimore")

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
  annotate("text", x=780000, y=200, label="BAL: r = 0.365, p < 0.001\nSLC: r = 0.341, p = 0.002", size=3.5)

cor.test(formula = ~ Home_Value+Flower_abund,
         data = CC_rich_abund,
         subset=City=="Salt Lake City")

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
  annotate("text", x=780000, y=, label="BAL: r = 0.598, p < 0.001\nSLC: r = 0.361, p = 0.001", size=3.5)

cor.test(formula = ~ Home_Value+Tree_abund,
         data = CC_rich_abund,
         subset=City=="Salt Lake City")

#Richness by median nb income data prep
CC_ra_MedInc<-CC_rich_abund %>% 
  filter(Med_Inc!="NA") %>% 
  group_by(City, Med_Inc) %>% 
  summarize(Lawn_rich_avg=mean(Lawn_rich, na.rm=T),
            Flower_rich_avg=mean(Flower_rich),
            Flower_abund_avg=mean(Flower_abund),
            Tree_rich_avg=mean(Tree_rich),
            Tree_abund_avg=mean(Tree_abund),
            sd_LRA=sd(Lawn_rich, na.rm=T),
            n_LRA=length(Lawn_rich[!is.na(Lawn_rich)]),
            sd_FRA=sd(Flower_rich),
            n_FRA=length(Flower_rich[!is.na(Flower_rich)]),
            sd_FAA=sd(Flower_abund),
            n_FAA=length(Flower_abund[!is.na(Flower_abund)]),
            sd_TRA=sd(Tree_rich),
            n_TRA=length(Tree_rich[!is.na(Tree_rich)]),
            sd_TAA=sd(Tree_abund),
            n_TAA=length(Tree_abund[!is.na(Tree_abund)])) %>% 
  mutate(se_LRA=sd_LRA/sqrt(n_LRA),
         se_FRA=sd_FRA/sqrt(n_FRA),
         se_FAA=sd_FAA/sqrt(n_FAA),
         se_TRA=sd_TRA/sqrt(n_TRA),
         se_TAA=sd_TAA/sqrt(n_TAA))

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
         subset=City=="Salt Lake City")

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
         subset=City=="Salt Lake City")

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
         subset=City=="Salt Lake City")

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

#SLC rank abundance curves


#############################################################


#Non-cleaned code: most of this code is copied directly from SLC analyses, so may be some filters, etc. that got carried over that aren't actually applicable or necessary. 

#Flower and grass divergences (Figure 5 in SLC paper) 
Toplot.FL_Balt<-All_Diversity_House_Balt %>% 
  filter(l.rich!="NA") %>% 
  left_join(nb_info_Balt) %>% 
  select(Nb, House_ID, med_inc,l.rich, f.rich)

PlotFL_melt.B<-melt(Toplot.FL_Balt, id=c("Nb","House_ID","med_inc"))%>%
  mutate(variable=recode(variable, l.rich="Lawn"),
         variable=recode(variable, f.rich="Flowers")) %>% 
  group_by(Nb,variable, med_inc)%>%
  summarize(Num.Species = mean(as.numeric(value, na.rm=T)),
            sd=sd(as.numeric(value, na.rm=T)),
            n=length(value[!is.na(value)]))%>%
  mutate(se=sd/sqrt(n))

ggplot(data = PlotFL_melt.B, aes(x=med_inc, y = Num.Species, group=variable))+
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
Toplot.TW_Balt<-Lawns_Balt %>% 
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
  left_join(nb_info_Balt)

ggplot(data=Toplot.TW_Balt, aes(x=med_inc, y=Num.Species_avg, group=Type))+
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





#Yard Colors
pie.test.1.B<-data1.B%>%
  filter(color1!="NoFlowers"&color1!="NoPhoto")%>%
  mutate(Color_area=(ifelse(color2!="", 0.5*TotalFlower_area,1*TotalFlower_area)))%>%
  left_join(nb_info_Balt)%>%
  select(Nb, House_ID, color1, color2,Color_area, nb_inc)

##across
pie.across.B<-melt(pie.test.1.B, id=c("Nb","House_ID","Color_area","nb_inc"))%>%
  filter(value!=""&value!="brown")%>%
  mutate(nb_inc=recode(nb_inc, low="Low"),
         nb_inc=recode(nb_inc, mid="Mid"),
         nb_inc=recode(nb_inc, high="High"),
         value=recode(value, blue="Blue")) %>% 
  group_by(value, nb_inc)%>%
  summarize(Sum_colarea=sum(Color_area, na.rm=T))

,
value=recode(value, Purple="Purple"),
value=recode(value, red-purple="Red-Purple"),
value=recode(value, red="Red"),
value=recode(value, red-orange="Red-Orange"),
value=recode(value, orange="Orange"),
value=recode(value, yellow-orange="Yellow_orange"),
value=recode(value, yellow="Yellow"),
value=recode(value, green-yellow="Green-Yellow"),
value=recode(value, green="Green"),
value=recode(value, white="White")


pie.across.B$value<-factor(pie.across.B$value, levels = c("Blue","purple-blue", "purple","red-purple", "red", "red-orange","orange","yellow-orange","yellow", "green-yellow", "green", "white"))


pie.across.B$nb_inc<-factor(pie.across.B$nb_inc, levels = c("Low", "Mid", "High"))

##all together
point <- format_format(big.mark = ",", decimal.mark = ",", scientific = FALSE)

ggplot(pie.across.B, aes(x = nb_inc, y=Sum_colarea, fill=value))+
  geom_bar(color="black", width = 0.75,stat="identity")+
  scale_y_continuous(labels = point)+
  scale_fill_manual(values=c("blue", "darkviolet", "purple","violetred3", "red","orangered1","orange","goldenrod1","yellow","yellowgreen","green","white"))+
  ylab("Color Area (m2)")+
  xlab("Neighborhood Income")+
  theme(axis.ticks=element_blank(),
        legend.title=element_blank(),
        legend.text=element_text(color="black",size=14),
        panel.grid=element_blank(),
        axis.title.x = element_text(color="black",size=20), 
        axis.title.y = element_text(color="black", size=20),
        axis.text.x = element_text(color="black",size=20), 
        axis.text.y = element_text(color="black",size=20))



#Front/Back Differences
FBflower_sums<-data1.B%>%
  group_by(Nb, House_ID, Front.Back)%>%
  summarize(total_plants = sum(num_plants),
            total_flower = sum(num_flowers),
            total_area = sum(TotalFlower_area, na.rm=T),
            total_genera = length(Genus))%>%
  group_by(Nb, Front.Back)%>%
  summarize(plants=mean(total_plants),
            sd.plants=sd(total_plants),
            n.plants=length(total_plants),
            flowers=mean(total_flower),
            area=mean(total_area),
            richnessAB=mean(total_genera))%>%
  mutate(se.plants=sd.plants/sqrt(n.plants))%>%
  left_join(nb_info_Balt)

ggplot(data = FBflower_sums, aes(x=med_inc, y = plants, group=Front.Back))+
  geom_point(size=3, aes(color=Front.Back))+
  geom_errorbar(aes(ymin=plants-se.plants, ymax=plants+se.plants, color=Front.Back), width=0.2)+
  geom_smooth(method="lm", se=F, aes(color=Front.Back))+
  guides(color=guide_legend(reverse=TRUE))+
  xlab("Median Neighborhood Income (Baltimore)")+
  ylab("Number Flowering Plants")+
  scale_color_manual(values=c("blue","sienna3"))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

#Lawn vs Flower Richness
Toplot.Flow.R<-community_structure(rich_calc.1, abundance.var= "FaGe.sum", replicate.var="House_ID")%>%
  select(-Evar)%>%
  left_join(nb_link.B)

###lawn richness
lawn.B<-read.csv("Lawn Quadrats_Balt18_072519.csv")

lawn2.B<-subset(lawn.B, Species.combined!="NO LAWN"&Species.combined!="Not collected")%>%
  mutate(AveCovF = (F1+F2)/2,
         AveCovB = (B1+B2)/2)

lawn2.B$AveCovAll<-rowMeans(lawn2.B[c("AveCovB", "AveCovF")], na.rm = T)

Toplot.Lawn.R<-lawn2.B%>%
  group_by(Nb, House_ID)%>%
  summarize(Lawnrich=length(unique(Species.combined)))

###flower lawn rich combined
Toplot.LF<-merge(Toplot.Lawn.R, Toplot.Flow.R, by=c("Nb","House_ID"), all=T)
Toplot.LF[is.na(Toplot.LF)]<-0
PlotLF_melt<-melt(Toplot.LF, id=c("Nb","House_ID"))%>%
  group_by(Nb,variable)%>%
  summarize(Num.Species = mean(as.numeric(value)),
            sd=sd(as.numeric(value)),
            n=length(as.numeric(value)))%>%
  mutate(se=sd/sqrt(n)) %>% 
  left_join(nb_info_Balt)%>%
  mutate(variable=recode(variable, Lawnrich="Lawn"),
         variable=recode(variable, richness="Flowers"))

ggplot(data = PlotLF_melt, aes(x=med_inc, y = Num.Species, group=variable))+
  geom_point(size=3, aes(color=variable))+
  geom_errorbar(aes(ymin=Num.Species-se, ymax=Num.Species+se, color=variable), width=0.2)+
  guides(color=guide_legend(reverse=TRUE))+
  geom_smooth(method="lm", se=F, aes(color=variable))+
  xlab("Median Neighborhood Income (Baltimore)")+
  ylab("Species/Genus Richness")+
  scale_color_manual(values=c("green4","mediumvioletred"))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

#Lawn and weeds richness
##STILL WORKING ON THIS ONE, FINISH UP TWEAKING
Grass.Weed.R<-lawn2.B%>%
  mutate(Type=recode(Type, seedling="Weed"),
         Type=recode(Type, lawn="Turf Grass"),
         House_ID=as.character(House_ID))%>%
  group_by(House_ID, Type)%>%
  summarize(Type.sum=length(unique(Species.combined)))%>%
  left_join(nb_link.B)%>%
  left_join(nb_info_Balt)%>%
  group_by(med_inc, Type)%>%
  summarize(Type.mean=mean(Type.sum),
            sd=sd(Type.sum),
            n=length(Type.sum)) %>% 
  mutate(se=sd/sqrt(n))

ggplot(data=Grass.Weed.R, aes(x=med_inc, y=Type.mean, group=Type))+
  geom_point(size=3, aes(color=Type))+
  geom_errorbar(aes(ymin=Type.mean-se, ymax=Type.mean+se, color=Type), width=0.2)+
  geom_smooth(method="lm", se=F, aes(color=Type))+
  guides(color=guide_legend(reverse=TRUE))+
  xlab("Median Neighborhood Income")+
  ylab("Species Richness")+
  scale_color_manual(values=c("green4","goldenrod"))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())



#######################################


#Baltimore data read in
Houses_surveyed<-read.csv("Houses surveyed_DHL.csv") %>% 
  select(Nb, House, House_ID)


Surveys_Balt<-read.csv("2018 Homeowner_Survey Data_081219_Balt.csv")
Surv_visit.B<-Surveys_Balt %>% 
  right_join(Houses_surveyed) %>% 
  select(-Clean, -Notes.Comments) %>% 
  mutate(City="Baltimore")




