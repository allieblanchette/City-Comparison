library(tidyverse)
library(codyn)
library(dplyr)
library(reshape)
library(ggplot2)
library(vegan)
library(naniar)

library(scales)


#lab computer 245
#setwd("C:\\Users\\ablanch4\\Dropbox\\Time V Money\\SLC Comparison\\")

#Allie's laptop
setwd("C:\\Users\\Allie\\Dropbox\\Time V Money\\SLC Comparison\\")

#remove gray background in plots
theme_set(theme_bw(12))

#Baltimore Data read in and merging
B_nbinfo<-read.csv("BAL_NB_Codes_200407.csv") %>% 
  mutate(Nb=as.character(Nb),
         Med_Inc=med_inc,
         Nb=recode(Nb, "34"="0034")) %>% 
  select(Nb, Style, med_age, nb_inc, Med_Inc)

#House keys linking House_ID and ACCTID 
#(had to update 18 BUCHANAN RD house 5003_101 address between the s_r_p and Houses_mailed files)
select_residential_parcels<-read.csv("BAL_select_residential_parcels_200424.csv") %>% 
  select(ACCTID, ADDRESS, YEARBLT, Shape_Area)

Houses_mailed<-read.csv("BAL_Houses to mail_selected_AB_200424.csv") %>% 
  filter(Home_ID!="") %>% 
  separate(Home_ID, c("Nb","House")) %>% 
  mutate(Nb=as.character(Nb),
         Nb=recode(Nb, "34"="0034"),
         House=as.character(House),
         House=recode(House, "1"="01"),
         House=recode(House, "2"="02"),
         House=recode(House, "3"="03"),
         House=recode(House, "4"="04"),
         House=recode(House, "5"="05"),
         House=recode(House, "6"="06"),
         House=recode(House, "7"="07"),
         House=recode(House, "8"="08"),
         House=recode(House, "9"="09"),
         House_ID=paste(Nb, House, sep="_")) %>% 
  select(House_ID, ADDRESS)

B_parcels_mailed<-merge(Houses_mailed, select_residential_parcels, by="ADDRESS", all=F) %>% 
  select(-ADDRESS) %>% 
  unique() %>% 
  mutate(House_ID=as.character(House_ID))

B_key_mailed_1235<-B_parcels_mailed %>% 
  select(-Shape_Area) %>% 
  unique()

write.csv(B_key_mailed_1235, file="B_key_mailed_1235.csv", row.names = F)

BAL_AllSurveys_raw<-read.csv("BAL_Surveys_200407.csv") %>% 
  select(-Clean, -Notes.Comments) %>% 
  mutate(House_ID1=House_ID) %>% 
  separate(House_ID1, c("Nb", "House"))


#filtering out duplicate houses (because they had 2 different parcel areas) only for the houses that ultimately were surveyed- not whole nb
B_parcels_1<-read.csv("BAL_housesmailed_area_homevalue.csv") %>% 
  mutate(Home_Value=new_full_market_total_value,
         Parcel_Area=Shape_Area,
         City="Baltimore") %>% 
  select(ACCTID, Home_Value, Parcel_Area) %>% 
  inner_join(B_key_mailed_1235) %>% 
  mutate(House_ID_rep=House_ID) %>% 
  separate(House_ID_rep, c("Nb","House")) %>% 
  mutate(Nb=as.character(Nb)) %>% 
  left_join(B_nbinfo) %>% 
  filter(Parcel_Area>105)

#left off here- need to get rid of ' in ACCTID for BACO
B_parcels_1<-read.csv("B_mailed_with_area_20200518.csv") %>%  
  mutate(Home_Value=new_full_market_total_value,
         Parcel_Area=Shape_Area,
         City="Baltimore",
         ACCTID=sub("'", "", ACCTID)) %>% 
  select(ACCTID, Home_Value, Parcel_Area) %>% 
  inner_join(B_key_mailed_1235) %>% 
  mutate(House_ID_rep=House_ID) %>% 
  separate(House_ID_rep, c("Nb","House")) %>% 
  mutate(Nb=as.character(Nb)) %>% 
  left_join(B_nbinfo) %>% 
  filter(Parcel_Area>105)

#filtered Parcel_area>105 b/c there were duplicates with incorrect areas (ex. 1003_02 had area of like 10 and then area of 165) and then filtered other 3 areas b/c there were other houses with duplicate incorrect areas that were greater than 105...so I just filtered for the smaller area from the 3 that we got surveys from: 2002_28, 5003_01, 5003_02)

B_parcels_2002_edit<-B_parcels_1 %>% 
  filter(Nb=="2002",
         Parcel_Area<363|
         Parcel_Area>365)

B_parcels_5003_edit<-B_parcels_1 %>% 
  filter(Nb=="5003",
         Parcel_Area>400)

B_parcels_others<-B_parcels_1 %>% 
  filter(Nb!="2002",
         Nb!="5003")

B_parcels<-B_parcels_others %>% 
  full_join(B_parcels_2002_edit) %>% 
  full_join(B_parcels_5003_edit)

write.csv(B_parcels, file="B_parcels.csv", row.names=F)

BAL_Surveys<-BAL_AllSurveys_raw %>% 
  mutate(City="Baltimore",
         House_ID=as.character(House_ID),
         Nb=as.character(Nb))%>% 
  left_join(B_parcels) %>% 
  select(City, House_ID, Nb, Home_Value, Parcel_Area, A1, A201, A202, A203, A204, A205, A206, A207, A301, A302, A303, A4, A5, A6, A701, A702, A703, A704, A705, A706, A707, A708, A709, A710, B101, B102, B103, B104, B105, B106, B107, B108, B201, B202, B203, B204, B205, B206, B207, B301, B302, B401, B402, C5, C601)

write.csv(BAL_Surveys, file="BAL_Surveys.csv", row.names = F)

B_key_responded_191<-BAL_Surveys %>% 
  left_join(B_key_mailed_1235)%>% 
  select(ACCTID, House_ID, YEARBLT) %>% 
  unique()

write.csv(B_key_responded_191, file="B_key_responded_191.csv", row.names = F)

B_houseinfo<-B_key_responded_191 %>% 
  left_join(B_parcels) %>% 
  left_join(B_nbinfo) %>% 
  mutate(City="Baltimore") %>% 
  select(City, Nb, House_ID, Nb, Med_Inc, Parcel_Area, YEARBLT, Home_Value, Style, med_age)

B_nbinfo2<-B_houseinfo%>% 
  group_by(City, Nb, Style, med_age, Med_Inc) %>% 
  summarize(area_min=min(Parcel_Area),
            area_max=max(Parcel_Area),
            area_med=median(Parcel_Area),
            year_min=min(YEARBLT),
            year_max=max(YEARBLT),
            year_med=median(YEARBLT),
            Homevalue_min=min(Home_Value),
            Homevalue_max=max(Home_Value),
            Homevalue_med=median(Home_Value),
            num_surveys=length(Nb))


All_Diversity_House_Balt<-read.csv("BAL_All_Diversity_House_200407.csv") %>%
  mutate(Nb=as.character(Nb),
         Nb=recode(Nb, "34"="0034"),
         House=as.character(House),
         House=recode(House, "1"="01"),
         House=recode(House, "2"="02"),
         House=recode(House, "3"="03"),
         House=recode(House, "4"="04"),
         House=recode(House, "5"="05"),
         House=recode(House, "6"="06"),
         House=recode(House, "7"="07"),
         House=recode(House, "8"="08"),
         House=recode(House, "9"="09"),
         House_ID=paste(Nb, House, sep="_"))

write.csv(All_Diversity_House_Balt, file="All_Diversity_House_Balt.csv", row.names = F)

B_key_visited_96<-All_Diversity_House_Balt %>% 
  left_join(B_key_mailed_1235) %>% 
  select(House_ID, ACCTID, YEARBLT) %>% 
  unique()
  
write.csv(B_key_visited_96, file="B_key_visited_96.csv", row.names = F)

B_parc_191<-B_key_responded_191 %>%
  left_join(B_parcels) %>% 
  unique()

B_parc_096<-B_key_visited_96 %>%
  left_join(B_parcels) %>% 
  unique

BAL_Direct<-All_Diversity_House_Balt %>% 
  mutate(Nb=as.character(Nb)) %>% 
  left_join(BAL_Surveys) %>% 
  left_join(B_nbinfo) %>% 
  mutate(Flower_Color=as.numeric(as.character(A702)),
         Biodiversity=as.numeric(as.character(A6)))

write.csv(BAL_Direct, file="BAL_Direct.csv", row.names = F)


#Salt Lake data read in and merging
SLC_AllSurveys_raw<-read.csv("SLC_2014 Homeowner_Survey Data_Final_homeowner ID intact_200407.csv") %>% 
  select(PARCEL_ID, A1, A201, A202, A203, A204, A205, A206, A207, A208, A301, A302, A303, A6, A7, A8, A901, A902, A903, A904, A905, A906, A907, A908, A909, A910, B101, B102, B103, B104, B105, B106, B107, B108, B201, B202, B203, B204, B205, B206, B207, B301, B302, B401, B402, D1, D2, D301, D4, D601A, D601B)

S_nbinfo<-read.csv("SLC_NB_details_200407.csv") %>% 
  select(-X, -Type) %>% 
  mutate(Nb=as.character(Nb))

S_parcels<-read.csv("SLC_Parcel_Info_200407.csv")%>% 
  mutate(Nb=as.character(NB_ID)) %>% 
  filter(Responded!="REPEAT") %>% 
  mutate(YEARBLT=Yr_Built,
         Parcel_Area_acre=Parcel_Area) %>% 
  select(Nb, House_ID, PARCEL_ID, Home_Value, Parcel_Area_acre, YEARBLT, -Parcel_Area) %>% 
  separate(House_ID, c("Nb1","Block","House")) %>% 
  mutate(House_ID=paste(Nb1, Block, House, sep = "_"),
         House_ID=as.character(House_ID),
         Parcel_Area=Parcel_Area_acre*4046.86) %>% 
  select(-Nb1) %>% 
  replace_with_na(replace = list(House_ID = "_NA_NA"))

CC_parcels<-S_parcels %>% 
  mutate(City="Salt Lake City") %>% 
  full_join(B_parcels) %>% 
  select(City, House_ID, PARCEL_ID, Home_Value, Parcel_Area)

SLC_Surveys<-merge(S_parcels, SLC_AllSurveys_raw, by="PARCEL_ID", all=F) %>% 
  mutate(City="Salt Lake City",
         Nb=as.character(Nb))

write.csv(SLC_Surveys, file="SLC_Surveys.csv", row.names = F)

S_key_responded_277<-SLC_Surveys %>% 
  select(PARCEL_ID, House_ID)

S_houseinfo<-S_key_responded_277 %>% 
  left_join(S_parcels) %>% 
  left_join(S_nbinfo) %>% 
  mutate(City="Salt Lake City") %>% 
  select(City, Nb, PARCEL_ID, House_ID, Nb, Med_Inc, Parcel_Area, YEARBLT, Home_Value)

S_nbinfo2<-S_houseinfo %>% 
  group_by(City, Nb, Med_Inc) %>% 
  summarize(area_min=min(Parcel_Area),
            area_max=max(Parcel_Area),
            area_med=median(Parcel_Area),
            year_min=min(YEARBLT),
            year_max=max(YEARBLT),
            year_med=median(YEARBLT),
            Homevalue_min=min(Home_Value),
            Homevalue_max=max(Home_Value),
            Homevalue_med=median(Home_Value),
            num_surveys=length(Nb))

CC_houseinfo<-S_houseinfo %>% 
  full_join(B_houseinfo)

CC_nbinfo2<-S_nbinfo2 %>% 
  full_join(B_nbinfo2)

write.csv(CC_houseinfo, file="CC_houseinfo.csv", row.names = F)

write.csv(CC_nbinfo2, file="CC_nbinfo2.csv", row.names = F)

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

write.csv(S_Prefs_avg, file="S_Prefs_avg.csv", row.names = F)

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

write.csv(B_Prefs_avg, file="B_Prefs_avg.csv", row.names=F)

CC_Prefs_avg<-B_Prefs_avg %>% 
  full_join(S_Prefs_avg)

write.csv(CC_Prefs_avg, file = "CC_Prefs_avg.csv", row.names = F)

#Preferences by home value data set up (can add in parcel_area here)
S_Prefs_parcel<-melt(SLC_Surveys, id=c("PARCEL_ID","House_ID","Home_Value","Parcel_Area","Nb", "Block", "House", "City"))%>%
  mutate(value2=(as.numeric(as.character(value))),
         Nb=as.character(Nb))%>%
  left_join(S_nbinfo) %>% 
  select(City, PARCEL_ID, Nb, Med_Inc, nb_inc, House_ID, Home_Value, Parcel_Area, Block, House, variable, value2) %>% 
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

write.csv(S_Prefs_parcel, file="S_Prefs_parcel.csv", row.names=F)

B_Prefs_parcel<-melt(BAL_Surveys, id=c("House_ID","Nb", "City", "Home_Value","Parcel_Area"))%>%
  mutate(value2=(as.numeric(as.character(value))))%>%
  left_join(B_nbinfo) %>%
  select(City, Nb, Med_Inc, nb_inc, House_ID, Home_Value, Parcel_Area, variable, value2) %>%
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

write.csv(B_Prefs_parcel, file="B_Prefs_parcel.csv", row.names = F)

CC_Prefs_parcel<-S_Prefs_parcel %>%
  full_join(B_Prefs_parcel)

write.csv(CC_Prefs_parcel, file="CC_Prefs_parcel.csv", row.names = F)

#Preferences by nb median income data set up
S_Prefs_inc<-S_Prefs_parcel%>% 
  group_by(City, Med_Inc, nb_inc, variable)%>%
  summarize(Importance=mean(value2, na.rm=T),
            sd=sd(as.numeric(value2), na.rm=T),
            n=length(value2[!is.na(value2)]))%>%
  mutate(se=sd/sqrt(n))

write.csv(S_Prefs_inc, file="S_Prefs_inc.csv", row.names = F)

B_Prefs_inc<-B_Prefs_parcel %>% 
  group_by(City, Med_Inc, nb_inc, variable)%>%
  summarize(Importance=mean(value2, na.rm=T),
            sd=sd(as.numeric(value2), na.rm=T),
            n=length(value2[!is.na(value2)]))%>%
  mutate(se=sd/sqrt(n))

write.csv(B_Prefs_inc, file="B_Prefs_inc.csv", row.names = F)

CC_Prefs_inc<-B_Prefs_inc %>% 
  full_join(S_Prefs_inc)

write.csv(CC_Prefs_inc, file="CC_Prefs_inc.csv", row.names = F)


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
  summarize(Lawn_rich=richcount(Species))

S_Trees_richabund<-SLC_Trees_raw %>% 
  mutate(Species=ifelse(Species=="",0, as.character(Species)),
         House_ID=as.character(House_ID),
         Nb=as.character(Nb)))%>%
  group_by(City, Nb, House_ID) %>% 
  summarize(Tree_rich=richcount(Species),
            Tree_abund=abundcount(Species)) %>% 
  left_join(S_nbinfo) %>% 
  select(City, Nb, House_ID, Med_Inc,Tree_rich, Tree_abund)

S_Flowers_richabund<-S_F_data1 %>% 
  mutate(FaGe=ifelse(FaGe=="NoFlowers_NoFlowers",0, as.character(FaGe)),
         House_ID=as.character(House_ID),
         Nb=as.character(Nb)) %>% 
  group_by(City, Nb, House_ID) %>% 
  summarize(Flower_rich=richcount(FaGe),
            Flower_abund=sum(num_plants)) %>% 
  select(City, Nb, House_ID, Flower_rich, Flower_abund)

S_rich_abund<-merge(S_Lawns_rich, S_Trees_richabund) %>% 
  full_join(S_Flowers_richabund)

#Both cities data prep/merge for richness/abundance (update for Home value)
CC_rich_abund<-B_rich_abund %>% 
  full_join(S_rich_abund) %>% 
  left_join(CC_parcels)

write.csv(CC_rich_abund, "CC_rich_abund.csv", row.names = F)

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

write.csv(CC_ra_MedInc, file="CC_ra_MedInc.csv", row.names = F)


#B3: Have you ever removed a tree? Why?
CC_B3<-SLC_Surveys %>%
  select(City, Nb, House_ID, PARCEL_ID, B301, B302) %>% 
  mutate(Nb=as.character(Nb)) %>% 
  full_join(BAL_Surveys) %>% 
  select(City, Nb, House_ID, PARCEL_ID, B301, B302)

#write.csv(CC_B3, file="CC_B3.csv", row.names = F)

CC_B3_nec_raw<-read.csv("CC_B3_nec_raw.csv")

S_B3_raw<-CC_B3_nec_raw %>% 
  filter(City=="Salt Lake City", B301=="1") %>% 
  mutate(Nb=as.character(Nb)) %>% 
  left_join(S_nbinfo) %>% 
  group_by(City, Nb, Med_Inc, B302_necessity) %>% 
  summarize(num_resp=length(B302_necessity),
            num_surveys=length(unique(PARCEL_ID)))

S_B3_survs<-S_B3_raw %>% 
  group_by(City, Nb, Med_Inc) %>% 
  summarize(total_surveys=sum(num_surveys))

S_B3_nec<-S_B3_raw %>% 
  select(-num_surveys) %>% 
  spread(B302_necessity, num_resp, fill=0) %>% 
  left_join(S_B3_survs)%>% 
  mutate(per.Nec=(Necessary/total_surveys)*100,
         per.Unn=(Unnecessary/total_surveys)*100)

B_B3_raw<-CC_B3_nec_raw %>% 
  filter(City=="Baltimore", B301=="1") %>% 
  mutate(Nb=as.character(Nb),
         Nb=recode(Nb, "34"="0034")) %>% 
  left_join(B_nbinfo) %>% 
  group_by(City, Nb, Med_Inc, B302_necessity) %>% 
  summarize(num_resp=length(B302_necessity),
            num_surveys=length(unique(House_ID)))

B_B3_survs<-B_B3_raw %>% 
  group_by(City, Nb, Med_Inc) %>% 
  summarize(total_surveys=sum(num_surveys))

B_B3_nec<-B_B3_raw %>% 
  select(-num_surveys) %>% 
  spread(B302_necessity, num_resp, fill=0) %>% 
  left_join(B_B3_survs) %>% 
  mutate(per.Nec=(Necessary/total_surveys)*100,
         per.Unn=(Unnecessary/total_surveys)*100)

CC_B3_nec<-B_B3_nec %>% 
  full_join(S_B3_nec)

write.csv(CC_B3_nec, file="CC_B3_nec.csv", row.names = F)

#Biodiversity data
#Flower data frames
B_F_data1<-read.csv("BAL_Floral_Data_Balt18_clean_AB_200410.csv") %>% 
  mutate(num_plants=X..F.plants,
         House_ID.rep=House_ID,
         City="Baltimore") %>% 
  select(-Nb, -House) %>% 
  separate(House_ID.rep, c("Nb","House"))

write.csv(B_F_data1, "B_F_data1.csv", row.names=F)

B_Fdata2<-B_F_data1%>% 
  filter(Flower.Width..cm.!="", Genus!="NoFlowers")%>%
  mutate(num_flowers=X.F.stems*ave_flower_perstem,
         area=as.numeric(as.character(Flower.Width..cm.))*as.numeric(as.character(Flower.Length..cm.))*num_flowers)%>%
  group_by(City, House_ID, Genus, Front.Back)%>%
  summarize(nplants=sum(num_plants),
            nflowers=sum(num_flowers),
            areatot=sum(area))

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

write.csv(S_F_data1, file="S_F_data1.csv", row.names = F)

S_Fdata2<-S_F_data1%>% 
  filter(Genus!="NoFlowers")%>%
  group_by(City, House_ID, Genus, Front.Back)%>%
  summarize(nplants=sum(num_plants),
            nflowers=sum(num_flowers),
            areatot=sum(TotalFlower_area))

#Lawn data frames
BAL_Lawns_raw<-read.csv("BAL_Lawn Quadrats_Balt18_AB_200505.csv") %>% 
  mutate(City="Baltimore",
         House_ID=paste(NB, House, sep="_"),
         Species=Species.combined,
         Nb=as.character(NB),
         Nb=recode(Nb, "34"="0034"),
         House=as.character(House),
         House=recode(House, "1"="01"),
         House=recode(House, "2"="02"),
         House=recode(House, "3"="03"),
         House=recode(House, "4"="04"),
         House=recode(House, "5"="05"),
         House=recode(House, "6"="06"),
         House=recode(House, "7"="07"),
         House=recode(House, "8"="08"),
         House=recode(House, "9"="09"),
         House_ID=paste(Nb, House, sep="_")) %>% 
  select(-NB)

write.csv(BAL_Lawns_raw, "BAL_Lawns_raw.csv", row.names = F)

B_lawn2<-BAL_Lawns_raw%>%
  select(City, Nb, House, House_ID, Species, F1, F2, B1, B2)%>%
  unique()%>%
  filter(Species!="Dead grass"&Species!="No Lawn")

B_lawn2$Cover<-rowMeans(B_lawn2[c("F1", "F2","B1","B2")], na.rm = T)

SLC_Lawns_raw<-read.csv("SLC_Lawns2014_200407.csv")%>%
  filter(House_ID!="387_11_4",
         House_ID!="651_1_4") %>% 
  mutate(City="Salt Lake City",
         Nb=as.character(Nb))

write.csv(SLC_Lawns_raw, "SLC_Lawns_raw.csv", row.names=F)

S_lawn2<-SLC_Lawns_raw%>%
  select(City, Nb, House_ID, Species, F1, F2, B1, B2)%>%
  unique()%>%
  filter(Species!="Dead grass",
         Species!="No Lawn",
         Species!="NO LAWN",
         Species!="Not collected")

S_lawn2$Cover<-rowMeans(S_lawn2[c("F1", "F2","B1","B2")], na.rm = T)

#Tree data frames
BAL_Trees_raw<-read.csv("BAL_Trees_Balt18_FB_clean_200413.csv") %>% 
  mutate(Nb=as.character(NB),
         Nb=recode(Nb, "34"="0034"),
         House=as.character(House),
         House=recode(House, "1"="01"),
         House=recode(House, "2"="02"),
         House=recode(House, "3"="03"),
         House=recode(House, "4"="04"),
         House=recode(House, "5"="05"),
         House=recode(House, "6"="06"),
         House=recode(House, "7"="07"),
         House=recode(House, "8"="08"),
         House=recode(House, "9"="09"),
         Tree.species=recode(Tree.species, "Cupressus × leylandii cypress"="Cupressocyparis × leylandii"),
         House_ID=paste(Nb,House, sep="_"),
         Species=Tree.species,
         City="Baltimore") %>% 
  select(-NB)

write.csv(BAL_Trees_raw, "BAL_Trees_raw.csv", row.names = F)

SLC_Trees_raw<-read.csv("SLC_ResTrees_200403.csv") %>% 
  mutate(City="Salt Lake City",
         Nb=as.character(Nb),
         Species=recode(Species, "Cotinus coggyria"="Cotinus coggygria"),
         Species=recode(Species, "Juniper sp."="Juniperus sp."),
         Species=recode(Species, "Juniperus scoparium"="Juniperus scopulorum"),
         Species=recode(Species, "Magnolia x soulangiana"="Magnolia x soulangeana"),
         Species=recode(Species, "Malus hybrid"="Malus hybrid FLOW"),
         Species=recode(Species, "Populus tremula"="Populus tremula erecta"),
         Species=recode(Species, "Prunus hybrid"="Prunus hybrid WEEP"),
         Species=recode(Species, "Quercus robur"="Quercus robur fastigiata"),
         Species=recode(Species, "Salix caprea"="Salix caprea pendula"),
         Species=recode(Species, "Ulmus hybrid"="Ulmus hybrid frontier"),
         Species=recode(Species, "Wisteria senensis"="Wisteria sinensis"),
         Species=recode(Species, "Laburnum anagyroides"="Laburnum x watereri"),
         Species=recode(Species, "Prunus salicina"="Prunus salicina satsuma"))

write.csv(SLC_Trees_raw, "SLC_Trees_raw.csv", row.names=F)


#native status key
#note- several flower genera have varied nativeness based on species
S_flow_nat<-S_F_data1 %>% 
  mutate(Species=Genus,
         Native_UT=Native_bin) %>% 
  select(City, Species, Native_UT) %>% 
  unique()

S_lawn_nat<-SLC_Lawns_raw %>% 
  mutate(Native_UT=Native_man) %>% 
  select(City, Species, Native_UT) %>% 
  unique()

S_tree_nat<-read.csv("SLC_Traits_PresentSpeciesOnly.csv") %>% 
  mutate(Native_UT=Native,
         City="Salt Lake City") %>% 
  select(City, Species, Native_UT)

SLC_natives<-rbind(S_flow_nat, S_lawn_nat, S_tree_nat)

B_flow_nat<-read.csv("BAL_Flower_natives_all_200505.csv") %>% 
  mutate(City="Baltimore",
         Species=Genus,
         type="floral") %>% 
  select(City, Species, Native_MD, type)

B_lawn_nat<-BAL_Lawns_raw %>% 
  mutate(Native_MD=Native,
         type="lawn") %>% 
  select(City, Species, Native_MD, type) %>% 
  unique()

B_tree_nat<-read.csv("Tree codes_200505.csv") %>%
  mutate(Species=genus.species,
         Native_MD=native_MD,
         City="Baltimore",
         type="tree") %>% 
  select(City, Species, Native_MD, type)
  
BAL_natives<-rbind(B_flow_nat, B_lawn_nat, B_tree_nat)

#Rank Abundance Curves
#RAC lawn
#note- did lawns lsightly different than Meghan's code b/c hers averaged the 0s from "no-lawn"s in back or front (ex. 7006_93 had no back yard and 4.0 cover of D.sang in front and so 0.0 in back...but then average D.sang cover turned into 2.0...what I did below keeps D.sang at 4.0)
B_lawnrank<-B_lawn2 %>% 
  group_by(City, Nb, House, House_ID, Species)%>%
  summarize(meancov=mean(Cover)) %>% 
  ungroup()%>%
  group_by(City, Species)%>%
  summarize(abund=sum(meancov), freq=length(Species))%>%
  mutate(Frank=rank(-freq), 
         Arank=rank(-abund), 
         type="lawn")

S_lawnrank<-S_lawn2 %>% 
  group_by(City, Nb, House_ID, Species)%>%
  summarize(meancov=mean(Cover)) %>% 
  ungroup()%>%
  group_by(City, Species)%>%
  summarize(abund=sum(meancov), freq=length(Species))%>%
  mutate(Frank=rank(-freq), 
         Arank=rank(-abund), 
         type="lawn")

#RAC Trees
B_treerank<-BAL_Trees_raw%>%
  filter(Species!="no trees")%>%
  group_by(City, Nb, House, House_ID, Species)%>%
  summarize(abund=length(DBH1))%>%
  ungroup()%>%
  group_by(City, Species)%>%
  summarize(abund=sum(abund), freq=length(Species))%>%
  mutate(Frank=rank(-freq), Arank=rank(-abund), type="tree")

S_treerank<-SLC_Trees_raw %>% 
  filter(Front_Back!="No Tree")%>%
  group_by(City, Nb, House_ID, Species)%>%
  summarize(abund=length(DBH1))%>%
  ungroup()%>%
  group_by(City, Species)%>%
  summarize(abund=sum(abund), freq=length(Species))%>%
  mutate(Frank=rank(-freq), Arank=rank(-abund), type="tree")

#RAC flowers
B_floralrank<-B_Fdata2%>%
  group_by(City, House_ID, Genus)%>%
  summarize(totplants=sum(nplants))%>%
  ungroup%>%
  group_by(City, Genus)%>%
  summarize(abund=sum(totplants), freq=length(Genus))%>%
  mutate(Frank=rank(-freq), Arank=rank(-abund), type="floral")%>%
  mutate(Species=Genus) %>% 
  select(-Genus)

S_floralrank<-S_Fdata2%>%
  group_by(City, House_ID, Genus)%>%
  summarize(totplants=sum(nplants))%>%
  ungroup%>%
  group_by(City, Genus)%>%
  summarize(abund=sum(totplants), freq=length(Genus))%>%
  mutate(Frank=rank(-freq), Arank=rank(-abund), type="floral")%>%
  mutate(Species=Genus) %>% 
  select(-Genus)

CC_ranks<-rbind(S_floralrank, S_treerank, S_lawnrank, B_floralrank, B_treerank, B_lawnrank) %>% 
  left_join(SLC_natives) %>% 
  left_join(BAL_natives)
write.csv(CC_ranks, "CC_ranks.csv", row.names = F)

S_ranks<-rbind(S_floralrank, S_treerank, S_lawnrank) %>% 
  left_join(SLC_natives)
write.csv(S_ranks, "S_ranks.csv", row.names = F)

B_ranks<-rbind(B_floralrank, B_treerank, B_lawnrank) %>% 
  left_join(BAL_natives)
write.csv(B_ranks, "B_ranks.csv", row.names = F)


#Tree Traits (TT)
#Wrangling the trait codes from the SLC-only csv and from the BAL csv that has some overlapping tree species with SLC that updated (and so no longer duplicates)
Sp_BS.p_TC<-read.csv("Tree Codes_200507.csv") %>% 
  mutate(Species=genus.species,
         Status="AB",
         Status=as.character(Status)) %>% 
  select(Species, Status)#marking all the species that AB updated

Sp_S_TC1<-read.csv("SLC_Traits_PresentSpeciesOnly.csv") %>% 
  select(Species) %>% 
  left_join(Sp_BS.p_TC)#marking the species that AB updated that overlap with SLC

Sp_S_TC1$Status[is.na(Sp_S_TC1$Status)]<-0#marking non-marked (so non-updated) species as "0" so that the filter in next df will work properly

TC_S_all<-read.csv("SLC_Traits_PresentSpeciesOnly.csv") %>% 
  mutate(shading_potential=as.numeric(as.character(shading_potential)),
         Family=Family.Name,
         fall_color=Fall.Color,
         flower_showy=Flowers,
         fruit_edible=Fruit,
         water=sunset_water) %>% 
  select(Species, Family, Continent, decid, allergen, aroma, fall_color, flower_showy, fruit_edible, Growth, Height, Longevity, litter, poison, root, shading_potential,water)#reading in SLC tree trait code for all trees in SLC (even ones that were later updated)
#don't have flower_phenology, fruit_contrast, or fruit_showy data for species only found in SLC. Removed native_UT b/c it was getting complicated.
  
Sp_S_TC2<-Sp_S_TC1%>% 
  filter(Status!="AB") %>% 
  left_join(TC_S_all)#applying tree trait codes for species that were never updated by AB (only)

TreeCodes<-read.csv("Tree Codes_200507.csv") %>% 
  mutate(Species=genus.species,
         shading_potential=as.numeric(as.character(shading_potential)),
         Growth=growth_rt.inches.per.year.,
         Height=Height.ft.,
         Longevity=Longevity.years.) %>% 
  select(-genus.species, -Notes, -pests, -Alternative.names, -On.CalPoly, -flower_phenology, -fruit_contrast, -fruit_showy, -native_MD, -growth_rt.inches.per.year., -Height.ft., -Longevity.years.) %>% 
  full_join(Sp_S_TC2) %>% 
  select(-Status)#reading in tree trait codes that AB did (for BAL and SLC overlap species) then merging it with the SLC-only tree species and their codes

CC_TT_prep<-BAL_Trees_raw %>% 
  mutate(Front_Back=Front.Back) %>% 
  select(-Front.Back) %>% 
  full_join(SLC_Trees_raw) %>% 
  filter(Species!="",Species!="no trees") %>% 
  select(City,Nb, House_ID, Front_Back, Species) %>% 
  mutate(Front_Back=recode(Front_Back, "f"="F"),
         Front_Back=recode(Front_Back, "b"="B"),
         Front_Back=recode(Front_Back, "S"="B"))

#write.csv(CC_TT_prep, file="CC_TT_prep.csv", row.names = F)
  
CC_TT_char<-CC_TT_prep %>% 
  left_join(TreeCodes) %>% 
  select(City, Nb, House_ID, Front_Back, Species, Family, Continent, decid)

#write.csv(CC_TT_char, file="CC_TT_char.csv", row.names = F)

#CC_TT_nb
CC_TT_nb<-CC_TT_FB1 %>% 
  group_by(City, Nb, Front_Back, trait) %>% 
  summarise(mean_score=mean(score_upd, na.rm=T),
            n=length(score_upd[!is.na(score_upd)]),
            sd=sd(score_upd, na.rm=T)) %>% 
  left_join(CC_nbinfo)

write.csv(CC_TT_nb, file="CC_TT_nb.csv", row.names = F)

CC_TT_FB1<-CC_TT_prep %>% 
  filter(Front_Back!="") %>% 
  left_join(TreeCodes) %>% 
  select(-Family, -Continent, -decid) %>% 
  gather(trait, score, -City, -Nb, -House_ID, -Front_Back, -Species) %>% 
  mutate(scale=trait,
         scale=recode(scale, "allergen"="3"),
         scale=recode(scale,"water"="3"),
         scale=recode(scale,"Growth"="3"),
         scale=recode(scale,"Height"="3"),
         scale=recode(scale,"Longevity"="3"),
         scale=recode(scale,"root"="3"),
         scale=recode(scale,"shading_potential"="3"),
         scale=recode(scale,"flower_showy"="2"),
         scale=recode(scale,"aroma"="1"),
         scale=recode(scale,"fall_color"="1"),
         scale=recode(scale,"fruit_edible"="1"),
         scale=recode(scale,"litter"="1"),
         scale=recode(scale,"poison"="1"),
         score_upd=as.numeric(as.character(score))/as.numeric(as.character(scale))) %>% 
  select(-score)

CC_TT_FB2<-CC_TT_FB1 %>% 
  group_by(City, Front_Back, trait) %>% 
  summarise(mean_score=mean(score_upd, na.rm=T),
            n=length(score_upd[!is.na(score_upd)]),
            sd=sd(score_upd, na.rm=T)) %>% 
  mutate(se=sd/sqrt(n),
         trait=recode(trait, "allergen"="Allergen"),
         trait=recode(trait, "aroma"="Aroma"),
         trait=recode(trait, "fall_color"="Fall Color"),
         trait=recode(trait, "flower_phenology"="Bloom Time"),
         trait=recode(trait, "flower_showy"="Showy Flowers"),
         trait=recode(trait, "fruit_contrast"="Fruit Contrast"),
         trait=recode(trait, "fruit_edible"="Edible Fruit"),
         trait=recode(trait, "fruit_showy"="Showy Fruit"),
         trait=recode(trait, "litter"="Litter"),
         trait=recode(trait, "poison"="Poisonous"),
         trait=recode(trait, "root"="Root Damage Potential"),
         trait=recode(trait, "shading_potential"="Shade"),
         trait=recode(trait, "water"="Water Req."),
         City_Loc=paste(City, Front_Back, sep="_"))

write.csv(CC_TT_FB1, file= "CC_TT_FB1.csv", row.names = F)
write.csv(CC_TT_FB2, file= "CC_TT_FB2.csv", row.names = F)

CC_TT_inc<-CC_TT_prep %>% 
  left_join(TreeCodes) %>% 
  select(-Family, -Continent, -decid, -Front_Back, -House_ID) %>% 
  gather(trait, score, -City, -Nb, -Species) %>% 
  mutate(scale=trait,
         scale=recode(scale, "allergen"="3"),
         scale=recode(scale,"water"="3"),
         scale=recode(scale,"Growth"="3"),
         scale=recode(scale,"Height"="3"),
         scale=recode(scale,"Longevity"="3"),
         scale=recode(scale,"root"="3"),
         scale=recode(scale,"shading_potential"="3"),
         scale=recode(scale,"flower_showy"="2"),
         scale=recode(scale,"aroma"="1"),
         scale=recode(scale,"fall_color"="1"),
         scale=recode(scale,"fruit_edible"="1"),
         scale=recode(scale,"litter"="1"),
         scale=recode(scale,"poison"="1"),
         score_upd=as.numeric(as.character(score))/as.numeric(as.character(scale))) %>% 
  select(-score) %>%
  group_by(City, Nb, trait) %>% 
  summarise(mean_score=mean(score_upd, na.rm=T),
            n=length(score_upd[!is.na(score_upd)]),
            sd=sd(score_upd, na.rm=T)) %>% 
  mutate(se=sd/sqrt(n),
         trait=recode(trait, "allergen"="Allergen"),
         trait=recode(trait, "aroma"="Aroma"),
         trait=recode(trait, "fall_color"="Fall Color"),
         trait=recode(trait, "flower_phenology"="Bloom Time"),
         trait=recode(trait, "flower_showy"="Showy Flowers"),
         trait=recode(trait, "fruit_contrast"="Fruit Contrast"),
         trait=recode(trait, "fruit_edible"="Edible Fruit"),
         trait=recode(trait, "fruit_showy"="Showy Fruit"),
         trait=recode(trait, "growth_rt.inches.per.year."="Growth Rate"),
         trait=recode(trait, "Height.ft."="Height"),
         trait=recode(trait, "litter"="Litter"),
         trait=recode(trait, "Longevity.years."="Longevity"),
         trait=recode(trait, "poison"="Poisonous"),
         trait=recode(trait, "root"="Root Damage Potential"),
         trait=recode(trait, "shading_potential"="Shade"),
         trait=recode(trait, "water"="Water Req.")) %>% 
  left_join(CC_nbinfo)

write.csv(CC_TT_inc, file= "CC_TT_inc.csv", row.names = F)

CC_TT_avg<-CC_TT_prep %>% 
  left_join(TreeCodes) %>% 
  select(-Family, -Continent, -decid, -Front_Back, -House_ID, -Nb) %>% 
  gather(trait, score, -City, -Species) %>%
  mutate(scale=trait,
         scale=recode(scale, "allergen"="3"),
         scale=recode(scale,"water"="3"),
         scale=recode(scale,"Growth"="3"),
         scale=recode(scale,"Height"="3"),
         scale=recode(scale,"Longevity"="3"),
         scale=recode(scale,"root"="3"),
         scale=recode(scale,"shading_potential"="3"),
         scale=recode(scale,"flower_showy"="2"),
         scale=recode(scale,"aroma"="1"),
         scale=recode(scale,"fall_color"="1"),
         scale=recode(scale,"fruit_edible"="1"),
         scale=recode(scale,"litter"="1"),
         scale=recode(scale,"poison"="1"),
         score_upd=as.numeric(as.character(score))/as.numeric(as.character(scale))) %>% 
  select(-score) %>%
  group_by(City, trait) %>% 
  summarise(mean_score=mean(score_upd, na.rm=T),
            n=length(score_upd[!is.na(score_upd)]),
            sd=sd(score_upd, na.rm=T)) %>% 
  mutate(se=sd/sqrt(n),
         trait=recode(trait, "allergen"="Allergen"),
         trait=recode(trait, "aroma"="Aroma"),
         trait=recode(trait, "fall_color"="Fall Color"),
         trait=recode(trait, "flower_showy"="Showy Flowers"),
         trait=recode(trait, "fruit_edible"="Edible Fruit"),
         trait=recode(trait, "Height.ft."="Height"),
         trait=recode(trait, "litter"="Litter"),
         trait=recode(trait, "Longevity.years."="Longevity"),
         trait=recode(trait, "poison"="Poisonous"),
         trait=recode(trait, "root"="Root Damage"),
         trait=recode(trait, "shading_potential"="Shade"),
         trait=recode(trait, "water"="Water Req."))

write.csv(CC_TT_avg, file= "CC_TT_avg.csv", row.names = F)
