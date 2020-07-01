library(dplyr)
library(tidyverse)

#Allie's laptop
setwd("C:\\Users\\Allie\\Dropbox\\For Allie SLC\\prelim stuff\\")

#A1 Satisfaction per house - NO not normally distributed
ggplot(subset(S_Prefs_parcel, variable%in%c("Satisfaction")), aes(x= value2))+
  geom_density()+
  geom_vline(aes(xintercept=mean(value2, na.rm=T)),
             color="blue", linetype="dashed", size=1)+
  ggtitle("Satisfaction answers, N = ~278 houses")

shapiro.test(S_Prefs_parcel$value2[S_Prefs_parcel$variable=="Satisfaction"])

#A1 Satisfaction per neighborhood (income)- YES normally distributed
ggplot(subset(S_Prefs_inc, variable%in%c("Satisfaction")), aes(x= Importance))+
  geom_density()+
  geom_vline(aes(xintercept=mean(Importance, na.rm=T)),
             color="blue", linetype="dashed", size=1)+
  ggtitle("Satisfaction answers, N = 9 nbs")

shapiro.test(S_Prefs_inc$Importance[S_Prefs_inc$variable=="Satisfaction"])
