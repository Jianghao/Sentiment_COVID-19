rm(list=ls())
setwd("~/Sentiment_COVID-19/")
library (readr)
library(readxl)
library(dplyr)
library(data.table)
library(tidyverse)
library(cowplot)
library(ggridges)
library(ggthemes)
library(hrbrthemes)
library(scales)
library(magrittr)
library(stringr)
library(lfe)
library(ggplot2)

Sys.setlocale(category = "LC_ALL", locale = "english")
devtools::source_gist("https://gist.github.com/Jianghao/d806dcc220a49df21024e0516f102b2f")

i<-"bert"

# 1. import results and other data --------------------------------------
# cases
global_country_cases<-read.csv("input/heter/global_country_case.csv")
global_country_cases$iso3<-as.character(global_country_cases$iso3)
# get the case number at the min date
  # STL:start and end dates
stl_dta<-read.csv(paste0("input/heter/country_info_Drop_Min.csv"),stringsAsFactors = F)%>%
  dplyr::select(Nation,paste0("drop_senti_",i),paste0("min_senti_",i),paste0("senti_",i,"_averageN"),
         paste0("drop_senti_",i,"_quan25"),paste0("min_senti_",i,"_quan25"),
         paste0("drop_senti_",i,"_quan75"),paste0("min_senti_",i,"_quan75"))%>%
  dplyr::rename(iso3=Nation,tweet_av=paste0("senti_",i,"_averageN"),
                DropDate=paste0("drop_senti_",i), MinDate=paste0("min_senti_",i))%>%
  filter(as.Date(MinDate,format="%Y-%m-%d")>as.Date(DropDate,format="%Y-%m-%d"))

stl_dta$tweet_av<-as.numeric(stl_dta$tweet_av)
stl_dta<-subset(stl_dta,!is.na(tweet_av))
stl_dta$iso3<-as.character(stl_dta$iso3)

dta_case<-global_country_cases%>%
  dplyr::select(iso3,date,cum_confirm,confirm)%>%
  left_join(stl_dta,by="iso3")%>%
  dplyr::filter(as.Date(date,format="%Y/%m/%d")==as.Date(MinDate,format="%Y-%m-%d"))

# culture tightness; individualism; relation mobility; religious
# source:Awad, Edmond, Sohan Dsouza, Azim Shariff, Iyad Rahwan, and Jean-Fran??ois Bonnefon. 2020. Universals and Variations in Moral Decisions Made in 42 Countries by 70,000 Participants.?? Proceedings of the National Academy of Sciences of the United States of America 117 (5): 2332?C37.
load("input/heter/countrylevelregressions.rdata") #ddl
ddl<-ddl%>%
  dplyr::rename(Two_Letter_Country_Code=two_letter_code)
ddl$Two_Letter_Country_Code<-as.character(ddl$Two_Letter_Country_Code)


# rd
dta_rd<-read.csv("input/heter/Final_Output_shock_covariates.csv",stringsAsFactors = F)
dta_rd<-dta_rd%>%
  mutate_at(vars(-iso3,-iso2,-country,-continent,-Group),as.numeric)
dta_rd$population<-as.numeric(dta_rd$population)/100000

#half-life
dta_halflife<-read.csv("input/heter/Final_Output_halflife.csv",stringsAsFactors = F)%>%
  select(HalfLife,sd_position_525,iso3,Quality)

dta_rd <- dta_rd[order(dta_rd$shock), ]
dta_rd<-subset(dta_rd,!(iso3=="TUR"& continent=="Europe") 
               & iso3!="HKG" &!(iso3=="KAZ"& continent=="Europe")
               & !(iso3=="CYP"& continent=="Asia")
               & !(iso3=="RUS"& continent=="Asia")
               & !(iso3=="AZE"& continent=="Asia"))
# rd data with min date cases
dta_rd$iso3<-as.character(dta_rd$iso3)
dta_rd<-dta_rd%>%
  left_join(dta_case,by="iso3")
dta_rd$l_cum_confirm<-log(dta_rd$cum_confirm+1)

dta_rd$l_income<-log(dta_rd$GDP_per_capita)
dta_rd$l_pop_density<-log(dta_rd$Population_Density)

# with culture related features
dta_rd$Two_Letter_Country_Code<-as.character(dta_rd$iso2)
dta_rd<-dta_rd%>%
  left_join(ddl,by="Two_Letter_Country_Code")

dta_rd <- dta_rd[order(dta_rd$shock), ]
dta_rd$iso3 <- factor(dta_rd$iso3, levels = dta_rd$iso3) 
dta_rd$country <- factor(dta_rd$country, levels = dta_rd$country) 
dta_rd<-subset(dta_rd, !is.na(shock)& iso3!="NA")

# high quality half-life
dta_complete<-dta_rd%>%
  left_join(dta_halflife,by="iso3")%>%
  filter(!is.na(HalfLife)& !is.na(iso3))%>%
  filter(sd_position_525>=-1)

# normalize shock
min_shock<-min(dta_rd$shock)
dta_rd$shock_norm<-((dta_rd$shock-min_shock)/(0-min_shock)-1)*-1
min_shock2<-min(dta_complete$shock)
dta_complete$shock_norm<-((dta_complete$shock-min_shock)/(0-min_shock)-1)*-1
max_halflife<-max(dta_complete$HalfLife)
dta_complete$halflife_norm<-(dta_complete$HalfLife-0)/(max_halflife-0)

dta_rd$shock_standard<- -1*dta_rd$shock_standard
dta_complete$shock_standard<- -1*dta_complete$shock_standard

# normalize covariates
dta_rd$cultural_norm<-(dta_rd$Cultural_tightness_and_looseness-min(dta_rd$Cultural_tightness_and_looseness,na.rm=T))/
  (max(dta_rd$Cultural_tightness_and_looseness,na.rm=T)-min(dta_rd$Cultural_tightness_and_looseness,na.rm=T))
dta_complete$cultural_norm<-(dta_complete$Cultural_tightness_and_looseness-min(dta_complete$Cultural_tightness_and_looseness,na.rm=T))/
  (max(dta_complete$Cultural_tightness_and_looseness,na.rm=T)-min(dta_complete$Cultural_tightness_and_looseness,na.rm=T))
dta_complete$RML_norm<-(dta_complete$RML-min(dta_complete$RML,na.rm=T))/
  (max(dta_complete$RML,na.rm=T)-min(dta_complete$RML,na.rm=T))



# 2. Correlational tests and plots--------------------------------------------------
# 2.1 Development --------------------------------
# SDI ------
cor.test(dta_complete$SDI,dta_complete$halflife_norm,method = "pearson")
cor.test(dta_complete$SDI,dta_complete$halflife_norm,method = "spearman")
cor.test(dta_complete$SDI,dta_complete$halflife_norm,method = "kendall")

cor.test(dta_rd$SDI,dta_rd$shock_standard,method = "pearson")
cor.test(dta_rd$SDI,dta_rd$shock_standard,method = "spearman")
cor.test(dta_rd$SDI,dta_rd$shock_standard,method = "kendall")

# income --------------
cor.test(dta_complete$l_income,dta_complete$halflife_norm,method = "pearson")
cor.test(dta_complete$l_income,dta_complete$halflife_norm,method = "spearman")
cor.test(dta_complete$l_income,dta_complete$halflife_norm,method = "kendall")

cor.test(dta_rd$l_income,dta_rd$shock_standard,method = "pearson")
cor.test(dta_rd$l_income,dta_rd$shock_standard,method = "spearman")
cor.test(dta_rd$l_income,dta_rd$shock_standard,method = "kendall")


# urbanization rate --------------
cor.test(dta_complete$urban_rate,dta_complete$halflife_norm,method = "pearson")
cor.test(dta_complete$urban_rate,dta_complete$halflife_norm,method = "spearman")
cor.test(dta_complete$urban_rate,dta_complete$halflife_norm,method = "kendall")

cor.test(dta_rd$urban_rate,dta_rd$shock_standard,method = "pearson")
cor.test(dta_rd$urban_rate,dta_rd$shock_standard,method = "spearman")
cor.test(dta_rd$urban_rate,dta_rd$shock_standard,method = "kendall")

# unemployement rate --------------
cor.test(dta_complete$unemployment,dta_complete$halflife_norm,method = "pearson")
cor.test(dta_complete$unemployment,dta_complete$halflife_norm,method = "spearman")
cor.test(dta_complete$unemployment,dta_complete$halflife_norm,method = "kendall")

cor.test(dta_rd$unemployment,dta_rd$shock_standard,method = "pearson")
cor.test(dta_rd$unemployment,dta_rd$shock_standard,method = "spearman")
cor.test(dta_rd$unemployment,dta_rd$shock_standard,method = "kendall")


# 2.2 Pandemic Severity -------

cor.test(dta_complete$l_cum_confirm,dta_complete$halflife_norm,method = "pearson")
cor.test(dta_complete$l_cum_confirm,dta_complete$halflife_norm,method = "spearman")
cor.test(dta_complete$l_cum_confirm,dta_complete$halflife_norm,method = "kendall")

cor.test(dta_rd$l_cum_confirm,dta_rd$shock_standard,method = "pearson")
cor.test(dta_rd$l_cum_confirm,dta_rd$shock_standard,method = "spearman")
cor.test(dta_rd$l_cum_confirm,dta_rd$shock_standard,method = "kendall")

# 2.3 Governance and public health management -------

# government efficiency ----------
cor.test(dta_complete$government_efficiency,dta_complete$halflife_norm,method = "pearson")
cor.test(dta_complete$government_efficiency,dta_complete$halflife_norm,method = "spearman")
cor.test(dta_complete$government_efficiency,dta_complete$halflife_norm,method = "kendall")

cor.test(dta_rd$government_efficiency,dta_rd$shock_standard,method = "pearson")
cor.test(dta_rd$government_efficiency,dta_rd$shock_standard,method = "spearman")
cor.test(dta_rd$government_efficiency,dta_rd$shock_standard,method = "kendall")


# GHSI (Health Security Index) --------
cor.test(dta_complete$GHSI,dta_complete$halflife_norm,method = "pearson")
cor.test(dta_complete$GHSI,dta_complete$halflife_norm,method = "spearman")
cor.test(dta_complete$GHSI,dta_complete$halflife_norm,method = "kendall")

cor.test(dta_rd$GHSI,dta_rd$shock_standard,method = "pearson")
cor.test(dta_rd$GHSI,dta_rd$shock_standard,method = "spearman")
cor.test(dta_rd$GHSI,dta_rd$shock_standard,method = "kendall")


# 2.4 Culture --------------------------------------------------
# cultural looseness --------
cor.test(dta_complete$Cultural_tightness_and_looseness,dta_complete$halflife_norm,method = "pearson")
cor.test(dta_complete$Cultural_tightness_and_looseness,dta_complete$halflife_norm,method = "spearman")
cor.test(dta_complete$Cultural_tightness_and_looseness,dta_complete$halflife_norm,method = "kendall")

cor.test(dta_rd$Cultural_tightness_and_looseness,dta_rd$shock_standard,method = "pearson")
cor.test(dta_rd$Cultural_tightness_and_looseness,dta_rd$shock_standard,method = "spearman")
cor.test(dta_rd$Cultural_tightness_and_looseness,dta_rd$shock_standard,method = "kendall")


# individualism -------------
cor.test(dta_complete$Individualism,dta_complete$halflife_norm,method = "pearson")
cor.test(dta_complete$Individualism,dta_complete$halflife_norm,method = "spearman")
cor.test(dta_complete$Individualism,dta_complete$halflife_norm,method = "kendall")

cor.test(dta_rd$Individualism,dta_rd$shock_standard,method = "pearson")
cor.test(dta_rd$Individualism,dta_rd$shock_standard,method = "spearman")
cor.test(dta_rd$Individualism,dta_rd$shock_standard,method = "kendall")

# religion -------------------
cor.test(dta_complete$Religion_is_very_important,dta_complete$halflife_norm,method = "pearson")
cor.test(dta_complete$Religion_is_very_important,dta_complete$halflife_norm,method = "spearman")
cor.test(dta_complete$Religion_is_very_important,dta_complete$halflife_norm,method = "kendall")

cor.test(dta_rd$Religion_is_very_important,dta_rd$shock_standard,method = "pearson")
cor.test(dta_rd$Religion_is_very_important,dta_rd$shock_standard,method = "spearman")
cor.test(dta_rd$Religion_is_very_important,dta_rd$shock_standard,method = "kendall")


# relational mobility -------------
cor.test(dta_complete$RML,dta_complete$halflife_norm,method = "pearson")
cor.test(dta_complete$RML,dta_complete$halflife_norm,method = "spearman")
cor.test(dta_complete$RML,dta_complete$halflife_norm,method = "kendall")

cor.test(dta_rd$RML,dta_rd$shock_standard,method = "pearson")
cor.test(dta_rd$RML,dta_rd$shock_standard,method = "spearman")
cor.test(dta_rd$RML,dta_rd$shock_standard,method = "kendall")

