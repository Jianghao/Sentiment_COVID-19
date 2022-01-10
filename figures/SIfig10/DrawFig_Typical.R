# title         : DrawFig.R
# purpose       : 
# author        : Yuchen Chai ycchai@mit.edu
# updates       : 2021-09-01
# ----------------------------------------------------------

DIR_HOME = "Sentiment_COVID19/"
DIR_DATA = paste0(DIR_HOME, "data/")
DIR_OUTPUT = paste0(DIR_HOME, "figure/SIfig10")
setwd(DIR_HOME)

# ------------------------
# import libraries
# ------------------------
library(tidyverse)
library(cowplot)
library(coronavirus)
library(readxl)
library(ggplot2)
library(reshape2)
devtools::source_gist("https://gist.github.com/Jianghao/d806dcc220a49df21024e0516f102b2f")

# ------------------------
# define global settings
# ------------------------
start.date <- "2020-01-01"
end.date   <- "2020-05-25"

# ------------------------
# read data
# ------------------------
# Country ISO list
dta_iso = read.csv(paste0(DIR_DATA, "iso_list.csv"))
dta_county_iso = dta_iso%>%
  select("country","Three_Letter_Country_Code","Two_Letter_Country_Code")%>%
  rename("iso3"="Three_Letter_Country_Code",
         "iso2"="Two_Letter_Country_Code")

# Drop date and min date
dta_drop_min = read.csv(paste0(DIR_DATA, "country_info_Drop_Min.csv"))
dta_drop_min = dta_drop_min %>%
  select("Nation",
         "min_senti_bert",
         "drop_senti_bert",
         "min_senti_bert_std")%>%
  rename("iso3"="Nation")
dta_drop_min$min_senti_bert = as.Date(dta_drop_min$min_senti_bert,"%Y-%m-%d")
dta_drop_min$min_senti_bert_std = as.Date(dta_drop_min$min_senti_bert_std,"%Y-%m-%d")
dta_drop_min$drop_senti_bert = as.Date(dta_drop_min$drop_senti_bert,"%Y-%m-%d")
drop_show = dta_drop_min %>%
  select(iso3,min_senti_bert_std,drop_senti_bert)

# Sentiment each day
dta_sentiment = read.csv(paste0(DIR_DATA, "country_info_STL_0722.csv"))
dta_sentiment$tweet_date = as.Date(dta_sentiment$tweet_date)
dta_sentiment = dta_sentiment%>%
  select("country","tweet_date","senti_bert", "senti_bert_std", "senti_bert_trend","senti_bert_std_trend")%>%
  rename("iso3"="country",
         "date"="tweet_date")

# Fitted curve
dta_fit = read.csv(paste0(DIR_DATA, "Final_Output_Bert_Fitted.csv"))
dta_fit$X = NULL
dta_fit = dta_fit %>%
  rename("iso3"="country",
         "ci_negative"="senti_bert_std_trend_fitted_normal.ci",
         "ci_positive"="senti_bert_std_trend_fitted_normal.ci.1") %>%
  select(iso3,senti_bert_std_trend,tweet_date,senti_bert_std_trend_fitted_normal,ci_positive, ci_negative)
dta_fit$tweet_date = as.Date(dta_fit$tweet_date)

# Half life
dta_halflife = read.csv(paste0(DIR_DATA, "Final_Output.csv"))
dta_halflife = dta_halflife %>%
  select(iso3,HalfLife)


# ------------------------
# figure 1: Fitted curve of 9 typical countries
# ------------------------
drawMap = function(mCountry)
{
  col.raw = "#000000"
  alpha.raw = 0.2
  col.detrend = "#000000"
  col.fit = "#E64B35"
  col.date = "#555555"
  col.halflife = "#00A087"
  
  minDate = subset(dta_drop_min,iso3==mCountry)$min_senti_bert_std
  before = subset(dta_fit,(iso3==mCountry) & (tweet_date<minDate-7))
  avg_value = mean(before$senti_bert_std_trend,na.rm=T)
  halflife = subset(dta_halflife,iso3==mCountry)
  halflife = halflife$HalfLife
  halflife_date = minDate + as.integer(halflife)
  halflife_text = halflife_date + 30
  
  
  
  p <-
    ggplot() +
    geom_line(subset(dta_sentiment,iso3==mCountry & date<="2020-05-25"),mapping=aes(x=date,y = senti_bert_std),color=col.raw, alpha = alpha.raw) +
    geom_line(subset(dta_fit,iso3==mCountry & tweet_date<="2020-05-25"),mapping=aes(x=tweet_date,y = senti_bert_std_trend),color=col.detrend, alpha = 1) +
    geom_line(subset(dta_fit,iso3==mCountry & !is.na(senti_bert_std_trend_fitted_normal) & tweet_date<="2020-05-25"),mapping=aes(x=tweet_date,y = senti_bert_std_trend_fitted_normal), color=col.fit, alpha = 1)+
    geom_ribbon(subset(dta_fit,iso3==mCountry & !is.na(ci_positive) & tweet_date<="2020-05-25"),mapping=aes(x=tweet_date,ymin=ci_negative, ymax=ci_positive), fill=col.fit,color=FALSE, alpha = 0.2)+
    geom_hline(mapping=aes(yintercept=avg_value),linetype="dashed",color=col.raw)+
    geom_vline(subset(drop_show,iso3==mCountry),mapping=aes(xintercept=min_senti_bert_std),linetype="dashed",color=col.raw)+
    geom_vline(subset(drop_show,iso3==mCountry),mapping=aes(xintercept=drop_senti_bert),linetype="dashed",color=col.raw)+
    geom_vline(mapping=aes(xintercept=halflife_date),linetype="dashed",color=col.halflife)+
    geom_text(mapping=aes(x=halflife_text,y=5),label=paste("Halflife = ",round(halflife,2)," days",sep=""))+
    scale_x_date(date_breaks = "1 month", date_labels = "%m-%d") +
    scale_y_continuous(limits=c(-6,6))+
    labs(x = NULL)+
    ylab("std sentiment")+
    theme_Publication() +
    theme(
      legend.position = c(0.25, 0.8),
      legend.title = element_text(size = 10), 
      legend.key.size= unit(0.5, "cm"),
    )
  
  p
}

countryList = c("DEU","CHN","GBR","USA","ITA","NLD","ESP","CHE","DNK")
titles = c("Germany","China","United Kingdom","United States","Italy","Netherlands","Spain","Switzerland","Denmark")

pdf(paste0(DIR_OUTPUT, "out_typical_country_fitted_curve.pdf"),width=16,height=9)
p1 = drawMap(countryList[1])
p2 = drawMap(countryList[2])
p3 = drawMap(countryList[3])
p4 = drawMap(countryList[4])
p5 = drawMap(countryList[5])
p6 = drawMap(countryList[6])
p7 = drawMap(countryList[7])
p8 = drawMap(countryList[8])
p9 = drawMap(countryList[9])

fig = plot_grid(p1,p2,p3,p4,p5,p6,p7,p8,p9, 
                labels = titles, label_size = 12)

print(fig)
dev.off()
