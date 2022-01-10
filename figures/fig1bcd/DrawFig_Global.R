# title         : DrawFig.R
# purpose       : 
# author        : Yuchen Chai ycchai@mit.edu
# updates       : 2021-09-01
# ----------------------------------------------------------

DIR_HOME = "Sentiment_COVID19/"
DIR_DATA = paste0(DIR_HOME, "data/")
DIR_OUTPUT = paste0(DIR_HOME, "figure/fig1bcd/")
setwd(DIR_HOME)

# ------------------------
# import libraries
# ------------------------
library(tidyverse)
library(cowplot)
library(coronavirus)
library(readxl)
library(zoo)
devtools::source_gist("https://gist.github.com/Jianghao/d806dcc220a49df21024e0516f102b2f")

# ------------------------
# define global settings
# ------------------------
start.date <- "2020-01-01"
end.date   <- "2020-05-31"

# ------------------------
# read data
# ------------------------
# Country ISO list
dta_iso = read.csv(paste0(DIR_DATA, "iso_list.csv"))
dta_county_iso = dta_iso%>%
  select("country","Three_Letter_Country_Code","Two_Letter_Country_Code")%>%
  rename("iso3"="Three_Letter_Country_Code",
         "iso2"="Two_Letter_Country_Code")

# Sentiment each day
dta_sentiment = read.csv(paste0(DIR_DATA, "country_info_STL_0722.csv"))
dta_sentiment$tweet_date = as.Date(dta_sentiment$tweet_date)
dta_sentiment = dta_sentiment%>%
  select("country","tweet_date","senti_bert_std","senti_bert_std_trend","senti_bert_n")%>%
  rename("iso3"="country",
         "date"="tweet_date")

dta_tweet_n = dta_sentiment %>%
  group_by(date)%>%
  summarise(total=sum(senti_bert_n))

dta_sentiment = left_join(dta_sentiment,dta_tweet_n,by=c("date"))
dta_sentiment$weight = dta_sentiment$senti_bert_n/dta_sentiment$total
dta_trend = dta_sentiment%>%
  group_by(date)%>%
  summarise(senti_bert = weighted.mean(senti_bert_std,weight),
            senti_bert_trend = weighted.mean(senti_bert_std_trend,weight))
dta_trend$total = dta_tweet_n$total

# Google mobility
temp = read.csv(paste0(DIR_DATA, "Global_Mobility_Report.csv"))
temp$date = as.Date(temp$date)
dta_mobility = temp%>%
  select("country_region_code","date","retail_and_recreation_percent_change_from_baseline",
         "grocery_and_pharmacy_percent_change_from_baseline",
         "parks_percent_change_from_baseline",
         "transit_stations_percent_change_from_baseline",
         "workplaces_percent_change_from_baseline",
         "residential_percent_change_from_baseline"
  )%>%
  rename("iso2"="country_region_code")%>%
  filter(date >= as.Date(start.date),date<=as.Date(end.date))%>%
  group_by(iso2,date)%>%
  summarise(retail=mean(retail_and_recreation_percent_change_from_baseline,na.rm = T),
            grocery=mean(grocery_and_pharmacy_percent_change_from_baseline,na.rm = T),
            parks=mean(parks_percent_change_from_baseline,na.rm = T),
            transit=mean(transit_stations_percent_change_from_baseline,na.rm = T),
            workplace=mean(workplaces_percent_change_from_baseline,na.rm = T),
            residential=mean(residential_percent_change_from_baseline,na.rm = T),
  )
dta_mobility = merge(dta_mobility,dta_county_iso,by.x="iso2",by.y="iso2")
dta_mobility = dta_mobility%>%
  group_by(date)%>%
  summarise(mean_retail=mean(retail,na.rm=T),
            mean_grocery=mean(grocery,na.rm=T),
            mean_parks=mean(parks,na.rm=T),
            mean_transit=mean(transit,na.rm=T),
            mean_workplace=mean(workplace,na.rm=T),
            mean_residential=mean(residential,na.rm=T))
dta_mobility_transit_min = min(dta_mobility$mean_transit)
dta_mobility_transit_max = max(dta_mobility$mean_transit)
dta_mobility_transit_range = dta_mobility_transit_max - dta_mobility_transit_min
dta_mobility$mean_transit = (dta_mobility$mean_transit-dta_mobility_transit_min)/dta_mobility_transit_range * 100 - 100


# Number of tweets and share of tweets
load(paste0(DIR_DATA, "Covid_topic_stat_v1.Rdata"))
dta_tweet = by.country%>%
  rename("iso3"="country",
         "date"="tweet_date")
dta_tweet$date = as.Date(dta_tweet$date)
dta_tweet_total = dta_tweet%>%
  group_by(date)%>%
  summarise(total_n=sum(n),
            total_covid = sum(n_covid))
dta_tweet_total$share = dta_tweet_total$total_covid/dta_tweet_total$total_n*100


# Number of cases
data("coronavirus")
temp = merge(coronavirus,dta_county_iso,by.x="country",by.y="country")
temp$cases[temp$cases<0] = 0
covid <- temp %>%
  group_by(date, type) %>%
  summarise(total_cases = sum(cases)) %>%
  filter(date >= as.Date(start.date), date <= as.Date(end.date))

covid = covid %>%
  group_by(type)%>%
  mutate(smooth_03da = rollmean(total_cases,k=3,fill=NA))%>%
  mutate(smooth_05da = rollmean(total_cases,k=5,fill=NA))%>%
  mutate(smooth_07da = rollmean(total_cases,k=7,fill=NA))%>%
  ungroup()

covid <- subset(covid, type == "confirmed")

# ------------------------
# figure 1: Global Data
# ------------------------
mRound = function(p_ratio)
{
  ret = 1
  if(p_ratio<=10){
    ret = p_ratio%/%5 * 5
  }
  else if(p_ratio<=100){
    if (p_ratio<20){
      ret = 10
    }
    ret = p_ratio%/%20 * 20
  }
  else if(p_ratio<=1000){
    if (p_ratio<200){
      ret = 100
    }
    else{
      ret = p_ratio%/%200 * 200
    }
    
  }
  else if(p_ratio<=10000){
    if (p_ratio<2000){
      ret = 1000
    }
    else{
      ret = p_ratio%/%2000 * 2000
    }
  }
  else if(p_ratio<=100000){
    if (p_ratio<20000){
      ret = 10000
    }
    else{
      ret = p_ratio%/%20000 * 20000 
    }
  }
  else if(p_ratio<=1000000){
    if (p_ratio<200000){
      ret = 100000
    }
    else{
      ret = p_ratio%/%200000 * 200000 
    }
  }
  else if(p_ratio<=10000000){
    if (p_ratio<2000000){
      ret = 1000000
    }
    else{
      ret = p_ratio%/%2000000 * 2000000 
    }
  }
  if(ret==0)
  {
    ret = 1
  }
  ret
}

drawMap_global = function(mSentiment,mIndex)
{
  col.average <- "#2e9fdf"
  col.quan25 = "#00A087"
  col.quan75 = "#FF7F0E"
  col.right1 <- "#00A087"
  col.right2 <- "#2E9FDF"
  col.lockdown <- "#555555"
  
  sentiment_max = max(dta_trend$display)
  sentiment_min = min(dta_trend$display)
  index_max = max(dta_mobility[mIndex])
  index_min = min(dta_mobility[mIndex])
  mOffset = sentiment_min-index_min
  mScale = (index_max-index_min)/(sentiment_max-sentiment_min)
  dta_mobility["display"] = dta_mobility[mIndex]
  dta_mobility$display = (dta_mobility$display-index_min)/mScale + sentiment_min
  mobility_zero = (0-index_min)/mScale + sentiment_min
  
  p_top_senti <-
    ggplot() +
    geom_line(dta_trend,mapping=aes(x=date,y = display),color="grey25", alpha = 0.2) +
    geom_line(subset(dta_trend,display_trend!=0), mapping=aes(x=date,y = display_trend), color="black", alpha = 1, size = 1.2)+
    geom_line(dta_mobility,mapping=aes(x=date,y = display), color=col.right1, alpha = 0.2) +
    geom_smooth(dta_mobility,mapping=aes(x=date,y = display),method = "loess", span = 0.1, color=col.right1, alpha = 1,se=F)+
    geom_hline(aes(yintercept=mobility_zero),linetype="dashed",color=col.right1)+
    geom_vline(aes(xintercept=as.Date("2020-03-11")),color="black")+
    scale_x_date(date_breaks = "1 month", date_labels = "%m-%d") +
    labs(x = NULL)+
    scale_y_continuous(
      name = paste("Standardized sentiment score"),
      # Add a second axis and specify its features
      sec.axis = sec_axis(~.*mScale-sentiment_min*mScale+index_min, name=paste("Google mobility index (",mIndex, ")",sep=""))
    )+
    theme_Publication() +
    theme(legend.position = "bottom",
          axis.line.y.right=element_line(colour=col.right1),
          axis.text.y.right=element_text(colour=col.right1),
          axis.ticks.y.right=element_line(colour=col.right1),
          axis.title.y.right=element_text(colour=col.right1),
          axis.line.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank()
    )
  
  
  # p_top_senti
  
  # Number of tweets and share of tweet
  num_tweet_max = max(dta_tweet_total$total_n)
  percentage_max = max(dta_tweet_total$share)
  ratio_scale = num_tweet_max/percentage_max
  ratio_scale = mRound(ratio_scale)
  p_middle_tweet <-
    ggplot(dta_tweet_total,aes(x=date)) +
    geom_smooth(aes(y = total_n), method = "loess", span = 0.1, alpha = 0.3, colour = "grey40", se = F) +
    scale_x_date(date_breaks = "1 month", date_labels = "%m-%d") +
    labs(x = NULL) +
    geom_area(aes(y = share*ratio_scale/2), alpha = 0.5, colour = col.right2, fill = col.right2) +
    scale_y_continuous(
      name = "Number of Tweets (day)",
      # Add a second axis and specify its features
      sec.axis = sec_axis(~./ratio_scale *2, name="Share of COVID-19 Tweet (%)")
    ) +
    theme_Publication() +
    theme(legend.position = "bottom",
          axis.line.y.right=element_line(colour=col.right2),
          axis.text.y.right=element_text(colour=col.right2),
          axis.ticks.y.right=element_line(colour=col.right2),
          axis.title.y.right=element_text(colour=col.right2),
          axis.line.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          panel.background = element_rect(fill="transparent",colour=NA),
          plot.background = element_rect(fill="transparent",colour=NA),
    )
  # p_middle_tweet
  
  
  # Number of cases
  p_bottom_case <-
    ggplot(covid, aes(x=date)) + 
    geom_area(aes(y = display),  fill = "purple", alpha = 0.6) +
    scale_x_date(date_breaks = "1 month", date_labels = "%m-%d") +
    labs(x = NULL, y = "Daily confirmed cases") +
    xlab("Date")+
    scale_fill_manual("Daily new cases", values = c("#E64B35", "#00A087", "#9632B8")) +
    theme_Publication() +
    theme(
      legend.position = c(0.15, 0.8),
      legend.title = element_text(size = 10), 
      legend.key.size= unit(0.5, "cm"),
    ) +
    guides(fill = guide_legend(title.position="top", title.hjust = 0))
  
  p <- plot_grid(p_top_senti, p_middle_tweet, p_bottom_case, ncol = 1, nrow = 3, rel_heights = c(3, 2, 1.5), align="vh",vjust=c(0,2.5,0))

  save_plot(paste0(DIR_OUTPUT, "out_Global_Combined.pdf"),
            p, base_width = 9, base_height = 12)
}


mTrend = "senti_bert"
mTrendd = paste(mTrend,"_trend",sep="")

# Settings: Google Mobility Index
#   mean_retail
#   mean_grocery
#   mean_parks
#   mean_transit
#   mean_workplace
#   mean_residential
mIndex = "mean_transit"

dta_trend['display'] = dta_trend[mTrend]
dta_trend['display_trend'] = dta_trend[mTrendd]

# Settings: Smoothing coronavirus
#   None: total_cases
#   3 days: smooth_03da
#   5 days: smooth_05da
#   7 days: smooth_07da
covid$display = covid$total_cases

drawMap_global(mTrend,mIndex)
