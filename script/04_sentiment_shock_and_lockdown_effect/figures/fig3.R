## Policy analysis using R package COVID-19

rm(list=ls())
setwd("~/Sentiment_COVID-19/")


library(haven)
library(readxl)
library(rapport)
library(cowplot)
library(ggridges)
library(ggthemes)
library(tidyverse)
library(RColorBrewer)
library(COVID19)
Sys.setlocale(category = "LC_ALL", locale = "english")
devtools::source_gist("https://gist.github.com/Jianghao/d806dcc220a49df21024e0516f102b2f")


# inference ---------------------------------------------------------------

senti<-"bert"
quan<-"mean"
type<-"loose"
trend<-"std_trend"

read_plus<-function(flnm){
  read_dta(flnm)%>%
    mutate(treatname=sub('.dta', '',sub('.*placebo_result_', '', flnm)))
}

get.rmspe<-function(x){
  sqrt(mean(x*x,na.rm=T))
}

get.rmspe.positive<-function(x){
  sqrt(mean(max(x,0)*max(x,0),na.rm=T))
}

get.rmspe.negative<-function(x){
  sqrt(mean(min(x,0)*min(x,0),na.rm=T))
}

placebo_list<-
  list.files(paste0("pipeline/lockdown/placebo/"),
             full.names = T) %>%
  map_df(~read_plus(.))%>%
  filter(rel_date>=-14)

# effect matrix
effect20<- placebo_list%>%
  filter(rel_date>=0 )%>%
  dplyr::select(starts_with("gap"),"treatname")%>% 
  group_by(treatname)%>%
  dplyr::summarise_if(is.numeric, mean, na.rm = TRUE)
effect20_vector<- as.numeric(effect20[1,3:length(colnames(effect20))])
for (i in 2:nrow(effect20)){
  tem<-as.numeric(effect20[i,3:length(colnames(effect20))])
  effect20_vector<-append(effect20_vector,tem)
}
# drop the observations with pre-MPSE>0.5
treat_effect20<-effect20[,1:2]

# inference matrix
pre_RMSPE<-placebo_list%>%
  filter(rel_date<0)%>%
  dplyr::select(starts_with("gap"),"treatname")%>%
  group_by(treatname)%>%
  dplyr::summarise_if(is.numeric, get.rmspe)

preRMSPE_vector<- as.numeric(pre_RMSPE[1,3:length(colnames(pre_RMSPE))])
for (i in 2:nrow(pre_RMSPE)){
  tem<-as.numeric(pre_RMSPE[i,3:length(colnames(pre_RMSPE))])
  preRMSPE_vector<-append(preRMSPE_vector,tem)
}
effect20_filtered<-effect20_vector[preRMSPE_vector<=0.5] # filter the ones with bad fit
effect20_filtered<-effect20_filtered[!is.na(effect20_filtered)]

# calculate p value based on effect size
treat_effect20$pvalue<-NULL
for (i in 1:nrow(effect20)){
  if(treat_effect20[i,2]<0){
    treat_effect20$pvalue[i]<-(sum(sapply(1:length(effect20_filtered),function(x){
      ifelse(effect20_filtered[x]<as.numeric(treat_effect20[i,2]),1,0)})))/length(effect20_filtered)
  }else{
    treat_effect20$pvalue[i]<-(sum(sapply(1:length(effect20_filtered),function(x){
      ifelse(effect20_filtered[x]>as.numeric(treat_effect20[i,2]),1,0)})))/length(effect20_filtered)
  }
}

treat_effect20$RMSPE<-pre_RMSPE$gap
treat_effect20<-subset(treat_effect20,RMSPE<=0.5)
treat_effect20<-treat_effect20[order(treat_effect20$pvalue),]

write.csv(treat_effect20,paste0("pipeline/lockdown/out/",quan,"_",trend,"_",senti,"_",type,"_inference.csv"))



# plot average effect (Figure 3)-----------------------------------------------------
treat_effect20<-read_csv(paste0("pipeline/lockdown/out/",quan,"_",trend,"_",senti,"_",type,"_inference.csv"))

plot_daily<-placebo_list%>%
  filter(treatname %in% treat_effect20$treatname)%>%
  dplyr::select(treatname,treat,counterfact,rel_date)%>%
  group_by(rel_date)%>%
  dplyr::summarise_if(is.numeric, mean, na.rm = TRUE)

treat<-plot_daily%>%
  dplyr::select(-counterfact)%>%
  mutate(type="Lockdown country")
control<-plot_daily%>%
  dplyr::select(-treat)%>%
  mutate(type="Synthetic control")%>%
  dplyr::rename(treat=counterfact)
plot_daily_2<-rbind(treat,control)

p1<-
  ggplot(data=plot_daily_2,aes(x=rel_date,y=treat,color=type))+
  geom_line(alpha=0.8,size=0.8)+
  geom_vline(xintercept = 0,linetype="dashed",color="black")+
  scale_color_manual(values=c("#E64B35","#357EBD"))+
  xlim(-14,7)+
  xlab("Relative Date to Lockdown")+
  ylab("Standardized sentiment relative to pre-COVID")+
  theme_Publication()+
  theme(legend.position = 'top',
        legend.direction = "horizontal",
        legend.title = element_blank(),
        legend.key.size = unit(0.6, 'cm'), 
        legend.spacing.x = unit(0.6, 'cm'))
p1


# histrogram
p2<-ggplot(treat_effect20,aes(x=gap))+
  geom_histogram(color="#357EBD", fill="#357EBD",alpha=0.6)+
  #geom_density(alpha=.2, fill="#FF6666") +
  geom_vline(xintercept=0,color="red",size=1)+
  xlab("Lockdown effect on standardized sentiment")+
  ylab("Country counts")+
  theme_Publication()
p2


p.main <- plot_grid(p1, p2, ncol = 2, nrow=1, 
                    align="hv", labels = c("c", "d"))
save_plot(paste0("pipeline/lockdown/out/sentiment_",quan,"_plots.pdf"), p.main, base_width = 14, base_height = 5.5)


