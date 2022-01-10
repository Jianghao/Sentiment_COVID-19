## Policy analysis using R package COVID-19

rm(list=ls())
setwd("~/script_clean/")


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



# plot the inference figure (Figure S4) --------------------------------
effect20_byday<- placebo_list%>%
  filter(rel_date>=-14 & rel_date<=7 )%>%
  select(starts_with("gap"),"treatname","rel_date")%>% 
  group_by(treatname,rel_date)%>%
  dplyr::summarise_if(is.numeric, mean, na.rm = TRUE)


alpha = 0.05
cll <- apply(sapply("grey70", col2rgb)/255, 2, function(x) rgb(x[1], x[2], x[3], alpha=alpha)) 

pdf(file=paste0("pipeline/lockdown/out/",quan,"_",trend,"_",senti,"_",type,"_plots.pdf"),height=6,width=8)
plot(unique(effect20_byday$rel_date),effect20_byday$gap[effect20_byday$treatname=="ARG"],ylim=c(-2,2),xlim=c(-14,8),type="l",col=cll,
     ylab="Sentiment Diff.(treatment - synthetic control)",xlab="Relative date to lockdown",main="Quantile75 sentiment",lwd=0.3,cex.axis=1,cex.lab=1,axes=F)
for(i in 1:length(unique(effect20_byday$treatname))){
  tem<-subset(effect20_byday,treatname==unique(effect20_byday$treatname)[i])
  exclude_list<-pre_RMSPE%>%
    filter(treatname==unique(effect20_byday$treatname)[i] )
  tem<-tem[colSums(!is.na(tem)) > 0] # remove columns with no values

  for(j in 4:length(colnames(tem))){
    if(exclude_list[1,j-1]<=0.5){ # remove the placebos with bad fit
      lines(tem$rel_date,pull(tem[,j]),col=cll,lwd=0.6)
    }
  }
}

# non-significant ones
for (i in 1:nrow(treat_effect20)){
  if(treat_effect20$pvalue[i]>=0.1){
    lines(effect20_byday$rel_date[effect20_byday$treatname==treat_effect20$treatname[i]],
          effect20_byday$gap[effect20_byday$treatname==treat_effect20$treatname[i]],col="mistyrose1",alpha=0.05,lwd=0.6)
  }
}


# significant ones
for (i in 1:nrow(treat_effect20)){
  if(treat_effect20$pvalue[i]<0.1){
    lines(effect20_byday$rel_date[effect20_byday$treatname==treat_effect20$treatname[i]],
          effect20_byday$gap[effect20_byday$treatname==treat_effect20$treatname[i]],col="#E64B35",lwd=1.2,alpha=0.8)
    if(treat_effect20$treatname[i]=="MYS"){
      text(7,effect20_byday$gap[effect20_byday$treatname==treat_effect20$treatname[i]
                                & effect20_byday$rel_date==7],treat_effect20$treatname[i],cex=0.7,adj=c(-0.3,1.5))
    }else if(treat_effect20$treatname[i]=="JAM"){
      text(7,effect20_byday$gap[effect20_byday$treatname==treat_effect20$treatname[i]
                                & effect20_byday$rel_date==7],treat_effect20$treatname[i],cex=0.7,adj=c(-0.3,1.2))
    } else if(treat_effect20$treatname[i]=="LBN"){
      text(7,effect20_byday$gap[effect20_byday$treatname==treat_effect20$treatname[i]
                                & effect20_byday$rel_date==7],treat_effect20$treatname[i],cex=0.7,adj=c(-0.3,-1.5))
    }else{
      text(7,effect20_byday$gap[effect20_byday$treatname==treat_effect20$treatname[i]
                                & effect20_byday$rel_date==7],treat_effect20$treatname[i],cex=0.7,pos=4)
    }
  }
}


# legend
legend(x=-14,y=2,legend=c("Treated (significant effect)","Treated (non-significant)","Placebo"),
       col=c("#E64B35","mistyrose1","grey70"),lty=1,cex=0.8,box.lty=0)

abline(h=0,lty=1)
abline(v=0,lty=2)
axis(1,at=seq(-14,8,2),labels=seq(-14,8,2),cex.axis=1,las=1)
axis(2,at=seq(-2,2,1),labels=seq(-2,2,1),cex.axis=1,las=1)
dev.off()
