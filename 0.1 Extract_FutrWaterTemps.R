library(tidyverse)
library(reshape2)
library(ggplot2)

setwd("~/UMNpostdoc/ProjectEWM/RProjects/EWM_ClimateChange")

###1. Read all the different temperature projection datasets
ACCESS.temp.metrics=read.csv("raw_data/ACCESS_thermal_metrics.tsv", sep="\t")
MIROC5.temp.metrics=read.csv("raw_data/MIROC5_thermal_metrics.tsv", sep="\t")
GFDL.temp.metrics=read.csv("raw_data/GFDL_thermal_metrics.tsv", sep="\t")
IPSL.temp.metrics=read.csv("raw_data/IPSL_thermal_metrics.tsv", sep="\t")
MRI.temp.metrics=read.csv("raw_data/MRI_thermal_metrics.tsv", sep="\t")

ACCESS.temp.GDD10c=ACCESS.temp.metrics[,c(1:2,15)]
MIROC5.temp.GDD10c=MIROC5.temp.metrics[,c(1:2,15)]
GFDL.temp.GDD10c=GFDL.temp.metrics[,c(1:2,15)]
IPSL.temp.GDD10c=IPSL.temp.metrics[,c(1:2,15)]
MRI.temp.GDD10c=MRI.temp.metrics[,c(1:2,15)]

head(ACCESS.temp.GDD10c)
head(MIROC5.temp.GDD10c)
head(GFDL.temp.GDD10c)
head(IPSL.temp.GDD10c)
head(MRI.temp.GDD10c)

mn.nhd_ids=read_csv("raw_data/MN_nhd2dowlknum.csv")
head(mn.nhd_ids)
dim(mn.nhd_ids) 

ACCESS.temp.GDD10c.DOWs=merge(ACCESS.temp.GDD10c,mn.nhd_ids, by="site_id")
write_csv(ACCESS.temp.GDD10c.DOWs, "processed_data/ACCESS.temp.GDD10c.DOWs.csv")
MIROC5.temp.GDD10c.DOWs=merge(MIROC5.temp.GDD10c,mn.nhd_ids, by="site_id")
write_csv(MIROC5.temp.GDD10c.DOWs, "processed_data/MIROC5.temp.GDD10c.DOWs.csv")
GFDL.temp.GDD10c.DOWs=merge(GFDL.temp.GDD10c,mn.nhd_ids, by="site_id")
write_csv(GFDL.temp.GDD10c.DOWs, "processed_data/GFDL.temp.GDD10c.DOWs.csv")
IPSL.temp.GDD10c.DOWs=merge(IPSL.temp.GDD10c,mn.nhd_ids, by="site_id")
write_csv(IPSL.temp.GDD10c.DOWs, "processed_data/IPSL.temp.GDD10c.DOWs.csv")
MRI.temp.GDD10c.DOWs=merge(MRI.temp.GDD10c,mn.nhd_ids, by="site_id")
write_csv(MRI.temp.GDD10c.DOWs, "processed_data/MRI.temp.GDD10c.DOWs.csv")

### the above final dataset has a lot of lake ids to work with; subset by  merging with EWM prsence/absence data
EWMprsabs.data=read.csv("processed_data/EWM.infes_relfrq.selpreds.prsabs.csv")
head(EWMprsabs.data)
head(ACCESS.temp.GDD10c.DOWs)

### Filter the water temperature data to only include years between 2040 and 2060; the years of most EWM sampling
ACCESS.GDD10c.fut=ACCESS.temp.GDD10c.DOWs%>%filter(year>2039 & year<2061)%>% group_by(dowlknum)%>% summarise(
  avg.ann.gdd=mean(gdd_wtr_10c)
)
dim(ACCESS.GDD10c.fut)
ACCESS.GDD10c.fut%>%View()

MIROC5.GDD10c.fut=MIROC5.temp.GDD10c.DOWs%>%filter(year>2039 & year<2061)%>% group_by(dowlknum)%>% 
  summarise(
    avg.ann.gdd=mean(gdd_wtr_10c)
  )
dim(MIROC5.GDD10c.fut)
MIROC5.GDD10c.fut%>%View()

GFDL.GDD10c.fut=GFDL.temp.GDD10c.DOWs%>%filter(year>2039 & year<2061)%>% group_by(dowlknum)%>% summarise(
  avg.ann.gdd=mean(gdd_wtr_10c)
)
dim(GFDL.GDD10c.fut)
GFDL.GDD10c.fut%>%View()

IPSL.GDD10c.fut=IPSL.temp.GDD10c.DOWs%>%filter(year>2039 & year<2061)%>% group_by(dowlknum)%>% summarise(
  avg.ann.gdd=mean(gdd_wtr_10c)
)
dim(IPSL.GDD10c.fut)
IPSL.GDD10c.fut%>%View()

MRI.GDD10c.fut=MRI.temp.GDD10c.DOWs%>%filter(year>2039 & year<2061)%>% group_by(dowlknum)%>% summarise(
  avg.ann.gdd=mean(gdd_wtr_10c)
)
dim(MRI.GDD10c.fut)
MRI.GDD10c.fut%>%View()

### Changing column names to match the EWM dataset
colnames(ACCESS.GDD10c.fut)[2]="ACCESS.avg.ann.gdd"
colnames(ACCESS.GDD10c.fut)[1]="DOWLKNUM"
ACCESS.GDD10c.fut

EWMprsabs.data_updated =merge(EWMprsabs.data,ACCESS.GDD10c.fut, by="DOWLKNUM")
head(EWMprsabs.data_updated)

colnames(MIROC5.GDD10c.fut)[2]="MIROC5.avg.ann.gdd"
colnames(MIROC5.GDD10c.fut)[1]="DOWLKNUM"
MIROC5.GDD10c.fut

EWMprsabs.data_updated =merge(EWMprsabs.data_updated,MIROC5.GDD10c.fut, by="DOWLKNUM")
head(EWMprsabs.data_updated)

colnames(IPSL.GDD10c.fut)[2]="IPSL.avg.ann.gdd"
colnames(IPSL.GDD10c.fut)[1]="DOWLKNUM"
IPSL.GDD10c.fut

EWMprsabs.data_updated =merge(EWMprsabs.data_updated,IPSL.GDD10c.fut, by="DOWLKNUM")
head(EWMprsabs.data_updated)

colnames(GFDL.GDD10c.fut)[2]="GFDL.avg.ann.gdd"
colnames(GFDL.GDD10c.fut)[1]="DOWLKNUM"
GFDL.GDD10c.fut

EWMprsabs.data_updated =merge(EWMprsabs.data_updated,GFDL.GDD10c.fut, by="DOWLKNUM")
head(EWMprsabs.data_updated)

colnames(MRI.GDD10c.fut)[2]="MRI.avg.ann.gdd"
colnames(MRI.GDD10c.fut)[1]="DOWLKNUM"
MRI.GDD10c.fut

EWMprsabs.data_updated =merge(EWMprsabs.data_updated,MRI.GDD10c.fut, by="DOWLKNUM")
head(EWMprsabs.data_updated)

LakeConn=read_csv("raw_data/LakeConn.data.csv")
LakeConn

EWMprsabs.data_final=left_join(EWMprsabs.data_updated,LakeConn, by="DOWLKNUM")
EWMprsabs.data_final.nona=EWMprsabs.data_final%>%na.omit()
dim(EWMprsabs.data_final.nona)
head(EWMprsabs.data_final.nona)

write_csv(EWMprsabs.data_final.nona, "processed_data/EWM.prsabs40to60_AllGCMs.csv")


############### MAKE A PLOT COMPARING PREDICTED WATER TEMPERATURE GDD
EWM.futr.data=read_csv("processed_data/EWM.prsabs40to60_AllGCMs.csv")
EWM.futr.data
EWM.curr.data=read_csv("processed_data/EWM.prsabs95to15_AllGCMs.csv")
EWM.curr.data
EWM.currtemp.GCMs=EWM.curr.data[,c(1:5,19:23)]
EWM.futrtemp.GCMs=EWM.futr.data[,c(1:5,17:21)]


EWM.currtemp.GCMs=EWM.currtemp.GCMs%>%mutate(Period= rep("Current",468))
EWM.futrtemp.GCMs=EWM.futrtemp.GCMs%>%mutate(Period= rep("Future",467))
bind_rows(EWM.currtemp.GCMs, EWM.futrtemp.GCMs)
EWM.CURR.FUTR.GCM_data=bind_rows(EWM.currtemp.GCMs, EWM.futrtemp.GCMs)
EWM.CURR.FUTR.GCM_data


EWM.CURR.FUTR.GCM_melt=melt(EWM.CURR.FUTR.GCM_data[,c(6:11)])

EWM.CURR.FUTR.GCM_melt$GCMs=gsub('.{12}$','',EWM.CURR.FUTR.GCM_melt$variable)
head(EWM.CURR.FUTR.GCM_melt)


ggplot(EWM.CURR.FUTR.GCM_melt, aes(x=GCMs, y=value, fill=Period)) +
geom_boxplot(outlier.shape=NA)+  geom_point(alpha=0.05,position=position_jitterdodge())+
xlab("Climate Model")+ylab("Annual GDD")+ theme(text = element_text(size = 20))
ggsave("CompareGCM_GDDs.png", path="./Figures", device = "png",width = 6, height = 4.5 )



