
library(tidyverse)
library(reshape2)
library(ggplot2)


setwd("~/UMNpostdoc/ProjectEWM/RProjects/EWM_ClimateChange")

###Extract EWM occurrence data

EWM.alllakes.data=read_csv("raw_data/EWM.occ_abund.data.csv")
EWM.alllakes.data%>%filter(EWMSTATUS!="U")%>%View()

EWM.alllakes.data%>%View()
EWM.secchi=EWM.alllakes.data[,c(1:7,19,21)]%>%na.omit()
EWM.secchi

LakeConn.data=read_csv("raw_data/LakeConn.data.csv")
RoadDensity.data=LakeConn.data%>%select(DOWLKNUM,roaddensity_density_mperha)%>%na.omit()

RoadDensity.data

EWM.alllakes.secchi.conn.data=left_join(EWM.secchi,RoadDensity.data, by="DOWLKNUM")%>%na.omit() ## ALL POTENTIAL LAKES THAT CAN BE PREDICTED
EWM.surveyed.lakes.secchi.conn.data=EWM.alllakes.secchi.conn.data%>%filter(EWMSTATUS!="U")
EWM.surveyed.lakes.secchi.conn.data
write_csv(EWM.surveyed.lakes.secchi.conn.data,"processed_data/EWM.surveyed.lakes.secchi.conn.data.csv")

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

head(ACCESS.temp.GDD10c.DOWs)
hist(ACCESS.temp.GDD10c.DOWs$year)

### the above final dataset has a lot of lake ids to work with; subset by  merging with EWM prsence/absence data

##EWMprsabs.data=read_csv("processed_data/EWM.infes_relfrq.selpreds.prsabs.csv") ### the original data with 10 covariates,                                                                                ### and lake conn. was added later

EWM.data=read_csv("processed_data/EWM.surveyed.lakes.secchi.conn.data.csv") ## the new version with only 3 key covariates, more lakes included
EWM.data

### Filter the water temperature data to only include years between 1995 and 2015; the years of most EWM sampling
ACCESS.temp.GDD10c.DOWs=read_csv("processed_data/ACCESS.temp.GDD10c.DOWs.csv")
ACCESS.GDD10c.curr=ACCESS.temp.GDD10c.DOWs%>%filter(year>1994 & year<2016)%>% group_by(dowlknum)%>% summarise(
  avg.ann.gdd=mean(gdd_wtr_10c)
)
dim(ACCESS.GDD10c.curr)
ACCESS.GDD10c.curr%>%View()


MIROC5.temp.GDD10c.DOWs=read_csv("processed_data/MIROC5.temp.GDD10c.DOWs.csv")
MIROC5.GDD10c.curr=MIROC5.temp.GDD10c.DOWs%>%filter(year>1994 & year<2016)%>% group_by(dowlknum)%>% 
  summarise(
    avg.ann.gdd=mean(gdd_wtr_10c)
  )
dim(MIROC5.GDD10c.curr)
MIROC5.GDD10c.curr%>%View()

GFDL.temp.GDD10c.DOWs=read_csv("processed_data/GFDL.temp.GDD10c.DOWs.csv")
GFDL.GDD10c.curr=GFDL.temp.GDD10c.DOWs%>%filter(year>1994 & year<2016)%>% group_by(dowlknum)%>% summarise(
  avg.ann.gdd=mean(gdd_wtr_10c)
)
dim(GFDL.GDD10c.curr)
GFDL.GDD10c.curr%>%View()

IPSL.temp.GDD10c.DOWs=read_csv("processed_data/IPSL.temp.GDD10c.DOWs.csv")
IPSL.GDD10c.curr=IPSL.temp.GDD10c.DOWs%>%filter(year>1994 & year<2016)%>% group_by(dowlknum)%>% summarise(
  avg.ann.gdd=mean(gdd_wtr_10c)
)
dim(IPSL.GDD10c.curr)
IPSL.GDD10c.curr%>%View()

MRI.temp.GDD10c.DOWs=read_csv("processed_data/MRI.temp.GDD10c.DOWs.csv")
MRI.GDD10c.curr=MRI.temp.GDD10c.DOWs%>%filter(year>1994 & year<2016)%>% group_by(dowlknum)%>% summarise(
  avg.ann.gdd=mean(gdd_wtr_10c)
)
dim(MRI.GDD10c.curr)
MRI.GDD10c.curr%>%View()

### Changing column names to match the EWM dataset
colnames(ACCESS.GDD10c.curr)[2]="ACCESS.avg.ann.gdd"
colnames(ACCESS.GDD10c.curr)[1]="DOWLKNUM"
ACCESS.GDD10c.curr

EWMprsabs.data_updated =merge(EWM.data,ACCESS.GDD10c.curr, by="DOWLKNUM")
head(EWMprsabs.data_updated)

colnames(MIROC5.GDD10c.curr)[2]="MIROC5.avg.ann.gdd"
colnames(MIROC5.GDD10c.curr)[1]="DOWLKNUM"
MIROC5.GDD10c.curr

EWMprsabs.data_updated =merge(EWMprsabs.data_updated,MIROC5.GDD10c.curr, by="DOWLKNUM")
head(EWMprsabs.data_updated)

colnames(IPSL.GDD10c.curr)[2]="IPSL.avg.ann.gdd"
colnames(IPSL.GDD10c.curr)[1]="DOWLKNUM"
IPSL.GDD10c.curr

EWMprsabs.data_updated =merge(EWMprsabs.data_updated,IPSL.GDD10c.curr, by="DOWLKNUM")
head(EWMprsabs.data_updated)

colnames(GFDL.GDD10c.curr)[2]="GFDL.avg.ann.gdd"
colnames(GFDL.GDD10c.curr)[1]="DOWLKNUM"
GFDL.GDD10c.curr

EWMprsabs.data_updated =merge(EWMprsabs.data_updated,GFDL.GDD10c.curr, by="DOWLKNUM")
head(EWMprsabs.data_updated)

colnames(MRI.GDD10c.curr)[2]="MRI.avg.ann.gdd"
colnames(MRI.GDD10c.curr)[1]="DOWLKNUM"
MRI.GDD10c.curr

EWMprsabs.data_updated =merge(EWMprsabs.data_updated,MRI.GDD10c.curr, by="DOWLKNUM")
head(EWMprsabs.data_updated)

write_csv(EWMprsabs.data_updated, "processed_data/EWM.prsabs95to15_AllGCMs_v2.csv") ## the second version with only three covariates- GDD, Secchi
                                                                                    ## and roads


write_csv(EWMprsabs.data_final.nona, "processed_data/EWM.prsabs95to15_AllGCMs.csv") ## the first version with fewer lakes, 
                                                                                    ## because it included other covariates

### A quick plot of all the lake GDD measures from 5 different GCMs
plot(EWMprsabs.data_updated$LAT, EWMprsabs.data_updated$mean.gdd_wtr_10c, col="gray", pch=16, xlab="Latitiude", ylab="Ann. GDD@10c")
points(EWMprsabs.data_updated$LAT, EWMprsabs.data_updated$IPSL.avg.ann.gdd, col="pink")
points(EWMprsabs.data_updated$LAT, EWMprsabs.data_updated$GFDL.avg.ann.gdd, col="dark green")
points(EWMprsabs.data_updated$LAT, EWMprsabs.data_updated$ACCESS.avg.ann.gdd, col="red")
points(EWMprsabs.data_updated$LAT, EWMprsabs.data_updated$MRI.avg.ann.gdd, col="orange")
points(EWMprsabs.data_updated$LAT, EWMprsabs.data_updated$MIROC5.avg.ann.gdd, col="light blue")


