library(tidyverse)
library(ggplot2)
library(miceadds)
library(randomForest)

########################### FUTURE PREDICTIONS with MANIPULATION of TOP RANKED COVARIATE - ROAD DENSITY ####################################
######################################## FOR NON-ANALOG TEMPERATURE CONDITIONS #################################################################

EWM.train.data_ACCESS.WtrTemp=read_csv("processed_data/TrainData/EWM.train.data_ACCESS.WtrTemp.csv")
EWM.train.data_ACCESS.WtrTemp
max(EWM.train.data_ACCESS.WtrTemp$ACCESS.avg.ann.gdd) ## the threshold gdd is 2367
head(curr.preds.df)
dim(curr.preds.df)

ACCESS.forecast=read_csv("processed_data/TestData/ForecastData/EWM.forecast.data_ACCESS.WtrTemp.csv")
ACCESS.forecast.DOWs=ACCESS.forecast%>%filter(avg.ann.gdd>2367)%>%select(DOWLKNUM) ### get the non-analog DOWs
curr.preds.df.non_analog=merge(curr.preds.df, ACCESS.forecast.DOWs)
head(curr.preds.df.non_analog) ### predictions for analog domain based on ACCESS gdd

Roads.quants=quantile(ACCESS.forecast$roaddensity_density_mperha,probs=seq(0,1, 0.1))
Roads.quants

ACCESS.forecast_2preds_varRoads_NonAnalog=ACCESS.forecast%>%select(avg_secchi, avg.ann.gdd)%>%filter(avg.ann.gdd>2367)
ACCESS.forecast_2preds_varRoads_NonAnalog=rename(ACCESS.forecast_2preds_varRoads_NonAnalog,  ACCESS.avg.ann.gdd =avg.ann.gdd)
preds_lst=rep(lst(ACCESS.forecast_2preds_varRoads_NonAnalog),11)
ACCESS.preds_lst_varRoadLevels=Map(cbind,preds_lst, roaddensity_density_mperha= Roads.quants)
str(ACCESS.preds_lst_varRoadLevels)

load.Rdata("processed_data/TrainData/EWM.train.data_ACCESS.WtrTempTop3_.Rdata", "ACCESS.m")

ACCESS.pred.results=list()
ACCESS.change=list()
ACCESS.negrisk=list()
for (i in 1:length(ACCESS.preds_lst_varRoadLevels)){
  ACCESS.pred.results[[i]]= predict(ACCESS.m, newdata = ACCESS.preds_lst_varRoadLevels[[i]])
  ACCESS.change[[i]]=ACCESS.pred.results[[i]]-curr.preds.df.non_analog$ACCESS
  ACCESS.negrisk[[i]]=length(which(ACCESS.change[[i]]<0))/216
}


########## MRI
EWM.train.data_MRI.WtrTemp=read_csv("processed_data/TrainData/EWM.train.data_MRI.WtrTemp.csv")
EWM.train.data_MRI.WtrTemp
max(EWM.train.data_MRI.WtrTemp$MRI.avg.ann.gdd)##2421


MRI.forecast=read_csv("processed_data/TestData/ForecastData/EWM.forecast.data_MRI.WtrTemp.csv")
MRI.forecast
MRI.forecast.DOWs=MRI.forecast%>%filter(avg.ann.gdd>2421)%>%select(DOWLKNUM)
curr.preds.df.non_analog=merge(curr.preds.df, MRI.forecast.DOWs)
head(curr.preds.df.non_analog)

MRI.forecast_2preds_varRoads_NonAnalog=MRI.forecast%>%select(avg_secchi, avg.ann.gdd)%>%filter(avg.ann.gdd>2421)
MRI.forecast_2preds_varRoads_NonAnalog=rename(MRI.forecast_2preds_varRoads_NonAnalog,  MRI.avg.ann.gdd =avg.ann.gdd)
preds_lst=rep(lst(MRI.forecast_2preds_varRoads_NonAnalog),11)
MRI.preds_lst_varRoadLevels=Map(cbind,preds_lst, roaddensity_density_mperha= Roads.quants)
str(MRI.preds_lst_varRoadLevels)

load.Rdata("processed_data/TrainData/EWM.train.data_MRI.WtrTempTop3_.Rdata", "MRI.m")
MRI.pred.results=list()
MRI.change=list()
MRI.negrisk=list()
for (i in 1:length(MRI.preds_lst_varRoadLevels)){
  MRI.pred.results[[i]]= predict(MRI.m, newdata = MRI.preds_lst_varRoadLevels[[i]])
  MRI.change[[i]]=MRI.pred.results[[i]]-curr.preds.df.non_analog$MRI
  MRI.negrisk[[i]]=length(which(MRI.change[[i]]<0))/12
}

########### GFDL
EWM.train.data_GFDL.WtrTemp=read_csv("processed_data/TrainData/EWM.train.data_GFDL.WtrTemp.csv")
EWM.train.data_GFDL.WtrTemp
max(EWM.train.data_GFDL.WtrTemp$GFDL.avg.ann.gdd)##2449

GFDL.forecast=read_csv("processed_data/TestData/ForecastData/EWM.forecast.data_GFDL.WtrTemp.csv")
GFDL.forecast.DOWs=GFDL.forecast%>%filter(avg.ann.gdd>2449)%>%select(DOWLKNUM)
curr.preds.df.non_analog=merge(curr.preds.df, GFDL.forecast.DOWs)
head(curr.preds.df.non_analog)

GFDL.forecast_2preds_varRoads.NonAnalog=GFDL.forecast%>%select(avg_secchi, avg.ann.gdd)%>%filter(avg.ann.gdd>2449)
GFDL.forecast_2preds_varRoads.NonAnalog= rename(GFDL.forecast_2preds_varRoads.NonAnalog,  GFDL.avg.ann.gdd =avg.ann.gdd)
preds_lst=rep(lst(GFDL.forecast_2preds_varRoads.NonAnalog),11)
GFDL.preds_lst_varRoadLevels=Map(cbind,preds_lst, roaddensity_density_mperha= Roads.quants)
str(GFDL.preds_lst_varRoadLevels)

load.Rdata("processed_data/TrainData/EWM.train.data_GFDL.WtrTempTop3_.Rdata", "GFDL.m")
GFDL.pred.results=list()
GFDL.change=list()
GFDL.negrisk=list()
for (i in 1:length(GFDL.preds_lst_varRoadLevels)){
  GFDL.pred.results[[i]]= predict(GFDL.m, newdata = GFDL.preds_lst_varRoadLevels[[i]])
  GFDL.change[[i]]=GFDL.pred.results[[i]]-curr.preds.df.non_analog$GFDL
  GFDL.negrisk[[i]]=length(which(GFDL.change[[i]]<0))/5
}

########### MIROC5
EWM.train.data_MIROC5.WtrTemp=read_csv("processed_data/TrainData/EWM.train.data_MIROC5.WtrTemp.csv")
EWM.train.data_MIROC5.WtrTemp
max(EWM.train.data_MIROC5.WtrTemp$MIROC5.avg.ann.gdd)

MIROC5.forecast=read_csv("processed_data/TestData/ForecastData/EWM.forecast.data_MIROC5.WtrTemp.csv")
MIROC5.forecast
MIROC5.forecast.DOWs=MIROC5.forecast%>%filter(avg.ann.gdd>2516)%>%select(DOWLKNUM)
curr.preds.df.non_analog=merge(curr.preds.df, MIROC5.forecast.DOWs)
head(curr.preds.df.non_analog)

MIROC5.forecast_2preds_varRoads.NonAnalog=MIROC5.forecast%>%select(avg_secchi, avg.ann.gdd)%>%filter(avg.ann.gdd>2516)
MIROC5.forecast_2preds_varRoads.NonAnalog= rename(MIROC5.forecast_2preds_varRoads.NonAnalog,  MIROC5.avg.ann.gdd =avg.ann.gdd)
preds_lst=rep(lst(MIROC5.forecast_2preds_varRoads.NonAnalog),11)
MIROC5.preds_lst_varRoadLevels=Map(cbind,preds_lst, roaddensity_density_mperha= Roads.quants)
str(MIROC5.preds_lst_varRoadLevels)

load.Rdata("processed_data/TrainData/EWM.train.data_MIROC5.WtrTempTop3_.Rdata", "MIROC5.m")
MIROC5.pred.results=list()
MIROC5.change=list()
MIROC5.negrisk=list()
for (i in 1:length(MIROC5.preds_lst_varRoadLevels)){
  MIROC5.pred.results[[i]]= predict(MIROC5.m, newdata = MIROC5.preds_lst_varRoadLevels[[i]])
  MIROC5.change[[i]]=MIROC5.pred.results[[i]]-curr.preds.df.non_analog$MIROC5
  MIROC5.negrisk[[i]]=length(which(MIROC5.change[[i]]<0))/200
}


########### IPSL
EWM.train.data_IPSL.WtrTemp=read_csv("processed_data/TrainData/EWM.train.data_IPSL.WtrTemp.csv")
EWM.train.data_IPSL.WtrTemp
max(EWM.train.data_IPSL.WtrTemp$IPSL.avg.ann.gdd)

IPSL.forecast=read_csv("processed_data/TestData/ForecastData/EWM.forecast.data_IPSL.WtrTemp.csv")
IPSL.forecast.DOWs=IPSL.forecast%>%filter(avg.ann.gdd>2559)%>%select(DOWLKNUM)
curr.preds.df.non_analog=merge(curr.preds.df, IPSL.forecast.DOWs)
head(curr.preds.df.non_analog)

IPSL.forecast_2preds_varRoads.NonAnalog=IPSL.forecast%>%select(avg_secchi, avg.ann.gdd)%>%filter(avg.ann.gdd>2559)
IPSL.forecast_2preds_varRoads.NonAnalog= rename(IPSL.forecast_2preds_varRoads.NonAnalog,  IPSL.avg.ann.gdd =avg.ann.gdd)
preds_lst=rep(lst(IPSL.forecast_2preds_varRoads.NonAnalog),11)
IPSL.preds_lst_varRoadLevels=Map(cbind,preds_lst, roaddensity_density_mperha= Roads.quants)
str(IPSL.preds_lst_varRoadLevels)

load.Rdata("processed_data/TrainData/EWM.train.data_IPSL.WtrTempTop3_.Rdata", "IPSL.m")
IPSL.pred.results=list()
IPSL.change=list()
IPSL.negrisk=list()
for (i in 1:length(IPSL.preds_lst_varRoadLevels)){
  IPSL.pred.results[[i]]= predict(IPSL.m, newdata = IPSL.preds_lst_varRoadLevels[[i]])
  IPSL.change[[i]]=IPSL.pred.results[[i]]-curr.preds.df.non_analog$IPSL
  IPSL.negrisk[[i]]=length(which(IPSL.change[[i]]<0))/35
}

All.Negrisk_NonAnalog=lapply(list(ACCESS.negrisk, MRI.negrisk,GFDL.negrisk, IPSL.negrisk,MIROC5.negrisk), unlist)
NegRisk.df_NonAnalog=as.data.frame(All.Negrisk_NonAnalog, col.names = c("ACCESS.negrisk", "MRI.negrisk","GFDL.negrisk", 
                                                                        "IPSL.negrisk","MIROC5.negrisk"))
NegRisk.df_NonAnalog
Road.level=seq(0,1,0.1)
NegRisk.df_NonAnalog$RoadPercentile=Road.level
write_csv(NegRisk.df_NonAnalog, "Results/NegRisk_NonAnalog.csv")

ggplot(NegRisk.df,aes(RoadPercentile,ACCESS.negrisk))+geom_point()+geom_smooth(method = "lm")
ggplot(NegRisk.df,aes(RoadPercentile,ACCESS.negrisk))+geom_point()+geom_smooth(method = "gam")


############################################################################################################################################
############################################################################################################################################
########################### FUTURE PREDICTIONS with MANIPULATION of TOP RANKED COVARIATE - SECCHI DEPTH ####################################
##### Shall use 10 levels of Secchi depth values to create 5 different predictor datasets 
##### with 5 DDISTINCT CONSTANT  levels of SECCHI DEPTH 

EWM.train.data_ACCESS.WtrTemp=read_csv("processed_data/TrainData/EWM.train.data_ACCESS.WtrTemp.csv")
EWM.train.data_ACCESS.WtrTemp
max(EWM.train.data_ACCESS.WtrTemp$ACCESS.avg.ann.gdd) ## the threshold gdd is 2367
curr.preds.df=read_csv("Results/Curr.Predictions.csv")
head(curr.preds.df)
dim(curr.preds.df)

ACCESS.forecast=read_csv("processed_data/TestData/ForecastData/EWM.forecast.data_ACCESS.WtrTemp.csv")
ACCESS.forecast.DOWs=ACCESS.forecast%>%filter(avg.ann.gdd>2367)%>%select(DOWLKNUM) ### get the non-analog DOWs
curr.preds.df.non_analog=merge(curr.preds.df, ACCESS.forecast.DOWs, by="DOWLKNUM")
head(curr.preds.df.non_analog) ### predictions for analog domain based on ACCESS gdd

Secchi.quants=quantile(ACCESS.forecast$avg_secchi,probs=seq(0,1, 0.1))
Secchi.quants

ACCESS.forecast_2preds_varSecchi_NonAnalog=ACCESS.forecast%>%select(roaddensity_density_mperha , avg.ann.gdd)%>%filter(avg.ann.gdd>2367)
ACCESS.forecast_2preds_varSecchi_NonAnalog=rename(ACCESS.forecast_2preds_varSecchi_NonAnalog,  ACCESS.avg.ann.gdd =avg.ann.gdd)
preds_lst=rep(lst(ACCESS.forecast_2preds_varSecchi_NonAnalog),11)
ACCESS.preds_lst_varSecchiLevels=Map(cbind,preds_lst, avg_secchi= Secchi.quants)
str(ACCESS.preds_lst_varSecchiLevels)

load.Rdata("processed_data/TrainData/EWM.train.data_ACCESS.WtrTempTop3_.Rdata", "ACCESS.m")

ACCESS.pred.results=list()
ACCESS.change=list()
ACCESS.negrisk=list()
for (i in 1:length(ACCESS.preds_lst_varSecchiLevels)){
  ACCESS.pred.results[[i]]= predict(ACCESS.m, newdata = ACCESS.preds_lst_varSecchiLevels[[i]])
  ACCESS.change[[i]]=ACCESS.pred.results[[i]]-curr.preds.df.non_analog$ACCESS
  ACCESS.negrisk[[i]]=length(which(ACCESS.change[[i]]<0))/216
}

########## MRI
EWM.train.data_MRI.WtrTemp=read_csv("processed_data/TrainData/EWM.train.data_MRI.WtrTemp.csv")
EWM.train.data_MRI.WtrTemp
max(EWM.train.data_MRI.WtrTemp$MRI.avg.ann.gdd)##2421


MRI.forecast=read_csv("processed_data/TestData/ForecastData/EWM.forecast.data_MRI.WtrTemp.csv")
MRI.forecast
MRI.forecast.DOWs=MRI.forecast%>%filter(avg.ann.gdd>2421)%>%select(DOWLKNUM)
curr.preds.df.non_analog=merge(curr.preds.df, MRI.forecast.DOWs, by="DOWLKNUM")
head(curr.preds.df.non_analog)

MRI.forecast_2preds_varSecchi_NonAnalog=MRI.forecast%>%select(roaddensity_density_mperha, avg.ann.gdd)%>%filter(avg.ann.gdd>2421)
MRI.forecast_2preds_varSecchi_NonAnalog=rename(MRI.forecast_2preds_varSecchi_NonAnalog,  MRI.avg.ann.gdd =avg.ann.gdd)
preds_lst=rep(lst(MRI.forecast_2preds_varSecchi_NonAnalog),11)
MRI.preds_lst_varSecchiLevels=Map(cbind,preds_lst, avg_secchi= Secchi.quants)
str(MRI.preds_lst_varSecchiLevels)

load.Rdata("processed_data/TrainData/EWM.train.data_MRI.WtrTempTop3_.Rdata", "MRI.m")
MRI.pred.results=list()
MRI.change=list()
MRI.negrisk=list()
for (i in 1:length(MRI.preds_lst_varSecchiLevels)){
  MRI.pred.results[[i]]= predict(MRI.m, newdata = MRI.preds_lst_varSecchiLevels[[i]])
  MRI.change[[i]]=MRI.pred.results[[i]]-curr.preds.df.non_analog$MRI
  MRI.negrisk[[i]]=length(which(MRI.change[[i]]<0))/12
}

########### GFDL
EWM.train.data_GFDL.WtrTemp=read_csv("processed_data/TrainData/EWM.train.data_GFDL.WtrTemp.csv")
EWM.train.data_GFDL.WtrTemp
max(EWM.train.data_GFDL.WtrTemp$GFDL.avg.ann.gdd)##2449

GFDL.forecast=read_csv("processed_data/TestData/ForecastData/EWM.forecast.data_GFDL.WtrTemp.csv")
GFDL.forecast.DOWs=GFDL.forecast%>%filter(avg.ann.gdd>2449)%>%select(DOWLKNUM)
curr.preds.df.non_analog=merge(curr.preds.df, GFDL.forecast.DOWs, by="DOWLKNUM")
head(curr.preds.df.non_analog)

GFDL.forecast_2preds_varSecchi.NonAnalog=GFDL.forecast%>%select(roaddensity_density_mperha, avg.ann.gdd)%>%filter(avg.ann.gdd>2449)
GFDL.forecast_2preds_varSecchi.NonAnalog= rename(GFDL.forecast_2preds_varSecchi.NonAnalog,  GFDL.avg.ann.gdd =avg.ann.gdd)
preds_lst=rep(lst(GFDL.forecast_2preds_varSecchi.NonAnalog),11)
GFDL.preds_lst_varSecchiLevels=Map(cbind,preds_lst, avg_secchi= Secchi.quants)
str(GFDL.preds_lst_varSecchiLevels)

load.Rdata("processed_data/TrainData/EWM.train.data_GFDL.WtrTempTop3_.Rdata", "GFDL.m")
GFDL.pred.results=list()
GFDL.change=list()
GFDL.negrisk=list()
for (i in 1:length(GFDL.preds_lst_varSecchiLevels)){
  GFDL.pred.results[[i]]= predict(GFDL.m, newdata = GFDL.preds_lst_varSecchiLevels[[i]])
  GFDL.change[[i]]=GFDL.pred.results[[i]]-curr.preds.df.non_analog$GFDL
  GFDL.negrisk[[i]]=length(which(GFDL.change[[i]]<0))/5
}

########### MIROC5
EWM.train.data_MIROC5.WtrTemp=read_csv("processed_data/TrainData/EWM.train.data_MIROC5.WtrTemp.csv")
EWM.train.data_MIROC5.WtrTemp
max(EWM.train.data_MIROC5.WtrTemp$MIROC5.avg.ann.gdd)

MIROC5.forecast=read_csv("processed_data/TestData/ForecastData/EWM.forecast.data_MIROC5.WtrTemp.csv")
MIROC5.forecast
MIROC5.forecast.DOWs=MIROC5.forecast%>%filter(avg.ann.gdd>2516)%>%select(DOWLKNUM)
curr.preds.df.non_analog=merge(curr.preds.df, MIROC5.forecast.DOWs, by="DOWLKNUM")
head(curr.preds.df.non_analog)

MIROC5.forecast_2preds_varSecchi.NonAnalog=MIROC5.forecast%>%select(roaddensity_density_mperha, avg.ann.gdd)%>%filter(avg.ann.gdd>2516)
MIROC5.forecast_2preds_varSecchi.NonAnalog= rename(MIROC5.forecast_2preds_varSecchi.NonAnalog,  MIROC5.avg.ann.gdd =avg.ann.gdd)
preds_lst=rep(lst(MIROC5.forecast_2preds_varSecchi.NonAnalog),11)
MIROC5.preds_lst_varSecchiLevels=Map(cbind,preds_lst,  avg_secchi= Secchi.quants)
str(MIROC5.preds_lst_varSecchiLevels)

load.Rdata("processed_data/TrainData/EWM.train.data_MIROC5.WtrTempTop3_.Rdata", "MIROC5.m")
MIROC5.pred.results=list()
MIROC5.change=list()
MIROC5.negrisk=list()
for (i in 1:length(MIROC5.preds_lst_varSecchiLevels)){
  MIROC5.pred.results[[i]]= predict(MIROC5.m, newdata = MIROC5.preds_lst_varSecchiLevels[[i]])
  MIROC5.change[[i]]=MIROC5.pred.results[[i]]-curr.preds.df.non_analog$MIROC5
  MIROC5.negrisk[[i]]=length(which(MIROC5.change[[i]]<0))/200
}


########### IPSL
EWM.train.data_IPSL.WtrTemp=read_csv("processed_data/TrainData/EWM.train.data_IPSL.WtrTemp.csv")
EWM.train.data_IPSL.WtrTemp
max(EWM.train.data_IPSL.WtrTemp$IPSL.avg.ann.gdd)

IPSL.forecast=read_csv("processed_data/TestData/ForecastData/EWM.forecast.data_IPSL.WtrTemp.csv")
IPSL.forecast.DOWs=IPSL.forecast%>%filter(avg.ann.gdd>2559)%>%select(DOWLKNUM)
curr.preds.df.non_analog=merge(curr.preds.df, IPSL.forecast.DOWs, by="DOWLKNUM")
head(curr.preds.df.non_analog)

IPSL.forecast_2preds_varSecchi.NonAnalog=IPSL.forecast%>%select(roaddensity_density_mperha, avg.ann.gdd)%>%filter(avg.ann.gdd>2559)
IPSL.forecast_2preds_varSecchi.NonAnalog= rename(IPSL.forecast_2preds_varSecchi.NonAnalog,  IPSL.avg.ann.gdd =avg.ann.gdd)
preds_lst=rep(lst(IPSL.forecast_2preds_varSecchi.NonAnalog),11)
IPSL.preds_lst_varSecchiLevels=Map(cbind,preds_lst,  avg_secchi= Secchi.quants)
str(IPSL.preds_lst_varSecchiLevels)

load.Rdata("processed_data/TrainData/EWM.train.data_IPSL.WtrTempTop3_.Rdata", "IPSL.m")
IPSL.pred.results=list()
IPSL.change=list()
IPSL.negrisk=list()
for (i in 1:length(IPSL.preds_lst_varSecchiLevels)){
  IPSL.pred.results[[i]]= predict(IPSL.m, newdata = IPSL.preds_lst_varSecchiLevels[[i]])
  IPSL.change[[i]]=IPSL.pred.results[[i]]-curr.preds.df.non_analog$IPSL
  IPSL.negrisk[[i]]=length(which(IPSL.change[[i]]<0))/35
}

#### Combine all the results
All.Negrisk_NonAnalog_Secchi=lapply(list(ACCESS.negrisk, MRI.negrisk,GFDL.negrisk, IPSL.negrisk,MIROC5.negrisk), unlist)
NegRisk.df_NonAnalog_Secchi=as.data.frame(All.Negrisk_NonAnalog_Secchi, col.names = c("ACCESS.negrisk", "MRI.negrisk","GFDL.negrisk", 
                                                                        "IPSL.negrisk","MIROC5.negrisk"))
NegRisk.df_NonAnalog_Secchi
Secchi.level=seq(0,1,0.1)
NegRisk.df_NonAnalog_Secchi$SecchiPercentile=Secchi.level
write_csv(NegRisk.df_NonAnalog_Secchi, "Results/Secchi_NegRisk_NonAnalog.csv")

ggplot(NegRisk.df_NonAnalog_Secchi,aes(SecchiPercentile,ACCESS.negrisk))+geom_point()+geom_smooth(method = "lm")
ggplot(NegRisk.df_NonAnalog_Secchi,aes(SecchiPercentile,ACCESS.negrisk))+geom_point()+geom_smooth(method = "gam")



