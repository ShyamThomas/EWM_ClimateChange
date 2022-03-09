library(tidyverse)
library(ggplot2)

########################### FUTURE PREDICTIONS with MANIPULATION of TOP RANKED COVARIATE - ROAD DENSITY ####################################
######################################## FOR ANALOG TEMPERATURE CONDITIONS #################################################################

EWM.train.data_ACCESS.WtrTemp=read_csv("processed_data/TrainData/EWM.train.data_ACCESS.WtrTemp.csv")
EWM.train.data_ACCESS.WtrTemp
max(EWM.train.data_ACCESS.WtrTemp$ACCESS.avg.ann.gdd) ## the threshold gdd is 2367
head(curr.preds.df)


ACCESS.forecast=read_csv("processed_data/TestData/ForecastData/EWM.forecast.data_ACCESS.WtrTemp.csv")
ACCESS.forecast.DOWs=ACCESS.forecast%>%filter(avg.ann.gdd<2367)%>%select(DOWLKNUM) ### get the analog DOWs
curr.preds.df.analog=merge(curr.preds.df, ACCESS.forecast.DOWs)
head(curr.preds.df.analog) ### predictions for analog domain based on ACCESS gdd

Roads.quants=quantile(ACCESS.forecast$roaddensity_density_mperha,probs=seq(0,1, 0.1))
Roads.quants

ACCESS.forecast_2preds_varRoads_Analog=ACCESS.forecast%>%select(avg_secchi, avg.ann.gdd)%>%filter(avg.ann.gdd<2367)
ACCESS.forecast_2preds_varRoads_Analog=rename(ACCESS.forecast_2preds_varRoads_Analog,  ACCESS.avg.ann.gdd =avg.ann.gdd)
preds_lst=rep(lst(ACCESS.forecast_2preds_varRoads_Analog),11)
ACCESS.preds_lst_varRoadLevels=Map(cbind,preds_lst, roaddensity_density_mperha= Roads.quants)
str(ACCESS.preds_lst_varRoadLevels)

load.Rdata("processed_data/TrainData/EWM.train.data_ACCESS.WtrTempTop3_.Rdata", "ACCESS.m")

ACCESS.pred.results=list()
ACCESS.change=list()
ACCESS.negrisk=list()
for (i in 1:length(ACCESS.preds_lst_varRoadLevels)){
  ACCESS.pred.results[[i]]= predict(ACCESS.m, newdata = ACCESS.preds_lst_varRoadLevels[[i]])
  ACCESS.change[[i]]=ACCESS.pred.results[[i]]-curr.preds.df.analog$ACCESS
  ACCESS.negrisk[[i]]=length(which(ACCESS.change[[i]]<0))/251
}


########## MRI
EWM.train.data_MRI.WtrTemp=read_csv("processed_data/TrainData/EWM.train.data_MRI.WtrTemp.csv")
EWM.train.data_MRI.WtrTemp
max(EWM.train.data_MRI.WtrTemp$MRI.avg.ann.gdd)##2421


MRI.forecast=read_csv("processed_data/TestData/ForecastData/EWM.forecast.data_MRI.WtrTemp.csv")
MRI.forecast
MRI.forecast.DOWs=MRI.forecast%>%filter(avg.ann.gdd<2421)%>%select(DOWLKNUM)
curr.preds.df.analog=merge(curr.preds.df, MRI.forecast.DOWs)
head(curr.preds.df.analog)

MRI.forecast_2preds_varRoads_Analog=MRI.forecast%>%select(avg_secchi, avg.ann.gdd)%>%filter(avg.ann.gdd<2421)
MRI.forecast_2preds_varRoads_Analog=rename(MRI.forecast_2preds_varRoads_Analog,  MRI.avg.ann.gdd =avg.ann.gdd)
preds_lst=rep(lst(MRI.forecast_2preds_varRoads_Analog),11)
MRI.preds_lst_varRoadLevels=Map(cbind,preds_lst, roaddensity_density_mperha= Roads.quants)
str(MRI.preds_lst_varRoadLevels)

load.Rdata("processed_data/TrainData/EWM.train.data_MRI.WtrTempTop3_.Rdata", "MRI.m")
MRI.pred.results=list()
MRI.change=list()
MRI.negrisk=list()
for (i in 1:length(MRI.preds_lst_varRoadLevels)){
  MRI.pred.results[[i]]= predict(MRI.m, newdata = MRI.preds_lst_varRoadLevels[[i]])
  MRI.change[[i]]=MRI.pred.results[[i]]-curr.preds.df.analog$MRI
  MRI.negrisk[[i]]=length(which(MRI.change[[i]]<0))/455
}

########### GFDL
EWM.train.data_GFDL.WtrTemp=read_csv("processed_data/TrainData/EWM.train.data_GFDL.WtrTemp.csv")
EWM.train.data_GFDL.WtrTemp
max(EWM.train.data_GFDL.WtrTemp$GFDL.avg.ann.gdd)##2449

GFDL.forecast=read_csv("processed_data/TestData/ForecastData/EWM.forecast.data_GFDL.WtrTemp.csv")
GFDL.forecast.DOWs=GFDL.forecast%>%filter(avg.ann.gdd<2449)%>%select(DOWLKNUM)
curr.preds.df.analog=merge(curr.preds.df, GFDL.forecast.DOWs)
head(curr.preds.df.analog)

GFDL.forecast_2preds_varRoads.Analog=GFDL.forecast%>%select(avg_secchi, avg.ann.gdd)%>%filter(avg.ann.gdd<2449)
GFDL.forecast_2preds_varRoads.Analog= rename(GFDL.forecast_2preds_varRoads.Analog,  GFDL.avg.ann.gdd =avg.ann.gdd)
preds_lst=rep(lst(GFDL.forecast_2preds_varRoads.Analog),11)
GFDL.preds_lst_varRoadLevels=Map(cbind,preds_lst, roaddensity_density_mperha= Roads.quants)
str(GFDL.preds_lst_varRoadLevels)

load.Rdata("processed_data/TrainData/EWM.train.data_GFDL.WtrTempTop3_.Rdata", "GFDL.m")
GFDL.pred.results=list()
GFDL.change=list()
GFDL.negrisk=list()
for (i in 1:length(GFDL.preds_lst_varRoadLevels)){
  GFDL.pred.results[[i]]= predict(GFDL.m, newdata = GFDL.preds_lst_varRoadLevels[[i]])
  GFDL.change[[i]]=GFDL.pred.results[[i]]-curr.preds.df.analog$GFDL
  GFDL.negrisk[[i]]=length(which(GFDL.change[[i]]<0))/462
}

########### MIROC5
EWM.train.data_MIROC5.WtrTemp=read_csv("processed_data/TrainData/EWM.train.data_MIROC5.WtrTemp.csv")
EWM.train.data_MIROC5.WtrTemp
max(EWM.train.data_MIROC5.WtrTemp$MIROC5.avg.ann.gdd)

MIROC5.forecast=read_csv("processed_data/TestData/ForecastData/EWM.forecast.data_MIROC5.WtrTemp.csv")
MIROC5.forecast
MIROC5.forecast.DOWs=MIROC5.forecast%>%filter(avg.ann.gdd<2516)%>%select(DOWLKNUM)
curr.preds.df.analog=merge(curr.preds.df, MIROC5.forecast.DOWs)
head(curr.preds.df.analog)

MIROC5.forecast_2preds_varRoads.Analog=MIROC5.forecast%>%select(avg_secchi, avg.ann.gdd)%>%filter(avg.ann.gdd<2516)
MIROC5.forecast_2preds_varRoads.Analog= rename(MIROC5.forecast_2preds_varRoads.Analog,  MIROC5.avg.ann.gdd =avg.ann.gdd)
preds_lst=rep(lst(MIROC5.forecast_2preds_varRoads.Analog),11)
MIROC5.preds_lst_varRoadLevels=Map(cbind,preds_lst, roaddensity_density_mperha= Roads.quants)
str(MIROC5.preds_lst_varRoadLevels)

load.Rdata("processed_data/TrainData/EWM.train.data_MIROC5.WtrTempTop3_.Rdata", "MIROC5.m")
MIROC5.pred.results=list()
MIROC5.change=list()
MIROC5.negrisk=list()
for (i in 1:length(MIROC5.preds_lst_varRoadLevels)){
  MIROC5.pred.results[[i]]= predict(MIROC5.m, newdata = MIROC5.preds_lst_varRoadLevels[[i]])
  MIROC5.change[[i]]=MIROC5.pred.results[[i]]-curr.preds.df.analog$MIROC5
  MIROC5.negrisk[[i]]=length(which(MIROC5.change[[i]]<0))/267
}


########### IPSL
EWM.train.data_IPSL.WtrTemp=read_csv("processed_data/TrainData/EWM.train.data_IPSL.WtrTemp.csv")
EWM.train.data_IPSL.WtrTemp
max(EWM.train.data_IPSL.WtrTemp$IPSL.avg.ann.gdd)

IPSL.forecast=read_csv("processed_data/TestData/ForecastData/EWM.forecast.data_IPSL.WtrTemp.csv")
IPSL.forecast.DOWs=IPSL.forecast%>%filter(avg.ann.gdd<2559)%>%select(DOWLKNUM)
curr.preds.df.analog=merge(curr.preds.df, IPSL.forecast.DOWs)
head(curr.preds.df.analog)

IPSL.forecast_2preds_varRoads.Analog=IPSL.forecast%>%select(avg_secchi, avg.ann.gdd)%>%filter(avg.ann.gdd<2559)
IPSL.forecast_2preds_varRoads.Analog= rename(IPSL.forecast_2preds_varRoads.Analog,  IPSL.avg.ann.gdd =avg.ann.gdd)
preds_lst=rep(lst(IPSL.forecast_2preds_varRoads.Analog),11)
IPSL.preds_lst_varRoadLevels=Map(cbind,preds_lst, roaddensity_density_mperha= Roads.quants)
str(IPSL.preds_lst_varRoadLevels)

load.Rdata("processed_data/TrainData/EWM.train.data_IPSL.WtrTempTop3_.Rdata", "IPSL.m")
IPSL.pred.results=list()
IPSL.change=list()
IPSL.negrisk=list()
for (i in 1:length(IPSL.preds_lst_varRoadLevels)){
  IPSL.pred.results[[i]]= predict(IPSL.m, newdata = IPSL.preds_lst_varRoadLevels[[i]])
  IPSL.change[[i]]=IPSL.pred.results[[i]]-curr.preds.df.analog$IPSL
  IPSL.negrisk[[i]]=length(which(IPSL.change[[i]]<0))/432
}

All.Negrisk=lapply(list(ACCESS.negrisk, MRI.negrisk,GFDL.negrisk, IPSL.negrisk,MIROC5.negrisk), unlist)
NegRisk.df_Analog=as.data.frame(All.Negrisk, col.names = c("ACCESS.negrisk", "MRI.negrisk","GFDL.negrisk", "IPSL.negrisk","MIROC5.negrisk"))
NegRisk.df_Analog
Road.level=seq(0,1,0.1)
NegRisk.df_Analog$RoadPercentile=Road.level
write_csv(NegRisk.df_Analog, "Results/NegRisk_Analog.csv")

ggplot(NegRisk.df,aes(RoadPercentile,ACCESS.negrisk))+geom_point()+geom_smooth(method = "lm")
ggplot(NegRisk.df,aes(RoadPercentile,ACCESS.negrisk))+geom_point()+geom_smooth(method = "gam")
