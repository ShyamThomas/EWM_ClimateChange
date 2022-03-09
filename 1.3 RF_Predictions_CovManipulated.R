library(tidyverse)
library(reshape2)


########################### FUTURE PREDICTIONS with MANIPULATION of TOP RANKED COVARIATE - ROAD DENSITY ####################################
##### Shall use five levels of Road density values to create 5 different predictor datasets 
##### with 5 DDISTINCT CONSTANT  levels of road density

RFmodels = list.files(path ="processed_data/TrainData/", pattern = "*Top3_.Rdata$")
RFmodels 

### List all test data files (i.e future water temperature)
TestDataFiles = list.files(path ="processed_data/TestData/ForecastData", pattern = "*.csv$")
TestDataFiles

ACCESS.forecast=read_csv("processed_data/TestData/ForecastData/EWM.forecast.data_ACCESS.WtrTemp.csv")

#Roads.quants=quantile(ACCESS.forecast$roaddensity_density_mperha,probs=seq(0,1, 0.1))
Roads.quants

ACCESS.forecast_2preds_varRoads=ACCESS.forecast%>%select(avg_secchi, avg.ann.gdd)
ACCESS.forecast_2preds_varRoads=rename(ACCESS.forecast_2preds_varRoads,  ACCESS.avg.ann.gdd =avg.ann.gdd)
preds_lst=rep(lst(ACCESS.forecast_2preds_varRoads),11)
ACCESS.preds_lst_varRoadLevels=Map(cbind,preds_lst, roaddensity_density_mperha= Roads.quants)
str(ACCESS.preds_lst_varRoadLevels)

load.Rdata("processed_data/TrainData/EWM.train.data_ACCESS.WtrTempTop3_.Rdata", "ACCESS.m")

ACCESS.pred.results=list()
ACCESS.change=list()
ACCESS.negrisk=list()
for (i in 1:length(ACCESS.preds_lst_varRoadLevels)){
  ACCESS.pred.results[[i]]= predict(ACCESS.m, newdata = ACCESS.preds_lst_varRoadLevels[[i]])
  ACCESS.change[[i]]=ACCESS.pred.results[[i]]-curr.preds.df.new$ACCESS
  ACCESS.negrisk[[i]]=length(which(ACCESS.change[[i]]<0))/467
}


########## MRI
MRI.forecast=read_csv("processed_data/TestData/ForecastData/EWM.forecast.data_MRI.WtrTemp.csv")
MRI.forecast_2preds_varRoads=MRI.forecast%>%select(avg_secchi, avg.ann.gdd)%>%
  rename(MRI.forecast_2preds_varRoads,  MRI.avg.ann.gdd =avg.ann.gdd)
preds_lst=rep(lst(MRI.forecast_2preds_varRoads),11)
MRI.preds_lst_varRoadLevels=Map(cbind,preds_lst, roaddensity_density_mperha= Roads.quants)
str(MRI.preds_lst_varRoadLevels)

load.Rdata("processed_data/TrainData/EWM.train.data_MRI.WtrTempTop3_.Rdata", "MRI.m")
MRI.pred.results=list()
MRI.change=list()
MRI.negrisk=list()
for (i in 1:length(MRI.preds_lst_varRoadLevels)){
  MRI.pred.results[[i]]= predict(MRI.m, newdata = MRI.preds_lst_varRoadLevels[[i]])
  MRI.change[[i]]=MRI.pred.results[[i]]-curr.preds.df.new$MRI
  MRI.negrisk[[i]]=length(which(MRI.change[[i]]<0))/467
}

########### GFDL
GFDL.forecast=read_csv("processed_data/TestData/ForecastData/EWM.forecast.data_GFDL.WtrTemp.csv")
GFDL.forecast_2preds_varRoads=GFDL.forecast%>%select(avg_secchi, avg.ann.gdd)
  GFDL.forecast_2preds_varRoads= rename(GFDL.forecast_2preds_varRoads,  GFDL.avg.ann.gdd =avg.ann.gdd)
    preds_lst=rep(lst(GFDL.forecast_2preds_varRoads),11)
    GFDL.preds_lst_varRoadLevels=Map(cbind,preds_lst, roaddensity_density_mperha= Roads.quants)
    str(GFDL.preds_lst_varRoadLevels)

load.Rdata("processed_data/TrainData/EWM.train.data_GFDL.WtrTempTop3_.Rdata", "GFDL.m")
GFDL.pred.results=list()
GFDL.change=list()
GFDL.negrisk=list()
for (i in 1:length(GFDL.preds_lst_varRoadLevels)){
  GFDL.pred.results[[i]]= predict(GFDL.m, newdata = GFDL.preds_lst_varRoadLevels[[i]])
  GFDL.change[[i]]=GFDL.pred.results[[i]]-curr.preds.df.new$GFDL
  GFDL.negrisk[[i]]=length(which(GFDL.change[[i]]<0))/467
}

########### MIROC5
MIROC5.forecast=read_csv("processed_data/TestData/ForecastData/EWM.forecast.data_MIROC5.WtrTemp.csv")
MIROC5.forecast_2preds_varRoads=MIROC5.forecast%>%select(avg_secchi, avg.ann.gdd)
MIROC5.forecast_2preds_varRoads= rename(MIROC5.forecast_2preds_varRoads,  MIROC5.avg.ann.gdd =avg.ann.gdd)
preds_lst=rep(lst(MIROC5.forecast_2preds_varRoads),11)
MIROC5.preds_lst_varRoadLevels=Map(cbind,preds_lst, roaddensity_density_mperha= Roads.quants)
str(MIROC5.preds_lst_varRoadLevels)

load.Rdata("processed_data/TrainData/EWM.train.data_MIROC5.WtrTempTop3_.Rdata", "MIROC5.m")
MIROC5.pred.results=list()
MIROC5.change=list()
MIROC5.negrisk=list()
for (i in 1:length(MIROC5.preds_lst_varRoadLevels)){
  MIROC5.pred.results[[i]]= predict(MIROC5.m, newdata = MIROC5.preds_lst_varRoadLevels[[i]])
  MIROC5.change[[i]]=MIROC5.pred.results[[i]]-curr.preds.df.new$MIROC5
  MIROC5.negrisk[[i]]=length(which(MIROC5.change[[i]]<0))/467
}



IPSL.forecast=read_csv("processed_data/TestData/ForecastData/EWM.forecast.data_IPSL.WtrTemp.csv")
IPSL.forecast_2preds_varRoads=IPSL.forecast%>%select(avg_secchi, avg.ann.gdd)
IPSL.forecast_2preds_varRoads= rename(IPSL.forecast_2preds_varRoads,  IPSL.avg.ann.gdd =avg.ann.gdd)
preds_lst=rep(lst(IPSL.forecast_2preds_varRoads),11)
IPSL.preds_lst_varRoadLevels=Map(cbind,preds_lst, roaddensity_density_mperha= Roads.quants)
str(IPSL.preds_lst_varRoadLevels)

load.Rdata("processed_data/TrainData/EWM.train.data_IPSL.WtrTempTop3_.Rdata", "IPSL.m")
IPSL.pred.results=list()
IPSL.change=list()
IPSL.negrisk=list()
for (i in 1:length(IPSL.preds_lst_varRoadLevels)){
  IPSL.pred.results[[i]]= predict(IPSL.m, newdata = IPSL.preds_lst_varRoadLevels[[i]])
  IPSL.change[[i]]=IPSL.pred.results[[i]]-curr.preds.df.new$IPSL
  IPSL.negrisk[[i]]=length(which(IPSL.change[[i]]<0))/467
}
All.Negrisk=lapply(list(ACCESS.negrisk, MRI.negrisk,GFDL.negrisk, IPSL.negrisk,MIROC5.negrisk), unlist)
NegRisk.df=as.data.frame(All.Negrisk, col.names = c("ACCESS.negrisk", "MRI.negrisk","GFDL.negrisk", "IPSL.negrisk","MIROC5.negrisk"))
NegRisk.df
Road.level=seq(0,1,0.1)
NegRisk.df$RoadPercentile=Road.level
ggplot(NegRisk.df,aes(RoadPercentile,ACCESS.negrisk))+geom_point()+geom_smooth(method = "lm")
ggplot(NegRisk.df,aes(RoadPercentile,ACCESS.negrisk))+geom_point()+geom_smooth(method = "gam")









