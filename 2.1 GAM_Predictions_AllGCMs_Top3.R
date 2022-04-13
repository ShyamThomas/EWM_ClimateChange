library(tidyverse)
library(miceadds)
library(ggplot2)
library(mgcv)
library(reshape2)


###################### FIRST WITH GAM with K = 3, SIMPLE GAM
### List all RF models
GAM_k3_models = list.files(path ="processed_data/TrainData/", pattern = "*k3.Rdata$")
GAM_k3_models

### List all test data files (i.e future water temperature)
TestDataFiles = list.files(path ="processed_data/TestData/ForecastData", pattern = "*.csv$")
TestDataFiles

new.data=list()

### Loop all test data files to make a new data file with just the three predictors
for (TestData in TestDataFiles){
  Test = read.csv(paste("processed_data/TestData/ForecastData/",TestData, sep=""))
  Test.preds=Test[,c(5,11,12)]
  colnames(Test.preds)[3]=paste(str_sub(TestData, 19,-13),".avg.ann.gdd", sep="")
  new.data[[TestData]]=Test.preds
}

fut.predictions=list()

for (i in 1:5){
  load.Rdata(paste("processed_data/TrainData/",GAM_k3_models[i], sep=""), "train.model")
  fut.predictions[[i]]=predict.gam(train.model, newdata = new.data[[i]], type = "response")
}
head(fut.predictions)

fut.preds.df=as.data.frame(do.call(cbind,fut.predictions))
head(fut.preds.df)
tail(fut.preds.df)

#################################################################################################
#### Repeat the above codes with Current temperature(i.e. the training data)
TrainDataFiles=list.files(path ="processed_data/TrainData", pattern = "*.csv$")
TrainDataFiles

new.data=list()

for (TrainData in TrainDataFiles){
  Train = read.csv(paste("processed_data/TrainData/",TrainData, sep=""))
  Train.preds=Train[,c(5,11,12)]
  new.data[[TrainData]]=Train.preds
}

curr.predictions=list()

for (i in 1:5){
  load.Rdata(paste("processed_data/TrainData/",GAM_k3_models[i], sep=""), "train.model")
  curr.predictions[[i]]=predict(train.model, newdata = new.data[[i]],type = "response")
}
head(curr.predictions)
curr.preds.df=as.data.frame(do.call(cbind,curr.predictions))
head(curr.preds.df)
tail(curr.preds.df)

##########################################################################################
ModelNames=c("ACCESS","GFDL","IPSL","MIROC5","MRI")
ModelNames
colnames(fut.preds.df)=ModelNames
colnames(fut.preds.df)
colnames(curr.preds.df)=ModelNames
colnames(curr.preds.df)

fut.preds.df$DOWLKNUM=Test$DOWLKNUM
curr.preds.df$DOWLKNUM=EWM.GCMs.data$DOWLKNUM
write_csv(fut.preds.df, "Results/GAM.k3_Fut.Predictions.csv")
write_csv(curr.preds.df, "Results/GAM.k3_Curr.Predictions.csv")

curr.preds.df$Period=rep("Current",468)
fut.preds.df$Period=rep("Future",467)
currANDfut_preds=bind_rows(curr.preds.df,fut.preds.df)
currANDfut_preds.melt=melt(currANDfut_preds)
head(currANDfut_preds.melt)

InvasionRisk_Plot_GAM_k3=ggplot(currANDfut_preds.melt, aes(x=variable, y=value, fill=Period))+geom_boxplot()+ylab("Invasion Risk")+xlab("GCMs")
ggsave("InvasionRisk_Plot_GAM.png", path="Figures/", device = "png",width = 6, height = 4.5 )

############################################################################################################################################
############################################################################################################################################
###################### SECOND WITH GAM with K = 10, COMPLEX GAM
### List all RF models
GAM_k10_models = list.files(path ="processed_data/TrainData/", pattern = "*GAM.Rdata$")
GAM_k10_models

### List all test data files (i.e future water temperature)
TestDataFiles = list.files(path ="processed_data/TestData/ForecastData", pattern = "*.csv$")
TestDataFiles

new.data=list()

### Loop all test data files to make a new data file with just the three predictors
for (TestData in TestDataFiles){
  Test = read.csv(paste("processed_data/TestData/ForecastData/",TestData, sep=""))
  Test.preds=Test[,c(5,11,12)]
  colnames(Test.preds)[3]=paste(str_sub(TestData, 19,-13),".avg.ann.gdd", sep="")
  new.data[[TestData]]=Test.preds
}

fut.predictions=list()

for (i in 1:5){
  load.Rdata(paste("processed_data/TrainData/",GAM_k10_models[i], sep=""), "train.model")
  fut.predictions[[i]]=predict.gam(train.model, newdata = new.data[[i]], type = "response")
}
head(fut.predictions)

fut.preds.df=as.data.frame(do.call(cbind,fut.predictions))
head(fut.preds.df)
tail(fut.preds.df)

############################################################################################################################################
#### Repeat the above codes with Current temperature(i.e. the training data)
TrainDataFiles=list.files(path ="processed_data/TrainData", pattern = "*.csv$")
TrainDataFiles

new.data=list()

for (TrainData in TrainDataFiles){
  Train = read.csv(paste("processed_data/TrainData/",TrainData, sep=""))
  Train.preds=Train[,c(5,11,12)]
  new.data[[TrainData]]=Train.preds
}

curr.predictions=list()

for (i in 1:5){
  load.Rdata(paste("processed_data/TrainData/",GAM_k10_models[i], sep=""), "train.model")
  curr.predictions[[i]]=predict(train.model, newdata = new.data[[i]],type = "response")
}
head(curr.predictions)
curr.preds.df=as.data.frame(do.call(cbind,curr.predictions))
head(curr.preds.df)
tail(curr.preds.df)

#############################################################################################
ModelNames=c("ACCESS","GFDL","IPSL","MIROC5","MRI")
ModelNames
colnames(fut.preds.df)=ModelNames
colnames(fut.preds.df)
colnames(curr.preds.df)=ModelNames
colnames(curr.preds.df)

fut.preds.df$DOWLKNUM=Test$DOWLKNUM
curr.preds.df$DOWLKNUM=EWM.GCMs.data$DOWLKNUM
write_csv(fut.preds.df, "Results/GAM.k10_Fut.Predictions.csv")
write_csv(curr.preds.df, "Results/GAM.k10_Curr.Predictions.csv")

curr.preds.df$Period=rep("Current",468)
fut.preds.df$Period=rep("Future",467)
currANDfut_preds_k10=bind_rows(curr.preds.df,fut.preds.df)
currANDfut_preds.melt_k10=melt(currANDfut_preds_k10)
head(currANDfut_preds.melt_k10)

InvasionRisk_Plot_GAM_k10=ggplot(currANDfut_preds.melt_k10, aes(x=variable, y=value, fill=Period))+geom_boxplot()+
  ylab("Invasion Risk")+xlab("GCMs")
ggsave("InvasionRisk_Plot_CompGAM.png", path="Figures/", device = "png",width = 6, height = 4.5 )
