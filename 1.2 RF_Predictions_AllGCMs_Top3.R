library(tidyverse)
library(miceadds)

RFmodels = list.files(path ="processed_data/TrainData/", pattern = "*Top3_.Rdata$")
RFmodels

TestDataFiles = list.files(path ="processed_data/TestData/ForecastData", pattern = "*.csv$")
TestDataFiles

new.data=list()

for (TestData in TestDataFiles){
  Test = read.csv(paste("processed_data/TestData/ForecastData/",TestData, sep=""))
  Test.preds=Test[,c(5,11,12)]
  colnames(Test.preds)[3]=paste(str_sub(TestData, 19,-13),".avg.ann.gdd", sep="")
  new.data[[TestData]]=Test.preds
}

fut.predictions=list()

for (i in 1:5){
  load.Rdata(paste("processed_data/TrainData/",RFmodels[i], sep=""), "train.model")
  fut.predictions[[i]]=predict(train.model, newdata = new.data[[i]])
}

fut.predictions
fut.preds.df=as.data.frame(do.call(cbind,fut.predictions))

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
  load.Rdata(paste("processed_data/TrainData/",RFmodels[i], sep=""), "train.model")
  curr.predictions[[i]]=predict(train.model, newdata = new.data[[i]])
}
head(curr.predictions)
curr.preds.df=as.data.frame(do.call(cbind,curr.predictions))

ModelNames=c("ACCESS","GFDL","IPSL","MIROC5","MRI")
ModelNames
colnames(fut.preds.df)=ModelNames
colnames(fut.preds.df)
colnames(curr.preds.df)=ModelNames
colnames(curr.preds.df)

curr.preds.df$Period=rep("Current",468)
fut.preds.df$Period=rep("Future",467)
currANDfut_preds=bind_rows(curr.preds.df,fut.preds.df)
currANDfut_preds.melt=melt(currANDfut_preds)
head(currANDfut_preds.melt)

ggplot(currANDfut_preds.melt, aes(x=variable, y=value, fill=Period))+geom_boxplot()+ylab("Invasion Risk")+xlab("GCMs")

