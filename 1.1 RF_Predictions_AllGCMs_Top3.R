library(tidyverse)
library(miceadds)
library(ggplot2)
library(randomForest)
library(reshape2)

### List all RF models
RFmodels = list.files(path ="processed_data/TrainData/", pattern = "*Top3_.Rdata$")
RFmodels

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
  load.Rdata(paste("processed_data/TrainData/",RFmodels[i], sep=""), "train.model")
  fut.predictions[[i]]=predict(train.model, newdata = new.data[[i]])
}

head(fut.predictions)
fut.preds.df=as.data.frame(do.call(cbind,fut.predictions))
head(fut.preds.df)
write_csv(fut.preds.df, "Results/Futr.Predictions.csv")

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
  load.Rdata(paste("processed_data/TrainData/",RFmodels[i], sep=""), "train.model")
  curr.predictions[[i]]=predict(train.model, newdata = new.data[[i]])
}
head(curr.predictions)
curr.preds.df=as.data.frame(do.call(cbind,curr.predictions))
head(curr.preds.df)
write_csv(curr.preds.df, "Results/Curr.Predictions.csv")

############################################################################################################################################
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

InvasionRisk_Plot=ggplot(currANDfut_preds.melt, aes(x=variable, y=value, fill=Period))+geom_boxplot()+ylab("Invasion Risk")+xlab("GCMs")
ggsave("InvasionRisk_Plot.png", path="Figures/", device = "png",width = 6, height = 4.5 )


