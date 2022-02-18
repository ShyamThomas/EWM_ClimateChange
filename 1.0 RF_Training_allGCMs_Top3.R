library(sf)
library(randomForest)
library(tidyverse)
library(corrplot)

setwd("~/UMNpostdoc/ProjectEWM/WaterTempData")

EWM.GCMs.data=read_csv("Data/EWM.prsabs95to15_AllGCMs.csv")
EWM.GCMs.data%>%View()
sdm.data=EWM.GCMs.data[,-c(1:3,6)]%>%
  na.omit()
sdm.data %>%View() ### this data was used for distribution modeling earlier
sdm.data 

### Change column names to make it concise and clear
colnames(sdm.data )=c("Long", "Lat","EWM","LakeDepth", "LakeSize", "pH", "SecchiDepth","Conductance",
                      " Phosphorus", "Chlorophyll", "CDOM", "StreamConnectivity", "RoadDensity", 
                      "WtrTemp_Avg.Ann.GDD","ACCESS.avg.ann.gdd","MIROC5.avg.ann.gdd","IPSL.avg.ann.gdd",
                      "GFDL.avg.ann.gdd", "MRI.avg.ann.gdd")
sdm.data 


EWM.train.data_WtrTemp=sdm.data[,-c(1,2,15:19)]
EWM.train.data_WtrTemp

EWM.train.data_ACCESS.WtrTemp=sdm.data[,-c(1,2,14,16:19)]
EWM.train.data_ACCESS.WtrTemp
write_csv(EWM.train.data_ACCESS.WtrTemp, "Data/TrainData/EWM.train.data_ACCESS.WtrTemp.csv")

EWM.train.data_MIROC5.WtrTemp=sdm.data[,-c(1,2,14,15,17:19)]
EWM.train.data_MIROC5.WtrTemp
write_csv(EWM.train.data_MIROC5.WtrTemp, "Data/TrainData/EWM.train.data_MIROC5.WtrTemp.csv")

EWM.train.data_IPSL.WtrTemp=sdm.data[,-c(1,2,14:16,18,19)]
EWM.train.data_IPSL.WtrTemp
write_csv(EWM.train.data_IPSL.WtrTemp, "Data/TrainData/EWM.train.data_IPSL.WtrTemp.csv")

EWM.train.data_GFDL.WtrTemp=sdm.data[,-c(1,2,14:17,19)]
EWM.train.data_GFDL.WtrTemp
write_csv(EWM.train.data_GFDL.WtrTemp, "Data/TrainData/EWM.train.data_GFDL.WtrTemp.csv")

EWM.train.data_MRI.WtrTemp=sdm.data[,-c(1,2,14:18)]
EWM.train.data_MRI.WtrTemp
write_csv(EWM.train.data_MRI.WtrTemp, "Data/TrainData/EWM.train.data_MRI.WtrTemp.csv")

### Created a new folder 'TrainData' within folder 'Data' and  moved the above saved files there!

################### ITERATING RF MODELS ACROSS ALL THE TRAIN DATSETS
### Start by creating a list of all the saved files in the new folder
Train.fileNames = list.files(path="Data/TrainData/",pattern=".csv")
Train.fileNames

### A simple loop that reads all the files and executes random forest algorithm; saves RF obj and OOB errors as text file
### Removed lake depth and conductance as these variables were correlated with secchi and wtr_temp_GDD respectively

results=NULL

for(Train.fileName in Train.fileNames) {
sample = read.csv(paste("Data/TrainData/",Train.fileName, sep=""))
rf = randomForest(sample[,c(3:5,7:12)], sample$EWM,importance=TRUE, ntree=5000, type="regression")
#save(rf, file=paste("Data/TrainData/", sub('....$','',Train.fileName), ".Rdata", sep=""))

#meanMSE = mean(rf$mse)
#results = rbind(results, data.frame(Train.fileName, meanMSE))
#write.table(results,"Results/RF_OOBerror.txt",sep = "\t")

ImpPlot=vip(rf)
ggsave(filename=paste(sub('....$','',Train.fileName),"_ImpPlot.png", sep=""), ImpPlot, 
       path="Figures/", units="in", width=9, height=6, dpi=900)
}


########################## REPEAT THE ITERATION WITH A REDUCED SUBSET OF PREDICTORS: THE TOP 3 PREDICTORS ONLY
###### A reduced subset of predictors: top 3 ranking predictors
###### The predictors are: Secchi depth, Road density, and WaterGDD

results_top3=NULL

for(Train.fileName in Train.fileNames) {
  sample = read.csv(paste("Data/TrainData/",Train.fileName, sep=""))
  rf = randomForest(sample[,c(5,11,12)], sample$EWMSTATUS_corrRelFrq,importance=TRUE, ntree=5000, type="regression")
  save(rf, file=paste("Data/TrainData/", sub('....$','',Train.fileName), "Top3_.Rdata", sep=""))
  
  meanMSE = mean(rf$mse)
  results_top3 = rbind(results_top3, data.frame(Train.fileName, meanMSE))
  write.table(results,"Results/RF_MSE_Top3.txt",sep = "\t")
  
  Top3Preds.Names=colnames(sample)[c(5,11,12)]
  
  for (Pred.Name in Top3Preds.Names){
    partial_plot=autoplot(partial(rf,pred.var = Pred.Name, ice=TRUE, rug=TRUE, train = sample, prob = TRUE),xlab=Pred.Name, ylab="Invasion risk", alpha=0.1)
    ggsave(filename=paste(sub('....$','',Train.fileName),"_",Pred.Name,"_IcePlot.png", sep=""), partial_plot, path="Figures/",units="in", width=9, height=6, dpi=900)
    }
}

##### Run 5-fold cross-validation and capture AUCs for each of the RF model
AUC_all=NULL

for(Train.fileName in Train.fileNames) {
  full.df = read.csv(paste("Data/TrainData/",Train.fileName, sep=""))
  folds = rep_len(1:5,nrow(full.df))
  sample.folds=sample(folds,nrow(sample))
  full.df$folds=sample.folds
  
  set.seed(007)

  for(i in 1:5){test.data=full.df[full.df$folds==i,]
                train.data= full.df[full.df$folds !=i,]
                train.rf = randomForest(train.data[,c(5,11,12)], train.data$EWMSTATUS_corrRelFrq,importance=TRUE, ntree=5000, 
                                        type="regression")
                preds.test=predict(train.rf, newdata=test.data)
                AUC=auc(roc(test.data$EWMSTATUS_corrRelFrq,preds.test))

                AUC_all = rbind(AUC_all, data.frame(Train.fileName, i, AUC))
  }
}

### Get rid off all the unwanted letters
AUC_all$Train.fileName=sub('EWM.train.data_', '',AUC_all$Train.fileName)
AUC_all
AUC_all$Train.fileName=sub('.WtrTemp.csv', '',AUC_all$Train.fileName)
AUC_all

write.table(AUC_all,"Results/AllGCMs_5foldCV_AUCs.txt", sep="\t")
