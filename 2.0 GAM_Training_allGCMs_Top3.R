library(sf)
library(mgcv)
library(tidyverse)
library(corrplot)
library(miceadds)

setwd("~/UMNpostdoc/ProjectEWM/RProjects/EWM_ClimateChange")

EWM.GCMs.data=read_csv("processed_data/EWM.prsabs95to15_AllGCMs_v2.csv")
EWM.GCMs.data
EWM.GCMs.data%>%View()
sdm.data=EWM.GCMs.data[,-c(1:3,6)]%>%
  na.omit()
sdm.data %>%View() ### this data was used for distribution modeling earlier
sdm.data 

### Change column names to make it concise and clear
#colnames(sdm.data )=c("Long", "Lat","EWM","LakeDepth", "LakeSize", "pH", "SecchiDepth","Conductance",
       #               " Phosphorus", "Chlorophyll", "CDOM", "StreamConnectivity", "RoadDensity", 
        #              "WtrTemp_Avg.Ann.GDD","ACCESS.avg.ann.gdd","MIROC5.avg.ann.gdd","IPSL.avg.ann.gdd",
         #             "GFDL.avg.ann.gdd", "MRI.avg.ann.gdd")
sdm.data 


EWM.train.data_WtrTemp=sdm.data[,-c(1,2,15:19)]
EWM.train.data_WtrTemp

EWM.train.data_ACCESS.WtrTemp=sdm.data[,-c(1,2,14,16:19)]
EWM.train.data_ACCESS.WtrTemp
write_csv(EWM.train.data_ACCESS.WtrTemp, "processed_data/TrainData/EWM.train.data_ACCESS.WtrTemp.csv")

EWM.train.data_MIROC5.WtrTemp=sdm.data[,-c(1,2,14,15,17:19)]
EWM.train.data_MIROC5.WtrTemp
write_csv(EWM.train.data_MIROC5.WtrTemp, "processed_data/TrainData/EWM.train.data_MIROC5.WtrTemp.csv")

EWM.train.data_IPSL.WtrTemp=sdm.data[,-c(1,2,14:16,18,19)]
EWM.train.data_IPSL.WtrTemp
write_csv(EWM.train.data_IPSL.WtrTemp, "processed_data/TrainData/EWM.train.data_IPSL.WtrTemp.csv")

EWM.train.data_GFDL.WtrTemp=sdm.data[,-c(1,2,14:17,19)]
EWM.train.data_GFDL.WtrTemp
write_csv(EWM.train.data_GFDL.WtrTemp, "processed_data/TrainData/EWM.train.data_GFDL.WtrTemp.csv")

EWM.train.data_MRI.WtrTemp=sdm.data[,-c(1,2,14:18)]
EWM.train.data_MRI.WtrTemp
write_csv(EWM.train.data_MRI.WtrTemp, "processed_data/TrainData/EWM.train.data_MRI.WtrTemp.csv")

################### ITERATING RF MODELS ACROSS ALL THE TRAIN DATSETS
### Start by creating a list of all the saved files in the new folder
Train.fileNames = list.files(path="processed_data/TrainData/",pattern=".csv")
Train.fileNames

########################## REPEAT THE ITERATION WITH A REDUCED SUBSET OF PREDICTORS: THE TOP 3 PREDICTORS ONLY
###### A reduced subset of predictors: top 3 ranking predictors
###### The predictors are: Secchi depth, Road density, and WaterGDD

#results_top3=NULL

## default K (k= 10)
for(Train.fileName in Train.fileNames) {
  sample = read.csv(paste("processed_data/TrainData/",Train.fileName, sep=""))
  fm <- paste('s(', names(sample[ -1 ]), ')', sep = "", collapse = ' + ')
  fm <- as.formula(paste('EWMSTATUS ~', fm))
  gam = gam(fm,data=sample, method="REML", family = "binomial")
  save(gam, file=paste("processed_data/TrainData/", sub('....$','',Train.fileName), "GAM.Rdata", sep=""))
}
load.Rdata("processed_data/TrainData/EWM.train.data_ACCESS.WtrTempGAM.Rdata", "ACCESS.GAM.model")
load.Rdata("processed_data/TrainData/EWM.train.data_GFDL.WtrTempGAM.Rdata", "GFDL.GAM.model")
load.Rdata("processed_data/TrainData/EWM.train.data_IPSL.WtrTempGAM.Rdata", "IPSL.GAM.model")
load.Rdata("processed_data/TrainData/EWM.train.data_MIROC5.WtrTempGAM.Rdata", "MIROC5.GAM.model")
load.Rdata("processed_data/TrainData/EWM.train.data_MRI.WtrTempGAM.Rdata", "MRI.GAM.model")

## setting k =3
for(Train.fileName in Train.fileNames) {
    sample = read.csv(paste("processed_data/TrainData/",Train.fileName, sep=""))
    fm <- paste('s(', names(sample[ -1 ]), ',k=3)', sep = "", collapse = ' + ')
    fm <- as.formula(paste('EWMSTATUS ~', fm))
    gam_k3 = gam(fm,data=sample, method="REML", family = "binomial")
    save(gam_k3, file=paste("processed_data/TrainData/", sub('....$','',Train.fileName), "GAM_k3.Rdata", sep=""))
}

load.Rdata("processed_data/TrainData/EWM.train.data_ACCESS.WtrTempGAM_k3.Rdata", "ACCESS.GAM_k3.model")
load.Rdata("processed_data/TrainData/EWM.train.data_GFDL.WtrTempGAM_k3.Rdata", "GFDL.GAM_k3.model")
load.Rdata("processed_data/TrainData/EWM.train.data_IPSL.WtrTempGAM_k3.Rdata", "IPSL.GAM_k3.model")
load.Rdata("processed_data/TrainData/EWM.train.data_MIROC5.WtrTempGAM_k3.Rdata", "MIROC5.GAM_k3.model")
load.Rdata("processed_data/TrainData/EWM.train.data_MRI.WtrTempGAM_k3.Rdata", "MRI.GAM_k3.model")

plot(ACCESS.GAM_k3.model, select=3)
plot(GFDL.GAM_k3.model, select=3)
plot(IPSL.GAM_k3.model, select=3)
plot(MIROC5.GAM_k3.model, select=3)

par(mfrow=c(2,3))

## setting k = 6

for(Train.fileName in Train.fileNames) {
  sample = read.csv(paste("processed_data/TrainData/",Train.fileName, sep=""))
  #sample_sub=sample[,c(1,5,11,12)]
  fm <- paste('s(', names(sample[ -1 ]), ',k=6)', sep = "", collapse = ' + ')
  fm <- as.formula(paste('EWMSTATUS ~', fm))
  gam_k6 = gam(fm,data=sample, method="REML")
  save(gam_k6, file=paste("processed_data/TrainData/", sub('....$','',Train.fileName), "GAM_k6.Rdata", sep=""))
  
}

load.Rdata("processed_data/TrainData/EWM.train.data_ACCESS.WtrTempGAM_k6.Rdata", "ACCESS.GAM_k6.model")
load.Rdata("processed_data/TrainData/EWM.train.data_GFDL.WtrTempGAM_k6.Rdata", "GFDL.GAM_k6.model")
load.Rdata("processed_data/TrainData/EWM.train.data_IPSL.WtrTempGAM_k6.Rdata", "IPSL.GAM_k6.model")
load.Rdata("processed_data/TrainData/EWM.train.data_MIROC5.WtrTempGAM_k6.Rdata", "MIROC5.GAM_k6.model")
load.Rdata("processed_data/TrainData/EWM.train.data_MRI.WtrTempGAM_k6.Rdata", "MRI.GAM_k6.model")

library(gridExtra)
library(gratia)

grid.arrange(
draw(ACCESS.GAM_k3.model, select=3),draw(ACCESS.GAM.model, select=3),
draw(GFDL.GAM_k3.model, select=3),draw(GFDL.GAM.model, select=3),
draw(IPSL.GAM_k3.model, select=3),draw(IPSL.GAM.model, select=3),
draw(MIROC5.GAM_k3.model, select=3),draw(MIROC5.GAM.model, select=3),
draw(MRI.GAM_k3.model, select=3),draw(MRI.GAM.model, select=3),
nrow=5)

############################################################################################################################################
##### Run 5-fold cross-validation and capture AUCs for each of the GAM models
AUC_all=NULL

for(Train.fileName in Train.fileNames) {
  full.df = read.csv(paste("processed_data/TrainData/",Train.fileName, sep=""))
  sub.df=full.df[,c(1:4)]
  folds = rep_len(1:5,nrow(sub.df))
  sample.folds=sample(folds,nrow(sample))
  sub.df$folds=sample.folds
  
  set.seed(007)
  
  for(i in 1:5){test.data=sub.df[sub.df$folds==i,]
      train.data= sub.df[sub.df$folds !=i,]
      fm <- paste('s(', names(sub.df[ -c(1,5) ]), ',k=3)', sep = "", collapse = ' + ')   ### FOR k=3 GAMs & then k=10
      fm <- as.formula(paste('EWMSTATUS ~', fm))
      gam_k3 = gam(fm,data=train.data, method="REML", family = "binomial")
      preds.test=predict(gam_k3, newdata=test.data, type="response")
  
  AUC=auc(roc(test.data$EWMSTATUS,preds.test))
  AUC_all = rbind(AUC_all, data.frame(Train.fileName, i, AUC))
  }
}

### Get rid off all the unwanted letters
AUC_all$Train.fileName=sub('EWM.train.data_', '',AUC_all$Train.fileName)
AUC_all
AUC_all$Train.fileName=sub('.WtrTemp.csv', '',AUC_all$Train.fileName)
AUC_all

MeanAUC_GCMs_k3=AUC_all%>%group_by(Train.fileName)%>%summarise(
meanAUC=mean(AUC)
)
MeanAUC_GCMs_k3

write.table(MeanAUC_GCMs_k3,"Results/AllGCMs_5foldCV_GAMk3_AUCs.txt", sep="\t")

########## now for GAM, k=10
AUC_all=NULL

for(Train.fileName in Train.fileNames) {
  full.df = read.csv(paste("processed_data/TrainData/",Train.fileName, sep=""))
  sub.df=full.df[,c(1:4)]
  folds = rep_len(1:5,nrow(sub.df))
  sample.folds=sample(folds,nrow(sample))
  sub.df$folds=sample.folds
  
  set.seed(007)
  
  for(i in 1:5){test.data=sub.df[sub.df$folds==i,]
  train.data= sub.df[sub.df$folds !=i,]
  fm <- paste('s(', names(sub.df[ -c(1,5) ]),',k=10)', sep = "", collapse = ' + ')   ### FOR GAMs k=10
  fm <- as.formula(paste('EWMSTATUS ~', fm))
  gam_k10 = gam(fm,data=train.data, method="REML", family = "binomial")
  preds.test=predict(gam_k10, newdata=test.data, type="response")
  
  AUC=auc(roc(test.data$EWMSTATUS,preds.test))
  AUC_all = rbind(AUC_all, data.frame(Train.fileName, i, AUC))
  }
}

### Get rid off all the unwanted letters
AUC_all$Train.fileName=sub('EWM.train.data_', '',AUC_all$Train.fileName)
AUC_all
AUC_all$Train.fileName=sub('.WtrTemp.csv', '',AUC_all$Train.fileName)
AUC_all

MeanAUC_GCMs_k10=AUC_all%>%group_by(Train.fileName)%>%summarise(
  meanAUC=mean(AUC)
)
MeanAUC_GCMs_k10
write.table(MeanAUC_GCMs_k10,"Results/AllGCMs_5foldCV_GAMk10_AUCs.txt", sep="\t")

