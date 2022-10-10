
library(precrec)
library(maptools)
library(spatstat)
library(blockCV)
library(sf)
library(tidyverse)


Minn.sf=read_sf(dsn="/Users/thom7552/UMNpostdoc/ProjectEWM/MinnEWM/MinnGISlayers", layer="Minn.map")
plot(Minn.sf$geometry)
Minn.sf
st_crs(Minn.sf)

EWM.GCMs.data=read_csv("processed_data/EWM.prsabs95to15_AllGCMs_v2.csv")
EWM.GCMs.data

EWM.GCMs.sf=st_as_sf(EWM.GCMs.data, coords=c("LON","LAT"), crs=32615)
EWM.GCMs.sf
st_crs(EWM.GCMs.sf)
Minn.sf=st_transform(Minn.sf, crs=32615)
plot(Minn.sf$geometry)
plot(EWM.GCMs.sf$geometry, add=T)


################ SPATIALLY BLOCKED CV ALONG LATITUDINAL GRADIENT
CheqBrd.trial.blocks = spatialBlock(speciesData = EWM.GCMs.sf, # sf or SpatialPoints
                                 species = "EWMSTATUS", # the response column (binomial or multi-class)
                                 #theRange = 100000, # size of the blocks in meters
                                 rows = 4, # number of folds
                                 selection = "checkerboard",
                                 iteration = 100, # find evenly dispersed folds
                                 biomod2Format = FALSE)

CB=CheqBrd.trial.blocks$plots+theme(panel.grid.major = element_blank())+
  geom_sf(data = Minn.sf, fill=NA, colour="gray", lwd=1)+
  geom_sf(data = EWM.GCMs.sf, alpha = 0.25, aes(colour=as.factor(EWMSTATUS_corrRelFrq)))+theme(legend.title=element_blank())
CB+theme_minimal()+theme(legend.title=element_blank())+ggtitle("Spatial blocks", subtitle = "EWM distribution")

p=ggplot()+geom_sf(data = Minn.sf, fill=NA, colour="gray", lwd=1)+geom_sf(data = EWM.GCMs.sf, alpha = 0.5, aes(colour=as.factor(EWMSTATUS_corrRelFrq)))+theme(legend.title=element_blank())
p+theme_minimal()+theme(legend.title=element_blank())+ggtitle("EWM distribution")


############## START WITH RANDOM FOREST MODELS
Train.fileNames = list.files(path="processed_data/TrainData/",pattern=".csv")
Train.fileNames


AUC_all=NULL

for(Train.fileName in Train.fileNames) {
  full.df = read.csv(paste("processed_data/TrainData/",Train.fileName, sep=""))
  #sub.df=full.df[,c(1,5,11,12)]

folds=CheqBrd.trial.blocks$folds

testTable <- full.df
testTable$RFpreds <- NA

  for(k in seq_len(length(folds))){
    # extracting the training and testing indices
    # this way works with folds list (but not foldID)
    trainSet <- unlist(folds[[k]][1]) # training set indices
    testSet <- unlist(folds[[k]][2]) # testing set indices
    rf <- randomForest(as.factor(EWMSTATUS)~., full.df[trainSet, ], ntree = 500) # model fitting on training set
    testTable$RFpreds[testSet] <- predict(rf, full.df[testSet, ], type = "prob")[,2] # predict the test set
  
      AUC=auc(roc(testTable$EWMSTATUS,testTable$RFpreds))
      AUC_all = rbind(AUC_all, data.frame(Train.fileName, k, AUC))
  }
}

AUC_all$Train.fileName=sub('EWM.train.data_', '',AUC_all$Train.fileName)
AUC_all
AUC_all$Train.fileName=sub('.WtrTemp.csv', '',AUC_all$Train.fileName)
AUC_all

MeanAUC_SpBlk_RF=AUC_all%>%group_by(Train.fileName)%>%summarise(
  meanAUC=mean(AUC)
)
MeanAUC_SpBlk_RF
write.table(MeanAUC_SpBlk_RF,"Results/AllGCMs_SpatialBlockCV_RF_AUCs.txt", sep="\t")

############## NOW WITH GAM, K=10
Train.fileNames = list.files(path="processed_data/TrainData/",pattern=".csv")
Train.fileNames

AUC_all=NULL

for(Train.fileName in Train.fileNames) {
  full.df = read.csv(paste("processed_data/TrainData/",Train.fileName, sep=""))
  #sub.df=full.df
  
  folds=CheqBrd.trial.blocks$folds
  testTable <- full.df
  testTable$GAMpreds <- NA

  for(k in seq_len(length(folds))){
    trainSet <- unlist(folds[[k]][1]) # training set indices
    testSet <- unlist(folds[[k]][2]) # testing set indices
    sample_sub=full.df[trainSet, ]
    fm <- paste('s(', names(sample_sub[ -1 ]), ',k=10)', sep = "", collapse = ' + ')
    fm <- as.formula(paste('EWMSTATUS ~', fm))
    GAM_k10 = gam(fm,data=sample_sub, method="REML", family = "binomial")
    testTable$GAMpreds[testSet] <- predict(GAM_k10, full.df[testSet, ], type = "response") # predict the test set
    
       AUC=auc(roc(testTable$EWMSTATUS,testTable$GAMpreds))
       AUC_all = rbind(AUC_all, data.frame(Train.fileName, k, AUC))
  }
}

AUC_all$Train.fileName=sub('EWM.train.data_', '',AUC_all$Train.fileName)
AUC_all
AUC_all$Train.fileName=sub('.WtrTemp.csv', '',AUC_all$Train.fileName)
AUC_all

MeanAUC_SpBlk_GAM.k10=AUC_all%>%group_by(Train.fileName)%>%summarise(
  meanAUC=mean(AUC)
)

write.table(MeanAUC_SpBlk_GAM.k10,"Results/AllGCMs_SpatialBlockCV_GAM.k10_AUCs.txt", sep="\t")

############## NOW WITH GAM, K=3
Train.fileNames = list.files(path="processed_data/TrainData/",pattern=".csv")
Train.fileNames

AUC_all=NULL

for(Train.fileName in Train.fileNames) {
  full.df = read.csv(paste("processed_data/TrainData/",Train.fileName, sep=""))
  #sub.df=full.df[,c(1,5,11,12)]
  
  folds=CheqBrd.trial.blocks$folds
  testTable <- full.df
  testTable$GAMk3preds <- NA
  
  for(k in seq_len(length(folds))){
    trainSet <- unlist(folds[[k]][1]) # training set indices
    testSet <- unlist(folds[[k]][2]) # testing set indices
    sample_sub=full.df[trainSet, ]
    fm <- paste('s(', names(sample_sub[ -1 ]), ',k=3)', sep = "", collapse = ' + ')
    fm <- as.formula(paste('EWMSTATUS ~', fm))
    GAM_k3 = gam(fm,data=sample_sub, method="REML", family = "binomial")
    testTable$GAMk3preds[testSet] <- predict(GAM_k3, full.df[testSet, ], type = "response") # predict the test set
    
    AUC=auc(roc(testTable$EWMSTATUS,testTable$GAMk3preds))
    AUC_all = rbind(AUC_all, data.frame(Train.fileName, k, AUC))
  }
}

AUC_all$Train.fileName=sub('EWM.train.data_', '',AUC_all$Train.fileName)
AUC_all
AUC_all$Train.fileName=sub('.WtrTemp.csv', '',AUC_all$Train.fileName)
AUC_all

MeanAUC_SpBlk_GAM.k3=AUC_all%>%group_by(Train.fileName)%>%summarise(
  meanAUC=mean(AUC)
)
MeanAUC_GCMs_GAM.k3
write.table(MeanAUC_SpBlk_GAM.k3,"Results/AllGCMs_SpatialBlockCV_GAM.k3_AUCs.txt", sep="\t")

###############################################################################################################################################
############################################## INDEPENDENT VALIDATION, 90% AND HIGHER GDD VALUES BLOCKED #######################################
################################################################################################################################################

Train.fileNames = list.files(path="processed_data/TrainData/",pattern=".csv")
Train.fileNames

AUC_all=NULL

for(Train.fileName in Train.fileNames) {
  full.df = read.csv(paste("processed_data/TrainData/",Train.fileName, sep=""))
  sub.df=full.df[,c(1:4)]
  q=quantile(sub.df[,4], probs=0.9) ### set the threshold at 90th percentile
  test.df=sub.df%>%filter(.[[4]]>q) ### test data containing lakes with upper 10th percent temperatures
  train.df=sub.df%>%filter(.[[4]]<q) ### train data all but
  
rf = randomForest(as.factor(EWMSTATUS)~., train.df[,-1 ], ntree = 500, data=train.df, keep.forest=TRUE)
test.df$preds=NULL
test.df$preds=predict(rf, test.df[,-1], type = "prob")[,2]

AUC=auc(roc(test.df$EWMSTATUS,test.df$preds))
AUC_all = rbind(AUC_all, data.frame(Train.fileName, AUC)) 

}

AUC_all$Train.fileName=sub('EWM.train.data_', '',AUC_all$Train.fileName)
AUC_all
AUC_all$Train.fileName=sub('.WtrTemp.csv', '',AUC_all$Train.fileName)
AUC_all

MeanAUC_Warmest_RF=AUC_all%>%group_by(Train.fileName)%>%summarise(
  meanAUC=mean(AUC)
)
MeanAUC_Warmest_RF
write.table(MeanAUC_Warmest_RF,"Results/AllGCMs_WarmestVal_RF_AUCs.txt", sep="\t")

################## NOW WITH GAM, K=10

Train.fileNames = list.files(path="processed_data/TrainData/",pattern=".csv")
Train.fileNames

AUC_all=NULL


for(Train.fileName in Train.fileNames) {
  full.df = read.csv(paste("processed_data/TrainData/",Train.fileName, sep=""))
  sub.df=full.df[,c(1:4)]
  q=quantile(sub.df[,4], probs=0.9)
  test.df=sub.df%>%filter(.[[4]]>q)
  train.df=sub.df%>%filter(.[[4]]<q)
  
  fm <- paste('s(', names(train.df[ -1 ]), ',k=10)', sep = "", collapse = ' + ')
  fm <- as.formula(paste('EWMSTATUS ~', fm))
  GAM_k10 = gam(fm,data=train.df, method="REML", family = "binomial")
  test.df$GAMpreds=NULL
  test.df$GAMpreds <- predict(GAM_k10, test.df[, -1], type = "response") # predict the test set
  
  AUC=auc(roc(test.df$EWMSTATUS,test.df$GAMpreds))
  AUC_all = rbind(AUC_all, data.frame(Train.fileName, AUC)) 
  
}

AUC_all$Train.fileName=sub('EWM.train.data_', '',AUC_all$Train.fileName)
AUC_all
AUC_all$Train.fileName=sub('.WtrTemp.csv', '',AUC_all$Train.fileName)
AUC_all

MeanAUC_Warmest_GAM.k10=AUC_all%>%group_by(Train.fileName)%>%summarise(
  meanAUC=mean(AUC)
)
MeanAUC_Warmest_GAM.k10
write.table(MeanAUC_Warmest_GAM.k10,"Results/AllGCMs_WarmestVal_GAM.k10_AUCs.txt", sep="\t")
  
################## NOW WITH GAM, K=3

Train.fileNames = list.files(path="processed_data/TrainData/",pattern=".csv")
Train.fileNames

AUC_all=NULL

for(Train.fileName in Train.fileNames) {
  full.df = read.csv(paste("processed_data/TrainData/",Train.fileName, sep=""))
  sub.df=full.df[,c(1:4)]
  q=quantile(sub.df[,4], probs=0.9)
  test.df=sub.df%>%filter(.[[4]]>q)
  train.df=sub.df%>%filter(.[[4]]<q)
  
  fm <- paste('s(', names(train.df[ -1 ]), ',k=3)', sep = "", collapse = ' + ')
  fm <- as.formula(paste('EWMSTATUS ~', fm))
  GAM_k3 = gam(fm,data=train.df, method="REML", family = "binomial")
  test.df$GAMpreds=NULL
  test.df$GAMpreds <- predict(GAM_k3, test.df[, -1], type = "response") # predict the test set
  
  AUC=auc(roc(test.df$EWMSTATUS,test.df$GAMpreds))
  AUC_all = rbind(AUC_all, data.frame(Train.fileName, AUC)) 
  
}

AUC_all$Train.fileName=sub('EWM.train.data_', '',AUC_all$Train.fileName)
AUC_all
AUC_all$Train.fileName=sub('.WtrTemp.csv', '',AUC_all$Train.fileName)
AUC_all

MeanAUC_Warmest_GAM.k3=AUC_all%>%group_by(Train.fileName)%>%summarise(
  meanAUC=mean(AUC)
)
MeanAUC_Warmest_GAM.k3
write.table(MeanAUC_Warmest_GAM.k3,"Results/AllGCMs_WarmestVal_GAM.k3_AUCs.txt", sep="\t")


################################################################################################################################################
AUC.fileNames = list.files(pattern="AUCs.txt")
result = lapply(AUC.fileNames, function(x) read.table(x,header = TRUE))
result


AllAUCs_combined=do.call(rbind, result)
AllAUCs_combined
AllAUCs_combined$ModelName=rep(AUC.fileNames, each=5)
AllAUCs_combined$meanAUC=round(AllAUCs_combined$meanAUC, 3)
AllAUCs_combined
SDMs=c("GAM (k=10)", "GAM (k=03)", "RF")
SDMs
AllAUCs_combined$SDM=rep(SDMs, each=5)
AllAUCs_combined$Test=rep(ValidationTests, each=15)
AllAUCs_combined


MeanAUC_bySDMnTest=AllAUCs_combined%>%group_by(Test,SDM)%>%summarise(
avgAUC=mean(meanAUC),
sdAUC=sd(meanAUC)
)




################################################################################################################################################
