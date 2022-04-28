
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

EWM.GCMs.data=read_csv("processed_data/EWM.prsabs95to15_AllGCMs.csv")
EWM.GCMs.data

EWM.GCMs.sf=st_as_sf(EWM.GCMs.data, coords=c("UTMX","UTMY"), crs=32615)
EWM.GCMs.sf
st_crs(EWM.GCMs.sf)
Minn.sf=st_transform(Minn.sf, crs=32615)
plot(Minn.sf$geometry)
plot(EWM.GCMs.sf$geometry, add=T)

################ RANDOM 5-FOLD CV BEGINS

Rand.trial.blocks = spatialBlock(speciesData = EWM.GCMs.sf, # sf or SpatialPoints
                            species = "EWMSTATUS_corrRelFrq", # the response column (binomial or multi-class)
                            theRange = 100000, # size of the blocks in meters
                            k = 5, # number of folds, 
                            selection = "random",
                            iteration = 100, # find evenly dispersed folds
                            biomod2Format = FALSE)

RB=Rand.trial.blocks$blocks+theme(panel.grid.major= element_blank())+geom_sf(data = Minn.sf, fill=NA, colour="gray", lwd=1)+
  geom_sf(data = EWM.GCMs.sf, alpha = 0.25, aes(colour=as.factor(EWMSTATUS_corrRelFrq)))+theme(legend.title=element_blank())

RB+theme_minimal()+theme(legend.title=element_blank())+ggtitle("Random spatial blocks", subtitle = "EWM distribution")

EWM.GCMs.data
sdm.data=EWM.GCMs.data[,-c(1:3,6)]%>%
  na.omit()
sdm.data
EWM.train.data_ACCESS.WtrTemp=sdm.data[,-c(1,2,14,16:19)]
EWM.train.data_ACCESS.WtrTemp
EWM.train.data_ACCESS.WtrTemp.Reduced=EWM.train.data_ACCESS.WtrTemp[,c(1,5,11,12)]

EWM.train.data_ACCESS.WtrTemp$RFpreds=NA

folds=Rand.trial.blocks$folds

for(k in seq_len(length(folds))){
  # extracting the training and testing indices
  # this way works with folds list (but not foldID)
  trainSet <- unlist(folds[[k]][1]) # training set indices
  testSet <- unlist(folds[[k]][2]) # testing set indices
  rf <- randomForest(as.factor(EWMSTATUS_corrRelFrq)~., EWM.train.data_ACCESS.WtrTemp.Reduced[trainSet, ], ntree = 500) # model fitting on training set
  EWM.train.data_ACCESS.WtrTemp$RFpreds[testSet] <- predict(rf, EWM.train.data_ACCESS.WtrTemp.Reduced[testSet, ], type = "prob")[,2] # predict the test set
}

precrec_obj = evalmod(scores = EWM.train.data_ACCESS.WtrTemp$RFpreds, labels = EWM.train.data_ACCESS.WtrTemp$EWMSTATUS_corrRelFrq)
precrec_obj

############## GAMs
EWM.train.data_ACCESS.WtrTemp$GAMpreds=NA


folds=Rand.trial.blocks$folds

for(k in seq_len(length(folds))){
  trainSet <- unlist(folds[[k]][1]) # training set indices
  testSet <- unlist(folds[[k]][2]) # testing set indices
  sample_sub=EWM.train.data_ACCESS.WtrTemp.Reduced[trainSet, ]
  fm <- paste('s(', names(sample_sub[ -1 ]), ',k=3)', sep = "", collapse = ' + ')
  fm <- as.formula(paste('EWMSTATUS_corrRelFrq ~', fm))
  GAM_k3 = gam(fm,data=sample_sub, method="REML", family = "binomial")
  
  EWM.train.data_ACCESS.WtrTemp$GAMpreds[testSet] <- predict(GAM_k3, EWM.train.data_ACCESS.WtrTemp.Reduced[testSet, ], type = "response") # predict the test set
}

precrec_obj = evalmod(scores = EWM.train.data_ACCESS.WtrTemp$GAMpreds, labels = EWM.train.data_ACCESS.WtrTemp$EWMSTATUS_corrRelFrq)
precrec_obj


################ SPATIALLY BLOCKED CV ALONG LATITUDINAL GRADIENT
CheqBrd.trial.blocks = spatialBlock(speciesData = EWM.GCMs.sf, # sf or SpatialPoints
                                 species = "EWMSTATUS_corrRelFrq", # the response column (binomial or multi-class)
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

############## SPATIALLY BLOCKED CROSS VALIDATION USING ACCESS DATA
EWM.GCMs.data
sdm.data=EWM.GCMs.data[,-c(1:3,6)]%>%
na.omit()
sdm.data
EWM.train.data_ACCESS.WtrTemp=sdm.data[,-c(1,2,14,16:19)]
EWM.train.data_ACCESS.WtrTemp
EWM.train.data_ACCESS.WtrTemp.Reduced=EWM.train.data_ACCESS.WtrTemp[,c(1,5,11,12)]

EWM.train.data_ACCESS.WtrTemp$RFpreds=NA


folds=CheqBrd.trial.blocks$folds

for(k in seq_len(length(folds))){
  # extracting the training and testing indices
  # this way works with folds list (but not foldID)
  trainSet <- unlist(folds[[k]][1]) # training set indices
  testSet <- unlist(folds[[k]][2]) # testing set indices
  rf <- randomForest(as.factor(EWMSTATUS_corrRelFrq)~., EWM.train.data_ACCESS.WtrTemp.Reduced[trainSet, ], ntree = 500) # model fitting on training set
  EWM.train.data_ACCESS.WtrTemp$RFpreds[testSet] <- predict(rf, EWM.train.data_ACCESS.WtrTemp.Reduced[testSet, ], type = "prob")[,2] # predict the test set
}

precrec_obj = evalmod(scores = EWM.train.data_ACCESS.WtrTemp$RFpreds, labels = EWM.train.data_ACCESS.WtrTemp$EWMSTATUS_corrRelFrq)
precrec_obj

##############
EWM.train.data_ACCESS.WtrTemp$GAMpreds=NA


folds=CheqBrd.trial.blocks$folds

for(k in seq_len(length(folds))){
  trainSet <- unlist(folds[[k]][1]) # training set indices
  testSet <- unlist(folds[[k]][2]) # testing set indices
  sample_sub=EWM.train.data_ACCESS.WtrTemp.Reduced[trainSet, ]
  fm <- paste('s(', names(sample_sub[ -1 ]), ',k=10)', sep = "", collapse = ' + ')
  fm <- as.formula(paste('EWMSTATUS_corrRelFrq ~', fm))
  GAM_k10 = gam(fm,data=sample_sub, method="REML", family = "binomial")

  EWM.train.data_ACCESS.WtrTemp$GAMpreds[testSet] <- predict(GAM_k10, EWM.train.data_ACCESS.WtrTemp.Reduced[testSet, ], type = "response") # predict the test set
}

precrec_obj = evalmod(scores = EWM.train.data_ACCESS.WtrTemp$GAMpreds, labels = EWM.train.data_ACCESS.WtrTemp$EWMSTATUS_corrRelFrq)
precrec_obj
