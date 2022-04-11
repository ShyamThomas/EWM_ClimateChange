library(randomForest)
library(tidyverse)
library(corrplot)

setwd("~/UMNpostdoc/ProjectEWM/RProjects/EWM_ClimateChange")

EWM.GCMs.data=read_csv("processed_data/EWM.prsabs95to15_AllGCMs.csv")
EWM.GCMs.data
EWM.GCMs.data%>%View()
sdm.data=EWM.GCMs.data[,-c(1:3,6)]%>%
na.omit()
sdm.data %>%View() ### this data was used for distribution modeling earlier
colnames(sdm.data)
RF.data_All5GCMs=sdm.data[,c(3,7,13,15:19)]
### Create an averaged dataset across all different GCMs
RFdata.MeanGCM=RF.data_All5GCMs%>%rowwise()%>%mutate(MeanGCM_GDD=mean(c_across(4:8)))
RFdata.MeanGCM
MeanGCM.RFmodel=randomForest(RFdata.MeanGCM[,c(2,3,9)], RFdata.MeanGCM$EWMSTATUS_corrRelFrq, importance = TRUE,ntree = 5000,type="regression")
MeanGCM.RFmodel

RF.plot=partial(MeanGCM.RFmodel, pred.var = "MeanGCM_GDD", train=RF.env.data)%>%
  plotPartial(smooth = FALSE, lwd = 2, ylab = "Invasion risk",xlab="Mean GDD", main = " ")
RF.plot


### Use the same averaged data for GAMs
MeanGCM.GAMmodel=gam(EWMSTATUS_corrRelFrq~s(avg_secchi)+s(roaddensity_density_mperha)+s(MeanGCM_GDD), data=RFdata.MeanGCM)
summary(MeanGCM.GAMmodel)
gam.check(MeanGCM.GAMmodel)
GAM.Plot_k10=plot(MeanGCM.GAMmodel, shift = coef(MeanGCM.GAMmodel)[1], select = 3, rug = FALSE, lwd=2,se=FALSE,
             xlab="Mean GDD", ylab="Invasion risk")
GAM.Plot_k10

MeanGCM.GAMmodel_K3=gam(EWMSTATUS_corrRelFrq~s(avg_secchi, k=3)+s(roaddensity_density_mperha,k=3)+s(MeanGCM_GDD,k=3), data=RFdata.MeanGCM)
gam.check(MeanGCM.GAMmodel_K3)
GAM.Plot_k3=plot(MeanGCM.GAMmodel_K3, shift = coef(MeanGCM.GAMmodel_K3)[1], select = 3, rug=FALSE, lwd=2, se=FALSE, xlab="Mean GDD", ylab="Invasion risk")


