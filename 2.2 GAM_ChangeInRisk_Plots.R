library(tidyverse)
library(ggplot2)

#################################### PLOT 1A: CHANGE IN RISK ARROW PLOTS AS A FUNCTION OF WATER TEMP ######################################
######### START WITH GAM MODELS WITH K=3, low wiggliness
##From 1.0 get the data needed: 
EWM.GCMs.data=read_csv("processed_data/EWM.prsabs95to15_AllGCMs.csv")
Futr.Preds=read_csv("Results/GAM.k3_Fut.Predictions.csv")
Curr.Preds=read_csv("Results/GAM.k3_Curr.Predictions.csv")
Curr.Preds%>%filter(DOWLKNUM!=47004901)

Curr.Preds_means=Curr.Preds%>%rowwise()%>%mutate(Curr.MeanRisk=mean(c_across(1:5)))%>%
  select("DOWLKNUM", "Curr.MeanRisk")%>%filter(DOWLKNUM!=47004901)
Curr.Preds_means
Futr.Preds_means=Futr.Preds%>%rowwise()%>%mutate(Futr.MeanRisk=mean(c_across(1:5)))%>%select("DOWLKNUM", "Futr.MeanRisk")
Futr.Preds_means
Curr.Futr_MeanRisk=left_join(Curr.Preds_means, Futr.Preds_means, by="DOWLKNUM")
Curr.Futr_MeanRisk

####### Now repeat the above with temperature data
### Read the temperature data and repeat the above steps
Curr.Temps=EWM.GCMs.data[,c(1,19:23)]%>%filter(DOWLKNUM!=47004901)
Curr.Temps
### Put together all the forecasted future temperatures
ACCESS.forecast=read_csv("processed_data/TestData/ForecastData/EWM.forecast.data_ACCESS.WtrTemp.csv")
GFDL.forecast=read_csv("processed_data/TestData/ForecastData/EWM.forecast.data_GFDL.WtrTemp.csv")
IPSL.forecast=read_csv("processed_data/TestData/ForecastData/EWM.forecast.data_IPSL.WtrTemp.csv")
MIROC5.forecast=read_csv("processed_data/TestData/ForecastData/EWM.forecast.data_MIROC5.WtrTemp.csv")
MRI.forecast=read_csv("processed_data/TestData/ForecastData/EWM.forecast.data_MRI.WtrTemp.csv")

library(magrittr)
Fut.Temps=bind_cols(ACCESS.forecast[,1],ACCESS.forecast[,12],GFDL.forecast[,12],IPSL.forecast[,12],MIROC5.forecast[,12],MRI.forecast[,12])%>%
  set_colnames(c("DOWLKNUM","ACCESS.avg.ann.gdd","GFDL.avg.ann.gdd","IPSL.avg.ann.gdd","MIROC5.avg.ann.gdd","MRI.avg.ann.gdd"))

Curr.Temps_Means=Curr.Temps%>%rowwise()%>%mutate(Curr.MeanTmp=mean(c_across(2:6)))%>%select("DOWLKNUM","Curr.MeanTmp")
Curr.Temps_Means
Futr.Temps_Means=Fut.Temps%>%rowwise()%>%mutate(Futr.MeanTmp=mean(c_across(2:6)))%>%select("DOWLKNUM","Futr.MeanTmp")
Futr.Temps_Means
Curr.Futr_MeanTemp=left_join(Curr.Temps_Means, Futr.Temps_Means, by="DOWLKNUM")

GAM.k3_CurrFut_Temp_InvRisk=left_join(Curr.Futr_MeanTemp, Curr.Futr_MeanRisk, by="DOWLKNUM")
GAM.k3_CurrFut_Temp_InvRisk
GAM.k3_CurrFut_Temp_InvRisk_new=GAM.k3_CurrFut_Temp_InvRisk%>%mutate(MeanTmpChange=Futr.MeanTmp-Curr.MeanTmp, 
                                                             MeanRiskChange=Futr.MeanRisk-Curr.MeanRisk)
GAM.k3_CurrFut_Temp_InvRisk_new

ggplot()+
  geom_segment(data=GAM.k3_CurrFut_Temp_InvRisk_new, 
               mapping=aes(x=Curr.MeanTmp, y=Curr.MeanRisk, xend=Curr.MeanTmp+MeanTmpChange, 
                           yend=Curr.MeanRisk+MeanRiskChange,color=MeanRiskChange), 
               arrow=arrow(), size=0.5) +
  geom_point(data=GAM.k3_CurrFut_Temp_InvRisk_new, mapping=aes(x=Curr.MeanTmp, y=Curr.MeanRisk), size=1, shape=21, fill="white")+
  scale_color_gradient2(high="#0072B2", low ="#D55E00")+xlab("Annual Growing Degree Days")+ylab("Invasion Risk")+
  labs(colour="Change\nin risk")+geom_vline(xintercept = 2205, lty=2)
  ggsave("InvasionRisk_ArrowPlot_GAM.k3.png", path="Figures/", device = "png",width = 7, height = 4.5 )
  
  ###########################################################################################################################################
  #################################### PLOT 2A: CHANGE IN RISK ARROW PLOTS AS A FUNCTION OF WATER TEMP ######################################
  ######### START WITH GAM MODELS WITH K=10, low wiggliness
  ### From 1.0 get the data needed: 
  EWM.GCMs.data=read_csv("processed_data/EWM.prsabs95to15_AllGCMs.csv")
  Futr.Preds=read_csv("Results/GAM.k10_Fut.Predictions.csv")
  Futr.Preds
  Curr.Preds=read_csv("Results/GAM.k10_Curr.Predictions.csv")
  Curr.Preds%>%filter(DOWLKNUM!=47004901)
  
  Curr.Preds_means=Curr.Preds%>%rowwise()%>%mutate(Curr.MeanRisk=mean(c_across(1:5)))%>%
    select("DOWLKNUM", "Curr.MeanRisk")%>%filter(DOWLKNUM!=47004901)
  Curr.Preds_means
  Futr.Preds_means=Futr.Preds%>%rowwise()%>%mutate(Futr.MeanRisk=mean(c_across(1:5)))%>%select("DOWLKNUM", "Futr.MeanRisk")
  Futr.Preds_means
  Curr.Futr_MeanRisk=left_join(Curr.Preds_means, Futr.Preds_means, by="DOWLKNUM")
  Curr.Futr_MeanRisk
  
  ####### Now repeat the above with temperature data
  ### Read the temperature data and repeat the above steps
  Curr.Temps=EWM.GCMs.data[,c(1,19:23)]%>%filter(DOWLKNUM!=47004901)
  Curr.Temps
  ### Put together all the forecasted future temperatures
  ACCESS.forecast=read_csv("processed_data/TestData/ForecastData/EWM.forecast.data_ACCESS.WtrTemp.csv")
  GFDL.forecast=read_csv("processed_data/TestData/ForecastData/EWM.forecast.data_GFDL.WtrTemp.csv")
  IPSL.forecast=read_csv("processed_data/TestData/ForecastData/EWM.forecast.data_IPSL.WtrTemp.csv")
  MIROC5.forecast=read_csv("processed_data/TestData/ForecastData/EWM.forecast.data_MIROC5.WtrTemp.csv")
  MRI.forecast=read_csv("processed_data/TestData/ForecastData/EWM.forecast.data_MRI.WtrTemp.csv")
  
  library(magrittr)
  Fut.Temps=bind_cols(ACCESS.forecast[,1],ACCESS.forecast[,12],GFDL.forecast[,12],IPSL.forecast[,12],MIROC5.forecast[,12],MRI.forecast[,12])%>%
    set_colnames(c("DOWLKNUM","ACCESS.avg.ann.gdd","GFDL.avg.ann.gdd","IPSL.avg.ann.gdd","MIROC5.avg.ann.gdd","MRI.avg.ann.gdd"))
  
  Curr.Temps_Means=Curr.Temps%>%rowwise()%>%mutate(Curr.MeanTmp=mean(c_across(2:6)))%>%select("DOWLKNUM","Curr.MeanTmp")
  Curr.Temps_Means
  Futr.Temps_Means=Fut.Temps%>%rowwise()%>%mutate(Futr.MeanTmp=mean(c_across(2:6)))%>%select("DOWLKNUM","Futr.MeanTmp")
  Futr.Temps_Means
  Curr.Futr_MeanTemp=left_join(Curr.Temps_Means, Futr.Temps_Means, by="DOWLKNUM")
  Curr.Futr_MeanTemp
  
  GAM.k10_CurrFut_Temp_InvRisk=left_join(Curr.Futr_MeanTemp, Curr.Futr_MeanRisk, by="DOWLKNUM")
  GAM.k10_CurrFut_Temp_InvRisk
  GAM.k10_CurrFut_Temp_InvRisk_new=GAM.k3_CurrFut_Temp_InvRisk%>%mutate(MeanTmpChange=Futr.MeanTmp-Curr.MeanTmp, 
                                                                       MeanRiskChange=Futr.MeanRisk-Curr.MeanRisk)
  GAM.k10_CurrFut_Temp_InvRisk_new
  
  ggplot()+
    geom_segment(data=GAM.k10_CurrFut_Temp_InvRisk_new, 
                mapping=aes(x=Curr.MeanTmp, y=Curr.MeanRisk, xend=Curr.MeanTmp+MeanTmpChange, 
                yend=Curr.MeanRisk+MeanRiskChange,color=MeanRiskChange), arrow=arrow(), size=0.5) +
                geom_point(data=GAM.k10_CurrFut_Temp_InvRisk_new, mapping=aes(x=Curr.MeanTmp, y=Curr.MeanRisk), size=1, shape=21, fill="white")+
                scale_color_gradient2(high="#0072B2", low ="#D55E00")+xlab("Annual Growing Degree Days")+ylab("Invasion Risk")+
                labs(colour="Change\nin risk")+geom_vline(xintercept = 2205, lty=2)
  
  ggsave("InvasionRisk_ArrowPlot_GAM.k10.png", path="Figures/", device = "png",width = 7, height = 4.5 )

