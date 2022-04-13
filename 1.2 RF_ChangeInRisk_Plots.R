library(tidyverse)
library(ggplot2)

#################################### PLOT 1: CHANGE IN RISK ARROW PLOTS AS A FUNCTION OF WATER TEMP ######################################

##From 1.0 get the data needed: 
EWM.GCMs.data=read_csv("processed_data/EWM.prsabs95to15_AllGCMs.csv")
Futr.Preds=read_csv("Results/Futr.Predictions.csv")
Curr.Preds=read_csv("Results/Curr.Predictions.csv")

Curr.Preds_means=Curr.Preds%>%rowwise()%>%mutate(Curr.MeanRisk=mean(c_across(1:5)))%>%select("DOWLKNUM", "Curr.MeanRisk")
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

RF_CurrFut_Temp_InvRisk=left_join(Curr.Futr_MeanTemp, Curr.Futr_MeanRisk, by="DOWLKNUM")
RF_CurrFut_Temp_InvRisk
RF_CurrFut_Temp_InvRisk_new=RF_CurrFut_Temp_InvRisk%>%mutate(MeanTmpChange=Futr.MeanTmp-Curr.MeanTmp, 
                                                            MeanRiskChange=Futr.MeanRisk-Curr.MeanRisk)
RF_CurrFut_Temp_InvRisk_new

#### Plot the final temperature - invasion risk results to show relative change in risk
ggplot()+
geom_segment(data=RF_CurrFut_Temp_InvRisk_new, 
             mapping=aes(x=Curr.MeanTmp, y=Curr.MeanRisk, xend=Curr.MeanTmp+MeanTmpChange, 
                                  yend=Curr.MeanRisk+MeanRiskChange,color=MeanRiskChange), 
                                                                 arrow=arrow(), size=0.5) +
geom_point(data=RF_CurrFut_Temp_InvRisk_new, mapping=aes(x=Curr.MeanTmp, y=Curr.MeanRisk), size=1, shape=21, fill="white")+
scale_color_gradient2(high="#0072B2", low ="#D55E00")+xlab("Annual Growing Degree Days")+ylab("Invasion Risk")+
labs(colour="Change\nin risk")+geom_vline(xintercept = 2205, lty=2)
ggsave("InvasionRisk_ArrowPlot_RF.png", path="Figures/", device = "png",width = 6, height = 4.5 )


######################################## PLOT 2: CHANGE IN RISK PLOTTED IN DIFFERENT COVARIATE SPACE ######################################
EWM.subdata=EWM.GCMs.data[,c(1,4,5,8:18)]
EWM.subdata.corr=EWM.subdata%>%filter(DOWLKNUM!=47004901)
EWM.subdata.corr2=EWM.subdata.corr[,c(1:3,7,13)]
EWM.subdata.corr2
RF_CurrFut_Temp_InvRisk_KeyCov=left_join(EWM.subdata.corr2,RF_CurrFut_Temp_InvRisk_new, by="DOWLKNUM")


p.secchi=ggplot(RF_CurrFut_Temp_InvRisk_KeyCov, mapping=aes(x=Curr.MeanTmp+MeanTmpChange, y= avg_secchi, color=MeanRiskChange))+
                                                                                              geom_point(shape=21, stroke=1.25)+
                                        scale_color_gradient2(high="#0072B2", low ="#D55E00")+labs(colour = "Change\n in risk") + 
                                 xlab("Annual growing degree days") + ylab("Secchi depth")+geom_vline(xintercept = 2205, lty=2)

p.roads=ggplot(RF_CurrFut_Temp_InvRisk_KeyCov, mapping=aes(x=Curr.MeanTmp+MeanTmpChange, y= roaddensity_density_mperha,
                                                              color=MeanRiskChange))+geom_point(shape=21, stroke=1.25)+
                              scale_color_gradient2(high="#0072B2", low ="#D55E00")+labs(colour = "Change\n in risk") + 
                          xlab("Annual growing degree days") + ylab("Road density")+geom_vline(xintercept = 2205, lty=2)

p.lat=ggplot(RF_CurrFut_Temp_InvRisk_KeyCov, mapping=aes(x=Curr.MeanTmp+MeanTmpChange, y= LAT, color=MeanRiskChange))+
                                                                                    geom_point(shape=21, stroke=1.25)+
                              scale_color_gradient2(high="#0072B2", low ="#D55E00")+labs(colour = "Change\n in risk") + 
                           xlab("Annual growing degree days") + ylab("Latitude")+geom_vline(xintercept = 2205, lty=2)



