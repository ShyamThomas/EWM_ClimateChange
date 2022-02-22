library(tidyverse)
library(ggplot2)

#################################### PLOT 1: CHANGE IN RISK ARROW PLOTS AS A FUNCTION OF WATER TEMP ######################################

##From 1.0 get the data needed: 
#### EWM.GCMs.data=read_csv("processed_data/EWM.prsabs95to15_AllGCMs.csv")

setdiff(EWM.GCMs.data$DOWLKNUM,Test$DOWLKNUM) ## Ensure training data DOWs and test data DOWs match perfectly
curr.preds.df$DOWLKNUM=EWM.GCMs.data$DOWLKNUM
fut.preds.df$DOWLKNUM=Test$DOWLKNUM ### Test data came from the iterations in 1.1

dim(fut.preds.df)
dim(curr.preds.df)
head(curr.preds.df)
head(fut.preds.df)

curr.preds.df.new=curr.preds.df%>%filter(DOWLKNUM!=47004901) ### Remove the only missing DOW in test dataset from train dataset too
head(curr.preds.df.new)

### Estimate average risk across the different GCMs
curr.preds.df.new$AvgRiskPred=rowMeans(curr.preds.df.new[,c(1:5)])
head(curr.preds.df.new)
fut.preds.df$AvgRiskPred=rowMeans(fut.preds.df[,c(1:5)])
head(fut.preds.df)
## More subsetting and merging
AvgCurr.Risk=curr.preds.df.new[,c(7:8)]
head(AvgCurr.Risk)
AvgFut.Risk=fut.preds.df[,c(7:8)]
head(AvgFut.Risk)
AvgRisk_CurrFut=merge(AvgCurr.Risk,AvgFut.Risk, by="DOWLKNUM")
head(AvgRisk_CurrFut)
colnames(AvgRisk_CurrFut)=c("DOWLKNUM", "Curr.AvgRisk", "Fut.AvgRisk")
AvgRisk_CurrFut$RiskChange=AvgRisk_CurrFut[,3]-AvgRisk_CurrFut[,2]
head(AvgRisk_CurrFut)
hist(AvgRisk_CurrFut$RiskChange)

### Read the temperature data and repeat the above steps
Curr.Temps=EWM.GCMs.data[,c(1,19:23)]%>%filter(DOWLKNUM!=47004901)
Curr.Temps
### Put together all the forecasted future temperatures
ACCESS.forecast=read_csv("processed_data/TestData/ForecastData/EWM.forecast.data_ACCESS.WtrTemp.csv")
GFDL.forecast=read_csv("processed_data/TestData/ForecastData/EWM.forecast.data_GFDL.WtrTemp.csv")
IPSL.forecast=read_csv("processed_data/TestData/ForecastData/EWM.forecast.data_IPSL.WtrTemp.csv")
MIROC5.forecast=read_csv("processed_data/TestData/ForecastData/EWM.forecast.data_MIROC5.WtrTemp.csv")
MRI.forecast=read_csv("processed_data/TestData/ForecastData/EWM.forecast.data_MRI.WtrTemp.csv")

Fut.Temps=bind_cols(ACCESS.forecast[,1],ACCESS.forecast[,12],GFDL.forecast[,12],IPSL.forecast[,12],MIROC5.forecast[,12],MRI.forecast[,12])%>%
  set_colnames(c("DOWLKNUM","ACCESS.avg.ann.gdd","GFDL.avg.ann.gdd","IPSL.avg.ann.gdd","MIROC5.avg.ann.gdd","MRI.avg.ann.gdd"))
  
Curr.Temps$AvgTemp=rowMeans(Curr.Temps[,2:6])
Curr.Temps
Fut.Temps$AvgTemp=rowMeans(Fut.Temps[,2:6])
Fut.Temps
AvgWtrTmps_CurrFut=left_join(Curr.Temps[,c(1,7)] ,Fut.Temps[,c(1,7)], by="DOWLKNUM")%>%
  set_colnames(c("DOWLKNUM","AvgCurr.WtrTmp","AvgFut.WtrTmp"))
AvgWtrTmps_CurrFut.df=as.data.frame(AvgWtrTmps_CurrFut)
AvgWtrTmps_CurrFut.df$TmpChange=AvgWtrTmps_CurrFut.df$AvgFut.WtrTmp- AvgWtrTmps_CurrFut.df$AvgCurr.WtrTmp
head(AvgWtrTmps_CurrFut.df)

Avg.RiskWtrTmp_CurrFut=merge(AvgWtrTmps_CurrFut.df,AvgRisk_CurrFut, by="DOWLKNUM")
head(Avg.RiskWtrTmp_CurrFut)
str(Avg.RiskWtrTmp_CurrFut)

#### Plot the final temperature - invasion risk results to show relative change in risk
ggplot()+
  geom_segment(data=Avg.RiskWtrTmp_CurrFut, mapping=aes(x=AvgCurr.WtrTmp, y=Curr.AvgRisk, xend=AvgCurr.WtrTmp+TmpChange, yend=Curr.AvgRisk+RiskChange,color=RiskChange), arrow=arrow(), size=0.5) +
  geom_point(data=Avg.RiskWtrTmp_CurrFut, mapping=aes(x=AvgCurr.WtrTmp, y=Curr.AvgRisk), size=1, shape=21, fill="white")+
  scale_color_gradient2(high="#0072B2", low ="#D55E00")+xlab("Annual Growing Degree Days")+ylab("Invasion Risk")+
  labs(colour="Change\nin risk")+geom_vline(xintercept = 2205, lty=2)

######################################## PLOT 2: CHANGE IN RISK PLOTTED IN DIFFERENT COVARIATE SPACE ######################################
EWM.subdata=EWM.GCMs.data[,c(1,4,5,8:18)]
EWM.subdata.corr=EWM.subdata%>%filter(DOWLKNUM!=47004901)
EWM.subdata.corr
head(Avg.RiskWtrTmp_CurrFut)
EWM.subdata.corr=EWM.subdata%>%filter(DOWLKNUM!=47004901)
EWM.subdata.corr2=EWM.subdata.corr[,c(1:3,7,13)]
EWM.subdata.corr2

RiskTmpCurrFut_Cov=merge(Avg.RiskWtrTmp_CurrFut,EWM.subdata.corr2, by="DOWLKNUM")
head(RiskTmpCurrFut_Cov)


p.secchi=ggplot(RiskTmpCurrFut_Cov, mapping=aes(x=AvgCurr.WtrTmp+TmpChange, y= avg_secchi, color=RiskChange))+geom_point(shape=21, stroke=1.25)+
  scale_color_gradient2(high="#0072B2", low ="#D55E00")+labs(colour = "Change\n in risk") + 
  xlab("Annual growing degree days") + ylab("Secchi depth")+geom_vline(xintercept = 2205, lty=2)

p.roads=ggplot(RiskTmpCurrFut_Cov, mapping=aes(x=AvgCurr.WtrTmp+TmpChange, y= roaddensity_density_mperha, color=RiskChange))+geom_point(shape=21, stroke=1.25)+
  scale_color_gradient2(high="#0072B2", low ="#D55E00")+labs(colour = "Change\n in risk") + 
  xlab("Annual growing degree days") + ylab("Road density")+geom_vline(xintercept = 2205, lty=2)

p.lat=ggplot(RiskTmpCurrFut_Cov, mapping=aes(x=AvgCurr.WtrTmp+TmpChange, y= LAT, color=RiskChange))+geom_point(shape=21, stroke=1.25)+
  scale_color_gradient2(high="#0072B2", low ="#D55E00")+labs(colour = "Change\n in risk") + 
  xlab("Annual growing degree days") + ylab("Latitude")+geom_vline(xintercept = 2205, lty=2)



