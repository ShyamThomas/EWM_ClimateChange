library(tidyverse)

##################### VISUALIZING CHANGE IN RISK RESULTS
#### MAPPING LAKES WITH CONSISTENT CHANGE IN RISK PREDICTIONS ACROSS ALL MODELS
## First, categorise the predicted change in risk estimates into 3 broad catehories: 'loss', 'no change', & ' gain'

RF_CurrFut_Temp_InvRisk_new ## from 1.2
GAM.k10_CurrFut_Temp_InvRisk_new ## from from 2.2
GAM.k3_CurrFut_Temp_InvRisk_new ## from 2.2

GAM.k3_ChangeStatus=GAM.k3_CurrFut_Temp_InvRisk_new%>%mutate(Change_Status= case_when(MeanRiskChange < -0.1 ~ 'loss',
MeanRiskChange < 0.1 ~ 'no change',
MeanRiskChange < 0.85 ~ 'gain'
))%>%select(DOWLKNUM, Change_Status)
GAM.k10_ChangeStatus=GAM.k10_CurrFut_Temp_InvRisk_new%>%mutate(Change_Status= case_when(MeanRiskChange < -0.1 ~ 'loss',
MeanRiskChange < 0.1 ~ 'no change',
MeanRiskChange < 0.85 ~ 'gain'
))%>%select(DOWLKNUM, Change_Status)
RF_ChangeStatus=RF_CurrFut_Temp_InvRisk_new%>%mutate(Change_Status= case_when(MeanRiskChange < -0.1 ~ 'loss',
MeanRiskChange < 0.1 ~ 'no change',
MeanRiskChange < 0.85 ~ 'gain'
))%>%select(DOWLKNUM, Change_Status)

AllSDMs_ChangeStatus.Lst=list(GAM.k3_ChangeStatus, GAM.k10_ChangeStatus, RF_ChangeStatus)
AllSDMs_ChangeStatus=AllSDMs_ChangeStatus.Lst%>%reduce(full_join, by='DOWLKNUM')

StatusCount=apply(AllSDMs_ChangeStatus[,-1],1,function(x) length(unique(x)))
StatusCount
AllSDMs_ChangeStatus$StatusCount=StatusCount
AllSDMs_ChangeStatus

EWM.GCMs.data
AllSDMs_ChangeStatus_GeoCoords=merge(EWM.GCMs.data[,1:5],AllSDMs_ChangeStatus, by="DOWLKNUM")
AllSDMs_ChangeStatus_GeoCoords
AllSDMs_ChangeStatus_GeoCoords.sf=st_as_sf(AllSDMs_ChangeStatus_GeoCoords, coords=c("UTMX", "UTMY"),crs=32615)
AllSDMs_ChangeStatus_GeoCoords.sf
AllSDMs_ChangeStatus_GeoCoords.sf2=AllSDMs_ChangeStatus_GeoCoords.sf%>%filter(StatusCount==1 |StatusCount==3)
AllSDMs_ChangeStatus_GeoCoords.sf2

ggplot(Minn.sf)+geom_sf()+geom_sf(data=AllSDMs_ChangeStatus_GeoCoords.sf2 ,aes(col=as.factor(StatusCount)), alpha=0.5, size=2)+
  scale_colour_viridis_d(name=" ", labels=c("Increase", "Uncertain"), option="turbo")+theme_minimal()+
  theme(text=element_text(size=16))+theme(legend.position = c(0.8, 0.4))
ggsave("ChangeInRisk_ConsensusMap.png", path="./Figures", device = "png",width = 6, height = 4.5 )
