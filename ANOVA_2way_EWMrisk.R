library(Matrix)
library(tidyverse)

### From line 84 in USpatial_dataset.R
#Final_MergedData=left_join(EWM.CurrFutr.Preds.Doms,curr.preds.GAM_k3[,1:6], by="DOWLKNUM")%>%
 # left_join(.,futr.preds.GAM_k3[,1:6], by="DOWLKNUM")%>%
# left_join(.,curr.preds.GAM_k10[,1:6], by="DOWLKNUM")%>%
 # left_join(.,futr.preds.GAM_k10[,1:6], by="DOWLKNUM")%>%
  #left_join(., RF_curr.preds[,-6],by="DOWLKNUM")%>%
  #left_join(., RF_futr.preds[,-6],by="DOWLKNUM")


MergedCurrData=left_join(EWM.CurrFutr.Preds.Doms[,1:15],curr.preds.GAM_k3[,1:6], by="DOWLKNUM")%>%
  left_join(.,curr.preds.GAM_k10[,1:6], by="DOWLKNUM")%>%
  left_join(., RF_curr.preds[,-6],by="DOWLKNUM")

MergedCurrData  

ggplot(Minn.sf)+geom_sf()+geom_sf(data=Sum.Sq_prpn_sf, aes(col=GCM))+scale_color_viridis_c(name="Propn. of TSS \n(GCMs)")+theme_minimal()+
AllResults_CurrPredslong=MergedCurrData[,c(1,3,4,16:30)]%>%pivot_longer(
cols=ends_with("CurrPred"),
values_to = "CurrPreds"
)

AllResults_CurrPredslong

SDM_string=c(rep("GAMk3",5),rep("GAMk10",5),rep("RF",5))
SDM_string

AllResults_CurrPredslong_newFacts=AllResults_CurrPredslong%>%mutate(SDM=rep(SDM_string, 578), GCM=rep(c("ACCESS","GFDL","IPSL","MIROC5","MRI"), 1734))
AllResults_CurrPredslong_newFacts

######################################################
### Now lets do the  Future predictions data
MergedFutrData=left_join(EWM.CurrFutr.Preds.Doms[,c(1:10,16:25)],futr.preds.GAM_k3[,1:6], by="DOWLKNUM")%>%
  left_join(.,futr.preds.GAM_k10[,1:6], by="DOWLKNUM")%>%
  left_join(., RF_futr.preds[,-6],by="DOWLKNUM")
MergedFutrData

AllResults_FutrPredslong=MergedFutrData[,c(1,3,4,21:35)]%>%pivot_longer(
  cols=ends_with("FutrPred"),
  values_to = "FutrPreds"
)

AllResults_FutrPredslong_newFacts=AllResults_FutrPredslong%>%mutate(SDM=rep(SDM_string, 578), GCM=rep(c("ACCESS","GFDL","IPSL","MIROC5","MRI"), 1734))
AllResults_FutrPredslong_newFacts

f
final_changeinrisk=bind_cols(AllResults_CurrPredslong_newFacts[,-4],AllResults_FutrPredslong_newFacts[,c(1,5)])%>%
  mutate(ChangeInRisk=FutrPreds-CurrPreds)
final_changeinrisk

Sum.Sq_list=final_changeinrisk%>%group_split(DOWLKNUM...1)%>%
map(~ summary(aov(ChangeInRisk ~ SDM+GCM, data = .))[[1]]$'Sum Sq')
class(Sum.Sq_list)
Sum.Sq_DF=as.data.frame(do.call(rbind, Sum.Sq_list))
colnames(Sum.Sq_DF)=c("SDM", "GCM", "Residual")
Sum.Sq_prpn=Sum.Sq_DF/rowSums(Sum.Sq_DF)
Sum.Sq_prpn$DOWLKNUM=curr.preds.df$DOWLKNUM
Sum.Sq_prpn$LON=EWM.CurrFutr.Preds.Doms$LON
Sum.Sq_prpn$LAT=EWM.CurrFutr.Preds.Doms$LAT
head(Sum.Sq_prpn)
boxplot(Sum.Sq_prpn[,1:3])

AvgTemp.Domain=EWM.CurrFutr.Preds.Doms[,1:4]
AvgTemp.Domain
AvgTemp.Domain$AvgFutrTemp=rowMeans(EWM.CurrFutr.Preds.Doms[,16:20])
AvgTemp.Domain
range(AvgTemp.Domain$AvgFutrTemp)
AvgTemp.Domain=AvgTemp.Domain%>%mutate(Domain=case_when(AvgFutrTemp > 2220 ~ 'A', AvgFutrTemp < 2220 ~ 'NA'))
Sum.Sq_prpn.Dom=left_join(Sum.Sq_prpn,AvgTemp.Domain[,c(1,5,6)], by="DOWLKNUM")
head(Sum.Sq_prpn.Dom)

library(reshape2)
Sum.Sq_prpn.Dom_melt=melt(Sum.Sq_prpn.Dom, id=c("DOWLKNUM", "LON", "LAT", "Domain", "AvgFutrTemp"))
head(Sum.Sq_prpn.Dom_melt)
ggplot(Sum.Sq_prpn.Dom_melt,aes(x=variable, y=value, fill=Domain))+
geom_boxplot()

geom_point(position=position_jitterdodge(0.2), alpha=0.2)+ylab("Propn. of Total Sum of Squares")+xlab(" ")+
theme(text=element_text(size=16))+theme(legend.position = c(0.85, 0.85))
ggsave("VariancePartionBySDMnDomain.png", path="Figures/", device="png",width=6, height=4.5)

Minn.sf=st_transform(Minn.sf, crs=32615)
Minn.sf
Sum.Sq_prpn_sf=st_as_sf(Sum.Sq_prpn,coords=c("LON", "LAT"),crs=32615)
Sum.Sq_prpn_sf

ggplot(Minn.sf)+geom_sf()+geom_sf(data=Sum.Sq_prpn_sf, aes(col=SDM))+scale_color_viridis_c(name="Variance \n(SDMs)")+theme_minimal()+
theme(text=element_text(size=16))+theme(legend.position = c(0.8, 0.4))
ggsave("PropnTSS_SDM_map.png", path="Figures/", device="png",width=4.5, height=6)
ggplot(Minn.sf)+geom_sf()+geom_sf(data=Sum.Sq_prpn_sf, aes(col=GCM))+scale_color_viridis_c(name="Propn. of TSS \n(GCMs)")+theme_minimal()+
theme(text=element_text(size=16))+theme(legend.position = c(0.85, 0.4))
ggsave("PropnTSS_GCM_map.png", path="Figures/", device="png",width=4.5, height=6)


