library(tidyverse)

################################# FINAL PLOTTING OF BOTH ANALOG & NON-ANALOG DOMAINS #########################################
#### EFFECT OF ROADS AS THE MANIPULATED PREDICTOR

NegRisk_na=read_csv("Results/NegRisk_NonAnalog.csv")
NegRisk_a=read_csv("Results/NegRisk_Analog.csv")
NegRisk_CombinedDomains=bind_rows(NegRisk_a, NegRisk_na)

NegRisk_Combined_Roads=NegRisk_CombinedDomains%>%rowwise()%>%mutate(Mean.NegRisk=mean(c_across(1:5)))
NegRisk_Combined_Roads$Domain=c(rep("Analog",11),rep("Non-Analog",11))
NegRisk_Combined_Roads

ggplot(NegRisk_Combined_Roads,aes(RoadPercentile,Mean.NegRisk,col=Domain))+geom_point()+geom_smooth(method="loess")+
  ylab("Proportion of lakes \n with DECREASED risk")+xlab("Road density levels")
ggsave("NegRisk_Plot_RoadEffect.png", path="./Figures", device = "png",width = 6, height = 4.5 )

NegRisk_CombinedDomains2$RoadDensity=rep(Roads.quants,2)
NegRisk_CombinedDomains3=NegRisk_CombinedDomains2%>%filter(RoadDensity<100)
NegRisk_CombinedDomains3
ggplot(NegRisk_CombinedDomains3,aes(RoadDensity,Mean.NegRisk,col=Domain))+geom_point()+geom_smooth(method="loess")+
  ylab("Proportion of lakes \n with DECREASED risk")+xlab("Road density")
ggsave("NegRisk_Plot_RoadEffect_DensityVals.png", path="./Figures", device = "png",width = 6, height = 4.5 )

RoadEffect=ggplot(NegRisk_Combined_Roads,aes(RoadPercentile,1-Mean.NegRisk,col=Domain))+geom_point()+geom_smooth(method="loess")+
ylab("Proportion of lakes \n with predicted INCREASE in invasion risk")+xlab("Road density")+ylim(0.0,0.8)

png("Figures/RoadEffect.png",width=6, height=4, units="in", res=900)
RoadEffect
dev.off()

################################# FINAL PLOTTING OF BOTH ANALOG & NON-ANALOG DOMAINS #########################################
#### EFFECT OF SECCHI DEPTH AS THE MANIPULATED PREDICTOR
NegRisk_na=read_csv("Results/Secchi_NegRisk_NonAnalog.csv")
NegRisk_na
NegRisk_a=read_csv("Results/Secchi_NegRisk_Analog.csv")
NegRisk_a
NegRisk_CombinedDomains=bind_rows(NegRisk_a, NegRisk_na)

NegRisk_Combined_Secchi=NegRisk_CombinedDomains%>%rowwise()%>%mutate(Mean.NegRisk=mean(c_across(1:5)))
NegRisk_Combined_Secchi$Domain=c(rep("Analog",11),rep("Non-Analog",11))

NegRisk_Combined_Secchi

ggplot(NegRisk_Combined_Secchi,aes(SecchiPercentile,Mean.NegRisk,col=Domain))+geom_point()+geom_smooth(method="loess")+
  ylab("Proportion of lakes \n with DECREASED risk")+xlab("Secchi % levels")
ggsave("NegRisk_Plot_SecchiEffect_DensityVals.png", path="./Figures", device = "png",width = 6, height = 4.5 )

SecchiEffect=ggplot(NegRisk_Combined_Secchi,aes(SecchiPercentile,1-Mean.NegRisk,col=Domain))+geom_point()+geom_smooth(method="loess")+
ylab("Proportion of lakes \n with predicted INCREASE in invasion risk")+xlab("Secchi depth")+ylim(0.0,0.8)
gridExtra::grid.arrange(RoadEffect, SecchiEffect)

png("Figures/SecchiEffect.png",width=6, height=4, units="in", res=900)
SecchiEffect
dev.off()

