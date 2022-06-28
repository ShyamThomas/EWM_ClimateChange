library(tidyverse)
library(tidybayes)
library(bayesplot)
library(brms)

Data_Combined = list.files(path="processed_data/TrainData/",pattern=".csv", full.names = TRUE) %>%
  lapply(read_csv)%>%
  bind_rows(id=NULL)%>%
  mutate(Id=rep(1:468,5))

Data_Combined%>%View()


TempAvgs=Data_Combined%>%group_by(Id)%>%summarise(
avgGDD=mean(c_across(ACCESS.avg.ann.gdd:MRI.avg.ann.gdd), na.rm=TRUE),
minGDD=min(c_across(ACCESS.avg.ann.gdd:MRI.avg.ann.gdd), na.rm=TRUE),
maxGDD=max(c_across(ACCESS.avg.ann.gdd:MRI.avg.ann.gdd), na.rm=TRUE)
)%>%ungroup()


Access.TrainData=read_csv("processed_data/TrainData/EWM.train.data_ACCESS.WtrTemp.csv")
colnames(Access.TrainData)
Avg.CurrTemp.data=bind_cols(Access.TrainData[,-12],TempAvgs[,2:4])
Avg.CurrTemp.data

### We need only 3 predictors
Curr.Brm.TrainData=Avg.CurrTemp.data[,c(1,5,11,12)]
Curr.Brm.TrainData

colnames(Curr.Brm.TrainData)[2:4]=c("avgSecchi", "avgRoads", "avgGDD")

ewm.brm.trial <- brm(bf(EWMSTATUS_corrRelFrq ~ s(avgGDD, k=10)+s(avgSecchi, k=10)+s(avgRoads, k=10)),
data = Curr.Brm.TrainData, family = bernoulli(), cores = 4, seed = 17,
iter = 4000, warmup = 1000, thin = 10, refresh = 0,
control = list(adapt_delta = 0.99))

summary(ewm.brm.trial)
pp_check(ewm.brm.trial)
plot(ewm.brm.trial)
post_brms.trial=posterior_samples(ewm.brm.trial)

### Lets make posterior predictions from sample draws
ewm.epreds=posterior_epred(ewm.brm.trial)
str(ewm.epreds)
head(ewm.epreds)


FutData_Combined = list.files(path="processed_data/TestData/ForecastData",pattern=".csv", full.names = TRUE) %>%
lapply(read_csv)%>%
bind_rows(id=NULL)


Fut.Brm.TestData=FutData_Combined%>%group_by(DOWLKNUM)%>%summarise(
avgGDD=mean(avg.ann.gdd),
avgSecchi=mean(avg_secchi),
avgRoads=mean(roaddensity_density_mperha)
)

Fut.Brm.TestData
ewm.epreds.forecast=posterior_epred(ewm.brm.trial, newdata =Fut.Brm.TestData[,-1], draw_ids =  c(1001:1200))

avg.fut.preds=colMeans(fut.ewm.epreds)
sd.fut.preds=colSds(fut.ewm.epreds)
var.fut.preds=colVars(fut.ewm.epreds)
plot(Fut.Brm.TestData$avgGDD, avg.fut.preds, abline(v=2200))
plot(Fut.Brm.TestData$avgGDD, var.fut.preds, abline(v=2200))

ggplot(Minn.sf)+geom_sf()+geom_sf(data=Fut.Brm.Preds_EWMGeoIndex ,aes(col=var.fut.preds), alpha=0.5, size=2)

