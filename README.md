# CaseStudy-Projects
The repository includes the case studies which carried out related to the Data Science, AI, Machine Learning algorithms. This is for code storage purpose.

#Bosch Case Study Project
library(jsonlite)
library(ggplot2)
library(randomForest)
PART_0<-stream_in(file("PART_0.JsON"))
PART_0_unnorm <- stream_in(file("PART_0.JSON"))
attach(PART_0)
head(PART_0)
summary(PART_0_unnorm$cu_sw)
PART_0 <-PART_0[,-which(names(PART_0) %in% c("outdoor_temp_max","outdoor_temp_min","outdoor_temp_mean") )]
PART_0$cu_sw<-as.numeric(PART_0$cu_sw)
PART_0<- PART_0[,-which(names(PART_0) %in% c("burner_typ","fault_lock_count","pump_usage_mean_in_operation","appliance_name","cu_typ","code_plug","hw_sys") )]
for(i in 1:ncol(PART_0)){
  PART_0[is.na(PART_0[,i]), i] <- median(PART_0[,i],na.rm=TRUE)
}
PART_0[is.na(PART_0)]<-0
attach(PART_0)

#range01 <- function(x){(x-min(x))/(max(x)-min(x))}

#attach(PART_0_divided)
min(energy_kwh)
max(energy_kwh)
min<-min(energy_kwh)
max<-max(energy_kwh)
length(energy_kwh)
for(i in 1:50640){
  energy_kwh[i]<-(energy_kwh[i]-min)/(max-min)
}
PART_0$energy_kwh<-energy_kwh


min(pow_hw_mean)
max(pow_hw_mean)
min<-min(pow_hw_mean)
max<-max(pow_hw_mean)
length(pow_hw_mean)
for(i in 1:50640){
  pow_hw_mean[i]<-(pow_hw_mean[i]-min)/(max-min)
}
PART_0$pow_hw_mean<-pow_hw_mean

min(no_ch_starts)
max(no_ch_starts)
min<-min(no_ch_starts)
max<-max(no_ch_starts)
length(no_ch_starts)
for(i in 1:50640){
  no_ch_starts[i]<-(no_ch_starts[i]-min)/(max-min)
}
PART_0$no_ch_starts<-no_ch_starts

min(no_ch_pump_starts)
max(no_ch_pump_starts)
min<-min(no_ch_pump_starts)
max<-max(no_ch_pump_starts)
length(no_ch_pump_starts)
for(i in 1:50640){
  no_ch_pump_starts[i]<-(no_ch_pump_starts[i]-min)/(max-min)
}
PART_0$no_ch_pump_starts<-no_ch_pump_starts


min(pow_uptime)
max(pow_uptime)
min<-min(pow_uptime)
max<-max(pow_uptime)
length(pow_uptime)
for(i in 1:50640){
  pow_uptime[i]<-(pow_uptime[i]-min)/(max-min)
}
PART_0$pow_uptime<-pow_uptime

min(data_loss)
max(data_loss)
min<-min(data_loss)
max<-max(data_loss)
length(data_loss)
for(i in 1:50640){
  data_loss[i]<-(data_loss[i]-min)/(max-min)
}
PART_0$data_loss<-data_loss

min(no_hw_taps)
max(no_hw_taps)
min<-min(no_hw_taps)
max<-max(no_hw_taps)
length(no_hw_taps)
for(i in 1:50640){
  no_hw_taps[i]<-(no_hw_taps[i]-min)/(max-min)
}
PART_0$no_hw_taps<-no_hw_taps


min(prim_t_mean)
max(prim_t_mean)
min<-min(prim_t_mean)
max<-max(prim_t_mean)
length(prim_t_mean)
for(i in 1:50640){
  prim_t_mean[i]<-(prim_t_mean[i]-min)/(max-min)
}
PART_0$prim_t_mean<-prim_t_mean


min(pow_ch_uptime)
max(pow_ch_uptime)
min<-min(pow_ch_uptime)
max<-max(pow_ch_uptime)
length(pow_ch_uptime)
for(i in 1:50640){
  pow_ch_uptime[i]<-(pow_ch_uptime[i]-min)/(max-min)
}
PART_0$pow_ch_uptime<-pow_ch_uptime


min(energy_ch_p)
max(energy_ch_p)
min<-min(energy_ch_p)
max<-max(energy_ch_p)
length(energy_ch_p)
for(i in 1:50640){
  energy_ch_p[i]<-(energy_ch_p[i]-min)/(max-min)
}
PART_0$energy_ch_p<-energy_ch_p


min(no_hw_rebound_starts)
max(no_hw_rebound_starts)
min<-min(no_hw_rebound_starts)
max<-max(no_hw_rebound_starts)
length(no_hw_rebound_starts)
for(i in 1:50640){
  no_hw_rebound_starts[i]<-(no_hw_rebound_starts[i]-min)/(max-min)
}
PART_0$no_hw_rebound_starts<-no_hw_rebound_starts

min(pow_ch_mean)
max(pow_ch_mean)
min<-min(pow_ch_mean)
max<-max(pow_ch_mean)
length(pow_ch_mean)
for(i in 1:50640){
  pow_ch_mean[i]<-(pow_ch_mean[i]-min)/(max-min)
}
PART_0$pow_ch_mean<-pow_ch_mean

min(prim_t_set_max)
max(prim_t_set_max)
min<-min(prim_t_set_max)
max<-max(prim_t_set_max)
length(prim_t_set_max)
for(i in 1:50640){
  prim_t_set_max[i]<-(prim_t_set_max[i]-min)/(max-min)
}
PART_0$prim_t_set_max<-prim_t_set_max


min(hw_flow_mean_in_operation)
max(hw_flow_mean_in_operation)
min<-min(hw_flow_mean_in_operation)
max<-max(hw_flow_mean_in_operation)
length(hw_flow_mean_in_operation)
for(i in 1:50640){
  hw_flow_mean_in_operation[i]<-(hw_flow_mean_in_operation[i]-min)/(max-min)
}
PART_0$hw_flow_mean_in_operation<-hw_flow_mean_in_operation

min(hw_t_outlet_mean)
max(hw_t_outlet_mean)
min<-min(hw_t_outlet_mean)
max<-max(hw_t_outlet_mean)
length(hw_t_outlet_mean)
for(i in 1:50640){
  hw_t_outlet_mean[i]<-(hw_t_outlet_mean[i]-min)/(max-min)
}
PART_0$hw_t_outlet_mean<-hw_t_outlet_mean


min(hw_taps_energy)
max(hw_taps_energy)
min<-min(hw_taps_energy)
max<-max(hw_taps_energy)
length(hw_taps_energy)
for(i in 1:50640){
  hw_taps_energy[i]<-(hw_taps_energy[i]-min)/(max-min)
}
PART_0$hw_taps_energy<-hw_taps_energy

min(dhw_comfort_mode_duration)
max(dhw_comfort_mode_duration)
min<-min(dhw_comfort_mode_duration)
max<-max(dhw_comfort_mode_duration)
length(dhw_comfort_mode_duration)
for(i in 1:50640){
  dhw_comfort_mode_duration[i]<-(dhw_comfort_mode_duration[i]-min)/(max-min)
}
PART_0$dhw_comfort_mode_duration<-dhw_comfort_mode_duration

min(nom_max_pow_ch_kw)
max(nom_max_pow_ch_kw)
min<-min(nom_max_pow_ch_kw)
max<-max(nom_max_pow_ch_kw)
length(nom_max_pow_ch_kw)
for(i in 1:50640){
  nom_max_pow_ch_kw[i]<-(nom_max_pow_ch_kw[i]-min)/(max-min)
}
PART_0$nom_max_pow_ch_kw<-nom_max_pow_ch_kw


min(nom_max_pow_dhw_kw)
max(nom_max_pow_dhw_kw)
min<-min(nom_max_pow_dhw_kw)
max<-max(nom_max_pow_dhw_kw)
length(nom_max_pow_dhw_kw)
for(i in 1:50640){
  nom_max_pow_dhw_kw[i]<-(nom_max_pow_dhw_kw[i]-min)/(max-min)
}
PART_0$nom_max_pow_dhw_kw<-nom_max_pow_dhw_kw

min(cu_sw)
max(cu_sw)
min<-min(cu_sw)
max<-max(cu_sw)
length(cu_sw)
for(i in 1:50640){
  cu_sw[i]<-(cu_sw[i]-min)/(max-min)
}
PART_0$cu_sw<-cu_sw

min(boil_op_time_min)
max(boil_op_time_min)
min<-min(boil_op_time_min)
max<-max(boil_op_time_min)
length(boil_op_time_min)
for(i in 1:50640){
  boil_op_time_min[i]<-(boil_op_time_min[i]-min)/(max-min)
}
PART_0$boil_op_time_min<-boil_op_time_min

min(hw_taps_energy_kwh)
max(hw_taps_energy_kwh)
min<-min(hw_taps_energy_kwh)
max<-max(hw_taps_energy_kwh)
length(hw_taps_energy_kwh)
for(i in 1:50640){
  hw_taps_energy_kwh[i]<-(hw_taps_energy_kwh[i]-min)/(max-min)
}
PART_0$hw_taps_energy_kwh<-hw_taps_energy_kwh


min(energy_p)
max(energy_p)
min<-min(energy_p)
max<-max(energy_p)
length(energy_p)
for(i in 1:50640){
  energy_p[i]<-(energy_p[i]-min)/(max-min)
}
PART_0$energy_p<-energy_p

min(msg_count)
max(msg_count)
min<-min(msg_count)
max<-max(msg_count)
length(msg_count)
for(i in 1:50640){
  msg_count[i]<-(msg_count[i]-min)/(max-min)
}
PART_0$msg_count<-msg_count

min(fault_block_count)
max(fault_block_count)
min<-min(fault_block_count)
max<-max(fault_block_count)
length(fault_block_count)
for(i in 1:50640){
  fault_block_count[i]<-(fault_block_count[i]-min)/(max-min)
}
PART_0$fault_block_count<-fault_block_count


min(burn_op_time_min)
max(burn_op_time_min)
min<-min(burn_op_time_min)
max<-max(burn_op_time_min)
length(burn_op_time_min)
for(i in 1:50640){
  burn_op_time_min[i]<-(burn_op_time_min[i]-min)/(max-min)
}
PART_0$burn_op_time_min<-burn_op_time_min

min(pow_mean_on_uptime)
max(pow_mean_on_uptime)
min<-min(pow_mean_on_uptime)
max<-max(pow_mean_on_uptime)
length(pow_mean_on_uptime)
for(i in 1:50640){
  pow_mean_on_uptime[i]<-(pow_mean_on_uptime[i]-min)/(max-min)
}
PART_0$pow_mean_on_uptime<-pow_mean_on_uptime

min(sampled_period)
max(sampled_period)
min<-min(sampled_period)
max<-max(sampled_period)
length(sampled_period)
for(i in 1:50640){
  sampled_period[i]<-(sampled_period[i]-min)/(max-min)
}
PART_0$sampled_period<-sampled_period

min(hw_t_set_max)
max(hw_t_set_max)
min<-min(hw_t_set_max)
max<-max(hw_t_set_max)
length(hw_t_set_max)
for(i in 1:50640){
  hw_t_set_max[i]<-(hw_t_set_max[i]-min)/(max-min)
}
PART_0$hw_t_set_max<-hw_t_set_max

min(pow_mean)
max(pow_mean)
min<-min(pow_mean)
max<-max(pow_mean)
length(pow_mean)
for(i in 1:50640){
  pow_mean[i]<-(pow_mean[i]-min)/(max-min)
}
PART_0$pow_mean<-pow_mean

min(pow_hw_uptime)
max(pow_hw_uptime)
min<-min(pow_hw_uptime)
max<-max(pow_hw_uptime)
length(pow_hw_uptime)
for(i in 1:50640){
  pow_hw_uptime[i]<-(pow_hw_uptime[i]-min)/(max-min)
}
PART_0$pow_hw_uptime<-pow_hw_uptime

min(energy_ch_kwh)
max(energy_ch_kwh)
min<-min(energy_ch_kwh)
max<-max(energy_ch_kwh)
length(energy_ch_kwh)
for(i in 1:50640){
  energy_ch_kwh[i]<-(energy_ch_kwh[i]-min)/(max-min)
}
PART_0$energy_ch_kwh<-energy_ch_kwh

min(no_starts)
max(no_starts)
min<-min(no_starts)
max<-max(no_starts)
length(no_starts)
for(i in 1:50640){
  no_starts[i]<-(no_starts[i]-min)/(max-min)
}
PART_0$no_starts<-no_starts

min(burn_no_start)
max(burn_no_start)
min<-min(burn_no_start)
max<-max(burn_no_start)
length(burn_no_start)
for(i in 1:50640){
  burn_no_start[i]<-(burn_no_start[i]-min)/(max-min)
}
PART_0$burn_no_start<-burn_no_start

min(no_hw_starts)
max(no_hw_starts)
min<-min(no_hw_starts)
max<-max(no_hw_starts)
length(no_hw_starts)
for(i in 1:50640){
  no_hw_starts[i]<-(no_hw_starts[i]-min)/(max-min)
}
PART_0$no_hw_starts<-no_hw_starts

min(energy_hw_p)
max(energy_hw_p)
min<-min(energy_hw_p)
max<-max(energy_hw_p)
length(energy_hw_p)
for(i in 1:50640){
  energy_hw_p[i]<-(energy_hw_p[i]-min)/(max-min)
}
PART_0$energy_hw_p<-energy_hw_p

min(hw_taps_duration)
max(hw_taps_duration)
min<-min(hw_taps_duration)
max<-max(hw_taps_duration)
length(hw_taps_duration)
for(i in 1:50640){
  hw_taps_duration[i]<-(hw_taps_duration[i]-min)/(max-min)
}
PART_0$hw_taps_duration<-hw_taps_duration

min(energy_hw_kwh)
max(energy_hw_kwh)
min<-min(energy_hw_kwh)
max<-max(energy_hw_kwh)
length(energy_hw_kwh)
for(i in 1:50640){
  energy_hw_kwh[i]<-(energy_hw_kwh[i]-min)/(max-min)
}
PART_0$energy_hw_kwh<-energy_hw_kwh

attach(PART_0)

model <- lm( energy_kwh~ ., data = PART_0)
ols_step_backward_p(model)
attach(PART_0_divided)
PART_0_divided<- PART_0_divided[,-6]
#install.packages("leaps")
library(leaps)
#install.packages("faraway")
x<-model.matrix(energy_kwh~. -1 ,data=PART_0_divided)
y<- PART_0_divided$energy_kwh
attach(PART_0_divided)
leaps<-regsubsets(energy_kwh~.,data = PART_0_divided,nbest=10)
                  
plot(leaps, scale="adjr2",cex.axis=2.5)
plot(leaps, scale="bic",cex.=2.5)
leaps.summary<-summary(leaps)
write.csv(PART_0,'PART_0.csv')
plot(leaps.summary$rsq,xlab = "number of variables",ylab = "Rsquare",type='l')
plot(leaps.summary$rss,xlab = "number of variables",ylab = "RSS",type='l')
which.max(leaps.summary$adjr2)
PART_0_divided<-PART_0_divided[,-c(4,5,6,7,8,9)]
PART_0_divided<- PART_0[which(PART_0$sysid=="kFt1vogbLItST_BoJB_s_g=="),]
PART_0_divided$date<- as.Date(PART_0_divided$date, "%Y-%m-%d")
PART_0_divided<-PART_0_divided[order(as.Date(PART_0_divided$date, "%Y-%m-%d"), decreasing = TRUE),] 
PART_0_divided<-PART_0_divided[order(rev(as.Date(PART_0_divided$date, "%Y-%m-%d")), decreasing = TRUE),] 
#install.packages("car")
#install.packages("rlang")
library(car)
unique(PART_0_unnorm["cu_typ"])
length(PART_0_unnorm["cu_typ" == "HTIII"])
a<-lm(energy_hw_kwh~.,PART_0)
influencePlot(a)
attach(PART_0)
install.packages("relaimpo")
library(relaimpo)
lmMod <- lm(energy_kwh ~ . , data = Uncor_PART_0)  # fit lm() model
relImportance <- calc.relimp(lmMod, type = "lmg", rela = TRUE)  # calculate relative import
sort(relImportance$lmg, decreasing=TRUE)
D30i<- read.csv(file="30i.csv", header=TRUE, sep=",")
D25i<- read.csv(file="25i.csv", header=TRUE, sep=",")
D24i <- read.csv(file="24i.csv", header=TRUE, sep=",")

ED30i<- read.csv(file="e30i.csv", header=TRUE, sep=",")
ED25i<- read.csv(file="e25i.csv", header=TRUE, sep=",")
ED24i <- read.csv(file="e24i.csv", header=TRUE, sep=",")


D25iseries<- ts(D25i)
a<-plot(cex.axis=1.5,(D25iseries))

D24iseries<- ts(D24i)
b<-plot(D24iseries)

ED25iseries<- ts(ED25i)
c<-plot(ED25iseries)

ED24iseries<- ts(ED24i)
d<-plot(ED24iseries)

# 4 figures arranged in 2 rows and 2 columns
windows(8,8)
attach(mtcars)
par(mfrow=c(2,2))
plot(D25iseries,D24iseries)
plot(D25iseries, main="msg_count vs. date of Greenstar25i",cex.axis=1.5)
plot(D24iseries, main="msg_count vs. date of Greenstar24i",cex.axis=1.5)
plot(ED25iseries, main="no_starts vs. date of Greenstar25i",cex.axis=0.9)
plot(ED24iseries, main="no_starts vs. date of Greenstar24i",cex.axis=0.9)

