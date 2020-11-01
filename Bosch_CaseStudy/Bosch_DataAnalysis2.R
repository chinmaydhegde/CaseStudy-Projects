library(jsonlite)
PART_0<-stream_in(file("PART_0.JsON"))
dim(PART_0)


library(jsonlite)
PART_0<-stream_in(file("PART_0.JsON"))
attach(PART_0)
write.csv(PART_0, file = "DATA.csv")

ggcorr(MyData,hc.order = TRUE,type = "lower")
install.packages("ggcorrplot")
library(ggcorrplot)

MyData<-MyData[ , -which(names(MyData) %in% c("outdoor_temp_max","outdoor_temp_min",
                                              "outdoor_temp_mean","nom_max_pow_ch_kw","nom_max_pow_dhw_kw","prim_t_set_max","sysid","date"))]
ggcorrplot(MyData)
ggcorrplot(MyData, hc.order = TRUE, outline.col = "white",type = "lower",
           lab = TRUE)
ggcorrplot(MyData, hc.order = TRUE, type = "lower",
           lab = TRUE)
Greenstar30 <- read.csv(file="E:/gummersbach_data/DATAgreenstar30i.csv", header=TRUE, sep=",")

burner_typ.unique
#PART_1<-stream_in(file("PART_1.JsON"))
#PART_2<-stream_in(file("PART_2.JsON"))
dim(PART_0)
attach(PART_0)
choices <- count(unique(PART_0$appliance_name))

attach(PART_sysid1)

####################TRYYYYYYYYY#######################
install.packages("glmnet")
library(glmnet)
library(Matrix)
n <- 1e5
nclusters <- 5
set.seed(420)
ls <- data.frame(sample(PART_sysid1, n, replace=TRUE))
xs <- sparse.model.matrix(~.,data=PART_sysid1)
print(head(xs))
#######Scaling

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

# check that we get mean of 0 and sd of 1
#colMeans(New_PART_0)  # faster version of apply(scaled.dat, 2, mean)
#apply(New_PART_0, 2, sd)

PART_sysid1[is.na(PART_sysid1)] <- 0
zv <- apply(PART_sysid1, 2, function(x) length(unique(x)) == 1)
sum(zv)
PART_sysid1 <- PART_sysid1[, !zv]
#####correlations
###install.packages("CRAN")
library(CRAN)
library(caret)

#cor_matrix<-cor(New_PART_0)
#pairs(New_PART_0[1:10])
str(New_PART_0)
zv <- apply(New_PART_0, 2, function(x) length(unique(x)) == 1)
sum(zv)
install.packages("ggplot2")
library(ggplot2)
New_PART_0 <- New_PART_0[, !zv]
cor_matrix<-cor(New_PART_0)
COR1<-findCorrelation(cor_matrix,cutoff = 0.8)
COR1
COR2=sort(COR1)
COR2
corrplot(COR1)
ggplot2(COR1)
corrplot(cor(X), order = "hclust")

install.packages("corrplot")
library(corrplot)
corrplot(New_PART_0, method="shade",shade.col=NA, tl.col="black", tl.srt=45)

#Cor_PART_0<-cbind(rownames(New_PART_0),New_PART_0,PART_sysid1$cu_sw,PART_sysid1$hw_sys,PART_sysid1$cu_typ,PART_sysid1$sysid,PART_sysid1$burner_typ,PART_sysid1$code_plug,PART_sysid1$appliance_name,PART_sysid1$date,row.names = NULL)
#####Removing Correlated data
Uncor_PART_0<-New_PART_0[ , -c(3,4,5,7,8,10,12,16,18,19,20,21,25,26,28,29,30,32,33,34)]
#############oredering date###########
PART_sysid1$date<-as.Date(PART_sysid1$date, "%Y-%m-%d")
PART_0<-PART_0[order(as.Date(PART_0$date,"%Y-%m-%d"),decreasing=TRUE),]

PART_sysid1<-PART_sysid1[order(as.Date(PART_sysid1$date,"%Y-%m-%d"),decreasing=TRUE),]
#####correlations
###install.packages("CRAN")
library(CRAN)
library(caret)

#cor_matrix<-cor(New_PART_0)
#pairs(New_PART_0[1:10])

cor_matrix<-cor(New_PART_0)
COR1<-findCorrelation(cor_matrix,cutoff = 0.8)
COR1
COR2=sort(COR1)
COR2

########## NA's############
install.packages("VIM")
install.packages("naniar")
library("VIM")
library(naniar)
gg_miss_var(Scaled_PART_0)
lot <- aggr(PART_0, col=c('navyblue','yellow'),
            numbers=TRUE, sortVars=TRUE,
            labels=names(PART_0), cex.axis=.7,cex.numbers=0.7,
            gap=3, ylab=c("Missing data","Pattern"))


#missForest
install.packages("missForest")
library(missForest)

iris.mis <- prodNA(Scaled_PART_0, noNA = 0.1)
summary(iris.mis)


#impute missing values, using all parameters as default values
iris.imp <- missForest(iris.mis)

#check imputed values
iris.imp$ximp

#check imputation error
iris.imp$OOBerror

#comparing actual data accuracy
iris.err <- mixError(iris.imp$ximp, iris.mis, Uncor_PART_0)
iris.err

#################Mars
install.packages("earth")
library(earth)
marsModel <- earth(energy_kwh ~ ., data=Scaled_PART_0) # build model
ev <- evimp (marsModel) # estimate variable importance
plot(ev)
###########Comparing correlations plot##########
pairs(Scaled_New_PART_0[1:5])
pairs(New_PART_0[1:5])

#######Step-wise Regresiion#######
base.mod <- lm(energy_kwh ~ 1 , data= MyData)  # base intercept only model
all.mod <- lm(energy_kwh ~ . , data= MyData) # full model with all predictors
stepMod <- step(base.mod, scope = list(lower = base.mod, upper = all.mod), direction = "both", trace = 0, steps = 1000)  # perform step-wise algorithm
shortlistedVars <- names(unlist(stepMod[[1]])) # get the shortlisted variable.
shortlistedVars <- shortlistedVars[!shortlistedVars %in% "(Intercept)"]  # remove intercept 
print(shortlistedVars)
step(shortlistedVars)
step(shortlistedVars[!shortlistedVars %in% "(Intercept)"] ) # remove intercept 

########Variance####
var(energy_kwh)
var(pow_mean)

##########Boruta#######
install.packages("Boruta")
library(Boruta)
# Decide if a variable is important or not using Boruta
boruta_output <- Boruta(energy_kwh ~ ., data=na.omit(Scaledr_PART_0), doTrace=5)  # perform Boruta search
boruta_signif <- names(boruta_output$finalDecision[boruta_output$finalDecision %in% c("Confirmed", "Tentative")])  # collect Confirmed and Tentative variables
print(boruta_signif)  # significant variables
dev.new(width=5,height=5)
windows(5,5)
plot(boruta_output, cex.axis=.7, las=2, xlab="", main="Variable Importance", xart="n")  # plot variable importance

png(filename = "div1.png",width=900,bg="white")
par(mar=c(15,15,4,2)+0.1)
plot(boruta_output, cex.axis=0.8, las=2, xlab="", main="Variable Importance")  # plot variable importance

dev.off()
###############Randomforest
install.packages("party")
library(party)
cf1 <- cforest(energy_kwh ~ . , data= Scaled_PART_0, control=cforest_unbiased(mtry=2,ntree=50)) # fit the random forest
varimp(cf1)
plot(cf1)
varimp(cf1, conditional=TRUE)# conditional=True, adjusts for correlations between predictors
varimpAUC(cf1)
str(PART_0)


install.packages("gbm")
require(gbm)
require(MASS)#package with the boston housing dataset

#separating training and test data
#train=sample(1:506,size=374)
attach(MyData)
MyData.boost=gbm(energy_kwh ~ . ,data = Scaled_PART_0,distribution = "gaussian",n.trees = 10000,
                 shrinkage = 0.01, interaction.depth = 4)
MyData.boost

summary(MyData.boost) #Summary gives a table of Variable Importance and a plot of Variable Importance
###############Mahalanobis##############

MD<- mahalanobis(PART_sysid1[,c(1,3)],colMeans(PART_sysid1[,c(1,2)]), cov(PART_sysid1[, c(1,2)]))
PART_sysid1$MD<-round(MD,3)
PART_sysid1$outlier_maha<-FALSE
PART_sysid1$outlier_maha[PART_sysid1$MD>16]<- TRUE
head(PART_sysid1)
PART_sysid1

install.packages("car")
faultsys<- lm(energy_kwh~. , data=MyData)
influence(faultsys)

MD[1:200] %>% round(2)
################Missing########
install.packages("VIM")
library(VIM)
aggr(PART_0)

###########K means ##############
set.seed(20)
PartCluster <- kmeans(PART_sysid1, 5, nstart = 20)
PartCluster

table(PartCluster$cluster, PART_sysid1$newdate)

library(ggplot2)
PartCluster$cluster <- as.factor(PartCluster$cluster)
ggplot(PART_sysid1, aes(energy_kwh, data_loss, color = PartCluster$cluster)) + geom_point()


