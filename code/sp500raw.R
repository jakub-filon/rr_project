##################################################################
##########    import financial data 2007-2020    #################

#read multiple text file 

#the path where text files are saved
mypath="/Users/luzhang/Desktop/SPX"
setwd(mypath)

#Create list of text files
SPX_ls=list.files(path=mypath, pattern = "*.txt")

#Read the files in, comma is the separator
SPX_df<-lapply(SPX_ls, function(x){read.table(file = x, 
                                              col.names = c("DateTime","Open", "High","Low","Close"),sep = ",")})

#Combine them
SPX <- do.call("rbind", lapply(SPX_df, as.data.frame))
#head(SPX)
#length(SPX$DataTime)

#DateTime: character->dateTime(double)
SPX$DateTime<-as.POSIXct(SPX$DateTime,taz="EST")

##################################################################
###################      data preparing     ######################

options(digits.secs=3)
Sys.setenv(TZ='EST')

# truncate data from 2009-01-01 to 2019-12-31, 9:30 AM to 16:00 PM

SPX$DateTime<-as.POSIXct(SPX$DateTime,format="%H:%M:%OS", taz="EST")
SPX<- subset(SPX, lubridate::hour(SPX$DateTime)*60
             +lubridate::minute(SPX$DateTime) >= 9*60+30)
SPX <- subset(SPX, lubridate::hour(SPX$DateTime)*60
              +lubridate::minute(SPX$DateTime) <= 16*60)
head(SPX)
tail(SPX)

#    Wavelet transform    #
#library(WaveletComp)
library(remotes)
#library(wmtsa)
library(astsa)
library(ggplot2)

Close<-SPX$Close
High<-SPX$High
Low<-SPX$Low
Open<-SPX$Open

# convert Posixct to xts format
#library(highfrequency)
#library(xts)
HL<-as.matrix(cbind(High,Low))
#HL<-xts(HL,SPX$DateTime)
#head(spx_HL)
HLC<-as.matrix(cbind(High,Low,Close))
#HLC<-xts(HLC,SPX$DateTime)
HLCO<-as.matrix(cbind(High,Low,Close,Open))
#HLCO<-xts(HLCO,SPX$DateTime)
CHL<-as.matrix(cbind(Close,High,Low))
#CHL<-xts(CHL, SPX$DateTime)
Close<-xts(Close,SPX$DateTime)

prices<-Close
names(prices)<-"prices"
#head(prices)

## calculating y_label based on average prices
day_index<-endpoints(prices, on = "days", k = 1)
#head(day_index)
#length(day_index)

# last min prices for each trading day
lmP<-prices[day_index,]
head(lmP)

# average prices from 1 to 360 mins
n<-length(day_index)-1
avg_360<-rep(0,n)
for (i in 1:n){
  start<-day_index[i]+1
  end<-day_index[i+1]-30
  avg_360[i]<-mean(prices[start:end])
}

#create y label
n<-length(lmP)
y_label<-rep(0,n)
for (i in 1:n ){
  if(avg_360[i]<lmP[i]){
    y_label[i]=1
  }else{
    y_label[i]=0
  }
}

table(y_label)
#y_label
#0    1 
#1531 1916


##indicators calculation
library(TTR)

n<-length(day_index)-1
start<-rep(0,n)
end<-rep(0,n)
for (i in 1:n){
  start[i]<-day_index[i]+1
  end[i]<-day_index[i+1]-30
}
#ADX,done

avg_ADX<-rep(0,n)
ADX<-ADX(HLC)
for (i in 1:n){
  avg_ADX[i]<-mean(ADX[start[i]:end[i]])
}

#aroon, done
avg_aroon<-rep(0,n)
aroon<-aroon(HL)
for (i in 1:n){
  avg_aroon[i]<-mean(aroon[start[i]:end[i]])
}
#ATR done
avg_ATR<-rep(0,n)
ATR<-ATR(HLC)
for (i in 1:n){
  avg_ATR[i]<-mean(ATR[start[i]:end[i]])
}
#BBands # partial done
avg_BBands<-rep(0,n)
BBands<-BBands(HLC)
for (i in 1:n){
  avg_BBands[i]<-mean(BBands[start[i]:end[i]])
}
#CCI done
avg_CCI<-rep(0,n)
CCI<-CCI(HLC)
for (i in 1:n){
  avg_CCI[i]<-mean(CCI[start[i]:end[i]])
}
#chaikinVolatility done
avg_chaikinVolatility<-rep(0,n)
chaikinVolatility<-chaikinVolatility(HLC)
for (i in 1:n){
  avg_chaikinVolatility[i]<-mean(chaikinVolatility[start[i]:end[i]])
}
#CLV done
avg_CLV<-rep(0,n)
CLV<-CLV(HLC)
for (i in 1:n){
  avg_CLV[i]<-mean(CLV[start[i]:end[i]])
}

#CMOClose done
avg_CMOClose<-rep(0,n)
CMOClose<-CMO(Close)
for (i in 1:n){
  avg_CMOClose[i]<-mean(CMOClose[start[i]:end[i]])
}
#CTI 
avg_CTI<-rep(0,n)
CTI<-CTI(HLC)
for (i in 1:n){
  avg_CTI[i]<-mean(CTI[start[i]:end[i]])
}
#DonchianChannel
avg_DonchianChannel<-rep(0,n)
DonchianChannel<-DonchianChannel(HL)
for (i in 1:n){
  avg_DonchianChannel[i]<-mean(DonchianChannel[start[i]:end[i]])
}
#DPOClose done
avg_DPOClose<-rep(0,n)
DPO<-DPO(Close)
for (i in 1:n){
  avg_DPOClose[i]<-mean(DPO[start[i]:end[i]])
}

#DVI done
avg_DVIClose<-rep(0,n)
DVI<-DVI(Close)
for (i in 1:n){
  avg_DVIClose[i]<-mean(DVI[start[i]:end[i]])
}
#GMMAClose done
avg_GMMAClose<-rep(0,n)
GMMA<-GMMA(Close)
for (i in 1:n){
  avg_GMMAClose[i]<-mean(GMMA[start[i]:end[i]])
}

#KSTClose done
avg_KSTClose<-rep(0,n)
KST<-KST(Close)
for (i in 1:n){
  avg_KSTClose[i]<-mean(KST[start[i]:end[i]])
}

#lags done
avg_lags<-rep(0,n)
lags<-lags(HLC)
for (i in 1:n){
  avg_lags[i]<-mean(lags[start[i]:end[i]])
}

#MACD done
avg_MACD<-rep(0,n)
MACD<-MACD(Close)
for (i in 1:n){
  avg_MACD[i]<-mean(MACD[start[i]:end[i]])
}

#PBands done
avg_PBandsClose<-rep(0,n)
PBands<-PBands(Close)
for (i in 1:n){
  avg_PBandsClose[i]<-mean(PBands[start[i]:end[i]])
}

#ROCclose done
avg_ROCClose<-rep(0,n)
ROC<-ROC(Close)
for (i in 1:n){
  avg_ROCClose[i]<-mean(ROC[start[i]:end[i]])
}

#momentum done
avg_momentumClose<-rep(0,n)
momentum<-momentum(Close)
for (i in 1:n){
  avg_momentumClose[i]<-mean(momentum[start[i]:end[i]])
}

#RSI done
avg_RSIClose<-rep(0,n)
RSI<-RSI(Close)
for (i in 1:n){
  avg_RSIClose[i]<-mean(RSI[start[i]:end[i]])
}

#runSum done
avg_runSum<-rep(0,n)
runSum<-runSum(Close)
for (i in 1:n){
  avg_runSum[i]<-mean(runSum[start[i]:end[i]])
}

#runMin  done
avg_runMin<-rep(0,n)
runMin<-runMin(Close)
for (i in 1:n){
  avg_runMin[i]<-mean(runMin[start[i]:end[i]])
}

#runMax done
avg_runMax<-rep(0,n)
runMax<-runMax(Close)
for (i in 1:n){
  avg_runMax[i]<-mean(runMax[start[i]:end[i]])
}

#runMedian done
avg_runMedian<-rep(0,n)
runMedian<-runMedian(Close)
for (i in 1:n){
  avg_runMedian[i]<-mean(runMedian[start[i]:end[i]])
}

#SAR done
avg_SAR<-rep(0,n)
SAR<-SAR(HL)
for (i in 1:n){
  avg_SAR[i]<-mean(SAR[start[i]:end[i]])
}

#SMA done
avg_SMAClose<-rep(0,n)
SMA<-SMA(Close)
for (i in 1:n){
  avg_SMAClose[i]<-mean(SMA[start[i]:end[i]])
}

#EMA done
avg_EMAClose<-rep(0,n)
EMA<-EMA(Close)
for (i in 1:n){
  avg_EMAClose[i]<-mean(EMA[start[i]:end[i]])
}

#DEMA done
avg_DEMAClose<-rep(0,n)
DEMA<-DEMA(Close)
for (i in 1:n){
  avg_DEMAClose[i]<-mean(DEMA[start[i]:end[i]])
}

#SNR done
avg_SNR<-rep(0,n)
SNR<-SNR(HLC, n=30)
for (i in 1:n){
  avg_SNR[i]<-mean(SNR[start[i]:end[i]])
}


#SMI done
avg_SMI<-rep(0,n)
SMI<-SMI(HLC)
for (i in 1:n){
  avg_SMI[i]<-mean(SMI[start[i]:end[i]])
}

#TDI done
avg_TDI<-rep(0,n)
TDI<-TDI(Close)
for (i in 1:n){
  avg_TDI[i]<-mean(TDI[start[i]:end[i]])
}

#TRIX done
avg_TRIX<-rep(0,n)
TRIX<-TRIX(Close)
for (i in 1:n){
  avg_TRIX[i]<-mean(TRIX[start[i]:end[i]])
}

#ultimateOscillator done
avg_ultimateOscillator<-rep(0,n)
ultimateOscillator<-ultimateOscillator(HLC)
for (i in 1:n){
  avg_ultimateOscillator[i]<-mean(ultimateOscillator[start[i]:end[i]])
}

#VHF done
avg_VHF<-rep(0,n)
VHF<-VHF(Close)
for (i in 1:n){
  avg_VHF[i]<-mean(VHF[start[i]:end[i]])
}

#Volatility done
avg_volatility<-rep(0,n)
volatility<-volatility(HLCO)
for (i in 1:n){
  avg_volatility[i]<-mean(volatility[start[i]:end[i]])
}

#williamsAD done
avg_williamsAD<-rep(0,n)
williamsAD<-williamsAD(HLC)
for (i in 1:n){
  avg_williamsAD[i]<-mean(williamsAD[start[i]:end[i]])
}

#WPR done
avg_WPR<-rep(0,n)
WPR<-WPR(HLC)
for (i in 1:n){
  avg_WPR[i]<-mean(WPR[start[i]:end[i]])
}
#ZigZag done
avg_ZigZag<-rep(0,n)
ZigZag<-ZigZag(HL)
for (i in 1:n){
  avg_ZigZag[i]<-mean(ZigZag[start[i]:end[i]])
}

csv<-data.frame(avg_ADX, avg_aroon,avg_BBands,avg_CCI, avg_chaikinVolatility,avg_CLV,
                avg_CMOClose,avg_CTI,avg_DonchianChannel,avg_DPOClose,avg_DVIClose,
                avg_GMMAClose,avg_KSTClose,avg_lags,avg_MACD,avg_PBandsClose,
                avg_ROCClose,avg_momentumClose,avg_RSIClose,avg_runSum,avg_runMin,
                avg_runMax,avg_runMedian,avg_SAR,avg_SMAClose,avg_EMAClose,avg_DEMAClose,
                avg_SNR,avg_SMI,avg_TDI,avg_TRIX,avg_ultimateOscillator,avg_VHF,
                avg_volatility,avg_williamsAD,avg_WPR,avg_ZigZag)

write.csv(csv, "/Users/luzhang/Desktop/indicator/features_360_raw.csv")


################## random forest _imbalanced dataset##############

rm(list = setdiff(ls(), lsf.str()))
library(fmlr)
library(quantmod)
library(TTR) # for various indicators
library(randomForestFML)
library(ROCR)

features <- read.csv("/Users/luzhang/Desktop/indicator/features_360_raw.csv", header = T)
head(features)
dim(features)
features$X<-NULL
allSet<-data.frame(Y=as.factor(y_label),features)
head(allSet)

write.csv(allSet,"/Users/luzhang/Desktop/indicator/allSet_360_raw.csv" )

features <- read.csv("/Users/luzhang/Desktop/indicator/allSet_360_raw.csv", header = T)
head(features)
dim(features)

allSet<-data.frame(features)
head(allSet)
allSet$X<-NULL
head(allSet)
#exclude NA at the begining of the indicators
idx_NA <- apply(allSet,1,function(x){sum(is.na(x))>0})
allSet <- subset(allSet, !idx_NA)
allSet$avg_WPR<-NULL # remove the columns that cause some errors
allSet$avg_ultimateOscillator<-NULL
allSet$Y<-as.factor(allSet$Y)
dim(allSet)
#3430 36
table(allSet$Y)
# 0 1
#1522 1908
nx <- nrow(allSet)
trainSet <- allSet[1:floor(nx*2/3),]
testSet <- allSet[(floor(nx*2/3)+1):nx,]
dim(allSet); dim(trainSet); dim(testSet)
#[1] 3430   36
#[1] 2286   36
#[1] 1144   36


table(trainSet$Y)
#0    1 
#977 1289
table(testSet$Y)
#0   1 
#525 619 
###### original imbalanced data set
library(caret)
set.seed(1)
model_rf <- caret::train(Y ~ .,
                         data = trainSet,
                         method = "rf",
                         preProcess = c("scale", "center"),
                         trControl = trainControl(method = "repeatedcv", 
                                                  number = 10, 
                                                  repeats = 10, 
                                                  verboseIter = FALSE))
final <- data.frame(actual = testSet$Y,
                    predict(model_rf, newdata = testSet, type = "prob"))
final$predict <- ifelse(final$X0 > 0.5, 0, 1)
cm_original <- confusionMatrix(as.factor(final$predict), testSet$Y)

#Confusion Matrix and Statistics

#Reference
#Prediction   0   1
#0 348 176
#1 177 443

#Accuracy : 0.6914          
#95% CI : (0.6638, 0.7181)
#No Information Rate : 0.5411          
#P-Value [Acc > NIR] : <2e-16          

#Kappa : 0.3786          

#Mcnemar's Test P-Value : 1               
                                          
#            Sensitivity : 0.6629          
#            Specificity : 0.7157          
#         Pos Pred Value : 0.6641          
#         Neg Pred Value : 0.7145          
#             Prevalence : 0.4589          
#         Detection Rate : 0.3042          
#   Detection Prevalence : 0.4580          
#      Balanced Accuracy : 0.6893          
                                          
#       'Positive' Class : 0               
                     

#################   random forest _balanced data set   #####

################    under-sampling  #######################
ctrl <- trainControl(method = "repeatedcv", 
                     number = 10, 
                     repeats = 10, 
                     verboseIter = FALSE,
                     sampling = "down")
set.seed(1)
model_rf_under <- caret::train( Y~ .,
                                data = trainSet,
                                method = "rf",
                                preProcess = c("scale", "center"),
                                trControl = ctrl)
final_under <- data.frame(actual = testSet$Y,
                          predict(model_rf_under, newdata = testSet, type = "prob"))
final_under$predict <- ifelse(final_under$X0 > 0.5, 0, 1)
cm_under <- confusionMatrix(as.factor(final_under$predict), testSet$Y)

#Confusion Matrix and Statistics

#Reference
#Prediction   0   1
#0 378 225
#1 147 394

#Accuracy : 0.6748          
#95% CI : (0.6468, 0.7019)
#No Information Rate : 0.5411          
#P-Value [Acc > NIR] : < 2.2e-16       

#Kappa : 0.3525          

#Mcnemar's Test P-Value : 6.545e-05       
                                          
#            Sensitivity : 0.7200          
#            Specificity : 0.6365          
#         Pos Pred Value : 0.6269          
 #        Neg Pred Value : 0.7283          
 #            Prevalence : 0.4589          
 #        Detection Rate : 0.3304          
 #  Detection Prevalence : 0.5271          
 #     Balanced Accuracy : 0.6783          
                                          
 #      'Positive' Class : 0   
################## Over Sampling ##################

ctrl <- trainControl(method = "repeatedcv", 
                     number = 10, 
                     repeats = 10, 
                     verboseIter = FALSE,
                     sampling = "up")
set.seed(1)
model_rf_over <- caret::train( Y~ .,
                               data = trainSet,
                               method = "rf",
                               preProcess = c("scale", "center"),
                               trControl = ctrl)
final_over <- data.frame(actual = testSet$Y,
                         predict(model_rf_over, newdata = testSet, type = "prob"))
final_over$predict <- ifelse(final_over$X0 > 0.5, 0, 1)
cm_over <- confusionMatrix(as.factor(final_over$predict), testSet$Y)

#Confusion Matrix and Statistics

#Reference
#Prediction   0   1
#0 323 165
#1 202 454

#Accuracy : 0.6792          
#95% CI : (0.6513, 0.7062)
#No Information Rate : 0.5411          
#P-Value [Acc > NIR] : < 2e-16         

#Kappa : 0.3506          

#Mcnemar's Test P-Value : 0.06022         
                                          
#            Sensitivity : 0.6152          
#            Specificity : 0.7334          
#         Pos Pred Value : 0.6619          
#         Neg Pred Value : 0.6921          
#             Prevalence : 0.4589          
#         Detection Rate : 0.2823          
#   Detection Prevalence : 0.4266          
#      Balanced Accuracy : 0.6743          
                                          
#       'Positive' Class : 0             
################## Rose ##################

ctrl <- trainControl(method = "repeatedcv", 
                     number = 10, 
                     repeats = 10, 
                     verboseIter = FALSE,
                     sampling = "rose")
set.seed(1)
model_rf_rose <- caret::train( Y~ .,
                               data = trainSet,
                               method = "rf",
                               preProcess = c("scale", "center"),
                               trControl = ctrl)
final_rose <- data.frame(actual = testSet$Y,
                         predict(model_rf_rose, newdata = testSet, type = "prob"))
final_rose$predict <- ifelse(final_rose$X0 > 0.5, 0, 1)
cm_rose <- confusionMatrix(as.factor(final_rose$predict), testSet$Y)

#Confusion Matrix and Statistics

#Reference
#Prediction   0   1
#0 236  49
#1 289 570

#Accuracy : 0.7045          
#95% CI : (0.6772, 0.7309)
#No Information Rate : 0.5411          
#P-Value [Acc > NIR] : < 2.2e-16       

#Kappa : 0.3837          

#Mcnemar's Test P-Value : < 2.2e-16       
                                          
#            Sensitivity : 0.4495          
#            Specificity : 0.9208          
#         Pos Pred Value : 0.8281          
#         Neg Pred Value : 0.6636          
#             Prevalence : 0.4589          
#         Detection Rate : 0.2063          
#   Detection Prevalence : 0.2491          
 #     Balanced Accuracy : 0.6852          
                                          
#       'Positive' Class : 0             


################## smote ##################

ctrl <- trainControl(method = "repeatedcv", 
                     number = 10, 
                     repeats = 10, 
                     verboseIter = FALSE,
                     sampling = "smote")
set.seed(1)
model_rf_smote <- caret::train( Y~ .,
                                data = trainSet,
                                method = "rf",
                                preProcess = c("scale", "center"),
                                trControl = ctrl)
final_smote <- data.frame(actual = testSet$Y,
                          predict(model_rf_smote, newdata = testSet, type = "prob"))
final_smote$predict <- ifelse(final_smote$X0 > 0.5, 0, 1)
cm_smote <- confusionMatrix(as.factor(final_smote$predict), testSet$Y)

Confusion Matrix and Statistics

#Reference
#Prediction   0   1
#0 431 252
#1  94 367

#Accuracy : 0.6976        
#95% CI : (0.67, 0.7241)
#No Information Rate : 0.5411        
#P-Value [Acc > NIR] : < 2.2e-16     

#Kappa : 0.4046        

#Mcnemar's Test P-Value : < 2.2e-16     
                                        
#            Sensitivity : 0.8210        
#            Specificity : 0.5929        
#         Pos Pred Value : 0.6310        
#         Neg Pred Value : 0.7961        
#             Prevalence : 0.4589        
#         Detection Rate : 0.3767        
#   Detection Prevalence : 0.5970        
#      Balanced Accuracy : 0.7069        
                                        
#       'Positive' Class : 0         
########### compare predictions ########
models <- list(original = model_rf,
               under = model_rf_under,
               over = model_rf_over,
               smote = model_rf_smote,
               rose = model_rf_rose)
resampling <- resamples(models)
bwplot(resampling)

library(dplyr)
comparison <- data.frame(model = names(models),
                         Specificity = rep(NA, length(models)),
                         Precision = rep(NA, length(models)),
                         Recall = rep(NA, length(models)),
                         F1 = rep(NA, length(models)))


for (name in names(models)) {
  model <- get(paste0("cm_", name))
  class<-model$byClass
  comparison[comparison$model == name, ] <- filter(comparison, model == name) %>%
    mutate(
           Specificity = class["Specificity"],
           Precision = class["Precision"],
           Recall = class["Recall"],
           F1 = class["F1"])
}



write.csv(comparison, "/Users/luzhang/Desktop/indicator/comparsion_raw.csv")

library(tidyr)
comparison %>%
  gather(x, y, Specificity:F1) %>%
  ggplot(aes(x = x, y = y, color = model)) +
  geom_jitter(width = 0.2, alpha = 0.5, size = 3)

