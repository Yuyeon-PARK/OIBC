solar<-read.csv("SolarPV_Elec_Problem.csv",header=F);attach(solar)
names(solar)<-c("date","pv") # 하루 전체 발전량 96개

##라이브러리 설치##
library(zoo); library(forecast); library(tseries); library(portes)

##원계열 시도표##
PV<-solar[,2]; PV.df = data.frame(PV)

row_sub = apply(PV.df, 1, function(row) all(row !=0 ))
pv1<-PV.df[row_sub,]
PV1 <- ts(pv1,start=0)
solar.PV <- as.zoo(PV1)
autoplot(PV1); summary(PV1)

###############################
########1월 31일 복원값 추정########
###############################

jan<-as.data.frame(solar[17665:20544,2])

lnPV<-log(jan) #ln 변환계열
jan.ln<-cbind(jan, lnPV);colnames(jan.ln)<-c("PV","lnPV")

row_jan = apply(jan.ln, 1, function(row) all(row !=0 ))
jan0<-jan.ln[row_jan,] 
PV11<-jan0$PV #1월 원계열
jan.ts<-ts(PV11, frequency=43) #0인 값 제외 관측량 일 43개
autoplot(decompose(jan.ts))

##1. 정상성 검정##
adf.test(jan.ts, alternative = "stationary", k=0) 
tsdiag(auto.arima(jan.ts))

##2. 모형 및 차수 추정##
fit.jan.ts <- auto.arima(jan.ts,seasonal=T)

#포트맨토우 검정: 검정 결과, 최적 모형은 아님.
portest(fit.jan.ts$residuals,lags=c(6,12,18,24,30,36),test="LjungBox")
acf(fit.jan.ts$residuals)

##3. 예측##
forecast(fit.jan.ts,h = 43) #일출(00:00-07:30)전 0*31개, 일몰(6:30-11:45) 후 0*22개 포함=총 96개 구간
plot(forecast(fit.jan.ts,h = 43))

#################################
########3월 31일 복원값 추정##########
#################################

mar<-as.data.frame(solar[23425:26304,2])

lnPV<-log(mar) #ln변환계열
mar.ln<-cbind(mar, lnPV);colnames(mar.ln)<-c("PV","lnPV")

row_mar = apply(mar.ln, 1, function(row) all(row !=0 ))
mar0<-mar.ln[row_mar,]
PV3<-mar0$PV #3월 원계열
mar.ts<-ts(PV3, frequency=53) #0인 값 제외 관측량 일 53개
autoplot(decompose(mar.ts))

##1. 정상성 검정##
adf.test(mar.ts, alternative = "stationary", k=0) 
tsdiag(auto.arima(mar.ts))   

##2. 모형 및 차수 추정##
fit.mar.ts <- auto.arima(mar.ts,seasonal=T)

#포트맨토우 검정: 검정 결과, 최적 모형은 아님.
portest(fit.mar.ts$residuals,lags=c(6,12,18,24,30,36),test="LjungBox")
acf(fit.mar.ts$residuals) 

##3. 예측##
forecast(fit.mar.ts,h = 53) #일출(00:00-06:00)전 0*25개, 일몰(7:30-11:45) 후 0*18개 포함=총 96개 구간
plot(forecast(fit.mar.ts,h = 53))

###############################
########5월 31일 복원값 추정########
###############################

may<-as.data.frame(solar[29281:32160,2])

lnPV<-log(may) #ln 변환계열
may.ln<-cbind(may, lnPV);colnames(may.ln)<-c("PV","lnPV")

row_may = apply(may.ln, 1, function(row) all(row !=0 ))
may0<-may.ln[row_may,]
PV5<-may0$PV #5월 원계열
may.ts<-ts(PV5, frequency=60) #0인 값 제외 관측량 일 60개
autoplot(decompose(may.ts))

##1. 정상성 검정##
adf.test(may.ts, alternative = "stationary", k=0)
tsdiag(auto.arima(may.ts))

##2. 모형 및 차수 추정##
fit.may.ts <- auto.arima(may.ts,seasonal=T) #ARIMA(1,0,1)(2,1,0)

#포트맨토우 검정: 검정 결과, 최적 모형은 아님.
portest(fit.may.ts$residuals,lags=c(6,12,18,24,30,36),test="LjungBox")
acf(fit.may.ts$residuals) 

##3. 예측##
forecast(fit.may.ts,h = 60) #일출(00:00-05:00)전 0*21개, 일몰(8:15-11:45) 후 0*15개 포함=총 96개 구간
plot(forecast(fit.may.ts,h = 60))

###############################
########7월 31일 예측값 추정########
###############################
solar7<-read.csv("Solar_PV_July.csv",header=F);attach(solar7)
names(solar7)<-c("date","pv")

PV7<-solar7[,2]; PV7.df = data.frame(PV7)

july<-ts(PV7.df, frequency=96) #0을 포함한 하루 전체 관측량///#일출(00:00-05:00)전 0*20개, 일몰(8:30-11:45) 후 0*14개 포함=총 62개 
plot(july, type="l"); summary(july)
plot(decompose(july))

##1.정상성 검정##
library(tseries); library(forecast)
tsdiag(auto.arima(july));tsdiag(fit.arima)

###2.차수 추정 및 예측###
fit.arima<-Arima(july, order=c(2,1,2),seasonal=list(order=c(0,1,0),period=96),method="ML")

#포트맨토우 검정: 검정 결과, 최적 모형은 아님.
portest(fit.arima$residuals,lags=c(6,12,18,24,30,36),test="LjungBox")
acf(fit.arima$residuals)

##3.예측##
fore<-as.data.frame(forecast(fit.arima, h=192)) #7월30일, 7월31일 예측
plot(forecast(fit.arima, h=192))