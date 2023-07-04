#install.packages("forecast")
#library(forecast)
setwd("C:/Users/Celin/Desktop")
getwd()
#question 1
a<- as.numeric(123)
str(a)
a<- as.character(123)
str(a)
a<- as.factor(123)
str(a)
b<- list(1,2,3)
str(b)
b1<- list(1,"2",3)
str(b1)
c<- data.frame(b)
str(c)
str(b1)

#question 2
DATA<-read.csv(file="DATA208582114_Liorbirendorf.csv.csv",stringsAsFactors= FALSE)
str(DATA)
DATA_TS.ts<-ts(DATA)
str(DATA_TS.ts)

#question 3
plot(DATA)
plot(DATA_TS.ts)

#question 4.1
#year
endYear<-as.numeric(format(Sys.time(), "%Y"))
ts.mydata_year<-ts(data = DATA, end = endYear, frequency = 12)
plot(ts.mydata_year)
str(ts.mydata_year)

#question 4.2
#quarters 
endYear<-as.numeric(format(Sys.time(), "%Y"))
ts.mydata_querter<-ts(data = DATA, end = endYear, frequency = 4)
plot(ts.mydata_querter)
str(ts.mydata_querter)

#question 5
plot(DATA_TS.ts)
dat.lm1 <- tslm(DATA_TS.ts ~ trend)
summary(dat.lm1)
lines(dat.lm1$fitted)
accuracy(dat.lm1)

#question 6
install.packages("ggplot2")
#library(ggplot2)
plot(DATA_TS.ts)
ma3 <- ma(DATA_TS.ts, order = 3)
lines(ma3, col="red")
series="moving average(3)"
ma6 <- ma(DATA_TS.ts, order = 6)
lines(ma6, col="green")
series="moving average(6)"
autoplot(DATA_TS.ts, series = "DATA",xlab = "Time", ylab = "Demand", main= "mooving averege") + autolayer(ma3, series = "ma3")+ autolayer(ma6, series = "ma6")

# question number 7
#exponential smoting
fc<- ses(DATA_TS.ts, h=1,alpha=0.1)
fc$mean
fc1<- ses(DATA_TS.ts, h=1,alpha=0.5)
fc1$mean
fc2<- ses(DATA_TS.ts, h=1,alpha=0.9)
fc2$mean

#Optimal alpha:
fc3<- ses(DATA_TS.ts, h=1)
# Accuracy of one-step-ahead training errors over period 1-12
fc3$model
accuracy(fc3)


#f
fc4 <- ses(DATA_TS.ts, h=1)
p2<-autoplot(DATA_TS.ts, series="Data") +autolayer(fc4, series="Exponential Smooting")
p2

##question number 8
#placement a new varible and cut the data coulmans
from108 <- window(ts.mydata_year, start=c(2021,2),end=c(2022,1))
till108 <- window(ts.mydata_year, end=c(2021,1))

hw <- HoltWinters(till108, seasonal = "multiplicative")
hw_forcast <- forecast(hw, h=12)

#8a- prediction with a-b-g 0.1
fit <- HoltWinters(till108,alpha = 0.1, beta = 0.1, gamma = 0.1 ,seasonal = "multiplicative")
p <- forecast(fit, h=12)
p
#8b-prediction with a-b-g 0.9
fit1 <- HoltWinters(till108,alpha = 0.9, beta = 0.9, gamma = 0.9 ,seasonal = "multiplicative")
p1 <- forecast(fit1, h=12)
p1
#8c+d - prediction with optimal a-b-g
fit2 <- HoltWinters(till108,seasonal = "multiplicative")
p2 <- forecast(fit2, h=12, level = c(90, 95))
summary(p2)
plot(p2)
#8e
accuracy(from108, p2$mean)

help(forecast)
fit3 <- HoltWinters(period_to108, seasonal = "multiplicative")
hw <- forecast(fit3, h=12, level=c(90,95))
summary(hw)
plot(hw)

fit1 <- HoltWinters(period_to108 ,seasonal = "multiplicative")
g <- forecast(fit1, h=12)
g
accuracy(period_to108, g$mean)



#### question 9
decomposedTimeSeries <- decompose(ts.mydata_year)
plot(decomposedTimeSeries)
plot(decomposedTimeSeries$random)
#b
max(ts.mydata_year)-min(ts.mydata_year)
#c 
dt<-decomposedTimeSeries$random
max_value<-dt[which.max(dt)]
min_value<-dt[which.min(dt)]
max_value-min_value
#d
if (max(DATA)-min(DATA)>max_value-min_value){
  print("answer is:-1")
}else("answer is 1")
#e
noise<-decomposedTimeSeries$random
noise1<-c(noise)
qqnorm(noise1); qqline(noise1, col = 2)

#############################part 2########################

#install.packages('cdcfluview')
#library(cdcfluview)
# a writing the code 
national_ili <- ilinet( "national" , years = c( 1997: 2022 ))
tail(national_ili)
# we see that we have database of illness till 2022 week 18

# b changing the data base till 2018 week 39.
ts.mydata <- ts( data = national_ili$total_patients,end = c ( 2022 , 14 ) , frequency = 52)
autoplot(ts.mydata, series = 'national_ili',xlab = "Time", ylab = "total_patients")+labs(title = " national_ili total_patients",subtitle ="from 1997-2022")
plot(ts.mydata) # just checking the data plot.

#c
ts_after2020 <- window(ts.mydata , start=c( 2019,52 ))
ts_untill2020 <- window(ts.mydata , end=c( 2020,1 ))
plot(ts_untill2020)
plot(ts_after2020)

ma <- ma (ts_untill2020 , order = 117 ) 
p2 <- forecast (ma , h = 117 )
accuracy ( ts_after2020 , p2$mean )

#d predict the data 
components_dfts <- decompose(ts.mydata)
plot(components_dfts)
#holt winter 1
HoltWinters1<- HoltWinters(ts_untill2020, seasonal = "additive") # Creating a Holt Winters model
#shows the optimal values:
HoltWinters1$alpha # Displays the alpha value
## alpha 
##     1 
HoltWinters1$beta # Displays the beta value
## beta 
##    0
HoltWinters1$gamma # Displays the gamma value
## gamma 
##     1

Prediction <- predict(HoltWinters1, 300, prediction.interval = FALSE) #Making predict for 300 periods ahead
plot(HoltWinters1, Prediction) # Print the forecast
HoltWinters1$SSE    

#HOLT WINTER a = 0.8 ,b = 0.003, g = 1

HoltWinters1<- HoltWinters(ts_untill2020, seasonal = "additive",alpha = 0.8 ,beta = 0.003 , gamma = 1) # Creating a Holt Winters model
Prediction <- predict(HoltWinters1, 300, prediction.interval = FALSE) #Making predict for 300 periods ahead
plot(HoltWinters1 ,Prediction) # Print the forecast
HoltWinters1$SSE

#holtwinters multi

ts_after2003 <- window(ts.mydata , start=c( 2003,1 ))
HoltWinters2<- HoltWinters(ts_after2003, seasonal = "multiplicative") # Creating a Holt Winters model
HoltWinters2$alpha # Displays the alpha value
## alpha 
## 0.5758711 
HoltWinters2$beta # Displays the beta value
## beta 
##    0
HoltWinters2$gamma # Displays the gamma value
## gamma 
##    1

Prediction <- predict(HoltWinters2, 500, prediction.interval = FALSE) #Making predict for 500 periods ahead
plot(HoltWinters2, Prediction) # Print the forecast
HoltWinters1$SSE

#predict with regression
plot(ts_till2020)
dat.lm1 <- tslm(ts_until12020 ~ trend)
summary(dat.lm1)
lines(dat.lm1$fitted,col = "blue", lwd = 1)
accuracy(dat.lm1)


#predict with Exponential Smoothing
fc <- ses(ts_till2020, h=300)
p1<-autoplot(ts_untill2020, series="Data") +
  autolayer(fc, series="Exponential Smooting") +  labs(title = "Monthly show",subtitle ="Exponential Smoothing" )
p1

#predict for (2022,5) till (2022,10)

HoltWinters_flu<- HoltWinters(ts.mydata, seasonal = "additive",alpha = 0.8 ,beta = 0.003 , gamma = 1) # Creating a Holt Winters model
HWflu.pred <- predict(HoltWinters_flu, 29, prediction.interval = TRUE, level=0.95)#
plot(HoltWinters_flu ,HWflu.pred , xlim=c((2021.1),(2023.1))) # Print the forecast
ma <- ma (ts.mydata , order = 29 ) 
p2 <- forecast (ma , h = 29 )
accuracy ( HWflu.pred , p2$mean )




