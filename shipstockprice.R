library(forecast)
library(tseries)
library(rugarch)
library(prophet)
library(tsfknn)
library(quantmod)
library(magrittr)
library(broom)
library(tidyverse)
library(ggplot2)
options(stringsAsFactors = FALSE)



#set start and end date for stock performance
start = as.Date("2012-01-01") 
end = as.Date("2023-01-22")

getSymbols(c("AMZN", "FDX", "UPS","^GSPC"), src = "yahoo", from = start, to = end)
stocks = as.xts(data.frame(A = AMZN[, "AMZN.Adjusted"], 
                           B = FDX[, "FDX.Adjusted"], 
                           C = UPS[, "UPS.Adjusted"],
                           D = GSPC[,"GSPC.Adjusted"]
                          ))
#summarize data
summary(`UPS`)
summary(`FDX`)
summary(`AMZN`)


names(stocks) = c("Amazon", "FedEx", "UPS", "S&P 500")
index(stocks) = as.Date(index(stocks))

#puts performance of each stock on 1 graph
stocks_series = tidy(stocks) %>% 
  
  ggplot(aes(x=index,y=value, color=series)) +
  labs(title = "Shipping Companies and S&P 500 Comparison",
       
       subtitle = "End of Day Adjusted Prices by Year",
       caption = " Source: Yahoo Finance") +
  
  xlab("Date") + ylab("Price") +
  scale_color_manual(values = c("Orange", "Purple", "DarkBlue", "Brown"))+
  geom_line()
stocks_series

#stacked performance graphs
stocks_series2 = tidy(stocks) %>%     
  
  ggplot(aes(x=index,y=value, color=series)) + 
  geom_line() +
  facet_grid(series~.,scales = "free") + 
  labs(title = "Shipping Companies Stock Performance Comparison vs the S&P 500",
       
       subtitle = "End of Day Adjusted Prices by Year",
       caption = " Source: Yahoo Finance") +
  
  xlab("Date") + ylab("Price") +
  scale_color_manual(values = c("Orange", "Purple", "DarkBlue","Brown"))
stocks_series2

#individual company graphs
ggplot(UPS, aes(x = index(UPS), y = UPS[,6])) + geom_line(color = "brown4") + ggtitle("UPS 10 Year Stock Prices") + xlab("Date") + ylab("Price") + theme(plot.title = element_text(hjust = 0.5)) + scale_x_date(date_labels = "%b %y", date_breaks = "6 months")
ggplot(AMZN, aes(x = index(AMZN), y = AMZN[,6])) + geom_line(color = "orange") + ggtitle("Amazon 10 Year Stock Prices") + xlab("Date") + ylab("Price") + theme(plot.title = element_text(hjust = 0.5)) + scale_x_date(date_labels = "%b %y", date_breaks = "6 months")
ggplot(FDX, aes(x = index(FDX), y = FDX[,6])) + geom_line(color = "purple") + ggtitle("FedEx 10 Year Stock Prices") + xlab("Date") + ylab("Price") + theme(plot.title = element_text(hjust = 0.5)) + scale_x_date(date_labels = "%b %y", date_breaks = "6 months")



#10 and 30 day moving average UPS price graph
UPS_mm <- subset(UPS, index(UPS) >= "2022-01-01")

UPS_mm10 <- rollmean(UPS_mm[,6], 10, fill = list(NA, NULL, NA), align = "right")
UPS_mm30 <- rollmean(UPS_mm[,6], 30, fill= list(NA, NULL, NA), align = "right")

UPS_mm$mm10 <- coredata(UPS_mm10)
UPS_mm$mm30 <- coredata(UPS_mm30)

ggplot(UPS_mm, aes(x = index(UPS_mm))) +
  geom_line(aes(y = UPS_mm[,6], color = "UPS")) + ggtitle("UPS Moving Average Price Series") +
  geom_line(aes(y = UPS_mm$mm10, color = "MM10")) +
  geom_line(aes(y = UPS_mm$mm30, color = "MM30")) + xlab("Date") + ylab("Price") +
  theme(plot.title = element_text(hjust = 0.5), panel.border = element_blank()) +
  scale_x_date(date_labels = "%b %y", date_breaks = "3 months") +
  scale_colour_manual("Series", values=c("UPS"="gray40", "MM10"="firebrick4", "MM30"="darkcyan"))

 
#10 and 30 day moving average Amazon price graph      
AMZN_mm <- subset(AMZN, index(AMZN) >= "2022-01-01")
 
AMZN_mm10 <- rollmean(AMZN_mm[,6], 10, fill = list(NA, NULL, NA), align = "right")
AMZN_mm30 <- rollmean(AMZN_mm[,6], 30, fill= list(NA, NULL, NA), align = "right")
 
AMZN_mm$mm10 <- coredata(AMZN_mm10)
AMZN_mm$mm30 <- coredata(AMZN_mm30)
 
ggplot(AMZN_mm, aes(x = index(AMZN_mm))) +
     geom_line(aes(y = AMZN_mm[,6], color = "UPS")) + ggtitle("Amazon Moving Average Price Series") +
     geom_line(aes(y = AMZN_mm$mm10, color = "MM10")) +
     geom_line(aes(y = AMZN_mm$mm30, color = "MM30")) + xlab("Date") + ylab("Price") +
     theme(plot.title = element_text(hjust = 0.5), panel.border = element_blank()) +
     scale_x_date(date_labels = "%b %y", date_breaks = "3 months") +
     scale_colour_manual("Series", values=c("UPS"="gray40", "MM10"="firebrick4", "MM30"="darkcyan"))

#10 and 30 day moving average FedEx price graph
FDX_mm <- subset(FDX, index(FDX) >= "2022-01-01")

FDX_mm10 <- rollmean(FDX_mm[,6], 10, fill = list(NA, NULL, NA), align = "right")
FDX_mm30 <- rollmean(FDX_mm[,6], 30, fill= list(NA, NULL, NA), align = "right")

FDX_mm$mm10 <- coredata(FDX_mm10)
FDX_mm$mm30 <- coredata(FDX_mm30)

ggplot(FDX_mm, aes(x = index(FDX_mm))) +
  geom_line(aes(y = FDX_mm[,6], color = "UPS")) + ggtitle("FedEx Moving Average Price Series") +
  geom_line(aes(y = FDX_mm$mm10, color = "MM10")) +
  geom_line(aes(y = FDX_mm$mm30, color = "MM30")) + xlab("Date") + ylab("Price") +
  theme(plot.title = element_text(hjust = 0.5), panel.border = element_blank()) +
  scale_x_date(date_labels = "%b %y", date_breaks = "3 months") +
  scale_colour_manual("Series", values=c("UPS"="gray40", "MM10"="firebrick4", "MM30"="darkcyan"))

#daily return of each stock
dailyReturn(UPS)
dailyReturn(AMZN)
dailyReturn(FDX)


#UPS stock returns
UPS_ret <- diff(log(UPS[,6]))
UPS_ret <- UPS_ret[-1,]

#plotting UPS returns over last decade
ggplot(UPS_ret, aes(x = index(UPS_ret), y = UPS_ret)) +
  geom_line(color = "brown4") +
  ggtitle("UPS 10 year returns series") +
  xlab("Date") + ylab("Return") +
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_date(date_labels = "%b %y", date_breaks = "6 months")

#UPS returns in 2022 specifically
UPS_ret22 <- subset(UPS_ret, index(UPS_ret) > "2022-01-01")

ggplot(UPS_ret22, aes(x = index(UPS_ret22), y = UPS_ret22)) +
  geom_line(color = "brown4") +
  ggtitle("UPS returns series in 2022") + xlab("Date") + ylab("Return") +
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_date(date_labels = "%b %y", date_breaks = "1 months")

#summarize
summary(UPS_ret22)
sd(UPS_ret22)


#FedEx stock returns
FDX_ret <- diff(log(FDX[,6]))
FDX_ret <- FDX_ret[-1,]

#plotting FDX returns over last decade
ggplot(FDX_ret, aes(x = index(FDX_ret), y = FDX_ret)) +
  geom_line(color = "purple") +
  ggtitle("FDX 10 year returns series") +
  xlab("Date") + ylab("Return") +
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_date(date_labels = "%b %y", date_breaks = "6 months")

#FDX returns in 2022 
FDX_ret22 <- subset(FDX_ret, index(FDX_ret) > "2022-01-01")

ggplot(FDX_ret22, aes(x = index(FDX_ret22), y = FDX_ret22)) +
  geom_line(color = "purple") +
  ggtitle("FDX returns series in 2022") + xlab("Date") + ylab("Return") +
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_date(date_labels = "%b %y", date_breaks = "1 months")

#Analyze returns 
summary(FDX_ret22)
sd(FDX_ret22)

#Amazon stock returns
AMZN_ret <- diff(log(AMZN[,6]))
AMZN_ret <- AMZN_ret[-1,]

#Amazon stock returns
AMZN_ret <- diff(log(AMZN[,6]))
AMZN_ret <- AMZN_ret[-1,]

#plotting AMZN returns over last decade
ggplot(AMZN_ret, aes(x = index(AMZN_ret), y = AMZN_ret)) +
  geom_line(color = "orange") +
  ggtitle("AMZN 10 year returns series") +
  xlab("Date") + ylab("Return") +
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_date(date_labels = "%b %y", date_breaks = "6 months")

#AMZN returns in 2022 
AMZN_ret22 <- subset(AMZN_ret, index(AMZN_ret) > "2022-01-01")

ggplot(AMZN_ret22, aes(x = index(AMZN_ret22), y = AMZN_ret22)) +
  geom_line(color = "orange") +
  ggtitle("AMZN returns series in 2022") + xlab("Date") + ylab("Return") +
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_date(date_labels = "%b %y", date_breaks = "1 months")

#Analyze returns
summary(AMZN_ret22)
sd(AMZN_ret22)



#Time Series Analysis of Stock Performance S&P 500
#ADF + ACF Test S&P500
print(adf.test(GSPC$GSPC.Close))  
par(mfrow = c(1,2))
acf(GSPC$GSPC.Close)
pacf(GSPC$GSPC.Close)
par(mfrow = c(1,1))
modelfit_GSPC <-auto.arima(GSPC$GSPC.Close, lambda = "auto")   #Fit the model to the price data
summary(modelfit_GSPC)
#analyze residuals
plot(resid(modelfit_GSPC),ylab="Residuals",main="GSPC Residuals vs. Time") 
hist(resid(modelfit_GSPC),freq=F,ylim=c(0,9500),main="Histogram of GSPC Residuals") #histo of residuals
e=resid(modelfit_GSPC)
curve(dnorm(x, mean=mean(e), sd=sd(e)), add=TRUE, col="darkred")
#Diagnostics test (Ljung-Box plot) S&P 500
tsdiag(modelfit_GSPC)
#Lag = 2 box test
Box.test(modelfit_GSPC$residuals, lag= 2, type="Ljung-Box") #Analyze p value
Box.test(modelfit_GSPC$residuals, type="Ljung-Box") # run generalized test depending on p value of ^
price_forecastGSPC <- forecast(modelfit_GSPC,h=30) #30 day forecast for S&P 500
plot(price_forecastGSPC, main = "30 Day GSPC Forecast")
head(price_forecastGSPC$mean)
head(price_forecastGSPC$upper) #Upper 80% and 95% conf int
head(price_forecastGSPC$lower) #Lower 80% and 95% conf int


#Time Series Analysis of Stock Performance UPS
#ADF + ACF Test UPS
print(adf.test(UPS$UPS.Close))  
par(mfrow = c(1,2))
acf(UPS$UPS.Close)
pacf(UPS$UPS.Close)
par(mfrow = c(1,1))
modelfit_UPS <-auto.arima(UPS$UPS.Close, lambda = "auto")   #Fit the model to the price data
summary(modelfit_UPS)
#analyze residuals
plot(resid(modelfit_UPS),ylab="Residuals",main="UPS Residuals vs. Time") 
hist(resid(modelfit_UPS),freq=F,ylim=c(0,300),main="Histogram of UPS Residuals") #histo of residuals
e=resid(modelfit_UPS)
curve(dnorm(x, mean=mean(e), sd=sd(e)), add=TRUE, col="darkred")
#Diagnostics test (Ljung-Box plot) UPS
tsdiag(modelfit_UPS)
#Lag = 2 box test
Box.test(modelfit_UPS$residuals, lag= 2, type="Ljung-Box") #Analyze p value
Box.test(modelfit_UPS$residuals, type="Ljung-Box") # run generalized test depending on p value of ^
price_forecastUPS <- forecast(modelfit_UPS,h=30) #30 day forecast for UPS
plot(price_forecastUPS, main = "30 Day UPS Forecast")
head(price_forecastUPS$mean)
head(price_forecastUPS$upper) #Upper 80% and 95% conf int
head(price_forecastUPS$lower) #Lower 80% and 95% conf int


#Time Series Analysis of Stock Performance FDX
#ADF + ACF Test FDX
print(adf.test(FDX$FDX.Close))
par(mfrow = c(1,2))
acf(FDX$FDX.Close)
pacf(FDX$FDX.Close)
par(mfrow = c(1,1))
modelfit_FDX <-auto.arima(FDX$FDX.Close, lambda = "auto")   #Fit the model to the price data
summary(modelfit_FDX)
#analyze residuals
plot(resid(modelfit_FDX),ylab="Residuals",main="FDX Residuals vs. Time") 
hist(resid(modelfit_FDX),freq=F,ylim=c(0,100),main="Histogram of FDX Residuals") #histo of residuals
e=resid(modelfit_FDX)
curve(dnorm(x, mean=mean(e), sd=sd(e)), add=TRUE, col="darkred")
#Diagnostics test (Ljung-Box plot) FDX
tsdiag(modelfit_FDX)
#Lag = 2 box test
Box.test(modelfit_FDX$residuals, lag= 2, type="Ljung-Box") #Analyze p value
Box.test(modelfit_FDX$residuals, type="Ljung-Box") # run generalized test depending on p value of ^
price_forecastFDX <- forecast(modelfit_FDX,h=30) #30 day forecast for FDX
plot(price_forecastFDX, main = "30 Day FDX Forecast")
head(price_forecastFDX$mean)
head(price_forecastFDX$upper) #Upper 80% and 95% conf int
head(price_forecastFDX$lower) #Lower 80% and 95% conf int


#Time Series Analysis of Stock Performance AMZN 
#ADF + ACF Test AMZN
print(adf.test(AMZN$AMZN.Close))
par(mfrow = c(1,2))
acf(AMZN$AMZN.Close)
pacf(AMZN$AMZN.Close)
par(mfrow = c(1,1))
modelfit_AMZN <-auto.arima(AMZN$AMZN.Close, lambda = "auto")   #Fit the model to the price data
summary(modelfit_AMZN)
#analyze residuals
plot(resid(modelfit_AMZN),ylab="Residuals",main="AMZN Residuals vs. Time") 
hist(resid(modelfit_AMZN),freq=F,ylim=c(0,300),main="Histogram of AMZN Residuals") #histo of residuals
e=resid(modelfit_AMZN)
curve(dnorm(x, mean=mean(e), sd=sd(e)), add=TRUE, col="darkred")
#Diagnostics test (Ljung-Box plot) AMZN
tsdiag(modelfit_AMZN)
#Lag = 2 box test
Box.test(modelfit_AMZN$residuals, lag= 2, type="Ljung-Box") #Analyze p value
Box.test(modelfit_AMZN$residuals, type="Ljung-Box") # run generalized test depending on p value of ^
price_forecastAMZN <- forecast(modelfit_AMZN,h=30) #30 day forecast for FDX
plot(price_forecastAMZN, main = "30 Day AMZN Forecast")
head(price_forecastAMZN$mean)
head(price_forecastAMZN$upper) #Upper 80% and 95% conf int
head(price_forecastAMZN$lower) #Lower 80% and 95% conf int










