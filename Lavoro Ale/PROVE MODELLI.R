library(dplyr)
library(stringi)
library(tidyr)
library(zoo)
library(ggplot2)
library(ggfortify)
library(lubridate)
library(xts)
library(forecast)


TSTOT2 <- read.csv.zoo("/Volumes/HDD_Ale/Progetto_DSLAB/DATASET SERIE STORICHE/Completissimo.csv")

TSTOT <-  ts(coredata(TSTOT2), freq = 8760, start = c(2012,1,1),  end = c(2015,8760))

x <- cbind( Consumi=TSTOT[,"ConsumiPol"],
            Gradi=TSTOT[,"GradiPol"],
            BankHolidayPol=TSTOT[,"BankHolidayPol"],
            EndYearPol=TSTOT[,"EndYearPol"],
            DayOfWeekPol=TSTOT[,"DayOfWeekPol"]
            #DayOffPol=TSTOT[,"GradiPol"]
)

y <- TSTOT[,"PrezzoPol"]

# NO DIFFED
fit1 <- auto.arima(y=y, xreg=x, seasonal=FALSE)
summary(fit1)
autoplot(fit1)
cbind("Regression Errors" = residuals(fit1, type="regression"),
      "ARIMA errors" = residuals(fit1, type="innovation")) %>%
  autoplot(facets=TRUE)

checkresiduals(fit1)
# diffed
library(urca)
TSTOT %>% diff() %>% ur.kpss() %>% summary()

TSTOTDIFFED <- diff(TSTOT, lag = 42)
x <- cbind( Consumi=TSTOTDIFFED[,"ConsumiPol"],
            Gradi=TSTOTDIFFED[,"GradiPol"],
            BankHolidayPol=TSTOTDIFFED[,"BankHolidayPol"],
            EndYearPol=TSTOTDIFFED[,"EndYearPol"],
            DayOfWeekPol=TSTOTDIFFED[,"DayOfWeekPol"]
            #DayOffPol=TSTOT[,"GradiPol"]
)

y <- TSTOTDIFFED[,"PrezzoPol"]

# NO DIFFED
fit1 <- auto.arima(y=y, xreg=x, seasonal=FALSE)
summary(fit1)
#fit3 <- tslm(TSTOTDIFFED[,"PrezzoPol"] ~ trend + season)
#summary(fit3)


beerfit1 <- meanf(TSTOTDIFFED[,"PrezzoPol"],h=8760)
beerfit2 <- rwf(TSTOTDIFFED[,"PrezzoPol"],h=8760)
beerfit3 <- snaive(TSTOTDIFFED[,"PrezzoPol"],h=8760)


autoplot(window(TSTOTDIFFED[,"PrezzoPol"], start=2015)) +
  autolayer(beerfit1, series="Mean", PI=FALSE) +
  autolayer(beerfit2, series="Naïve", PI=FALSE) +
  #autolayer(beerfit3, series="Seasonal naïve", PI=FALSE) +
  xlab("Year") + ylab("Euros") +
  ggtitle("Forecasts for price") +
  guides(colour=guide_legend(title="Forecast"))

ggseasonplot(TSTOTDIFFED[,"PrezzoPol"], year.labels=TRUE, year.labels.left=TRUE) +
  ylab("$ million") +
  ggtitle("Seasonal plot: antidiabetic drug sales")


# lag=h and fitdf=K
#Box.test(TSTOTDIFFED[,"PrezzoPol"], lag=7000, fitdf=0)

#Box.test(TSTOTDIFFED[,"PrezzoPol"],lag=7000, fitdf=0, type="Lj")

#checkresiduals(naive(TSTOTDIFFED[,"PrezzoPol"]))


googfc1 <- meanf(TSTOTDIFFED[,"PrezzoPol"], h=8760)
googfc2 <- rwf(TSTOTDIFFED[,"PrezzoPol"], h=8760)
googfc3 <- rwf(TSTOTDIFFED[,"PrezzoPol"], drift=TRUE, h=8760)
autoplot(window(TSTOTDIFFED[,"PrezzoPol"], start=2015)) +
  autolayer(googfc1, PI=FALSE, series="Mean") +
  autolayer(googfc2, PI=FALSE, series="Naïve") +
  autolayer(googfc3, PI=FALSE, series="Drift") +
  xlab("Day") + ylab("Closing Price (US$)") +
  ggtitle("Google stock price (daily ending 6 Dec 13)") +
  guides(colour=guide_legend(title="Forecast"))
x <- naive(TSTOTDIFFED[,"PrezzoPol"], h=8760)
autoplot(window(TSTOTDIFFED[,"PrezzoPol"], start=2015))+
  autolayer(x, PI=FALSE, series="Naïve")

x <- forecast(TSTOTDIFFED[,"PrezzoPol"], h=168)
autoplot(window(TSTOTDIFFED[,"PrezzoPol"], start=c(2015,12,25)))+
  autolayer(x, PI=FALSE)
?naive
?window
?autolayer
?holt
?croston
fit <- auto.arima(TSTOTDIFFED[,"PrezzoPol"], seasonal=FALSE)
fit1 <- auto.arima(y=y, xreg=as.matrix(x), seasonal=FALSE)

fit1 %>% forecast(h=504) %>% autoplot(include=80)
fit %>% forecast(h=504) %>% autoplot(include=80)
forecastfit <- fit %>% forecast(h=24)

install.packages("fUnitRoots")
library("fUnitRoots")
urkpssTest(TSTOTDIFFED[,"PrezzoPol"], type = c("tau"), lags = c("short"),use.lag = NULL, doplot = TRUE)
tsstationary = diff(TSTOTDIFFED[,"PrezzoPol"], differences=1)
plot(tsstationary)

firArima <- auto.arima(TSTOTDIFFED[,"PrezzoPol"], trace=TRUE, seasonal = FALSE)

futurVal <- forecast(firArima ,h=504, level=c(99.5))

autoplot(window(TSTOTDIFFED[,"PrezzoPol"], start=c(2015,12))) +
  autolayer(googfc1, PI=FALSE, series="Mean")
?fourier
fit <- auto.arima(TSTOTDIFFED[,"PrezzoPol"], seasonal=FALSE,
                  xreg=fourier(TSTOTDIFFED[,"PrezzoPol"], K=c(8760,2)))

nrow(TSTOTDIFFED[,"PrezzoPol"])
z = fourier(TSTOTDIFFED[,"PrezzoPol"], K=c(17520,17520) ,h=17520)


train <- TSTOTDIFFED[1:27998,"PrezzoPol"]
test <- TSTOTDIFFED[27999:34998,"PrezzoPol"]
arima_model <- auto.arima(train, seasonal = FALSE)
forecast1=predict(arima_model,7000)

library(DMwR)
autoplot(forecast1)
accmeasures1=regr.eval(test, forecast1$pred)
