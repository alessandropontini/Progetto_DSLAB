library(dplyr)
library(stringi)
library(tidyr)
library(zoo)
library(ggplot2)
library(ggfortify)
library(lubridate)
library(xts)
library(forecast)
#install.packages("GGally")
library(GGally)
library(Hmisc)

library(pander)

library(car)

library(olsrr)

library(systemfit)

library(het.test)


?auto.arima


TSTOT2 <- read.csv.zoo("/Volumes/HDD_Ale/Progetto_DSLAB/DATASET SERIE STORICHE/Completissimo.csv")

TSTOT <-  ts(coredata(TSTOT2), freq = 8760, start = c(2012,1,1),  end = c(2015,8760))

x <- cbind( Prezzi=TSTOT[,"PrezzoPol"],
            Gradi=TSTOT[,"GradiPol"],
            BankHolidayPol=TSTOT[,"BankHolidayPol"],
            EndYearPol=TSTOT[,"EndYearPol"],
            DayOfWeekPol=TSTOT[,"DayOfWeekPol"]
            #DayOffPol=TSTOT[,"GradiPol"]
)

y <- TSTOT[,"ConsumiPol"]

polonia <- cbind(Prezzi=TSTOT[,"PrezzoPol"],
                 Gradi=TSTOT[,"GradiPol"],
                 BankHolidayPol=TSTOT[,"BankHolidayPol"],
                 EndYearPol=TSTOT[,"EndYearPol"],
                 DayOfWeekPol=TSTOT[,"DayOfWeekPol"],
                 Consumi = TSTOT[,"ConsumiPol"])

regressione <- tslm(y  ~ x, data = TSTOT)

# polonia %>%
#   as.data.frame() %>%
#   GGally::ggpairs()
# 
# summary(regressione)
# 
# autoplot(polonia[,'Consumi'], series="Data") +
#   autolayer(fitted(regressione), series="Fitted") +
#   xlab("Year") + ylab("") +
#   guides(colour=guide_legend(title=" "))
# 
# #checkresiduals(regressione)
# 
df <- as.data.frame(polonia)
# df[,"Residuals"]  <- as.numeric(residuals(regressione))
# p1 <- ggplot(df, aes(x=Prezzi, y=Residuals)) +
#   geom_point()
# p2 <- ggplot(df, aes(x=Gradi, y=Residuals)) +
#   geom_point()
# 
# gridExtra::grid.arrange(p1, p2, nrow=2)

white.test <- function(lmod,data){
  
  u2 <- lmod$residuals^2
  
  y <- fitted(lmod)
  
  Ru2 <- summary(lm(u2 ~ y + I(y^2)))$r.squared
  
  LM <- nrow(data)*Ru2
  
  p.value <- 1-pchisq(LM, 2)
  
  data.frame("Test statistic"=LM,"P value"=p.value)
  
}

# OMOSCHEDASTICO
white.test(regressione,polonia)

# dwatson
dwtest(regressione)

# aggiungiamo dummy
Dummies_frame_Scandi[rep(seq_len(nrow(Dummies_frame_Scandi)), each = 24), ]-> dummy_scandi2

df1 <-data.frame("lunedi" = 1:1460, "martedi" = 1:1460, "mercoledi" = 1:1460, "giovedi" = 1:1460, "venerdi" = 1:1460, "sabato" = 1:1460, "domenica"=1:1460)
# df1$lunedi <- ""
# df1$martedi <- ""
# df1$mercoledi <- ""
# df1$giovedi <- ""
# df1$venerdi <- ""
# df1$sabato <- ""
# df1$domenica <- ""



j = 1
while(j<59){
    df1$domenica[j] = 1
    df1$lunedi[j] = 0
    df1$martedi[j] = 0
    df1$mercoledi[j] = 0
    df1$giovedi[j] = 0
    df1$venerdi[j] = 0
    df1$sabato[j]= 0
    j = j + 7
}  

j = 2
while(j<59){
  df1$domenica[j] = 0
  df1$lunedi[j] = 1
  df1$martedi[j] = 0
  df1$mercoledi[j] = 0
  df1$giovedi[j] = 0
  df1$venerdi[j] = 0
  df1$sabato[j]= 0
  j = j + 7
}  

j = 3
while(j<59){
  df1$domenica[j] = 0
  df1$lunedi[j] = 0
  df1$martedi[j] = 1
  df1$mercoledi[j] = 0
  df1$giovedi[j] = 0
  df1$venerdi[j] = 0
  df1$sabato[j]= 0
  j = j + 7
}  
j = 4
while(j<59){
  df1$domenica[j] = 0
  df1$lunedi[j] = 0
  df1$martedi[j] = 0
  df1$mercoledi[j] = 1
  df1$giovedi[j] = 0
  df1$venerdi[j] = 0
  df1$sabato[j]= 0
  j = j + 7
}  
j = 5
while(j<59){
  df1$domenica[j] = 0
  df1$lunedi[j] = 0
  df1$martedi[j] = 0
  df1$mercoledi[j] = 0
  df1$giovedi[j] = 1
  df1$venerdi[j] = 0
  df1$sabato[j]= 0
  j = j + 7
}  
j = 6
while(j<59){
  df1$domenica[j] = 0
  df1$lunedi[j] = 0
  df1$martedi[j] = 0
  df1$mercoledi[j] = 0
  df1$giovedi[j] = 0
  df1$venerdi[j] = 1
  df1$sabato[j]= 0
  j = j + 7
}  
j = 7
while(j<59){
  df1$domenica[j] = 0
  df1$lunedi[j] = 0
  df1$martedi[j] = 0
  df1$mercoledi[j] = 0
  df1$giovedi[j] = 0
  df1$venerdi[j] = 0
  df1$sabato[j]= 1
  j = j + 7
}  
df1

j = 59
  df1$domenica[j] = 0
  df1$lunedi[j] = 0
  df1$martedi[j] = 1
  df1$mercoledi[j] = 0
  df1$giovedi[j] = 0
  df1$venerdi[j] = 0
  df1$sabato[j]= 0
  
  
j = 60
  while(j<nrow(df1)){
    df1$domenica[j] = 0
    df1$lunedi[j] = 0
    df1$martedi[j] = 0
    df1$mercoledi[j] = 0
    df1$giovedi[j] = 1
    df1$venerdi[j] = 0
    df1$sabato[j]= 0
    j = j + 7
  }  
  
  j = 61
  while(j<nrow(df1)){
    df1$domenica[j] = 0
    df1$lunedi[j] = 0
    df1$martedi[j] = 0
    df1$mercoledi[j] = 0
    df1$giovedi[j] = 0
    df1$venerdi[j] = 1
    df1$sabato[j]= 0
    j = j + 7
  }  
  
  j = 62
  while(j<nrow(df1)){
    df1$domenica[j] = 0
    df1$lunedi[j] = 0
    df1$martedi[j] = 0
    df1$mercoledi[j] = 0
    df1$giovedi[j] = 0
    df1$venerdi[j] = 0
    df1$sabato[j]= 1
    j = j + 7
  }  
  j = 63
  while(j<nrow(df1)){
    df1$domenica[j] = 1
    df1$lunedi[j] = 0
    df1$martedi[j] = 0
    df1$mercoledi[j] = 0
    df1$giovedi[j] = 0
    df1$venerdi[j] = 0
    df1$sabato[j]= 0
    j = j + 7
  }  
  j = 64
  while(j<nrow(df1)){
    df1$domenica[j] = 0
    df1$lunedi[j] = 1
    df1$martedi[j] = 0
    df1$mercoledi[j] = 0
    df1$giovedi[j] = 0
    df1$venerdi[j] = 0
    df1$sabato[j]= 0
    j = j + 7
  }  
  j = 65
  while(j<nrow(df1)){
    df1$domenica[j] = 0
    df1$lunedi[j] = 0
    df1$martedi[j] = 1
    df1$mercoledi[j] = 0
    df1$giovedi[j] = 0
    df1$venerdi[j] = 0
    df1$sabato[j]= 0
    j = j + 7
  }  
  j = 66
  while(j<nrow(df1)){
    df1$domenica[j] = 0
    df1$lunedi[j] = 0
    df1$martedi[j] = 0
    df1$mercoledi[j] = 1
    df1$giovedi[j] = 0
    df1$venerdi[j] = 0
    df1$sabato[j]= 0
    j = j + 7
  }  

  j = 1460
  df1$domenica[j] = 0
  df1$lunedi[j] = 0
  df1$martedi[j] = 0
  df1$mercoledi[j] = 0
  df1$giovedi[j] = 0
  df1$venerdi[j] = 1
  df1$sabato[j]= 0  

  
summary(df1)
nrow(df1)
Dummies_frame_Scandi[rep(seq_len(nrow(Dummies_frame_Scandi)), each = 24), ]-> dummy_scandi2
df1[rep(seq_len(nrow(df1)), each = 24), ]-> df3

nrow(df3)
summary(df3)
write.csv(df3, "/Users/alessandropontini/Desktop/dummyseasonal.csv")
df1ts <- ts(df3, frequency = 8760, start = c(2012, 1),end=c(2015, 8760))

tail(df1ts)
tot <- cbind(TSTOT, df1ts)
tot2 <- ts.union(tot,df1ts)

tot2 %>%  as.data.frame() ->ghj


summary(ghj)
anyNA(tot2)
tail(tot2)
str(tot)
write.csv(tot2, "/Users/alessandropontini/Desktop/totalecondummyseasonal.csv")
#regressione2 <- tslm(y ~ trend + season )
names(tot)[1]

tot <- read.csv("/Volumes/HDD_Ale/Progetto_DSLAB/DATASET SERIE STORICHE/totalecondummyseasonal.csv")

totts <- ts(tot, frequency = 8760, start = c(2012, 1),end=c(2015, 8760))


consumi <- cbind(Scandinavia =  totts [,"TSTOT.ConsumiScandi"],
                 ITA = totts[,"TSTOT.ConsumiITA"]
                )
prezzi <- cbind(Scandinavia =  tot[,"TSTOT.PrezzoScandi"],
                 Polonia = tot[,"TSTOT.PrezzoPol"],
                 UK = tot[,"TSTOT.PrezzoUK"],
                 ITA = tot[,"TSTOT.PrezzoITA"]
)



ggseasonplot(consumi, polar = T) +
  ylab("$ kilowatt") +
  ggtitle("Seasonal plot: kilowatt")

ggsubseriesplot(consumi) +
  ylab("$ million") +
  ggtitle("Seasonal subseries plot: antidiabetic drug sales")

png("plot.png",4032,3024, units="px", res=500)
autoplot(consumi) + xlab("Anni") + ylab("Kilowatt per Ora") + ggtitle("Dati dal 2012 al 2015 del Mercato Elettrico")
dev.off()



png("plot2.png",4032,3024, units="px", res=500)
autoplot(prezzi) + xlab("Anni") + ylab("Prezzi Kilowatt € per Ora") + ggtitle("Dati dal 2012 al 2015 del Mercato Elettrico")
dev.off()

tot 
x <- cbind( Prezzi=tot[,"TSTOT.PrezzoPol"],
            Gradi=tot[,"TSTOT.GradiPol"],
            BankHolidayPol=tot[,"TSTOT.BankHolidayPol"],
            EndYearPol=tot[,"TSTOT.EndYearPol"],
            DayOfWeekPol=tot[,"TSTOT.DayOfWeekPol"],
            lunedi = tot[,"df1ts.lunedi"],
            martedi = tot[,"df1ts.martedi"],
            mercoledi = tot[,"df1ts.mercoledi"],
            giovedi = tot[,"df1ts.giovedi"],
            venerdi = tot[,"df1ts.venerdi"],
            sabato = tot[,"df1ts.sabato"]
)
x %>% as.data.frame() -> df2

y <- tot[,"TSTOT.ConsumiPol"]

regressione1 <- tslm(y  ~ x, data = tot, na.rm=T)

summary(regressione1)
# NO DIFFED

fit1 <- auto.arima(y=y, xreg=x, seasonal=FALSE)
summary(fit1)
autoplot(fit1)
cbind("Regression Errors" = residuals(fit1, type="regression"),
      "ARIMA errors" = residuals(fit1, type="innovation")) %>%
  autoplot(facets=TRUE)




#checkresiduals(fit1)
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
?accuracy
?auto.arima
library(DMwR)
autoplot(forecast1)
accmeasures1=regr.eval(test, forecast1$pred)

?fourier
