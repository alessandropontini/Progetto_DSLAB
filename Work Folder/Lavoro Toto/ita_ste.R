ds_ita8 <- read.csv("C:\\Users\\vizzi\\PROG_DSLAB_GITHUB\\Progetto_DSLAB\\DATASET SERIE STORICHE\\italia8mattina.csv")

ds_ita8_diff<-diff(ds_ita8[,"TSTOT.ConsumiITA"], lag = 7)

trainzero<-ds_ita8_diff[724:1088]


# testzero<- ds_ita8_diff[1089:1453]
testts<-ds_ita8[1089:1453,"TSTOT.ConsumiITA"]
testzerots <-  ts(coredata(testzero), freq = 365, start = c(2015,1),  end = c(2015,365))
train8orets <-  ts(coredata(trainzero), freq = 365, start = c(2014,1),  end = c(2014,365))


lambda2 <- BoxCox.lambda(train8orets, method = c("guerrero"))

dsboxcox <-  BoxCox(train8orets, lambda2)

x7 <- fourier(train8orets, K=16)


fit10 <- auto.arima(dsboxcox, seasonal=T, xreg =x7, lambda = lambda2, trace = T)

pred <- InvBoxCox(forecastboxcox$mean,lambda2)

summary(fit10)

fit10 %>%
  forecast(xreg=fourier(dsboxcox, K=16, h=365)) -> forecastboxcox

forecast8<-testts[8:365]
forecast1358<-forecastboxcox$mean[1:358]

final<-forecast8+forecast1358

finalts <-  ts(coredata(final), freq = 365, start = c(2015,1),  end = c(2015,365))


test2 <-  ts(coredata(testts), freq = 365, start = c(2015,1),  end = c(2015,365))
plot(finalts, )+ lines(test2, col=2)

accmeasures1=regr.eval(test2,finalts)

vet<-c(1:5)

diff1<-diff(vet)
