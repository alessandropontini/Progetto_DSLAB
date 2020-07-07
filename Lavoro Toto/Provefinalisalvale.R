library(zoo)
library(forecast)


# boxcox quello buono
ds_ita8 <- read.csv("/Volumes/HDD_Ale/Progetto_DSLAB/DATASET SERIE STORICHE/italia8mattina.csv")

ds_ita8 <-  ts(coredata(ds_ita8[,"TSTOT.ConsumiITA"]), freq = 365, start = c(2012,1),  end = c(2015,365))

lambda <- BoxCox.lambda(ds_ita8, method = "guerrero")

dsboxcox <-  BoxCox(ds_ita8, lambda)

x7 <- fourier(dsboxcox, K=16)

fit10 <- auto.arima(dsboxcox, seasonal=T, xreg=x7, trace = T)

summary(fit10)

# prova salvatore

ds_ita8 <-  ts(coredata(ds_ita8[,"TSTOT.ConsumiITA"]), freq = 365, start = c(2012,1),  end = c(2015,365))

lambda <- BoxCox.lambda(ds_ita8, method = "guerrero")

#dsboxcox <-  BoxCox(ds_ita8, lambda)

x8 <- fourier(ds_ita8, K=16)

fit11 <- auto.arima(ds_ita8, seasonal=T,lambda = lambda, xreg=x8, trace = T)

summary(fit11)

# terza prova finale

ds_ita8 <-  ts(coredata(ds_ita8[,"TSTOT.ConsumiITA"]), freq = 365, start = c(2012,1),  end = c(2015,365))

lambda2 <- BoxCox.lambda(ds_ita8, method = "guerrero")

dsboxcox <-  BoxCox(ds_ita8, lambda2)

x9 <- fourier(ds_ita8, K=16)

fit12 <- auto.arima(dsboxcox, seasonal=T , lambda = lambda2, xreg=x9, trace = T)

summary(fit12)

fit12 %>%
  forecast(xreg=fourier(dsboxcox, K=16, h=7)) -> forecastboxcox

pred <- InvBoxCox(forecastboxcox$mean,lambda2)
predlow <- InvBoxCox(forecastboxcox$lower,lambda2)

predhigh <- InvBoxCox(forecastboxcox$upper,lambda2)


predts <-  ts(pred, freq = 365, start = c(2016,1),  end = c(2016,7))
predtslow <-  ts(predlow, freq = 365, start = c(2016,1),  end = c(2016,7))
predtshigh<-  ts(predhigh, freq = 365, start = c(2016,1),  end = c(2016,7))

intervalliconfidenza <- ts.intersect(predtslow,predtshigh)



dicembre_2015 <-  ts(coredata(ds_ita8[1429:1460]), freq = 365, start = c(2015,335),  end = c(2015,365))
plot(dicembre_2015, col=1,) + lines(predts, col=3)

dfdicembre_2015 <- as.data.frame(dicembre_2015)
dfpred <- as.data.frame(predts)
dfci <- as.data.frame(intervalliconfidenza)

values = seq(from = as.Date("2015-12-01"), to = as.Date("2015-12-31"), by = 'day')
dfdicembre_2015 <- cbind(dfdicembre_2015,values)

values = seq(from = as.Date("2016-01-01"), to = as.Date("2016-01-07"), by = 'day')
dfpred <- cbind(dfpred,values)
dfci <- cbind(dfci,values)
dftot <- cbind(dfpred,dfci)
dftot <- dftot[-2]

dftot <-  as.data.frame(dftot)


library(ggplot2)

ggplot(data = dfdicembre_2015, aes(x = values, y = x))+
  geom_line(color = 1, size = 0.5) +
  geom_line(data=dftot,aes(x = values, y = x),color = 2, size = 0.5) +
  geom_ribbon(data=dftot,aes(ymin=`predtslow.80%`,ymax=`predtshigh.80%`),alpha=0.3) +
  geom_ribbon(data=dftot,aes(ymin=`predtslow.95%`,ymax=`predtshigh.95%`),alpha=0.1) + 
  ylab("Consumi in KW") + xlab("Tempo") + ggtitle("Predizione Arima Ora 8 di Mattina") +
  scale_y_continuous(expand = c(0.3,0.3))

  #geom_line(color = 3, size = 2) 
# autoplot(dicembre_2015) + autolayer(predts, col=3) + autolayer(intervalliconfidenza[,"predtslow.80%"], col=4) +
# autolayer(intervalliconfidenza[,"predtshigh.80%"], col=4)

