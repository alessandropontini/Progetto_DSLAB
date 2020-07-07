
library(zoo)
library(forecast)

ds_sca8 <- read.csv("C:\\Users\\vizzi\\PROG_DSLAB_GITHUB\\Progetto_DSLAB\\DATASET SERIE STORICHE\\scandinavia8mattina.csv")

# ds_sca8
ds_sca8<-  ts(coredata(ds_sca8[,"TSTOT.ConsumiScandi"]), freq = 365, start = c(2012,1,1),  end = c(2015,365))

library(ggplot2)

ds_sca8 %>% decompose(type="multiplicative") %>%
  autoplot() + xlab("Anni") +
  ggtitle("Classical multiplicative decomposition")

ds_ita8 <- read.csv("C:\\Users\\vizzi\\PROG_DSLAB_GITHUB\\Progetto_DSLAB\\DATASET SERIE STORICHE\\italia8mattina.csv")

ds_ita8<-  ts(coredata(ds_ita8[,"TSTOT.ConsumiITA"]), freq = 365, start = c(2012,1,1),  end = c(2015,365))

ds_ita8 %>% decompose(type="multiplicative") %>%
  autoplot() + xlab("Anni") +
  ggtitle("Classical multiplicative decomposition")

###########################################################

ds_ita8 <- read.csv("C:\\Users\\vizzi\\PROG_DSLAB_GITHUB\\Progetto_DSLAB\\DATASET SERIE STORICHE\\totalecondummyseasonal.csv")

ds_ita8<-  ts(coredata(ds_ita8[,"TSTOT.ConsumiITA"]), freq = 365, start = c(2012,1,1),  end = c(2015,365))

ds_ita8 %>% decompose(type="multiplicative") %>%
  autoplot() + xlab("Anni") +
  ggtitle("Classical multiplicative decomposition_Consumi Italia")


png("plotscandinaviConsumi.png",4032,3024, units="px", res=500)
fitsca %>%
  forecast(xreg=fourier(ds_sca12, K=4, h=7)) %>%
  autoplot() + xlab("Anni") + ylab("Consumi in KW") +
  ggtitle("Previsione consumi Scandinavia con ARIMA(5,1,1)")

dev.off()




################################################
##################################################################


# Arima

ds_ita8 <- read.csv("C:\\Users\\vizzi\\PROG_DSLAB_GITHUB\\Progetto_DSLAB\\DATASET SERIE STORICHE\\italia8mattina.csv")

ds_ita8 <-  ts(coredata(ds_ita8[,"TSTOT.ConsumiITA"]), freq = 365, start = c(2012,1),  end = c(2015,365))

lambda2 <- BoxCox.lambda(ds_ita8, method = c("guerrero"))


# lambda2 <- BoxCox.lambda(ds_ita8, method = "loglik")

# dsboxcox <-  BoxCox(ds_ita8, lambda)

dsboxcox <-  BoxCox(ds_ita8, lambda2)

x7 <- fourier(ds_ita8, K=16)


fit10 <- auto.arima(dsboxcox, seasonal=TRUE, xreg=x7, lambda = lambda2,  trace = T)

summary(fit10)

fit10 %>%
  forecast(xreg=fourier(dsboxcox, K=16, h=7),7) -> forecastboxcox

pred <- InvBoxCox(forecastboxcox$mean,lambda2)

plot(window(ds_ita8, start = c(2015,12))) + lines(pred, col=3)




################################################
# Train e test

ds_ita8 <- read.csv("C:\\Users\\vizzi\\PROG_DSLAB_GITHUB\\Progetto_DSLAB\\DATASET SERIE STORICHE\\italia8mattina.csv")
# ds_ita8<-  ts(coredata(ds_ita8[,"TSTOT.ConsumiITA"]), freq = 365, start = c(2012,1,1),  end = c(2015,365))

train8ore <- ds_ita8[1:1095,"TSTOT.ConsumiITA"]
test8ore <- ds_ita8[1096:1490,"TSTOT.ConsumiITA"]

train8orets <-  ts(coredata(train8ore), freq = 365, start = c(2012,1),  end = c(2014,365))
test8orets <-  ts(coredata(test8ore), freq = 365, start = c(2015,1),  end = c(2015,365))

lambda2 <- BoxCox.lambda(train8orets, method = c("guerrero"))



dsboxcox <-  BoxCox(train8orets, lambda2)

x7 <- fourier(train8orets, K=16)


fit10 <- auto.arima(dsboxcox, seasonal=TRUE, xreg=x7, lambda = lambda2,  trace = T)

summary(fit10)
#########################

fit10 %>%
  forecast(xreg=fourier(dsboxcox, K=16, h=342)) -> forecastboxcox


pred <- InvBoxCox(forecastboxcox$mean,lambda)
predts <- ts(coredata(pred[1:7]), freq = 7, start = c(2015,1),  end = c(2015,7))

??regr.eval
library(DMwR)

plot(test8orets, col=2) + lines(pred, col=3)

# predts <- ts(coredata(pred[1:7]), freq = 7, start = c(2015,1),  end = c(2015,7))
accmeasures1=regr.eval(test8orets,predts)



####################


# fit11 <- auto.arima(train8orets, seasonal=TRUE, xreg=x7, lambda = lambda2,  trace = T)

# summary(fit11)


############################################################################

dsboxcox <-  BoxCox(train8orets, lambda2)

x7 <- fourier(dsboxcox, K=16)

fit10 <- auto.arima(train8orets, seasonal=F, xreg=x7, trace = T)

summary(fit10)

fit10 %>%
  forecast(xreg=fourier(dsboxcox, K=16, h=7)) -> forecastboxcox




