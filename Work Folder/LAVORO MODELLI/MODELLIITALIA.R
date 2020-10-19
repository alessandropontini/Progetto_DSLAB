# 8 ore
library(zoo)
library(forecast)
ds_ita8 <- read.csv("/Volumes/HDD_Ale/Progetto_DSLAB/DATASET SERIE STORICHE/italia8mattina.csv")

ds_ita8 <-  ts(coredata(ds_ita8[,"TSTOT.ConsumiITA"]), freq = 365, start = c(2012,1),  end = c(2015,365))

x7 <- fourier(ds_ita8, K=16)

fit9 <- auto.arima(ds_ita8, seasonal=T, lambda=0, xreg=x7, trace = T)

summary(fit9)
# ARIMA (5,1,2)
# MAE_TRAINING : 1661.747
# RMSE_TRAINIG : 1123.495

png("/Volumes/HDD_Ale/Progetto_DSLAB/IMMAGINI/plotitaliaarima8.png",4032,3024, units="px", res=500)

fit9 %>%
  forecast(xreg=fourier(ds_ita8, K=16, h=365)) %>%
  autoplot(include=365) + xlab("Anni") + ylab("Consumi in KW") +
  ggtitle("Previsione consumi Italia con ARIMA(5,1,2)")

dev.off()

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

summary(fit11)





fit10 %>%
  forecast(xreg=fourier(dsboxcox, K=16, h=7)) ->

pred <- InvBoxCox(fit10$fitted,lambda)

fit10 %>%
  forecast(xreg=fourier(dsboxcox, K=16, h=365)) %>%
  autoplot(include=365) + xlab("Anni") + ylab("Consumi in KW") +
  ggtitle("Previsione consumi Italia con ARIMA(5,1,2)")

plot(pred, col=2) + lines(ds_ita8)
# TEST E TRAIN 

train8ore <- ds_ita8[1:1095,"TSTOT.ConsumiITA"]
test8ore <- ds_ita8[1096:1490,"TSTOT.ConsumiITA"]

train8orets <-  ts(coredata(train8ore), freq = 365, start = c(2012,1),  end = c(2014,365))
test8orets <-  ts(coredata(test8ore), freq = 365, start = c(2015,1),  end = c(2015,365))

lambda <- BoxCox.lambda(train8orets, method = "guerrero")

dsboxcox <-  BoxCox(train8orets, lambda)

x7 <- fourier(dsboxcox, K=16)

fit10 <- auto.arima(dsboxcox, seasonal=T, xreg=x7, trace = T)

summary(fit10)

fit10 %>%
  forecast(xreg=fourier(dsboxcox, K=16, h=365), 365) -> forecastboxcox

# fit10 %>%
#   forecast(xreg=fourier(dsboxcox, K=16, h=365), 365) %>%
#   autoplot(include=365) + xlab("Anni") + ylab("Consumi in KW") +
#   ggtitle("Previsione consumi Italia con ARIMA(5,1,1)")

pred <- InvBoxCox(forecastboxcox$mean,lambda)
pred

plot(train8orets, col=1) + lines(test8orets, col=2) + lines(pred, col=3)

acc=accuracy(pred, test = test8orets)
accmeasures1=regr.eval(test8orets, pred)

accmeasures1
fit8 %>%
  forecast(xreg=fourier(train8orets, K=16, h=365)) -> forecast8
install.packages("MLmetrics")

library(MLmetrics)
acc=Accuracy(y_pred = forecast8, y_true = test8orets)
library(DMwR)
forecast1=predict(fit8,365)
class(forecast8)

accmeasures1=regr.eval(test8orets, forecast8$Forecast)
acc
accmeasures1
?auto.arima
 # fit9 %>%
#   forecast(xreg=fourier(ds_ita8, K=16, h=7)) %>%
#   autoplot(include=31) + xlab("Anni") + ylab("Consumi in KW") +
#   ggtitle("Previsione consumi Italia con ARIMA(5,1,1)")


# 12 ore

ds_ita8 <- read.csv("/Volumes/HDD_Ale/Progetto_DSLAB/DATASET SERIE STORICHE/italia12mattina.csv")

ds_ita8 <-  ts(coredata(ds_ita8[,"TSTOT.ConsumiITA"]), freq = 365, start = c(2012,1),  end = c(2015,365))

x7 <- fourier(ds_ita8, K=16)

fit9 <- auto.arima(ds_ita8, seasonal=T, lambda=0, xreg=x7, trace = T)

summary(fit9)
# ARIMA (5,1,1)
# MAE_TRAINING : 3608.489
# RMSE_TRAINIG : 2835.01

png("/Volumes/HDD_Ale/Progetto_DSLAB/IMMAGINI/plotitaliaarima12.png",4032,3024, units="px", res=500)

fit9 %>%
  forecast(xreg=fourier(ds_ita8, K=16, h=7)) %>%
  autoplot(include=31) + xlab("Anni") + ylab("Consumi in KW") +
  ggtitle("Previsione consumi Italia con ARIMA(5,1,1)")

dev.off()

# TRAIN TEST

train8ore <- ds_ita8[1:1095,"TSTOT.ConsumiITA"]
test8ore <- ds_ita8[1096:1490,"TSTOT.ConsumiITA"]

train8orets <-  ts(coredata(train8ore), freq = 365, start = c(2012,1),  end = c(2014,365))
test8orets <-  ts(coredata(test8ore), freq = 7, start = c(2015,1),  end = c(2015,7))

lambda <- BoxCox.lambda(train8orets, method = "guerrero")


lambda2 <- BoxCox.lambda(train8orets, method = "loglik")


dsboxcox <-  BoxCox(train8orets, lambda2)

x7 <- fourier(dsboxcox, K=16)
#x7 <- fourier(train8orets, K=64)

fit10 <- auto.arima(train8orets, seasonal=F, xreg=x7, trace = T)

summary(fit10)

fit10 %>%
   forecast(xreg=fourier(dsboxcox, K=16, h=7)) -> forecastboxcox

# fit10 %>%
#   forecast(xreg=fourier(train8orets, K=64, h=365), 365) -> forecastboxcox

pred <- InvBoxCox(forecastboxcox$mean,lambda)

plot(predts, col=2) + lines(test8orets, col=3)




predts <- ts(coredata(pred[1:7]), freq = 7, start = c(2015,1),  end = c(2015,7))
accmeasures1=regr.eval(test8orets, forecastboxcox$mean)
accmeasures1

RMSE(test8orets, forecastboxcox$mean)
MAE(test8orets, forecastboxcox$mean)


?MLmetrics
acc
accmeasures1
?Accuracy

###########################################################
###########################################################

ds_ita8 <- read.csv("/Volumes/HDD_Ale/Progetto_DSLAB/DATASET SERIE STORICHE/italia12mattina.csv")

ds_ita8 <-  ts(coredata(ds_ita8[,"TSTOT.ConsumiITA"]), freq = 365, start = c(2012,1),  end = c(2015,365))

lambda <- BoxCox.lambda(ds_ita8, method = c("guerrero","loglik"))


lambda2 <- BoxCox.lambda(ds_ita8, method = "loglik")

dsboxcox <-  BoxCox(ds_ita8, lambda)

x7 <- fourier(dsboxcox, K=16)


fit10 <- auto.arima(ds_ita8, seasonal=F, xreg=x7, trace = T)

summary(fit10)

fit10 %>%
  forecast(xreg=fourier(dsboxcox, K=16, h=7),7) -> forecastboxcox

pred <- InvBoxCox(forecastboxcox$mean,lambda)

forecastboxcox$mean %>%
  InvBoxCox(lambda) %>% 
  autoplot() + xlab("Anni") + ylab("Consumi in KW") +
  ggtitle("Previsione consumi Italia con ARIMA(5,1,2)") + lines(ds_ita8, col=3)


plot(window(ds_ita8, start = c(2015,12))) + lines(pred, col=3)

summary(fit9)
# ARIMA (5,1,1)
# MAE_TRAINING : 3608.489
# RMSE_TRAINIG : 2835.01


##################################################################
##################################################################

# 20 ore

ds_ita8 <- read.csv("/Volumes/HDD_Ale/Progetto_DSLAB/DATASET SERIE STORICHE/italia20sera.csv")

ds_ita8 <-  ts(coredata(ds_ita8[,"TSTOT.ConsumiITA"]), freq = 365, start = c(2012,1),  end = c(2015,365))

x7 <- fourier(ds_ita8, K=16)

fit9 <- auto.arima(ds_ita8, seasonal=T, lambda=0, xreg=x7, trace = T)

summary(fit9)
# ARIMA (5,1,1)
# MAE_TRAINING : 3243.808
# RMSE_TRAINIG : 2495.087

png("/Volumes/HDD_Ale/Progetto_DSLAB/IMMAGINI/plotitaliaarima20.png",4032,3024, units="px", res=500)

fit9 %>%
  forecast(xreg=fourier(ds_ita8, K=16, h=7)) %>%
  autoplot(include=31) + xlab("Anni") + ylab("Consumi in KW") +
  ggtitle("Previsione consumi Italia con ARIMA(5,1,2)")

dev.off()

####################################################################################