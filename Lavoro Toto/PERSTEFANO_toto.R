###################################################################################################
###################### SENZA TRAIN E TEST #########################################################
###################################################################################################
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

plot(window(ds_ita8, start = c(2015,12))) + lines(pred, col=3)

###################################################################################################
################################# CON TRAIN E TEST ################################################
###################################################################################################

train8ore <- ds_ita8[1:1095,"TSTOT.ConsumiITA"]
test8ore <- ds_ita8[1096:1490,"TSTOT.ConsumiITA"]

train8orets <-  ts(coredata(train8ore), freq = 365, start = c(2012,1),  end = c(2014,365))
test8orets <-  ts(coredata(test8ore), freq = 7, start = c(2015,1),  end = c(2015,7))

# 2 TIPI LAMBDA
lambda <- BoxCox.lambda(train8orets, method = "guerrero")
#@lambda <- BoxCox.lambda(train8orets, method = "loglik")

dsboxcox <-  BoxCox(train8orets, lambda2)

x7 <- fourier(dsboxcox, K=16)

fit10 <- auto.arima(train8orets, seasonal=F, xreg=x7, trace = T)

summary(fit10)

fit10 %>%
  forecast(xreg=fourier(dsboxcox, K=16, h=7)) -> forecastboxcox

pred <- InvBoxCox(forecastboxcox$mean,lambda)
predts <- ts(coredata(pred[1:7]), freq = 7, start = c(2015,1),  end = c(2015,7))



plot(predts, col=2) + lines(test8orets, col=3)

predts <- ts(coredata(pred[1:7]), freq = 7, start = c(2015,1),  end = c(2015,7))
accmeasures1=regr.eval(test8orets, forecastboxcox$mean)


RMSE(test8orets, forecastboxcox$mean)
MAE(test8orets, forecastboxcox$mean)
