###############

library(zoo)
library(forecast)
ds_sca8 <- read.csv("C:\\Users\\vizzi\\PROG_DSLAB_GITHUB\\Progetto_DSLAB\\DATASET SERIE STORICHE\\scandinavia8mattina.csv")


train8ore <- ds_sca8[731:1095,"TSTOT.ConsumiScandi"]
test8ore <- ds_sca8[1096:1460,"TSTOT.ConsumiScandi"]

ds_sca_lav<-ds_sca8[731:1460,"TSTOT.ConsumiScandi"]


train8orets <-  ts(coredata(train8ore), freq = 365, start = c(2014,1),  end = c(2014,365))
test8orets <-  ts(coredata(test8ore), freq = 365, start = c(2015,1),  end = c(2015,365))
#####################

ds_sca_lavts<-ts(coredata(ds_sca_lav), freq = 365, start = c(2014,1),  end = c(2015,365))

# y <- msts(train8orets, c(7,30*3, 365)) # multiseasonal ts
# ytest<- msts(test8orets, c(7,365)) # multiseasonal ts


lambda2 <- BoxCox.lambda(ds_sca_lavts, method = c("guerrero"))


dsboxcox <-  BoxCox(ds_sca_lavts, lambda2)

x7 <- fourier(ds_sca_lavts, K=8)


fit10 <- auto.arima(dsboxcox, seasonal=T, xreg =x7, lambda =lambda2,  trace = T)

summary(fit10)

##############################

fit10 %>%
  forecast(xreg=fourier(dsboxcox, K=8, h=7)) -> forecastboxcox







plot(test8orets, col=2) + lines(forecastboxcox$mean, col=4)

plot(forecastboxcox, col=4)+ lines(test8orets, col=2)
predts <- ts(coredata(pred), freq = 365, start = c(2015,1),  end = c(2015,365))
# predts <- ts(coredata(pred[1:7]), freq = 7, start = c(2015,1),  end = c(2015,7))
accmeasures1=regr.eval(test8orets,forecastboxcox$mean)

###################################


# pred <- InvBoxCox(forecastboxcox$mean,lambda2)
# predts <- ts(coredata(pred[1:7]), freq = 7, start = c(2015,1),  end = c(2015,7))

??regr.eval
library(DMwR)

#####################################################
trainplot<-ds_ita8[731:1095,"TSTOT.ConsumiITA"]
predplot <- InvBoxCox(forecastboxcox$mean,lambda)
predplot<- ts(coredata(predplot), freq = 365, start = c(2015,1),  end = c(2015,365))
test8orets<-ds_ita8[1096:1460,"TSTOT.ConsumiITA"]



trainplot <- as.data.frame(trainplot)
predplot <- as.data.frame(predplot)
test8orets <- as.data.frame(test8orets)


values = seq(from = as.Date("2014-01-01"), to = as.Date("2014-12-31"), by = 'day')
trainplot2 <- cbind(trainplot,values)

values = seq(from = as.Date("2015-01-01"), to = as.Date("2015-12-31"), by = 'day')
test8oretsplot <- cbind(test8orets,values)


predplot<-cbind(predplot, values)



write.csv(predplot , "C:\\Users\\vizzi\\PROG_DSLAB_GITHUB\\Progetto_DSLAB\\DATASET SERIE STORICHE\\predplot_ale.csv")
write.csv(trainplot2 , "C:\\Users\\vizzi\\PROG_DSLAB_GITHUB\\Progetto_DSLAB\\DATASET SERIE STORICHE\\trainplot2_ale.csv")
write.csv(test8oretsplot , "C:\\Users\\vizzi\\PROG_DSLAB_GITHUB\\Progetto_DSLAB\\DATASET SERIE STORICHE\\test8oretsplot_ale.csv")









###########################
ds_sca8 <- read.csv("C:\\Users\\vizzi\\PROG_DSLAB_GITHUB\\Progetto_DSLAB\\DATASET SERIE STORICHE\\scandinavia8mattina.csv")


train8ore <- ds_sca8[731:1095,"TSTOT.ConsumiScandi"]
test8ore <- ds_sca8[1096:1460,"TSTOT.ConsumiScandi"]



train8orets <-  ts(coredata(train8ore), freq = 365, start = c(2014,1),  end = c(2014,365))
test8orets <-  ts(coredata(test8ore), freq = 365, start = c(2015,1),  end = c(2015,365))
#####################


lambda2 <- BoxCox.lambda(train8orets, method = c("guerrero"))



dsboxcox <-  BoxCox(train8orets, lambda2)

x7 <- fourier(dsboxcox, K=8)


fit10 <- auto.arima(dsboxcox, seasonal=T, xreg =x7, lambda = lambda2, trace = T)

summary(fit10)

##############################

fit10 %>%
  forecast(xreg=fourier(dsboxcox, K=8, h=365)) -> forecastboxcox

pred <- InvBoxCox(forecastboxcox$mean,lambda2)

plot(test8orets, col=2) + lines(pred, col=4)

plot(pred, col=4)+ lines(test8orets, col=2)

autoplot(forecastboxcox) 
predts <- ts(coredata(pred), freq = 365, start = c(2015,1),  end = c(2015,365))
# predts <- ts(coredata(pred[1:7]), freq = 7, start = c(2015,1),  end = c(2015,7))
accmeasures1=regr.eval(test8orets,forecastboxcox$mean)


#############################################

#####################################################
trainplot<-ds_ita8[731:1095,"TSTOT.ConsumiITA"]
predplot <- InvBoxCox(forecastboxcox$mean,lambda)
predplot<- ts(coredata(predplot), freq = 365, start = c(2015,1),  end = c(2015,365))
test8orets<-ds_ita8[1096:1460,"TSTOT.ConsumiITA"]



trainplot <- as.data.frame(trainplot)
predplot <- as.data.frame(predplot)
test8orets <- as.data.frame(test8orets)


values = seq(from = as.Date("2014-01-01"), to = as.Date("2014-12-31"), by = 'day')
trainplot2 <- cbind(trainplot,values)

values = seq(from = as.Date("2015-01-01"), to = as.Date("2015-12-31"), by = 'day')
test8oretsplot <- cbind(test8orets,values)


predplot<-cbind(predplot, values)



write.csv(predplot , "C:\\Users\\vizzi\\PROG_DSLAB_GITHUB\\Progetto_DSLAB\\DATASET SERIE STORICHE\\predplot_ale.csv")
write.csv(trainplot2 , "C:\\Users\\vizzi\\PROG_DSLAB_GITHUB\\Progetto_DSLAB\\DATASET SERIE STORICHE\\trainplot2_ale.csv")
write.csv(test8oretsplot , "C:\\Users\\vizzi\\PROG_DSLAB_GITHUB\\Progetto_DSLAB\\DATASET SERIE STORICHE\\test8oretsplot_ale.csv")
################
trainplot <- as.data.frame(train8orets)
predplot <- as.data.frame(forecastboxcox)
test8orets <- as.data.frame(test8orets)

values = seq(from = as.Date("2014-01-01"), to = as.Date("2014-12-31"), by = 'day')
trainplot2 <- cbind(trainplot,values)


values = seq(from = as.Date("2015-01-01"), to = as.Date("2015-12-31"), by = 'day')
test8oretsplot <- cbind(test8orets,values)


predplot<-cbind(predplot, values)



write.csv(predplot , "C:\\Users\\vizzi\\PROG_DSLAB_GITHUB\\Progetto_DSLAB\\DATASET SERIE STORICHE\\pome_predplot.csv")
write.csv(trainplot2 , "C:\\Users\\vizzi\\PROG_DSLAB_GITHUB\\Progetto_DSLAB\\DATASET SERIE STORICHE\\pome_trainplot2_ale.csv")
write.csv(test8oretsplot , "C:\\Users\\vizzi\\PROG_DSLAB_GITHUB\\Progetto_DSLAB\\DATASET SERIE STORICHE\\pome_test8oretsplot_ale.csv")
