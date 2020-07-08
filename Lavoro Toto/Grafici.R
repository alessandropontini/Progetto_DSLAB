
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

# 2043

library(zoo)
library(forecast)
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
  forecast(xreg=fourier(dsboxcox, K=16, h=365)) -> forecastboxcox


pred <- InvBoxCox(forecastboxcox$mean,lambda)
predts <- ts(coredata(pred[1:7]), freq = 7, start = c(2015,1),  end = c(2015,7))

??regr.eval
library(DMwR)

plot(test8orets, col=2) + lines(pred, col=4)
predts <- ts(coredata(pred), freq = 365, start = c(2015,1),  end = c(2015,365))
# predts <- ts(coredata(pred[1:7]), freq = 7, start = c(2015,1),  end = c(2015,7))
accmeasures1=regr.eval(test8orets,predts)

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

# plot(test8orets, col=2) + lines(pred, col=4)

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


##################################################################


################### Scandinavia
############################################################################################
######################################
###############

library(zoo)
library(forecast)
ds_sca8 <- read.csv("C:\\Users\\vizzi\\PROG_DSLAB_GITHUB\\Progetto_DSLAB\\DATASET SERIE STORICHE\\scandinavia8mattina.csv")


train8ore <- ds_sca8[1:1095,"TSTOT.ConsumiScandi"]
test8ore <- ds_sca8[1096:1460,"TSTOT.ConsumiScandi"]



train8orets <-  ts(coredata(train8ore), freq = 365, start = c(2012,1),  end = c(2014,365))
test8orets <-  ts(coredata(test8ore), freq = 365, start = c(2015,1),  end = c(2015,365))

y <- msts(train8orets, c(7,30*3, 365)) # multiseasonal ts
# ytest<- msts(test8orets, c(7,365)) # multiseasonal ts


lambda2 <- BoxCox.lambda(y, method = ("loglik"))



dsboxcox <-  BoxCox(y, lambda2)

x7 <- fourier(y, K=c(1,1,1))


fit10 <- auto.arima(dsboxcox, seasonal=F, xreg =x7, lambda =lambda2,  trace = T)

summary(fit10)
#########################

fit10 %>%
  forecast(xreg=fourier(y, K=c(1,1,1), h=365)) -> forecastboxcox
# library(DMwR)

# autoplot(train8orets)
#autoplot(dsboxcox)

pred <- InvBoxCox(forecastboxcox$mean,lambda2)

# xtest <- msts(pred, c(7,365)) # multiseasonal ts

# autoplot(xtest)
# autoplot(pred)
# autoplot(ytest)


pred3 <-  ts(coredata(pred), freq = 365, start = c(2015,1),  end = c(2015,365))
plot(test8orets, col=2) + lines(pred3, col=4)

# xtest2<-as.ts(xtest)



predts <- ts(coredata(pred), freq = 365, start = c(2015,1),  end = c(2015,365))
accmeasures1=regr.eval(test8orets,pred3)

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

# plot(test8orets, col=2) + lines(pred, col=4)



################### Scandinavia22
############################################################################################
######################################
###############

library(zoo)
library(forecast)
ds_sca8 <- read.csv("C:\\Users\\vizzi\\PROG_DSLAB_GITHUB\\Progetto_DSLAB\\DATASET SERIE STORICHE\\scandinavia8mattina.csv")


train8ore <- ds_sca8[1:1095,"TSTOT.ConsumiScandi"]
test8ore <- ds_sca8[1096:1460,"TSTOT.ConsumiScandi"]



train8orets <-  ts(coredata(train8ore), freq = 365, start = c(2012,1),  end = c(2014,365))
test8orets <-  ts(coredata(test8ore), freq = 365, start = c(2015,1),  end = c(2015,365))

# y <- msts(train8orets, c(7,30*3, 365)) # multiseasonal ts
# ytest<- msts(test8orets, c(7,365)) # multiseasonal ts


lambda2 <- BoxCox.lambda(train8orets, method = ("loglik"))



dsboxcox <-  BoxCox(train8orets, lambda2)

x7 <- fourier(train8orets, K=3)


fit10 <- auto.arima(dsboxcox, seasonal=F, xreg =x7, lambda =lambda2,  trace = T)

summary(fit10)
#########################

fit10 %>%
  forecast(xreg=fourier(dsboxcox, K=3, h=365)) -> forecastboxcox
# library(DMwR)

# autoplot(train8orets)
#autoplot(dsboxcox)

pred <- InvBoxCox(forecastboxcox$mean,lambda2)

# xtest <- msts(pred, c(7,365)) # multiseasonal ts

# autoplot(xtest)
# autoplot(pred)
# autoplot(ytest)


# pred3 <-  ts(coredata(pred), freq = 365, start = c(2015,1),  end = c(2015,365))
plot(test8orets, col=2) + lines(pred, col=4)

# xtest2<-as.ts(xtest)



predts <- ts(coredata(pred), freq = 365, start = c(2015,1),  end = c(2015,365))
accmeasures1=regr.eval(test8orets,pred3)

################### Scandinavia3
############################################################################################
######################################
###############

library(zoo)
library(forecast)
ds_sca8 <- read.csv("C:\\Users\\vizzi\\PROG_DSLAB_GITHUB\\Progetto_DSLAB\\DATASET SERIE STORICHE\\scandinavia8mattina.csv")


train8ore <- ds_sca8[1:1453,"TSTOT.ConsumiScandi"]
test8ore <- ds_sca8[1454:1460,"TSTOT.ConsumiScandi"]



train8orets <-  ts(coredata(train8ore), freq = 7)
test8orets <-  ts(coredata(test8ore), freq = 7)

ytrain <- msts(train8orets, c(7,30*3, 365)) # multiseasonal ts
# ytest<- msts(test8orets, c(7,365)) # multiseasonal ts

lambda2 <- BoxCox.lambda(train8orets, method = ("loglik"))



dsboxcox <-  BoxCox(train8orets, lambda2)

x7 <- fourier(train8orets, K=2)


fit10 <- auto.arima(dsboxcox, seasonal=F, xreg =x7, lambda =lambda2,  trace = T)


summary(fit10)
#########################

fit10 %>%
  forecast(xreg=fourier(dsboxcox, K=2, h=7)) -> forecastboxcox
# library(DMwR)

# autoplot(train8orets)
#autoplot(dsboxcox)

pred <- InvBoxCox(forecastboxcox$mean,lambda2)

# xtest <- msts(pred, c(7,365)) # multiseasonal ts

# autoplot(xtest)
# autoplot(pred)
# autoplot(ytest)


pred3 <-  ts(coredata(pred), freq = 7)
plot(test8orets, col=2) + lines(pred3, col=4)

# xtest2<-as.ts(xtest)



# predts <- ts(coredata(pred), freq = 365, start = c(2015,1),  end = c(2015,365))
accmeasures1=regr.eval(test8orets,pred3)


################### Scandinaviam Prove 12
############################################################################################
######################################
###############

library(zoo)
library(forecast)
ds_sca8 <- read.csv("C:\\Users\\vizzi\\PROG_DSLAB_GITHUB\\Progetto_DSLAB\\DATASET SERIE STORICHE\\scandinavia8mattina.csv")


train8ore <- ds_sca8[1:1453,"TSTOT.ConsumiScandi"]
test8ore <- ds_sca8[1454:1460,"TSTOT.ConsumiScandi"]



train8orets <-  ts(coredata(train8ore), freq = 7)
test8orets <-  ts(coredata(test8ore), freq = 7)

ytrain <- msts(train8orets, c(7,30*3, 365)) # multiseasonal ts
# ytest<- msts(test8orets, c(7,365)) # multiseasonal ts

lambda2 <- BoxCox.lambda(train8orets, method = ("loglik"))



dsboxcox <-  BoxCox(train8orets, lambda2)

x7 <- fourier(train8orets, K=2)


fit10 <- auto.arima(dsboxcox, seasonal=F, xreg =x7, lambda =lambda2,  trace = T)


summary(fit10)
#########################

fit10 %>%
  forecast(xreg=fourier(dsboxcox, K=2, h=7)) -> forecastboxcox
# library(DMwR)

# autoplot(train8orets)
#autoplot(dsboxcox)

pred <- InvBoxCox(forecastboxcox$mean,lambda2)

# xtest <- msts(pred, c(7,365)) # multiseasonal ts

# autoplot(xtest)
# autoplot(pred)
# autoplot(ytest)


pred3 <-  ts(coredata(pred), freq = 7)
plot(test8orets, col=2) + lines(pred3, col=4)

# xtest2<-as.ts(xtest)



# predts <- ts(coredata(pred), freq = 365, start = c(2015,1),  end = c(2015,365))
accmeasures1=regr.eval(test8orets,pred3)




########################################


library(zoo)
library(forecast)
ds_sca8 <- read.csv("C:\\Users\\vizzi\\PROG_DSLAB_GITHUB\\Progetto_DSLAB\\DATASET SERIE STORICHE\\scandinavia8mattina.csv")


train8ore <- ds_sca8[1:1453,"TSTOT.ConsumiScandi"]
test8ore <- ds_sca8[1454:1460,"TSTOT.ConsumiScandi"]



train8orets <-  ts(coredata(train8ore), freq = 365)
test8orets <-  ts(coredata(test8ore), freq = 7)



fit6 <- 
  model(
    `K = 1` = ARIMA(train8orets ~ fourier(K = 1) + PDQ(0,0,0)),
    `K = 2` = ARIMA(train8orets ~ fourier(K = 2) + PDQ(0,0,0)),
    `K = 3` = ARIMA(train8orets ~ fourier(K = 3) + PDQ(0,0,0)),
    `K = 4` = ARIMA(train8orets ~ fourier(K = 4) + PDQ(0,0,0)),
    `K = 5` = ARIMA(train8orets ~ fourier(K = 5) + PDQ(0,0,0)),
    `K = 6` = ARIMA(train8orets ~ fourier(K = 6) + PDQ(0,0,0))
  )

train8orets


Arima(y=train8orets, xreg = fourier(train8orets, K = 6), order = c(5,1,2))

fit %>%
  forecast(h = "2 years") %>%
  autoplot(aus_cafe) +
  facet_wrap(vars(.model), ncol = 2) +
  guides(colour = FALSE) +
  geom_label(
    aes(x = yearmonth("2007 Jan"), y = 4250, label = paste0("AICc = ", format(AICc))),
    data = glance(fit)
  )




################### Scandinaviam Prove 12-34
############################################################################################
######################################
###############

library(zoo)
library(forecast)
ds_sca8 <- read.csv("C:\\Users\\vizzi\\PROG_DSLAB_GITHUB\\Progetto_DSLAB\\DATASET SERIE STORICHE\\scandinavia8mattina.csv")


train8ore <- ds_sca8[1:1095,"TSTOT.ConsumiScandi"]
test8ore <- ds_sca8[1096:1460,"TSTOT.ConsumiScandi"]



train8orets <-  ts(coredata(train8ore), freq = 365, start=c(2012,1), end=c(2014,12))
test8orets <-  ts(coredata(test8ore), freq = 365, start=c(2015,1), end=c(2015,12))

# ytrain <- msts(train8orets, c(7,30*3, 365)) # multiseasonal ts
# ytest<- msts(test8orets, c(7,365)) # multiseasonal ts

# lambda2 <- BoxCox.lambda(train8orets, method = ("loglik"))



# dsboxcox <-  BoxCox(train8orets, lambda2)

# test8orets <- diff(test8orets, lag=7)
train8orets_diff<-diff(train8orets, lag =365 )
x7 <- fourier(train8orets_diff, K=3)

# t<-decompose(train8orets, "additive")


# giusta<-train8orets-t$seasonal

# autoplot(giusta)
##############################

# test8orets <- diff(test8orets, lag=7)
train8orets_diff<-diff(train8orets, lag =365 )
x7 <- fourier(train8orets_diff, K=3)

t<-decompose(train8orets, "additive")


giusta<-train8orets/t$seasonal

autoplot(t)




##################################
# yyyy<- diffinv(train8orets_diff,  )

v <- cumsum(x)

autoplot(yyyy)

fit10 <- auto.arima(train8orets_diff, seasonal=F, xreg =x7,  trace = T)


summary(fit10)
#########################

fit10 %>%
  forecast(xreg=fourier(train8orets_diff, K=3, h=358)) -> forecastboxcox
# library(DMwR)

# autoplot(train8orets)
#autoplot(dsboxcox)

# pred <- InvBoxCox(forecastboxcox$mean,lambda2)

# xtest <- msts(pred, c(7,365)) # multiseasonal ts

# autoplot(xtest)
# autoplot(pred)
# autoplot(ytest)
pred <- diffinv(forecastboxcox$mean,lag= 7 )

pred<-cumsum(diff(forecastboxcox$mean))

pred3 <-  ts(coredata(pred), freq = 7)


plot(test8orets, col=2) + lines(pred, col=4)

# xtest2<-as.ts(xtest)



# predts <- ts(coredata(pred), freq = 365, start = c(2015,1),  end = c(2015,365))
accmeasures1=regr.eval(test8orets,pred3)
