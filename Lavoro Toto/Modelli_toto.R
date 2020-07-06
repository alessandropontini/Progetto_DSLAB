ds_ita8 <- read.csv("C:\\Users\\vizzi\\PROG_DSLAB_GITHUB\\Progetto_DSLAB\\DATASET SERIE STORICHE\\italia8mattina.csv")

ds_ita8
ds_ita8<-  ts(coredata(ds_ita8[,"TSTOT.ConsumiITA"]), freq = 365, start = c(2015,1,1),  end = c(2015,365))

# y <- msts(ds_ita8[,"TSTOT.ConsumiITA"], c(7*24,365*24)) # multiseasonal ts

# xi<- ds_ita8[,"TSTOT.ConsumiITA"]
#############
xu<- fourier(y, K=c(4,4))

xu
fit7 <- auto.arima(y, seasonal=F, xreg= xu, trace = T)
summary(fit7)
#####################################
plot(fit2)

fit <- auto.arima(ds_ita8[,"TSTOT.ConsumiITA"], seasonal=F, xreg=x)
summary(fit)







####################################################################
x7 <- fourier(ds_ita8, K=16)

# xf<-ds8[,"TSTOT.ConsumiITA"]

fit9 <- auto.arima(ds_ita8, seasonal=FALSE, lambda=0, xreg=x7, trace = T)

autoplot(fit9)

summary(fit9)


fit9 %>%  forecast(xreg=x7 )%>%
  autoplot()


fit <- auto.arima(calls, seasonal=FALSE, lambda=0,
                  xreg=fourier(calls, K=c(10,10)))
fit %>%
  forecast(xreg=fourier(calls, K=c(10,10), h=2)) %>%
  autoplot(include=5*169) +
  ylab("Call volume") + xlab("Weeks")




####################################################################



####################################################################
x7 <- fourier(ds_ita8, K=16)

# xf<-ds8[,"TSTOT.ConsumiITA"]

fit9 <- auto.arima(ds_ita8, seasonal=FALSE, lambda=0, xreg=x7, trace = T)

autoplot(fit9)

summary(fit9)


fit9 %>%  forecast(xreg=x7, h=1 )%>%
  autoplot()
########################################################

fit9 %>%
  forecast(xreg=fourier(ds_ita8, K=16, h=7)) %>%
  autoplot(include=31)

fit9 %>%
  forecast(xreg=fourier(ds_ita8, K=16, h=7)) ->fit10

fit10



#############################################################
############DATASET COMPLETO
###################


ds_ita8completo <- read.csv("C:\\Users\\vizzi\\PROG_DSLAB_GITHUB\\Progetto_DSLAB\\DATASET SERIE STORICHE\\italia8mattina.csv")

ds_ita8
ds_ita8<-  ts(coredata(ds_ita8completo[,"TSTOT.ConsumiITA"]), freq = 365, start = c(2012,1,1),  end = c(2015,365))

x7 <- fourier(ds_ita8, K=16)

# xf<-ds8[,"TSTOT.ConsumiITA"]

fit9 <- auto.arima(ds_ita8, seasonal=FALSE, lambda=0, xreg=x7, trace = T)

autoplot(fit9)

summary(fit9)


fit9 %>%  forecast(xreg=x7, h=1 )%>%
  autoplot()
########################################################

fit9 %>%
  forecast(xreg=fourier(ds_ita8, K=16, h=365)) %>%
  autoplot(include=365)

fit9 %>%
  forecast(xreg=fourier(ds_ita8, K=16, h=7)) ->fit10

fit10




######################################


ds_ita8completo <- read.csv("C:\\Users\\vizzi\\PROG_DSLAB_GITHUB\\Progetto_DSLAB\\DATASET SERIE STORICHE\\italia8mattina.csv")

ds_ita8
ds_ita8<-  ts(coredata(ds_ita8completo[,"TSTOT.ConsumiITA"]), freq = 365, start = c(2012,1,1),  end = c(2015,365))

# x7 <- fourier(ds_ita8, K=16)

# xf<-ds8[,"TSTOT.ConsumiITA"]

fit9 <- auto.arima(ds_ita8, seasonal=T, lambda=0, trace = T)

autoplot(fit9)

summary(fit9)


fit9 %>%  forecast(xreg=x7, h=1 )%>%
  autoplot()
########################################################

fit9 %>%
  forecast(h=365) %>%
  autoplot(include=365)
########################################################################
############################## Dataset Scandinavia
ds_sca8 <- read.csv("C:\\Users\\vizzi\\PROG_DSLAB_GITHUB\\Progetto_DSLAB\\DATASET SERIE STORICHE\\scandinavia8mattina.csv")

ds_sca8
ds_sca8<-  ts(coredata(ds_sca8[,"TSTOT.ConsumiScandi"]), freq = 365, start = c(2012,1,1),  end = c(2015,365))

xsca <- fourier(ds_sca8, K=16)

fitsca <- auto.arima(ds_sca8, seasonal=FALSE, lambda=0, xreg=xsca, trace = T)

autoplot(fitsca)

summary(fitsca)


fitsca %>%
  forecast(xreg=fourier(ds_sca8, K=16, h=7)) %>%
  autoplot(include=31)

fitsca %>%
  forecast(xreg=fourier(ds_sca8, K=16, h=7)) ->fit10

fit10
#################
#### K4

ds_sca8 <- read.csv("C:\\Users\\vizzi\\PROG_DSLAB_GITHUB\\Progetto_DSLAB\\DATASET SERIE STORICHE\\scandinavia8mattina.csv")

ds_sca8
ds_sca8<-  ts(coredata(ds_sca8[,"TSTOT.ConsumiScandi"]), freq = 365, start = c(2012,1,1),  end = c(2015,365))

xsca <- fourier(ds_sca8, K=4)

fitsca <- auto.arima(ds_sca8, seasonal=FALSE, lambda=0, xreg=xsca, trace = T)

autoplot(fitsca)

summary(fitsca)


fitsca %>%
  forecast(xreg=fourier(ds_sca8, K=4, h=7)) %>%
  autoplot(include=31)

#fitsca %>%
 #  forecast(xreg=fourier(ds_sca8, K=16, h=7)) ->fit10

##################################################################

ds_sca8 <- read.csv("C:\\Users\\vizzi\\PROG_DSLAB_GITHUB\\Progetto_DSLAB\\DATASET SERIE STORICHE\\scandinavia8mattina.csv")

ds_sca8
ds_sca8<-  ts(coredata(ds_sca8[,"TSTOT.ConsumiScandi"]), freq = 365, start = c(2012,1,1),  end = c(2015,365))

xsca <- fourier(ds_sca8, K=4)

fitsca <- auto.arima(ds_sca8, seasonal=T, lambda=0, xreg=xsca, trace = T)

autoplot(fitsca)

summary(fitsca)

library(ggplot2)
fitsca %>%
  forecast(xreg=fourier(ds_sca8, K=4, h=7)) %>%
  autoplot(include=31) + xlab("Anni") + ylab("Consumi in KW") +
  ggtitle("Previsione consumi Scandinavia con ARIMA(5,1,2)")

png("plotscandinaviarima.png",4032,3024, units="px", res=500)
fitsca %>%
  forecast(xreg=fourier(ds_sca8, K=4, h=7)) %>%
  autoplot(include=31) + xlab("Anni") + ylab("Consumi in KW") +
  ggtitle("Previsione consumi Scandinavia con ARIMA(5,1,2)")

dev.off()
