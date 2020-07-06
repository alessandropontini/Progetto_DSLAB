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
ds_ita8<-  ts(coredata(ds_ita8[,"TSTOT.ConsumiITA"]), freq = 365, start = c(2012,1,1),  end = c(2015,365))

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



