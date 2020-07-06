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

# ds_sca8
ds_sca8<-  ts(coredata(ds_sca8[,"TSTOT.ConsumiScandi"]), freq = 365, start = c(2012,1,1),  end = c(2015,365))

xsca <- fourier(ds_sca8, K=4)

fitsca <- auto.arima(ds_sca8, seasonal=FALSE, lambda=0, xreg=xsca, trace = T)

autoplot(fitsca)

summary(fitsca)

# FIT Scandinavia K=4, Seasonal False
# Series: ds_sca8 
# Regression with ARIMA(5,1,2) errors 
# Box Cox transformation: lambda= 0 

# Coefficients:
#   ar1      ar2      ar3      ar4      ar5      ma1     ma2  S1-365  C1-365  S2-365   C2-365  S3-365   C3-365  S4-365
# 0.1261  -0.7501  -0.2703  -0.3588  -0.5766  -0.6562  0.5756  0.0797  0.1749  0.0194  -0.0182  0.0181  -0.0176  0.0025
# s.e.  0.0353   0.0217   0.0314   0.0206   0.0283   0.0465  0.0274  0.0408  0.0406  0.0204   0.0203  0.0136   0.0136  0.0102
# C4-365
# 0.0061
# s.e.  0.0102

# sigma^2 estimated as 0.003414:  log likelihood=2078.31
# AIC=-4124.62   AICc=-4124.25   BIC=-4040.05

#Training set error measures:
#   ME     RMSE      MAE        MPE     MAPE      MASE        ACF1
# Training set 43.2377 2133.406 1582.484 -0.1564989 4.401035 0.4365747 -0.00245258


fitsca %>%
  forecast(xreg=fourier(ds_sca8, K=4, h=7)) %>%
  autoplot(include=365)

png("plotscandinaviarima_h8_SeasonalFalse_365g.png",4032,3024, units="px", res=500)
fitsca %>%
  forecast(xreg=fourier(ds_sca8, K=4, h=7)) %>%
  autoplot(include=365) + xlab("Anni") + ylab("Consumi in KW") +
  ggtitle("Previsione consumi Scandinavia con ARIMA(5,1,2)")

dev.off()



# fitsca %>%
#   forecast(xreg=fourier(ds_sca8, K=16, h=7)) ->fit10

fit10
#################
#### K4

ds_sca8 <- read.csv("C:\\Users\\vizzi\\PROG_DSLAB_GITHUB\\Progetto_DSLAB\\DATASET SERIE STORICHE\\scandinavia8mattina.csv")

# ds_sca8
ds_sca8<-  ts(coredata(ds_sca8[,"TSTOT.ConsumiScandi"]), freq = 365, start = c(2012,1,1),  end = c(2015,365))

xsca <- fourier(ds_sca8, K=4)

fitsca <- auto.arima(ds_sca8, seasonal=TRUE, lambda=0, xreg=xsca, trace = T)

autoplot(fitsca)

summary(fitsca)


# Fit Arima summary Seasonal = TRUE H8 K=4

# Series: ds_sca8 
# Regression with ARIMA(5,1,2) errors 
# Box Cox transformation: lambda= 0 

# Coefficients:
#   ar1      ar2      ar3      ar4      ar5      ma1     ma2  S1-365  C1-365  S2-365   C2-365  S3-365   C3-365  S4-365
# 0.1261  -0.7501  -0.2703  -0.3588  -0.5766  -0.6562  0.5756  0.0797  0.1749  0.0194  -0.0182  0.0181  -0.0176  0.0025
# s.e.  0.0353   0.0217   0.0314   0.0206   0.0283   0.0465  0.0274  0.0408  0.0406  0.0204   0.0203  0.0136   0.0136  0.0102
# C4-365
# 0.0061
# s.e.  0.0102

# sigma^2 estimated as 0.003414:  log likelihood=2078.31
# AIC=-4124.62   AICc=-4124.25   BIC=-4040.05

# Training set error measures:
#   ME     RMSE      MAE        MPE     MAPE      MASE        ACF1
# Training set 43.2377 2133.406 1582.484 -0.1564989 4.401035 0.4365747 -0.00245258


fitsca %>%
  forecast(xreg=fourier(ds_sca8, K=4, h=7)) %>%
  autoplot(include=31)

png("plotscandinaviarima_h8_SeasonalTRUE_365g.png",4032,3024, units="px", res=500)
fitsca %>%
  forecast(xreg=fourier(ds_sca8, K=4, h=7)) %>%
  autoplot(include=365) + xlab("Anni") + ylab("Consumi in KW") +
  ggtitle("Previsione consumi Scandinavia con ARIMA(5,1,2)")

dev.off()

#fitsca %>%
 #  forecast(xreg=fourier(ds_sca8, K=16, h=7)) ->fit10

##################################################################

ds_sca12 <- read.csv("C:\\Users\\vizzi\\PROG_DSLAB_GITHUB\\Progetto_DSLAB\\DATASET SERIE STORICHE\\scandinavia12mattina.csv")

# ds_sca8
ds_sca12 <-  ts(coredata(ds_sca12[,"TSTOT.ConsumiScandi"]), freq = 365, start = c(2012,1,1),  end = c(2015,365))

xsca <- fourier(ds_sca12, K=4)

fitsca <- auto.arima(ds_sca12, seasonal=FALSE, lambda=0, xreg=xsca, trace = T)

autoplot(fitsca)

summary(fitsca)

# Fit H12 Seasonal False 

# Series: ds_sca12 
# Regression with ARIMA(5,1,1) errors 
# Box Cox transformation: lambda= 0 

# Coefficients:
#   ar1      ar2      ar3      ar4      ar5      ma1  S1-365  C1-365  S2-365  C2-365  S3-365   C3-365   S4-365   C4-365
# -0.0905  -0.5530  -0.3446  -0.3179  -0.5857  -0.4533  0.0525  0.1682  0.0104  0.0034  0.0096  -0.0172  -0.0021  -0.0035
# s.e.   0.0248   0.0199   0.0242   0.0199   0.0223   0.0219  0.0295  0.0294  0.0148  0.0147  0.0099   0.0099   0.0075   0.0074

# sigma^2 estimated as 0.005261:  log likelihood=1762.85
# AIC=-3495.69   AICc=-3495.36   BIC=-3416.41

# Training set error measures:
#   ME     RMSE      MAE        MPE     MAPE      MASE       ACF1
# Training set 100.5914 3050.145 2366.608 -0.2195918 5.684167 0.5671275 -0.1185437


# library(ggplot2)
fitsca %>%
  forecast(xreg=fourier(ds_sca12, K=4, h=7)) %>%
  autoplot(include=31) + xlab("Anni") + ylab("Consumi in KW") +
  ggtitle("Previsione consumi Scandinavia con ARIMA(5,1,2)")

png("plotscandinaviarima_h12_seasFalse_365g.png",4032,3024, units="px", res=500)
fitsca %>%
  forecast(xreg=fourier(ds_sca12, K=4, h=7)) %>%
  autoplot(include=365) + xlab("Anni") + ylab("Consumi in KW") +
  ggtitle("Previsione consumi Scandinavia con ARIMA(5,1,1)")

dev.off()

#################################################################

##################################################################

ds_sca12 <- read.csv("C:\\Users\\vizzi\\PROG_DSLAB_GITHUB\\Progetto_DSLAB\\DATASET SERIE STORICHE\\scandinavia12mattina.csv")

# ds_sca8
ds_sca12 <-  ts(coredata(ds_sca12[,"TSTOT.ConsumiScandi"]), freq = 365, start = c(2012,1,1),  end = c(2015,365))

xsca <- fourier(ds_sca12, K=4)

fitsca <- auto.arima(ds_sca12, seasonal=TRUE, lambda=0, xreg=xsca, trace = T)

autoplot(fitsca)

summary(fitsca)

# Fit H12 Seasonal True

# Series: ds_sca12 
# Regression with ARIMA(5,1,1) errors 
# Box Cox transformation: lambda= 0 

# Coefficients:
#   ar1      ar2      ar3      ar4      ar5      ma1  S1-365  C1-365  S2-365  C2-365  S3-365   C3-365   S4-365   C4-365
# -0.0905  -0.5530  -0.3446  -0.3179  -0.5857  -0.4533  0.0525  0.1682  0.0104  0.0034  0.0096  -0.0172  -0.0021  -0.0035
# s.e.   0.0248   0.0199   0.0242   0.0199   0.0223   0.0219  0.0295  0.0294  0.0148  0.0147  0.0099   0.0099   0.0075   0.0074

# sigma^2 estimated as 0.005261:  log likelihood=1762.85
# AIC=-3495.69   AICc=-3495.36   BIC=-3416.41

# Training set error measures:
#   ME     RMSE      MAE        MPE     MAPE      MASE       ACF1
# Training set 100.5914 3050.145 2366.608 -0.2195918 5.684167 0.5671275 -0.1185437


library(ggplot2)
fitsca %>%
  forecast(xreg=fourier(ds_sca12, K=4, h=7)) %>%
  autoplot(include=31) + xlab("Anni") + ylab("Consumi in KW") +
  ggtitle("Previsione consumi Scandinavia con ARIMA(5,1,1)")

png("plotscandinaviarima_h12_seasTRue_365g.png",4032,3024, units="px", res=500)
fitsca %>%
  forecast(xreg=fourier(ds_sca12, K=4, h=7)) %>%
  autoplot(include=365) + xlab("Anni") + ylab("Consumi in KW") +
  ggtitle("Previsione consumi Scandinavia con ARIMA(5,1,1)")

dev.off()
##########################################################
library(zoo)
library(forecast)
# H20

ds_sca20 <- read.csv("C:\\Users\\vizzi\\PROG_DSLAB_GITHUB\\Progetto_DSLAB\\DATASET SERIE STORICHE\\scandinavia20sera.csv")

# ds_sca8
ds_sca20 <-  ts(coredata(ds_sca20[,"TSTOT.ConsumiScandi"]), freq = 365, start = c(2012,1,1),  end = c(2015,365))

xsca <- fourier(ds_sca20, K=4)

fitsca <- auto.arima(ds_sca20, seasonal=TRUE, lambda=0, xreg=xsca, trace = T)

autoplot(fitsca)

summary(fitsca)

# Fit H20 Seasonal True

# Series: ds_sca20 
# Regression with ARIMA(5,1,2) errors 
# Box Cox transformation: lambda= 0 

# Coefficients:
#   ar1      ar2      ar3      ar4      ar5      ma1     ma2  S1-365  C1-365  S2-365  C2-365  S3-365   C3-365   S4-365
# 0.1799  -0.7453  -0.2476  -0.3541  -0.5121  -0.6231  0.5505  0.0474  0.2165  0.0152  0.0178  0.0096  -0.0239  -0.0034
# s.e.  0.0475   0.0247   0.0356   0.0219   0.0348   0.0600  0.0328  0.0313  0.0312  0.0157  0.0156  0.0105   0.0104   0.0079
# C4-365
# -0.0048
# s.e.   0.0079

# sigma^2 estimated as 0.00178:  log likelihood=2553.9
# AIC=-5075.79   AICc=-5075.42   BIC=-4991.22

# Training set error measures:
#   ME     RMSE      MAE         MPE     MAPE      MASE       ACF1
# Training set 38.62937 1807.761 1353.791 -0.04723699 3.209378 0.4476327 0.03712667


library(ggplot2)
fitsca %>%
  forecast(xreg=fourier(ds_sca20, K=4, h=7)) %>%
  autoplot(include=31) + xlab("Anni") + ylab("Consumi in KW") +
  ggtitle("Previsione consumi Scandinavia con ARIMA(5,1,2)")

png("plotscandinaviarima_h20_seasTrue_365g.png",4032,3024, units="px", res=500)
fitsca %>%
  forecast(xreg=fourier(ds_sca20, K=4, h=7)) %>%
  autoplot(include=365) + xlab("Anni") + ylab("Consumi in KW") +
  ggtitle("Previsione consumi Scandinavia con ARIMA(5,1,2)")

dev.off()

