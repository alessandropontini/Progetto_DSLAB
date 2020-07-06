# 8 ore
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
  forecast(xreg=fourier(ds_ita8, K=16, h=7)) %>%
  autoplot(include=31) + xlab("Anni") + ylab("Consumi in KW") +
  ggtitle("Previsione consumi Italia con ARIMA(5,1,2)")

dev.off()

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