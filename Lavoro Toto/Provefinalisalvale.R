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