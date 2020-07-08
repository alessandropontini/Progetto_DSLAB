##############

ds_ita8 <- read.csv("C:\\Users\\vizzi\\PROG_DSLAB_GITHUB\\Progetto_DSLAB\\DATASET SERIE STORICHE\\italia8mattina.csv")


train8ore <- ds_ita8[731:1095,"TSTOT.ConsumiITA"]
test8ore <- ds_ita8[1096:1460,"TSTOT.ConsumiITA"]



train8orets <-  ts(coredata(train8ore), freq = 365, start = c(2014,1),  end = c(2014,365))
test8orets <-  ts(coredata(test8ore), freq = 365, start = c(2015,1),  end = c(2015,365))
#####################


lambda2 <- BoxCox.lambda(train8orets, method = c("guerrero"))



dsboxcox <-  BoxCox(train8orets, lambda2)

x7 <- fourier(train8orets, K=16)


fit10 <- auto.arima(dsboxcox, seasonal=T, xreg =x7, lambda = lambda2, trace = T)

summary(fit10)

##############################

fit10 %>%
  forecast(xreg=fourier(dsboxcox, K=16, h=365)) -> forecastboxcox

pred <- InvBoxCox(forecastboxcox$mean,lambda2)

plot(test8orets, col=2) + lines(pred, col=4)

plot(forecastboxcox, col=4)+ lines(test8orets, col=2)

autoplot(forecastboxcox) 
predts <- ts(coredata(pred), freq = 365, start = c(2015,1),  end = c(2015,365))
# predts <- ts(coredata(pred[1:7]), freq = 7, start = c(2015,1),  end = c(2015,7))
accmeasures1=regr.eval(test8orets,forecastboxcox$mean)


################
trainplot <- as.data.frame(train8orets)
predplot <- as.data.frame(forecastboxcox)
test8orets <- as.data.frame(test8orets)

values = seq(from = as.Date("2014-01-01"), to = as.Date("2014-12-31"), by = 'day')
trainplot2 <- cbind(trainplot,values)


values = seq(from = as.Date("2015-01-01"), to = as.Date("2015-12-31"), by = 'day')
test8oretsplot <- cbind(test8orets,values)


predplot<-cbind(predplot, values)



write.csv(predplot , "C:\\Users\\vizzi\\PROG_DSLAB_GITHUB\\Progetto_DSLAB\\DATASET SERIE STORICHE\\pomeITA_predplot.csv")
write.csv(trainplot2 , "C:\\Users\\vizzi\\PROG_DSLAB_GITHUB\\Progetto_DSLAB\\DATASET SERIE STORICHE\\pomeITA_trainplot2_ale.csv")
write.csv(test8oretsplot , "C:\\Users\\vizzi\\PROG_DSLAB_GITHUB\\Progetto_DSLAB\\DATASET SERIE STORICHE\\pomeITA_test8oretsplot_ale.csv")
###############################

# last 


ds_ita8 <- read.csv("C:\\Users\\vizzi\\PROG_DSLAB_GITHUB\\Progetto_DSLAB\\DATASET SERIE STORICHE\\italia8mattina.csv")

ds_ita8_diff<-diff(ds_ita8[,"TSTOT.ConsumiITA"], lag = 7)



##############
ds_ita8 <- read.csv("C:\\Users\\vizzi\\PROG_DSLAB_GITHUB\\Progetto_DSLAB\\DATASET SERIE STORICHE\\italia8mattina.csv")

testzero<- ds_ita8[1096:1460,"TSTOT.ConsumiITA"]


testzerots <-  ts(coredata(testzero), freq = 365, start = c(2015,1),  end = c(2015,365))
  
  
tail(ds_ita8_diff)
anyNA(ds_ita8_diff)
train8ore <- ds_ita8_diff[731:1095]
test8ore <- ds_ita8_diff[1096:1460]

autoplot(ds_ita8_diff)



train8orets <-  ts(coredata(train8ore), freq = 365, start = c(2014,1),  end = c(2014,365))
test8orets <-  ts(coredata(test8ore), freq = 365, start = c(2015,1),  end = c(2015,365))
#####################

autoplot(test8orets)

lambda2 <- BoxCox.lambda(train8orets, method = c("guerrero"))



dsboxcox <-  BoxCox(train8orets, lambda2)

x7 <- fourier(train8orets, K=16)


fit10 <- auto.arima(dsboxcox, seasonal=T, xreg =x7, lambda = lambda2, trace = T)

summary(fit10)

##############################

fit10 %>%
  forecast(xreg=fourier(dsboxcox, K=16, h=365)) -> forecastboxcox


##############################
forecast8<-testzerots[8:365]
forcast1358<-forecastboxcox$mean[1:358]



final<-gino+gino2

plot(final, col=2) + lines(testzero, col=4)


##################

pred <- InvBoxCox(forecastboxcox$mean,lambda2)

plot(test8orets, col=2) + lines(pred, col=4)

plot(forecastboxcox, col=4)+ lines(test8orets, col=2)

autoplot(forecastboxcox) 
predts <- ts(coredata(pred), freq = 365, start = c(2015,1),  end = c(2015,365))
# predts <- ts(coredata(pred[1:7]), freq = 7, start = c(2015,1),  end = c(2015,7))
accmeasures1=regr.eval(testzero[1:358],final)


################
trainplot <- as.data.frame(train8orets)
predplot <- as.data.frame(forecastboxcox)
test8orets <- as.data.frame(test8orets)

values = seq(from = as.Date("2014-01-01"), to = as.Date("2014-12-31"), by = 'day')
trainplot2 <- cbind(trainplot,values)


values = seq(from = as.Date("2015-01-01"), to = as.Date("2015-12-31"), by = 'day')
test8oretsplot <- cbind(test8orets,values)


predplot<-cbind(predplot, values)



write.csv(predplot , "C:\\Users\\vizzi\\PROG_DSLAB_GITHUB\\Progetto_DSLAB\\DATASET SERIE STORICHE\\pomeITA_predplot.csv")
write.csv(trainplot2 , "C:\\Users\\vizzi\\PROG_DSLAB_GITHUB\\Progetto_DSLAB\\DATASET SERIE STORICHE\\pomeITA_trainplot2_ale.csv")
write.csv(test8oretsplot , "C:\\Users\\vizzi\\PROG_DSLAB_GITHUB\\Progetto_DSLAB\\DATASET SERIE STORICHE\\pomeITA_test8oretsplot_ale.csv")
###############################


#######################

ds_tot <- read.csv("C:\\Users\\vizzi\\PROG_DSLAB_GITHUB\\Progetto_DSLAB\\DATASET SERIE STORICHE\\SerieStorica3NazioniDummy.csv")

ds_tot$X <-NULL
ds_tot$PrezzoScandi <-NULL
ds_tot = subset(ds_tot, select = -c(3:6) )
