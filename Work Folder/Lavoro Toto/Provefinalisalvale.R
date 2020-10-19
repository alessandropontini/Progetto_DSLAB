library(zoo)
library(forecast)
library(gtools)

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

summary(fit12)

fit12 %>%
  forecast(xreg=fourier(dsboxcox, K=16, h=7)) -> forecastboxcox

pred <- InvBoxCox(forecastboxcox$mean,lambda2)
predlow <- InvBoxCox(forecastboxcox$lower,lambda2)
predhigh <- InvBoxCox(forecastboxcox$upper,lambda2)

# ggplot
predts <-  ts(pred, freq = 365, start = c(2016,1),  end = c(2016,7))
predtslow <-  ts(predlow, freq = 365, start = c(2016,1),  end = c(2016,7))
predtshigh<-  ts(predhigh, freq = 365, start = c(2016,1),  end = c(2016,7))

intervalliconfidenza <- ts.intersect(predtslow,predtshigh)

dicembre_2015 <-  ts(coredata(ds_ita8[1429:1460]), freq = 365, start = c(2015,335),  end = c(2015,365))

dfdicembre_2015 <- as.data.frame(dicembre_2015)
dfpred <- as.data.frame(predts)
dfci <- as.data.frame(intervalliconfidenza)

values = seq(from = as.Date("2015-12-01"), to = as.Date("2015-12-31"), by = 'day')
dfdicembre_2015 <- cbind(dfdicembre_2015,values)

pred <- read.csv("/Volumes/HDD_Ale/Progetto_DSLAB/DATASET SERIE STORICHE/predts_SCANDIzio.csv")
intervalli <- read.csv("/Volumes/HDD_Ale/Progetto_DSLAB/DATASET SERIE STORICHE/intervalli.csv")
tot <- cbind(pred,intervalli)

values = seq(from = as.Date("2016-01-01"), to = as.Date("2016-01-07"), by = 'day')
tot <- cbind(tot,values)
tot <- tot[,-c(1,3)]
# dfci <- cbind(dfci,values)
# dftot <- cbind(dfpred,dfci)
# dftot <- dftot[-2]

# dftot <-  as.data.frame(dftot)
# datepred <- dftot$values
# datedicembre <- dfdicembre_2015$values
# 
# datepred <- as.data.frame(datepred)
# datedicembre <- as.data.frame(datedicembre)

#names(datedicembre)[1] <- "datepred"

# dftotdate <- smartbind(datedicembre,datepred )
# rownames(dftotdate) <- NULL
tot$values <- as.Date(tot$values)

# ggplot
library(ggplot2)

z <- ggplot(data = dfdicembre_2015, aes(x = values, y = x))+
  geom_line(color = 1, size = 0.7) +
  geom_line(data=dftot,aes(x = values, y = x),color = 2, size = 0.7) +
  geom_ribbon(data=dftot,aes(ymin=`predtslow.80%`,ymax=`predtshigh.80%`),alpha=0.5,fill="#5482F3") +
  geom_ribbon(data=dftot,aes(ymin=`predtslow.95%`,ymax=`predtshigh.95%`),alpha=0.3,fill="#7FA1F7") + 
  geom_line(aes(color="Forecast 7 Giorni")) +
  geom_line(aes(color="Dicembre")) +
  scale_color_manual(name = "", values = c("Forecast 7 Giorni" = "red", "Dicembre"="black")) +
  ylab("Consumi in KW") + xlab("Tempo") + ggtitle("Predizione Arima (5,1,2) H8 K=16") +
  scale_y_continuous(expand = c(0.3,0.3)) +
  scale_x_date(date_labels = "%Y %b %d")+
  theme(panel.grid.major = element_line(colour = "#F4F7FE", size=0.5),
        panel.grid.minor = element_line(colour = "#F4F7FE", size=0.5),
        legend.background=element_rect(fill="#F4F7FE"),
        legend.text = element_text(color = 1, size = 12, face = "bold"),
        axis.text = element_text(face = "bold"),
        axis.title = element_text(size= 15, face = "bold", colour = "#37383C"),
        plot.title = element_text(size = 20, face = "bold"),
        panel.background = element_rect(fill="#EAEBF0"),
        axis.text.x = element_text(angle = 0, size = 10, vjust = 0.5),
        plot.background=element_rect(fill="#F4F7FE")) +
        ggsave("italia8ICarima.jpeg",width = 400, height=250, units="mm", dpi=320)

################################################################################################
######################## DISEGNO TEST TRAIN ####################################################
################################################################################################

train <- read.csv("/Users/alessandropontini/Downloads/trainplot2_ale.csv")
test <- read.csv("/Users/alessandropontini/Downloads/test8oretsplot_ale.csv")
pred <- read.csv("/Users/alessandropontini/Downloads/predplot_ale.csv")

train$values <- as.Date(train$values)
test$values <- as.Date(test$values)
pred$values <- as.Date(pred$values)

p <- ggplot(data = train, aes(x = values, y = trainplot))+
  geom_line(color = "#20232F", size = 0.7) +
  geom_line(data=test,aes(x = values, y = test8orets),color = "#5877F7", size = 0.7) +
  geom_line(data=pred,aes(x = values, y = x),color = "#FACD28", size = 0.7) +
  geom_line(aes(color="Predizione")) +
  geom_line(aes(color="Test")) +
  geom_line(aes(color="Train")) +
  scale_color_manual(name = "", values = c("Predizione" = "#FACD28", "Train"="#20232F", "Test"="#5877F7")) +
  ylab("Consumi in KW") + xlab("Tempo") + ggtitle("Predizione Arima Train/Test Italia H8 K=16") +
  scale_y_continuous(expand = c(0.3,0.3)) +
  scale_x_date(date_labels = "%Y %b %d")+
  theme(panel.grid.major = element_line(colour = "#F4F7FE", size=0.5),
        panel.grid.minor = element_line(colour = "#F4F7FE", size=0.5),
        legend.background=element_rect(fill="#F4F7FE"),
        legend.text = element_text(color = 1, size = 12, face = "bold"),
        axis.text = element_text(face = "bold"),
        axis.title = element_text(size= 15, face = "bold", colour = "#37383C"),
        plot.title = element_text(size = 20, face = "bold"),
        panel.background = element_rect(fill="#EAEBF0"),
        axis.text.x = element_text(angle = 0, size = 10, vjust = 0.5),
        plot.background=element_rect(fill="#F4F7FE")) +
        ggsave("italia8traintestarima.jpeg",width = 400, height=250, units="mm", dpi=320)

test7 <- test[1:7,]
pred7 <- pred[1:7,]
pred7 <- pred7[,-c(1,4)]
c
c <- ggplot(data = pred7, aes(x = values, y = x))+
  geom_line(color = "#FACD28", size = 0.7) +
  geom_line(data=test7,aes(x = values, y = test8orets),color = "#5877F7", size = 0.7) +
  ylab("Consumi in KW") + xlab("Tempo") + ggtitle("Predizione Arima (5,1,2) 7 Giorni") +
  scale_y_continuous(expand = c(0.3,0.3)) +
  scale_x_date(date_labels = "%Y %b %d")+
  theme(panel.grid.major = element_line(colour = "#F4F7FE", size=0.5),
        panel.grid.minor = element_line(colour = "#F4F7FE", size=0.5),
        legend.background=element_rect(fill="#F4F7FE"),
        legend.text = element_text(color = 1, size = 12, face = "bold"),
        axis.text = element_text(face = "bold"),
        axis.title = element_text(size= 15, face = "bold", colour = "#37383C"),
        plot.title = element_text(size = 20, face = "bold"),
        panel.background = element_rect(fill="#EAEBF0"),
        axis.text.x = element_text(angle = 0, size = 10, vjust = 0.5),
        plot.background=element_rect(fill="#F4F7FE")) +
  ggsave("predarima.jpeg",width = 400, height=250, units="mm", dpi=320)


# #EAEBF0
#A1A3A7
###################################################################################################
###################### STEFANO ####################################################################
###################################################################################################

# 
# df_ste = seq(from = as.Date("2012-01-01"), to = as.Date("2012-02-28"), by = 'day')
# df_ste2 = seq(from = as.Date("2012-03-01"), to = as.Date("2015-12-31"), by = 'day')
# 
# df_ste <-  as.data.frame(df_ste)
# df_ste2 <- as.data.frame(df_ste2)
# 
# names(df_ste2)[1] <- "df_ste"
# 
# #install.packages("gtools")
# library(gtools)
# names(dftotste)[1] <- "date"
# 
# dftotste <- smartbind(df_ste,df_ste2)
# dftotste <-  as.data.frame(dftotste)
# rownames(dftotste) <- NULL
# 
# dftotste
# write.csv(dftotste, "/Volumes/HDD_Ale/Progetto_DSLAB/ste_reg_8_12_20/date1490daaggiungere.csv")
# 

#####################################################################
# boxcox quello buono
########################################################################
######################## scandi ########################################

train <- read.csv("/Volumes/HDD_Ale/Progetto_DSLAB/DATASET SERIE STORICHE/pome_trainplot2_ale.csv")
test <- read.csv("/Volumes/HDD_Ale/Progetto_DSLAB/DATASET SERIE STORICHE/pome_test8oretsplot_ale.csv")
pred <- read.csv("/Volumes/HDD_Ale/Progetto_DSLAB/DATASET SERIE STORICHE/pome_predplot.csv")

train$values <- as.Date(train$values)
test$values <- as.Date(test$values)
pred$values <- as.Date(pred$values)

s <- ggplot(data = train, aes(x = values, y = x))+
  geom_line(color = 1, size = 0.7) +
  geom_ribbon(aes(x = pred$values, ymin=pred$Lo.80 , ymax=pred$Hi.80),alpha=0.5,fill="#5482F3") +
  geom_ribbon(aes(x = pred$values, ymin=pred$Lo.95 , ymax=pred$Hi.95),alpha=0.3,fill="#7FA1F7") + 
  geom_line(data=pred,aes(x = values, y = Point.Forecast),color = "#5877F7", size = 0.7) +
  geom_line(data=test,aes(x = values, y = x),color = "#DA2617", size = 0.7, alpha=0.7) +
  geom_line(aes(color="Forecast")) +
  geom_line(aes(color="Test")) +
  geom_line(aes(color="Train")) +
  scale_color_manual(name = "", values = c("Forecast" = "#5877F7", "Test"="#DA2617", "Train"="black")) +
  ylab("Consumi in KW") + xlab("Tempo") + ggtitle("Scandinavia Predizione Arima (2,0,0) H8 K=8") +
  scale_y_continuous(expand = c(0.3,0.3)) +
  scale_x_date(date_labels = "%Y %b %d")+
  theme(panel.grid.major = element_line(colour = "#F4F7FE", size=0.5),
        panel.grid.minor = element_line(colour = "#F4F7FE", size=0.5),
        legend.background=element_rect(fill="#F4F7FE"),
        legend.text = element_text(color = 1, size = 12, face = "bold"),
        axis.text = element_text(face = "bold"),
        axis.title = element_text(size= 15, face = "bold", colour = "#37383C"),
        plot.title = element_text(size = 20, face = "bold"),
        panel.background = element_rect(fill="#EAEBF0"),
        axis.text.x = element_text(angle = 0, size = 10, vjust = 0.5),
        plot.background=element_rect(fill="#F4F7FE")) +
  ggsave("scandinavia.jpeg",width = 400, height=250, units="mm", dpi=320)


dicembre_2015 <- test[347:365,c(2,3)]
rownames(dicembre_2015) <- NULL
dicembre_2015

predizione <- pred[1:7,]
predizione <- predizione[,-c(1)]
tot
z <- ggplot(data = dicembre_2015, aes(x = values, y = x))+
  geom_line(color = 1, size = 0.7) +
  geom_line(data=tot,aes(x = values, y = x),color = "#5877F7", size = 0.7) +
  geom_ribbon(data = tot, aes(ymin=predtslow.80. , ymax=predtshigh.80.),alpha=0.5,fill="#5482F3") +
  geom_ribbon(data=tot,aes(ymin=predtslow.95. , ymax=predtshigh.95.),alpha=0.3,fill="#7FA1F7") + 
  geom_line(aes(color="Forecast")) +
  geom_line(aes(color="Dicembre 2015")) +
  scale_color_manual(name = "", values = c("Forecast" = "#5877F7","Dicembre 2015"="black")) +
  ylab("Consumi in KW") + xlab("Tempo") + ggtitle("Scandinavia Predizione Arima (2,0,0) H8 K=8") +
  scale_y_continuous(expand = c(0.3,0.3)) +
  scale_x_date(date_labels = "%Y %b %d")+
  theme(panel.grid.major = element_line(colour = "#F4F7FE", size=0.5),
        panel.grid.minor = element_line(colour = "#F4F7FE", size=0.5),
        legend.background=element_rect(fill="#F4F7FE"),
        legend.text = element_text(color = 1, size = 12, face = "bold"),
        axis.text = element_text(face = "bold"),
        axis.title = element_text(size= 15, face = "bold", colour = "#37383C"),
        plot.title = element_text(size = 20, face = "bold"),
        panel.background = element_rect(fill="#EAEBF0"),
        axis.text.x = element_text(angle = 0, size = 10, vjust = 0.5),
        plot.background=element_rect(fill="#F4F7FE")) +
  ggsave("scandinavia.jpeg",width = 400, height=250, units="mm", dpi=320)
z
################ ITALIA
ds_ita8 <- read.csv("/Volumes/HDD_Ale/Progetto_DSLAB/DATASET SERIE STORICHE/italia8mattina.csv")

ds_ita8 <-  ts(coredata(ds_ita8[,"TSTOT.ConsumiITA"]), freq = 365, start = c(2012,1),  end = c(2015,365))

lambda <- BoxCox.lambda(ds_ita8, method = "guerrero")

dsboxcox <-  BoxCox(ds_ita8, lambda)

x7 <- fourier(dsboxcox, K=16)

fit10 <- auto.arima(dsboxcox, seasonal=T, xreg=x7, trace = T)

summary(fit10)

z <- ggplot(data = dfdicembre_2015, aes(x = values, y = x))+
  geom_line(color = 1, size = 0.7) +
  geom_line(data=dftot,aes(x = values, y = x),color = 2, size = 0.7) +
  geom_ribbon(data=dftot,aes(ymin=`predtslow.80%`,ymax=`predtshigh.80%`),alpha=0.5,fill="#5482F3") +
  geom_ribbon(data=dftot,aes(ymin=`predtslow.95%`,ymax=`predtshigh.95%`),alpha=0.3,fill="#7FA1F7") + 
  geom_line(aes(color="Forecast 7 Giorni")) +
  geom_line(aes(color="Dicembre")) +
  scale_color_manual(name = "", values = c("Forecast 7 Giorni" = "red", "Dicembre"="black")) +
  ylab("Consumi in KW") + xlab("Tempo") + ggtitle("Predizione Arima (5,1,2) H8 K=16") +
  scale_y_continuous(expand = c(0.3,0.3)) +
  scale_x_date(date_labels = "%Y %b %d")+
  theme(panel.grid.major = element_line(colour = "#F4F7FE", size=0.5),
        panel.grid.minor = element_line(colour = "#F4F7FE", size=0.5),
        legend.background=element_rect(fill="#F4F7FE"),
        legend.text = element_text(color = 1, size = 12, face = "bold"),
        axis.text = element_text(face = "bold"),
        axis.title = element_text(size= 15, face = "bold", colour = "#37383C"),
        plot.title = element_text(size = 20, face = "bold"),
        panel.background = element_rect(fill="#EAEBF0"),
        axis.text.x = element_text(angle = 0, size = 10, vjust = 0.5),
        plot.background=element_rect(fill="#F4F7FE")) +
  ggsave("italia8ICarima.jpeg",width = 400, height=250, units="mm", dpi=320)

################################################################################################
######################## DISEGNO TEST TRAIN ####################################################
################################################################################################

train <- read.csv("/Users/alessandropontini/Downloads/trainplot2_ale.csv")
test <- read.csv("/Users/alessandropontini/Downloads/test8oretsplot_ale.csv")
pred <- read.csv("/Users/alessandropontini/Downloads/predplot_ale.csv")

train$values <- as.Date(train$values)
test$values <- as.Date(test$values)
pred$values <- as.Date(pred$values)

p <- ggplot(data = train, aes(x = values, y = trainplot))+
  geom_line(color = "#20232F", size = 0.7) +
  geom_line(data=test,aes(x = values, y = test8orets),color = "#5877F7", size = 0.7) +
  geom_line(data=pred,aes(x = values, y = x),color = "#FACD28", size = 0.7) +
  geom_line(aes(color="Predizione")) +
  geom_line(aes(color="Test")) +
  geom_line(aes(color="Train")) +
  scale_color_manual(name = "", values = c("Predizione" = "#FACD28", "Train"="#20232F", "Test"="#5877F7")) +
  ylab("Consumi in KW") + xlab("Tempo") + ggtitle("Predizione Arima Train/Test Italia H8 K=16") +
  scale_y_continuous(expand = c(0.3,0.3)) +
  scale_x_date(date_labels = "%Y %b %d")+
  theme(panel.grid.major = element_line(colour = "#F4F7FE", size=0.5),
        panel.grid.minor = element_line(colour = "#F4F7FE", size=0.5),
        legend.background=element_rect(fill="#F4F7FE"),
        legend.text = element_text(color = 1, size = 12, face = "bold"),
        axis.text = element_text(face = "bold"),
        axis.title = element_text(size= 15, face = "bold", colour = "#37383C"),
        plot.title = element_text(size = 20, face = "bold"),
        panel.background = element_rect(fill="#EAEBF0"),
        axis.text.x = element_text(angle = 0, size = 10, vjust = 0.5),
        plot.background=element_rect(fill="#F4F7FE")) +
  ggsave("italia8traintestarima.jpeg",width = 400, height=250, units="mm", dpi=320)



##### ITALIA 

train <- read.csv("/Volumes/HDD_Ale/Progetto_DSLAB/DATASET SERIE STORICHE/pomeITA_trainplot2_ale.csv")
test <- read.csv("/Volumes/HDD_Ale/Progetto_DSLAB/DATASET SERIE STORICHE/pomeITA_test8oretsplot_ale.csv")
pred <- read.csv("/Volumes/HDD_Ale/Progetto_DSLAB/DATASET SERIE STORICHE/pomeITA_predplot.csv")

train$values <- as.Date(train$values)
test$values <- as.Date(test$values)
pred$values <- as.Date(pred$values)

s <- ggplot(data = train, aes(x = values, y = x))+
  geom_line(color = 1, size = 0.7) +
  geom_ribbon(aes(x = pred$values, ymin=pred$Lo.80 , ymax=pred$Hi.80),alpha=0.5,fill="#5482F3") +
  geom_ribbon(aes(x = pred$values, ymin=pred$Lo.95 , ymax=pred$Hi.95),alpha=0.3,fill="#7FA1F7") + 
  geom_line(data=pred,aes(x = values, y = Point.Forecast),color = "#5877F7", size = 0.7) +
  geom_line(data=test,aes(x = values, y = x),color = "#DA2617", size = 0.7, alpha=0.7) +
  geom_line(aes(color="Forecast")) +
  geom_line(aes(color="Test")) +
  geom_line(aes(color="Train")) +
  scale_color_manual(name = "", values = c("Forecast" = "#5877F7", "Test"="#DA2617", "Train"="black")) +
  ylab("Consumi in KW") + xlab("Tempo") + ggtitle("Italia Predizione Arima (1,0,2) H8 K=16") +
  scale_y_continuous(expand = c(0.3,0.3)) +
  scale_x_date(date_labels = "%Y %b %d")+
  theme(panel.grid.major = element_line(colour = "#F4F7FE", size=0.5),
        panel.grid.minor = element_line(colour = "#F4F7FE", size=0.5),
        legend.background=element_rect(fill="#F4F7FE"),
        legend.text = element_text(color = 1, size = 12, face = "bold"),
        axis.text = element_text(face = "bold"),
        axis.title = element_text(size= 15, face = "bold", colour = "#37383C"),
        plot.title = element_text(size = 20, face = "bold"),
        panel.background = element_rect(fill="#EAEBF0"),
        axis.text.x = element_text(angle = 0, size = 10, vjust = 0.5),
        plot.background=element_rect(fill="#F4F7FE")) +
  ggsave("scandinavia.jpeg",width = 400, height=250, units="mm", dpi=320)


ottodimattinaita <- read.csv("/Volumes/HDD_Ale/Progetto_DSLAB/DATASET SERIE STORICHE/italia8mattina.csv")
ottodimattinascandinavia <- read.csv("/Volumes/HDD_Ale/Progetto_DSLAB/DATASET SERIE STORICHE/scandinavia8mattina.csv")

ottodimattinaita <- ottodimattinaita[,-c(1,2,4:16)]
ottodimattinaita <- as.data.frame(ottodimattinaita)

ottodimattinascandinavia <- ottodimattinascandinavia[,-c(1,2,4:17)]
ottodimattinascandinavia <- as.data.frame(ottodimattinascandinavia)

df_ste = seq(from = as.Date("2012-01-01"), to = as.Date("2012-02-28"), by = 'day')
df_ste2 = seq(from = as.Date("2012-03-01"), to = as.Date("2015-12-31"), by = 'day')

df_ste <-  as.data.frame(df_ste)
df_ste2 <- as.data.frame(df_ste2)

names(df_ste2)[1] <- "df_ste"
library(gtools)

dftotste <- smartbind(df_ste,df_ste2)
rownames(dftotste) <- NULL

ottodimattinascandinavia <- cbind(ottodimattinascandinavia,dftotste)
ottodimattinaita <- cbind(ottodimattinaita,dftotste)

ottodimattinaita$df_ste <- as.Date(ottodimattinaita$df_ste)
ottodimattinascandinavia$df_ste <- as.Date(ottodimattinascandinavia$df_ste)

totale <- cbind(ottodimattinaita,ottodimattinascandinavia)
totale <- totale[,-c(4)]


ggplot(totale, aes(y=ottodimattinaita,x=df_ste)) + geom_line()+facet_grid(. ~ ottodimattinaita)

library(dplyr)
econdatalong <- gather(tot, key="measure", value="value", c("ottodimattinaita", "ottodimattinascandinavia"))


ggplot(totale) + 
  geom_line(aes(date,ottodimattinaita)) + 
  facet_wrap(vars(ottodimattinascandinavia,ottodimattinaita))

?facet_wrap

autoplot()
ggplot(data = totale, aes(x = df_ste, y = ottodimattinaita))+
  geom_line(color = 1, size = 0.5) +
  geom_line(data=ottodimattinascandinavia,aes(x = df_ste, y = ottodimattinascandinavia),color = "#5877F7", size = 0.5) +
  facet_grid(ottodimattinaita ~ ottodimattinascandinavia)

economics_long

str(economics_long)
x <- facet_grid(align ~ .)

x

     geom_line(aes(color="Forecast")) +
     geom_line(aes(color="Test")) +
     geom_line(aes(color="Train")) +
  scale_color_manual(name = "", values = c("Forecast" = "#5877F7", "Test"="#DA2617", "Train"="black")) +
  ylab("Consumi in KW") + xlab("Tempo") + ggtitle("Italia Predizione Arima (1,0,2) H8 K=16") +
  scale_y_continuous(expand = c(0.3,0.3)) +
  scale_x_date(date_labels = "%Y %b %d")+
  theme(panel.grid.major = element_line(colour = "#F4F7FE", size=0.5),
        panel.grid.minor = element_line(colour = "#F4F7FE", size=0.5),
        legend.background=element_rect(fill="#F4F7FE"),
        legend.text = element_text(color = 1, size = 12, face = "bold"),
        axis.text = element_text(face = "bold"),
        axis.title = element_text(size= 15, face = "bold", colour = "#37383C"),
        plot.title = element_text(size = 20, face = "bold"),
        panel.background = element_rect(fill="#EAEBF0"),
        axis.text.x = element_text(angle = 0, size = 10, vjust = 0.5),
        plot.background=element_rect(fill="#F4F7FE")) +
  ggsave("scandinavia.jpeg",width = 400, height=250, units="mm", dpi=320)
     
     
     
     
library(vars)
totale <- totale[,-c(2)]
totts <- ts(coredata(totale), freq=365, start = c(2012,1), end = c(2015,365))
colnames(totts)[1] <- "Italia"
colnames(totts)[2] <- "Scandinavia"


pallete = c('red', 'blue', 'green', 'orange')
autoplot(totts, size = 1, colour = 'variable', facets = T) + scale_colour_manual(values=pallete)


library(ggfortify)
pallete <- c("red", "blue")
autoplot(totts, facets = T, col=1) +
  scale_color_manual(labels = c(Italia = "Italia", Scandinavia = "Scandinavia"),col=palette)

  xlab("Anni") + ggtitle("Consumi nei Mercati") +
  theme(panel.grid.major = element_line(colour = "#F4F7FE", size=0.5),
        panel.grid.minor = element_line(colour = "#F4F7FE", size=0.5),
        legend.background=element_rect(fill="#F4F7FE"),
        legend.text = element_text(color = 1, size = 12, face = "bold"),
        axis.text = element_text(face = "bold"),
        axis.title = element_text(size= 15, face = "bold", colour = "#37383C"),
        plot.title = element_text(size = 20, face = "bold"),
        panel.background = element_rect(fill="#EAEBF0"),
        axis.text.x = element_text(angle = 0, size = 10, vjust = 0.5),
        plot.background=element_rect(fill="#F4F7FE")) +
  ggsave("Mercati.jpeg",width = 400, height=250, units="mm", dpi=320)

