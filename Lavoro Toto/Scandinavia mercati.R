# Librerie
library(stringi)
library(readxl)
library(dplyr)
library(tidyr)

library(zoo)
library(ggfortify)
library(xts)

######################################################
# Carico i dati
ABSOLUTE_PATH <- "C:\\Users\\vizzi\\PROG_DSLAB_GITHUB\\Lavoro mio\\toto"

Dummies_frame <- read_excel(paste0(ABSOLUTE_PATH,"\\dummies1215a.xls"))
Price_frame <- read.csv(paste0(ABSOLUTE_PATH,"\\NP_1215_Price_byhour.csv"))
Loads_frame <- read.csv(paste0(ABSOLUTE_PATH,"\\NP_1215_Loads_byhour.csv"))

###### PRE-PROCESSING

df_prezzi <- Price_frame

# SISTEMO DATE PREZZI
df_prezzi$anno <- stri_sub(df_prezzi$DateYYYYMMDD,1,4)
df_prezzi$mese <- stri_sub(df_prezzi$DateYYYYMMDD,5,6)
df_prezzi$giorno <- stri_sub(df_prezzi$DateYYYYMMDD,7,8)

df_prezzi <- df_prezzi %>% gather("ora", "prezzo", H01:H24)
df_prezzi$ora <- stri_sub(df_prezzi$ora,2,3)
df_prezzi <- df_prezzi %>% mutate(ora = paste0(ora,":00:00"))
df_prezzi$date <- as.Date(with(df_prezzi, paste(anno, mese, giorno,sep="-")), "%Y-%m-%d")
df_prezzi <- df_prezzi %>% arrange(date)
df_prezzi$dateTime = as.POSIXct(paste(df_prezzi$date,df_prezzi$ora), format="%Y-%m-%d %H:%M:%S")
df_prezzi$DateYYYYMMDD <- NULL

# df_prezzi$date <- NULL


#####################################################################
df_consumi <- Loads_frame

# SISTEMO DATE CONSUMI
df_consumi$anno <- stri_sub(df_consumi$DateYYYYMMDD,1,4)
df_consumi$mese <- stri_sub(df_consumi$DateYYYYMMDD,5,6)
df_consumi$giorno <- stri_sub(df_consumi$DateYYYYMMDD,7,8)

df_consumi <- df_consumi %>% gather("ora", "consumo", H01:H24)
df_consumi$ora <- stri_sub(df_consumi$ora,2,3)
df_consumi <- df_consumi %>% mutate(ora = paste0(ora,":00:00"))
df_consumi$date <- as.Date(with(df_consumi, paste(anno, mese, giorno,sep="-")), "%Y-%m-%d")
df_consumi <- df_consumi %>% arrange(date)
df_consumi$dateTime = as.POSIXct(paste(df_consumi$date,df_consumi$ora), format="%Y-%m-%d %H:%M:%S")
df_consumi$DateYYYYMMDD <- NULL


# LEFT JOIN

totale <- left_join(df_prezzi, df_consumi, by = c("anno" = "anno", "mese" = "mese", "giorno"="giorno", "ora"="ora"))
totale <- totale[, -c(9:10)]


# CONTROLLO NA E NAN
apply(totale, 2, function(x) any(is.na(x)))
apply(totale, 2, function(x) any(is.nan(x)))
summary(totale)

# Correzione
# ci sono 4 date da sistemare inizio dalla prima
# 2012   03     25 02:00:00
# 2013   03     31 02:00:00
# 2014   03     30 02:00:00
# 2015   03     29 02:00:00

totale %>%
  mutate(dateTime.x = if_else((giorno =="25")
                              & (anno == "2012")
                              & (mese == "03")
                              & (ora=="02:00:00"),
                              as.POSIXct("2012-03-25 02:00:00"),
                              dateTime.x)) -> totale

totale %>% mutate(dateTime.x = if_else((giorno =="31")
                                       & (anno == "2013")
                                       & (mese == "03")
                                       & (ora=="02:00:00"),
                                       as.POSIXct("2013-03-31 02:00:00"),
                                       dateTime.x)) -> totale

totale %>% mutate(dateTime.x = if_else((giorno =="30")
                                       & (anno == "2014")
                                       & (mese == "03")
                                       & (ora=="02:00:00"),
                                       as.POSIXct("2014-03-30 02:00:00"),
                                       dateTime.x)) -> totale

totale %>% mutate(dateTime.x = if_else((giorno =="29")
                                       & (anno == "2015")
                                       & (mese == "03")
                                       & (ora=="02:00:00"),
                                       as.POSIXct("2015-03-29 02:00:00"),
                                       dateTime.x)) -> totale
# totale pulito
###################################################################
# df_prezzi

df_prezzi %>%
  mutate(dateTime = if_else((giorno =="25")
                              & (anno == "2012")
                              & (mese == "03")
                              & (ora=="02:00:00"),
                              as.POSIXct("2012-03-25 02:00:00"),
                              dateTime)) -> df_prezzi

df_prezzi %>% mutate(dateTime = if_else((giorno =="31")
                                       & (anno == "2013")
                                       & (mese == "03")
                                       & (ora=="02:00:00"),
                                       as.POSIXct("2013-03-31 02:00:00"),
                                       dateTime)) -> df_prezzi

df_prezzi %>% mutate(dateTime = if_else((giorno =="30")
                                       & (anno == "2014")
                                       & (mese == "03")
                                       & (ora=="02:00:00"),
                                       as.POSIXct("2014-03-30 02:00:00"),
                                       dateTime)) -> df_prezzi

df_prezzi %>% mutate(dateTime = if_else((giorno =="29")
                                       & (anno == "2015")
                                       & (mese == "03")
                                       & (ora=="02:00:00"),
                                       as.POSIXct("2015-03-29 02:00:00"),
                                       dateTime)) -> df_prezzi

# df_consumi

df_consumi %>%
  mutate(dateTime = if_else((giorno =="25")
                            & (anno == "2012")
                            & (mese == "03")
                            & (ora=="02:00:00"),
                            as.POSIXct("2012-03-25 02:00:00"),
                            dateTime)) -> df_consumi

df_consumi %>% mutate(dateTime = if_else((giorno =="31")
                                        & (anno == "2013")
                                        & (mese == "03")
                                        & (ora=="02:00:00"),
                                        as.POSIXct("2013-03-31 02:00:00"),
                                        dateTime)) ->df_consumi

df_consumi %>% mutate(dateTime = if_else((giorno =="30")
                                        & (anno == "2014")
                                        & (mese == "03")
                                        & (ora=="02:00:00"),
                                        as.POSIXct("2014-03-30 02:00:00"),
                                        dateTime)) -> df_consumi

df_consumi %>% mutate(dateTime = if_else((giorno =="29")
                                        & (anno == "2015")
                                        & (mese == "03")
                                        & (ora=="02:00:00"),
                                        as.POSIXct("2015-03-29 02:00:00"),
                                        dateTime)) -> df_consumi

############################################################################################
# Ci sono na in dateTime.x__________________FATTO

# Vediamo quanti sono
apply(is.na(totale),2,sum)

# Sono presenti 4 Na in dateTime.x 

summary(df_prezzi)
# Esattamente si trovano in df_prezzi -> dateTime, sistemare df_prezzi
which(is.na(df_prezzi$dateTime))

summary(df_consumi)
# Esattamente si trovano in df_consumi -> dateTime, sistemare df_consumi


# CREO DATASET PER OGNI ANNO 
# Prezzo
lista_anni <- c(2012,2013,2014,2015)
datasets <- list()
for(i in lista_anni){
  df_prezzi %>% filter(anno==i) -> x
  assign(paste0('df_anno_prezzi_',i), x)
  datasets <- append(datasets,paste0('df_anno_prezzi',i))
}

# Consumi
datasets <- list()
for(i in lista_anni){
  df_consumi %>% filter(anno==i) -> x
  assign(paste0('df_anno_consumi_',i), x)
  datasets <- append(datasets,paste0('df_anno_consumi',i))
}

# SPOSTO LA COLONNA DATA ALL'INIZIO
# Prezzo
df_anno_prezzi_2012 <- df_anno_prezzi_2012 %>%
  select(date, everything())
df_anno_prezzi_2013 <- df_anno_prezzi_2013 %>%
  select(date, everything())
df_anno_prezzi_2014 <- df_anno_prezzi_2014 %>%
  select(date, everything())
df_anno_prezzi_2015 <- df_anno_prezzi_2015 %>%
  select(date, everything())

# Consumi
df_anno_consumi_2012 <- df_anno_consumi_2012 %>%
  select(date, everything())
df_anno_consumi_2013 <- df_anno_consumi_2013 %>%
  select(date, everything())
df_anno_consumi_2014 <- df_anno_consumi_2014 %>%
  select(date, everything())
df_anno_consumi_2015 <- df_anno_consumi_2015 %>%
  select(date, everything())


# 365 x 24 = 8760

# 2012
# PREZZI
df_anno_prezzi_2012$ID <- seq.int(nrow(df_anno_prezzi_2012))
# FIX NA
summary(df_anno_prezzi_2012$dateTime)

which(is.na(df_anno_prezzi_2012$dateTime))

new_DF<-subset(df_anno_prezzi_2012,is.na(df_anno_prezzi_2012$dateTime))

summary(df_anno_prezzi_2012)
# CONSUMI
df_anno_consumi_2012$ID <- seq.int(nrow(df_anno_consumi_2012))
# FIX NA
summary(df_anno_consumi_2012$dateTime)

which(is.na(df_anno_consumi_2012$dateTime))

summary(df_anno_consumi_2012)

# 2013
# PREZZI
df_anno_prezzi_2013$ID <- seq.int(nrow(df_anno_prezzi_2013))
# FIX NA
summary(df_anno_prezzi_2013$dateTime)

which(is.na(df_anno_prezzi_2013$dateTime))

new_DF<-subset(df_anno_prezzi_2013,is.na(df_anno_prezzi_2013$dateTime))

summary(df_anno_prezzi_2013)

# CONSUMI
df_anno_consumi_2013$ID <- seq.int(nrow(df_anno_consumi_2013))
# FIX NA
summary(df_anno_consumi_2013$dateTime)

which(is.na(df_anno_consumi_2013$dateTime))

new_DF<-subset(df_anno_consumi_2013,is.na(df_anno_consumi_2013$dateTime))

summary(df_anno_consumi_2013)

# 2014
# PREZZI
df_anno_prezzi_2014$ID <- seq.int(nrow(df_anno_prezzi_2014))
# FIX NA
summary(df_anno_prezzi_2014$dateTime)

which(is.na(df_anno_prezzi_2014$dateTime))

new_DF<-subset(df_anno_prezzi_2014,is.na(df_anno_prezzi_2014$dateTime))

summary(df_anno_prezzi_2014)

# CONSUMI
df_anno_consumi_2014$ID <- seq.int(nrow(df_anno_consumi_2014))
# FIX NA
summary(df_anno_consumi_2014$dateTime)

which(is.na(df_anno_consumi_2014$dateTime))

new_DF<-subset(df_anno_consumi_2014,is.na(df_anno_consumi_2014$dateTime))

summary(df_anno_consumi_2014)

# 2015
# PREZZI
df_anno_prezzi_2015$ID <- seq.int(nrow(df_anno_prezzi_2015))
# FIX NA
summary(df_anno_prezzi_2015$dateTime)

which(is.na(df_anno_prezzi_2015$dateTime))

new_DF<-subset(df_anno_prezzi_2015,is.na(df_anno_prezzi_2015$dateTime))

summary(df_anno_prezzi_2015)

df_anno_prezzi_2015[2090,]

# 2015
# CONSUMI
df_anno_consumi_2015$ID <- seq.int(nrow(df_anno_consumi_2015))
# FIX NA
summary(df_anno_consumi_2015$dateTime)

which(is.na(df_anno_consumi_2015$dateTime))

new_DF<-subset(df_anno_consumi_2015,is.na(df_anno_consumi_2015$dateTime))

summary(df_anno_consumi_2015)

########################################################################################################
# PROVE ALESSANDRO
# mi concentro su 2012
############################################################
########## TENTATIVI DI TS #################################
############################################################
# aggrego per 365 giorni 2012
totale %>%
  group_by(mese,giorno) %>%
  summarize(prezzimediani = median(prezzo)) -> stagioni_2012_giorno

ts_season_median <- zoo(stagioni_2012_giorno$prezzimediani, order.by = c(stagioni_2012_giorno$mese,stagioni_2012_giorno$giorno))
ts_season_median <- ts(ts_season_median, frequency = 366)
autoplot(ts_season_median)


#write.csv(df_anno_prezzi_2012, "/Volumes/HDD_Ale/Progetto_DSLAB/Lavoro Ale/Dataset/prezzi_polonia_2012.csv")
#1
cc <- zoo(df_anno_prezzi_2012[, c("dateTime", "prezzo")])
cc <- ts(cc, frequency = 24)
autoplot(cc, main = "Prezzi")

#2
ts_2012 <- zoo(df_anno_prezzi_2012$prezzo, df_anno_prezzi_2012$dateTime)
ts_2012 <- ts(ts_2012, frequency = 24)
autoplot(ts_2012, main = "Prezzi")

#3
firstHour <- 24*(as.Date("2012-12-31 23:00:00")-as.Date("2012-1-1 00:00:00"))
tt <- ts(df_anno_prezzi_2012$prezzo,start=c(2012,firstHour),frequency=(24*365)+1)
autoplot(tt, main = "Prezzi")

#4
time_index <- seq(from =df_anno_prezzi_2012$dateTime[1], 
                  to = df_anno_prezzi_2012$dateTime[length(df_anno_prezzi_2012$dateTime)], by = "hour")
eventdata <- xts(df_anno_prezzi_2012$prezzo, order.by = time_index)

#5 migliore di tutti xts
eventdata2 <- xts(df_anno_prezzi_2012$prezzo, order.by = df_anno_prezzi_2012$dateTime)
plot.xts(eventdata2)


##########################################################

# Prove grafici

totale_prezzi_ID<- rbind(df_anno_prezzi_2012, df_anno_prezzi_2013, df_anno_prezzi_2014, 
               df_anno_prezzi_2015)

totale_consumi_ID<- rbind(df_anno_consumi_2012, df_anno_consumi_2013, df_anno_consumi_2014, 
                         df_anno_consumi_2015)

totale_prezzi_ID %>% group_by(date, anno, ID) %>%  # raggruppo
  summarise(prezzo= sum(prezzo)) %>%           # sintetizzo
  arrange(anno, date) %>%                         # ordino
  ungroup() -> totale_anno_somma_prezzi

totale_consumi_ID %>% group_by(date, anno, ID) %>%  # raggruppo
  summarise(consumo= sum(consumo)) %>%           # sintetizzo
  arrange(anno, date) %>%                         # ordino
  ungroup() -> totale_anno_somma_consumi

# Grafico totale
totale_anno_somma_prezzi %>% group_by(date, anno, ID) %>% summarise(prezzo) %>%
  ggplot(aes(x = ID , y = prezzo, color = anno)) +
  geom_line()

totale_anno_somma_consumi %>% group_by(date, anno, ID) %>% summarise(consumo) %>%
  ggplot(aes(x = ID , y = consumo, color = anno)) +
  geom_line()

# Mese gennaio
gennaio_prezzi <- totale_prezzi_ID %>% filter(mese == "01")

# Grafico totale gennaio
gennaio_prezzi %>% group_by(date, anno, ID) %>%
  ggplot(aes(x = ID , y = prezzo, color = anno)) +
  geom_line()

gennaio_consumi <- totale_consumi_ID %>% filter(mese == "01")

# Grafico totale gennaio
gennaio_consumi %>% group_by(date, anno, ID) %>%
  ggplot(aes(x = ID , y = consumo, color = anno)) +
  geom_line()

###################################################################
# Prove decomposizione stagionale

# ordinato<- totale_ID %>% arrange(ID)

yprezzi<- ts(totale_prezzi_ID$prezzo, start=c(2012), end=c(2015), frequency=8760)

yconsumi<- ts(totale_consumi_ID$consumo, start=c(2012), end=c(2015), frequency=8760)

yprezzi %>% decompose(type="multiplicative") %>%
  autoplot() + xlab("ID") +
  ggtitle("Classical multiplicative decomposition")

yconsumi %>% decompose(type="multiplicative") %>%
  autoplot() + xlab("ID") +
  ggtitle("Classical multiplicative decomposition")


# Seasonal plots
library(forecast)

# Prezzi

ggseasonplot(yprezzi, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("prezzo") +
  ggtitle("Seasonal plot")

ggseasonplot(yprezzi, polar=TRUE) +
  ylab("prezzo") +
  ggtitle("Polar seasonal plot")

ggsubseriesplot(yprezzi) +
  ylab("prezzo") +
  ggtitle("Seasonal subseries plot")


# Consumi
ggseasonplot(yconsumi, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("consumo") +
  ggtitle("Seasonal plot")

ggseasonplot(yconsumi, polar=TRUE) +
  ylab("consumo") +
  ggtitle("Polar seasonal plot")

ggsubseriesplot(yconsumi) +
  ylab("consumo") +
  ggtitle("Seasonal subseries plot")



# PLOT two ts in the same plot 
# two graph or only one

# Prezzi e consumi
elecdemand<-c(yprezzi, yconsumi)

# autoplot(elecdemand[,c("yprezzi","yconsumi")], facets=TRUE) +
#  xlab("Year:2014") + ylab("") +
 # ggtitle("Hourly electricity demand")

# require(graphics)

#ts.plot(yprezzi, yconsumi,
 #       gpars=list(xlab="year", ylab="valori", lty=c(1:3)))


# git per modifica e aggiornamento?????

##################################################################################################




