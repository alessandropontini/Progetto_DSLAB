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

##################################################################################################

#############################################################################

write.csv(df_prezzi, "C:\\Users\\vizzi\\PROG_DSLAB_GITHUB\\Progetto_DSLAB\\Lavoro Toto\\replace\\prezzigiorno.csv")

temp_Scandi<- read.csv("C:\\Users\\vizzi\\PROG_DSLAB_GITHUB\\Progetto_DSLAB\\Lavoro Toto\\finito\\temperature_toto_totale_pulito.csv")

summary(totale)
summary(temp_Scandi)

# Sistemato#######################################
df_prezzi_genna<- read.csv("C:\\Users\\vizzi\\PROG_DSLAB_GITHUB\\Progetto_DSLAB\\Lavoro Toto\\replace\\prezzigiorno.csv")

df_prezzi_genna$X <- NULL

# SECONDO LEFT JOIN 

totale2_pulito <- left_join(df_prezzi_genna, df_consumi, by = c("anno" = "anno", "mese" = "mese", "giorno"="giorno", "ora"="ora"))
totale2_pulito <- totale2_pulito[, -c(9:10)]

df_prezzi_genna$anno <- as.character(df_prezzi_genna$anno)  
# df_consumi$anno <- as.character(df_consumi$anno)  

df_prezzi_genna$mese <- as.character(df_prezzi_genna$mese)  
df_prezzi_genna$giorno <- as.character(df_prezzi_genna$giorno)  
df_prezzi_genna$ora <- as.character(df_prezzi_genna$ora)  

str(df_prezzi_genna)
str(df_consumi)


v = list("1", "2", "3", "4", "5", "6", "7", "8", "9")
# z = list("0:00:00","1:00:00", "2:00:00", "3:00:00", "4:00:00", "5:00:00", "6:00:00", "7:00:00", "8:00:00", "9:00:00")
k = list("24:00:00")

df_prezzi_genna %>% mutate(giorno = if_else(giorno %in% v, paste0("0",giorno), giorno)) -> df_prezzi_genna
df_prezzi_genna %>% mutate(mese = if_else(mese %in% v, paste0("0",mese), mese)) -> df_prezzi_genna


temp_Scandi$Mese <- as.character(temp_Scandi$Mese)  
temp_Scandi$Giorno <- as.character(temp_Scandi$Giorno)  
temp_Scandi$OraFix5 <- as.character(temp_Scandi$OraFix5) 
temp_Scandi$Anno <- as.character(temp_Scandi$Anno)  

str(temp_Scandi)
temp_Scandi %>% mutate(Giorno = if_else(Giorno %in% v, paste0("0",Giorno), Giorno)) -> temp_Scandi

temp_Scandi %>% mutate(Mese = if_else(Mese %in% v, paste0("0",Mese), Mese)) -> temp_Scandi


# totale %>% mutate(ora = if_else(ora %in% z, paste0("0",ora), ora)) -> totale
# df_prezzi_genna %>% mutate(ora = if_else(ora %in% k, "00:00:00", ora)) -> df_prezzi_genna
# df_consumi %>% mutate(ora = if_else(ora %in% k, "00:00:00", ora)) -> df_prezzi_genna

totale2_pulito %>% mutate(ora = if_else(ora %in% k, "00:00:00", ora)) -> totale2_pulito

totale_prova_temp <- left_join(totale2_pulito, temp_Scandi, by = c("anno" = "Anno", "mese" = "Mese", "giorno"="Giorno", "ora"="OraFix5"))

summary(totale_prova_temp)


### 2012

# gennaio
m = list("01")
a = list("2012")

totale_prova_temp %>% filter(mese=='01', anno=='2012') -> gennaio_2012
mediana <- median(gennaio_2012$GradoFix, na.rm = TRUE)

totale_prova_temp %>% mutate(GradoFix = if_else(anno %in% a & mese %in% m & is.na(GradoFix), mediana, GradoFix)) -> totale_prova_temp

# febbraio
m = list("02")
a = list("2012")

totale_prova_temp %>% filter(mese=='02', anno=='2012') -> gennaio_2012
mediana <- median(gennaio_2012$GradoFix, na.rm = TRUE)

totale_prova_temp %>% mutate(GradoFix = if_else(anno %in% a & mese %in% m & is.na(GradoFix), mediana, GradoFix)) -> totale_prova_temp

# marzo
m = list("03")
a = list("2012")

totale_prova_temp %>% filter(mese=='03', anno=='2012') -> gennaio_2012
mediana <- median(gennaio_2012$GradoFix, na.rm = TRUE)

totale_prova_temp %>% mutate(GradoFix = if_else(anno %in% a & mese %in% m & is.na(GradoFix), mediana, GradoFix)) -> totale_prova_temp

# aprile
m = list("04")
a = list("2012")

totale_prova_temp %>% filter(mese=='04', anno=='2012') -> gennaio_2012
mediana <- median(gennaio_2012$GradoFix, na.rm = TRUE)

totale_prova_temp %>% mutate(GradoFix = if_else(anno %in% a & mese %in% m & is.na(GradoFix), mediana, GradoFix)) -> totale_prova_temp

# maggio
m = list("05")
a = list("2012")

totale_prova_temp %>% filter(mese=='05', anno=='2012') -> gennaio_2012
mediana <- median(gennaio_2012$GradoFix, na.rm = TRUE)

totale_prova_temp %>% mutate(GradoFix = if_else(anno %in% a & mese %in% m & is.na(GradoFix), mediana, GradoFix)) -> totale_prova_temp


# giugno
m = list("06")
a = list("2012")

totale_prova_temp %>% filter(mese=='06', anno=='2012') -> gennaio_2012
mediana <- median(gennaio_2012$GradoFix, na.rm = TRUE)

totale_prova_temp %>% mutate(GradoFix = if_else(anno %in% a & mese %in% m & is.na(GradoFix), mediana, GradoFix)) -> totale_prova_temp


# luglio
m = list("07")
a = list("2012")

totale_prova_temp %>% filter(mese=='07', anno=='2012') -> gennaio_2012
mediana <- median(gennaio_2012$GradoFix, na.rm = TRUE)

totale_prova_temp %>% mutate(GradoFix = if_else(anno %in% a & mese %in% m & is.na(GradoFix), mediana, GradoFix)) -> totale_prova_temp

# agosto
m = list("08")
a = list("2012")

totale_prova_temp %>% filter(mese=='08', anno=='2012') -> gennaio_2012
mediana <- median(gennaio_2012$GradoFix, na.rm = TRUE)

totale_prova_temp %>% mutate(GradoFix = if_else(anno %in% a & mese %in% m & is.na(GradoFix), mediana, GradoFix)) -> totale_prova_temp

# settembre
m = list("09")
a = list("2012")

totale_prova_temp %>% filter(mese=='09', anno=='2012') -> gennaio_2012
mediana <- median(gennaio_2012$GradoFix, na.rm = TRUE)

totale_prova_temp %>% mutate(GradoFix = if_else(anno %in% a & mese %in% m & is.na(GradoFix), mediana, GradoFix)) -> totale_prova_temp

# ottobre
m = list("10")
a = list("2012")

totale_prova_temp %>% filter(mese=='10', anno=='2012') -> gennaio_2012
mediana <- median(gennaio_2012$GradoFix, na.rm = TRUE)

totale_prova_temp %>% mutate(GradoFix = if_else(anno %in% a & mese %in% m & is.na(GradoFix), mediana, GradoFix)) -> totale_prova_temp

# novembre
m = list("11")
a = list("2012")

totale_prova_temp %>% filter(mese=='11', anno=='2012') -> gennaio_2012
mediana <- median(gennaio_2012$GradoFix, na.rm = TRUE)

totale_prova_temp %>% mutate(GradoFix = if_else(anno %in% a & mese %in% m & is.na(GradoFix), mediana, GradoFix)) -> totale_prova_temp

# dicembre
m = list("12")
a = list("2012")

totale_prova_temp %>% filter(mese=='12', anno=='2012') -> gennaio_2012
mediana <- median(gennaio_2012$GradoFix, na.rm = TRUE)

totale_prova_temp %>% mutate(GradoFix = if_else(anno %in% a & mese %in% m & is.na(GradoFix), mediana, GradoFix)) -> totale_prova_temp

### 2013

# gennaio
m = list("01")
a = list("2013")

totale_prova_temp %>% filter(mese=='01', anno=='2013') -> gennaio_2012
mediana <- median(gennaio_2012$GradoFix, na.rm = TRUE)

totale_prova_temp %>% mutate(GradoFix = if_else(anno %in% a & mese %in% m & is.na(GradoFix), mediana, GradoFix)) -> totale_prova_temp

# febbraio
m = list("02")
a = list("2013")

totale_prova_temp %>% filter(mese=='02', anno=='2013') -> gennaio_2012
mediana <- median(gennaio_2012$GradoFix, na.rm = TRUE)

totale_prova_temp %>% mutate(GradoFix = if_else(anno %in% a & mese %in% m & is.na(GradoFix), mediana, GradoFix)) -> totale_prova_temp

# marzo
m = list("03")
a = list("2013")

totale_prova_temp %>% filter(mese=='03', anno=='2013') -> gennaio_2012
mediana <- median(gennaio_2012$GradoFix, na.rm = TRUE)

totale_prova_temp %>% mutate(GradoFix = if_else(anno %in% a & mese %in% m & is.na(GradoFix), mediana, GradoFix)) -> totale_prova_temp

# aprile
m = list("04")
a = list("2013")

totale_prova_temp %>% filter(mese=='04', anno=='2013') -> gennaio_2012
mediana <- median(gennaio_2012$GradoFix, na.rm = TRUE)

totale_prova_temp %>% mutate(GradoFix = if_else(anno %in% a & mese %in% m & is.na(GradoFix), mediana, GradoFix)) -> totale_prova_temp

# maggio
m = list("05")
a = list("2013")

totale_prova_temp %>% filter(mese=='05', anno=='2013') -> gennaio_2012
mediana <- median(gennaio_2012$GradoFix, na.rm = TRUE)

totale_prova_temp %>% mutate(GradoFix = if_else(anno %in% a & mese %in% m & is.na(GradoFix), mediana, GradoFix)) -> totale_prova_temp


# giugno
m = list("06")
a = list("2013")

totale_prova_temp %>% filter(mese=='06', anno=='2013') -> gennaio_2012
mediana <- median(gennaio_2012$GradoFix, na.rm = TRUE)

totale_prova_temp %>% mutate(GradoFix = if_else(anno %in% a & mese %in% m & is.na(GradoFix), mediana, GradoFix)) -> totale_prova_temp


# luglio
m = list("07")
a = list("2013")

totale_prova_temp %>% filter(mese=='07', anno=='2013') -> gennaio_2012
mediana <- median(gennaio_2012$GradoFix, na.rm = TRUE)

totale_prova_temp %>% mutate(GradoFix = if_else(anno %in% a & mese %in% m & is.na(GradoFix), mediana, GradoFix)) -> totale_prova_temp

# agosto
m = list("08")
a = list("2013")

totale_prova_temp %>% filter(mese=='08', anno=='2013') -> gennaio_2012
mediana <- median(gennaio_2012$GradoFix, na.rm = TRUE)

totale_prova_temp %>% mutate(GradoFix = if_else(anno %in% a & mese %in% m & is.na(GradoFix), mediana, GradoFix)) -> totale_prova_temp

# settembre
m = list("09")
a = list("2013")

totale_prova_temp %>% filter(mese=='09', anno=='2013') -> gennaio_2012
mediana <- median(gennaio_2012$GradoFix, na.rm = TRUE)

totale_prova_temp %>% mutate(GradoFix = if_else(anno %in% a & mese %in% m & is.na(GradoFix), mediana, GradoFix)) -> totale_prova_temp

# ottobre
m = list("10")
a = list("2013")

totale_prova_temp %>% filter(mese=='10', anno=='2013') -> gennaio_2012
mediana <- median(gennaio_2012$GradoFix, na.rm = TRUE)

totale_prova_temp %>% mutate(GradoFix = if_else(anno %in% a & mese %in% m & is.na(GradoFix), mediana, GradoFix)) -> totale_prova_temp

# novembre
m = list("11")
a = list("2013")

totale_prova_temp %>% filter(mese=='11', anno=='2013') -> gennaio_2012
mediana <- median(gennaio_2012$GradoFix, na.rm = TRUE)

totale_prova_temp %>% mutate(GradoFix = if_else(anno %in% a & mese %in% m & is.na(GradoFix), mediana, GradoFix)) -> totale_prova_temp

# dicembre
m = list("12")
a = list("2013")

totale_prova_temp %>% filter(mese=='12', anno=='2013') -> gennaio_2012
mediana <- median(gennaio_2012$GradoFix, na.rm = TRUE)

totale_prova_temp %>% mutate(GradoFix = if_else(anno %in% a & mese %in% m & is.na(GradoFix), mediana, GradoFix)) -> totale_prova_temp

### 2014

# gennaio
m = list("01")
a = list("2014")

totale_prova_temp %>% filter(mese=='01', anno=='2014') -> gennaio_2012
mediana <- median(gennaio_2012$GradoFix, na.rm = TRUE)

totale_prova_temp %>% mutate(GradoFix = if_else(anno %in% a & mese %in% m & is.na(GradoFix), mediana, GradoFix)) -> totale_prova_temp

# febbraio
m = list("02")
a = list("2014")

totale_prova_temp %>% filter(mese=='02', anno=='2014') -> gennaio_2012
mediana <- median(gennaio_2012$GradoFix, na.rm = TRUE)

totale_prova_temp %>% mutate(GradoFix = if_else(anno %in% a & mese %in% m & is.na(GradoFix), mediana, GradoFix)) -> totale_prova_temp

# marzo
m = list("03")
a = list("2014")

totale_prova_temp %>% filter(mese=='03', anno=='2014') -> gennaio_2012
mediana <- median(gennaio_2012$GradoFix, na.rm = TRUE)

totale_prova_temp %>% mutate(GradoFix = if_else(anno %in% a & mese %in% m & is.na(GradoFix), mediana, GradoFix)) -> totale_prova_temp

# aprile
m = list("04")
a = list("2014")

totale_prova_temp %>% filter(mese=='04', anno=='2014') -> gennaio_2012
mediana <- median(gennaio_2012$GradoFix, na.rm = TRUE)

totale_prova_temp %>% mutate(GradoFix = if_else(anno %in% a & mese %in% m & is.na(GradoFix), mediana, GradoFix)) -> totale_prova_temp

# maggio
m = list("05")
a = list("2014")

totale_prova_temp %>% filter(mese=='05', anno=='2014') -> gennaio_2012
mediana <- median(gennaio_2012$GradoFix, na.rm = TRUE)

totale_prova_temp %>% mutate(GradoFix = if_else(anno %in% a & mese %in% m & is.na(GradoFix), mediana, GradoFix)) -> totale_prova_temp


# giugno
m = list("06")
a = list("2014")

totale_prova_temp %>% filter(mese=='06', anno=='2014') -> gennaio_2012
mediana <- median(gennaio_2012$GradoFix, na.rm = TRUE)

totale_prova_temp %>% mutate(GradoFix = if_else(anno %in% a & mese %in% m & is.na(GradoFix), mediana, GradoFix)) -> totale_prova_temp


# luglio
m = list("07")
a = list("2014")

totale_prova_temp %>% filter(mese=='07', anno=='2014') -> gennaio_2012
mediana <- median(gennaio_2012$GradoFix, na.rm = TRUE)

totale_prova_temp %>% mutate(GradoFix = if_else(anno %in% a & mese %in% m & is.na(GradoFix), mediana, GradoFix)) -> totale_prova_temp

# agosto
m = list("08")
a = list("2014")

totale_prova_temp %>% filter(mese=='08', anno=='2014') -> gennaio_2012
mediana <- median(gennaio_2012$GradoFix, na.rm = TRUE)

totale_prova_temp %>% mutate(GradoFix = if_else(anno %in% a & mese %in% m & is.na(GradoFix), mediana, GradoFix)) -> totale_prova_temp

# settembre
m = list("09")
a = list("2014")

totale_prova_temp %>% filter(mese=='09', anno=='2014') -> gennaio_2012
mediana <- median(gennaio_2012$GradoFix, na.rm = TRUE)

totale_prova_temp %>% mutate(GradoFix = if_else(anno %in% a & mese %in% m & is.na(GradoFix), mediana, GradoFix)) -> totale_prova_temp

# ottobre
m = list("10")
a = list("2014")

totale_prova_temp %>% filter(mese=='10', anno=='2014') -> gennaio_2012
mediana <- median(gennaio_2012$GradoFix, na.rm = TRUE)

totale_prova_temp %>% mutate(GradoFix = if_else(anno %in% a & mese %in% m & is.na(GradoFix), mediana, GradoFix)) -> totale_prova_temp

# novembre
m = list("11")
a = list("2014")

totale_prova_temp %>% filter(mese=='11', anno=='2014') -> gennaio_2012
mediana <- median(gennaio_2012$GradoFix, na.rm = TRUE)

totale_prova_temp %>% mutate(GradoFix = if_else(anno %in% a & mese %in% m & is.na(GradoFix), mediana, GradoFix)) -> totale_prova_temp

# dicembre
m = list("12")
a = list("2014")

totale_prova_temp %>% filter(mese=='12', anno=='2014') -> gennaio_2012
mediana <- median(gennaio_2012$GradoFix, na.rm = TRUE)

totale_prova_temp %>% mutate(GradoFix = if_else(anno %in% a & mese %in% m & is.na(GradoFix), mediana, GradoFix)) -> totale_prova_temp

### 2015

# gennaio
m = list("01")
a = list("2015")

totale_prova_temp %>% filter(mese=='01', anno=='2015') -> gennaio_2012
mediana <- median(gennaio_2012$GradoFix, na.rm = TRUE)

totale_prova_temp %>% mutate(GradoFix = if_else(anno %in% a & mese %in% m & is.na(GradoFix), mediana, GradoFix)) -> totale_prova_temp

# febbraio
m = list("02")
a = list("2015")

totale_prova_temp %>% filter(mese=='02', anno=='2015') -> gennaio_2012
mediana <- median(gennaio_2012$GradoFix, na.rm = TRUE)

totale_prova_temp %>% mutate(GradoFix = if_else(anno %in% a & mese %in% m & is.na(GradoFix), mediana, GradoFix)) -> totale_prova_temp

# marzo
m = list("03")
a = list("2015")

totale_prova_temp %>% filter(mese=='03', anno=='2015') -> gennaio_2012
mediana <- median(gennaio_2012$GradoFix, na.rm = TRUE)

totale_prova_temp %>% mutate(GradoFix = if_else(anno %in% a & mese %in% m & is.na(GradoFix), mediana, GradoFix)) -> totale_prova_temp

# aprile
m = list("04")
a = list("2015")

totale_prova_temp %>% filter(mese=='04', anno=='2015') -> gennaio_2012
mediana <- median(gennaio_2012$GradoFix, na.rm = TRUE)

totale_prova_temp %>% mutate(GradoFix = if_else(anno %in% a & mese %in% m & is.na(GradoFix), mediana, GradoFix)) -> totale_prova_temp

# maggio
m = list("05")
a = list("2015")

totale_prova_temp %>% filter(mese=='05', anno=='2015') -> gennaio_2012
mediana <- median(gennaio_2012$GradoFix, na.rm = TRUE)

totale_prova_temp %>% mutate(GradoFix = if_else(anno %in% a & mese %in% m & is.na(GradoFix), mediana, GradoFix)) -> totale_prova_temp


# giugno
m = list("06")
a = list("2015")

totale_prova_temp %>% filter(mese=='06', anno=='2015') -> gennaio_2012
mediana <- median(gennaio_2012$GradoFix, na.rm = TRUE)

totale_prova_temp %>% mutate(GradoFix = if_else(anno %in% a & mese %in% m & is.na(GradoFix), mediana, GradoFix)) -> totale_prova_temp


# luglio
m = list("07")
a = list("2015")

totale_prova_temp %>% filter(mese=='07', anno=='2015') -> gennaio_2012
mediana <- median(gennaio_2012$GradoFix, na.rm = TRUE)

totale_prova_temp %>% mutate(GradoFix =if_else(anno %in% a & mese %in% m & is.na(GradoFix), mediana, GradoFix)) -> totale_prova_temp

# agosto
m = list("08")
a = list("2015")

totale_prova_temp %>% filter(mese=='08', anno=='2015') -> gennaio_2012
mediana <- median(gennaio_2012$GradoFix, na.rm = TRUE)

totale_prova_temp %>% mutate(GradoFix = if_else(anno %in% a & mese %in% m & is.na(GradoFix), mediana, GradoFix)) -> totale_prova_temp

# settembre
m = list("09")
a = list("2015")

totale_prova_temp %>% filter(mese=='09', anno=='2015') -> gennaio_2012
mediana <- median(gennaio_2012$GradoFix, na.rm = TRUE)

totale_prova_temp %>% mutate(GradoFix = if_else(anno %in% a & mese %in% m & is.na(GradoFix), mediana, GradoFix)) -> totale_prova_temp

# ottobre
m = list("10")
a = list("2015")

totale_prova_temp %>% filter(mese=='10', anno=='2015') -> gennaio_2012
mediana <- median(gennaio_2012$GradoFix, na.rm = TRUE)

totale_prova_temp %>% mutate(GradoFix = if_else(anno %in% a & mese %in% m & is.na(GradoFix), mediana, GradoFix)) -> totale_prova_temp

# novembre
m = list("11")
a = list("2015")

totale_prova_temp %>% filter(mese=='11', anno=='2015') -> gennaio_2012
mediana <- median(gennaio_2012$GradoFix, na.rm = TRUE)

totale_prova_temp %>% mutate(GradoFix = if_else(anno %in% a & mese %in% m & is.na(GradoFix), mediana, GradoFix)) -> totale_prova_temp

# dicembre
m = list("12")
a = list("2015")

totale_prova_temp %>% filter(mese=='12', anno=='2015') -> gennaio_2012
mediana <- median(gennaio_2012$GradoFix, na.rm = TRUE)

totale_prova_temp %>% mutate(GradoFix = if_else(anno %in% a & mese %in% m & is.na(GradoFix), mediana, GradoFix)) -> totale_prova_temp



summary(totale_prova_temp)

write.csv(totale_prova_temp, "C:\\Users\\vizzi\\PROG_DSLAB_GITHUB\\Progetto_DSLAB\\Lavoro Toto\\dataset_finish\\dataset_finish_totale_prova_temp.csv")


########################################################################################


percentuali <- function(x,y){
  
  risultato <- list()
  risultato[[1]] <- 0
  k <- x[1,y]
  
  for (i in 2:35068) {
    risultato[[i]] <- ((x[i,y]-k)*100)/k
    k <- x[i,y]
  }
  return(risultato)
}




consumiperc <- percentuali(totale_prova_temp,8)
consumiperc <- data.frame(matrix(unlist(consumiperc), nrow=35068, byrow=T),stringsAsFactors=FALSE)



prezziperc <- percentuali(totale_prova_temp,5)
prezziperc <- data.frame(matrix(unlist(prezziperc), nrow=35068, byrow=T),stringsAsFactors=FALSE)




consumitsperc <- ts(consumiperc, frequency = 8760, start = c(2012, 1),end=c(2015, 8760))
prezzitsperc <- ts(prezziperc, frequency = 8760, start = c(2012, 1),end=c(2015, 8760))
TSTOT <-  ts(coredata(totale_prova_temp), freq = 8760, start = c(2012,1,1),  end = c(2015,8760))

summary(TSTOT)

tot <- cbind(TSTOT, consumitsperc)
tot <- cbind(tot, prezzitsperc)



tot <- tot[,-c(1:4,6:7)]



colnames(tot)[1] <- "Prezzo"
colnames(tot)[2] <- "Consumi"
colnames(tot)[3] <- "Gradi"
colnames(tot)[4] <- "Consumiperc"
colnames(tot)[5] <- "Prezzoperc"

write.csv(tot, "C:\\Users\\vizzi\\PROG_DSLAB_GITHUB\\Progetto_DSLAB\\DATASET SERIE STORICHE\\SERIESTORICASCANDINAVIA.csv")
############################################################################################################################

TSTOT2 <- read.csv.zoo("C:\\Users\\vizzi\\PROG_DSLAB_GITHUB\\Progetto_DSLAB\\DATASET SERIE STORICHE\\SERIESTORICASCANDINAVIA.csv")

TSTOT <-  ts(coredata(TSTOT2), freq = 8760, start = c(2012,1,1),  end = c(2015,8760))

##########################################


TSTOT2 <- read.csv.zoo("C:\\Users\\vizzi\\PROG_DSLAB_GITHUB\\Progetto_DSLAB\\DATASET SERIE STORICHE\\SERIESTORICAUK.csv")

TSTOTUK <-  ts(coredata(TSTOT2), freq = 8760, start = c(2012,1,1),  end = c(2015,8760))

###########################################################

TSTOT2 <- read.csv.zoo("C:\\Users\\vizzi\\PROG_DSLAB_GITHUB\\Progetto_DSLAB\\DATASET SERIE STORICHE\\SERIESTORICAPOLONIA.csv")

TSTOTPOL <-  ts(coredata(TSTOT2), freq = 8760, start = c(2012,1,1),  end = c(2015,8760))

#############################


ts.union(c(TSTOT, TSTOTUK, TSTOTPOL)) -> prova_ts

cbind(TSTOT, TSTOTPOL, TSTOTUK) -> prova_ts1
########################################################

colnames(prova_ts1)[1] <- "PrezzoScandi"
colnames(prova_ts1)[2] <- "ConsumiScandi"
colnames(prova_ts1)[3] <- "GradiScandi"
colnames(prova_ts1)[4] <- "ConsumipercScandi"
colnames(prova_ts1)[5] <- "PrezzopercScandi"

colnames(prova_ts1)[6] <- "PrezzoPol"
colnames(prova_ts1)[7] <- "ConsumiPol"
colnames(prova_ts1)[8] <- "ConsumipercPol"
colnames(prova_ts1)[9] <- "PrezzopercPol"
colnames(prova_ts1)[10] <- "GradiPol"

colnames(prova_ts1)[11] <- "PrezzoUK"
colnames(prova_ts1)[12] <- "ConsumiUK"
colnames(prova_ts1)[13] <- "GradiUK"
colnames(prova_ts1)[14] <- "ConsumipercUK"
colnames(prova_ts1)[15] <- "PrezzopercUK"


write.csv(prova_ts1, "C:\\Users\\vizzi\\PROG_DSLAB_GITHUB\\Progetto_DSLAB\\DATASET SERIE STORICHE\\SerieStorica3Nazioni.csv")

############################################

Dummies_frame_Scandi <- read_excel(paste0(ABSOLUTE_PATH,"\\dummies1215a.xls") , sheet = "NP") 

Dummies_frame_Scandi[rep(seq_len(nrow(Dummies_frame_Scandi)), each = 24), ]-> dummy_scandi2

str(dummy_scandi2)

dummy_scandi2$DayOfWeek <- as.character(dummy_scandi2$DayOfWeek)  

gino = list("1","2", "3", "4" , "5")
pino= list("6","7")

dummy_scandi2 %>% mutate(DayOfWeek = if_else(DayOfWeek %in% gino, "0", DayOfWeek)) -> dummy_scandi2

dummy_scandi2 %>% mutate(DayOfWeek = if_else(DayOfWeek %in% pino, "1", DayOfWeek)) -> dummy_scandi2


dummy_scandi2$Date <- NULL
dummy_scandi2$DayYear <- NULL


dummy_ts_scandi<-  ts(coredata(dummy_scandi2), freq = 8760, start = c(2012,1,1),  end = c(2015,8760))

prova_ts2<-cbind(prova_ts1, dummy_ts_scandi)


############################################

Dummies_frame_UK <- read_excel(paste0(ABSOLUTE_PATH,"\\dummies1215a.xls") , sheet = "UKPX") 

Dummies_frame_UK[rep(seq_len(nrow(Dummies_frame_UK)), each = 24), ]-> dummy_uk

str(dummy_uk)

dummy_uk$DayOfWeek <- as.character(dummy_uk$DayOfWeek)  

gino = list("1","2", "3", "4" , "5")
pino= list("6","7")

dummy_uk %>% mutate(DayOfWeek = if_else(DayOfWeek %in% gino, "0", DayOfWeek)) -> dummy_uk

dummy_uk %>% mutate(DayOfWeek = if_else(DayOfWeek %in% pino, "1", DayOfWeek)) -> dummy_uk


dummy_uk$Date <- NULL
dummy_uk$DayYear <- NULL


dummy_ts_uk<-  ts(coredata(dummy_uk), freq = 8760, start = c(2012,1,1),  end = c(2015,8760))

prova_ts2<-cbind(prova_ts2, dummy_ts_uk)
################################################

############################################

Dummies_frame_Pol <- read_excel(paste0(ABSOLUTE_PATH,"\\dummies1215a.xls") , sheet = "PL") 

Dummies_frame_Pol[rep(seq_len(nrow(Dummies_frame_Pol)), each = 24), ]-> dummy_Pol

str(dummy_Pol)

dummy_Pol$DayOfWeek <- as.character(dummy_Pol$DayOfWeek)  

gino = list("1","2", "3", "4" , "5")
pino= list("6","7")

dummy_Pol %>% mutate(DayOfWeek = if_else(DayOfWeek %in% gino, "0", DayOfWeek)) -> dummy_Pol

dummy_Pol %>% mutate(DayOfWeek = if_else(DayOfWeek %in% pino, "1", DayOfWeek)) -> dummy_Pol


dummy_Pol$Date <- NULL
dummy_Pol$DayYear <- NULL


dummy_ts_Pol<-  ts(coredata(dummy_Pol), freq = 8760, start = c(2012,1,1),  end = c(2015,8760))

prova_ts2<-cbind(prova_ts2, dummy_ts_Pol)
################################################

colnames(prova_ts2)[1] <- "PrezzoScandi"
colnames(prova_ts2)[2] <- "ConsumiScandi"
colnames(prova_ts2)[3] <- "GradiScandi"
colnames(prova_ts2)[4] <- "ConsumipercScandi"
colnames(prova_ts2)[5] <- "PrezzopercScandi"

colnames(prova_ts2)[6] <- "PrezzoPol"
colnames(prova_ts2)[7] <- "ConsumiPol"
colnames(prova_ts2)[8] <- "ConsumipercPol"
colnames(prova_ts2)[9] <- "PrezzopercPol"
colnames(prova_ts2)[10] <- "GradiPol"

colnames(prova_ts2)[11] <- "PrezzoUK"
colnames(prova_ts2)[12] <- "ConsumiUK"
colnames(prova_ts2)[13] <- "GradiUK"
colnames(prova_ts2)[14] <- "ConsumipercUK"
colnames(prova_ts2)[15] <- "PrezzopercUK"
################################################################################


colnames(prova_ts2)[16] <- "BankHolidayScandi"
colnames(prova_ts2)[17] <- "EndYearScandi"
colnames(prova_ts2)[18] <- "DayOfWeekScandi"
colnames(prova_ts2)[19] <- "DayOffScandi"


#########################
colnames(prova_ts2)[20] <- "BankHolidayUK"
colnames(prova_ts2)[21] <- "EndYearUK"
colnames(prova_ts2)[22] <- "DayOfWeekUK"
colnames(prova_ts2)[23] <- "DayOffUK"

########################################

colnames(prova_ts2)[24] <- "BankHolidayPol"
colnames(prova_ts2)[25] <- "EndYearPol"
colnames(prova_ts2)[26] <- "DayOfWeekPol"
colnames(prova_ts2)[27] <- "DayOffPol"
####################################################################################



write.csv(prova_ts2, "C:\\Users\\vizzi\\PROG_DSLAB_GITHUB\\Progetto_DSLAB\\DATASET SERIE STORICHE\\SerieStorica3NazioniDummy.csv")



##########################
##########################################################################
###########################################################
library(forecast)

autoplot(prova_ts2[,c("ConsumiScandi","ConsumiUK", "ConsumiPol", "ConsumiITA")])


autoplot(prova_ts2[,c("PrezzoScandi","PrezzoUK", "PrezzoPol")])




##################################################################################

# -- R CODE
# mod1 <- lm(I(log(peso))~I(log(bicipite)),d)

d<-prova_ts2
#-- R CODE
mod1 <- lm(PrezzoScandi~ConsumiScandi,d)
summary(mod1)

mod2<- lm(ConsumiScandi~PrezzoScandi+ GradiScandi,d)
summary(mod2)

mod3<- lm(ConsumiScandi~PrezzoScandi+ GradiScandi+DayOfWeekScandi,d)
summary(mod3)


mod4<- lm(ConsumiScandi~PrezzoScandi+ GradiScandi+DayOfWeekScandi+ BankHolidayScandi,d)
summary(mod4)


mod5<- lm(ConsumiScandi~PrezzoScandi+ GradiScandi+DayOfWeekScandi+ BankHolidayScandi+EndYearScandi,d)
summary(mod5)

mod6<- lm(I(log(ConsumiScandi))~PrezzoScandi+ GradiScandi+DayOfWeekScandi+ BankHolidayScandi+EndYearScandi,d)
summary(mod6)

# mod3 <- lm(I(log(peso))~bicipite,d)


mod7<- lm(ConsumiScandi~I(log(PrezzoScandi))+ GradiScandi+DayOfWeekScandi+ BankHolidayScandi+EndYearScandi,d)
summary(mod7)


###################################################################################
###############################################################################
##############################################################
###########################################
library(zoo)

df_ita <- read.csv.zoo("C:\\Users\\vizzi\\PROG_DSLAB_GITHUB\\Progetto_DSLAB\\DATASET SERIE STORICHE\\ita_finish_totale_prova_temp.csv")

df_ita<-  ts(coredata(df_ita), freq = 8760, start = c(2012,1,1),  end = c(2015,8760))


prova_ts2<-cbind(prova_ts2, df_ita)

###########################################

colnames(prova_ts2)[1] <- "PrezzoScandi"
colnames(prova_ts2)[2] <- "ConsumiScandi"
colnames(prova_ts2)[3] <- "GradiScandi"
colnames(prova_ts2)[4] <- "ConsumipercScandi"
colnames(prova_ts2)[5] <- "PrezzopercScandi"

colnames(prova_ts2)[6] <- "PrezzoPol"
colnames(prova_ts2)[7] <- "ConsumiPol"
colnames(prova_ts2)[8] <- "ConsumipercPol"
colnames(prova_ts2)[9] <- "PrezzopercPol"
colnames(prova_ts2)[10] <- "GradiPol"

colnames(prova_ts2)[11] <- "PrezzoUK"
colnames(prova_ts2)[12] <- "ConsumiUK"
colnames(prova_ts2)[13] <- "GradiUK"
colnames(prova_ts2)[14] <- "ConsumipercUK"
colnames(prova_ts2)[15] <- "PrezzopercUK"
################################################################################


colnames(prova_ts2)[16] <- "BankHolidayScandi"
colnames(prova_ts2)[17] <- "EndYearScandi"
colnames(prova_ts2)[18] <- "DayOfWeekScandi"
colnames(prova_ts2)[19] <- "DayOffScandi"


#########################
colnames(prova_ts2)[20] <- "BankHolidayUK"
colnames(prova_ts2)[21] <- "EndYearUK"
colnames(prova_ts2)[22] <- "DayOfWeekUK"
colnames(prova_ts2)[23] <- "DayOffUK"

########################################

colnames(prova_ts2)[24] <- "BankHolidayPol"
colnames(prova_ts2)[25] <- "EndYearPol"
colnames(prova_ts2)[26] <- "DayOfWeekPol"
colnames(prova_ts2)[27] <- "DayOffPol"
####################################################################################

colnames(prova_ts2)[28] <- "PrezzoITA"
colnames(prova_ts2)[29] <- "ConsumiITA"
colnames(prova_ts2)[30] <- "GradiITA"
colnames(prova_ts2)[31] <- "ConsumipercITA"
colnames(prova_ts2)[32] <- "PrezzopercITA"
################################################################################




colnames(prova_ts2)[33] <- "BankHolidayITA"
colnames(prova_ts2)[34] <- "EndYearITA"
colnames(prova_ts2)[37] <- "DayOfWeekITA"
colnames(prova_ts2)[38] <- "DayOffITA"
################################################################################


colnames(prova_ts2)[35] <- "AugustBorderITA"
colnames(prova_ts2)[36] <- "AugustCenterITA"


write.csv(prova_ts2, "C:\\Users\\vizzi\\PROG_DSLAB_GITHUB\\Progetto_DSLAB\\DATASET SERIE STORICHE\\Completissimo.csv")


#################################################

x <- cbind( Prezzi=prova_ts2[,"PrezzoScandi"],
            Gradi=prova_ts2[,"GradiScandi"])


y <- prova_ts2[,"ConsumiScandi"]

# NO DIFFED
fit1 <- auto.arima(y=y, xreg=NULL, seasonal=FALSE)
summary(fit1)

autoplot(forecast(fit1))
##################################################
############################
##########################################################################



########################################DA QUA#################################################
#########################################################################################
library(zoo)
df_comp <- read.csv.zoo("C:\\Users\\vizzi\\PROG_DSLAB_GITHUB\\Progetto_DSLAB\\DATASET SERIE STORICHE\\Completissimo.csv")

df_comp<-  ts(coredata(df_comp), freq = 8760, start = c(2012,1,1),  end = c(2015,8760))

autoplot(df_comp[,c("ConsumiScandi","ConsumiUK", "ConsumiPol", "ConsumiITA")])


autoplot(df_comp[,c("PrezzoScandi","PrezzoUK", "PrezzoPol", "PrezzoITA")])


##################################################################

# OKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKK


#(fit <- auto.arima(usconsumption[,1],
 #                  xreg=usconsumption[,2]))

x <- cbind( Prezzi=df_comp[,"PrezzoScandi"],
            Gradi=df_comp[,"GradiScandi"])


y <- df_comp[,"ConsumiScandi"]

# NO DIFFED
fit1 <- auto.arima(y=y, xreg=NULL, seasonal=FALSE)
summary(fit1)

autoplot(forecast(fit1))




##################################


freq<- outer(1:nrow(df_comp), 1:6) *2* pi / 12
cs <- cos(freq)
sn <- sin(freq)

X <- cbind(cs, sn[, 1:5])
colnames(X) <- c(paste0("cos", 1:6), paste0("sin", 1:5))

matplot(cs[1:12, ], type = "l", main = "Coseni")



library(forecast)

ly <- (AirPassengers)

autoplot(ly)

###############################################################
library(ggplot2)
df_comp[,c("PrezzoScandi")] %>% decompose(type="multiplicative") %>%
  autoplot() + xlab("Year") +
  ggtitle("Classical multiplicative decomposition")

#yconsumi %>% decompose(type="multiplicative") %>%
 # autoplot() + xlab("ID") +
  # ggtitle("Classical multiplicative decomposition")
#############################################################
################NO

fit3 <- auto.arima(df_comp[,c("PrezzoScandi")], seasonal=FALSE, lambda=0,
                  xreg=fourier(df_comp[,c("PrezzoScandi")], K=c(10,10)))
fit3 %>%
  forecast(xreg=fourier(df_comp[,c("PrezzoScandi")], K=c(10,10), h=2*169)) %>%
  autoplot(include=5*169) +
  ylab("Call volume") + xlab("Weeks")



df_comp[,c("PrezzoScandi")]%>% mstl() %>%
  autoplot() + xlab("Week")


df_comp[,c("PrezzoScandi")]%>%  stlf(h=24) %>% window(start=c(2015,12))%>%
  autoplot() + xlab("Year")


df_comp[,c("PrezzoScandi")]%>%  stlf(h=504) -> h
# autoplot(window(h,start=c(2015,12)))




autoplot( window(df_comp[,c("PrezzoScandi")],start=c(2015,12))) + 
  autolayer(h, PI=TRUE, series="STLF")



###########################################################
# Prova forecast
df_comp[,c("PrezzoScandi")]%>%  forecast(h=504) -> z




autoplot( window(df_comp[,c("PrezzoScandi")],start=c(2015,12))) + 
autolayer(z, PI=TRUE, series="Forecast")

autoplot( window(df_comp[,c("PrezzoScandi")],start=c(2015,12))) + 
  autolayer(h, PI=TRUE, series="STLF")

#########################################
#### Snaive

df_comp[,c("PrezzoScandi")]%>%  snaive(h=504) -> z1

autoplot( window(df_comp[,c("PrezzoScandi")],start=c(2015,12))) + 
  autolayer(z1, PI=TRUE, series="Snaive")



#########################################

df_comp[,c("PrezzoScandi")]%>%  meanf(h=504) -> z2

autoplot( window(df_comp[,c("PrezzoScandi")],start=c(2015,12))) + 
  autolayer(z2, PI=TRUE, series="Meanf")


##################################################
df_comp[,c("PrezzoScandi")]%>%  holt(h=504) -> z3

autoplot( window(df_comp[,c("PrezzoScandi")],start=c(2015,12))) + 
  autolayer(z3, PI=TRUE, series="Holt")


############################################


#################Croston     Non parteeeeeeeeeeeee
df_comp[,c("PrezzoScandi")]%>%  croston(h=504) -> z4

autoplot( window(df_comp[,c("PrezzoScandi")],start=c(2015,12))) + 
  autolayer(z4, PI=TRUE, series="Croston")



##########################################

fitzio<- auto.arima(df_comp[,c("PrezzoScandi")], seasonal=FALSE, lambda=0,
                  xreg=fourier(df_comp[,c("PrezzoScandi")], K=c(0.01,0.1)))

fitzio %>%
  forecast(xreg=fourier(df_comp[,c("PrezzoScandi")], K=3, h=24)) %>%
  autoplot(include=7*24) +
  ylab("Call volume") + xlab("Weeks")

summary(fitzio)


####################################################################################
library(forecast)

df_comp2ms<- read.csv.zoo("C:\\Users\\vizzi\\PROG_DSLAB_GITHUB\\Progetto_DSLAB\\DATASET SERIE STORICHE\\Completissimo.csv")

ts <-  ts(coredata(df_comp2ms), freq = 8760, start = c(2012,1,1),  end = c(2015,8760))
# ts<- msts(df_comp2ms, c(7*24,365*24)) # multiseasonal ts

y <- msts(ts, c(7*24,365*24)) # multiseasonal ts

fit <- auto.arima(y[,"PrezzoScandi"], seasonal=F, xreg=fourier(y[,"PrezzoScandi"], K=c(2,2)))


fit_f <- forecast(fit, xreg= fourier(y[,"PrezzoScandi"], K=c(2,2), h= 24*7), 24*7)
plot(fit_f)

autoplot( window(df_comp[,c("PrezzoScandi")],start=c(2015,12))) + 
  autolayer(fit_f, PI=TRUE, series="zio")
summary(fit)


