#install.packages("stringi")
#install.packages("ggfortify")
####################################
###### LIBRERIE ####################
####################################
library(dplyr)
library(stringi)
library(tidyr)
library(zoo)
library(ggplot2)
library(ggfortify)
library(lubridate)
library(xts)
library(forecast)
####################################
###### IMPORTO I DATASET ###########
####################################
dir_df1 <- "/Volumes/HDD_Ale2/ElData/PL_1215_Price_byhour.csv"
dir_df2 <- "/Volumes/HDD_Ale2/ElData/PL_1215_Load_byhour.csv"

df_prezzi <- read.csv(dir_df1)
df_consumi <- read.csv(dir_df2)

####################################
###### PRE-PROCESSING ##############
####################################

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

totale <- left_join(df_prezzi, df_consumi, by = c("anno" = "anno", "mese" = "mese", "giorno"="giorno", "ora"="ora"))
totale <- totale[, -c(9:10)]

##########################################
########### NAN prezzi ###################
##########################################

summary(totale)
new_DF<-subset(totale,is.na(totale$prezzo))

# dobbiamo sistemare na.

# 14/03/2013 è un giovedì e va sistemato. provo a vedere il giorno successivo e precedente
totale %>% filter(anno=="2013",
                     mese=="03",
                     giorno==c("15")) %>% 
          select(anno,mese,giorno,ora,prezzo) -> fixna15

totale %>% filter(anno=="2013",
                  mese=="03",
                  giorno==c("13")) %>% 
  select(anno,mese,giorno,ora,prezzo) -> fixna13

fixna15 <- left_join(fixna15, fixna13, by = c("anno" = "anno", "mese" = "mese","ora"="ora"))

fixna15$giorno.y <- NULL
fixna15$giorno.x <- NULL
names(fixna15)[4] <- "prezzo15"
names(fixna15)[5] <- "prezzo13"

fixna15$prezzi14 <- apply(fixna15[,c(4,5)], 1, mean)

fixna15$giorno14 <- "14"

totale <- left_join(totale, fixna15, by = c("anno" = "anno", "mese" = "mese", "giorno"="giorno14", "ora"="ora"))
str(totale)
?if_else
totale %>% mutate(prezzo = if_else((giorno =="14") & (anno == "2013") & (mese == "03"),prezzi14,prezzo)) -> totale
new_DF<-subset(totale,is.na(totale$prezzo))

# 21/03/2013 provo a vedere il giorno successivo e precedente
totale %>% filter(anno=="2013",
                  mese=="03",
                  giorno==c("20")) %>% 
  select(anno,mese,giorno,ora,prezzo) -> fixna15

totale %>% filter(anno=="2013",
                  mese=="03",
                  giorno==c("22")) %>% 
  select(anno,mese,giorno,ora,prezzo) -> fixna13

fixna15 <- left_join(fixna15, fixna13, by = c("anno" = "anno", "mese" = "mese","ora"="ora"))

fixna15$giorno.y <- NULL
fixna15$giorno.x <- NULL
names(fixna15)[4] <- "prezzo20"
names(fixna15)[5] <- "prezzo22"

fixna15$prezzi21 <- apply(fixna15[,c(4,5)], 1, mean)

fixna15$giorno21 <- "21"

totale <- left_join(totale, fixna15, by = c("anno" = "anno", "mese" = "mese", "giorno"="giorno21", "ora"="ora"))

totale %>% mutate(prezzo = if_else((giorno =="21") & (anno == "2013") & (mese == "03"),prezzi21,prezzo)) -> totale

totale <- totale[, -c(9:15)]

new_DF <- subset(totale,is.na(totale$prezzo))

# 12/06/2013 mancano le 24
totale %>% filter(anno=="2013",
                  mese=="06",
                  giorno==c("11"),
                  ora=="24:00:00") %>% 
  select(anno,mese,giorno,ora,prezzo) -> fixna15

totale %>% filter(anno=="2013",
                  mese=="06",
                  giorno==c("14"),
                  ora=="24:00:00") %>% 
  select(anno,mese,giorno,ora,prezzo) -> fixna13

fixna15 <- left_join(fixna15, fixna13, by = c("anno" = "anno", "mese" = "mese","ora"="ora"))

fixna15$giorno.y <- NULL
fixna15$giorno.x <- NULL
names(fixna15)[4] <- "prezzo11"
names(fixna15)[5] <- "prezzo14"

fixna15$prezzi12 <- apply(fixna15[,c(4,5)], 1, mean)

fixna15$giorno12 <- "12"
fixna15
totale <- left_join(totale, fixna15, by = c("anno" = "anno", "mese" = "mese", "giorno"="giorno12", "ora"="ora"))

totale %>% mutate(prezzo = if_else((giorno =="12") & (anno == "2013") & (mese == "06") & (ora=="24:00:00"), prezzi12, prezzo)) -> totale
#totale <- totale[, -c(9:15)]

new_DF <- subset(totale,is.na(totale$prezzo))

# 13/06/2013 tutta la giornata
totale %>% filter(anno=="2013",
                  mese=="06",
                  giorno==c("12")) %>% 
  select(anno,mese,giorno,ora,prezzo) -> fixna15

totale %>% filter(anno=="2013",
                  mese=="06",
                  giorno==c("14")) %>% 
  select(anno,mese,giorno,ora,prezzo) -> fixna13

fixna15 <- left_join(fixna15, fixna13, by = c("anno" = "anno", "mese" = "mese","ora"="ora"))

fixna15$giorno.y <- NULL
fixna15$giorno.x <- NULL
names(fixna15)[4] <- "prezzo12"
names(fixna15)[5] <- "prezzo14"

fixna15$prezzi13 <- apply(fixna15[,c(4,5)], 1, mean)

fixna15$giorno13 <- "13"

totale <- left_join(totale, fixna15, by = c("anno" = "anno", "mese" = "mese", "giorno"="giorno13", "ora"="ora"))

totale %>% mutate(prezzo = if_else((giorno =="13") & (anno == "2013") & (mese == "06"), prezzi13, prezzo)) -> totale
totale <- totale[, -c(9:15)]

new_DF <- subset(totale,is.na(totale$prezzo))

# 23/01/2014 tutta la giornata vedere perchè siamo in festività
totale %>% filter(anno=="2014",
                  mese=="01",
                  giorno==c("22")) %>% 
  select(anno,mese,giorno,ora,prezzo) -> fixna15

totale %>% filter(anno=="2014",
                  mese=="01",
                  giorno==c("24")) %>% 
  select(anno,mese,giorno,ora,prezzo) -> fixna13

fixna15 <- left_join(fixna15, fixna13, by = c("anno" = "anno", "mese" = "mese","ora"="ora"))

fixna15$giorno.y <- NULL
fixna15$giorno.x <- NULL
names(fixna15)[4] <- "prezzo22"
names(fixna15)[5] <- "prezzo24"

fixna15$prezzi23 <- apply(fixna15[,c(4,5)], 1, mean)

fixna15$giorno23 <- "23"

totale <- left_join(totale, fixna15, by = c("anno" = "anno", "mese" = "mese", "giorno"="giorno23", "ora"="ora"))

totale %>% mutate(prezzo = if_else((giorno =="23") & (anno == "2014") & (mese == "01"), prezzi23, prezzo)) -> totale
#totale <- totale[, -c(9:15)]

new_DF <- subset(totale,is.na(totale$prezzo))

# 15/07/2014 mancano le 24
totale %>% filter(anno=="2014",
                  mese=="07",
                  giorno==c("14"),
                  ora=="24:00:00") %>% 
  select(anno,mese,giorno,ora,prezzo) -> fixna15

totale %>% filter(anno=="2014",
                  mese=="07",
                  giorno==c("17"),
                  ora=="24:00:00") %>% 
  select(anno,mese,giorno,ora,prezzo) -> fixna13

fixna15 <- left_join(fixna15, fixna13, by = c("anno" = "anno", "mese" = "mese","ora"="ora"))

fixna15$giorno.y <- NULL
fixna15$giorno.x <- NULL
names(fixna15)[4] <- "prezzo14"
names(fixna15)[5] <- "prezzo17"

fixna15$prezzi15 <- apply(fixna15[,c(4,5)], 1, mean)

fixna15$giorno15 <- "15"
fixna15
totale <- left_join(totale, fixna15, by = c("anno" = "anno", "mese" = "mese", "giorno"="giorno15", "ora"="ora"))

totale %>% mutate(prezzo = if_else((giorno =="15") & (anno == "2014") & (mese == "07") & (ora=="24:00:00"), prezzi15, prezzo)) -> totale
totale <- totale[, -c(9:15)]

new_DF <- subset(totale,is.na(totale$prezzo))

# 16/07/2014 manca la giornata intera
totale %>% filter(anno=="2014",
                  mese=="07",
                  giorno==c("15")) %>% 
  select(anno,mese,giorno,ora,prezzo) -> fixna15

totale %>% filter(anno=="2014",
                  mese=="07",
                  giorno==c("17")) %>% 
  select(anno,mese,giorno,ora,prezzo) -> fixna13

fixna15 <- left_join(fixna15, fixna13, by = c("anno" = "anno", "mese" = "mese","ora"="ora"))

fixna15$giorno.y <- NULL
fixna15$giorno.x <- NULL
names(fixna15)[4] <- "prezzo15"
names(fixna15)[5] <- "prezzo17"

fixna15$prezzi16 <- apply(fixna15[,c(4,5)], 1, mean)

fixna15$giorno16 <- "16"
fixna15
totale <- left_join(totale, fixna15, by = c("anno" = "anno", "mese" = "mese", "giorno"="giorno16", "ora"="ora"))

totale %>% mutate(prezzo = if_else((giorno =="16") & (anno == "2014") & (mese == "07"), prezzi16, prezzo)) -> totale

new_DF <- subset(totale,is.na(totale$prezzo))
totale <- totale[, -c(9:11)]

#Sistemato tutti i nan
summary(totale)

########################################
##### SISTEMO DATETIME NA ##############
########################################

new_DF<-subset(totale,is.na(totale$dateTime.x))

# ci sono 4 date da sistemare inizio dalla prima
# 2012   03     25 02:00:00
# 2013   03     31 02:00:00
# 2014   03     30 02:00:00
# 2015   03     29 02:00:00
totale %>% mutate(dateTime.x = if_else((giorno =="25") & (anno == "2012") & (mese == "03") & (ora=="02:00:00"), as.POSIXct("2012-03-25 02:00:00"), dateTime.x)) -> totale 
totale %>% mutate(dateTime.x = if_else((giorno =="31") & (anno == "2013") & (mese == "03") & (ora=="02:00:00"), as.POSIXct("2012-03-25 02:00:00"), dateTime.x)) -> totale 
totale %>% mutate(dateTime.x = if_else((giorno =="30") & (anno == "2014") & (mese == "03") & (ora=="02:00:00"), as.POSIXct("2012-03-25 02:00:00"), dateTime.x)) -> totale 
totale %>% mutate(dateTime.x = if_else((giorno =="29") & (anno == "2015") & (mese == "03") & (ora=="02:00:00"), as.POSIXct("2012-03-25 02:00:00"), dateTime.x)) -> totale 

summary(totale) 

########################################
##### FINITI NA ########################
########################################

########################################
######## TEMPERATURE AGGIUNTE ##########
########################################

temperature <- read.csv("/Users/alessandropontini/Desktop/tot_temperature.csv")

temperature %>% filter(Anno=="2012") -> temperature_2012
totale %>% filter(anno=="2012") -> totale_2012
v = list("1", "2", "3", "4", "5", "6", "7", "8", "9")


temperature_2012$Giorno <-  as.character(temperature_2012$Giorno)
temperature_2012$Mese <-  as.character(temperature_2012$Mese)

temperature_2012 %>% mutate(Giorno = if_else(Giorno %in% v, paste0("0",Giorno), Giorno)) -> temperature_2012
temperature_2012 %>% mutate(Mese = if_else(Mese %in% v, paste0("0",Mese), Mese)) -> temperature_2012

temperature_2012$OraFix <-  as.character(temperature_2012$OraFix)
temperature_2012 %>% mutate(OraFix = paste0(OraFix,":00")) -> temperature_2012


totale_2012 <- left_join(totale_2012, temperature_2012, by = c("giorno" = "Giorno", "mese" = "Mese", "ora" = "OraFix"))
totale_2012 <- totale_2012[-c(9)]
tail(temperature_2012)

#########################################
##### fix na ############################
######################################### 

# gennaio
totale_2012 %>% filter(mese=='01') -> gennaio_2012
mediana <- median(gennaio_2012$GradoFix,na.rm = TRUE )
totale_2012 %>% filter(mese=='01') %>% replace_na(list(GradoFix=mediana)) -> gennaio_2012

# febbraio 
totale_2012 %>% filter(mese=='02') -> febbraio_2012
mediana <- median(febbraio_2012$GradoFix,na.rm = TRUE )
totale_2012 %>% filter(mese=='02') %>% replace_na(list(GradoFix=mediana)) -> febbraio_2012

# marzo 
totale_2012 %>% filter(mese=='03') -> marzo_2012
mediana <- median(marzo_2012$GradoFix,na.rm = TRUE )
totale_2012 %>% filter(mese=='03') %>% replace_na(list(GradoFix=mediana)) -> marzo_2012

# aprile 
totale_2012 %>% filter(mese=='04') -> aprile_2012
mediana <- median(aprile_2012$GradoFix,na.rm = TRUE )
totale_2012 %>% filter(mese=='04') %>% replace_na(list(GradoFix=mediana)) -> aprile_2012

# maggio 
totale_2012 %>% filter(mese=='05') -> maggio_2012
mediana <- median(maggio_2012$GradoFix,na.rm = TRUE )
totale_2012 %>% filter(mese=='05') %>% replace_na(list(GradoFix=mediana)) -> maggio_2012

# giugno 
totale_2012 %>% filter(mese=='06') -> giugno_2012
mediana <- median(giugno_2012$GradoFix,na.rm = TRUE )
totale_2012 %>% filter(mese=='06') %>% replace_na(list(GradoFix=mediana)) -> giugno_2012

# luglio 
totale_2012 %>% filter(mese=='07') -> luglio_2012
mediana <- median(luglio_2012$GradoFix,na.rm = TRUE )
totale_2012 %>% filter(mese=='07') %>% replace_na(list(GradoFix=mediana)) -> luglio_2012

# agosto 
totale_2012 %>% filter(mese=='08') -> agosto_2012
mediana <- median(agosto_2012$GradoFix,na.rm = TRUE )
totale_2012 %>% filter(mese=='08') %>% replace_na(list(GradoFix=mediana)) -> agosto_2012

# settembre 
totale_2012 %>% filter(mese=='09') -> settembre_2012
mediana <- median(settembre_2012$GradoFix,na.rm = TRUE )
totale_2012 %>% filter(mese=='09') %>% replace_na(list(GradoFix=mediana)) -> settembre_2012

# ottobre 
totale_2012 %>% filter(mese=='10') -> ottobre_2012
mediana <- median(ottobre_2012$GradoFix,na.rm = TRUE )
totale_2012 %>% filter(mese=='10') %>% replace_na(list(GradoFix=mediana)) -> ottobre_2012

# novembre 
totale_2012 %>% filter(mese=='11') -> novembre_2012
mediana <- median(novembre_2012$GradoFix,na.rm = TRUE )
totale_2012 %>% filter(mese=='11') %>% replace_na(list(GradoFix=mediana)) -> novembre_2012

# dicembre 
totale_2012 %>% filter(mese=='12') -> dicembre_2012
mediana <- median(dicembre_2012$GradoFix,na.rm = TRUE )
totale_2012 %>% filter(mese=='12') %>% replace_na(list(GradoFix=mediana)) -> dicembre_2012

totale_2012<- do.call("rbind", list(gennaio_2012, febbraio_2012, marzo_2012, aprile_2012, maggio_2012, giugno_2012,luglio_2012, agosto_2012, settembre_2012, ottobre_2012, novembre_2012, dicembre_2012))
nrow(totale_2012)

# 2013
temperature %>% filter(Anno=="2013") -> temperature_2013
totale %>% filter(anno=="2013") -> totale_2013

nrow(totale_2013)
temperature_2013$Giorno <-  as.character(temperature_2013$Giorno)
temperature_2013$Mese <-  as.character(temperature_2013$Mese)

temperature_2013 %>% mutate(Giorno = if_else(Giorno %in% v, paste0("0",Giorno), Giorno)) -> temperature_2013
temperature_2013 %>% mutate(Mese = if_else(Mese %in% v, paste0("0",Mese), Mese)) -> temperature_2013

temperature_2013$OraFix <-  as.character(temperature_2013$OraFix)
temperature_2013 %>% mutate(OraFix = paste0(OraFix,":00")) -> temperature_2013


totale_2013 <- left_join(totale_2013, temperature_2013, by = c("giorno" = "Giorno", "mese" = "Mese", "ora" = "OraFix"))
nrow(totale_2013)

totale_2013 <- unique(totale_2013_prova)

# gennaio
totale_2013 %>% filter(mese=='01') -> gennaio_2013
mediana <- median(gennaio_2013$GradoFix,na.rm = TRUE )
totale_2013 %>% filter(mese=='01') %>% replace_na(list(GradoFix=mediana)) -> gennaio_2013

# febbraio 
totale_2013 %>% filter(mese=='02') -> febbraio_2013
mediana <- median(febbraio_2013$GradoFix,na.rm = TRUE )
totale_2013 %>% filter(mese=='02') %>% replace_na(list(GradoFix=mediana)) -> febbraio_2013

# marzo 
totale_2013 %>% filter(mese=='03') -> marzo_2013
mediana <- median(marzo_2013$GradoFix,na.rm = TRUE )
totale_2013 %>% filter(mese=='03') %>% replace_na(list(GradoFix=mediana)) -> marzo_2013

# aprile 
totale_2013 %>% filter(mese=='04') -> aprile_2013
mediana <- median(aprile_2013$GradoFix,na.rm = TRUE )
totale_2013 %>% filter(mese=='04') %>% replace_na(list(GradoFix=mediana)) -> aprile_2013

# maggio 
totale_2013 %>% filter(mese=='05') -> maggio_2013
mediana <- median(maggio_2013$GradoFix,na.rm = TRUE )
totale_2013 %>% filter(mese=='05') %>% replace_na(list(GradoFix=mediana)) -> maggio_2013

# giugno 
totale_2013 %>% filter(mese=='06') -> giugno_2013
mediana <- median(giugno_2013$GradoFix,na.rm = TRUE )
totale_2013 %>% filter(mese=='06') %>% replace_na(list(GradoFix=mediana)) -> giugno_2013

# luglio 
totale_2013 %>% filter(mese=='07') -> luglio_2013
mediana <- median(luglio_2013$GradoFix,na.rm = TRUE )
totale_2013 %>% filter(mese=='07') %>% replace_na(list(GradoFix=mediana)) -> luglio_2013

# agosto 
totale_2013 %>% filter(mese=='08') -> agosto_2013
mediana <- median(agosto_2013$GradoFix,na.rm = TRUE )
totale_2013 %>% filter(mese=='08') %>% replace_na(list(GradoFix=mediana)) -> agosto_2013

# settembre 
totale_2013 %>% filter(mese=='09') -> settembre_2013
mediana <- median(settembre_2013$GradoFix,na.rm = TRUE )
totale_2013 %>% filter(mese=='09') %>% replace_na(list(GradoFix=mediana)) -> settembre_2013

# ottobre 
totale_2013 %>% filter(mese=='10') -> ottobre_2013
mediana <- median(ottobre_2013$GradoFix,na.rm = TRUE )
totale_2013 %>% filter(mese=='10') %>% replace_na(list(GradoFix=mediana)) -> ottobre_2013

# novembre 
totale_2013 %>% filter(mese=='11') -> novembre_2013
mediana <- median(novembre_2013$GradoFix,na.rm = TRUE )
totale_2013 %>% filter(mese=='11') %>% replace_na(list(GradoFix=mediana)) -> novembre_2013

# dicembre 
totale_2013 %>% filter(mese=='12') -> dicembre_2013
mediana <- median(dicembre_2013$GradoFix,na.rm = TRUE )
totale_2013 %>% filter(mese=='12') %>% replace_na(list(GradoFix=mediana)) -> dicembre_2013

totale_2013<- do.call("rbind", list(gennaio_2013, febbraio_2013, marzo_2013, aprile_2013, maggio_2013, giugno_2013,luglio_2013, agosto_2013, settembre_2013, ottobre_2013, novembre_2013, dicembre_2013))

totale_2013 <- totale_2013[-c(9)]

nrow(totale_2013)

# 2014
temperature %>% filter(Anno=="2014") -> temperature_2014
totale %>% filter(anno=="2014") -> totale_2014

nrow(totale_2014)
temperature_2014$Giorno <-  as.character(temperature_2014$Giorno)
temperature_2014$Mese <-  as.character(temperature_2014$Mese)

temperature_2014 %>% mutate(Giorno = if_else(Giorno %in% v, paste0("0",Giorno), Giorno)) -> temperature_2014
temperature_2014 %>% mutate(Mese = if_else(Mese %in% v, paste0("0",Mese), Mese)) -> temperature_2014

temperature_2014$OraFix <-  as.character(temperature_2014$OraFix)
temperature_2014 %>% mutate(OraFix = paste0(OraFix,":00")) -> temperature_2014


totale_2014 <- left_join(totale_2014, temperature_2014, by = c("giorno" = "Giorno", "mese" = "Mese", "ora" = "OraFix"))
nrow(totale_2014)

totale_2014 <- unique(totale_2014)

# gennaio
totale_2014 %>% filter(mese=='01') -> gennaio_2014
mediana <- median(gennaio_2014$GradoFix,na.rm = TRUE )
totale_2014 %>% filter(mese=='01') %>% replace_na(list(GradoFix=mediana)) -> gennaio_2014

# febbraio 
totale_2014 %>% filter(mese=='02') -> febbraio_2014
mediana <- median(febbraio_2014$GradoFix,na.rm = TRUE )
totale_2014 %>% filter(mese=='02') %>% replace_na(list(GradoFix=mediana)) -> febbraio_2014

# marzo 
totale_2014 %>% filter(mese=='03') -> marzo_2014
mediana <- median(marzo_2014$GradoFix,na.rm = TRUE )
totale_2014 %>% filter(mese=='03') %>% replace_na(list(GradoFix=mediana)) -> marzo_2014

# aprile 
totale_2014 %>% filter(mese=='04') -> aprile_2014
mediana <- median(aprile_2014$GradoFix,na.rm = TRUE )
totale_2014 %>% filter(mese=='04') %>% replace_na(list(GradoFix=mediana)) -> aprile_2014

# maggio 
totale_2014 %>% filter(mese=='05') -> maggio_2014
mediana <- median(maggio_2014$GradoFix,na.rm = TRUE )
totale_2014 %>% filter(mese=='05') %>% replace_na(list(GradoFix=mediana)) -> maggio_2014

# giugno 
totale_2014 %>% filter(mese=='06') -> giugno_2014
mediana <- median(giugno_2014$GradoFix,na.rm = TRUE )
totale_2014 %>% filter(mese=='06') %>% replace_na(list(GradoFix=mediana)) -> giugno_2014

# luglio 
totale_2014 %>% filter(mese=='07') -> luglio_2014
mediana <- median(luglio_2014$GradoFix,na.rm = TRUE )
totale_2014 %>% filter(mese=='07') %>% replace_na(list(GradoFix=mediana)) -> luglio_2014

# agosto 
totale_2014 %>% filter(mese=='08') -> agosto_2014
mediana <- median(agosto_2014$GradoFix,na.rm = TRUE )
totale_2014 %>% filter(mese=='08') %>% replace_na(list(GradoFix=mediana)) -> agosto_2014

# settembre 
totale_2014 %>% filter(mese=='09') -> settembre_2014
mediana <- median(settembre_2014$GradoFix,na.rm = TRUE )
totale_2014 %>% filter(mese=='09') %>% replace_na(list(GradoFix=mediana)) -> settembre_2014

# ottobre 
totale_2014 %>% filter(mese=='10') -> ottobre_2014
mediana <- median(ottobre_2014$GradoFix,na.rm = TRUE )
totale_2014 %>% filter(mese=='10') %>% replace_na(list(GradoFix=mediana)) -> ottobre_2014

# novembre 
totale_2014 %>% filter(mese=='11') -> novembre_2014
mediana <- median(novembre_2014$GradoFix,na.rm = TRUE )
totale_2014 %>% filter(mese=='11') %>% replace_na(list(GradoFix=mediana)) -> novembre_2014

# dicembre 
totale_2014 %>% filter(mese=='12') -> dicembre_2014
mediana <- median(dicembre_2014$GradoFix,na.rm = TRUE )
totale_2014 %>% filter(mese=='12') %>% replace_na(list(GradoFix=mediana)) -> dicembre_2014

totale_2014<- do.call("rbind", list(gennaio_2014, febbraio_2014, marzo_2014, aprile_2014, maggio_2014, giugno_2014,luglio_2014, agosto_2014, settembre_2014, ottobre_2014, novembre_2014, dicembre_2014))

totale_2014 <- totale_2014[-c(9)]

nrow(totale_2014)

# 2015

temperature %>% filter(Anno=="2015") -> temperature_2015
totale %>% filter(anno=="2015") -> totale_2015

nrow(totale_2015)
temperature_2015$Giorno <-  as.character(temperature_2015$Giorno)
temperature_2015$Mese <-  as.character(temperature_2015$Mese)

temperature_2015 %>% mutate(Giorno = if_else(Giorno %in% v, paste0("0",Giorno), Giorno)) -> temperature_2015
temperature_2015 %>% mutate(Mese = if_else(Mese %in% v, paste0("0",Mese), Mese)) -> temperature_2015

temperature_2015$OraFix <-  as.character(temperature_2015$OraFix)
temperature_2015 %>% mutate(OraFix = paste0(OraFix,":00")) -> temperature_2015


totale_2015 <- left_join(totale_2015, temperature_2015, by = c("giorno" = "Giorno", "mese" = "Mese", "ora" = "OraFix"))
nrow(totale_2015)

totale_2015 <- unique(totale_2015)
tail(totale_2015)
# gennaio
totale_2015 %>% filter(mese=='01') -> gennaio_2015
mediana <- median(gennaio_2015$GradoFix,na.rm = TRUE )
totale_2015 %>% filter(mese=='01') %>% replace_na(list(GradoFix=mediana)) -> gennaio_2015

# febbraio 
totale_2015 %>% filter(mese=='02') -> febbraio_2015
mediana <- median(febbraio_2015$GradoFix,na.rm = TRUE )
totale_2015 %>% filter(mese=='02') %>% replace_na(list(GradoFix=mediana)) -> febbraio_2015

# marzo 
totale_2015 %>% filter(mese=='03') -> marzo_2015
mediana <- median(marzo_2015$GradoFix,na.rm = TRUE )
totale_2015 %>% filter(mese=='03') %>% replace_na(list(GradoFix=mediana)) -> marzo_2015

# aprile 
totale_2015 %>% filter(mese=='04') -> aprile_2015
mediana <- median(aprile_2015$GradoFix,na.rm = TRUE )
totale_2015 %>% filter(mese=='04') %>% replace_na(list(GradoFix=mediana)) -> aprile_2015

# maggio 
totale_2015 %>% filter(mese=='05') -> maggio_2015
mediana <- median(maggio_2015$GradoFix,na.rm = TRUE )
totale_2015 %>% filter(mese=='05') %>% replace_na(list(GradoFix=mediana)) -> maggio_2015

# giugno 
totale_2015 %>% filter(mese=='06') -> giugno_2015
mediana <- median(giugno_2015$GradoFix,na.rm = TRUE )
totale_2015 %>% filter(mese=='06') %>% replace_na(list(GradoFix=mediana)) -> giugno_2015

# luglio 
totale_2015 %>% filter(mese=='07') -> luglio_2015
mediana <- median(luglio_2015$GradoFix,na.rm = TRUE )
totale_2015 %>% filter(mese=='07') %>% replace_na(list(GradoFix=mediana)) -> luglio_2015

# agosto 
totale_2015 %>% filter(mese=='08') -> agosto_2015
mediana <- median(agosto_2015$GradoFix,na.rm = TRUE )
totale_2015 %>% filter(mese=='08') %>% replace_na(list(GradoFix=mediana)) -> agosto_2015

# settembre 
totale_2015 %>% filter(mese=='09') -> settembre_2015
mediana <- median(settembre_2015$GradoFix,na.rm = TRUE )
totale_2015 %>% filter(mese=='09') %>% replace_na(list(GradoFix=mediana)) -> settembre_2015

# ottobre 
totale_2015 %>% filter(mese=='10') -> ottobre_2015
mediana <- median(ottobre_2015$GradoFix,na.rm = TRUE )
totale_2015 %>% filter(mese=='10') %>% replace_na(list(GradoFix=mediana)) -> ottobre_2015

# novembre 
totale_2015 %>% filter(mese=='11') -> novembre_2015
mediana <- median(novembre_2015$GradoFix,na.rm = TRUE )
totale_2015 %>% filter(mese=='11') %>% replace_na(list(GradoFix=mediana)) -> novembre_2015

# dicembre 
totale_2015 %>% filter(mese=='12') -> dicembre_2015
mediana <- median(dicembre_2015$GradoFix,na.rm = TRUE )
totale_2015 %>% filter(mese=='12') %>% replace_na(list(GradoFix=mediana)) -> dicembre_2015

totale_2015<- do.call("rbind", list(gennaio_2015, febbraio_2015, marzo_2015, aprile_2015, maggio_2015, giugno_2015,luglio_2015, agosto_2015, settembre_2015, ottobre_2015, novembre_2015, dicembre_2015))

totale_2015 <- totale_2015[-c(9)]

nrow(totale_2015)

totale <- do.call("rbind", list(totale_2012,totale_2013, totale_2014, totale_2015))


########################################
######### CONSUMI ######################
########################################
consumits <- ts(totale$consumo, frequency = 8760, start = c(2012, 1), end=c(2015, 12))
autoplot(consumits)

########################################
############### PREZZI #################
########################################
prezzi2 <- ts(totale$prezzo, frequency = 8760, start = c(2012, 1), end=c(2015, 12))
autoplot(prezzi2)
datalist <- list(consumi = consumits, prezzi = prezzi2)

fit.consMR <- tslm(datalist$consumi ~ datalist$prezzi ,data=datalist)
summary(fit.consMR)
uschange
summary(totale)
firstHour <- 24*(as.Date("2016-01-01 00:00:00")-as.Date("2012-01-01 00:00:00"))
tt <- ts(totale$consumo,start=c(2012, 01:00:00 ),frequency=24*365)
autoplot(tt)

naive(consumits)
naive(prezzi2)
rwf(consumits , h=10, drift=TRUE)

# Plot some forecasts
autoplot(consumits) +
  autolayer(meanf(consumits, h=8760),
            series="Mean", PI=FALSE) +
  autolayer(naive(consumits, h=8760),
            series="Naïve", PI=FALSE) +
  autolayer(snaive(consumits, h=8760),
            series="Seasonal naïve", PI=FALSE) +
  ggtitle("Forecasts for Elettricity production") +
  xlab("Year") + ylab("Kwatt") +
  guides(colour=guide_legend(title="Forecast"))


(lambda <- BoxCox.lambda(consumits))

autoplot(BoxCox(consumits,lambda))

fc <- rwf(consumits, drift=TRUE, lambda=1, h=, level=80)
fc2 <- rwf(consumits, drift=TRUE, lambda=1, h=100, level=80,
           biasadj=TRUE)
autoplot(consumits) +
  autolayer(fc, series="Simple back transformation") +
  autolayer(fc2, series="Bias adjusted", PI=FALSE) +
  guides(colour=guide_legend(title="Forecast"))


autoplot(datalist$consumi) +
  ylab("% change") + xlab("Year")

#############################################
######### temp in serie storica #############
#############################################

gradi3 <- ts(totale$GradoFix, frequency = 8760, start = c(2012, 1),end=c(2015, 12))
autoplot(gradi3)
#############################################
######COME METTERE IN REGRESSIONE DATI#######
#############################################
tot <- cbind(consumits,prezzi2)
tot <- cbind(tot,gradi3)
tot

tot %>% as.data.frame() -> totdata

colnames(tot)[1] <- "Consumi"
colnames(tot)[2] <- "Prezzi"
colnames(tot)[3] <- "Gradi"

########################################
##PREPARATO LE VARIAZIONI PERCENTUALI###
########################################
percentuali <- function(x,y){
  
  risultato <- list()
  risultato[[1]] <- 0
  k <- x[1,y]
  
  for (i in 2:35064) {
    risultato[[i]] <- ((x[i,y]-k)*100)/k
    k <- x[i,y]
  }
  return(risultato)
}

consumiperc <- percentuali(totdata,1)
consumiperc <- data.frame(matrix(unlist(consumiperc), nrow=35064, byrow=T),stringsAsFactors=FALSE)

prezziperc <- percentuali(totdata,2)
prezziperc <- data.frame(matrix(unlist(prezziperc), nrow=35064, byrow=T),stringsAsFactors=FALSE)



consumitsperc <- ts(consumiperc, frequency = 8760, start = c(2012, 1),end=c(2015, 12))
prezzitsperc <- ts(prezziperc, frequency = 8760, start = c(2012, 1),end=c(2015, 12))


tot <- cbind(tot, consumitsperc)
tot <- cbind(tot, prezzitsperc)

colnames(tot)[1] <- "Consumi"
colnames(tot)[2] <- "Prezzi"
colnames(tot)[3] <- "Gradi"
colnames(tot)[4] <- "Consumiperc"
colnames(tot)[5] <- "Prezzoperc"

tot 
anyNA(totale)
consumoseasonal <- decompose(consumits)

plot(consumoseasonal)

consumoadjusted <- consumits - consumoseasonal$seasonal

plot(consumoadjusted)

consumoadjusted <- holt(consumoadjusted, h=24)

fit.consMR <- tslm(
  Consumi ~ Prezzi + Gradi,
  data=tot)
summary(fit.consMR)

acf(tot, lag.max=20)  

totsarima <- arima(consumits, order=c(0,1,1))

totsarima <-  Arima(consumits, order=c(0,1,1))
forecast <- forecast(totsarima, h=24)
plot(forecast(totsarima, h=24))
totale_2015

#-- Extract Training Data, Fit the Wrong Model, and Forecast
tot
yt<-window(consumits,end=2014.023)

yfit<-Arima(yt,order=c(1,0,1))

yfor<-forecast(yfit)
library(scales)
#---Extract the Data for ggplot using funggcast()

funggcast <- function(dn, fcast) { 
  require(zoo) #needed for the 'as.yearmon()' function
  
  en <- max(time(fcast$mean)) #extract the max date used in the forecast
  
  #Extract Source and Training Data
  ds <- as.data.frame(window(dn, end=en))
  names(ds) <- 'observed'
  ds$date <- as.Date(time(window(dn, end=en)))
  
  #Extract the Fitted Values (need to figure out how to grab confidence intervals)
  dfit <- as.data.frame(fcast$fitted)
  dfit$date <- as.Date(time(fcast$fitted))
  names(dfit)[1] <- 'fitted'
  
  ds <- merge(ds, dfit, all.x=T) #Merge fitted values with source and training data
  
  #Exract the Forecast values and confidence intervals
  dfcastn <- as.data.frame(fcast)
  dfcastn$date <- as.Date(as.yearmon(row.names(dfcastn)))
  names(dfcastn) <- c('forecast','lo80','hi80','lo95','hi95','date')
  
  pd <- merge(ds, dfcastn, all.x=T) #final data.frame for use in ggplot
  return(pd)
  
}

pd<-funggcast(consumits,yfor)
??funggcast
p1a<-ggplot(data=totale,aes(x = date.x, y = consumo)) 
p1a<-p1a+geom_line(col='red')
p1a<-p1a+geom_line(aes(y=fitted),col='blue')

plot(totsarima)
predict(consumits,n.ahead = 24)
futurVal <- forecast.Arima(fitARIMA,h=10, level=c(99.5))

library(lubridate)


int <- interval(ymd("2012-01-01"), ymd("2015-12-31"))

time_length(int, unit = "hour")

z <- zooreg(totale, start = as.Date("2012-01-01"), end=as.Date("2015-12-31"))

zz <- z[format(time(z), "%m %d") != "02 29"]

TS <- ts(coredata(zz), freq = 8760, start = as.Date("2012-01-01"), end=as.Date("2015-12-31"))
#d <- decompose(TS)



totale

class(totale$dateTime.x)

library(zoo)

hourly_tsprezzo <- zoo(
  x         = c(totale$prezzo),
  order.by  = totale$dateTime.x,
  frequency = 24
)

hourly_tsprezzo <- hourly_tsprezzo[format(time(hourly_tsprezzo), "%m %d") != "02 29"]

TS1 <- ts(coredata(hourly_tsprezzo), freq = 24, start = c(2012,1,1), end=c(2015,12,31))



hourly_tsconsumi <- zoo(
  x         = c(totale$consumo),
  order.by  = totale$dateTime.x,
  frequency = 24
)

hourly_tsconsumi  <- hourly_tsconsumi[format(time(hourly_tsconsumi), "%m %d") != "02 29"]

TS2 <- ts(coredata(hourly_tsprezzo), freq = 24, start = c(2012,1,1), end=c(2015,12,31))

TSTOT <- ts.union(TS1,TS2)

########################################
##PREPARATO LE VARIAZIONI PERCENTUALI###
########################################
percentuali <- function(x,y){
  
  risultato <- list()
  risultato[[1]] <- 0
  k <- x[1,y]
  
  for (i in 2:35064) {
    risultato[[i]] <- ((x[i,y]-k)*100)/k
    k <- x[i,y]
  }
  return(risultato)
}

nrow(TSTOT)
consumiperc <- percentuali(TSTOT,1)
consumiperc <- data.frame(matrix(unlist(consumiperc), nrow=35064, byrow=T),stringsAsFactors=FALSE)

prezziperc <- percentuali(TSTOT,2)
prezziperc <- data.frame(matrix(unlist(prezziperc), nrow=35064, byrow=T),stringsAsFactors=FALSE)



consumitsperc <- ts(consumiperc, frequency = 8760, start = c(2012, 1),end=c(2015, 12))
prezzitsperc <- ts(prezziperc, frequency = 8760, start = c(2012, 1),end=c(2015, 12))


tot <- cbind(tot, consumitsperc)
tot <- cbind(tot, prezzitsperc)
#autoplot(TS)
#appesantisce il tutto
#checkresiduals(fit.consMR)

# prova <- window(tot)
# 
# fit.beer <- tslm(prova ~ trend + season)
# summary(fit.beer)


####################################################
############ PROVE KERAS ###########################
####################################################
library(reticulate)
use_condaenv(condaenv = "tf")
library(keras)
install.packages("tis")
library(tis)
tot
diffed = diff(tot, differences = 1)

# lag_transform <- function(x, k= 1){
#   
#   lagged =  c(rep(NA, k), x[1:(length(x)-k)])
#   DF = as.data.frame(cbind(lagged, x))
#   colnames(DF) <- c( paste0('x-', k), 'x')
#   DF[is.na(DF)] <- 0
#   return(DF)
# }
supervised = lags(tot, -24)
head(supervised)

N = nrow(supervised)
n = round(N *0.7, digits = 0)
train = supervised[1:n, ]
test  = supervised[(n+1):N,  ]

# Random sample indexes
train_index <- sample(1:nrow(supervised), 0.8 * nrow(supervised))
test_index <- setdiff(1:nrow(supervised), train_index)

# Build X_train, y_train, X_test, y_test
X_train <- supervised[train_index, "Consumi(-24)"]
y_train <- supervised[train_index, "Prezzi(-24)"]

X_test <- supervised[test_index, "Consumi(-24)"]
y_test <- supervised[test_index, "Prezzi(-24)"]



dim(X_train) <- c(length(X_train), 1, 1)

# specify required arguments
X_shape2 = dim(X_train)[2]
X_shape3 = dim(X_train)[3]
batch_size = 1                # must be a common factor of both the train and test samples
units = 1                     # can adjust this, in model tuninig phase

#=========================================================================================

model <- keras_model_sequential() 
model%>%
  layer_lstm(units, batch_input_shape = c(batch_size, X_shape2, X_shape3), stateful= TRUE)%>%
  layer_dense(units = 1)

model %>% compile(
  loss = 'mean_squared_error',
  optimizer = optimizer_adam( lr= 0.02, decay = 1e-6 ),  
  metrics = c('accuracy')
)

Epochs = 50   

model %>% fit(X_train, y_train, epochs=10, batch_size=batch_size, verbose=1, shuffle=FALSE) 

# data <- data.matrix(totale[-1])
# train_data <- data[1:30000,]

# mean <-  apply(train_data, 2, mean)
# std <-  apply(train_data, 2, sd)
# data <- scale(train_data, center = mean, scale = std)

# generator <- function(data, lookback, delay, min_index, max_index,
#                       shuffle = FALSE, batch_size = 128, step = 6) {
#   if (is.null(max_index))
#     max_index <- nrow(data) - delay - 1
#   i <- min_index + lookback
#   function() {
#     if (shuffle) {
#       rows <- sample(c((min_index+lookback):max_index), size = batch_size)
#     } else {
#       if (i + batch_size >= max_index)
#         i <<- min_index + lookback
#       rows <- c(i:min(i+batch_size-1, max_index))
#       i <<- i + length(rows)
#     }
#     
#     samples <- array(0, dim = c(length(rows),
#                                 lookback / step,
#                                 dim(data)[[-1]]))
#     targets <- array(0, dim = c(length(rows)))
#     
#     for (j in 1:length(rows)) {
#       indices <- seq(rows[[j]] - lookback, rows[[j]]-1,
#                      length.out = dim(samples)[[2]])
#       samples[j,,] <- data[indices,]
#       targets[[j]] <- data[rows[[j]] + delay,2]
#     }           
#     list(samples, targets)
#   }
# }
# 
# 
# lookback <- 1440
# step <- 6
# delay <- 144
# batch_size <- 128
# 
# train_gen <- generator(
#   data,
#   lookback = lookback,
#   delay = delay,
#   min_index = 1,
#   max_index = 30000,
#   shuffle = TRUE,
#   step = step, 
#   batch_size = batch_size
# )
# 
# val_gen = generator(
#   data,
#   lookback = lookback,
#   delay = delay,
#   min_index = 30001,
#   max_index = 47652,
#   step = step,
#   batch_size = batch_size
# )
# 
# test_gen <- generator(
#   data,
#   lookback = lookback,
#   delay = delay,
#   min_index = 30001,
#   max_index = NULL,
#   step = step,
#   batch_size = batch_size
# )
# 
# # How many steps to draw from val_gen in order to see the entire validation set
# val_steps <- (40000 - 30001 - lookback) / batch_size
# 
# # How many steps to draw from test_gen in order to see the entire test set
# test_steps <- (nrow(data) - 40001 - lookback) / batch_size
# 
# 
# model <- keras_model_sequential() %>% 
#   layer_flatten(input_shape = c(lookback / step, dim(data)[-1])) %>% 
#   layer_dense(units = 32, activation = "relu") %>% 
#   layer_dense(units = 1)
# 
# model %>% compile(
#   optimizer = optimizer_rmsprop(),
#   loss = "mae"
# )
# 
# history <- model %>% fit_generator(
#   train_gen,
#   steps_per_epoch = 500,
#   epochs = 20,
#   validation_data = val_gen,
#   validation_steps = val_steps
# )



# totdata <- cbind(totdata,consumiperc)
# totdata <- cbind(totdata,prezziperc)
# colnames(totdata)[1] <- "Consumi"
# colnames(totdata)[2] <- "Prezzi"
# colnames(totdata)[3] <- "Gradi"
# colnames(totdata)[4] <- "Consumiperc"
# colnames(totdata)[5] <- "Prezzoperc"
# 
# 
# totdata
# 
# totdata %>%
#   as.data.frame() %>%
#   ggplot(aes(x=Consumiperc, y=Prezzoperc)) +
#   ylab("Consume") +
#   xlab("Price") +
#   geom_point() +
#   geom_smooth(method="lm", se=FALSE)
# 
# 
# colnames(tot)[1] <- "Consumi"
# colnames(tot)[2] <- "Prezzi"
# colnames(tot)[3] <- "consumitsperc"


########################################
######### TS TOTALE ####################
########################################
eventdata2 <- xts(totale$prezzo, order.by = totale$date.x)
plot.xts(eventdata2)

ts_2012 <- zoo(totale$prezzo, order.by = totale$date.x)
ts_2012 <- ts(ts_2012, frequency = 8760)
autoplot(ts_2012, main = "Prezzi")

ggseasonplot(ts_2012, year.labels=TRUE, year.labels.left=TRUE)

# STAGIONALITÀ mediana
totale %>%
  group_by(anno,mese) %>%
  summarize(prezzimediani = median(prezzo)) -> stagioni

ts_season_median <- zoo(stagioni$prezzimediani, order.by = stagioni$anno)
ts_season_median <- ts(ts_season_median, frequency = 12, names=c("2012","2013","2014","2015"))
ggseasonplot(ts_season_median,year.labels=TRUE)
ggseasonplot(ts_season_median,polar = TRUE)


# CREO DATASET PER OGNI ANNO
lista_anni <- c(2012,2013,2014,2015)
datasets <- list()
for(i in lista_anni){
  df_prezzi %>% filter(anno==i) -> x
  assign(paste0('df_anno_prezzi_',i), x)
  datasets <- append(datasets,paste0('df_anno_prezzi',i))
}

# SPOSTO LA COLONNA DATA ALL'INIZIO
df_anno_prezzi_2012 <- df_anno_prezzi_2012 %>%
  select(date, everything())
df_anno_prezzi_2013 <- df_anno_prezzi_2013 %>%
  select(date, everything())
df_anno_prezzi_2014 <- df_anno_prezzi_2014 %>%
  select(date, everything())
df_anno_prezzi_2015 <- df_anno_prezzi_2015 %>%
  select(date, everything())

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

# FIX NA
summary(df_anno_prezzi_2012$dateTime)

new_DF<-subset(df_anno_prezzi_2012,is.na(df_anno_prezzi_2012$dateTime))

df_anno_prezzi_2012[is.na(df_anno_prezzi_2012)] <- as.POSIXct("2012-03-25 02:00:00")
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
