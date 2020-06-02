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
require(xts)
####################################
###### IMPORTO I DATASET ###########
####################################
dir_df1 <- "/Volumes/HDD_Ale/ElData/PL_1215_Price_byhour.csv"
dir_df2 <- "/Volumes/HDD_Ale/ElData/PL_1215_Load_byhour.csv"

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
fixna15
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
fixna15
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
summary(totale$prezzo)

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

eventdata2 <- xts(df_prezzi$prezzo, order.by = df_prezzi$date)
plot.xts(eventdata2)

ts_2012 <- zoo(df_prezzi$prezzo, order.by = df_prezzi$date)
ts_2012 <- ts(ts_2012, frequency = 35040)
autoplot(ts_2012, main = "Prezzi")
