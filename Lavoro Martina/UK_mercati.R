library(dplyr)
library(stringi)
library(tidyr)
library(zoo)
library(ggplot2)
library(ggfortify)
library(lubridate)
library(xts)
library(forecast)

dir_df1 <- "/Users/martinagulino/Desktop/DataScience/1 anno/2 semestre/DataScience Lab/Progetto/ElData/UKPX_1215_Price_byhour.csv"
dir_df2 <- "/Users/martinagulino/Desktop/DataScience/1 anno/2 semestre/DataScience Lab/Progetto/ElData/UKPX_1215_Loads_byhour.csv"

df_prezzi1 <- read.csv(dir_df1)
df_consumi1 <- read.csv(dir_df2)


hour <- c(01:24)
appo1 <- data.frame()

for (j in 2:ncol(df_prezzi1)) {
  for (i in 1:nrow(df_prezzi1)) {
    if(j%%2 == 0){
      appo1[i,j/2] <- (df_prezzi1[i,j]+df_prezzi1[i,j+1])/2
    }
  }
}
colnames(appo1) <- paste0("H", hour)
DateYYYYMMDD <- df_prezzi1[,1]
appo1 <- cbind(DateYYYYMMDD, appo1)

df_prezzi <- appo1
df_prezzi


appo2 <- data.frame()
for (j in 2:ncol(df_consumi1)) {
  for (i in 1:nrow(df_consumi1)) {
    if(j%%2 == 0){
      appo2[i,j/2] <- df_consumi1[i,j]+df_consumi1[i,j+1]
    }
  }
}  
colnames(appo2) <- paste0("H", hour)
DateYYYYMMDD <- df_consumi1[,1]
appo2 <- cbind(DateYYYYMMDD, appo2)

df_consumi  <- appo2


df_prezzi$anno <- stri_sub(df_prezzi$DateYYYYMMDD,1,4)
df_prezzi$mese <- stri_sub(df_prezzi$DateYYYYMMDD,5,6)
df_prezzi$giorno <- stri_sub(df_prezzi$DateYYYYMMDD,7,8)

df_prezzi <- df_prezzi %>% gather("ora", "prezzo", H1:H24)
df_prezzi$ora <- stri_sub(df_prezzi$ora,2,3)
df_prezzi <- df_prezzi %>% mutate(ora = paste0(ora,":00:00"))
df_prezzi$date <- as.Date(with(df_prezzi, paste(anno, mese, giorno,sep="-")), "%Y-%m-%d")
df_prezzi <- df_prezzi %>% arrange(date)
df_prezzi$dateTime = as.POSIXct(paste(df_prezzi$date,df_prezzi$ora), format="%Y-%m-%d %H:%M:%S")
df_prezzi$DateYYYYMMDD <- NULL

df_prezzi

df_consumi$anno <- stri_sub(df_consumi$DateYYYYMMDD,1,4)
df_consumi$mese <- stri_sub(df_consumi$DateYYYYMMDD,5,6)
df_consumi$giorno <- stri_sub(df_consumi$DateYYYYMMDD,7,8)

df_consumi <- df_consumi %>% gather("ora", "consumo", H1:H24)
df_consumi$ora <- stri_sub(df_consumi$ora,2,3)
df_consumi <- df_consumi %>% mutate(ora = paste0(ora,":00:00"))
df_consumi$date <- as.Date(with(df_consumi, paste(anno, mese, giorno,sep="-")), "%Y-%m-%d")
df_consumi <- df_consumi %>% arrange(date)
df_consumi$dateTime = as.POSIXct(paste(df_consumi$date,df_consumi$ora), format="%Y-%m-%d %H:%M:%S")
df_consumi$DateYYYYMMDD <- NULL

df_consumi

summary(df_prezzi)
summary(df_consumi)

totale

apply(totale, 2, function(x) any(is.na(x)))
apply(totale, 2, function(x) any(is.nan(x)))
summary(totale)

apply(is.na(totale),2,sum)

which(is.na(df_prezzi$dateTime))
which(is.na(df_consumi$dateTime))

totale <- left_join(df_prezzi, df_consumi, by = c("anno" = "anno", "mese" = "mese", "giorno"="giorno", "ora"="ora"))
totale <- totale[, -c(9:10)]

summary(totale)
new_DF<-subset(totale,is.na(totale$prezzo))

#Sistemato tutti i nan

totale %>% mutate(dateTime.x = if_else((giorno =="25") & (anno == "2012") & (mese == "03") & (ora=="02:00:00"), as.POSIXct("2012-03-25 02:00:00"), dateTime.x)) -> totale 
totale %>% mutate(dateTime.x = if_else((giorno =="31") & (anno == "2013") & (mese == "03") & (ora=="02:00:00"), as.POSIXct("2013-03-31 02:00:00"), dateTime.x)) -> totale 
totale %>% mutate(dateTime.x = if_else((giorno =="30") & (anno == "2014") & (mese == "03") & (ora=="02:00:00"), as.POSIXct("2014-03-30 02:00:00"), dateTime.x)) -> totale 
totale %>% mutate(dateTime.x = if_else((giorno =="29") & (anno == "2015") & (mese == "03") & (ora=="02:00:00"), as.POSIXct("2015-03-29 02:00:00"), dateTime.x)) -> totale 

summary(totale)

which(is.na(totale$dateTime.x))
totale$dateTime.x[2017]