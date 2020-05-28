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

####################################
###### PRE-PROCESSING ##############
####################################

# SISTEMO DATE
df_prezzi$anno <- stri_sub(df_prezzi$DateYYYYMMDD,1,4)
df_prezzi$mese <- stri_sub(df_prezzi$DateYYYYMMDD,5,6)
df_prezzi$giorno <- stri_sub(df_prezzi$DateYYYYMMDD,7,8)

df_prezzi <- df_prezzi %>% gather("ora", "prezzo", H01:H24)
df_prezzi$ora <- stri_sub(df_prezzi$ora,2,3)
df_prezzi <- df_prezzi %>% mutate(ora = paste0(ora,":00:00"))
df_prezzi$date <- as.Date(with(df_prezzi, paste(anno, mese, giorno,sep="-")), "%Y-%m-%d")
df_prezzi <- df_prezzi %>% arrange(date)
df_prezzi$dateTime = as.POSIXct(paste(df_prezzi$date,df_prezzi$ora), format = "%Y-%m-%d %H:%M:%S")
df_prezzi$DateYYYYMMDD <- NULL

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
