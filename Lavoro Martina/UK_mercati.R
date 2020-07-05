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

temperature <- read.csv("/Users/martinagulino/Desktop/Progetto_DSLAB/Lavoro Martina/tot_temperature.csv")
temperature
str(temperature)
str(totale)
v = list("1", "2", "3", "4", "5", "6", "7", "8", "9")
z = list("0:00:00","1:00:00", "2:00:00", "3:00:00", "4:00:00", "5:00:00", "6:00:00", "7:00:00", "8:00:00", "9:00:00")
k = list("24:00:00")

temperature$Giorno <-  as.character(temperature$Giorno)
temperature$Mese <-  as.character(temperature$Mese)
temperature$Anno <-  as.character(temperature$Anno)
temperature$OraFix5 <-  as.character(temperature$OraFix5)

temperature %>% mutate(Giorno = if_else(Giorno %in% v, paste0("0",Giorno), Giorno)) -> temperature
temperature %>% mutate(Mese = if_else(Mese %in% v, paste0("0",Mese), Mese)) -> temperature
totale %>% mutate(ora = if_else(ora %in% z, paste0("0",ora), ora)) -> totale
totale %>% mutate(ora = if_else(ora %in% k, "00:00:00", ora)) -> totale

totaleprova <- left_join(totale, temperature, by = c("giorno" = "Giorno", "mese" = "Mese", "ora" = "OraFix5", "anno" = "Anno"))
summary(totaleprova)
str(totaleprova)
new_DF<-subset(totaleprova,is.na(totaleprova$GradoFix))


### 2012

# gennaio
m = list("01")
a = list("2012")

totaleprova %>% filter(mese=='01', anno=='2012') -> gennaio_2012
mediana <- median(gennaio_2012$GradoFix, na.rm = TRUE)

totaleprova %>% mutate(GradoFix = if_else(anno %in% a & mese %in% m & is.na(GradoFix), mediana, GradoFix)) -> totaleprova

# febbraio
m = list("02")
a = list("2012")

totaleprova %>% filter(mese=='02', anno=='2012') -> gennaio_2012
mediana <- median(gennaio_2012$GradoFix, na.rm = TRUE)

totaleprova %>% mutate(GradoFix = if_else(anno %in% a & mese %in% m & is.na(GradoFix), mediana, GradoFix)) -> totaleprova

# marzo
m = list("03")
a = list("2012")

totaleprova %>% filter(mese=='03', anno=='2012') -> gennaio_2012
mediana <- median(gennaio_2012$GradoFix, na.rm = TRUE)

totaleprova %>% mutate(GradoFix = if_else(anno %in% a & mese %in% m & is.na(GradoFix), mediana, GradoFix)) -> totaleprova

# aprile
m = list("04")
a = list("2012")

totaleprova %>% filter(mese=='04', anno=='2012') -> gennaio_2012
mediana <- median(gennaio_2012$GradoFix, na.rm = TRUE)

totaleprova %>% mutate(GradoFix = if_else(anno %in% a & mese %in% m & is.na(GradoFix), mediana, GradoFix)) -> totaleprova

# maggio
m = list("05")
a = list("2012")

totaleprova %>% filter(mese=='05', anno=='2012') -> gennaio_2012
mediana <- median(gennaio_2012$GradoFix, na.rm = TRUE)

totaleprova %>% mutate(GradoFix = if_else(anno %in% a & mese %in% m & is.na(GradoFix), mediana, GradoFix)) -> totaleprova


# giugno
m = list("06")
a = list("2012")

totaleprova %>% filter(mese=='06', anno=='2012') -> gennaio_2012
mediana <- median(gennaio_2012$GradoFix, na.rm = TRUE)

totaleprova %>% mutate(GradoFix = if_else(anno %in% a & mese %in% m & is.na(GradoFix), mediana, GradoFix)) -> totaleprova


# luglio
m = list("07")
a = list("2012")

totaleprova %>% filter(mese=='07', anno=='2012') -> gennaio_2012
mediana <- median(gennaio_2012$GradoFix, na.rm = TRUE)

totaleprova %>% mutate(GradoFix = if_else(anno %in% a & mese %in% m & is.na(GradoFix), mediana, GradoFix)) -> totaleprova

# agosto
m = list("08")
a = list("2012")

totaleprova %>% filter(mese=='08', anno=='2012') -> gennaio_2012
mediana <- median(gennaio_2012$GradoFix, na.rm = TRUE)

totaleprova %>% mutate(GradoFix = if_else(anno %in% a & mese %in% m & is.na(GradoFix), mediana, GradoFix)) -> totaleprova

# settembre
m = list("09")
a = list("2012")

totaleprova %>% filter(mese=='09', anno=='2012') -> gennaio_2012
mediana <- median(gennaio_2012$GradoFix, na.rm = TRUE)

totaleprova %>% mutate(GradoFix = if_else(anno %in% a & mese %in% m & is.na(GradoFix), mediana, GradoFix)) -> totaleprova

# ottobre
m = list("10")
a = list("2012")

totaleprova %>% filter(mese=='10', anno=='2012') -> gennaio_2012
mediana <- median(gennaio_2012$GradoFix, na.rm = TRUE)

totaleprova %>% mutate(GradoFix = if_else(anno %in% a & mese %in% m & is.na(GradoFix), mediana, GradoFix)) -> totaleprova

# novembre
m = list("11")
a = list("2012")

totaleprova %>% filter(mese=='11', anno=='2012') -> gennaio_2012
mediana <- median(gennaio_2012$GradoFix, na.rm = TRUE)

totaleprova %>% mutate(GradoFix = if_else(anno %in% a & mese %in% m & is.na(GradoFix), mediana, GradoFix)) -> totaleprova

# dicembre
m = list("12")
a = list("2012")

totaleprova %>% filter(mese=='12', anno=='2012') -> gennaio_2012
mediana <- median(gennaio_2012$GradoFix, na.rm = TRUE)

totaleprova %>% mutate(GradoFix = if_else(anno %in% a & mese %in% m & is.na(GradoFix), mediana, GradoFix)) -> totaleprova

### 2013

# gennaio
m = list("01")
a = list("2013")

totaleprova %>% filter(mese=='01', anno=='2013') -> gennaio_2012
mediana <- median(gennaio_2012$GradoFix, na.rm = TRUE)

totaleprova %>% mutate(GradoFix = if_else(anno %in% a & mese %in% m & is.na(GradoFix), mediana, GradoFix)) -> totaleprova

# febbraio
m = list("02")
a = list("2013")

totaleprova %>% filter(mese=='02', anno=='2013') -> gennaio_2012
mediana <- median(gennaio_2012$GradoFix, na.rm = TRUE)

totaleprova %>% mutate(GradoFix = if_else(anno %in% a & mese %in% m & is.na(GradoFix), mediana, GradoFix)) -> totaleprova

# marzo
m = list("03")
a = list("2013")

totaleprova %>% filter(mese=='03', anno=='2013') -> gennaio_2012
mediana <- median(gennaio_2012$GradoFix, na.rm = TRUE)

totaleprova %>% mutate(GradoFix = if_else(anno %in% a & mese %in% m & is.na(GradoFix), mediana, GradoFix)) -> totaleprova

# aprile
m = list("04")
a = list("2013")

totaleprova %>% filter(mese=='04', anno=='2013') -> gennaio_2012
mediana <- median(gennaio_2012$GradoFix, na.rm = TRUE)

totaleprova %>% mutate(GradoFix = if_else(anno %in% a & mese %in% m & is.na(GradoFix), mediana, GradoFix)) -> totaleprova

# maggio
m = list("05")
a = list("2013")

totaleprova %>% filter(mese=='05', anno=='2013') -> gennaio_2012
mediana <- median(gennaio_2012$GradoFix, na.rm = TRUE)

totaleprova %>% mutate(GradoFix = if_else(anno %in% a & mese %in% m & is.na(GradoFix), mediana, GradoFix)) -> totaleprova


# giugno
m = list("06")
a = list("2013")

totaleprova %>% filter(mese=='06', anno=='2013') -> gennaio_2012
mediana <- median(gennaio_2012$GradoFix, na.rm = TRUE)

totaleprova %>% mutate(GradoFix = if_else(anno %in% a & mese %in% m & is.na(GradoFix), mediana, GradoFix)) -> totaleprova


# luglio
m = list("07")
a = list("2013")

totaleprova %>% filter(mese=='07', anno=='2013') -> gennaio_2012
mediana <- median(gennaio_2012$GradoFix, na.rm = TRUE)

totaleprova %>% mutate(GradoFix = if_else(anno %in% a & mese %in% m & is.na(GradoFix), mediana, GradoFix)) -> totaleprova

# agosto
m = list("08")
a = list("2013")

totaleprova %>% filter(mese=='08', anno=='2013') -> gennaio_2012
mediana <- median(gennaio_2012$GradoFix, na.rm = TRUE)

totaleprova %>% mutate(GradoFix = if_else(anno %in% a & mese %in% m & is.na(GradoFix), mediana, GradoFix)) -> totaleprova

# settembre
m = list("09")
a = list("2013")

totaleprova %>% filter(mese=='09', anno=='2013') -> gennaio_2012
mediana <- median(gennaio_2012$GradoFix, na.rm = TRUE)

totaleprova %>% mutate(GradoFix = if_else(anno %in% a & mese %in% m & is.na(GradoFix), mediana, GradoFix)) -> totaleprova

# ottobre
m = list("10")
a = list("2013")

totaleprova %>% filter(mese=='10', anno=='2013') -> gennaio_2012
mediana <- median(gennaio_2012$GradoFix, na.rm = TRUE)

totaleprova %>% mutate(GradoFix = if_else(anno %in% a & mese %in% m & is.na(GradoFix), mediana, GradoFix)) -> totaleprova

# novembre
m = list("11")
a = list("2013")

totaleprova %>% filter(mese=='11', anno=='2013') -> gennaio_2012
mediana <- median(gennaio_2012$GradoFix, na.rm = TRUE)

totaleprova %>% mutate(GradoFix = if_else(anno %in% a & mese %in% m & is.na(GradoFix), mediana, GradoFix)) -> totaleprova

# dicembre
m = list("12")
a = list("2013")

totaleprova %>% filter(mese=='12', anno=='2013') -> gennaio_2012
mediana <- median(gennaio_2012$GradoFix, na.rm = TRUE)

totaleprova %>% mutate(GradoFix = if_else(anno %in% a & mese %in% m & is.na(GradoFix), mediana, GradoFix)) -> totaleprova

### 2014

# gennaio
m = list("01")
a = list("2014")

totaleprova %>% filter(mese=='01', anno=='2014') -> gennaio_2012
mediana <- median(gennaio_2012$GradoFix, na.rm = TRUE)

totaleprova %>% mutate(GradoFix = if_else(anno %in% a & mese %in% m & is.na(GradoFix), mediana, GradoFix)) -> totaleprova

# febbraio
m = list("02")
a = list("2014")

totaleprova %>% filter(mese=='02', anno=='2014') -> gennaio_2012
mediana <- median(gennaio_2012$GradoFix, na.rm = TRUE)

totaleprova %>% mutate(GradoFix = if_else(anno %in% a & mese %in% m & is.na(GradoFix), mediana, GradoFix)) -> totaleprova

# marzo
m = list("03")
a = list("2014")

totaleprova %>% filter(mese=='03', anno=='2014') -> gennaio_2012
mediana <- median(gennaio_2012$GradoFix, na.rm = TRUE)

totaleprova %>% mutate(GradoFix = if_else(anno %in% a & mese %in% m & is.na(GradoFix), mediana, GradoFix)) -> totaleprova

# aprile
m = list("04")
a = list("2014")

totaleprova %>% filter(mese=='04', anno=='2014') -> gennaio_2012
mediana <- median(gennaio_2012$GradoFix, na.rm = TRUE)

totaleprova %>% mutate(GradoFix = if_else(anno %in% a & mese %in% m & is.na(GradoFix), mediana, GradoFix)) -> totaleprova

# maggio
m = list("05")
a = list("2014")

totaleprova %>% filter(mese=='05', anno=='2014') -> gennaio_2012
mediana <- median(gennaio_2012$GradoFix, na.rm = TRUE)

totaleprova %>% mutate(GradoFix = if_else(anno %in% a & mese %in% m & is.na(GradoFix), mediana, GradoFix)) -> totaleprova


# giugno
m = list("06")
a = list("2014")

totaleprova %>% filter(mese=='06', anno=='2014') -> gennaio_2012
mediana <- median(gennaio_2012$GradoFix, na.rm = TRUE)

totaleprova %>% mutate(GradoFix = if_else(anno %in% a & mese %in% m & is.na(GradoFix), mediana, GradoFix)) -> totaleprova


# luglio
m = list("07")
a = list("2014")

totaleprova %>% filter(mese=='07', anno=='2014') -> gennaio_2012
mediana <- median(gennaio_2012$GradoFix, na.rm = TRUE)

totaleprova %>% mutate(GradoFix = if_else(anno %in% a & mese %in% m & is.na(GradoFix), mediana, GradoFix)) -> totaleprova

# agosto
m = list("08")
a = list("2014")

totaleprova %>% filter(mese=='08', anno=='2014') -> gennaio_2012
mediana <- median(gennaio_2012$GradoFix, na.rm = TRUE)

totaleprova %>% mutate(GradoFix = if_else(anno %in% a & mese %in% m & is.na(GradoFix), mediana, GradoFix)) -> totaleprova

# settembre
m = list("09")
a = list("2014")

totaleprova %>% filter(mese=='09', anno=='2014') -> gennaio_2012
mediana <- median(gennaio_2012$GradoFix, na.rm = TRUE)

totaleprova %>% mutate(GradoFix = if_else(anno %in% a & mese %in% m & is.na(GradoFix), mediana, GradoFix)) -> totaleprova

# ottobre
m = list("10")
a = list("2014")

totaleprova %>% filter(mese=='10', anno=='2014') -> gennaio_2012
mediana <- median(gennaio_2012$GradoFix, na.rm = TRUE)

totaleprova %>% mutate(GradoFix = if_else(anno %in% a & mese %in% m & is.na(GradoFix), mediana, GradoFix)) -> totaleprova

# novembre
m = list("11")
a = list("2014")

totaleprova %>% filter(mese=='11', anno=='2014') -> gennaio_2012
mediana <- median(gennaio_2012$GradoFix, na.rm = TRUE)

totaleprova %>% mutate(GradoFix = if_else(anno %in% a & mese %in% m & is.na(GradoFix), mediana, GradoFix)) -> totaleprova

# dicembre
m = list("12")
a = list("2014")

totaleprova %>% filter(mese=='12', anno=='2014') -> gennaio_2012
mediana <- median(gennaio_2012$GradoFix, na.rm = TRUE)

totaleprova %>% mutate(GradoFix = if_else(anno %in% a & mese %in% m & is.na(GradoFix), mediana, GradoFix)) -> totaleprova

### 2015

# gennaio
m = list("01")
a = list("2015")

totaleprova %>% filter(mese=='01', anno=='2015') -> gennaio_2012
mediana <- median(gennaio_2012$GradoFix, na.rm = TRUE)

totaleprova %>% mutate(GradoFix = if_else(anno %in% a & mese %in% m & is.na(GradoFix), mediana, GradoFix)) -> totaleprova

# febbraio
m = list("02")
a = list("2015")

totaleprova %>% filter(mese=='02', anno=='2015') -> gennaio_2012
mediana <- median(gennaio_2012$GradoFix, na.rm = TRUE)

totaleprova %>% mutate(GradoFix = if_else(anno %in% a & mese %in% m & is.na(GradoFix), mediana, GradoFix)) -> totaleprova

# marzo
m = list("03")
a = list("2015")

totaleprova %>% filter(mese=='03', anno=='2015') -> gennaio_2012
mediana <- median(gennaio_2012$GradoFix, na.rm = TRUE)

totaleprova %>% mutate(GradoFix = if_else(anno %in% a & mese %in% m & is.na(GradoFix), mediana, GradoFix)) -> totaleprova

# aprile
m = list("04")
a = list("2015")

totaleprova %>% filter(mese=='04', anno=='2015') -> gennaio_2012
mediana <- median(gennaio_2012$GradoFix, na.rm = TRUE)

totaleprova %>% mutate(GradoFix = if_else(anno %in% a & mese %in% m & is.na(GradoFix), mediana, GradoFix)) -> totaleprova

# maggio
m = list("05")
a = list("2015")

totaleprova %>% filter(mese=='05', anno=='2015') -> gennaio_2012
mediana <- median(gennaio_2012$GradoFix, na.rm = TRUE)

totaleprova %>% mutate(GradoFix = if_else(anno %in% a & mese %in% m & is.na(GradoFix), mediana, GradoFix)) -> totaleprova


# giugno
m = list("06")
a = list("2015")

totaleprova %>% filter(mese=='06', anno=='2015') -> gennaio_2012
mediana <- median(gennaio_2012$GradoFix, na.rm = TRUE)

totaleprova %>% mutate(GradoFix = if_else(anno %in% a & mese %in% m & is.na(GradoFix), mediana, GradoFix)) -> totaleprova


# luglio
m = list("07")
a = list("2015")

totaleprova %>% filter(mese=='07', anno=='2015') -> gennaio_2012
mediana <- median(gennaio_2012$GradoFix, na.rm = TRUE)

totaleprova %>% mutate(GradoFix = if_else(anno %in% a & mese %in% m & is.na(GradoFix), mediana, GradoFix)) -> totaleprova

# agosto
m = list("08")
a = list("2015")

totaleprova %>% filter(mese=='08', anno=='2015') -> gennaio_2012
mediana <- median(gennaio_2012$GradoFix, na.rm = TRUE)

totaleprova %>% mutate(GradoFix = if_else(anno %in% a & mese %in% m & is.na(GradoFix), mediana, GradoFix)) -> totaleprova

# settembre
m = list("09")
a = list("2015")

totaleprova %>% filter(mese=='09', anno=='2015') -> gennaio_2012
mediana <- median(gennaio_2012$GradoFix, na.rm = TRUE)

totaleprova %>% mutate(GradoFix = if_else(anno %in% a & mese %in% m & is.na(GradoFix), mediana, GradoFix)) -> totaleprova

# ottobre
m = list("10")
a = list("2015")

totaleprova %>% filter(mese=='10', anno=='2015') -> gennaio_2012
mediana <- median(gennaio_2012$GradoFix, na.rm = TRUE)

totaleprova %>% mutate(GradoFix = if_else(anno %in% a & mese %in% m & is.na(GradoFix), mediana, GradoFix)) -> totaleprova

# novembre
m = list("11")
a = list("2015")

totaleprova %>% filter(mese=='11', anno=='2015') -> gennaio_2012
mediana <- median(gennaio_2012$GradoFix, na.rm = TRUE)

totaleprova %>% mutate(GradoFix = if_else(anno %in% a & mese %in% m & is.na(GradoFix), mediana, GradoFix)) -> totaleprova

# dicembre
m = list("12")
a = list("2015")

totaleprova %>% filter(mese=='12', anno=='2015') -> gennaio_2012
mediana <- median(gennaio_2012$GradoFix, na.rm = TRUE)

totaleprova %>% mutate(GradoFix = if_else(anno %in% a & mese %in% m & is.na(GradoFix), mediana, GradoFix)) -> totaleprova

########################################
##PREPARATO LE VARIAZIONI PERCENTUALI###
########################################

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


consumiperc <- percentuali(totaleprova,5)
consumiperc <- data.frame(matrix(unlist(consumiperc), nrow=35068, byrow=T),stringsAsFactors=FALSE)

prezziperc <- percentuali(totaleprova,8)
prezziperc <- data.frame(matrix(unlist(prezziperc), nrow=35068, byrow=T),stringsAsFactors=FALSE)


consumitsperc <- ts(consumiperc, frequency = 8760, start = c(2012, 1),end=c(2015, 12))
prezzitsperc <- ts(prezziperc, frequency = 8760, start = c(2012, 1),end=c(2015, 12))
TSTOT <-  ts(coredata(totaleprova), freq = 8760, start = c(2012,1,1),  end = c(2015,8760))

tot <- cbind(tot, consumitsperc)
tot <- cbind(tot, prezzitsperc)

colnames(tot)[1] <- "Consumi"
colnames(tot)[2] <- "Prezzi"
colnames(tot)[3] <- "Gradi"
colnames(tot)[4] <- "Consumiperc"
colnames(tot)[5] <- "Prezzoperc"