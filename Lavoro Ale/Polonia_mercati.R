#install.packages("stringi")

####################################
###### LIBRERIE ####################
####################################
library(dplyr)
library(stringi)

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

df_prezzi$date <- as.Date(with(df_prezzi, paste(anno, mese, giorno,sep="-")), "%Y-%m-%d")
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

