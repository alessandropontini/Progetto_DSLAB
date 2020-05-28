library(dplyr)
install.packages("stringi")
library(stringi)


dir_df1 <- "/Volumes/HDD_Ale/ElData/PL_1215_Price_byhour.csv"
dir_df2 <- "/Volumes/HDD_Ale/ElData/PL_1215_Load_byhour.csv"

df_prezzi <- read.csv(dir_df1)


df_prezzi$anno <- stri_sub(df_prezzi$DateYYYYMMDD,1,4)
df_prezzi$mese <- stri_sub(df_prezzi$DateYYYYMMDD,5,6)
df_prezzi$giorno <- stri_sub(df_prezzi$DateYYYYMMDD,7,8)

df_prezzi

df_prezzi$date <- as.Date(with(df_prezzi, paste(anno, mese, giorno,sep="-")), "%Y-%m-%d")

