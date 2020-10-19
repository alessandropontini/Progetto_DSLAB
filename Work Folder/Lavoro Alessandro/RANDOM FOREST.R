df8scandi <-  read.csv("/Volumes/HDD_Ale/Progetto_DSLAB/DATASET SERIE STORICHE/scandinavia8mattina.csv")

ts8scandi <- ts(df8scandi, frequency = 365, start = c(2012, 1),end=c(2015, 365))

plot_org <- ts8scandi %>% 
  ggplot(aes(X, TSTOT.ConsumiScandi)) + # to get the axis on a more manageable scale
  geom_line() +
  theme_minimal() +
  labs(title = "", x = "Year", y = "Consume")
x <- as.Date("2012-01-01") + c(1:1460)
tsx <- ts(as.Date(x), frequency = 365, start = c(2012, 1),end=c(2015, 365))
df = data.frame(datefield=x,test=1:length(x))

require(zoo)
## convert to a zoo object, with order given by the `datefield`
df.zoo <- with(df, zoo(test, order.by = x))
## or to a regular zoo object
df.zoo2 <- with(df, zooreg(test, order.by = x))
tsx <-as.ts(df.zoo2)
ts8scandi <- cbind(ts8scandi,tsx)
ts8scandi

# faccio colonna giorni
df8scandi
df1 <-data.frame("giorni_settimana" = 1:1460)
df1$giorni_settimana <-  as.character(df1$giorni_settimana)
j = 1

while(j<61){
  df1$giorni_settimana[j] = "dom"
  j = j + 7
}  

j = 2

while(j<61){
  df1$giorni_settimana[j] = "lun"
  j = j + 7
}

j = 3

while(j<61){
  df1$giorni_settimana[j] = "mar"
  j = j + 7
}

j = 4

while(j<61){
  df1$giorni_settimana[j] = "merc"
  j = j + 7
}
j = 5

while(j<61){
  df1$giorni_settimana[j] = "giov"
  j = j + 7
}
j = 6

while(j<61){
  df1$giorni_settimana[j] = "ven"
  j = j + 7
}
j = 7

while(j<61){
  df1$giorni_settimana[j] = "sab"
  j = j + 7
}  

df1$giorni_settimana[60] = "giov"



j = 61

while(j<nrow(df1)){
  df1$giorni_settimana[j] = "dom"
  j = j + 7
}  

j = 62

while(j<nrow(df1)){
  df1$giorni_settimana[j] = "lun"
  j = j + 7
}

j = 63

while(j<nrow(df1)){
  df1$giorni_settimana[j] = "mar"
  j = j + 7
}

j = 64

while(j<nrow(df1)){
  df1$giorni_settimana[j] = "merc"
  j = j + 7
}
j = 65

while(j<nrow(df1)){
  df1$giorni_settimana[j] = "giov"
  j = j + 7
}
j = 66

while(j<nrow(df1)){
  df1$giorni_settimana[j] = "ven"
  j = j + 7
}
j = 67

while(j<nrow(df1)){
  df1$giorni_settimana[j] = "sab"
  j = j + 7
}  

tail(df1$giorni_settimana)
df1$giorni_settimana[1460] = "sab"
str(df1)
df1ts <- ts(df1, frequency = 365, start = c(2012, 1),end=c(2015, 365))
write.csv(df1, "/Volumes/HDD_Ale/Progetto_DSLAB/DATASET SERIE STORICHE/colonna_charsettimana.csv")
