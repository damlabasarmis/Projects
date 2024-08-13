#Change the directory
setwd("/Users/damlabasarmis/Desktop/Staj")

#Install necessary packages
install.packages("data.table")
library(data.table)

install.packages("zoo")
library(zoo)

install.packages("readr")
library(readr)

install.packages("tidyr")
library(tidyr)

install.packages("dplyr")
library(dplyr)

install.packages("readr")
library(readr)

install.packages("reshape2")
library(reshape2)
#Upload the data
veri<- fread("/Users/damlabasarmis/Desktop/Staj/pivot.csv", encoding = "UTF-8")

#Separate the data correctly
veri <- read_delim("pivot.csv", delim = "|")

#Remove the 1st, 5th and the last row
veri <- veri[-c(1,nrow(veri)), ]

#Change the position of rows and columns
veri <- t(veri)


#Remove the 1st, 5th and the last row
veri <- veri[-c(1,2,10,nrow(veri)), ]

#Remove the 3rd column
veri <- veri[, -3]

#Change the matrix to data.table
veri <- as.data.table(veri)

#Fill NA yil values
veri$V1 <- na.locf(veri$V1, na.rm = FALSE)

#Remove the last row
veri <- veri[-.N, ]

#Separate V2 into categories 
veri <- veri %>%
  separate(V1, into = c("cinsiyet", "yas_grubu", "egitim_duzey"), sep = " ve ")

#Change the column name
setnames(veri, "V2", "yil")

#Add column names
colnames(veri) <- c("yil","cinsiyet","yas_grubu","egitim_duzey",veri[1,5:16])

#Remove the first row
veri <- veri[-1,]

#Add "bolge" and "frekans" columns
veri <- veri %>% pivot_longer(cols=c(`Akdeniz-TR6`,`Batı Anadolu-TR5`, `Batı Karadeniz-TR8`,
`Batı Marmara-TR2`, `Doğu Karadeniz-TR9`, `Doğu Marmara-TR4`, `Ege-TR3`,`Güneydoğu Anadolu-TRC`,`İstanbul-TR1`,
`Kuzeydoğu Anadolu-TRA`,`Orta Anadolu-TR7`,`Ortadoğu Anadolu-TRB`),names_to="bolge",values_to = "frekans")

# Sütun adlarını değiştirme
setnames(veri, "a", "yas_grubu")

#Change column positions
setcolorder(veri, c("bolge", "yil", "cinsiyet", "yas_grubu", "egitim_duzey", "frekans"))

#Change the order alphabetically
setorder(veri, bolge)

#Changing some variables as factors and some last checks
veri$bolge <- as.factor(veri$bolge)
levels(veri$bolge)

veri$cinsiyet <- as.factor(veri$cinsiyet)
levels(veri$cinsiyet)

veri$yas_grubu <- as.factor(veri$yas_grubu)
levels(veri$yas_grubu)

veri$egitim_duzey <- as.factor(veri$egitim_duzey)
levels(veri$egitim_duzey)

summary(veri)

#Changing yil and frekans into numeric
veri$yil <- as.numeric(veri$yil)
summary(veri)

veri$frekans <- as.numeric(veri$frekans)
summary(veri)
str(veri)

#Upload the clean data
write.csv(veri, file = "veri.csv", row.names = FALSE)
