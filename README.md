#Laden van de libraries 
library(tidyverse)
library(pacman)
library(dplyr)
library(tidyr)
library(ggplot2)

#Importeren Excel bestand
TSH <- library(readxl)
url <- "https://github.com/Toadhex/de-R-space/raw/master/TSH.xlsx"
destfile <- "TSH.xlsx"  
curl::curl_download(url, destfile) 
TSH <- read_excel(destfile, col_types = c("text", 
                                          "text", "text", "text", "numeric", "numeric", 
                                          "numeric", "skip"))

#Datatabel weergeven in nieuwe tab
View(TSH)

#Eerste waarden weergeven in de console
head(TSH)

#Statistische gegevens van de kolom 'TSH'
summary(TSH$TSH)

#Standaard deviatie van de kolom 'TSH'
sd(TSH$TSH, na.rm = TRUE)

#Statistische gegevens van de kolom 'FT4'
summary(TSH$FT4)

#Standaard deviatie van de kolom 'FT4'
sd(TSH$FT4, na.rm = TRUE)

#Statistische gegevens van de kolom 'FT3'
summary(TSH$FT3)

#Standaard deviatie van de kolom 'FT3'
sd(TSH$FT3, na.rm = TRUE)

#Boxplot TSH
TSH_1 <-ggplot(TSH, aes(y = TSH))
TSH_1 + geom_boxplot()

#Boxplot FT4
FT4_1 <-ggplot(TSH, aes(y = FT4))
FT4_1 + geom_boxplot() 

#Boxplot FT3
FT3_1 <-ggplot(data = TSH, aes(y = FT3))
FT3_1 + geom_boxplot()

#Criteria van Chauvenette toepassen op TSH, berekende waarden toegevoegd in een nieuwe kolom genaam 'TSHC'
TSH$TSHC <- (TSH[,5]-2.301)/2.813338

#Criteria van Chauvenette toepassen op FT4, berekende waarden toegevoegd in een nieuwe kolom genaam 'FT4C'
TSH$FT4C <- (TSH[,6]-16.46)/3.083652

#Criteria van Chauvenette toepassen op FT3, berekende waarden toegevoegd in een nieuwe kolom genaam 'FT3C'
TSH$FT3C <- (TSH[,7]-4.668)/1.464226

#Verander NA naar 0
TSH$TSHC[is.na(TSH$TSHC)] <- 0
TSH$FT4C[is.na(TSH$FT4C)] <- 0
TSH$FT3C[is.na(TSH$FT3C)] <- 0

#TSH waarden buiten het kritiekegebied filtered
TSHFilter <- filter (TSH, TSHC < 3.49, FT4C < 3.49, FT3C < 3.49)

#Verander 0 terug naar NA
TSHFilter <- na_if(TSHFilter, 0)

#Boxplot TSH na filter
TSH_2 <-ggplot(TSHFilter, aes(y = TSH))
TSH_2 + geom_boxplot()

#Boxplot FT4 na filter
FT4_2 <-ggplot(TSHFilter, aes(y = FT4))
FT4_2 + geom_boxplot()

#Boxplot FT3 na filter
FT3_2 <-ggplot(TSHFilter, aes(y = FT3))
FT3_2 + geom_boxplot()


