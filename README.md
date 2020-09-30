#Laden van de libraries 
library(tidyverse)
library(pacman)
library(dplyr)
library(tidyr)
library(ggplot2)

#Importeren Excel bestand
TSH_Data_Cleaned_Knime <- read_excel("DAS_DTD_8/TSH_Data_Cleaned_Knime.xlsx")

#Datatabel weergeven in nieuwe tab
View(TSH_Data_Cleaned_Knime)

#Eerste waarden weergeven in de console
head(TSH_Data_Cleaned_Knime)

#Statistische gegevens van de kolom 'TSH'
summary(TSH_Data_Cleaned_Knime$TSH)

#Standaard deviatie van de kolom 'TSH'
sd(TSH_Data_Cleaned_Knime$TSH, na.rm = TRUE)

#Statistische gegevens van de kolom 'FT4'
summary(TSH_Data_Cleaned_Knime$FT4)

#Standaard deviatie van de kolom 'FT4'
sd(TSH_Data_Cleaned_Knime$FT4, na.rm = TRUE)

#Statistische gegevens van de kolom 'FT3'
summary(TSH_Data_Cleaned_Knime$FT3)

#Standaard deviatie van de kolom 'FT3'
sd(TSH_Data_Cleaned_Knime$FT3, na.rm = TRUE)

#Boxplot TSH
TSH_1 <-ggplot(TSH_Data_Cleaned_Knime, aes(y = TSH))
TSH_1 + geom_boxplot()

#Boxplot FT4
FT4_1 <-ggplot(TSH_Data_Cleaned_Knime, aes(y = FT4))
FT4_1 + geom_boxplot() 

#Boxplot FT3
FT3_1 <-ggplot(data = TSH_Data_Cleaned_Knime, aes(y = FT3))
FT3_1 + geom_boxplot()

#Criteria van Chauvenette toepassen op TSH, berekende waarden toegevoegd in een nieuwe kolom genaam 'TSHChauvv'
TSH_Data_Cleaned_Knime$TSHChauvv <- (TSH_Data_Cleaned_Knime[,5]-2.301)/2.813338

#Criteria van Chauvenette toepassen op FT4, berekende waarden toegevoegd in een nieuwe kolom genaam 'FT4Chauva'
TSH_Data_Cleaned_Knime$FT4Chauva <- (TSH_Data_Cleaned_Knime[,6]-16.46)/3.083652

#Criteria van Chauvenette toepassen op FT3, berekende waarden toegevoegd in een nieuwe kolom genaam 'FT3Chauv'
TSH_Data_Cleaned_Knime$FT3Chauv <- (TSH_Data_Cleaned_Knime[,7]-4.668)/1.464226

#Verander NA naar 0
TSH_Data_Cleaned_Knime$TSHChauvv[is.na(TSH_Data_Cleaned_Knime$TSHChauvv)] <- 0
TSH_Data_Cleaned_Knime$FT4Chauva[is.na(TSH_Data_Cleaned_Knime$FT4Chauva)] <- 0
TSH_Data_Cleaned_Knime$FT3Chauv[is.na(TSH_Data_Cleaned_Knime$FT3Chauv)] <- 0

#TSH waarden buiten het kritiekegebied filtered
TSHFilter <- filter (TSH_Data_Cleaned_Knime, TSHChauvv < 3.49, FT4Chauva < 3.49, FT3Chauv < 3.49)

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

geef mij lik M
