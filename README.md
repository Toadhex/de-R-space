#Laden van de libraries 
library(tidyverse)
library(pacman)
library(dplyr)
library(tidyr)
library(ggplot2)
library(data.table)
library(ggpmisc)

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

#Berekenen cumulatieve frequentie TSH
TSHecdf <-ggplot(TSHFilter, aes(x = TSH))
TSHecdf + stat_ecdf()

#Berekenen cumulatieve frequentie FT4
FT4ecdf <-ggplot(TSHFilter, aes(x = FT4))
FT4ecdf + stat_ecdf()

#Berekenen cumulatieve frequentie FT3
FT3ecdf <-ggplot(TSHFilter, aes(x = FT3))
FT3ecdf + stat_ecdf()

#Verander NA naar 0
TSHFilter$TSH[is.na(TSHFilter$TSH)] <- 0
TSHFilter$FT4[is.na(TSHFilter$FT4)] <- 0
TSHFilter$FT3[is.na(TSHFilter$FT3)] <- 0

#TSH waarden buiten het visuele lineaire gebied filtered
TSHFilterCUMFreq <- filter (TSHFilter, FT3 <= 5 & FT3 >= 3.5 | FT3 == 0.00, TSH <= 2.3 & TSH >= 1 | TSH == 0.00, FT4 <= 18 & FT4 >= 13.5 | FT4 == 0.00)

#Verander 0 terug naar NA
TSHFilterCUMFreq <- na_if(TSHFilterCUMFreq, 0)

#Boxplot FT3 na visueel bepalem lineaire deel
FT3LinFil <- ggplot(TSHFilterCUMFreq, aes(y = FT3))
FT3LinFil + geom_boxplot()

#Boxplot TSH na visueel bepalem lineaire deel
TSHLinFil <- ggplot(TSHFilterCUMFreq, aes(y = TSH))
TSHLinFil + geom_boxplot()

#Boxplot FT4 na visueel bepalem lineaire deel
FT4LinFil <- ggplot(TSHFilterCUMFreq, aes(y = FT4))
FT4LinFil + geom_boxplot()

#Cumulatieve frequentietabel van lineaire deel TSH
TSHtab <- table(TSHFilterCUMFreq$TSH, exclude = NULL)
TSHcsum <- cumsum((TSHtab/ 7538)*100)
TSHcsum

#TSH frequentietabel omzetten in dataframe
TSHcsumdata <- data.frame(as.matrix(TSHcsum))
setDT(TSHcsumdata, keep.rownames = TRUE)

#TSH regressielijn
plot(TSHcsumdata)

TSHcsumdata$rn <- as.numeric(as.character(TSHcsumdata$rn))

TSHRegLijn <- ggplot(TSHcsumdata, aes(x = TSHcsum, y = rn)) + geom_point(shape=1) + geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ x)
TSHRegLijn <- TSHRegLijn + scale_x_continuous(name = "TSH waarde (mU/L)") + scale_y_continuous(name = "Cumelatieve frequentie (%)")
TSHRegLijn <- TSHRegLijn + ggtitle("TSH Regressielijn") +  annotate("rect", xmin = 0.00, xmax = 0.1, ymin = -0.056, ymax = -0.044, fill="white", colour="red") + annotate("text", x = 0.05, y = -0.05, label = equation(fit), parse = TRUE)
TSHRegLijn 


TSHRegLijn

fit <- lm(formula = rn ~ as.matrix.TSHcsum., data = TSHcsumdata)
coefficients(fit)




#Cumulatieve frequentietabel van lineaire deel FT4
FT4tab <- table(TSHFilterCUMFreq$FT4, exclude = NULL)
FT4csum <- cumsum((FT4tab/ 7538)*100)
FT4csum

#FT4 frequentietabel omzetten in dataframe
FT4csumdata <- data.frame(as.matrix(FT4csum))
setDT(FT4csumdata, keep.rownames = TRUE)

#FT4 regressielijn
plot(FT4csumdata, na.rm = TRUE)

#Cumulatieve frequentietabel van lineaire deel FT3
FT3tab <- table(TSHFilterCUMFreq$FT3, exclude = NULL)
FT3csum <- cumsum((FT3tab/ 7538)*100)
FT3csum

#FT3 frequentietabel omzetten in dataframe
FT3csumdata <- data.frame(as.matrix(FT3csum))
setDT(FT3csumdata, keep.rownames = TRUE)

#FT3 regressielijn
plot(FT3csumdata)

