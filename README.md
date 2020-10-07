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
TSHFilterCUMFreq <- filter (TSHFilter, FT3 <= 4.8 & FT3 >= 4 | FT3 == 0.00, TSH <= 2.3 & TSH >= 1 | TSH == 0.00, FT4 <= 18 & FT4 >= 13.5 | FT4 == 0.00)

#Verander 0 terug naar NA
TSHFilterCUMFreq <- na_if(TSHFilterCUMFreq, 0)

#Boxplot FT3 na visueel bepalem lineaire deel
FT3LinFil <- ggplot(TSHFilterCUMFreq, aes(y = FT3)) + scale_fill_hue(l=40, c=35)
FT3LinFil + geom_boxplot() 


#Boxplot TSH na visueel bepalem lineaire deel
TSHLinFil <- ggplot(TSHFilterCUMFreq, aes(y = TSH))
TSHLinFil + geom_boxplot()

#Boxplot FT4 na visueel bepalem lineaire deel
FT4LinFil <- ggplot(TSHFilterCUMFreq, aes(y = FT4))
FT4LinFil + geom_boxplot()

#Aanmaken ggplotRegressiefunctie
ggplotRegression <- function(fit){
  
  require(ggplot2)
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(subtitle = paste("R2 = ",signif(summary(fit)$adj.r.squared, 5),
                          "       Intercept =",signif(fit$coef[[1]],5 ),
                          "       Slope =",signif(fit$coef[[2]], 5)))
}

#Cumulatieve frequentietabel van lineaire deel TSH
TSHtab <- table(TSHFilterCUMFreq$TSH, exclude = NULL)
TSHcsum <- cumsum((TSHtab/ 7480))
TSHcsum

#TSH frequentietabel omzetten in dataframe
TSHcsumdata <- data.frame(as.matrix(TSHcsum))
setDT(TSHcsumdata, keep.rownames = TRUE)

#TSH regressielijn plotten
TSHcsumdata$rn <- as.numeric(as.character(TSHcsumdata$rn))

TSHRegLijn <- ggplotRegression(lm(rn ~ as.matrix.TSHcsum., data = TSHcsumdata))
TSHRegLijn <- TSHRegLijn + scale_x_continuous(name = "Cumelatieve frequentie") + scale_y_continuous(name = "TSH waarde (mU/L)")
TSHRegLijn <- TSHRegLijn + ggtitle("TSH Regressielijn")
TSHRegLijn 

#Dataset met alleen FT4 aanmaken
FT4waardes <- TSHFilterCUMFreq$FT4
view(FT4waardes)
FT4waardes <- na.omit(FT4waardes)

#Cumulatieve frequentietabel van lineaire deel FT4
FT4tab <- table(FT4waardes)
FT4csum <- cumsum((FT4tab/ 5565))
FT4csum
view(FT4csum)
view(FT4tab)

#FT4 frequentietabel omzetten in dataframe
FT4csumdata <- data.frame(as.matrix(FT4csum))
setDT(FT4csumdata, keep.rownames = TRUE)

#FT4regressielijn plotten
FT4csumdata$rn <- as.numeric(as.character(FT4csumdata$rn))

FT4RegLijn <- ggplotRegression(lm(rn ~ as.matrix.FT4csum., data = FT4csumdata))
FT4RegLijn <- FT4RegLijn + scale_x_continuous(name = "Cumelatieve frequentie") + scale_y_continuous(name = "FT4 waarde (mU/L)")
FT4RegLijn <- FT4RegLijn + ggtitle("FT4 Regressielijn") 
FT4RegLijn 

#Dataset met alleen FT3 aanmaken
FT3waardes <- TSHFilterCUMFreq$FT3
FT3waardes <- na.omit(FT3waardes)
view(FT3waardes)

#Cumulatieve frequentietabel van lineaire deel FT3
FT3tab <- table(FT3waardes)
FT3csum <- cumsum((FT3tab/ 137))
FT3csum
view(FT3csumdata)
view(FT3tab)

#FT3 frequentietabel omzetten in dataframe
FT3csumdata <- data.frame(as.matrix(FT3csum))
setDT(FT3csumdata, keep.rownames = TRUE)

#FT3regressielijn plotten
FT3csumdata$rn <- as.numeric(as.character(FT3csumdata$rn))

FT3RegLijn <-ggplotRegression(lm(rn ~ as.matrix.FT3csum., data = FT3csumdata))
FT3RegLijn <- FT3RegLijn + scale_x_continuous(name = "Cumelatieve frequentie") + scale_y_continuous(name = "FT3 waarde (mU/L)")
FT3RegLijn <- FT3RegLijn + ggtitle("FT3 Regressielijn") 
FT3RegLijn 

#Reference Interval Minimum TSH 
TSHRImin <- (1.2695*0.025) + 1.003
TSHRImin

#Reference Interval Maximum TSH 
TSHRImax <- (1.2695*0.975) + 1.003
TSHRImax

#Reference Interval Minimum FT4 
FT4RImin <- (4.2295*0.025) + 13.714
FT4RImin

#Reference Interval Maximum FT4 
FT4RImax <- (4.2295*0.975) + 13.714
FT4RImax

#Reference Interval Minimum FT3 
FT3RImin <- (0.77639*0.025) + 4.0912
FT3RImin 

#Reference Interval Maximum FT3 
FT3RImax <- (0.77639*0.975) + 4.0912
FT3RImax

#RCV perccntage TSH
TSHRCV <- ((2^0.5)*(1.96)*(((0.15^2)+(0.193^2))^0.5))*100
TSHRCV

#RCV perccntage FT4
FT4RCV <- ((2^0.5)*(1.96)*(((0.16^2)+(0.057^2))^0.5))*100
FT4RCV

#RCV perccntage FT3
FT3RCV <- ((2^0.5)*(1.96)*(((0.24^2)+(0.079^2))^0.5))*100
FT3RCV

#TSHmin afwijking met literatuur
TSHminafw <- ((0.3/ TSHRImin)*100)
TSHminafw

#TSHmax afwijking met literatuur
TSHmaxafw <- ((TSHRImax/ 4)*100)
TSHmaxafw

#FT4min afwijking met literatuur
FT4minafw <- ((10/ FT4RImin)*100)
FT4minafw

#FT4max afwijking met literatuur
FT4maxafw <- ((FT4RImax/ 24)*100)
FT4maxafw

#FT3min afwijking met literatuur
FT3minafw <- ((3.5/ FT3RImin)*100)
FT3minafw

#FT3max afwijking met literatuur
FT3maxafw <- ((FT3RImax/ 6.5)*100)
FT3maxafw

