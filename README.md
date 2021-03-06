#Laden van de libraries 
library(tidyverse)
library(pacman)
library(dplyr)
library(tidyr)
library(ggplot2)
library(data.table)
library(ggpmisc)
library(rmarkdown)
library(backports)
library(referenceIntervals)
library(ggpubr)
library(nortest)

#Importeren Excel bestand
TSH <- library(readxl)
url <- "https://github.com/Toadhex/de-R-space/raw/master/TSH.xlsx"
destfile <- "TSH.xlsx"  
curl::curl_download(url, destfile) 
TSH <- read_excel(destfile, col_types = c("text", 
                                          "text", "text", "text", "numeric", "numeric", 
                                          "numeric", "skip"))

#Controleren of TSH normaal is verdeeld m.b.v. qqplot en histogram
qqnorm(TSH$TSH, pch = 1, frame = FALSE, main = "TSH Normal Q-Q Plot")
qqline(TSH$TSH, col = "steelblue", lwd = 2)
hist(TSH$TSH, main = "Histogram of TSH", xlab = "TSH", col="steelblue")
DensTSH <- density(TSH$TSH)
plot(DensTSH, main = "Densityplot of TSH", xlab = "TSH waarde (mU/L)")
polygon(DensTSH, col="steelblue", border="lightblue")

#Controleren of FT4 normaal is verdeeld m.b.v. qqplot en histogram
qqnorm(TSH$FT4, pch = 1, frame = FALSE, main = "FT4 Normal Q-Q Plot")
qqline(TSH$FT4, col = "steelblue", lwd = 2)
hist(TSH$FT4, main = "Histogram of FT4", xlab = "FT4", col="steelblue")
DensFT4 <- density(TSH$FT4, na.rm = TRUE)
plot(DensFT4, main = "Densityplot of FT4", xlab = "FT4 waarde (pmol/L)")
polygon(DensFT4, col="steelblue", border="lightblue")

#Controleren of FT3 normaal is verdeeld m.b.v. qqplot en histogram
qqnorm(TSH$FT3, pch = 1, frame = FALSE, main = "FT3 Normal Q-Q Plot")
qqline(TSH$FT3, col = "steelblue", lwd = 2)
hist(TSH$FT3, main = "Histogram of FT3", xlab = "FT3", col="steelblue")
DensFT3 <- density(TSH$FT3, na.rm = TRUE)
plot(DensFT3, main = "Densityplot of FT3", xlab = "FT3 waarde (pmol/L)")
polygon(DensFT3, col="steelblue", border="lightblue")

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
TSH_1 + geom_boxplot() + scale_y_continuous(name = "TSH waarde (mU/L)") + ggtitle("Boxplot TSH voor toepassen Chauvenette")

#Boxplot FT4
FT4_1 <-ggplot(TSH, aes(y = FT4))
FT4_1 + geom_boxplot() + scale_y_continuous(name = "FT4 waarde (pmol/L)") + ggtitle("Boxplot FT4 voor toepassen Chauvenette")

#Boxplot FT3
FT3_1 <-ggplot(data = TSH, aes(y = FT3))
FT3_1 + geom_boxplot() + scale_y_continuous(name = "FT3 waarde (pmol/L)") + ggtitle("Boxplot FT3 voor toepassen Chauvenette")

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
TSH_2 + geom_boxplot() + scale_y_continuous(name = "TSH waarde (mU/L)") + ggtitle("Boxplot TSH na Criterium van Chauvanet")

#Boxplot FT4 na filter
FT4_2 <-ggplot(TSHFilter, aes(y = FT4))
FT4_2 + geom_boxplot() + scale_y_continuous(name = "FT4 waarde (pmol/L)") + ggtitle("Boxplot FT4 na Criterium van Chauvanet")

#Boxplot FT3 na filter
FT3_2 <-ggplot(TSHFilter, aes(y = FT3))
FT3_2 + geom_boxplot() + scale_y_continuous(name = "FT3 waarde (pmol/L)") + ggtitle("Boxplot FT3 na Criterium van Chauvanet")

#Berekenen cumulatieve frequentie TSH
TSHecdf <-ggplot(TSHFilter, aes(x = TSH))
TSHecdf + stat_ecdf() + scale_y_continuous(name = "Cumulatieve frequentie") + scale_x_continuous(name = "TSH waarde (mU/L)") + ggtitle("Cumulatieve frequentie TSH")

#Berekenen cumulatieve frequentie FT4
FT4ecdf <-ggplot(TSHFilter, aes(x = FT4))
FT4ecdf + stat_ecdf() + scale_y_continuous(name = "Cumulatieve frequentie") + scale_x_continuous(name = "FT4 waarde (pmol/L)") + ggtitle("Cumulatieve frequentie FT4")

#Berekenen cumulatieve frequentie FT3
FT3ecdf <-ggplot(TSHFilter, aes(x = FT3))
FT3ecdf + stat_ecdf() + scale_y_continuous(name = "Cumulatieve frequentie") + scale_x_continuous(name = "FT3 waarde (pmol/L)") + ggtitle("Cumulatieve frequentie FT3")

#Verander NA naar 0
TSHFilter$TSH[is.na(TSHFilter$TSH)] <- 0
TSHFilter$FT4[is.na(TSHFilter$FT4)] <- 0
TSHFilter$FT3[is.na(TSHFilter$FT3)] <- 0

#TSH waarden buiten het visuele lineaire gebied filtered
TSHFilterCUMFreq <- filter (TSHFilter, FT3 <= 4.8 & FT3 >= 4 | FT3 == 0.00, TSH <= 2.3 & TSH >= 1 | TSH == 0.00, FT4 <= 18 & FT4 >= 13.5 | FT4 == 0.00)

#Verander 0 terug naar NA
TSHFilterCUMFreq <- na_if(TSHFilterCUMFreq, 0)

#Boxplot TSH na visueel bepalem lineaire deel
TSHLinFil <- ggplot(TSHFilterCUMFreq, aes(y = TSH))
TSHLinFil + geom_boxplot() + scale_y_continuous(name = "TSH waarde (mU/L)") + ggtitle("Boxplot TSH na Cumulatieve frequentieanalyse")

#Boxplot FT4 na visueel bepalem lineaire deel
FT4LinFil <- ggplot(TSHFilterCUMFreq, aes(y = FT4))
FT4LinFil + geom_boxplot() + scale_y_continuous(name = "FT4 waarde (pmol/L)") + ggtitle("Boxplot FT4 na Cumulatieve frequentieanalyse")

#Boxplot FT3 na visueel bepalem lineaire deel
FT3LinFil <- ggplot(TSHFilterCUMFreq, aes(y = FT3)) + scale_fill_hue(l=40, c=35)
FT3LinFil + geom_boxplot() + scale_y_continuous(name = "FT3 waarde (pmol/L)") + ggtitle("Boxplot FT3 na Cumulatieve frequentieanalyse")

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
TSHRegLijn <- TSHRegLijn + scale_x_continuous(name = "Cumulatieve frequentie") + scale_y_continuous(name = "TSH waarde (mU/L)")
TSHRegLijn <- TSHRegLijn + ggtitle("TSH Regressielijn") + coord_flip()
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
FT4RegLijn <- FT4RegLijn + scale_x_continuous(name = "Cumulatieve frequentie") + scale_y_continuous(name = "FT4 waarde (pmol/L)")
FT4RegLijn <- FT4RegLijn + ggtitle("FT4 Regressielijn") + coord_flip()
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
FT3RegLijn <- FT3RegLijn + scale_x_continuous(name = "Cumulatieve frequentie") + scale_y_continuous(name = "FT3 waarde (pmol/L)")
FT3RegLijn <- FT3RegLijn + ggtitle("FT3 Regressielijn") + coord_flip()
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

#Densiteitsgrafiek van het referentie-interval TSH
DensTSHref <- density(TSHcsumdata$rn)
plot(DensTSHref, main = "Densityplot of TSH reference-interval", xlab = "TSH waarde (mU/L)")
polygon(DensTSHref, col="steelblue", border="lightblue")

#Densiteitsgrafiek van het referentie-interval FT3
DensFT3ref <- density(FT3csumdata$rn)
plot(DensFT3ref, main = "Densityplot of FT3 reference-interval", xlab = "FT3 waarde (pmol/L)")
polygon(DensFT3ref, col="steelblue", border="lightblue")

#Densiteitsgrafiek van het referentie-interval FT4
DensFT4ref <- density(FT4csumdata$rn)
plot(DensFT4ref, main = "Densityplot of FT4 reference-interval", xlab = "FT4 waarde (pmol/L)")
polygon(DensFT4ref, col="steelblue", border="lightblue")

#bepalen referentieintervallen TSH middels Cookmethode
cookTSH <- cook.outliers(TSH$TSH)
cookTSHmax <-max(sapply(cookTSH$subset, max))
cookTSHmax
cookTSHmin <- min(sapply(cookTSH$subset, min))
cookTSHmin
DensCookTSH <- density(cookTSH[["subset"]])
plot(DensCookTSH, main = "Densityplot of Cooks Distance TSH", xlab = "TSH waarde (mU/L)")
polygon(DensCookTSH, col="steelblue", border="lightblue")

#bepalen referentieintervallen FT3 middels Cookmethode
cookFT3 <- cook.outliers(TSH$FT3)
cookFT3max <- max(sapply(cookFT3$subset, max))
cookFT3max
cookFT3min <- min(sapply(cookFT3$subset, min))
cookFT3min
DensCookFT3 <- density(cookFT3[["subset"]], na.rm = TRUE)
plot(DensCookFT3, main = "Densityplot of Cooks Distance FT3", xlab = "FT3 waarde (pmol/L)")
polygon(DensCookFT3, col="steelblue", border="lightblue")

#bepalen referentieintervallen FT4 middels Cookmethode
cookFT4 <- cook.outliers(TSH$FT4)
cookFT4max <- max(sapply(cookFT4$subset, max))
cookFT4max
cookFT4min <- min(sapply(cookFT4$subset, min))
cookFT4min
DensCookFT4 <- density(cookFT4[["subset"]], na.rm = TRUE)
plot(DensCookFT4, main = "Densityplot of Cooks Distance FT4", xlab = "FT4 waarde (pmol/L)")
polygon(DensCookFT4, col="steelblue", border="lightblue")

#bepalen referentieintervallen TSH middels Hornmethode
hornTSH <- horn.outliers(TSH$TSH)
hornTSHmax <- max(sapply(hornTSH$subset, max))
hornTSHmax
hornTSHmin <- min(sapply(hornTSH$subset, min))
hornTSHmin
DensHornTSH <- density(hornTSH[["subset"]])
plot(DensHornTSH, main = "Densityplot of Horn TSH", xlab = "TSH waarde (mU/L)")
polygon(DensHornTSH, col="steelblue", border="lightblue")

#bepalen referentieintervallen FT3 middels Hornmethode
hornFT3 <- horn.outliers(TSH$FT3)
hornFT3max <- max(sapply(hornFT3$subset, max, na.rm = TRUE))
hornFT3max
hornFT3min <- min(sapply(hornFT3$subset, min, na.rm = TRUE))
hornFT3min
DensHornFT3 <- density(hornFT3[["subset"]], na.rm = TRUE)
plot(DensHornFT3, main = "Densityplot of Horn FT3", xlab = "FT3 waarde (pmol/L)")
polygon(DensHornFT3, col="steelblue", border="lightblue")

#bepalen referentieintervallen FT4 middels Hornmethode
hornFT4 <- horn.outliers(TSH$FT4)
hornFT4max <-max(sapply(hornFT4$subset, max, na.rm = TRUE))
hornFT4max
hornFT4min <- min(sapply(hornFT4$subset, min, na.rm = TRUE))
hornFT4min
DensHornFT4 <- density(hornFT4[["subset"]], na.rm = TRUE)
plot(DensHornFT4, main = "Densityplot of Horn FT4", xlab = "FT4 waarde (pmol/L)")
polygon(DensHornFT4, col="steelblue", border="lightblue")

