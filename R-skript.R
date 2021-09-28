library(readr)
library(ggplot2)
library(dplyr)
library(mosaic)

setwd("/Users/marcb/Documents/Private tutoring/R-Seminararbeiten/CO2/")

data <- read_delim("Untitled.csv", 
                            ";", escape_double = FALSE, trim_ws = TRUE)


GDP <- read_delim("GDP.csv", ";", escape_double = FALSE, 
                     trim_ws = TRUE)

data <- data[,-(5:7)]

data$food_category <- as.factor(data$food_category)

levels(data$food_category)



fleisch <- data[data$food_category %in% c("Pork", "Poultry","Beef","Lamb & Goat","Fish"), ]    

vege_vega <- data[ !(data$food_category %in% fleisch$food_category), ]

nrow(data)
nrow(fleisch)
nrow(vege_vega)

#  nrow of data = 1430 / nrow fleisch 650 / nrow vege_vegga 780 -> 650+780=1430 

# Summe der Emissionen geteilt durch die Anzahl der Länder

fleisch_co2<- (sum(fleisch$co2_emmission)/130)
vege_vega_co2 <- (sum(vege_vega$co2_emmission)/130)

co2 <- c(fleisch_co2, vege_vega_co2)

co2 <- round(co2, digits = 0)

#Durchschnittliche Emission pro Person weltweit 

ylim <- c(0, 1.1*max(co2))




xx <- barplot(co2, main="CO2 Emissionen für Lebensmittel", 
        col = c("darksalmon","palegreen4"), xlab = "pro Jahr",ylab="Kg pro Person",  ylim=ylim)
legend("topright", 
       legend = c("Fleisch & Fisch", "Vegetarisch & Vegan"), 
       fill = c("darksalmon", "palegreen4"))
text(x = xx, y = co2, label = co2, pos = 3, cex = 1, col = "black")



fleisch_con<- (sum(fleisch$consumption)/130)
vege_vega_con <- (sum(vege_vega$consumption)/130)
  
con <- c(fleisch_con, vege_vega_con)

con <- round(con,digits = 0)

ylimm <- c(0, 1.1*max(con))

yy<- barplot(con, main="Durchschnittlicher Nahrungsmittelkonsum", 
        col = c("darksalmon","palegreen4"), xlab = "pro Jahr", ylab="Kg pro Person", ylim = ylimm)
legend("topleft", 
       legend = c("Fleisch & Fisch", "Vegetarisch & Vegan"), 
       fill = c("darksalmon", "palegreen4"))

text(x = yy, y = con, label = con, pos = 3, cex = 1, col = "black")



# t-test für Mittelwerte

t.test(fleisch$co2_emmission, vege_vega$co2_emmission, alternative = "greater")


Deutschland<- subset(data, data$country == "Germany")
China <- subset(data, data$country == "China")
Brasilen <- subset(data,data$country=="Brazil")
USA <- subset(data,data$country=="USA")
Australien <- subset(data,data$country=="Australia")
Nigeria <- subset(data,data$country=="Nigeria")


Deutschland <- sum(Deutschland$co2_emmission)
China <- sum(China$co2_emmission)
Brasilen <- sum(Brasilen$co2_emmission)
USA <- sum(USA$co2_emmission)
Australien <- sum(Australien$co2_emmission)
Nigeria <- sum(Nigeria$co2_emmission)

weltweit <- c(Deutschland,China,Brasilen,USA,Australien,Nigeria)

Legende <- c("Deutschland","China","Brasilen","USA","Australien","Nigeria")

welt <- data.frame(Legende, weltweit)

View(welt)


d <- ggplot(welt, aes(x=Legende, y=weltweit, fill=Legende)) + 
  geom_bar(stat="identity")+
  labs(x = "Länder", y = "Kg pro Person/Jahr", title = "Durschnittliche weltweite CO2 Emission durch Nahrungsmittel")+
  theme_light()+
  scale_fill_manual(values=c("deepskyblue4","seagreen3", "red4","black","darksalmon","dodgerblue", name = "Lnde"))

d


sum_country<- aggregate(cbind(data$co2_emmission) ~ data$country, data = data, sum)

dd<- data.frame(sum_country,GDP)

View(dd)



hist(dd$GDP)

dd$GDP_l <- log(dd$GDP)

hist(dd$GDP_l)

hist(dd$V1)


modelI <- lm(dd$V1~dd$GDP_l, data=dd)

summary(modelI)


plot(dd$V1~dd$GDP_l, main="Streudiagramm GDP & CO2 Emissionen Jahr 2018", 
     xlab="log GDP", ylab="Durchschnittliche Emmission", pch=19)
 abline(lm(dd$V1~dd$GDP_l), col="red") 
 
 
 
 
 
 


