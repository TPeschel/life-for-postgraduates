#Potenzrechnung in unterschiedlichen Varianten.
4^3+3^(2+1)
4**3+3**(2+1)

v <- c(2,5,4,10,8) #Vektor v erstellt.

#Unterschiedliche Berechnungen mit v.
v^2
v**2
v - 6
(v - 9)^2


data(ChickWeight)
dim(ChickWeight) #Zeigt Zeilen- und Spaltenanzahl (als Gesamtes) an.
nrow(ChickWeight) #Nur Zeilenanzahl.
ncol(ChickWeight) #Nur Spaltenanzahl.

summary(ChickWeight)
head(ChickWeight) #Erste 6 Zeilen der Tabelle
mean(ChickWeight$weight) #Durchschnitt
min(ChickWeight$weight) #Minimalwert
max(ChickWeight$weight) #Maximalwert

? boxplot #Hilfe zu Boxplots.
plot(ChickWeight$Diet,ChickWeight$weight) 
boxplot(ChickWeight$weight~ChickWeight$Diet) 
boxplot(ChickWeight$Diet,ChickWeight$weight,ChickWeight$Time)
plot(ChickWeight$weight~ChickWeight$Diet) 


getwd() #Zeigt aktuelles Arbeitsverzeichnis.
setwd("/home/owagner/R/R-Kurs/session2") #Ändern des Arbeitsverzeichnis.   
getwd() #Kontrolle
dir() #Anzeige des Ordners


#Packages laden 
library(ggplot2)
library(readxl)
library(dplyr)


#Excel-File mit unterschiedlichen Tabellenblättern laden.
x1 <- read_excel("data/20160523evaluation_Kopie.xlsx",1)
x2 <- read_excel("data/20160523evaluation_Kopie.xlsx",2)
x3 <- read_excel("data/20160523evaluation_Kopie.xlsx",3)
x4 <- read_excel("data/20160523evaluation_Kopie.xlsx",4)

##Anzeigen der Tabellenblätter aus der Excel-Tabelle.
excel_sheets("data/20160523evaluation_Kopie.xlsx")


#Für .sav-Datei
library(Hmisc)

s1 <- spss.get("data/mz2010_cf.sav")
head(s1)
names(s1)

#Spalte Geschlecht in PDF aus Ordner gesucht (Spaltennummer für sav gesucht).
table(s1$EF46)

#Prozente
prop.table(table(s1$EF46))

table(s1$EF310)
table(s1$EF49)

head(s1$EF310)
summary(s1$EF310)

table(s1$EF49,s1$EF310,s1$EF46)


#Pivot-Tabelle
ftable(s1$EF46,s1$EF310)

prop.table(table(s1$EF46,s1$EF310),2)


#Noch mal prüfen
?ggplot
?scale_y_discrete


#Unterschiedliche Plots

ggplot(s1,aes(x = EF310)) +
  geom_bar() +
  scale_x_discrete(labels = c(1:8)) +
  ylab("Anzahl der Personen") +
  xlab("höchster Schulabschluss") +
  ggtitle("Barplot")


ggplot(s1,aes(x = EF310, fill = EF46)) +
  geom_bar() +
  scale_x_discrete() +
  ylab("Anzahl der Personen") +
  xlab("höchster Schulabschluss") +
  ggtitle("Barplot")


ggplot(s1,aes(x = EF310, fill = EF46)) +
  geom_bar() +
  scale_x_discrete(labels = c(1:8)) +
  ylab("Anzahl der Personen") +
  xlab("höchster Schulabschluss") +
  ggtitle("Barplot")


#Unterschiedliche Farben für männlich und weiblich.
ggplot(s1,aes(x = EF310, fill = EF46)) +
  geom_bar() +
  scale_x_discrete(labels = c(1:8)) +
  scale_fill_manual(values = c("männlich" = "midnightblue", 
                               "weiblich" = "deeppink3")) +
  ylab("Anzahl der Personen") +
  xlab("höchster Schulabschluss") +
  ggtitle("Barplot")


#Zwei unterschiedliche Säulen durch position_dodge()
ggplot(s1,aes(x = EF310, fill = EF46)) +
    geom_bar(position = position_dodge()) +
    scale_x_discrete(labels = c(1:8)) +
    scale_fill_manual(values = c("männlich" = "midnightblue", 
                                 "weiblich" = "deeppink3")) +
    ylab("Anzahl der Personen") +
    xlab("höchster Schulabschluss") +
    ggtitle("Barplot")


#Aufgefüllte Leisten durch position_fill() und gedrehte Achsen durch coord_flip()
ggplot(s1,aes(x = EF310, fill = EF46)) +
    geom_bar(position = position_fill()) +
    scale_x_discrete(labels = c(1:8)) +
    scale_fill_manual(values = c("männlich" = "midnightblue", 
                                 "weiblich" = "deeppink3")) +
    ylab("Anzahl der Personen") +
    xlab("höchster Schulabschluss") +
    coord_flip()
    ggtitle("Barplot")