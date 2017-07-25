getwd()
setwd("/home/owagner/R/R-Kurs/session3") #Arbeitsverzeichnis auf Session3-Ordner gesetzt

bird <- read.table("data/bird.dat")
summary(bird)
head(bird)


library(Hmisc) #Hmisc geladen um .sav-Datei zu laden.
s1 <- spss.get("data/mz2010_cf.sav")

library(ggplot2)

#Einfacher Barplot
ggplot(s1,aes(x = EF49, fill = EF46)) +
  geom_bar(position = position_fill()) +
  ggtitle("Plot 1")

#Einfacher Scatterplot
ggplot(s1,aes(x = EF44, y = EF20, color = EF46)) +
  geom_point() +
  ggtitle("Plot 2")

#Jitterplot, bei größeren Datenmengen
ggplot(s1,aes(x = EF44, y = EF20, color = EF46)) +
  geom_jitter(alpha = 0.3) + #Transparenz für bessere Darstellung
  ggtitle("Plot 2")

#Verlagert Grafikausgabe in seperates Fenster
x11()


s2 <- spss.get("data/ZA5240_v2-0-0.sav")
dim(s2)

#Boxplot mit verkürzter y-Skala
ggplot(s2,aes(x = V81, y = V417)) +
  geom_boxplot() +
  scale_y_continuous(limits = c(0,10000)) +
  ggtitle("Plot 3")
  
table(s2$V81)

#Mittelwert, ohne NA's, oben und unten 5% "abgeschnitten"
mean(s2$V417, na.rm = T, trim = 0.05)

