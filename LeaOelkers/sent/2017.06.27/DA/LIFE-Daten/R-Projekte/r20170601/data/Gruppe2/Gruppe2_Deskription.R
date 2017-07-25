##Deskriptive Statistik Gruppe 2, n=1229.

rm( list = ls( ) ) ##Speicher lÃ¶schenren
library(ggplot2)
library(reshape2) #u.a. melt funktion
library(readxl) ##standart excel tbl lesen
library( dplyr ) ##group by befehl, summarise,...
library( openxlsx ) ##lesen und schreiben
library(xlsx) ##lesen und schreiben
library(WriteXLS) ##standart zum schreiben
library(psych) ##für describeBY
my.theme <-  # bevorzugtes Schema fuer Grafiken
  theme_bw( )

setwd("c:/Users/Lea/Desktop/DA/LIFE-Daten/R-Projekte/r20170601/") ##Pfad setzen
t4 <- read_excel("C:/Users/Lea/Desktop/DA/LIFE-Daten/R-Projekte/r20170601/data/Gruppe2/Gruppe2_fertigerDatensatz.xlsx")
nrow(t4)

##Tbl: wieviele PubStat nach SEX?
table(t4$SEX, t4$C_PUB_STAT_PUB_STATUS) ##tabelle um zu gucken wieviele in welchem pubstat

#wieviele male/female?
table(t4$SEX)

##Mittelwert etc des ALters
describeBy(t4$AGE, t4$SEX)

##describe Verschiedenes
describeBy(t4$LH_S_NUM_VALUE)
describeBy(t4$C_ANTHRO_KH_BMI_ORIG)

##Atersverteilung: 
table(t4$SEX)

maennlicheProbanden <- subset(t4, t4$SEX == "male")

hist(maennlicheProbanden$AGE, xlim = c(3, 20), ylim = c(0, 80), col = "blue", breaks = 20)

weiblicheProbanden <- subset(t4, t4$SEX == "female")
hist(weiblicheProbanden$AGE, xlim = c(3, 20), ylim = c(0, 80), col = "blue", breaks = 20)


##nach sex
ggplot(t4) +
  my.theme +
  #scale_fill_manual(name = "Geschlecht", values = c("red", "blue")) +
  xlab("PubStat") +
  ylab("Anzahl der Probanden") +
  geom_histogram(aes(x = as.factor( C_PUB_STAT_PUB_STATUS ) ), stat = "count", position = "stack") +
  facet_grid(. ~ SEX)  ##übereinander: position="stack", nebeneinander wäre: "dodge", ##stat= count: zählt anzhal pro pubstat
facet_grid(. ~ SEX) teilt Zählung nach sex
# ggsave( filename = "AnzahlProbanden~PubStat.pdf" )

##ohne sex aufteilung
ggplot(t4) +
  my.theme +
  #scale_fill_manual(name = "Geschlecht", values = c("red", "blue")) +
  xlab("PubStat") +
  ylab("Anzahl der Probanden") +
  geom_histogram(aes(x = as.factor( C_PUB_STAT_PUB_STATUS ) ), stat = "count", position = "stack")
# ggsave( filename = "AnzahlBesuche~PubStatOhneSEX.pdf" )


##Scatterplots erstellen
# LH~alter  nach sex
ggplot(t4) +
  geom_point(aes(x = AGE, y = LH_S_NUM_VALUE, col=SEX))+                 #geom_point()= setzt punkte
  facet_grid(.~SEX)
# ggsave( filename = "LH~Alter_nachSEX.pdf" )

# oder beide sex zusammen:
ggplot(t4) +
  geom_point(aes(x = AGE, y = LH_S_NUM_VALUE, col=SEX))
# ggsave( filename = "LH~Alter_SexZusammen.pdf" )

##LH ~ Alter, nach Sex und PubStat
ggplot(t4) +
  geom_point(aes(x = AGE, y = LH_S_NUM_VALUE, col=SEX))+
  facet_grid(C_PUB_STAT_PUB_STATUS~SEX )

# LH~Alter nach SEX und PubStat mit limitierterer Y-Achse:
ggplot(t4) +
  geom_point(aes(x = AGE, y = LH_S_NUM_VALUE, col=SEX))+
  facet_grid(C_PUB_STAT_PUB_STATUS~SEX ) +
  ylim( c(0, 10))
# ggsave( filename = "LH~Alter_nachSEXundPubstat.pdf" )

##im selben Plot wie vorher die Adipösen-Falgs markieren
t4$C_DISEASE_TX_ADIP[ is.na( t4$C_DISEASE_TX_ADIP ) ] <- 2

ggplot(t4) +
  geom_point(aes(x = AGE, y = LH_S_NUM_VALUE, col=as.factor(C_DISEASE_TX_ADIP)))+ #as factor: da adipostitas flag 0 oder 1
  facet_grid(C_PUB_STAT_PUB_STATUS~SEX ) +
  scale_color_manual( name = "ADI", values = c( "red", "green", "blue") )+
  ylim( c(0, 10))
# ggsave( filename = "LH~Alter_nachSEXundPubstatAdipöseMarkiert.pdf" )


# # neue spalte für kohorten zuweisung anlegen
t4$Kohorte <- NA
t4$Kohorte[grepl( "A", t4$SCIGROUP, perl = T) ] <- "A-Kohorte"
t4$Kohorte[grepl( "B", t4$SCIGROUP, perl = T) ] <- "B-Kohorte"

ggplot(t4) +
  geom_point(aes(x = AGE, y = LH_S_NUM_VALUE, col=Kohorte))+ #as factor: da adipostitas flag 0 oder 1
  facet_grid(C_PUB_STAT_PUB_STATUS~SEX )
#ggsave( filename = "LH~Alter_nachSEXundPubstat.pdf" )

t4$Kohorte <- NULL ##spalte wieder löschen wenn ich sie nicht mehr brauche
# unique( t4$SCIGROUP)



##Winkler
t4$D00177_SCORE_FAM <- cut(t4$D00177_SCORE_FAM, breaks = c(3, 8, 14, 21))
hist(t4$D00177_SCORE_FAM, breaks=0:22, col="darkseagreen3", main="")
table(t4$D00177_SCORE_FAM)

##Barplot in 3 Winklerkategorien
barplot(table(t4$D00177_SCORE_FAM))
# ggplot(t4) +
#   geom_point(aes(x = AGE, y = LH_S_NUM_VALUE, col=SEX))

#Plot: Anzahl der Probanden nach Winkler
ggplot(t4) +
  my.theme +
  xlab("Winkler-Kategorie") +
  ylab("Anzahl der Probanden") +
  geom_histogram(aes(x = as.factor( D00177_SCORE_FAM ) ), stat = "count", position = "stack")
ggsave( filename = "AnzahlBesuche~Winkler.pdf" )


##lineare Regression LH 
plot(t4$C_ANTHRO_KH_BMI_ORIG, t4$LH_S_NUM_VALUE, xlim = c(0, 50))
#Regressionsgerade
abline(lm(t4$C_ANTHRO_KH_BMI_ORIG~t4$LH_S_NUM_VALUE), col="red")

# ggplot:
ggplot (daten, aes (x = C_ANTHRO_KH_BMI_ORIG, y = LH_S_NUM_VALUE, colour = GESCHLECHT)) + geom_point() + geom_smooth(method=loess) + ylim(0, 10)

## Boxplot: mittlerer LH wert in Abhaengigkeit vom Geschlecht, 
# boxplot(t4$LH_S_NUM_VALUE ~ SEX, data = t4) #sagt kaum was aus


##SÃ¤ulendiagramm LH wert in Abh. vom geschlecht
histogram( ~  LH_S_NUM_VALUE | SEX, data = t4)




