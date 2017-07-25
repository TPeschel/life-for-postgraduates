#Packages installieren
install.packages("psych")
install.packages("ggplot")
# install.packages('QuantPsyc')
# install.packages("car")
# install.packages("lsr")
# install.packages("Rmisc")
# install.packages("corrplot")
# install.packages("hexbin")
# install.packages("ggsignif")
# install.packages("lavaan")
# install.packages("plyr")
#laden
library(ggplot2)
library(reshape2) #u.a. melt funktion
library(psych)
library(readxl) ##standart excel tbl lesen
library(WriteXLS) ##standart zum schreiben
library( dplyr ) ##group by befehl, summarise,...
library( openxlsx ) ##lesen und schreiben
library(xlsx) ##lesen und schreiben

# library(plyr)
# library(hexbin)
# library(QuantPsyc) # f?r standardised coefficients
# library(car)
# library(lsr)
# library(Rmisc)
# library(corrplot)
# library(lattice)
# library(plyr)
# library(ggsignif)
# library(lavaan)

# bevorzugtes Schema fuer Grafiken
my.theme <-
  theme_bw( )

# Anzahl aller Besuche
# nrow( gesamt.Join.alle.spalten )

# Anzahl der Probanden
# length( unique( gesamt.Join.alle.spalten$SIC ) )

# Altersbereich
# summary(tbl.weniger.spalten1$AGE)

# maennliche und weiliche Besuche mit FSH
#table(tbl.weniger.spalten1$SEX)
 
# Pseudoaggregierungsfunktion fuer die Variable SEX ?
# fun <-
#   function(v)
#     v[1]

# ermittle fuer jede SIC einmal das Geschlecht
# tbl.sex <-
#   tbl.weniger.spalten1 %>%
#   group_by(SIC) %>%
#   summarise(sex = fun(SEX))

# maennliche und weiliche Besucher mit FSH
# table(tbl.sex$sex)


# loesche temporaere Tabelle tbl.sex
# rm("tbl.sex")

# Besuche
# length(tbl.weniger.spalten1$SIC)

# Kinder (Probanden, nicht besuche)
# length(unique(tbl.weniger.spalten1$SIC))

# erstelle Altersgruppen
# tbl.weniger.spalten1$AGE.CATEGORIE <-
#   cut(x = tbl.weniger.spalten1$AGE,
#       c(0:20))

# plotte Anzahlen der Besuche nach Geschlecht
# ggplot(tbl.weniger.spalten1) +
#   my.theme +
#   scale_fill_manual(name = "Geschlecht", values = c("red", "blue")) +
#   xlab("Altersklassen") +
#   ylab("Anzahl der Besuche") +
#   geom_histogram(aes(AGE.CATEGORIE, fill = SEX), stat = "count") +
#   facet_grid(. ~ SEX)

# ermittle fuer jede SIC Nummer der Besuche
# tbl.visits <-
#   tbl.weniger.spalten1 %>%
#   group_by(SIC) %>%
#   mutate(VISIT = as.factor(dense_rank(EDAT)))

# plotte Anzahlen der Kinder nach Geschlecht
# ggplot(tbl.visits) +
#   my.theme +
#   scale_fill_manual(name = "Besuche",
#                     values = c("#ffe0e0", "#ffb0b0", "#ff8080", "#ff5050")) +
#   xlab("Altersklassen") +
#   ylab("Anzahl der Besuche") +
#   geom_histogram(aes(AGE.CATEGORIE, fill = VISIT),
#                  stat = "count",
#                  position = "stack") +
#   facet_grid(. ~ SEX)


# ermittle fuer jede SIC Anzahl der Besuche
# tbl.visits <-
#   tbl.weniger.spalten1 %>%
#   group_by(SIC) %>%
#   summarise(VISITS = n(),
#             SEX = fun(SEX))
# 
# (visits <-
#     table(tbl.visits$SEX, tbl.visits$VISITS))


# plotte Anzahlen der Kinder nach Geschlecht
# ggplot() +
#   my.theme +
#   scale_fill_manual(name = "Geschlecht", values = c("red", "blue", "black")) +
#   xlab("Anzahl der Besuche") +
#   ylab("Anzahl Kinder mit n Besuchen") +
#   geom_histogram(data = tbl.visits, aes(VISITS, fill = SEX), stat = "count") +
#   facet_grid(. ~ tbl.visits$SEX)

##Plot:
# ggsave(filename = "C:/Users/Lea/Desktop/DA/LIFE-Daten/R-Projekte/r20170601/plots/anzahlBesuche.png", width = 9, height = 5 )
#oder setwd(Pfad)
# tbl.weniger.spalten1.curation <-
#   tbl.weniger.spalten1[!is.na(tbl.weniger.spalten1$SIC) &
#                          !is.na(tbl.weniger.spalten1$SCIGROUP), c("SIC", "SCIGROUP", "FSH_S_NUM_VALUE")]

## Boxplot: FSH/LH wert in Abhaengigkeit vom Geschlecht, 
## Variablen aus dem datensatz daten
# boxplot(FSH_S_NUM_VALUE ~ sex, data = daten)
# boxplot(LH_S_NUM_VALUE ~ sex, data = daten)

##Säulendiagramm FSH wert in Abh. vom geschlecht
# histogram( ~  FSH_S_NUM_VALUE | sex, data = daten)
# histogram( ~  LH_S_NUM_VALUE | sex, data = daten)

## Deskription:
#describeBy(tableMatze$C_ANTHRO_KH_AGE_REF, tableMatze$TEILNEHMER_GESCHLECHT)





