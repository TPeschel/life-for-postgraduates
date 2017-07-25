# Der erste Schritt ist das Laden einiger hilfreicher Pakete, die Befehle kannst du einfach in deinen
# Editor kopieren
install.packages("psych")
install.packages("ggplot2")
install.packages('QuantPsyc')
install.packages("car")
install.packages("lsr")
install.packages("Rmisc")
install.packages("corrplot")
install.packages("hexbin")
install.packages("ggsignif")
install.packages("lavaan")
install.packages("plyr")
# Nach der Installation muss du die Pakete zum Beginn jeder Sitzung laden!
library(plyr)
library(hexbin)
library(psych)
library(ggplot2)
library(QuantPsyc) # f?r standardised coefficients
library(car)
library(lsr)
library(Rmisc)
library(corrplot)
library(ggplot2)
library(reshape)
library(lattice)
library(plyr)
library(ggsignif)
library(lavaan)
library(readxl)

# Ich erstelle eine Tabelle mit dem Befehl read_excel und der Zuweisung auf den Namen tableMatze. Den
# Pfad zur Datei habe ich einfach aus der Information des Derivats hier eingefügt.
tableMatze <- read_excel("⧵Users⧵Hans⧵Documents⧵Uni⧵Doktorarbeit⧵DieArbeitII⧵LifE⧵R⧵data")
tableMatze <- read_excel("PV0365_Gesamt_Join.xlsx")
setwd("/Users/Hans/Documents/Uni/Doktorarbeit/DieArbeitII/LIfE/R/data")
tableMatze <- read_excel("PV0365_Gesamt_Join.xlsx")

# Mit dem Befehl unique und dem anzeigen der geforderten Variable kann ich alle doppelten SICs
# ausschließen.
unique(tableMatze$TEILNEHMER_SIC)

# Der Befehl describe erstelle schon erste deskriptive Betrachtungen einer Variable. Dabei bezieht sich
# der eigentliche Befehl auf das Alter (C_ANTHRO_KH_AGE_REF). nach dem Komma setze ich noch das 
# Geschlecht ein, um die Deskription nach Geschlecht zu teilen (group 1 -> männlich).

describeBy(tableMatze$C_ANTHRO_KH_AGE_REF, tableMatze$TEILNEHMER_GESCHLECHT)

# Das probiere ich jetzt noch mit dem BMI und dem TSH.

describeBy(tableMatze$C_ANTHRO_KH_BMI_ORIG, tableMatze$TEILNEHMER_GESCHLECHT)
describeBy(tableMatze$TSH_S_NUM_VALUE, tableMatze$TEILNEHMER_GESCHLECHT)

# Um mir die TSH- Verteilung besser darstellen zu können, erstelle ich mir ein Histogramm der TSH Werte.
# Dabei lege ich mit xlim den Grenzwert der x- Achse fest (meine Werte haben ein Mittel von 2,64 mit einer
# Standartabweichung von 2,02) und mit ylim den Grenzwert der y- Achse (im Histogramm die freuquency).
#altersverteilungdescribeBy(tableMatze$C_ANTHRO_KH_BMI_ORIG, tableMatze$TEILNEHMER_GESCHLECHT)
hist(tableMatze$TSH_S_NUM_VALUE, xlim = c(0, 10), ylim = c(0, 700), col = "blue", breaks = 500)

#BMI_schilddrüse
#log transformation von TSH Werten?
# histogramm TSH Werte
hist(tableMatze$TSH_S_NUM_VALUE,
     xlim = c(0, 80), 
     ylim = c(0,150), 
     xlab = "TSH", 
     main = "Histo", 
     col = "lightblue")) # vorherhiger Versuch der Erstellung hat leider nicht geklappt

# Erster Überblick über den BMI und die TSH- Werte meiner Probanden
tableMatze$C_ANTHRO_KH_BMI_ORIG 
tableMatze$TSH_S_RAW_VALUE

# Jetzt kann man schon einfach eine Korrelation erstellen. Ein neuer Wert (cor)wird die Funktion
# cor.test zugewiesen. Dabei lasse ich den BMI mit dem TSH- Wert korrelieren. Es wird "two-sided" gerechnet,
# habe aber noch nicht ganz verstanden, was das bedeutet. Auch über die Art dr Korrelation (pearson,
# kendall oder spearman müssen wir uns nochmal schlau machen)

cor <- cor.test(tableMatze$C_ANTHRO_KH_BMI_ORIG, tableMatze$TSH_S_NUM_VALUE, alternative = "two.sided", method = "pearson")
 
# Der p-Wert ist zwar niedrig aber R ist lediglich -0,03. Habe eben jetzt mal schnell irgendwelche Daten genommen,
# könnte also daran liegen. Aber eig müsste ja die Korrelation viel stärker sein.
# Ich erstelle mir zur Verdeutlichung der eventuellen einen Graph.

plot(tableMatze$C_ANTHRO_KH_BMI_ORIG, tableMatze$TSH_S_NUM_VALUE, ylim = c(0, 10)) 

# Im Plot steigt der BMI deutlich mit dem TSH, scheint also alles klar zu sein.
# Ich möchte mir aber noch eine Regressionsgerade durch den Plot ziehen, um das ganze zu verdeutlichen.


abline(lm(tableMatze$C_ANTHRO_KH_BMI_ORIG~tableMatze$TSH_S_NUM_VALUE), col="red")

# Was leider nicht funktioniert^^ Nach Matzes Meinung "geht dieser Scheiß oft nicht" 
# Wir versuchen das Regressionsmodell anders aufzubauen, damit wir dann vielleicht die Gerade
# durchziehen können. Funktioniert aber leider immer noch nicht.
#regression BMI und TSH

z <- lm(C_ANTHRO_KH_BMI_ORIG ~ TSH_S_NUM_VALUE, data = tableMatze) #regressionsmodell
plot(tableMatze$C_ANTHRO_KH_BMI_ORIG, tableMatze$TSH_S_NUM_VALUE, ylim = c(0, 10)) 

# Funktioniert aber leider immer noch nicht. Matze wurde ab diesem Punkt langsam aggro.
abline(z) #das soll eigtl die regressionsgerade sein
abline(coef = coef(z)) #das auch
summary(z) # das gibt den Modelloutput der Regression

# Danach wollen wir uns noch einen ggplot der Funktion erstellen. Aber irgendwie kann ich das 
# Paket bei mir nicht installieren. Muss ich mal mit Thomas bequatschen.

ggplot (tableMatze, aes (x = C_ANTHRO_KH_BMI_ORIG, y = TSH_S_NUM_VALUE, colour = TEILNEHMER_GESCHLECHT)) + geom_point() + geom_smooth(method=loess) + ylim(0, 10)
tableMatze$TSH_S_NUM_VALUE

# Das war alles, falls du mal nicht weiter weißt, benutz auch immer mal den Befehl help.
# Bis dann Liebling:*
describeBy(tableMatze$TSH_S_NUM_VALUE, tableMatze$TEILNEHMER_GESCHLECHT)
help(cor)
help(two-sided)
