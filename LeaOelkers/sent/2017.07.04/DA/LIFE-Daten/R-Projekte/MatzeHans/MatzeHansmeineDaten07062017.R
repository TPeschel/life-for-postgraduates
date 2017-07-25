# Wir lesen die Daten in R ein, dazu ben�tigen wir als erstes das passende Arbeitsverzeichnis.
getwd()
tableGesamt <- read_excel() # Dateipfad oder Name der Datei einf�gen
View(daten) # Ansicht der Daten

#SICs nur einmal anzeigen (Mehrfach ausschlie�en)
unique(daten$SIC)

# Der Befehl describeBy erstelle schon erste deskriptive Betrachtungen einer Variable. Dabei bezieht sich
# der eigentliche Befehl auf das Alter (C_ANTHRO_KH_AGE_REF). nach dem Komma setze ich noch das 
# Geschlecht ein, um die Deskription nach Geschlecht zu teilen (group 1 -> männlich).

describeBy(daten$C_ANTHRO_KH_BMI_ORIG, daten$GESCHLECHT) 
help(describe)

# Das machen wir jetzt noch mit FSH:
describeBy(daten$FSH_S_NUM_VALUE, daten$GESCHLECHT)

# Um mir die TSH- Verteilung besser darstellen zu können, erstelle ich mir ein Histogramm der TSH Werte.
# Dabei lege ich mit xlim den Grenzwert der x- Achse fest (meine Werte haben ein Mittel von 2,64 mit einer
# Standartabweichung von 2,02) und mit ylim den Grenzwert der y- Achse (im Histogramm die freuquency).

hist(daten$FSH_S_NUM_VALUE, xlim = c(0, 15), ylim = c(0, 75), col = "blue", breaks = 300) #Breaks = Balken

# Jetzt kann man schon einfach eine Korrelation erstellen. Ein neuer Wert (cor)wird die Funktion
# cor.test zugewiesen. Dabei lasse ich den BMI mit dem TSH- Wert korrelieren. Es wird "two-sided" gerechnet,
# habe aber noch nicht ganz verstanden, was das bedeutet. Auch über die Art dr Korrelation (pearson,
# kendall oder spearman müssen wir uns nochmal schlau machen)

corBMIFSH <- cor.test(daten$C_ANTHRO_KH_BMI_ORIG, daten$FSH_S_NUM_VALUE, alternative = "two.sided", method = "pearson")
show(corBMIFSH) # Was bedeutet der hier angegebene p- Wert?

# Der p-Wert ist zwar niedrig aber R ist lediglich -0,03. Habe eben jetzt mal schnell irgendwelche Daten genommen,
# könnte also daran liegen. Aber eig müsste ja die Korrelation viel stärker sein.
# Ich erstelle mir zur Verdeutlichung der eventuellen einen Graph.

plot(daten$FSH_S_NUM_VALUE, daten$C_ANTHRO_KH_BMI_ORIG, xlim = c(0, 10))

# Im Plot steigt der BMI deutlich mit dem TSH, scheint also alles klar zu sein.
# Ich möchte mir aber noch eine Regressionsgerade durch den Plot ziehen, um das ganze zu verdeutlichen.


abline(lm(daten$C_ANTHRO_KH_BMI_ORIG~daten$FSH_S_NUM_VALUE), col="red")

# Zur weiteren Verdeutlichung mchen wir jetzt noch einen ggplot:
ggplot (daten, aes (x = C_ANTHRO_KH_BMI_ORIG, y = FSH_S_NUM_VALUE, colour = GESCHLECHT)) + geom_point() + geom_smooth(method=loess) + ylim(0, 10)

# Wie erstelle ich eine Untergruppe?

tableweiblich <- subset(daten, daten$GESCHLECHT==2)
View(tableweiblich)
