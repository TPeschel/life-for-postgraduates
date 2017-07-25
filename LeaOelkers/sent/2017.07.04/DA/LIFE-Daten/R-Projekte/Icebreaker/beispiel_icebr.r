##Beispiel Icebreaker:Durchmesser und Größe von Bäumen
##https://cran.r-project.org/doc/contrib/Robinson-icebreaker.pdf
##Datensatz lesen

ufc <- read.csv("ufc.csv")
##sample design parameter and population size for later computation
ufc_baf <- 7
ufc_area <- 300

##examine the structure of the data
str(ufc)
##jede spalte in Tabellenform auflisten
head(ufc)
##Fehlwerte zählen
colSums(is.na(ufc))

## m und cm als Einheit zuweisen --> funktioniert nicht..., für mich: Einheiten zuweisen!
##Heigt war in dm, dbh in mm
ufc$height_m <- ufc$Height/10
ufc$dbh_cm <- ufc$Dbh/10

##dbh in meter umrechnen (mm*1000)
ufc$dbh_m <- ufc$Dbh/1000

##Volumen des Zylinders (pi*r^2 oder pi*r**2)  ausrechnen in m^3
ufc$volumen_m3 <- pi * (ufc$dbh_m/2)**2 * ufc$height_m

##Range des Druchmessers in cm (Spannweite)
range(ufc$dbh_cm)

##Flags markieren (werte die nicht stimmen können), missing value=NA
ufc$height_m[ufc$height_m < 0.1] <- NA
range(ufc$height_m, na.rm=TRUE)
##was bedeutet die range mit true? --> wrong results from mean

##(H�ufigkeits-)Tabelle (table): welche Ziffer gehört zu welchem Baum
table(ufc$Species)
##man kann die Namen ändern und zusammenfassen mit:
##ufc$species[ufc$species %in% c("F","FG")] <- "GF"
##ufc$species <- factor(ufc$species)
##table(ufc$species)

##wieviel NA für jede Spezies? --> bei mir wird ichts angezeigt?
table(ufc$species[is.na(ufc$height_m)])

##Boxplot erstellen: durchmesser/Hoehe in Abh.  von Spezies
boxplot(dbh_cm ~ Species, data=ufc, ylab="Dbh (cm)")
boxplot(height_m ~ Species, data=ufc, ylab="Height (m)")

##Scatterplot Höhe~durchmesser
scatter.smooth(ufc$dbh_cm, ufc$height_m)

##S�ulendiagramm H�ufigkeit eines Merkmals
hist(ufc$dbh_cm, breaks=(0:50) * 2.5, col="darkseagreen3", main="")

##Histogramm: dbh ~ Spezies UND Prozent der Gesamtheit --> 3 Parameter!
histogram( ~ dbh_cm | Species, data = ufc)

##tapply-Funktion: erlaubt die Anwendung beliebiger Funktionen auf Untergruppen von Daten und R�ckgabe der Ergebnisse als Tabelle bzw. Liste.
##d.h. ich will eine Anwendung auf die folgenden 3 Parameter beziehen: dbh, Spezies, H�he
tapply(ufc$dbh_cm, ufc$Species, length)

      