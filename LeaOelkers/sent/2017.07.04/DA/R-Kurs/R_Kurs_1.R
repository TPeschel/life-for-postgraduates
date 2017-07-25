#Einfache Berechnungen + Zuweisung von Variablen + Rechnen mit Variablen
sqrt(4)
2 + 2
x <- 2
y <- 2
x + y
x - y

a <- "Wort 1"
b <- "Wort 2"
sentence <- "Hello World"

a + b #Nicht möglich, da a und B Worte sind.
t <- x < 3 #Gibt den Wert TRUE zurück.
v <- c(1,4,2,6,10) #Vektor erstellt.

length (v) #Zeichenlänge des Vektors

#Datentyp einer Variable herausfinden.
typeof(v) #Double
typeof(a) #Character


data(iris)

summary(iris) #Zusammenfassung der Tabelle iris (Min/Max, Quantile, Mean, Median).
head(iris) #Zeigt erste 6 Zeilen der Tabelle iris.
tail(iris,3) #Zeigt letzte 3 Zeilen der Tabelle iris.
names(iris) #Zeigt Spaltennamen der Tabelle iris.
nrow(iris) #Zählt Zeilen der Tabelle iris.
ncol(iris) #Zählt Spalten der Tabelle iris.


plot (iris)
##Scatter-Plot
plot(iris$Sepal.Length,iris$Petal.Length)
##Boxplot
plot(iris$Species,iris$Petal.Length) #Boxplot
##H?ufigkeiten S?ulendiagramm Barchart
plot(iris$Species) #Barchart
#komisches Ding
plot(iris$Petal.Length,iris$Species)
#mit Farbe f?r jede Spezies
plot(iris$Sepal.Length,iris$Petal.Length,col=iris$Species) #Farbe für jede Spezies


letters[20] # Stellt 20. Zeichen des Vektors letters dar.
letters[c(8,5,12,12,15)]

iris[1,1] #Zeigt Wert in [Reihe, Spalte] der Tabelle iris an.
iris[c(1,3),1]

iris[1,] #Zeigt komplette erste Reihe an.
iris[,1] #Zeigt komplette erste Spalte an.

iris[c(11:15),] #Zeile 11-15 dargestellt
iris[c(2,4,100),] #Zeilen 2,4,100 dargestellt