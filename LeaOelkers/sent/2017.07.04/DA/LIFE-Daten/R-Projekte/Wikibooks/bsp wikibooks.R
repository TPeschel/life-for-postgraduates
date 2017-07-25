#Daten einlesen: klappt nicht
load(url("http://www.uni-kiel.de/psychologie/dwoll/r/dat.Rdata")) 
head(subW, n=4)

#allg. Befehl: cor.test(x=(vektor1), y=(vektor2), alternative=c("two.sided", "less", "greater"))
#Daten einlesen, bsp. aus wikibooks
bsp4 <- data.frame( Name=character(), Geschlecht=factor(), Lieblingsfarbe=factor(), Einkommen=numeric())

bsp4 <- structure(list(Name = c("Hans", "Caro", "Lars", "Ines", "Samira", "Peter", "Sarah"), 
                       Geschlecht = structure(c(2L, 3L, 1L, 3L, 3L,2L, 3L), .Label = c("intersexuell", "maennlich", "weiblich"), 
                                              class = "factor"),Lieblingsfarbe = structure(c(3L, 1L, 2L, 4L, 2L, 3L, 1L), .Label = c("blau","gelb", "gruen", "schwarz"), class = "factor"), Einkommen = c(1233,800,2400,4000,899,1100,1900)), .Names = c("Name","Geschlecht", "Lieblingsfarbe", "Einkommen"), row.names = c(NA,7L), class = "data.frame")
#spalte auswählen [,x]
bsp4 [,2]
#Zeile angucken mit Befehl: [x,]
bsp4[3,]
