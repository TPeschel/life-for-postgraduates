##Skript: Erstellung der Gruppe 4 (tbl.gruppe4)

#Krankheiten: C_DISEASE_TX_FRUEHGEB 
#Freitext-Ausschlüsse: schwere chronische Krankheiten (Tumore, Asthma mit Gluko-Behandlung, etc)
#Medis: Glukocortikoide, Wachstumshormon, Immunsuppressiva
#Fehlenden Pubertätsstatus??

rm( list = ls( ) )
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
setwd("c:/Users/Lea/Desktop/DA/LIFE-Daten/R-Projekte/r20170601/") ##Pfad setzen zu Tabelle, in der LH/FSH schon aussortiert sind
load("data/tbl.weniger.spalten1.Rd")  ##Daten laden
tbl.gruppe4 <- tbl.weniger.spalten1 ## die neue Tabelle für Gruppe 3 benennen
nrow(tbl.gruppe4) ## 2907 Zeilen hat unsere Gruppe vor den Ausschluessen der Flags

## NAs =0, dann protokollieren
# Krankheiten:
tbl.gruppe4$C_DISEASE_TX_FRUEHGEB[is.na(tbl.gruppe4$C_DISEASE_TX_FRUEHGEB)] <- 0


# Medis:
tbl.gruppe4$CHILD_MED_H_GLUCO_CORT[is.na(tbl.gruppe4$CHILD_MED_H_GLUCO_CORT)] <- 0
tbl.gruppe4$CHILD_MED_H_WACHSTUM[is.na(tbl.gruppe4$CHILD_MED_H_WACHSTUM)] <- 0
tbl.gruppe4$CHILD_MED_H_IMMUNSUPP[is.na(tbl.gruppe4$CHILD_MED_H_IMMUNSUPP)] <- 0



##Flags aussortieren

##PubStat: NAs 0 zuweisen, zählen, fehlende Werte (=0) ausschließen
tbl.gruppe4$C_PUB_STAT_PUB_STATUS[is.na(tbl.gruppe4$C_PUB_STAT_PUB_STATUS)] <- 0
sum(tbl.gruppe4$C_PUB_STAT_PUB_STATUS == 0) 
tbl.gruppe4 <- tbl.gruppe4[tbl.gruppe4$C_PUB_STAT_PUB_STATUS != 0,]

nrow(tbl.gruppe4) #2260

#Krankheiten
tbl.gruppe4 <- tbl.gruppe4[tbl.gruppe4$C_DISEASE_TX_FRUEHGEB == 0,]
# #Medis
tbl.gruppe4 <- tbl.gruppe4[tbl.gruppe4$CHILD_MED_H_GLUCO_CORT == 0,]
tbl.gruppe4 <- tbl.gruppe4[tbl.gruppe4$CHILD_MED_H_WACHSTUM == 0,]
tbl.gruppe4 <- tbl.gruppe4[tbl.gruppe4$CHILD_MED_H_IMMUNSUPP== 0,]

nrow(tbl.gruppe4) #2129

##Als excel abspeichern in tabelle
write.xlsx(x = tbl.gruppe4,file = "C:/Users/Lea/Desktop/DA/LIFE-Daten/R-Projekte/r20170601/data/Gruppe4/tabelle.gruppe4.mitFreitext.xlsx") 


##Freitexte 
# in excel tablle freitexte durchgehen
#    -->"tbl.gruppe4.codierteAks"
#Freitexte in R ausschließen
tbl.gruppe4.codierteAKs <- read_excel("C:/Users/Lea/Desktop/DA/LIFE-Daten/R-Projekte/r20170601/data/Gruppe4/tbl.gruppe4.codierteAks.xlsx")
nrow(tbl.gruppe4.codierteAKs) #2129 
tbl.gruppe4.codierteAKs$Aks[is.na(tbl.gruppe4.codierteAKs$Aks)] <- 0
tbl.gruppe4.codierteAKs <- tbl.gruppe4.codierteAKs[tbl.gruppe4.codierteAKs$Aks == 0, ]
nrow(tbl.gruppe4.codierteAKs)#  2221 
##Excel: Tbl.gruppe4.codierteAks --> Gruppe4FreitexteAusgeschlossen.xlsx
write.xlsx(x = tbl.gruppe4.codierteAKs,file = "C:/Users/Lea/Desktop/DA/LIFE-Daten/R-Projekte/r20170601/data/Gruppe4/Gruppe4FreitexteAusgeschlossen.xlsx")


##Analyse mit Mehrfachbesuchen: benenne t3
t3 <- tbl.gruppe4.codierteAKs[ !is.na( tbl.gruppe4.codierteAKs$EDAT ), ] ##fehlendende edats ausschließen# 
nrow(t3) ##2121
t3 %<>%       ## pipe operator wird ausgeführt und an t1 zurückgeschickt
  group_by( SIC ) %>%  ##gruppen nach sics erstellt
  mutate( visit = dense_rank( EDAT ) ) ##mutate hängt neue spalte ran, hier: visit: dem ersten visit wird =1 zugewisesn, dann 1en zählen


##Erstbesuche Zählen
sum( t3$visit == 1, na.rm = T )  #1201 erstbesuche
table(t3$visit,t3$SEX) 

##wieviele männliche/weibliche?
table(t3$SEX)
# 
##plotte gesamtzahl der Besuche nach SEX
ggplot( t3 ) +
  geom_histogram( aes( visit ), stat = "count" ) +
  facet_grid( . ~ SEX ) +
  scale_fill_brewer( type = "qual", palette = 3 ) +
  xlab( "Gesamtzahl Besuche" ) +
  ylab( "Zahl der Probanden" ) +
  theme_bw( )

##Merhfachbesuch:erstelle Altersgruppen : neue spalte erstellen: age categorie
t3$AGE.CATEGORIE <-
  cut(x = t3$AGE,breaks = c(0:20), labels = c(1:20))
#View(t3[,c("SIC", "EDAT", "visit", "AGE", "AGE.CATEGORIE")])

## Mehrfachb.: plotte Anzahlen der Besuche nach Alter/Geschlecht
ggplot(t3) +
  my.theme +
  #scale_fill_manual(name = "Geschlecht", values = c("red", "blue")) +
  xlab("Altersklassen") +
  ylab("Anzahl der Besucher") +
  geom_histogram(aes(AGE.CATEGORIE, fill = as.factor( visit ) ), stat = "count", position = "stack") +
  facet_grid(. ~ SEX)
# ggsave( filename = "BesuchAltersklassen.pdf" )

##Mehrfachb.: wieviele besuche pro pubstat
ggplot(t3) +
  my.theme +
  #scale_fill_manual(name = "Geschlecht", values = c("red", "blue")) +
  xlab("PubStat") +
  ylab("Anzahl der Besucher") +
  geom_histogram(aes(C_PUB_STAT_PUB_STATUS, fill = as.factor( visit ) ), stat = "count", position = "stack") +
  facet_grid(. ~ SEX) #. bedeutet: man könnte noch andere variable nehmen
# ggsave( filename = "AnzahlBesuche~PubStat.pdf" )

fun <-
  function( v ) { return( v[ 1 ] ) }
##visits markieren
tbl.visits <-
  t3 %>%
  group_by(SIC) %>% ##sortieren nach sic
  summarise(VISITS = as.factor(n()),  ##jede gruppe kriegt einen eintrag: visits=n zählt gruppengröße
            SEX = fun(SEX))
#neue Tabelle: visits: zeigt besuche nach SEX?????
(visits <-
    table(tbl.visits$SEX, tbl.visits$VISITS)) ##???tbl macht keinen Sinn: nur 963 erstbesucher, aberl 3 visit kategorien werden angezeigt

# ggplot(tbl.visits) +
#   geom_histogram(aes(VISITS, fill = SEX ), stat = "count" )+
#   facet_grid(SEX~.)
# 
# ggplot(tbl.visits) +
#   geom_histogram(aes(VISITS, fill = SEX ), stat = "count" )+
#   facet_grid(.~SEX)
# 
##Mehrfachb.: AnzahlBesucher~Alter mit Anteil des jeweiligen PubStat, nach SEX
ggplot(t3) +
  my.theme +
  scale_fill_manual(name = "Geschlecht", values = c("red", "blue")) +
  xlab("Altersklassen") +
  ylab("Anzahl der Besucher") +
  geom_histogram(aes(AGE.CATEGORIE, fill = as.factor( C_PUB_STAT_PUB_STATUS ) ), stat = "count", position = "stack") +
  facet_grid(. ~ SEX)  ##übereinander: position="stack", nebeneinander wäre: "dodge"
# ggsave( filename = "AnzahlBesuche~AlterMitPubStat.pdf" )

# ##mit neuer tabelle gruppe 1, visit= der 1-3. besuch  ??
ggplot(t3) +
  my.theme +
  scale_fill_manual(name = "Geschlecht", values = c("red", "blue")) +
  xlab("Altersklassen") +
  ylab("Anzahl der Besuche") +
  geom_histogram(aes(AGE.CATEGORIE, fill = as.factor( C_PUB_STAT_PUB_STATUS ) ), stat = "count", position = "stack") +
  facet_grid(. ~ SEX)  ##übereinander: position="stack", nebeneinander wäre: "dodge"


##4. nur Erstbesucher nehmen: t4
t4<- t3[t3$visit==1,] #links vom Komma: zeilen, rechts splaten (wenn nur (,)= alle spalten nehmen
nrow(t3)
nrow(t4) #1320 Erstbesuche
##in Excel übertragen fertiger DatensatzGruppe4
write.xlsx(x = t4,file = "C:/Users/Lea/Desktop/DA/LIFE-Daten/R-Projekte/r20170601/data/Gruppe4/Gruppe4_fertigerDatensatz.xlsx")

##--> Datensatz Gruppe 4: n= 1320.
## mit: 31 Kontrazeptiva Einnahmen.
t4$CHILD_MED_H_KONTRAZEPT[is.na(t4$CHILD_MED_H_KONTRAZEPT)] <- 0
sum(t4$CHILD_MED_H_KONTRAZEPT == 1) 



