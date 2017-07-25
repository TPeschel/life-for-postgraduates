
## Datenbereinigung Gruppe2, inklusive Analysen mit Mehrfachbesuchern

##AKs: C_DISEASE_TX_SD_ALLG, C_DISEASE_TX_SD_HYPER, C_DISEASE_TX_SD_HYPO, C_DISEASE_TX_FRUEHGEB, C_DISEASE_TX_ENDOKR, 
#      C_DISEASE_TX_DM1, C_DISEASE_TX_DM2, C_DISEASE_TX_BLUT, C_DISEASE_TX_GERIN, C_DISEASE_TX_EPIKRAMPF  
#       und Freitext-Ausschlüsse: schwere chronische Krankheiten (Tumore, Asthma mit Gluko-Behandlung, etc)
#       --> Fruehgeburt, Endokrinologische/metabolische Erkrankungen, Blut/Gerinnungs-Erkrankungen, Epilepsie und schwere chronische EKs 
##AKs Medis: Glukocortikoide, Insulin, Metformin, L-Thyroxin, Stoffwechsel, Hormone, Wachstumshormon, Immunsuppressiva,
#             Neuroleptika, Kontrazeptiva. 


rm( list = ls( ) ) ##Speicher lÃ¶schen
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

tbl.gruppe2 <- tbl.weniger.spalten1 ## die neue Tabelle für Gruppe 2 benennen
nrow(tbl.gruppe2) ## 2907 Zeilen hat unsere Gruppe vor den Ausschluessen der Flags

## NAs =0, dann protokollieren
#für krankheiten:
tbl.gruppe2$C_DISEASE_TX_SD_ALLG[is.na(tbl.gruppe2$C_DISEASE_TX_SD_ALLG)] <- 0
tbl.gruppe2$C_DISEASE_TX_SD_HYPO[is.na(tbl.gruppe2$C_DISEASE_TX_SD_HYPO)] <- 0
tbl.gruppe2$C_DISEASE_TX_SD_HYPER[is.na(tbl.gruppe2$C_DISEASE_TX_SD_HYPER)] <- 0
tbl.gruppe2$C_DISEASE_TX_FRUEHGEB[is.na(tbl.gruppe2$C_DISEASE_TX_FRUEHGEB)] <- 0
tbl.gruppe2$C_DISEASE_TX_ENDOKR[is.na(tbl.gruppe2$C_DISEASE_TX_ENDOKR)] <- 0
tbl.gruppe2$C_DISEASE_TX_DM1[is.na(tbl.gruppe2$C_DISEASE_TX_DM1)] <- 0
tbl.gruppe2$C_DISEASE_TX_DM2[is.na(tbl.gruppe2$C_DISEASE_TX_DM2)] <- 0
tbl.gruppe2$C_DISEASE_TX_BLUT[is.na(tbl.gruppe2$C_DISEASE_TX_BLUT)] <- 0
tbl.gruppe2$C_DISEASE_TX_GERIN[is.na(tbl.gruppe2$C_DISEASE_TX_GERIN)] <- 0
tbl.gruppe2$C_DISEASE_TX_EPIKRAMPF[is.na(tbl.gruppe2$C_DISEASE_TX_EPIKRAMPF)] <- 0


protokoll.Krankheit <-
data.frame(
  Krankheit = as.character( c( "SD_Allg", "SD_Hypo", "Sd_Hyper", "Fruehgeb", "Endokr", "DM1", "DM2", "Blut", "Gerinnung",
                               "Epikrampf")),
                               Anzahl = c(1:10)
  )
protokoll.Krankheit$Anzahl[ protokoll.Krankheit$Krankheit=="SD_Allg" ] <- sum(tbl.gruppe2$C_DISEASE_TX_SD_ALLG == 1)
protokoll.Krankheit$Anzahl[ protokoll.Krankheit$Krankheit=="SD_Hypo" ] <- sum(tbl.gruppe2$C_DISEASE_TX_SD_HYPO == 1)
protokoll.Krankheit$Anzahl[ protokoll.Krankheit$Krankheit=="SD_Hyper" ] <- sum(tbl.gruppe2$C_DISEASE_TX_SD_HYPER == 1)
protokoll.Krankheit$Anzahl[ protokoll.Krankheit$Krankheit=="Fruehgeb" ] <- sum(tbl.gruppe2$C_DISEASE_TX_FRUEHGEB == 1)
protokoll.Krankheit$Anzahl[ protokoll.Krankheit$Krankheit=="Endokr" ] <- sum(tbl.gruppe2$C_DISEASE_TX_ENDOKR == 1)
protokoll.Krankheit$Anzahl[ protokoll.Krankheit$Krankheit=="DM1" ] <- sum(tbl.gruppe2$C_DISEASE_TX_DM1 == 1)
protokoll.Krankheit$Anzahl[ protokoll.Krankheit$Krankheit=="DM2" ] <- sum(tbl.gruppe2$C_DISEASE_TX_DM2 == 1)
protokoll.Krankheit$Anzahl[ protokoll.Krankheit$Krankheit=="Blut" ] <- sum(tbl.gruppe2$C_DISEASE_TX_BLUT == 1)
protokoll.Krankheit$Anzahl[ protokoll.Krankheit$Krankheit=="Gerin" ] <- sum(tbl.gruppe2$C_DISEASE_TX_GERIN == 1)
protokoll.Krankheit$Anzahl[ protokoll.Krankheit$Krankheit=="Epikrampf" ] <- sum(tbl.gruppe2$C_DISEASE_TX_EPIKRAMPF == 1)

protokoll.Krankheit

#Fuer Medis:
tbl.gruppe2$CHILD_MED_H_GLUCO_CORT[is.na(tbl.gruppe2$CHILD_MED_H_GLUCO_CORT)] <- 0
tbl.gruppe2$CHILD_MED_H_INSULIN[is.na(tbl.gruppe2$CHILD_MED_H_INSULIN)] <- 0
tbl.gruppe2$CHILD_MED_H_METFORMIN[is.na(tbl.gruppe2$CHILD_MED_H_METFORMIN)] <- 0
tbl.gruppe2$CHILD_MED_H_LTHYROX[is.na(tbl.gruppe2$CHILD_MED_H_LTHYROX)] <- 0
tbl.gruppe2$CHILD_MED_H_STOFFWECHSEL[is.na(tbl.gruppe2$CHILD_MED_H_STOFFWECHSEL)] <- 0
tbl.gruppe2$CHILD_MED_H_HORMONE[is.na(tbl.gruppe2$CHILD_MED_H_HORMONE)] <- 0
tbl.gruppe2$CHILD_MED_H_WACHSTUM[is.na(tbl.gruppe2$CHILD_MED_H_WACHSTUM)] <- 0
tbl.gruppe2$CHILD_MED_H_IMMUNSUPP[is.na(tbl.gruppe2$CHILD_MED_H_IMMUNSUPP)] <- 0
tbl.gruppe2$CHILD_MED_H_NEUROLEPTIKA[is.na(tbl.gruppe2$CHILD_MED_H_NEUROLEPTIKA)] <- 0
tbl.gruppe2$CHILD_MED_H_KONTRAZEPT[is.na(tbl.gruppe2$CHILD_MED_H_KONTRAZEPT)] <- 0
tbl.gruppe2$CHILD_MED_H_TESTO[is.na(tbl.gruppe2$CHILD_MED_H_TESTO)] <- 0



protokoll.Medikamente <-
  data.frame( 
    Medikamente = as.character( c("Gluko", "Insulin", "Metformin", "L-Thyroxin","Stoffwechsel","Hormone", "Wachstumshormon",
                                  "Immunsuppr", "Neuroleptika", "Kontrazeptiva", "Testosteron")),
    Anzahl = c(1:11)
  )
protokoll.Medikamente$Anzahl[ protokoll.Medikamente$Medikament=="Gluko" ] <- sum(tbl.gruppe2$CHILD_MED_H_GLUCO_CORT == 1)
protokoll.Medikamente$Anzahl[ protokoll.Medikamente$Medikament=="Wachstumshormon" ] <- sum(tbl.gruppe2$CHILD_MED_H_WACHSTUM == 1)
protokoll.Medikamente$Anzahl[ protokoll.Medikamente$Medikament=="Immunsuppr" ] <- sum(tbl.gruppe2$CHILD_MED_H_IMMUNSUPP == 1)
protokoll.Medikamente$Anzahl[ protokoll.Medikamente$Medikament=="Neuroleptika" ] <- sum(tbl.gruppe2$CHILD_MED_H_NEUROLEPTIKA == 1)
protokoll.Medikamente$Anzahl[ protokoll.Medikamente$Medikament=="Hormone" ] <- sum(tbl.disease.gruppe2$CHILD_MED_H_HORMONE == 1)
protokoll.Medikamente$Anzahl[ protokoll.Medikamente$Medikament=="Stwoffwechsel" ] <- sum(tbl.gruppe2$CHILD_MED_H_STOFFWECHSEL == 1)
protokoll.Medikamente$Anzahl[ protokoll.Medikamente$Medikament=="Metformon" ] <- sum(tbl.gruppe2$CHILD_MED_H_METFORMIN == 1)
protokoll.Medikamente$Anzahl[ protokoll.Medikamente$Medikament=="Insulin" ] <- sum(tbl.gruppe2$CHILD_MED_H_INSULIN == 1)
protokoll.Medikamente$Anzahl[ protokoll.Medikamente$Medikament=="L-Thyroxin" ] <- sum(tbl.gruppe2$CHILD_MED_H_LTHYROX == 1)
protokoll.Medikamente$Anzahl[ protokoll.Medikamente$Medikament=="Kontrazeptiva" ] <- sum(tbl.gruppe2$CHILD_MED_H_KONTRAZEPT == 1)
protokoll.Medikamente$Anzahl[ protokoll.Medikamente$Medikament=="Testosteron" ] <- sum(tbl.gruppe2$CHILD_MED_H_TESTO == 1)

protokoll.Medikamente


##Flags aussortieren

#PubStat
##PubStat: NAs 0 zuweisen, zählen, fehlende Werte (=0) ausschließen
tbl.gruppe2$C_PUB_STAT_PUB_STATUS[is.na(tbl.gruppe2$C_PUB_STAT_PUB_STATUS)] <- 0
sum(tbl.gruppe2$C_PUB_STAT_PUB_STATUS == 0) ## 568 fehlende PubStat
tbl.gruppe2 <- tbl.gruppe2[tbl.gruppe2$C_PUB_STAT_PUB_STATUS != 0,] # ungleich 0= (!=), wir nehmen alle 0 raus
# View(tbl.gruppe2[,c("SIC","C_PUB_STAT_PUB_STATUS")]) ##bestimmte spalten aungucken
nrow(tbl.gruppe2)

#Krankheiten
tbl.gruppe2 <- tbl.gruppe2[tbl.gruppe2$C_DISEASE_TX_SD_ALLG == 0,] 
tbl.gruppe2 <- tbl.gruppe2[tbl.gruppe2$C_DISEASE_TX_SD_HYPO == 0,]
tbl.gruppe2 <- tbl.gruppe2[tbl.gruppe2$C_DISEASE_TX_SD_HYPER == 0,]
tbl.gruppe2 <- tbl.gruppe2[tbl.gruppe2$C_DISEASE_TX_FRUEHGEB == 0,]
tbl.gruppe2 <- tbl.gruppe2[tbl.gruppe2$C_DISEASE_TX_ENDOKR == 0,]
tbl.gruppe2 <- tbl.gruppe2[tbl.gruppe2$C_DISEASE_TX_DM1 == 0,]
tbl.gruppe2 <- tbl.gruppe2[tbl.gruppe2$C_DISEASE_TX_DM2 == 0,]
tbl.gruppe2 <- tbl.gruppe2[tbl.gruppe2$C_DISEASE_TX_BLUT == 0,]
tbl.gruppe2 <- tbl.gruppe2[tbl.gruppe2$C_DISEASE_TX_GERIN == 0,]
tbl.gruppe2 <- tbl.gruppe2[tbl.gruppe2$C_DISEASE_TX_EPIKRAMPF == 0,]
nrow(tbl.gruppe2) #2195
#Medis
tbl.gruppe2 <- tbl.gruppe2[tbl.gruppe2$CHILD_MED_H_GLUCO_CORT == 0,]
tbl.gruppe2 <- tbl.gruppe2[tbl.gruppe2$CHILD_MED_H_INSULIN == 0,]
tbl.gruppe2 <- tbl.gruppe2[tbl.gruppe2$CHILD_MED_H_METFORMIN == 0,]
tbl.gruppe2 <- tbl.gruppe2[tbl.gruppe2$CHILD_MED_H_LTHYROX == 0,]
tbl.gruppe2 <- tbl.gruppe2[tbl.gruppe2$CHILD_MED_H_STOFFWECHSEL == 0,]
tbl.gruppe2 <- tbl.gruppe2[tbl.gruppe2$CHILD_MED_H_HORMONE == 0,]
tbl.gruppe2 <- tbl.gruppe2[tbl.gruppe2$CHILD_MED_H_WACHSTUM == 0,]
tbl.gruppe2 <- tbl.gruppe2[tbl.gruppe2$CHILD_MED_H_IMMUNSUPP== 0,]
tbl.gruppe2 <- tbl.gruppe2[tbl.gruppe2$CHILD_MED_H_NEUROLEPTIKA == 0,]
tbl.gruppe2 <- tbl.gruppe2[tbl.gruppe2$CHILD_MED_H_KONTRAZEPT == 0,]
tbl.gruppe2 <- tbl.gruppe2[tbl.gruppe2$CHILD_MED_H_TESTO == 0,]


nrow(tbl.gruppe2) #1910

##Freitexte 
##Als excel abspeichern in tabelle
write.xlsx(x = tbl.gruppe2,file = "data/Gruppe2/tabelle.gruppe2.aussortiert.xlsx") 
## in excel tablle freitexte durchgehen
#    -->"tbl.gruppe2.codierteAks"
#Freitexte in R ausschließen
tbl.gruppe2.codierteAKs <- read_excel("C:/Users/Lea/Desktop/DA/LIFE-Daten/R-Projekte/r20170601/data/Gruppe2/tbl.gruppe2.codierteAKs.xlsx")
nrow(tbl.gruppe2.codierteAKs) #2068
tbl.gruppe2.codierteAKs$Aks[is.na(tbl.gruppe2.codierteAKs$Aks)] <- 0

# ##Flags aussortieren
tbl.gruppe2.codierteAKs <- tbl.gruppe2.codierteAKs[tbl.gruppe2.codierteAKs$Aks == 0, ]
nrow(tbl.gruppe2.codierteAKs)#  2061 
##Excel: Tbl.gruppe2.codierteAks --> Gruppe2FreitexteAusgeschlossen.xlsx
write.xlsx(x = tbl.gruppe2.codierteAKs,file = "C:/Users/Lea/Desktop/DA/LIFE-Daten/R-Projekte/r20170601/data/Gruppe2/Gruppe2FreitexteAusgeschlossen.xlsx")



##Analyse mit Mehrfachbesuchen: benenne t3
t3 <- tbl.gruppe2.codierteAKs[ !is.na( tbl.gruppe2.codierteAKs$EDAT ), ] ##fehlendende edats ausschließen# 
nrow(t3) ##1895
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
nrow(t4) #1229 Erstbesuche
##in Excel übertragen fertiger DatensatzGruppe2
write.xlsx(x = t4,file = "C:/Users/Lea/Desktop/DA/LIFE-Daten/R-Projekte/r20170601/data/Gruppe2/Gruppe2_fertigerDatensatz.xlsx")

h##--> Datensatz Gruppe 2: n=1201.

