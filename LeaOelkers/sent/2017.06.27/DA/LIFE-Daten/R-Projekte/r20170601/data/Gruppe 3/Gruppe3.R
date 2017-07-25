##Skript: Erstellung der Gruppe 3 (tbl.gruppe3)

#Krankheiten: C_DISEASE_TX_FRUEHGEB 
#Freitext-Ausschlüsse: schwere chronische Krankheiten (Tumore, Asthma mit Gluko-Behandlung, etc)
#Medis: Glukocortikoide, Wachstumshormon, Immunsuppressiva, Kontrazeptiva, Testosteron
#Fehlenden Pubertätsstatus??

rm( list = ls( ) )
setwd("c:/Users/Lea/Desktop/DA/LIFE-Daten/R-Projekte/r20170601/") 
load("data/tbl.weniger.spalten1.Rd") 
tbl.gruppe3 <- tbl.weniger.spalten1 ## die neue Tabelle für Gruppe 3 benennen
nrow(tbl.gruppe3) ## 2907 

# Krankheiten:
tbl.gruppe3$C_DISEASE_TX_FRUEHGEB[is.na(tbl.gruppe3$C_DISEASE_TX_FRUEHGEB)] <- 0 ##ist nur 1 Besucher

protokoll.Krankheit <-
   data.frame(
     Krankheit = as.character( c( "Fruehgeb.")),
     Anzahl = c(1:1)
   )
protokoll.Krankheit$Anzahl[ protokoll.Krankheit$Krankheit=="Fruehgeb" ] <- sum(tbl.gruppe3$C_DISEASE_TX_FRUEHGEB == 1)
protokoll.Krankheit

# Medis:
tbl.gruppe3$CHILD_MED_H_GLUCO_CORT[is.na(tbl.gruppe3$CHILD_MED_H_GLUCO_CORT)] <- 0
tbl.gruppe3$CHILD_MED_H_WACHSTUM[is.na(tbl.gruppe3$CHILD_MED_H_WACHSTUM)] <- 0
tbl.gruppe3$CHILD_MED_H_IMMUNSUPP[is.na(tbl.gruppe3$CHILD_MED_H_IMMUNSUPP)] <- 0
tbl.gruppe3$CHILD_MED_H_KONTRAZEPT[is.na(tbl.gruppe3$CHILD_MED_H_KONTRAZEPT)] <- 0
tbl.gruppe3$CHILD_MED_H_TESTO[is.na(tbl.gruppe3$CHILD_MED_H_TESTO )] <- 0

protokoll.Medikamente <-
  data.frame(
    Medikamente = as.character( c("Gluko", "Wachstumshormon",
                                  "Immunsuppr", "Kontrazeptiva", "Testosteron")),
    Anzahl = c(1:5)
  )
protokoll.Medikamente$Anzahl[ protokoll.Medikamente$Medikament=="Gluko" ] <- sum(tbl.gruppe3$CHILD_MED_H_GLUCO_CORT == 1)
protokoll.Medikamente$Anzahl[ protokoll.Medikamente$Medikament=="Wachstumshormon" ] <- sum(tbl.gruppe3$CHILD_MED_H_WACHSTUM == 1)
protokoll.Medikamente$Anzahl[ protokoll.Medikamente$Medikament=="Immunsuppr" ] <- sum(tbl.gruppe3$CHILD_MED_H_IMMUNSUPP == 1)
protokoll.Medikamente$Anzahl[ protokoll.Medikamente$Medikament=="Kontrazeptiva" ] <- sum(tbl.gruppe3$CHILD_MED_H_KONTRAZEPT == 1)
protokoll.Medikamente$Anzahl[ protokoll.Medikamente$Medikament=="Testosteron" ] <- sum(tbl.gruppe3$CHILD_MED_H_TESTO == 1)

protokoll.Medikamente

##Flags aussortieren
#PubStat:
tbl.gruppe3$C_PUB_STAT_PUB_STATUS[is.na(tbl.gruppe3$C_PUB_STAT_PUB_STATUS)] <- 0
sum(tbl.gruppe3$C_PUB_STAT_PUB_STATUS == 0) ## 647 fehlende PubStat
tbl.gruppe3 <- tbl.gruppe3[tbl.gruppe3$C_PUB_STAT_PUB_STATUS != 0,] # ungleich 0= (!=), wir nehmen alle 0 raus
# View(tbl.gruppe2[,c("SIC","C_PUB_STAT_PUB_STATUS")]) ##bestimmte spalten aungucken
nrow(tbl.gruppe3)

#Krankheiten
tbl.gruppe3 <- tbl.gruppe3[tbl.gruppe3$C_DISEASE_TX_FRUEHGEB == 0,]
nrow(tbl.gruppe3) ##2259
# #Medis
tbl.gruppe3 <- tbl.gruppe3[tbl.gruppe3$CHILD_MED_H_GLUCO_CORT == 0,]
tbl.gruppe3 <- tbl.gruppe3[tbl.gruppe3$CHILD_MED_H_WACHSTUM == 0,]
tbl.gruppe3 <- tbl.gruppe3[tbl.gruppe3$CHILD_MED_H_IMMUNSUPP== 0,]
tbl.gruppe3 <- tbl.gruppe3[tbl.gruppe3$CHILD_MED_H_KONTRAZEPT== 0,]
#tbl.gruppe3 <- tbl.gruppe3[tbl.gruppe3$CHILD_MED_H_TESTO== 0,] ##erzeugt bei Excel probleme und ist schonn ausgeschlossen


nrow(tbl.gruppe3) #2068


##Als excel abspeichern in tabelle
write.xlsx(x = tbl.gruppe3,file = "C:/Users/Lea/Desktop/DA/LIFE-Daten/R-Projekte/r20170601/data/Gruppe 3/tabelle.gruppe3.mitFreitext.xlsx") 


##Freitexte 
# in excel tablle freitexte durchgehen
#    -->"tbl.gruppe3.codierteAks"
#Freitexte in R ausschließen
tbl.gruppe3.codierteAKs <- read_excel("C:/Users/Lea/Desktop/DA/LIFE-Daten/R-Projekte/r20170601/data/Gruppe 3/tbl.gruppe3.codierteAKs.xlsx")
nrow(tbl.gruppe3.codierteAKs) #1910 
tbl.gruppe3.codierteAKs$Aks[is.na(tbl.gruppe3.codierteAKs$Aks)] <- 0
tbl.gruppe3.codierteAKs <- tbl.gruppe3.codierteAKs[tbl.gruppe3.codierteAKs$Aks == 0, ]
nrow(tbl.gruppe3.codierteAKs)#  2061 
##Excel: Tbl.gruppe2.codierteAks --> Gruppe2FreitexteAusgeschlossen.xlsx
write.xlsx(x = tbl.gruppe3.codierteAKs,file = "C:/Users/Lea/Desktop/DA/LIFE-Daten/R-Projekte/r20170601/data/Gruppe 3/Gruppe3FreitexteAusgeschlossen.xlsx")



##Analyse mit Mehrfachbesuchen: benenne t3
t3 <- tbl.gruppe3[ !is.na( tbl.gruppe3$EDAT ), ] ##fehlendende edats ausschließen
# 
t3 %<>%       ## pipe operator wird ausgeführt und an t1 zurückgeschickt
  group_by( SIC ) %>%  ##gruppen nach sics erstellt
  mutate( visit = dense_rank( EDAT ) ) ##mutate hängt neue spalte ran, hier: visit: dem ersten visit wird =1 zugewisesn, dann 1en zählen


##Erstbesuche Zählen
sum( t3$visit == 1, na.rm = T )  #true=erstbesuch =1, false= alle anderen Besuche ##1238 erstbesuche!
nrow(t3) ##insgesamt ohne rausschmiss 2068 besucher
table(t3$visit,t3$SEX) 

##4. nur Erstbesucher nehmen: t4
t4<- t3[t3$visit==1,] #links vom Komma: zeilen, rechts splaten (wenn nur (,)= alle spalten nehmen
nrow(t4) #1296 Erstbesuche

