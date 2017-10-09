rm( list = ls( ) )

# installiere devtools, falls noch nicht geschehen
if( !"devtools" %in% rownames( installed.packages( ) ) ) install.packages( "devtools" )

# installiere neueste Version von helperForLife, falls noch nicht geschehen
devtools::install_github( "TPeschel/hlpr4life" )

hlpr4life::load.pkgs(
    c(
        "hlpr4life",
        "dplyr",
        "reshape2",
        "readxl",
        "openxlsx",
        "ggplot2") )   ##openxlsx ist viel schneller als xlsx, WriteXLS ist nicht gut


# setze Zeitzone auf Berlin Mean Time
Sys.setenv( TZ = "Europe/Berlin" ) 

##
# setze Pfad zu aktuellem R-Arbeitsverzeichnis, Pfad zum Laden
# setwd( "~/LIFE/life-for-postgraduates/LeaOelkers/AllesNeu20170725/data/original/" )
setwd( "c:/Users/Lea/Desktop/AllesNeu20170725/data/generated/" )  ##Pfad in dem die original Excel tabl stehen

load("main.table.Rd")

nrow(tbl)

# unique(tbl$SCI_GROUP) ##alle unterschiedlichen SCI groups anzeigen

## NAs =0, dann protokollieren
#f?r krankheiten:
tbl$C_DISEASE_TX_SD_ALLG[is.na(tbl$C_DISEASE_TX_SD_ALLG)] <- 0
tbl$C_DISEASE_TX_SD_HYPO[is.na(tbl$C_DISEASE_TX_SD_HYPO)] <- 0
tbl$C_DISEASE_TX_SD_HYPER[is.na(tbl$C_DISEASE_TX_SD_HYPER)] <- 0
tbl$C_DISEASE_TX_FRUEHGEB[is.na(tbl$C_DISEASE_TX_FRUEHGEB)] <- 0
tbl$C_DISEASE_TX_ENDOKR[is.na(tbl$C_DISEASE_TX_ENDOKR)] <- 0
tbl$C_DISEASE_TX_DM1[is.na(tbl$C_DISEASE_TX_DM1)] <- 0
tbl$C_DISEASE_TX_DM2[is.na(tbl$C_DISEASE_TX_DM2)] <- 0
tbl$C_DISEASE_TX_BLUT[is.na(tbl$C_DISEASE_TX_BLUT)] <- 0
tbl$C_DISEASE_TX_GERIN[is.na(tbl$C_DISEASE_TX_GERIN)] <- 0
tbl$C_DISEASE_TX_EPIKRAMPF[is.na(tbl$C_DISEASE_TX_EPIKRAMPF)] <- 0


protokoll.Krankheit <-
    data.frame(
        Krankheit = as.character( c( "SD_Allg", "SD_Hypo", "Sd_Hyper", "Fruehgeb", "Endokr", "DM1", "DM2", "Blut", "Gerinnung",
                                     "Epikrampf")),
        Anzahl = c(1:10) )
protokoll.Krankheit$Anzahl[ protokoll.Krankheit$Krankheit=="SD_Allg" ] <- sum(tbl$C_DISEASE_TX_SD_ALLG == 1)
protokoll.Krankheit$Anzahl[ protokoll.Krankheit$Krankheit=="SD_Hypo" ] <- sum(tbl$C_DISEASE_TX_SD_HYPO == 1)
protokoll.Krankheit$Anzahl[ protokoll.Krankheit$Krankheit=="SD_Hyper" ] <- sum(tbl$C_DISEASE_TX_SD_HYPER == 1)
protokoll.Krankheit$Anzahl[ protokoll.Krankheit$Krankheit=="Fruehgeb" ] <- sum(tbl$C_DISEASE_TX_FRUEHGEB == 1)
protokoll.Krankheit$Anzahl[ protokoll.Krankheit$Krankheit=="Endokr" ] <- sum(tbl$C_DISEASE_TX_ENDOKR == 1)
protokoll.Krankheit$Anzahl[ protokoll.Krankheit$Krankheit=="DM1" ] <- sum(tbl$C_DISEASE_TX_DM1 == 1)
protokoll.Krankheit$Anzahl[ protokoll.Krankheit$Krankheit=="DM2" ] <- sum(tbl$C_DISEASE_TX_DM2 == 1)
protokoll.Krankheit$Anzahl[ protokoll.Krankheit$Krankheit=="Blut" ] <- sum(tbl$C_DISEASE_TX_BLUT == 1)
protokoll.Krankheit$Anzahl[ protokoll.Krankheit$Krankheit=="Gerin" ] <- sum(tbl$C_DISEASE_TX_GERIN == 1)
protokoll.Krankheit$Anzahl[ protokoll.Krankheit$Krankheit=="Epikrampf" ] <- sum(tbl$C_DISEASE_TX_EPIKRAMPF == 1)

protokoll.Krankheit

#Fuer Medis:
tbl$CHILD_MED_H_GLUCO_CORT[is.na(tbl$CHILD_MED_H_GLUCO_CORT)] <- 0
tbl$CHILD_MED_H_INSULIN[is.na(tbl$CHILD_MED_H_INSULIN)] <- 0
tbl$CHILD_MED_H_METFORMIN[is.na(tbl$CHILD_MED_H_METFORMIN)] <- 0
tbl$CHILD_MED_H_LTHYROX[is.na(tbl$CHILD_MED_H_LTHYROX)] <- 0
tbl$CHILD_MED_H_STOFFWECHSEL[is.na(tbl$CHILD_MED_H_STOFFWECHSEL)] <- 0
tbl$CHILD_MED_H_HORMONE[is.na(tbl$CHILD_MED_H_HORMONE)] <- 0
tbl$CHILD_MED_H_WACHSTUM[is.na(tbl$CHILD_MED_H_WACHSTUM)] <- 0
tbl$CHILD_MED_H_IMMUNSUPP[is.na(tbl$CHILD_MED_H_IMMUNSUPP)] <- 0
tbl$CHILD_MED_H_NEUROLEPTIKA[is.na(tbl$CHILD_MED_H_NEUROLEPTIKA)] <- 0
tbl$CHILD_MED_H_KONTRAZEPT[is.na(tbl$CHILD_MED_H_KONTRAZEPT)] <- 0
tbl$CHILD_MED_H_TESTO[is.na(tbl$CHILD_MED_H_TESTO)] <- 0



protokoll.Medikamente <-
    data.frame( 
        Medikamente = as.character( c("Gluko", "Insulin", "Metformin", "L-Thyroxin","Stoffwechsel","Hormone", "Wachstumshormon",
                                      "Immunsuppr", "Neuroleptika", "Kontrazeptiva", "Testosteron")),
        Anzahl = c(1:11)
    )
protokoll.Medikamente$Anzahl[ protokoll.Medikamente$Medikament=="Gluko" ] <- sum(tbl$CHILD_MED_H_GLUCO_CORT == 1)
protokoll.Medikamente$Anzahl[ protokoll.Medikamente$Medikament=="Wachstumshormon" ] <- sum(tbl$CHILD_MED_H_WACHSTUM == 1)
protokoll.Medikamente$Anzahl[ protokoll.Medikamente$Medikament=="Immunsuppr" ] <- sum(tbl$CHILD_MED_H_IMMUNSUPP == 1)
protokoll.Medikamente$Anzahl[ protokoll.Medikamente$Medikament=="Neuroleptika" ] <- sum(tbl$CHILD_MED_H_NEUROLEPTIKA == 1)
protokoll.Medikamente$Anzahl[ protokoll.Medikamente$Medikament=="Hormone" ] <- sum(tbl$CHILD_MED_H_HORMONE == 1)
protokoll.Medikamente$Anzahl[ protokoll.Medikamente$Medikament=="Stwoffwechsel" ] <- sum(tbl$CHILD_MED_H_STOFFWECHSEL == 1)
protokoll.Medikamente$Anzahl[ protokoll.Medikamente$Medikament=="Metformon" ] <- sum(tbl$CHILD_MED_H_METFORMIN == 1)
protokoll.Medikamente$Anzahl[ protokoll.Medikamente$Medikament=="Insulin" ] <- sum(tbl$CHILD_MED_H_INSULIN == 1)
protokoll.Medikamente$Anzahl[ protokoll.Medikamente$Medikament=="L-Thyroxin" ] <- sum(tbl$CHILD_MED_H_LTHYROX == 1)
protokoll.Medikamente$Anzahl[ protokoll.Medikamente$Medikament=="Kontrazeptiva" ] <- sum(tbl$CHILD_MED_H_KONTRAZEPT == 1)
protokoll.Medikamente$Anzahl[ protokoll.Medikamente$Medikament=="Testosteron" ] <- sum(tbl$CHILD_MED_H_TESTO == 1)

protokoll.Medikamente


##Flags aussortieren


##PubStat:
nrow(tbl)
tbl$C_PUB_STAT_PUB_STATUS[is.na(tbl$C_PUB_STAT_PUB_STATUS)] <- 0
sum(tbl$C_PUB_STAT_PUB_STATUS == 0) ## 647 fehlende PubStat
tbl <- tbl[tbl$C_PUB_STAT_PUB_STATUS != 0,] # ungleich 0= (!=), wir nehmen alle 0 raus
# View(tbl[,c("SIC","C_PUB_STAT_PUB_STATUS")]) ##bestimmte spalten aungucken
nrow(tbl)

#Krankheiten
tbl <- tbl[tbl$C_DISEASE_TX_SD_ALLG == 0,] 
tbl <- tbl[tbl$C_DISEASE_TX_SD_HYPO == 0,]
tbl <- tbl[tbl$C_DISEASE_TX_SD_HYPER == 0,]
tbl <- tbl[tbl$C_DISEASE_TX_FRUEHGEB == 0,]
tbl <- tbl[tbl$C_DISEASE_TX_ENDOKR == 0,]
tbl <- tbl[tbl$C_DISEASE_TX_DM1 == 0,]
tbl <- tbl[tbl$C_DISEASE_TX_DM2 == 0,]
tbl <- tbl[tbl$C_DISEASE_TX_BLUT == 0,]
tbl <- tbl[tbl$C_DISEASE_TX_GERIN == 0,]
tbl <- tbl[tbl$C_DISEASE_TX_EPIKRAMPF == 0,]
nrow(tbl) 
#Medis
tbl <- tbl[tbl$CHILD_MED_H_GLUCO_CORT == 0,]
tbl <- tbl[tbl$CHILD_MED_H_INSULIN == 0,]
tbl <- tbl[tbl$CHILD_MED_H_METFORMIN == 0,]
tbl <- tbl[tbl$CHILD_MED_H_LTHYROX == 0,]
tbl <- tbl[tbl$CHILD_MED_H_STOFFWECHSEL == 0,]
tbl <- tbl[tbl$CHILD_MED_H_HORMONE == 0,]
tbl <- tbl[tbl$CHILD_MED_H_WACHSTUM == 0,]
tbl <- tbl[tbl$CHILD_MED_H_IMMUNSUPP== 0,]
tbl <- tbl[tbl$CHILD_MED_H_NEUROLEPTIKA == 0,]
tbl <- tbl[tbl$CHILD_MED_H_KONTRAZEPT == 0,]
tbl <- tbl[tbl$CHILD_MED_H_TESTO == 0,]

nrow(tbl) 

 

 sum(is.na(tbl$C_PUB_STAT_PH)) ##NAs zählen

sapply( tbl, function( col ) sum( is.na( col ) ) )  ##zeigt für alle Variablen die NAs an
sapply( tbl, function( col ) sum( !is.na( col ) ) )
sapply( tbl, table )
summary(tbl)

nrow(tbl)

##underweights raus: sind schon in "join.all.tables aussortiert worden!
# tbl<- tbl[tbl$WGHT_GRPS=="normalweight"|tbl$WGHT_GRPS=="overweight.and.obese",] 



##jetzt: Freitexte ausschließen: mit neuem Datensatz
##neuer Datensatz-tbl.gruppe2.codierteAks= nur hinzugekommene Daten. Freitexte durchgucken mit Aks-Spalte.Dann codierte Tbl mergen und Aks ausschließen.


##Freitexte ausschließen: in tbl.fuer.Aks.xlsx-->aks codieren-->mit tbl. wieder mergen

write.xlsx(x = tbl[, c("SIC", "SEX","SCI_GROUP", "AGE", "CHILD_MED_H_MED_NAME" ,"C_DISEASE_TX_FREITEXT_ANGABE")  ], file = "tbl.fuer.Aks.xlsx")

tbl.FT<- read_excel("tbl.fuer.Aks.codiert.xlsx")
tbl.FT<- tbl.FT[!is.na( tbl.FT$Aks) & tbl.FT$Aks==1, c( "SIC", "SCI_GROUP", "Aks" )] ##tbl.FT, in der Spalte Aks, alle Zeilen, die in der Spalte Aks kein NA und eine 1 haben., davon die spalten SCI group und SIC 
nrow(tbl.FT) ##23 Freitexte
nrow(tbl) ##2980 vor Ausschluss der FT
tbl <- merge(tbl, tbl.FT, by = c("SIC", "SCI_GROUP" ), all = T )

tbl<- tbl[is.na(tbl$Aks), ]
nrow(tbl)  ##2960 nach FT-Ausschluss

##3jährige und NAs raus
tbl<- tbl[!is.na(tbl$AGE) & (tbl$AGE>4), ]
nrow(tbl)

#outlier 1 menarchealter=16 raus
tbl<- tbl[is.na(tbl$C_PUB_STAT_MENARCHE_WANN)|(!is.na( tbl$C_PUB_STAT_MENARCHE_WANN ) & tbl$C_PUB_STAT_MENARCHE_WANN<16), ]
 ##--> 3j?hrige und outlier f?hre ich unter "ausgeschlossene Freitexte" f?r die Dokumentation

nrow(tbl)
save(tbl, file = "main.table.curated.20170727.Rd")

write.xlsx( x = tbl, file = "main.table.curated.20170727.xlsx" ) ##in Excel nur 2951...

names(tbl)



##LH/FSH:NAs raus
tbl<- tbl[!is.na(tbl$LH_S_NUM_VALUE)&!is.na(tbl$FSH_S_NUM_VALUE),]
nrow(tbl)

##Missing BMIs raus
tbl<-tbl[!is.na(tbl$C_ANTHRO_KH_BMI_ADJ),]

##Mehrfachbesuche ausschließen

#visits: zeigt Anzahl der Besuche
tbl.visits <-
    tbl %>%
    group_by(SIC, SEX) %>% ##sortieren nach sic und SEX
    summarise(VISITS = as.factor(n()))  ##jede gruppe kriegt einen eintrag: visits=n z?hlt gruppengr??e
             
#neue Tabelle: visits: zeigt besuche nach SEX
(visits <-
        table(tbl.visits$SEX, tbl.visits$VISITS))

##visits markieren: zeigt Reihenfolge der Besuchs: alte Tbl mit neuer spalte visit
tbl%<>%
    group_by(SIC, SEX)%>%
    mutate(visit=dense_rank(EDAT))

# tbl[, c("EDAT", "SIC", "visit")] ##anzeigen lassen

##nur Erstbesucher
nrow(tbl)  #2956
table(tbl$visit)
tbl<- tbl[!is.na(tbl$visit)& tbl$visit==1, ]
 sapply(tbl, function(col)sum(is.na(col))) ##von allen Spalten NAs zählen
 sapply(tbl, function(col)sum(!is.na(col))) 
 sapply(tbl[, c("SEX", "EDAT")], function(col)sum(!is.na(col))) ##Bsp: 2 Spalten zählen
 sapply(tbl[, c("SEX", "WGHT_GRPS")], table) ##Bsp: 2 Spalten zählen
 nrow(tbl)  #1467 Erstbesucher
 
 save(tbl, file = "main.table.curated.first.visit.20170727.Rd")
 
 write.xlsx( x = tbl, file = "main.table.curated.first.visit.20170727.xlsx" )
 

 
save(tbl, file = "main.table.curated.first.visit.complete.20170727.Rd")


write.xlsx( x = tbl, file = "main.table.curated.first.visit.complete.20170727.xlsx" )



 ##Bsp:outlier angucken
 # ggplot(tbl)+
 #     geom_point(aes(AGE, LH_S_NUM_VALUE))
 # 
  #jetzt den einen Wert angucken: 1. filtern
tbl[tbl$LH_S_NUM_VALUE>35, c("C_DISEASE_TX_FREITEXT_ANGABE", "SEX", "C_ANTHRO_KH_BMI_ORIG", "AGE","LH_S_COMMENT" )]

##theoretisch ausschließen
# tbl<- tbl[tbl$LH_S_NUM_VALUE<35,]



 