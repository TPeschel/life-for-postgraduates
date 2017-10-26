rm( list = ls( ) )

# installiere devtools, falls noch nicht geschehen
if( !"devtools" %in% rownames( installed.packages( ) ) ) install.packages( "devtools" )

# installiere neueste Version von helperForLife, falls noch nicht geschehen
# devtools::install_github( "TPeschel/hlpr4life" )

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

options( max.print = 100000 )


##
# setze Pfad zu aktuellem R-Arbeitsverzeichnis, Pfad zum Laden
# setwd( "~/LIFE/life-for-postgraduates/LeaOelkers/AllesNeu20170725/data/original/" )
# setwd( "c:/Users/Lea/Desktop/AllesNeu20170725/data/generated/" )  ##Pfad in dem die original Excel tabl stehen
setwd( "~/LIFE/life-for-postgraduates/LeaOelkers/2017.09.27/data/generated/" )

load( "main.table.Rd" )

nrow( tbl )
# unique(tbl$SCI_GROUP) ##alle unterschiedlichen SCI groups anzeigen

# Ein umfangreicheres Protokoll kuerzer geschrieben
# hole alle Krankheitsspalten
( dis <-
        get.columns( tbl, "C_DISEASE_TX_" ) )

# schaue mal die Werte der Spalten an
table.df( tbl[, dis ], T, T )

# schreibe Nullen fuer jedes NA in diese Spalten
tbl[ , dis ][ is.na( tbl[, dis ] ) ] <- 0

# erzeuge Protokoll
data.frame( 
    NUMBER = sapply( 
        tbl[ , get.columns( tbl, "DIS" ) ],
        function( s ) { sum( s == 1, na.rm = T ) } ) )

( med <-
        get.columns( tbl, "CHILD_MED_H_(?!.*NAME)(?!.*CODE)" ) )

# schaue mal die Werte der Spalten an
table.df( tbl[, med ], T, T )

# schreibe Nullen fuer jedes NA in diese Spalten
tbl[ , med ][ is.na( tbl[, med ] ) ] <- 0

data.frame(
    NUMBER = 
        sapply( 
            tbl[ , med ], 
            function( s ) { sum( s == 1, na.rm = T ) } ) )


##PubStat:
nrow( tbl )

tbl$C_PUB_STAT_PUB_STATUS[ is.na( tbl$C_PUB_STAT_PUB_STATUS ) ] <- 0

#1043
sum( tbl$C_PUB_STAT_PUB_STATUS == 0 ) ## 647 fehlende PubStat

tbl <-
    tbl[ tbl$C_PUB_STAT_PUB_STATUS != 0, ] # ungleich 0= (!=), wir nehmen alle 0 raus

# View(tbl[,c("SIC","C_PUB_STAT_PUB_STATUS")]) ##bestimmte spalten aungucken

#3581
nrow( tbl )

#Krankheiten
tbl <- 
    tbl[ 
        tbl$C_DISEASE_TX_SD_ALLG == 0 &
            tbl$C_DISEASE_TX_SD_HYPO == 0 &
            tbl$C_DISEASE_TX_SD_HYPER == 0 &
            tbl$C_DISEASE_TX_FRUEHGEB == 0 &
            tbl$C_DISEASE_TX_ENDOKR == 0 &
            tbl$C_DISEASE_TX_DM1 == 0 &
            tbl$C_DISEASE_TX_DM2 == 0 &
            tbl$C_DISEASE_TX_BLUT == 0 &
            tbl$C_DISEASE_TX_GERIN == 0 &
            tbl$C_DISEASE_TX_EPIKRAMPF == 0, ]

nrow( tbl ) 
#3507
#Medis
tbl <-
    tbl[ 
        tbl$CHILD_MED_H_GLUCO_CORT == 0 &
            tbl$CHILD_MED_H_INSULIN == 0 &
            tbl$CHILD_MED_H_METFORMIN == 0 &
            tbl$CHILD_MED_H_LTHYROX == 0 &
            tbl$CHILD_MED_H_STOFFWECHSEL == 0 &
            tbl$CHILD_MED_H_HORMONE == 0 &
            tbl$CHILD_MED_H_WACHSTUM == 0 &
            tbl$CHILD_MED_H_IMMUNSUPP== 0 &
            tbl$CHILD_MED_H_NEUROLEPTIKA == 0 &
            tbl$CHILD_MED_H_KONTRAZEPT == 0 &
            tbl$CHILD_MED_H_TESTO == 0, ]
#3155
nrow( tbl ) 

sum( is.na( tbl$C_PUB_STAT_PH ) ) ##NAs zählen

table.df( tbl, T,T )
sapply( tbl, table )

summary( tbl )

##underweights raus: sind schon in "join.all.tables aussortiert worden!
# tbl<- tbl[tbl$WGHT_GRPS=="normalweight"|tbl$WGHT_GRPS=="overweight.and.obese",] 

##jetzt: Freitexte ausschließen: mit neuem Datensatz
##neuer Datensatz-tbl.gruppe2.codierteAks= nur hinzugekommene Daten. Freitexte durchgucken mit Aks-Spalte.Dann codierte Tbl mergen und Aks ausschließen.


##Freitexte ausschließen: in tbl.fuer.Aks.xlsx-->aks codieren-->mit tbl. wieder mergen

write.xlsx( 
    x = tbl[ ,
             c(
                 "SIC", "SEX","SCI_GROUP", "AGE", "CHILD_MED_H_MED_NAME", "C_DISEASE_TX_FREITEXT_ANGABE" ) ],
    file = "tbl.fuer.Aks.xlsx" )

tbl.FT <-
    read_excel( "tbl.fuer.Aks.codiert.xlsx" )

tbl.FT <-
    tbl.FT[ !is.na( tbl.FT$Aks ) & tbl.FT$Aks == 1,
            c( "SIC", "SCI_GROUP", "Aks" ) ] ##tbl.FT, in der Spalte Aks, alle Zeilen, die in der Spalte Aks kein NA und eine 1 haben., davon die spalten SCI group und SIC 

nrow( tbl.FT ) ##23 Freitexte
#3155
nrow( tbl ) ##2980 vor Ausschluss der FT

tbl <-
    merge( tbl, tbl.FT, by = c("SIC", "SCI_GROUP" ), all = T )

tbl<-
    tbl[ is.na( tbl$Aks ), ]

#3132
nrow( tbl )  ##2960 nach FT-Ausschluss

sum( is.na( tbl$AGE ) )

##3jährige und NAs raus
tbl <-
    tbl[ !is.na( tbl$AGE ) & ( tbl$AGE > 4 ), ]

# nur ein 3jaehriger und keine NAs
# 3131
nrow( tbl )

#outlier 1 menarchealter=16 raus
tbl <- 
    tbl[ is.na( tbl$C_PUB_STAT_MENARCHE_WANN ) | ( !is.na( tbl$C_PUB_STAT_MENARCHE_WANN ) & tbl$C_PUB_STAT_MENARCHE_WANN < 16 ), ]
#3130
##--> 3j?hrige und outlier f?hre ich unter "ausgeschlossene Freitexte" f?r die Dokumentation

nrow( tbl )

fname <-
    paste0( "main.table.curated.", today( ) )
    
save( tbl, file = paste0( fname, ".Rd" ) )

write.xlsx( x = tbl, file = paste0( fname, ".xlsx" ) ) ##in Excel nur 2951...

names( tbl )

##LH/FSH:NAs raus
tbl <- 
    tbl[ !is.na( tbl$LH_S_NUM_VALUE ) & !is.na( tbl$FSH_S_NUM_VALUE ), ]

# 3064
nrow(tbl)

##Missing BMIs raus
tbl <-
    tbl[ !is.na( tbl$BMI.ADJ ), ]

##Mehrfachbesuche ausschließen

#visits: zeigt Anzahl der Besuche
tbl.visits <-
    tbl %>%
    group_by( SIC, SEX ) %>% ##sortieren nach sic und SEX
    summarise( VISITS = as.factor( n( ) ) )  ##jede gruppe kriegt einen eintrag: visits=n z?hlt gruppengr??e

#neue Tabelle: visits: zeigt besuche nach SEX
( visits <-
        table( tbl.visits[ , c( "SEX", "VISITS" ) ] ) )

##visits markieren: zeigt Reihenfolge der Besuchs: alte Tbl mit neuer spalte visit
tbl%<>%
    group_by( SIC, SEX ) %>%
    mutate( visit = dense_rank( EDAT ) )

# tbl[, c("EDAT", "SIC", "visit")] ##anzeigen lassen

##nur Erstbesucher
nrow( tbl )  #2956
# 3064

table( tbl$visit )

tbl<-
    tbl[ !is.na( tbl$visit ) & tbl$visit == 1, ]
#1536

table.df( tbl[ , c("SEX", "EDAT", "WGHT_GRPS" ) ] ) ##Bsp: 3 Spalten zählen

nrow( tbl )  #1467 Erstbesucher
#1536

fname <-
    paste0( "main.table.curated.first.visit.", today( ) )

save( tbl, file = paste0( fname, ".Rd" ) )

write.xlsx( x = tbl, file = paste0( fname, ".xlsx" ) )

#save( tbl, file = "main.table.curated.first.visit.complete.20170727.Rd")


#write.xlsx( x = tbl, file = "main.table.curated.first.visit.complete.20170727.xlsx" )



##Bsp:outlier angucken
# ggplot(tbl)+
#     geom_point(aes(AGE, LH_S_NUM_VALUE))
# 
#jetzt den einen Wert angucken: 1. filtern
tbl[ tbl$LH_S_NUM_VALUE > 35, c( "C_DISEASE_TX_FREITEXT_ANGABE", "SEX", "BMI", "AGE", "LH_S_COMMENT", "LH_S_NUM_VALUE" ) ]

##theoretisch ausschließen
# tbl<- tbl[tbl$LH_S_NUM_VALUE<35,]

