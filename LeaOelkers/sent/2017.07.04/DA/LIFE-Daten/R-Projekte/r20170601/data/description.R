
##
# loesche speicher
##
rm( list = ls( ) )

##
# setze Pfad zu aktuellem R-Arbeitsverzeichnis
setwd( "c:/Users/Lea/Desktop/DA/LIFE-Daten/data/" )

##
# lade Grafik Paket
##
library( ggplot2 )

##
# lade dplyr Paket
##
library( dplyr )

##
# lade Paket openxlsx zum Speichern von Tabellen im Excel xlsx-Format
##
library( openxlsx )

##
# bevorzugtes Schema fuer Grafiken
##
my.theme <-
    theme_bw( )

##
# lade Gesamtjoin: statt in data sind die Daten in anderem Pfad (..)
##
load( "../R-Projekte/r20170601/data/tabelleGesamtJoinAlleSpalten.Rd" )

# save( meine.tabelle.male, meine.Tabelle.female, "meineTabelle.Rd")##Bsp. um Tabelle abzuspeichern und dann wieder zu laden, auch für mehrere Tabellen möglich
# load( "meineTabelle.Rd")
##
# Anzahl aller Besuche
##
nrow( gesamt.Join.alle.spalten )
##Tablle Gesamtjoin umbenennen: ##gespeichert, zum öffnen : load("Tabellendame.Rd")

##load("../R-Projekte/r20170601/data/tabelleGesamtJoinAlleSpalten.Rd")
##
# Anzahl der Probanden
##
length( unique( gesamt.Join.alle.spalten$SIC ) )

##
# erzeuge kleinere Tabelle, die nur Zeilen mit gueltigen FSH-Werten enthaelt
## nur fsh: müssen flag haben,  flag muss 1 sein, num wert muss da sein
tbl.weniger.spalten1 <-
    gesamt.Join.alle.spalten[ !is.na( gesamt.Join.alle.spalten$FSH_S_VALUE_FLAG ) & ( gesamt.Join.alle.spalten$FSH_S_VALUE_FLAG == 1 ) & !is.na( gesamt.Join.alle.spalten$FSH_S_NUM_VALUE ), ]
table(tbl.weniger.spalten1$FSH_S_VALUE_FLAG)
table(tbl.weniger.spalten1$FSH_S_UNIT) 

##
# wirft alle unnoetigen Spalten raus
# bzw. behalte nur interessante Spalten
##
tbl.weniger.spalten1 <- tbl.weniger.spalten1[ , c( "SIC", "SCIGROUP", "EDAT", "SEX", "AGE", "C_ANTHRO_KH_HEIGHT_ORIG", "C_ANTHRO_KH_WEIGHT_ORIG" , "C_ANTHRO_KH_BMI_ORIG" , "C_ANTHRO_KH_BMI_ADJ" ,                     
                  "C_PUB_STAT_MENARCHE",         "C_PUB_STAT_PUB_STATUS",      "C_PUB_STAT_MENARCHE_WANN",   
                         "C_DISEASE_TX_FREITEXT_ANGABE",
                        "C_DISEASE_TX_SD_ALLG",       "C_DISEASE_TX_SD_HYPER",       "C_DISEASE_TX_SD_HYPO",      
                "C_DISEASE_TX_FRUEHGEB",       "C_DISEASE_TX_ENDOKR",        
              "C_DISEASE_TX_DM1",           "C_DISEASE_TX_DM2",             "C_DISEASE_TX_NGSCREEN",       
                "C_DISEASE_TX_ADIP",            "C_DISEASE_TX_BLUT",            "C_DISEASE_TX_GERIN" ,         
                "C_DISEASE_TX_PULMO" ,           "C_DISEASE_TX_ASTHMA",          "C_DISEASE_TX_GIT",           
                    "C_DISEASE_TX_AUGE",            "C_DISEASE_TX_HNO",             "C_DISEASE_TX_MOE",            
                      "C_DISEASE_TX_INFEKT",          "C_DISEASE_TX_HWI_PN",          "C_DISEASE_TX_NEPHRO",         
                "C_DISEASE_TX_NIERENFEHL",      "C_DISEASE_TX_KARDIO",          "C_DISEASE_TX_BD_HOCH",        
                "C_DISEASE_TX_BD_TIEF",         "C_DISEASE_TX_KARDIO_RHYTH",    "C_DISEASE_TX_ANGIO",          
                  "C_DISEASE_TX_ALLERG",          "C_DISEASE_TX_NEURODER",        "C_DISEASE_TX_SOOR",           
                "C_DISEASE_TX_DERMA",           "C_DISEASE_TX_NEUROL",          "C_DISEASE_TX_KOPFSCH",       
              "C_DISEASE_TX_EPIKRAMPF",       "C_DISEASE_TX_PSY",            "C_DISEASE_TX_ADHS",           
                  "C_DISEASE_TX_DEPRES",          "C_DISEASE_TX_SUCHT",           "C_DISEASE_TX_SPRACH",         
                    "C_DISEASE_TX_MUSKEL",                      "CHILD_MED_H_MED_NAME",                 
                  "CHILD_MED_H_ATC_NAME",                           "CHILD_MED_H_JOD",             
                  "CHILD_MED_H_KALIUM",          "CHILD_MED_H_HOMOE",           
                         
              "CHILD_MED_H_STOFFWECHSEL",     "CHILD_MED_H_METFORMIN",        "CHILD_MED_H_INSULIN",         
              "CHILD_MED_H_HORMONE",          "CHILD_MED_H_LTHYROX",          "CHILD_MED_H_KONTRAZEPT",      
                    "CHILD_MED_H_SEX_STEROIDE",     "CHILD_MED_H_WACHSTUM",         "CHILD_MED_H_DESMOPRESS",      
              "CHILD_MED_H_MINERALOCORT",     "CHILD_MED_H_GLUCO_CORT",       "CHILD_MED_H_TESTO",           
              "CHILD_MED_H_ASTHMA",           "CHILD_MED_H_ALLERGIE",         "CHILD_MED_H_ANTIHIST",        
                  "CHILD_MED_H_HYPOSENS",         "CHILD_MED_H_ATEM_STIM",        "CHILD_MED_H_ANALGETIKA",      
              "CHILD_MED_H_ERKAELT",          "CHILD_MED_H_ANTIBIO",          "CHILD_MED_H_MAGDRAM",         
                         "CHILD_MED_H_HERZ_BLUTDRUCK",   "CHILD_MED_H_UROLOGIE",        
            "CHILD_MED_H_IMMUNSUPP",        "CHILD_MED_H_NEUROLEPTIKA",     "CHILD_MED_H_ANTIPSYCH",
        
              "CHILD_MED_H_ADHS",                       "FAM_PSEUDO",                  
                    "N_IN_FAM",                     "SGROUP",                       "D00177_JAHR",                 
                         "D00177_HHAEQ",                        
                       "D00177_SCORE_FAM",            "FSH_S_NUM_VALUE",             
                         "FSH_S_RAW_REF_RAN",            "FSH_S_REF_RANGE_L" ,          
                "FSH_S_REF_RANGE_H",            "LH_S_NUM_VALUE",               "LH_S_VALUE_FLAG",            
                  "LH_S_RAW_REF_RAN" ,            "LH_S_REF_RANGE_L",             "LH_S_REF_RANGE_H")]           
                         
##Tabelle im richtigen pfad speichern
setwd("c:/Users/Lea/Desktop/DA/LIFE-Daten/R-Projekte/r20170601/data/")

##
# speichere eingedampfte Tabelle als Excel-File ab


write.xlsx( x = tbl.weniger.spalten1, file = "tbl.weniger.spalten1.xlsx")
            
##
# speichere eingedampfte Tabelle als RData-File ab
##



##


##
# Altersbereich
##
summary(tbl.weniger.spalten1$AGE)

##
# maennliche und weiliche Besuche mit FSH
##
table(tbl.weniger.spalten1$SEX)

##
# Pseudoaggregierungsfunktion fuer die Variable SEX
##
fun <-
function(v)
v[1]

##
# ermittle fuer jede SIC einmal das Geschlecht
##
tbl.sex <-
tbl.weniger.spalten1 %>%
group_by(SIC) %>%
summarise(sex = fun(SEX))

##
# maennliche und weiliche Besucher mit FSH
##
table(tbl.sex$sex)

##
# loesche temporaere Tabelle tbl.sex
##
rm("tbl.sex")

##
# Besuche
##
length(tbl.weniger.spalten1$SIC)

##
# Kinder
##
length(unique(tbl.weniger.spalten1$SIC))

##
# erstelle Altersgruppen
##
tbl.weniger.spalten1$AGE.CATEGORIE <-
cut(x = tbl.weniger.spalten1$AGE,
c(0:20))

##
# plotte Anzahlen der Besuche nach Geschlecht
##
ggplot(tbl.weniger.spalten1) +
my.theme +
scale_fill_manual(name = "Geschlecht", values = c("red", "blue")) +
xlab("Altersklassen") +
ylab("Anzahl der Besuche") +
geom_histogram(aes(AGE.CATEGORIE, fill = SEX), stat = "count") +
facet_grid(. ~ SEX)

##
# ermittle fuer jede SIC Nummer der Besuche
##
tbl.visits <-
tbl.weniger.spalten1 %>%
group_by(SIC) %>%
mutate(VISIT = as.factor(dense_rank(EDAT)))

##
# plotte Anzahlen der Kinder nach Geschlecht
##
ggplot(tbl.visits) +
my.theme +
scale_fill_manual(name = "Besuche",
values = c("#ffe0e0", "#ffb0b0", "#ff8080", "#ff5050")) +
xlab("Altersklassen") +
  ylab("Anzahl der Besuche") +
  geom_histogram(aes(AGE.CATEGORIE, fill = VISIT),
                 stat = "count",
                 position = "stack") +
  facet_grid(. ~ SEX)

##
# ermittle fuer jede SIC Anzahl der Besuche
##
tbl.visits <-
  tbl.weniger.spalten1 %>%
  group_by(SIC) %>%
  summarise(VISITS = n(),
            SEX = fun(SEX))

(visits <-
    table(tbl.visits$SEX, tbl.visits$VISITS))

##
# plotte Anzahlen der Kinder nach Geschlecht
##
ggplot() +
  my.theme +
  scale_fill_manual(name = "Geschlecht", values = c("red", "blue", "black")) +
  xlab("Anzahl der Besuche") +
  ylab("Anzahl Kinder mit n Besuchen") +
  geom_histogram(data = tbl.visits, aes(VISITS, fill = SEX), stat = "count") +
  facet_grid(. ~ tbl.visits$SEX)

ggsave(filename = "C:/Users/Lea/Desktop/DA/LIFE-Daten/R-Projekte/r20170601/plots/anzahlBesuche.png", width = 9, height = 5 )
##oder setwd(Pfad)
tbl.weniger.spalten1.curation <-
  tbl.weniger.spalten1[!is.na(tbl.weniger.spalten1$SIC) &
            !is.na(tbl.weniger.spalten1$SCIGROUP), c("SIC", "SCIGROUP", "FSH_S_NUM_VALUE")]

