##
# loesche speicher
##
rm( list = ls( ) )

##
# installiere devtools, falls noch nicht passiert
##
if( !"devtools" %in% installed.packages( ) ) {
    install.packages( "devtools" ) }

##
# installiere Paket HelperForLife
##
devtools::install_github( "TPeschel/hlpr4life" )

##
# lader hlpr4life
##
library( "hlpr4life" )

##
# lade paket readxl
##
# load.pkgs installiert Pakete, die noch nicht installiert sind, bevor versucht wird, diese zu laden
load.pkgs( c( "readxl", "ggplot2", "lubridate" ) )

setwd( "C:/Users/Anwender/Documents/Dissertation/PV208/data/data_neu_20160929/R" )

##
# lade t6
##
load( "T6.Rd" )

##
# lade Winkler
##
d177 <-
    read_excel( "PV0208_D00177.xlsx" )

##
# lade medikamente
##
d129 <-
  read_excel( "PV0208_D00129_Medikamente.xlsx" )
##
# verbinde ueber sic und edat
# hier mit einer toleranz von einem monat (31 Tage)
##
# t6. <-
#   merge.likely( 
#     d1 = t6, 
#     d2 = d129, 
#     by.x = c( "TEILNEHMER_SIC" ), 
#     by.y = c( "CHILD_MED_H_SIC" ),
#     by.lk.x = c( "Entnahmedatum.x" ),
#     by.lk.y = c( "CHILD_MED_H_EDAT" ),
#     min = c( -32 ),
#     max = c( +31 ) )

t6. <-
  merge( 
    x = t6, 
    y = d129, 
    by.x = c( "TEILNEHMER_SIC", "C_AUFKL_SCI_GROUP" ), 
    by.y = c( "CHILD_MED_H_SIC", "CHILD_MED_H_SCI_GROUP" ),
    all.x = T )


##
# verbinde winkler mit t6 uber sic und edat
# da edat beim winkler nur halbjaehrig angegeben ist
# verbinde uber datumsdifferenz von maximal einem halben jahr (182 Tage)
## 
t6.. <-
    merge.likely(
        t6.,
        d177,
        by.x = "TEILNEHMER_SIC",
        by.y = "PSEUDONYM",
        by.lk.x = "Entnahmedatum.x",
        by.lk.y = "EDAT",
        min = c( -183 ),
        max = c( +182 ) )

t6... <-
    t6..[ , c(
        "Materialnummer",
        "TEILNEHMER_SIC",
        "C_AUFKL_SCI_GROUP",
        "Entnahmedatum.x",
        "lfd. Nr.x",
        "Cortisol",
        "C_BP_SDS_BP_DIA_3",
        "C_BP_SDS_BP_SYS_3",
        "C_PUB_STAT_PUB_STATUS",
        "C_ANTHRO_AGE",
        "C_AUFKL_GENDER",
        "C_ANTHRO_KH_BMI_ORIG",
        "C_ANTHRO_KH_BMI_ADJ",
        "C_ANTHRO_KH_HEIGHT_ADJ",
#        "Haarwaschfrequenz",
        "HaarwaschfrequenzGruppen",
        "CHILD_MED_H_GLUCO_CORT",
        "CHILD_MED_H_MINERALOCORT",
        "CHILD_MED_H_SEX_STEROIDE",
        "D00177_SCORE_FAM" ) ]

t6 <-
    t6...

save( "t6", file = "AnthroWinklerMedik.Rd" )

ggplot( t6 ) + 
  theme_classic( ) +
  geom_histogram( aes( as.factor( round( C_ANTHRO_AGE ) ) ), stat = "count" ) +
  facet_grid( . ~ C_AUFKL_GENDER )

