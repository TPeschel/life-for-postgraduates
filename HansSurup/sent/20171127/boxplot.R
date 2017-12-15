# Boxploterstellung
rm( list = ls( ) ) 


# Installiere gegebenenfalls und lade noetige Pakete

hlpr4life::load.pkgs(
  c( 
    "hlpr4life",
    "ggplot2",
    "readxl",
    "openxlsx",
    "xlsx",
    "dplyr" ) )


# Pfadanpassung
setwd("/Users/Hans/Documents/Schule und Uni/Uni/Doktorarbeit/DieArbeitII/LIfE/Eigen Erstelltes/")

load("GesamtJoin_BereinigtSIC1411.Rd")

my.histo.theme <-
  list(
    theme_bw(),
    scale_color_manual( "SEX", values = c( "red", "blue" ) ),
    scale_fill_manual( "SEX", values = c( "red", "blue" ) ),
    facet_grid( .  ~  SEX ))

names( wt )[ names( wt ) == "C_ANTHRO_KH_AGE" ]        <- "AGE"
names( wt )[ names( wt ) == "C_ANTHRO_KH_EDAT" ]       <- "EDAT"
names( wt )[ names( wt ) == "C_ANTHRO_KH_GRP" ]        <- "SCIGROUP"
names( wt )[ names( wt ) == "TEILNEHMER_SIC" ]         <- "SIC"
names( wt )[ names( wt ) == "TEILNEHMER_GESCHLECHT" ]  <- "SEX"
names( wt )[ names( wt ) == "TEILNEHMER_GEB_JJJJMM" ]  <- "BIRTH"
names( wt )[ names( wt ) == "TSH_S_NUM_VALUE" ]  <- "TSHSERUMWERT"
names( wt )[ names( wt ) == "FT3_S_NUM_VALUE" ]  <- "FT3SERUMWERT"
names( wt )[ names( wt ) == "FT4_S_NUM_VALUE" ]  <- "FT4SERUMWERT"
names( wt )[ names( wt ) == "A_TGAK_S_NUM_VALUE" ]  <- "TGAK"
names( wt )[ names( wt ) == "A_TRAK_S_NUM_VALUE" ]  <- "TRAK"
names( wt )[ names( wt ) == "C_ANTHRO_KH_HEIGHT_ORIG" ]  <- "HEIGHT"
names( wt )[ names( wt ) == "C_ANTHRO_KH_HEIGHT_ADJ" ]  <- "HEIGHT_ADJ"
names( wt )[ names( wt ) == "C_ANTHRO_KH_WEIGHT_ORIG" ]  <- "WEIGHT"
names( wt )[ names( wt ) == "C_ANTHRO_KH_WEIGHT_ADJ" ]  <- "WEIGHT_ADJ"
names( wt )[ names( wt ) == "C_DISEASE_TX_FREITEXT_ANGABE" ]  <- "DISEASE_FREITEXT"


# Baue als erstes SEX-Spalte mit Woertern statt Zahlen

wt$SEX <-
  as.factor( c( "male", "female" )[ match( wt$SEX, c( 1, 2 ) ) ] )
