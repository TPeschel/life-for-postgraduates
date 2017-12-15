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
# setwd( "/Users/Hans/Documents/Schule und Uni/Uni/Doktorarbeit/DieArbeitII/LIfE/Eigen Erstelltes/")

setwd( "~/LIFE/life-for-postgraduates/HansSurup/sent/20171128/")

load( "GesamtJoin_BereinigtSIC1411.Rd" )

# names( wt )[ names( wt ) == "C_ANTHRO_KH_AGE" ]        <- "AGE"
# names( wt )[ names( wt ) == "C_ANTHRO_KH_EDAT" ]       <- "EDAT"
# names( wt )[ names( wt ) == "C_ANTHRO_KH_GRP" ]        <- "SCIGROUP"
# names( wt )[ names( wt ) == "TEILNEHMER_SIC" ]         <- "SIC"
# names( wt )[ names( wt ) == "TEILNEHMER_GESCHLECHT" ]  <- "SEX"
# names( wt )[ names( wt ) == "TEILNEHMER_GEB_JJJJMM" ]  <- "BIRTH"
# names( wt )[ names( wt ) == "TSH_S_NUM_VALUE" ]  <- "TSHSERUMWERT"
# names( wt )[ names( wt ) == "FT3_S_NUM_VALUE" ]  <- "FT3SERUMWERT"
# names( wt )[ names( wt ) == "FT4_S_NUM_VALUE" ]  <- "FT4SERUMWERT"
# names( wt )[ names( wt ) == "A_TGAK_S_NUM_VALUE" ]  <- "TGAK"
# names( wt )[ names( wt ) == "A_TRAK_S_NUM_VALUE" ]  <- "TRAK"
# names( wt )[ names( wt ) == "C_ANTHRO_KH_HEIGHT_ORIG" ]  <- "HEIGHT"
# names( wt )[ names( wt ) == "C_ANTHRO_KH_HEIGHT_ADJ" ]  <- "HEIGHT_ADJ"
# names( wt )[ names( wt ) == "C_ANTHRO_KH_WEIGHT_ORIG" ]  <- "WEIGHT"
# names( wt )[ names( wt ) == "C_ANTHRO_KH_WEIGHT_ADJ" ]  <- "WEIGHT_ADJ"
# names( wt )[ names( wt ) == "C_DISEASE_TX_FREITEXT_ANGABE" ]  <- "DISEASE_FREITEXT"

##
# geht auch mit nur einem Befehl
##

wt <-
    rename.columns( wt,

        c( "C_ANTHRO_KH_AGE",         "C_ANTHRO_KH_EDAT",        "C_ANTHRO_KH_GRP",        "TEILNEHMER_SIC",
            "TEILNEHMER_GESCHLECHT",  "TEILNEHMER_GEB_JJJJMM",   "TSH_S_NUM_VALUE",        "FT3_S_NUM_VALUE",
            "FT4_S_NUM_VALUE",        "A_TGAK_S_NUM_VALUE",      "A_TRAK_S_NUM_VALUE",     "C_ANTHRO_KH_HEIGHT_ORIG",
            "C_ANTHRO_KH_HEIGHT_ADJ", "C_ANTHRO_KH_WEIGHT_ORIG", "C_ANTHRO_KH_WEIGHT_ADJ", "C_DISEASE_TX_FREITEXT_ANGABE" ),

        c( "AGE",                     "EDAT",                    "SCIGROUP",               "SIC",
           "SEX.12",                  "BIRTH",                   "TSHSERUMWERT",           "FT3SERUMWERT",
           "FT4SERUMWERT",            "TGAK",                    "TRAK",                   "HEIGHT",
           "HEIGHT_ADJ",              "WEIGHT",                  "WEIGHT_ADJ",             "DISEASE_FREITEXT" ) )

##
# kurze Uebersicht ueber Missings und Availables
##
table.df( wt, F )

##
# Die SEX-Spalte mit male und female gibt es schon.
##

# Baue als erstes SEX-Spalte mit Woertern statt Zahlen // nicht mehr noetig

# wt$SEX <-
#     as.factor( c( "male", "female" )[ match( wt$SEX, c( 1, 2 ) ) ] )

# Nimm lieber diesen Befehl
# Macht das gleiche

# wt$SEX <-
#     factor( wt$SEX, levels = c( 1, 2 ), labels = c( "male", "female" ) )


# TSH_S_RAW_VALUE ist als Text abgespeichert, wegen Eintraegen wie "<5"

wt$TSH_S_RAW_VALUE.dbl <-
    as.numeric( wt$TSH_S_RAW_VALUE )

# deswegen werden hier NA's erzeugt

# es scheint aber nur einen solchen Eintrag zu geben
wt[ is.na( wt$TSH_S_RAW_VALUE.dbl ), c( "SIC", "SCIGROUP", "TSH_S_RAW_VALUE.dbl", "TSH_S_RAW_VALUE" ) ]

# setze diesen erstmal auf 0
wt$TSH_S_RAW_VALUE.dbl[ is.na( wt$TSH_S_RAW_VALUE.dbl ) ] <- 
    5

# ok, ermitteln wir die Quantilen
( quants <-
    quantile( wt$TSH_S_RAW_VALUE.dbl, c( .1, .9 ) ) )

# ersmal histogram
hist( wt$TSH_S_RAW_VALUE.dbl )
plot( wt$TSH_S_RAW_VALUE.dbl )
abline( quants[ 1 ], 0, col = "green" ) # quantilen einzeichnen
abline( quants[ 2 ], 0, col = "red" ) # quantilen einzeichnen
# Wie Du siehst ist die Verteilung nicht normal

# Sieht ehr logarithmisch aus
# Versuchen wir das
wt$TSH_S_RAW_VALUE.dbl.log <-
    log10( wt$TSH_S_RAW_VALUE.dbl )

# Quantilen neu berechnen
( quants <-
    quantile( wt$TSH_S_RAW_VALUE.dbl.log, c( .1, .9 ) ) )

hist( wt$TSH_S_RAW_VALUE.dbl.log )
plot( wt$TSH_S_RAW_VALUE.dbl.log )
abline( quants[ 1 ], 0, col = "green" )
abline( quants[ 2 ], 0, col = "red" )
# optisch schoen normal jetzt

# nun bauen wir 3 Gruppen fuer die Bereiche / Quantilen [0,10), [10,90], (90,100] 

wt$quant_group[ wt$TSH_S_RAW_VALUE.dbl.log < quants[ 1 ] ] <- 
    "LESS10"

wt$quant_group[ quants[ 1 ] <= wt$TSH_S_RAW_VALUE.dbl.log & wt$TSH_S_RAW_VALUE.dbl.log <= 90 ] <- 
    "BETWEEN10AND90"

wt$quant_group[ wt$TSH_S_RAW_VALUE.dbl.log > quants[ 2 ] ] <- 
    "OVER90"

my.histo.theme <-
  list(
    theme_bw(),
    scale_color_manual( "QUANTILE", values = c( "OVER90" = "red", "LESS10" = "green", "BETWEEN10AND90" = "blue" ) ),
    scale_fill_manual( "QUANTILE", values = c( "OVER90" = "red", "LESS10" = "green", "BETWEEN10AND90" = "blue" ) ) )

ggplot( 
    wt, 
    aes( 
        SEX, 
        TSH_S_RAW_VALUE.dbl.log ) ) + 
    geom_jitter( aes( SEX, TSH_S_RAW_VALUE.dbl.log, col = quant_group ), size = 1.5, alpha = .25 ) +
    geom_boxplot( outlier.alpha = 0, alpha = .5 ) + 
    my.histo.theme
    
ggplot( 
    wt, 
    aes( 
        SEX, 
        TSH_S_RAW_VALUE.dbl.log ) ) + 
    geom_jitter( aes( SEX, TSH_S_RAW_VALUE.dbl.log, col = quant_group ), size = 1.5, width = .2, alpha = .1 ) +
    geom_boxplot( outlier.alpha = 0, alpha = .5 ) + 
    my.histo.theme
    
ggplot( 
    wt, 
    aes( 
        SEX, 
        TSH_S_RAW_VALUE.dbl.log ) ) + 
    geom_point( aes( SEX, TSH_S_RAW_VALUE.dbl.log, col = quant_group ), size = 1.5 ) +
    geom_boxplot( outlier.alpha = 0, alpha = .5 ) + 
    my.histo.theme
    
ggplot( 
    wt, 
    aes( 
        SEX, 
        TSH_S_RAW_VALUE.dbl.log ) ) + 
    geom_point( aes( SEX, TSH_S_RAW_VALUE.dbl.log, col = quant_group ), size = 1.5 ) +
    geom_boxplot( outlier.shape = 1, alpha = .5 ) + 
    my.histo.theme
    

