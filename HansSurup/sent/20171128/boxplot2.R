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


wt$TSH_S_RAW_VALUE.dbl <-
    as.numeric( wt$TSH_S_RAW_VALUE )

# es scheint aber nur einen solchen Eintrag zu geben
wt[ is.na( wt$TSH_S_RAW_VALUE.dbl ), c( "SIC", "SCIGROUP", "TSH_S_RAW_VALUE.dbl", "TSH_S_RAW_VALUE" ) ]

# setze diesen erstmal auf 5
wt$TSH_S_RAW_VALUE.dbl[ is.na( wt$TSH_S_RAW_VALUE.dbl ) ] <- 
    5

wt$TSH_S_RAW_VALUE.dbl.log <-
    log10( wt$TSH_S_RAW_VALUE.dbl )

##############################################################################################
# Quantilen neu berechnen
(
    quants <-
        wt %>% 
            group_by( SEX ) %>%
            summarise( 
                Q000 = min( TSH_S_RAW_VALUE.dbl.log ),
                Q010 = quantile( TSH_S_RAW_VALUE.dbl.log, .025 ),
                Q090 = quantile( TSH_S_RAW_VALUE.dbl.log, .975 ),
                Q100 = max( TSH_S_RAW_VALUE.dbl.log, 1. ) ) )

# nun bauen wir 6 Gruppen fuer die Bereiche / Quantilen [0,10), [10,90], (90,100] / Mann Frau
wt$QG <-
    NA


wt$QG[ wt$SEX == "female" ] <-
    paste0( 
        "F",
        cut(
            x = wt$TSH_S_RAW_VALUE.dbl.log[ wt$SEX == "female" ],
            breaks = quants[ quants$SEX == "female", -1 ],
            labels = c( "0_10", "10_90", "90_100" ),
            include.lowest = T ) )

wt$QG[ wt$SEX == "male" ] <-
    paste0( 
        "M",
        cut(
            x = wt$TSH_S_RAW_VALUE.dbl.log[ wt$SEX == "male" ],
            breaks = quants[ quants$SEX == "male", -1 ],
            labels = c( "0_10", "10_90", "90_100" ),
            include.lowest = T ) )

table( wt$QG )

my.histo.theme <-
  list(
    theme_bw(),
    scale_color_manual( "QG", values = c( "F0_10" = "red4", "F10_90" = "green4", "F90_100" = "blue4", "M0_10" = "red", "M10_90" = "green", "M90_100" = "blue" ) ) )

ggsubplot( 
    ggplot( 
        wt, 
        aes( 
            SEX, 
            TSH_S_RAW_VALUE.dbl.log ) ) + 
        geom_jitter( aes( SEX, TSH_S_RAW_VALUE.dbl.log, col = QG ), size = 1.5, alpha = .25 ) +
        geom_boxplot( outlier.alpha = 0, alpha = .5 ) + 
        my.histo.theme,
        
    ggplot( 
        wt, 
        aes( 
            SEX, 
            TSH_S_RAW_VALUE.dbl.log ) ) + 
        geom_jitter( aes( SEX, TSH_S_RAW_VALUE.dbl.log, col = QG ), size = 1.5, width = .2, alpha = .1 ) +
        geom_boxplot( outlier.alpha = 0, alpha = .5 ) + 
        my.histo.theme,
        
    ggplot( 
        wt, 
        aes( 
            SEX, 
            TSH_S_RAW_VALUE.dbl.log ) ) + 
        geom_point( aes( SEX, TSH_S_RAW_VALUE.dbl.log, col = QG ), size = 1.5 ) +
        geom_boxplot( outlier.alpha = 0, alpha = .5 ) + 
        my.histo.theme,
        
    ggplot( 
        wt, 
        aes( 
            SEX, 
            TSH_S_RAW_VALUE.dbl.log ) ) + 
        geom_point( aes( SEX, TSH_S_RAW_VALUE.dbl.log, col = QG ), size = 1.5 ) +
        geom_boxplot( outlier.shape = 1, alpha = .5 ) + 
        my.histo.theme,
    cols = 2 )

ggplot( 
    wt, 
    aes( 
        SEX, 
        TSH_S_RAW_VALUE.dbl.log ) ) + 
    geom_jitter( aes( SEX, TSH_S_RAW_VALUE.dbl.log, col = QG ), size = 1.5, alpha = .25 ) +
    geom_boxplot( outlier.alpha = 0, alpha = .5 ) + 
    my.histo.theme
    
ggplot( 
    wt, 
    aes( 
        SEX, 
        TSH_S_RAW_VALUE.dbl.log ) ) + 
    geom_jitter( aes( SEX, TSH_S_RAW_VALUE.dbl.log, col = QG ), size = 1.5, width = .2, alpha = .1 ) +
    geom_boxplot( outlier.alpha = 0, alpha = .5 ) + 
    my.histo.theme
    
ggplot( 
    wt, 
    aes( 
        SEX, 
        TSH_S_RAW_VALUE.dbl.log ) ) + 
    geom_point( aes( SEX, TSH_S_RAW_VALUE.dbl.log, col = QG ), size = 1.5 ) +
    geom_boxplot( outlier.alpha = 0, alpha = .5 ) + 
    my.histo.theme
    
ggplot( 
    wt, 
    aes( 
        SEX, 
        TSH_S_RAW_VALUE.dbl.log ) ) + 
    geom_point( aes( SEX, TSH_S_RAW_VALUE.dbl.log, col = QG ), size = 1.5 ) +
    geom_boxplot( outlier.shape = 1, alpha = .5 ) + 
    my.histo.theme

# Hat sich kaum gelohnt
# Jungs und Maedels unterscheiden sich kaum
quants
