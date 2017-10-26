# loesche speicher
###############################################

rm( list = ls( ) )

# installiere devtools, 
# falls noch nicht geschehen
###############################################

if( !"devtools" %in% rownames( installed.packages( ) ) ) install.packages( "devtools" )

# installiere helper for life paket von github,
# falls noch nicht geschehen
###############################################
devtools::install_github( "TPeschel/hlpr4life" )

# installiere - wenn noetig -
# und lade Pakete
###############################################
hlpr4life::load.pkgs( 
    c(
        "hlpr4life",
        "readxl",
        "openxlsx",
        "dplyr" ) )

comb.cols <-
    function( data, cols, sep = " ~ " ) {
        s <- 
            data[[ cols[ 1 ] ]]
        
        for( i in 2 : length( cols ) ) {
            
            s <-
                paste0( s, sep, data[[ cols[ i ] ]] ) }
        s }


# musste an Deinen Pfad anpassen
###############################################
setwd( "~/LIFE/life-for-postgraduates/TobiasDienerowitz/2017.09.25/" )

options( max.print = 1000000 )

# bei mir liegen die Tabellen in einem
# Unterordner data/
# Unterordner data/original/
# Unterordner data/generated/
###############################################
main.1205 <-
    read_excel( "data/original/PV0278_datajoin20161205.xlsx" )

names( main.1205 )

get.columns( main.1205, "T00865." )

# korriegiere fehlerhafte spaltennamen
main.1205 <-
    rename.columns( 
        main.1205,
        c( "C_PUB_STAT_MET00865RCHE", "C_PUB_STAT_MET00865RCHE_WANN", "T00865.VRP_FILET00865ME" ),
        c( "C_PUB_STAT_MENARCHE", "C_PUB_STAT_MENARCHE_WANN", "T00865.VRP_FILENAME" ) )

get.columns( main.1205, "T00865." )

# lade sic pseudonym liste
load( "data/original/20170922sicpseudoliste.rdata" ) 

# SIC Kuration
# LI00254152 -> LI01301935
# LI00236737 -> LI03111117

sicpseudo$SIC[ sicpseudo$SIC == "LI00254152" ] <- "LI01301935" ## nicht nehmen, da sich das geschlecht geaendert hat"
sicpseudo$SIC[ sicpseudo$SIC == "LI00236737" ] <- "LI03111117" 

# haenge noch sic spalte ran, weil die rohdaten original daten enthalten
main.1205$SIC <-
    sicpseudo$SIC[ match( main.1205$PSEUDONYM, sicpseudo$PSEUDONYM ) ]

main.1205 <-
    main.1205[ main.1205$SIC != "LI01301935", ]

t( table.df( main.1205 ) )

# die 32 ohne Geschlecht haben die ganze Anthro nicht
t( table.df( main.1205[ is.na( main.1205$sex ), ] ) )

# die sind nicht ueber pubstat zu retten
t( table.df( main.1205[ is.na( main.1205$age ) & is.na( main.1205$C_PUB_STAT_AGE ), ] ) )
main.1205[ is.na( main.1205$sex ) & is.na( main.1205$C_PUB_STAT_GENDER ), c( "PSEUDONYM", "C_ANTHRO_KH_GRP" ) ]
main.1205[ is.na( main.1205$age ) & is.na( main.1205$C_PUB_STAT_AGE ), c( "PSEUDONYM", "C_ANTHRO_KH_GRP" ) ]

# die koennte man ueber PUB kurieren
main.1205[ is.na( main.1205$sex ) & !is.na( main.1205$C_PUB_STAT_GENDER ), c( "PSEUDONYM", "C_ANTHRO_KH_GRP", "C_PUB_STAT_GENDER", "C_PUB_STAT_AGE" ) ]
main.1205[ is.na( main.1205$age ) & !is.na( main.1205$C_PUB_STAT_AGE ), c( "PSEUDONYM", "C_ANTHRO_KH_GRP", "C_PUB_STAT_GENDER", "C_PUB_STAT_AGE" ) ]

# main.1205$sex[ is.na( main.1205$sex ) & !is.na( main.1205$C_PUB_STAT_GENDER ) ] <-
#     c( "female", "male" )[ match( main.1205$C_PUB_STAT_GENDER, c( 2, 1 ) ) ]
# 
# main.1205$age[ is.na( main.1205$age ) & !is.na( main.1205$C_PUB_STAT_AGE ) ] <-
#     main.1205$C_PUB_STAT_AGE

( out <-
    comb.cols( main.1205[ is.na( main.1205$age ) & is.na( main.1205$C_PUB_STAT_AGE ), c( "PSEUDONYM", "C_ANTHRO_KH_GRP" ) ], c( "PSEUDONYM", "C_ANTHRO_KH_GRP" ) ) )

t( table.df( main.1205[ comb.cols( main.1205, c( "PSEUDONYM", "C_ANTHRO_KH_GRP" ) ) %in% out, ] ) )

# die drei fliegen raus
main.1205 <-
    main.1205[ !comb.cols( main.1205, c( "PSEUDONYM", "C_ANTHRO_KH_GRP" ) ) %in% out, ] 

# geschlechter umkodieren
main.1205$sex[ is.na( main.1205$sex ) ] <- c( "female", "male" )[ match( main.1205$C_PUB_STAT_GENDER[ is.na( main.1205$sex ) ], c( 2, 1 ) ) ]
main.1205$age[ is.na( main.1205$age ) ] <- round( main.1205$C_PUB_STAT_AGE[ is.na( main.1205$age ) ], 2 )

# noch was uebrig? NEIN!
###############################################
t( table.df( main.1205 ) )

# loesche sex.12
main.1205 <-
    select( main.1205, -sex.12 )

rm( sicpseudo )

# Speichere Tabelle wieder als Excel und
# als RData (viel schneller!!!) ab 
###############################################
# WriteXLS( main.1205, ExcelFileName = "data/main/PV0278_datajoin_20170929.xlsx" ) #doesn't save correctly pseudonyms
write.xlsx( main.1205, file = "data/generated/main.table.xlsx" )

save( main.1205, file = "data/generated/main.table.Rd" )

rm( list = ls( ) )
