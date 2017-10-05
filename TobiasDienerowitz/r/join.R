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
        "openxlsx" ) )

# musste an Deinen Pfad anpassen
###############################################
setwd( "~/LIFE/life-for-postgraduates/TobiasDienerowitz/" )

# bei mir liegen die Tabellen in einem
# Unterordner data/main/
###############################################
main.1205 <-
    read_excel( "data/main/PV0278_datajoin20161205.xlsx" )
#    read_excel( "data/main/PV0278_datajoin_20161004.xlsx" )

main.1205[ , grep( "sex", names( main.1205 ) ) ]

sapply( grep( "sex", names( main.1205 ) ), function( s ) sum( is.na( main.1205[ , s ] ) ) )

# im aufklaerungsgespraech sind alle drin, 
# die fehlen
###############################################
aufkl <-
    read_excel( "data/PV0278_R00001.xlsx" )

# Kuriere der Geschlechter
###############################################
for( s in unique( main.1205$PSEUDONYM[ is.na( main.1205$sex ) ] ) ) {
    
    main.1205$sex[ main.1205$PSEUDONYM == s ] <-
        c( "male", "female" )[ match( aufkl$TEILNEHMER_GESCHLECHT[ aufkl$TEILNEHMER_SIC == s ][ 1 ], c( 1, 2 ) ) ] }

# noch was uebrig? NEIN!
###############################################
sum( is.na( main.1205$sex ) )
sum( is.na( main.1205$sex.12 ) )

# loesche sex.12
main.1205 <-
    select( main.1205, -sex.12 )

# korriegiere fehlerhafte spaltennamen
main.1205 <-
    rename.columns( 
        main.1205,
        c( "C_PUB_STAT_MET00865RCHE", "C_PUB_STAT_MET00865RCHE_WANN", "T00865.VRP_FILET00865ME" ),
        c( "C_PUB_STAT_MENARCHE", "C_PUB_STAT_MENARCHE_WANN", "T00865.VRP_FILENAME" ) )

# Speichere Tabelle wieder als Excel und
# als RData (viel schneller!!!) ab 
###############################################
# WriteXLS( main.1205, ExcelFileName = "data/main/PV0278_datajoin_20170929.xlsx" ) #doesn't save correctly pseudonyms
write.xlsx( main.1205, file = "data/main/PV0278_datajoin_2017_09_25.xlsx" )

save( main.1205, file = "data/main/main.table.2017.09.25.Rd" )
