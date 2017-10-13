<<<<<<< HEAD
    # loesche speicher
=======
# loesche speicher
>>>>>>> e55827e4e68a2da1380e5cde1a10c31a120aab6a
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

# musste an Deinen Pfad anpassen
###############################################
setwd( "~/LIFE/life-for-postgraduates/TobiasDienerowitz/2017.09.25/" )

# bei mir liegen die Tabellen in einem
<<<<<<< HEAD
# Unterordner data/main/
=======
# Unterordner data/original/
>>>>>>> e55827e4e68a2da1380e5cde1a10c31a120aab6a
###############################################
main.1205 <-
    read_excel( "data/original/PV0278_datajoin20161205.xlsx" )

main.1205[ , grep( "sex", names( main.1205 ) ) ]

sapply( grep( "sex", names( main.1205 ) ), function( s ) sum( is.na( main.1205[ , s ] ) ) )

# im aufklaerungsgespraech sind alle drin, 
# die fehlen
###############################################
aufkl <-
    read_excel( "data/original/PV0278_R00001.xlsx" )

# Kurieren der Geschlechter
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
write.xlsx( main.1205, file = "data/generated/main.table.2017.09.25.xlsx" )

save( main.1205, file = "data/generated/main.table.2017.09.25.Rd" )
