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
        "readxl",
        "WriteXLS" ) )

# musste an Deinen Pfad anpassen
###############################################
setwd( "~/LIFE/life-for-postgraduates/TobiasDienerowitz/" )

# bei mir liegen die Tabellen in einem
# Unterordner data/main/
###############################################
main.1004 <-
    read_excel( "data/main/PV0278_datajoin_20161004.xlsx" )

# im aufklaerungsgespraech sind alle drin, 
# die fehlen
###############################################
aufkl <-
    read_excel( "data/PV0278_R00001.xlsx" )

# Kuriere der Geschlechter
###############################################
for( s in unique( main.1004$PSEUDONYM[ is.na( main.1004$sex ) ] ) ) {
    
    main.1004$sex[ main.1004$PSEUDONYM == s ] <-
        c( "male", "female" )[ match( aufkl$TEILNEHMER_GESCHLECHT[ aufkl$TEILNEHMER_SIC == s ][ 1 ], c( 1, 2 ) ) ] }

# noch was uebrig? NEIN!
###############################################
sum( is.na( main.1004$sex ) )

# Speichere Tabelle wieder als Excel und
# als RData (viel schneller!!!) ab 
###############################################
WriteXLS( main.1004, ExcelFileName = "data/main/PV0278_datajoin_20170929.xlsx" )
save( main.1004, file = "data/main/main.table.Rd" )
