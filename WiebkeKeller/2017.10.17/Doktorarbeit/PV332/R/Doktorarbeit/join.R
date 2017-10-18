rm( list = ls( ) )

library( dplyr )
library( readxl )
library( lubridate )

owd <- getwd( )

setwd( "../../data/" )

tbl.join <-
    read_excel( "PV0332_Gesamt_Join2.xlsx" )

tbl.wnklr <-
    read_excel( "PV0332_D00177.xlsx" )

tbl.join$JAHR <-
    year( tbl.join$E_SDQ_EDAT )

tbl <-
    merge( 
        tbl.join,
        tbl.wnklr,
        by.x = c( "TEILNEHMER_SIC", "JAHR" ),
        by.y = c( "PSEUDONYM", "JAHR" ),
        all = F )

tbl <-
    tbl[ !is.na( tbl$SCORE_FAM ),]

tbl$SIC <-
    tbl$TEILNEHMER_SIC

tbl$TEILNEHMER_SIC <-
    NULL

tbl$SEX <-
    factor( 
        tbl$TEILNEHMER_GESCHLECHT,
        levels = c( 1, 2 ),
        labels = c( "male", "female") )
        
tbl$TEILNEHMER_GESCHLECHT <- 
    NULL

tbl$wnklr <-
    cut( 
        tbl$SCORE_FAM,
        breaks = c( 0, 8, 14, 21 ),
        labels = c( "LOW", "MID", "HIGH" ) )

setwd( owd )

rm( list = c( "tbl.wnklr", "tbl.join" ) )

