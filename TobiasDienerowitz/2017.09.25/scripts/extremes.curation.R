##
# gib speicher frei
##
rm( list = ls( ) )

##
# lade alle noetigen Pakete
##
hlpr4life::load.pkgs(
    c(
        "hlpr4life",
        "readxl",
        "ggplot2",
        "dplyr",
        "reshape2",
        "knitr" ) )

##
# setze Pfad auf Projektverzeichnis
##
# setwd( "~/LIFE/life-for-postgraduates/TobiasDienerowitz/2017.09.25" )

load( file = "data/generated/main.table.only.usable.with.extremes.Rd" )

mt <-
    main.1205.only.usable.with.extremes

rm( main.1205.only.usable.with.extremes )

get.sic.columns( mt )
get.scigroup.columns( mt )
get.date.columns( mt )
table.df( mt, F )

load( "data/original/raw.dat.Rd" )

table.df( res, F )

res$dsi

extr <-
    key.df( mt[ mt$EXTREME, ], c( "SIC", "C_ANTHRO_KH_GRP" ) )

nrow( mt[ key.df( mt, c( "SIC", "C_ANTHRO_KH_GRP" ) ) == extr[ 2 ], ] )


mt <-
    mt[ !mt$EXTREME, ]
#2681
save( mt, file = "data/generated/main.table.only.usable.without.extremes.Rd" )
WriteXLS::WriteXLS( mt, "data/generated/main.table.only.usable.without.extremes.xlsx" )

table.df( mt, F, T, F )

rm( mt )
