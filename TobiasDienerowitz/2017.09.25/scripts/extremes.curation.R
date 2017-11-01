##
# gib speicher frei
##
rm( list = ls( ) )

##
# install newest version
##
devtools::install_github( "TPeschel/hlpr4life" )

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
setwd( "~/LIFE/life-for-postgraduates/TobiasDienerowitz/2017.09.25" )

load( file = "data/generated/main.table.only.usable.with.extremes.Rd" )

mt <-
    main.1205.only.usable.with.extremes

rm( main.1205.only.usable.with.extremes )

get.sic.columns( mt )
get.scigroup.columns( mt )
get.date.columns( mt )

( keys <-
    key( mt[ mt$EXTREME, ], c( "SIC", "C_ANTHRO_KH_GRP" ), " | " ) )

t <-
    mt[ !key( mt, c( "SIC", "C_ANTHRO_KH_GRP" ), " | " ) %in% keys, ]

table.df( mt, F )

save( mt, file = "data/generated/main.table.only.usable.without.extremes.Rd" )
WriteXLS::WriteXLS( mt, "data/generated/main.table.only.usable.without.extremes.xlsx" )

