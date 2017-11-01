rm( list = ls( ) )

hlpr4life::load.pkgs(
    c(
        "hlpr4life",
        "ggplot2",
        "dplyr",
        "reshape2",
        "openxlsx",
        "db.access" ) )

setwd( "~/LIFE/life-for-postgraduates/TobiasDienerowitz/2017.09.25/" )

# lade join mit korrigierten Geschlechtern
load( "data/generated/main.table.Rd" ) 

obs.non.usable <-
    read.csv( "data/original/non_usable_obs.csv" )
#    read.csv( "data/original/NX2P7X~W" )

# da stimmen ein paar namen nicht
names( obs.non.usable )

names( obs.non.usable ) <-
    gsub( "^NA\\.", "T00865\\.", names( obs.non.usable ) )

names( obs.non.usable )
# out <-
#     paste0( obs.non.usable$PSEUDONYM, obs.non.usable$C_ANTHRO_KH_GRP )
# 
# main.1205.only.usable <-
#     main.1205[ !paste0( main.1205$PSEUDONYM, main.1205$C_ANTHRO_KH_GRP ) %in% out, ]

# macht dasselbe wie die beiden kommentierten befehle zuvor
main.1205.only.usable <-
    anti_join( main.1205, obs.non.usable, by = c( "PSEUDONYM", "C_ANTHRO_KH_GRP" ) )

# lade tabelle mit extremen werten
obs.extreme <-
    read.csv( "data/original/obs_extremes.csv" )

names( obs.extreme )

# hier stimmen auch manche namen nicht
names( obs.extreme ) <-
    gsub( "^NA\\.", "T00865\\.", names( obs.extreme ) )

names( obs.extreme )

# baue kleinen dataframe fuers mergen
oe <-
    obs.extreme[ , c( "PSEUDONYM", "C_ANTHRO_KH_GRP" ) ]

# erzeuge Spalte Extreme in der fuer alle extremen Messungen ein TRUE steht
oe$EXTREME <-
    T

main.1205.only.usable.with.extremes <-
    merge( 
        main.1205.only.usable,
        oe,
        by = c( "PSEUDONYM", "C_ANTHRO_KH_GRP" ),
        all.x = T )

# fuelle die NAs in der EXTREME Spalte mit FALSE auf
main.1205.only.usable.with.extremes$EXTREME[ is.na( main.1205.only.usable.with.extremes$EXTREME ) ] <-
    F

# main.1205.only.usable.with.extremes <-
#     rename.columns( 
#         main.1205.only.usable.with.extremes,
#         "C_ANTHRO_KH_AGE", 
#         "age" )

main.1205.only.usable.with.extremes$AGE <-
    floor( main.1205.only.usable.with.extremes$age )

table.df( main.1205.only.usable.with.extremes, F )

# WriteXLS( main.1205, ExcelFileName = "data/main/PV0278_datajoin_20170929.xlsx" ) #doesn't save correctly pseudonyms
write.xlsx( main.1205.only.usable.with.extremes, file = "data/generated/main.table.only.usable.with.extremes.xlsx" )

save( main.1205.only.usable.with.extremes, file = "data/generated/main.table.only.usable.with.extremes.Rd" )

rm( list = ls( ) )

