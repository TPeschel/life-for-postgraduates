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
setwd( "~/LIFE/life-for-postgraduates/TobiasDienerowitz/2017.09.25" )

load( file = "data/generated/main.table.only.usable.with.extremes.Rd" )

names( main.1205.only.usable.with.extremes )

tbl <-
    main.1205.only.usable.with.extremes

rm( main.1205.only.usable.with.extremes )

t( table.df( tbl[ , get.date.columns( tbl ) ] ) )
t( table.df( tbl[ , get.columns( tbl, "sex|gender|geschlecht" ) ] ) )

not.usable <-
    read.csv( "data/original/non_usable_obs.csv" )

extr <-
    read.csv( "data/original/obs_extremes.csv" )

t( table.df( extr ) )

names( extr ) <-
    gsub( "^NA.", "T00865.", names( extr ) )

names( not.usable ) <-
    gsub( "^NA.", "T00865.", names( not.usable ) )

extr$AGE <-
    as.factor( extr$AGE )

not.usable$AGE <-
    as.factor( not.usable$AGE )

t( table.df( extr[, get.date.columns( extr ) ] ) )
t( table.df( not.usable[, get.date.columns( not.usable ) ] ) )

theme.histo <-
    list(
        theme_bw( ),
        geom_histogram( stat = "count" ),
        scale_fill_manual( values = c( "red", "blue" ) ),
        facet_grid( sex ~ . ) )

plot.tbl.age.sex <-
    ggplot( tbl, aes( AGE, fill = sex ) ) +
        theme.histo

plot.extr.age.sex <-
    ggplot( extr, aes( AGE, fill = sex ) ) +
        theme.histo

plot.nu.age.sex <-
    ggplot( not.usable[ !is.na( not.usable$AGE ), ], aes( AGE, fill = sex ) ) +
    theme.histo

plot.tbl.motivation <-
    ggplot( tbl, aes( U_Sing.singen_mot, fill = sex ) ) +
    theme.histo

plot.extr.motivation <-
    ggplot( extr[ !is.na( extr$U_Sing.singen_mot ), ], aes( U_Sing.singen_mot, fill = sex ) ) +
    theme.histo

plot.nu.motivation <-
    ggplot( not.usable[ !is.na( not.usable$U_Sing.singen_mot ), ], aes( U_Sing.singen_mot, fill = sex ) ) +
    theme.histo

plot.tbl.winkler <-
    ggplot( tbl[ !is.na( tbl$WINKLER_SCORE_FAM ), ], aes( as.factor( round( WINKLER_SCORE_FAM ) ), fill = sex ) ) +
    theme.histo

plot.extr.winkler <-
    ggplot( extr[ !is.na( extr$WINKLER_SCORE_FAM ), ], aes( as.factor( round( WINKLER_SCORE_FAM ) ), fill = sex ) ) +
    theme.histo

plot.nu.winkler <-
    ggplot( not.usable[ !is.na( not.usable$WINKLER_SCORE_FAM ), ], aes( as.factor( round( WINKLER_SCORE_FAM ) ), fill = sex ) ) +
    theme.histo

setwd( "pdf/")

today <-
    gsub( "-", "", Sys.Date( ) )

outname <-
    paste0( "extremes.stat[", today, "].tex" )

knit2pdf( "../scripts/extremes.stat.Rnw", outname )

#options( warn = oldw )
     