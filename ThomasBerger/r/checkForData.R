## check for data

## delete all data
rm( list = ls( ) )

library( dplyr )
library( gamlss )
library( ggplot2 )
library( readxl )
library( reshape2 )
library( WriteXLS )

## day of computation
date.today <- gsub( x = Sys.Date( ), pattern = "-1perFam", replacement = "" )

load( file = "~/LIFE/github-tpeschel/R/ThomasBerger/results/refs.Rda" )

tbl <-
    rbind(
        data.frame(
            age = refs$age,
            sprech2.p03 = qBCCG( 0.03, mu = refs$sprech2.female.m, nu = refs$sprech2.female.l, sigma = refs$sprech2.female.s ),
            sprech2.p10 = qBCCG( 0.10, mu = refs$sprech2.female.m, nu = refs$sprech2.female.l, sigma = refs$sprech2.female.s ),
            sprech2.p50 = qBCCG( 0.50, mu = refs$sprech2.female.m, nu = refs$sprech2.female.l, sigma = refs$sprech2.female.s ),
            sprech2.p90 = qBCCG( 0.90, mu = refs$sprech2.female.m, nu = refs$sprech2.female.l, sigma = refs$sprech2.female.s ),
            sprech2.p97 = qBCCG( 0.97, mu = refs$sprech2.female.m, nu = refs$sprech2.female.l, sigma = refs$sprech2.female.s ),
            sex = "female" ),
        data.frame(
            age = refs$age,
            sprech2.p03 = qBCCG( 0.03, mu = refs$sprech2.male.m, nu = refs$sprech2.male.l, sigma = refs$sprech2.male.s ),
            sprech2.p10 = qBCCG( 0.10, mu = refs$sprech2.male.m, nu = refs$sprech2.male.l, sigma = refs$sprech2.male.s ),
            sprech2.p50 = qBCCG( 0.50, mu = refs$sprech2.male.m, nu = refs$sprech2.male.l, sigma = refs$sprech2.male.s ),
            sprech2.p90 = qBCCG( 0.90, mu = refs$sprech2.male.m, nu = refs$sprech2.male.l, sigma = refs$sprech2.male.s ),
            sprech2.p97 = qBCCG( 0.97, mu = refs$sprech2.male.m, nu = refs$sprech2.male.l, sigma = refs$sprech2.male.s ),
            sex = "male" ) )
tbl <-
    tbl[ between( refs$age, 6.5, 17.5 ), ]

tbl$age.cat <-
    cut( 
        tbl$age,
        breaks = seq( from = 6.25, to = 17.75, by = .5 ),
        labels = as.character( seq( from = 6.5, to = 17.5, by = .5 ) ) )

tbl.perc <-
    tbl %>%
    group_by( age.cat, sex ) %>%
    summarise( 
        p03 = round( sprech2.p03[ which.min( abs( as.numeric( age.cat ) - age ) ) ], 0 ),
        p10 = round( sprech2.p10[ which.min( abs( as.numeric( age.cat ) - age ) ) ], 0 ),
        p50 = round( sprech2.p50[ which.min( abs( as.numeric( age.cat ) - age ) ) ], 0 ),
        p90 = round( sprech2.p90[ which.min( abs( as.numeric( age.cat ) - age ) ) ], 0 ),
        p97 = round( sprech2.p97[ which.min( abs( as.numeric( age.cat ) - age ) ) ], 0 ) )

tbl.perc <-
    tbl %>%
    group_by( age.cat, sex ) %>%
    summarise( 
        p03 = round( mean( sprech2.p03, na.rm = T ), 0 ),
        p10 = round( mean( sprech2.p10, na.rm = T ), 0 ),
        p50 = round( mean( sprech2.p50, na.rm = T ), 0 ),
        p90 = round( mean( sprech2.p90, na.rm = T ), 0 ),
        p97 = round( mean( sprech2.p97, na.rm = T ), 0 ) )

tbl.perc.plot <-
    melt( 
        tbl.perc,
        c( "age.cat", "sex" ) )

f0.sprech2.perc.orig <-
    read_excel( "../original/f0.sprech2.perc.orig.xlsx" )

tbl.orig.perc.plot <-
    melt( 
        f0.sprech2.perc.orig,
        c( "age", "sex" ) )

tbl.perc.plot$age.cat <-
    as.numeric( as.character( tbl.perc.plot$age.cat ) )

ggplot( ) +
    geom_point( data = tbl.orig.perc.plot, aes( age, value, group = variable, col = variable ) ) +
    geom_path( data = tbl.orig.perc.plot, aes( age, value, group = variable, col = variable ) ) +
    geom_point( data = tbl.perc.plot, aes( age.cat, value, group = variable, col = variable ) ) +
    geom_path( data = tbl.perc.plot, aes( age.cat, value, group = variable, col = variable ) ) +
    labs( label = "Unterschiede der Perzentilenberechnung von Frau Dr. Pietzner und mir", x = "Alter [y]", y = "Sprechen 2 - f0 [Hz]" ) +
    annotate( "text", x = 16, y = 277, label = "Neu", col = "#FF4040"  ) +
    annotate( "text", x = 16, y = 285, label = "Original", col = "#4040FF" ) +
    theme_bw( ) +
    facet_grid( . ~ sex )

ggsave( "../results/unterschiede-petzentilkurven1.png" )

ggplot( ) +
    geom_point( data = tbl.orig.perc.plot, aes( age, value, group = variable ), col = "#4040FF" ) +
    geom_path( data = tbl.orig.perc.plot, aes( age, value, group = variable ), col = "#4040FF" ) +
    geom_point( data = tbl.perc.plot, aes( age.cat, value, group = variable ), col = "#FF4040" ) +
    geom_path( data = tbl.perc.plot, aes( age.cat, value, group = variable ), col = "#FF4040" ) +
    labs( label = "Unterschiede der Perzentilenberechnung von Frau Dr. Pietzner und mir", x = "Alter [y]", y = "Sprechen 2 - f0 [Hz]" ) +
    annotate( "text", x = 16, y = 277, label = "Neu", col = "#FF4040"  ) +
    annotate( "text", x = 16, y = 285, label = "Original", col = "#4040FF" ) +
    theme_bw( ) +
    facet_grid( . ~ sex )

ggsave( "../results/unterschiede-petzentilkurven2.png" )

tbl.perc <- 
    arrange( tbl.perc, sex, age.cat )

WriteXLS( tbl.perc, "../results/f0_sprech2-perc.xlsx" )
