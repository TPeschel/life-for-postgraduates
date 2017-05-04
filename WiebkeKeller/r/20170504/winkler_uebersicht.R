setwd( "~/LIFE/github-tpeschel/life-for-postgraduates/WiebkeKeller/r/20170504/" )

source( "join.R" )
library( ggplot2 )

my.theme <-
    theme_bw( )

## ## Wieviele Besuche haben welchen Winklerindex?
table( tbl$wnklr, tbl$SEX )

ggplot( 
    tbl, 
    aes( wnklr, fill = SEX ) ) + 
    geom_bar( stat = "count" ) +
    facet_grid( . ~ SEX ) +
    my.theme +
    scale_fill_brewer( type = "qual", palette = 6, direction = -1 ) +
    labs( title = "Besuche" )

## Ermittle zu jeder Familie einen Eintrag fuer den Famscore aus Mittelwert aller
tbl.fam <-
    tbl %>%
    group_by( FAM_PSEUDO ) %>%
    summarise( score.fam = mean( SCORE_FAM ) )

## Gruppiere in LOW, MID, HIGH
tbl.fam$wnklr <-
    cut( 
        tbl.fam$score.fam,
        breaks = c( 0, 8, 14, 21 ),
        labels = c( "LOW", "MID", "HIGH" ) )

## Wieviele Familien haben welchen Winklerindex?
table( tbl.fam$wnklr )

ggplot( 
    tbl.fam, 
    aes( wnklr ) ) + 
    geom_bar( stat = "count" ) +
    my.theme +
    labs( title = "Familien" )

## Ermittle zu jedem Kind einen Eintrag fuer den Famscore aus Mittelwert aller
tbl.knd <-
    tbl %>%
    group_by( SIC, SEX ) %>%
    summarise( score.fam = mean( SCORE_FAM ) )

## Gruppiere in LOW, MID, HIGH
tbl.knd$wnklr <-
    cut( 
        tbl.knd$score.fam,
        breaks = c( 0, 8, 14, 21 ),
        labels = c( "LOW", "MID", "HIGH" ) )

## Wieviele Kinder haben welchen Winklerindex?
table( tbl.knd$wnklr, tbl.knd$SEX )

ggplot( 
    tbl.knd, 
    aes( wnklr, fill = SEX ) ) + 
    geom_bar( stat = "count" ) +
    facet_grid( . ~ SEX ) +
    theme_bw( ) +
    scale_fill_brewer( type = "qual", palette = 6, direction = -1 ) +
    labs( title = "Kinder" )

