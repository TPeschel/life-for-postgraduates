setwd( "~/LIFE/github-tpeschel/life-for-postgraduates/WiebkeKeller/r/" )

source( "join.R" )
library( ggplot2 )

tbl.sdq.ges <-
    tbl[ !is.na( tbl$SDQ_GES_SCORE ), ]

table( tbl.sdq.ges$SDQ_GES_SCORE )

summary( tbl.sdq.ges$SDQ_GES_SCORE )

ggplot( 
    tbl.sdq.ges,
    aes( SDQ_GES_SCORE, fill = SEX ) ) +
    geom_bar( binwidth = 1 ) +
    facet_grid( . ~ SEX ) +
    theme_bw( ) +
    scale_fill_brewer( type = "qual", palette = 6, direction = -1 ) +
    labs( title = "Besuche SDQ-Gesamtscore" )

## Auffaelliger Gesamt-Score ########################################################################################
## Wie viele Besuche haben auffälligen Gesamt-Score?
tbl.20 <- 
    tbl.sdq.ges[tbl.sdq.ges$SDQ_GES_SCORE >= 20,]

ggplot( 
    tbl.20,
    aes( SDQ_GES_SCORE, fill = SEX ) ) +
    geom_bar( binwidth = 1 ) +
    facet_grid( . ~ SEX ) +
    theme_bw( ) +
    scale_fill_brewer( type = "qual", palette = 6, direction = -1 ) +
    labs( title = "Besuche SDQ-Gesamtscore ueber 20" )

table( tbl.20$SDQ_GES_SCORE, tbl.20$SEX )

## Auffaelliger Gesamt-Score und Hyperaktivitaet #####################################################################
## Wie viele der Besuche mit auffälligen Gesamt-Score zeigen Hyperaktivität?
tbl.20.Hyp <-
    tbl.20[ tbl.20$SDQ_HYP_SUM >= 7, ]

summary( tbl.20.Hyp$SDQ_HYP_SUM )
table( tbl.20.Hyp$SDQ_HYP_SUM, tbl.20.Hyp$SEX )

ggplot( 
    tbl.20.Hyp, 
    aes( SDQ_HYP_SUM, fill = SEX ) ) +
    geom_bar( binwidth = 1 ) +
    facet_grid( . ~ SEX ) +
    theme_bw( ) +
    scale_fill_brewer( type = "qual", palette = 6, direction = -1 ) +
    labs( title = "Besuche SDQ-Gesamtscore >= 20 und Hyperaktivitaet HYP_SUM >= 7" )




## Auffaelliger Gesamt-Score und emotionale Probleme #################################################################
## Wie viele der Besuche mit auffälligen Gesamt-Score zeigen emotionale Probleme?
tbl.20.Emo <-
    tbl.20[ tbl.20$SDQ_EMO_SUM >= 7, ]

summary( tbl.20.Emo$SDQ_EMO_SUM )

table( tbl.20.Emo$SDQ_EMO_SUM, tbl.20.Emo$SEX )

ggplot( 
    tbl.20.Emo, 
    aes( SDQ_EMO_SUM, fill = SEX ) ) +
    geom_bar( binwidth = 1 ) +
    facet_grid( . ~ SEX ) +
    theme_bw( ) +
    scale_fill_brewer( type = "qual", palette = 6, direction = -1 ) +
    labs( title = "Besuche SDQ-Gesamtscore >= 20 und Emotionale Problem EMO_SUM >= 7" )



## Auffaelliger Gesamt-Score und Verhaltensauffaelligkeiten ##########################################################
## Wie viele der Besuche mit auffälligen Gesamt-Score zeigen Verhaltens-auffälligkeiten?
tbl.20.Ver <-
    tbl.20[ tbl.20$SDQ_VER_SUM >= 5, ]

summary( tbl.20.Ver$SDQ_VER_SUM )

table( tbl.20.Ver$SDQ_VER_SUM, tbl.20.Ver$SEX )

ggplot( 
    tbl.20.Ver, 
    aes( SDQ_VER_SUM, fill = SEX ) ) +
    geom_bar( binwidth = 1 ) +
    facet_grid( . ~ SEX ) +
    theme_bw( ) +
    scale_fill_brewer( type = "qual", palette = 6, direction = -1 ) +
    labs( title = "Besuche SDQ-Gesamtscore >= 20 und Auffaelliges Verhalten VER_SUM >= 5" )




## Auffaelliger Gesamt-Score und Probleme mit Gleichaltrigen #########################################################
##Wie viele der Besuche mit auffälligem Gesamt-Score zeigen Probleme mit Gleichaltrigen?
tbl.20.Sop <-
    tbl.20[ tbl.20$SDQ_SOP_SUM >= 6, ]

summary( tbl.20.Sop$SDQ_SOP_SUM )

table( tbl.20.Sop$SDQ_SOP_SUM, tbl.20.Sop$SEX )

ggplot( 
    tbl.20.Sop, 
    aes( SDQ_SOP_SUM, fill = SEX ) ) +
    geom_bar( binwidth = 1 ) +
    facet_grid( . ~ SEX ) +
    theme_bw( ) +
    scale_fill_brewer( type = "qual", palette = 6, direction = -1 ) +
    labs( title = "Besuche SDQ-Gesamtscore >= 20 und Problemen mit Gleichaltrigen SOP_SUM >= 6" )

