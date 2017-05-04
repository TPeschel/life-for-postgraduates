setwd( "~/LIFE/github-tpeschel/life-for-postgraduates/WiebkeKeller/r/20170504/" )

source( "join.R" )
library( ggplot2 )

tbl.esdq.ges <-
    tbl[ !is.na( tbl$E_SDQ_GES_SCORE ), ]

table( tbl.esdq.ges$E_SDQ_GES_SCORE, tbl.esdq.ges$SEX )

summary( tbl.esdq.ges$E_SDQ_GES_SCORE )

ggplot( 
    tbl.esdq.ges,
    aes( E_SDQ_GES_SCORE, fill = SEX ) ) +
    geom_bar( binwidth = 1 ) +
    facet_grid( . ~ SEX ) +
    theme_bw( ) +
    scale_fill_brewer( type = "qual", palette = 6, direction = -1 ) +
    labs( title = "Besuche E-SDQ-Gesamtscore" )




##Wie viele Kinder haben auffälliges Ergebnis (>17), bzw. grenzwertiges
#Ergebnis(14-16)
tbl.17 <-
    tbl.esdq.ges[ tbl.esdq.ges$E_SDQ_GES_SCORE >= 17, ]

table( tbl.17$E_SDQ_GES_SCORE, tbl.17$SEX )

summary( tbl.17$E_SDQ_GES_SCORE )

ggplot( 
    tbl.17,
    aes( E_SDQ_GES_SCORE, fill = SEX ) ) +
    geom_bar( binwidth = 1 ) +
    facet_grid( . ~ SEX ) +
    theme_bw( ) +
    scale_fill_brewer( type = "qual", palette = 6, direction = -1 ) +
    labs( title = "Besuche E-SDQ-Gesamtscore >= 17" )

tbl.14.16 <-
    tbl.esdq.ges[ between( tbl.esdq.ges$E_SDQ_GES_SCORE, 14, 16.99 ), ]

table( tbl.14.16$E_SDQ_GES_SCORE, tbl.14.16$SEX )

summary( tbl.14.16$E_SDQ_GES_SCORE )

ggplot( 
    tbl.14.16,
    aes( E_SDQ_GES_SCORE, fill = SEX ) ) +
    geom_bar( binwidth = 1 ) +
    facet_grid( . ~ SEX ) +
    theme_bw( ) +
    scale_fill_brewer( type = "qual", palette = 6, direction = -1 ) +
    labs( title = "Besuche E-SDQ-Gesamtscore zwischen 14 und 16" )


## Ab hier habe ich eine Lust mehr
## Das Prinzip sollte klar geworden sein.
## Verwechsle bitte Kinder nicht Besuchen!

##Wie viele der Kinder mit auffälligem Gesamt-Score zeigen Hyperaktivität?
d00149.17$E_SDQ_HYP_SUM
d00149.17[d00149.17$E_SDQ_HYP_SUM >= 7,]
summary(d00149.17$E_SDQ_HYP_SUM >= 7, all=F)
ggplot(d00149.17, aes(x = E_SDQ_HYP_SUM))+geom_bar()

##Wie viele der Kinder mit auffälligem Gesamt-Score zeigen emotionale
#Probleme?
d00149.17$E_SDQ_EMO_SUM
d00149.17[d00149.17$E_SDQ_EMO_SUM >= 5,]
summary(d00149.17$E_SDQ_EMO_SUM >= 5, all=F)
ggplot(d00149.17, aes(x= E_SDQ_EMO_SUM))+geom_bar()

##Wie viele der Kinder mit auffälligem Gesamt-Score zeigen 
#Verhaltensauffälligkeiten?
d00149.17$E_SDQ_VER_SUM
d00149.17[d00149.17$E_SDQ_VER_SUM > 4,]
summary(d00149.17$E_SDQ_VER_SUM >4, all=F)
ggplot(d00149.17, aes(x=E_SDQ_VER_SUM))+geom_bar()

##Wie viele Kinder mit auffälligem Gesamt-Score zeigen Probleme mit
#Gleichaltrigen?
d00149.17$E_SDQ_SOP_SUM
d00149.17[d00149$E_SDQ_SOP_SUM >=4,]
summary(d00149.17$E_SDQ_SOP_SUM >=4, all=F)
ggplot(d00149.17, aes(x=E_SDQ_SOP_SUM))+geom_bar()
