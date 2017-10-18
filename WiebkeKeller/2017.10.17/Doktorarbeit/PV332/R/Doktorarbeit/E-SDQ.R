setwd("../Doktorarbeit/")

source( "join.R" )
library( ggplot2 )
library( dplyr )
library( readxl )
library( lubridate )

##Gesamtscore der Besuche
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

##Wie viele Besuche haben auffälliges Ergebnis (>17), bzw. grenzwertiges
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


##Wie viele der Besuche mit auffälligem Gesamt-Score zeigen Hyperaktivität?
tbl.17.hyp <- tbl.17[tbl.17$E_SDQ_HYP_SUM >= 7,]
table( tbl.17.hyp$E_SDQ_HYP_SUM, tbl.17.hyp$SEX)
summary(tbl.17.hyp$E_SDQ_HYP_SUM)
ggplot(tbl.17.hyp,
       aes(E_SDQ_HYP_SUM, fill = SEX))+
      geom_bar(binwidth = 1)+
      facet_grid( . ~ SEX)+
      theme_bw()+
      scale_fill_brewer(type = "qual", palette = 6, direction = -1)+
      labs( title = "Besuche E-SDQ-Gesamtscore >=17 & Hyperaktivität >= 7")

##Wie viele der Besuche mit auffälligem Gesamt-Score zeigen emotionale
#Probleme?
tbl.17.emo <- tbl.17[tbl.17$E_SDQ_EMO_SUM >=5,]
table( tbl.17.emo$E_SDQ_EMO_SUM, tbl.17.emo$SEX)
summary(tbl.17.emo$E_SDQ_EMO_SUM)
ggplot(tbl.17.emo, aes(E_SDQ_EMO_SUM, fill= SEX))+ geom_bar(binwidth = 1)+
      facet_grid( . ~ SEX)+ theme_bw()+ scale_fill_brewer(type= "qual",
      palette = 6, direction = -1)+ labs( title =
      "Besuche E-SDQ-Gesamtscore >= 17 & emotionale Probleme >= 5")

##Wie viele der Besuche mit auffälligem Gesamt-Score zeigen 
#Verhaltensauffälligkeiten?
tbl.17.ver <- tbl.17[tbl.17$E_SDQ_VER_SUM >= 4,]
table( tbl.17.ver$E_SDQ_VER_SUM, tbl.17.ver$SEX)
summary(tbl.17.ver$E_SDQ_VER_SUM)
ggplot(tbl.17.ver, aes(E_SDQ_VER_SUM, fill= SEX))+ geom_bar(binwidth = 1)+
  facet_grid( . ~ SEX)+ theme_bw()+ scale_fill_brewer(type= "qual",
  palette = 6, direction = -1)+ 
  labs( title = "Besuche E-SDQ-Gesamtscore >= 17 & Verhaltensauffälligkeiten >=4")
                                                                                            
##Wie viele Besuche mit auffälligem Gesamt-Score zeigen Probleme mit
#Gleichaltrigen?
tbl.17.sop <- tbl.17[tbl.17$E_SDQ_SOP_SUM >=4,]
table( tbl.17.sop$E_SDQ_SOP_SUM, tbl.17.sop$SEX)
summary(tbl.17.sop$E_SDQ_SOP_SUM)
ggplot(tbl.17.sop, aes(E_SDQ_SOP_SUM, fill= SEX))+ geom_bar(binwidth = 1)+
  facet_grid( . ~ SEX)+ theme_bw()+ scale_fill_brewer(type= "qual",
  palette = 6, direction = -1)+ 
  labs( title = "Besuche E-SDQ-Gesamtscore >= 17 & Probleme Peers >=4")

##Gesamtscore der Kinder
tbl.esdq.ges.knd <- tbl.esdq.ges %>% group_by(SIC,SEX) %>% summarise(
                score.ges = mean(E_SDQ_GES_SCORE))

table(tbl.esdq.ges.knd$score.ges, tbl.esdq.ges.knd$SEX)

summary(tbl.esdq.ges.knd$score.ges)

ggplot( 
  tbl.esdq.ges.knd,
  aes( score.ges, fill = SEX ) ) +
  geom_bar( binwidth = 1 ) +
  facet_grid( . ~ SEX ) +
  theme_bw( ) +
  scale_fill_brewer( type = "qual", palette = 6, direction = -1 ) +
  labs( title = "Kinder E-SDQ-Gesamtscore" )

##Wie viele Kinder haben auffälliges Ergebnis (>17), bzw. grenzwertiges
#Ergebnis(14-16)

tbl.17.knd <- tbl.esdq.ges.knd [tbl.esdq.ges.knd$score.ges >=17,]

table(tbl.17.knd$score.ges, tbl.17.knd$SEX)

summary(tbl.17.knd$score.ges)

ggplot( 
  tbl.17.knd,
  aes( score.ges, fill = SEX ) ) +
  geom_bar( binwidth = 1 ) +
  facet_grid( . ~ SEX ) +
  theme_bw( ) +
  scale_fill_brewer( type = "qual", palette = 6, direction = -1 ) +
  labs( title = "Kinder E-SDQ-Gesamtscore >= 17" )

tbl.14.16.knd <-
  tbl.esdq.ges.knd[ between( tbl.esdq.ges.knd$score.ges, 14, 16.99 ), ]

table( tbl.14.16.knd$score.ges, tbl.14.16.knd$SEX )

summary( tbl.14.16.knd$score.ges )

ggplot( 
  tbl.14.16.knd,
  aes( score.ges, fill = SEX ) ) +
  geom_bar( binwidth = 1 ) +
  facet_grid( . ~ SEX ) +
  theme_bw( ) +
  scale_fill_brewer( type = "qual", palette = 6, direction = -1 ) +
  labs( title = "Kinder E-SDQ-Gesamtscore zwischen 14 und 16" )

##Wie viele der Kinder mit auffälligen Gesamtscore zeigen
#Hyperaktivität?

tbl.esdq.hyp <- tbl[ !is.na(tbl$E_SDQ_HYP_SUM),]
  
tbl.esdq.hyp.knd <- tbl.esdq.hyp %>% group_by(SIC,SEX) %>% summarise(
  score.hyp = mean(E_SDQ_HYP_SUM))

table(tbl.esdq.hyp.knd$score.hyp, tbl.esdq.hyp.knd$SEX)

summary(tbl.esdq.hyp.knd$score.hyp)

ggplot( 
  tbl.esdq.hyp.knd,
  aes( score.hyp, fill = SEX ) ) +
  geom_bar( binwidth = 1 ) +
  facet_grid( . ~ SEX ) +
  theme_bw( ) +
  scale_fill_brewer( type = "qual", palette = 6, direction = -1 ) +
  labs( title = "Kinder E-SDQ Hyperaktivität")

tbl.17.hyp.knd <- tbl.esdq.ges.knd [tbl.esdq.ges.knd$score.ges >= 17 &
                                      tbl.esdq.hyp.knd$score.hyp >=7,]
##Kann man das so machen?

table( tbl.17.hyp.knd$score.ges, tbl.17.hyp.knd$SEX)

summary(tbl.17.hyp.knd$score.ges)

ggplot( 
  tbl.17.hyp.knd,
  aes( score.ges, fill = SEX ) ) +
  geom_bar( binwidth = 1 ) +
  facet_grid( . ~ SEX ) +
  theme_bw( ) +
  scale_fill_brewer( type = "qual", palette = 6, direction = -1 ) +
  labs( title = "Kinder E-SDQ-Gesamtscore >= 17 & E-SDQ-Hyperaktivität >= 7" )


##Wie viele der Kinder mit auffälligem Gesamt-Score zeigen emotionale
#Probleme?

tbl.esdq.emo <- tbl[ !is.na(tbl$E_SDQ_EMO_SUM),]

tbl.esdq.emo.knd <- tbl.esdq.emo %>% group_by(SIC,SEX) %>% summarise(
  score.emo = mean(E_SDQ_EMO_SUM))

table(tbl.esdq.emo.knd$score.emo, tbl.esdq.emo.knd$SEX)

summary(tbl.esdq.emo.knd$score.emo)

ggplot( 
  tbl.esdq.emo.knd,
  aes( score.emo, fill = SEX ) ) +
  geom_bar( binwidth = 1 ) +
  facet_grid( . ~ SEX ) +
  theme_bw( ) +
  scale_fill_brewer( type = "qual", palette = 6, direction = -1 ) +
  labs( title = "Kinder E-SDQ Emotionale Probleme")


tbl.17.emo.knd <- tbl.esdq.ges.knd [tbl.esdq.ges.knd$score.ges >= 17 &
                                      tbl.esdq.emo.knd$score.emo >=5,]

table( tbl.17.emo.knd$score.ges, tbl.17.emo.knd$SEX)

summary(tbl.17.emo.knd$score.ges)

ggplot( 
  tbl.17.emo.knd,
  aes( score.ges, fill = SEX ) ) +
  geom_bar( binwidth = 1 ) +
  facet_grid( . ~ SEX ) +
  theme_bw( ) +
  scale_fill_brewer( type = "qual", palette = 6, direction = -1 ) +
  labs( title = "Kinder E-SDQ-Gesamtscore >= 17 & E-SDQ Emotionale Probleme >= 5" )

##Wie viele der Kinder mit auffälligen Gesamt-Score zeigen 
#Verhaltensauffälligkeiten?

tbl.esdq.ver <- tbl[ !is.na(tbl$E_SDQ_VER_SUM),]

tbl.esdq.ver.knd <- tbl.esdq.ver %>% group_by(SIC,SEX) %>% summarise(
  score.ver = mean(E_SDQ_VER_SUM))

table(tbl.esdq.ver.knd$score.ver, tbl.esdq.ver.knd$SEX)

summary(tbl.esdq.ver.knd$score.ver)

ggplot( 
  tbl.esdq.ver.knd,
  aes( score.ver, fill = SEX ) ) +
  geom_bar( binwidth = 1 ) +
  facet_grid( . ~ SEX ) +
  theme_bw( ) +
  scale_fill_brewer( type = "qual", palette = 6, direction = -1 ) +
  labs( title = "Kinder E-SDQ Verhaltensauffälligkeiten")


tbl.17.ver.knd <- tbl.esdq.ges.knd [tbl.esdq.ges.knd$score.ges >= 17 &
                                      tbl.esdq.ver.knd$score.ver >=4,]

table( tbl.17.ver.knd$score.ges, tbl.17.ver.knd$SEX)

summary(tbl.17.ver.knd$score.ges)

ggplot( 
  tbl.17.ver.knd,
  aes( score.ges, fill = SEX ) ) +
  geom_bar( binwidth = 1 ) +
  facet_grid( . ~ SEX ) +
  theme_bw( ) +
  scale_fill_brewer( type = "qual", palette = 6, direction = -1 ) +
  labs( title = "Kinder E-SDQ-Gesamtscore >= 17 & E-SDQ-Verhaltensauffälligkeiten >=4")

##Wie viele der Kinder mit auffälligem Gesamt-Score haben Probleme
#mit Gleichaltrigen?

tbl.esdq.sop <- tbl[ !is.na(tbl$E_SDQ_SOP_SUM),]

tbl.esdq.sop.knd <- tbl.esdq.sop %>% group_by(SIC,SEX) %>% summarise(
  score.sop = mean(E_SDQ_SOP_SUM))

table(tbl.esdq.sop.knd$score.sop, tbl.esdq.sop.knd$SEX)

summary(tbl.esdq.sop.knd$score.sop)

ggplot( 
  tbl.esdq.sop.knd,
  aes( score.sop, fill = SEX ) ) +
  geom_bar( binwidth = 1 ) +
  facet_grid( . ~ SEX ) +
  theme_bw( ) +
  scale_fill_brewer( type = "qual", palette = 6, direction = -1 ) +
  labs( title = "Kinder E-SDQ Probleme mit Gleichaltrigen")


tbl.17.sop.knd <- tbl.esdq.ges.knd [tbl.esdq.ges.knd$score.ges >= 17 &
                                      tbl.esdq.sop.knd$score.sop >=4,]

table( tbl.17.sop.knd$score.ges, tbl.17.sop.knd$SEX)

summary(tbl.17.sop.knd$score.ges)

ggplot( 
  tbl.17.sop.knd,
  aes( score.ges, fill = SEX ) ) +
  geom_bar( binwidth = 1 ) +
  facet_grid( . ~ SEX ) +
  theme_bw( ) +
  scale_fill_brewer( type = "qual", palette = 6, direction = -1 ) +
  labs( title = "Kinder E-SDQ-Gesamtscore >= 17 & E-SDQ Probleme Peers >= 4" )
