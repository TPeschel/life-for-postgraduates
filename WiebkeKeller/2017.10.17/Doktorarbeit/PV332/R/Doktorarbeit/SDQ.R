setwd("../Doktorarbeit/")

source( "join.R" )
library( ggplot2 )
library( dplyr )
library( readxl )
library( lubridate )

##Gesamtscore der Besuche
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
    labs( title = "Besuche SDQ-Gesamtscore >= 20 & Hyperaktivität >= 7" )


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
    labs( title = "Besuche SDQ-Gesamtscore >= 20 & Emotionale Probleme >= 7" )


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
    labs( title = "Besuche SDQ-Gesamtscore >= 20 & Verhaltensauffälligkeiten >= 5" )

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
    labs( title = "Besuche SDQ-Gesamtscore >= 20 & Problemen mit Gleichaltrigen >= 6" )

##Gesamtscore der Kinder
tbl.sdq.ges.knd <- tbl.sdq.ges %>% group_by(SIC,SEX) %>% summarise(
  score.ges = mean(SDQ_GES_SCORE))

table(tbl.sdq.ges.knd$score.ges, tbl.sdq.ges.knd$SEX)

summary(tbl.sdq.ges.knd$score.ges)

ggplot( 
  tbl.sdq.ges.knd,
  aes( score.ges, fill = SEX ) ) +
  geom_bar( binwidth = 1 ) +
  facet_grid( . ~ SEX ) +
  theme_bw( ) +
  scale_fill_brewer( type = "qual", palette = 6, direction = -1 ) +
  labs( title = "Kinder SDQ-Gesamtscore" )

##Wie viele der Kinder haben einen auffälligen Gesamt-Score?
tbl.20.knd <- tbl.sdq.ges.knd [tbl.sdq.ges.knd$score.ges >=20,]

table(tbl.20.knd$score.ges, tbl.20.knd$SEX)

summary(tbl.20.knd$score.ges)

ggplot( 
  tbl.20.knd,
  aes( score.ges, fill = SEX ) ) +
  geom_bar( binwidth = 1 ) +
  facet_grid( . ~ SEX ) +
  theme_bw( ) +
  scale_fill_brewer( type = "qual", palette = 6, direction = -1 ) +
  labs( title = "Kinder SDQ-Gesamtscore >= 20" )

##Wie viele der Kinder mit auffälligen Gesamt-Score zeigen
#Hyperaktivität?

tbl.sdq.hyp <- tbl[ !is.na(tbl$SDQ_HYP_SUM),]

tbl.sdq.hyp.knd <- tbl.sdq.hyp %>% group_by(SIC,SEX) %>% summarise(
  score.hyp = mean(SDQ_HYP_SUM))

table(tbl.sdq.hyp.knd$score.hyp, tbl.sdq.hyp.knd$SEX)

summary(tbl.sdq.hyp.knd$score.hyp)

ggplot( 
  tbl.sdq.hyp.knd,
  aes( score.hyp, fill = SEX ) ) +
  geom_bar( binwidth = 1 ) +
  facet_grid( . ~ SEX ) +
  theme_bw( ) +
  scale_fill_brewer( type = "qual", palette = 6, direction = -1 ) +
  labs( title = "Kinder SDQ Hyperaktivität")


tbl.20.hyp.knd <- tbl.sdq.ges.knd [tbl.sdq.ges.knd$score.ges >= 20 &
                                      tbl.sdq.hyp.knd$score.hyp >=7,]

table( tbl.20.hyp.knd$score.ges, tbl.20.hyp.knd$SEX)

summary(tbl.20.hyp.knd$score.ges)

ggplot( 
  tbl.20.hyp.knd,
  aes( score.ges, fill = SEX ) ) +
  geom_bar( binwidth = 1 ) +
  facet_grid( . ~ SEX ) +
  theme_bw( ) +
  scale_fill_brewer( type = "qual", palette = 6, direction = -1 ) +
  labs( title = "Kinder SDQ-Gesamtscore >= 20 & SDQ Hyperaktivität >= 7" )

##Wie viele Kinder mit auffälligem Gesamt-Score zeigen emotionale 
#Probleme?

tbl.sdq.emo <- tbl[ !is.na(tbl$SDQ_EMO_SUM),]

tbl.sdq.emo.knd <- tbl.sdq.emo %>% group_by(SIC,SEX) %>% summarise(
  score.emo = mean(SDQ_EMO_SUM))

table(tbl.sdq.emo.knd$score.emo, tbl.sdq.emo.knd$SEX)

summary(tbl.sdq.emo.knd$score.emo)

ggplot( 
  tbl.sdq.emo.knd,
  aes( score.emo, fill = SEX ) ) +
  geom_bar( binwidth = 1 ) +
  facet_grid( . ~ SEX ) +
  theme_bw( ) +
  scale_fill_brewer( type = "qual", palette = 6, direction = -1 ) +
  labs( title = "Kinder SDQ Emotionale Probleme")


tbl.20.emo.knd <- tbl.sdq.ges.knd [tbl.sdq.ges.knd$score.ges >= 20 &
                                     tbl.sdq.emo.knd$score.emo >=7,]

table( tbl.20.emo.knd$score.ges, tbl.20.emo.knd$SEX)

summary(tbl.20.emo.knd$score.ges)

ggplot( 
  tbl.20.emo.knd,
  aes( score.ges, fill = SEX ) ) +
  geom_bar( binwidth = 1 ) +
  facet_grid( . ~ SEX ) +
  theme_bw( ) +
  scale_fill_brewer( type = "qual", palette = 6, direction = -1 ) +
  labs( title = "Kinder SDQ-Gesamtscore >= 20 & SDQ Emotionale Probleme >= 7" )

##Wie viele Kinder mit auffälligem Gesamt-Score  zeigen Verhaltens-
#auffälligkeiten?

tbl.sdq.ver <- tbl[ !is.na(tbl$SDQ_VER_SUM),]

tbl.sdq.ver.knd <- tbl.sdq.ver %>% group_by(SIC,SEX) %>% summarise(
  score.ver = mean(SDQ_VER_SUM))

table(tbl.sdq.ver.knd$score.ver, tbl.sdq.ver.knd$SEX)

summary(tbl.sdq.ver.knd$score.ver)

ggplot( 
  tbl.sdq.ver.knd,
  aes( score.ver, fill = SEX ) ) +
  geom_bar( binwidth = 1 ) +
  facet_grid( . ~ SEX ) +
  theme_bw( ) +
  scale_fill_brewer( type = "qual", palette = 6, direction = -1 ) +
  labs( title = "Kinder SDQ Verhaltensauffälligkeiten")


tbl.20.ver.knd <- tbl.sdq.ges.knd [tbl.sdq.ges.knd$score.ges >= 20 &
                                     tbl.sdq.ver.knd$score.ver >=5,]

table( tbl.20.ver.knd$score.ges, tbl.20.ver.knd$SEX)

summary(tbl.20.ver.knd$score.ges)

ggplot( 
  tbl.20.ver.knd,
  aes( score.ges, fill = SEX ) ) +
  geom_bar( binwidth = 1 ) +
  facet_grid( . ~ SEX ) +
  theme_bw( ) +
  scale_fill_brewer( type = "qual", palette = 6, direction = -1 ) +
  labs( title = "Kinder SDQ-Gesamtscore >= 20 & SDQ Verhaltensauffälligkeiten >= 5" )

##Wie viele der Kinder mit auffälligem Gesamt-Score zeigen Probleme
#mit Gleichaltrigen?

tbl.sdq.sop <- tbl[ !is.na(tbl$SDQ_SOP_SUM),]

tbl.sdq.sop.knd <- tbl.sdq.sop %>% group_by(SIC,SEX) %>% summarise(
  score.sop = mean(SDQ_SOP_SUM))

table(tbl.sdq.sop.knd$score.sop, tbl.sdq.sop.knd$SEX)

summary(tbl.sdq.sop.knd$score.sop)

ggplot( 
  tbl.sdq.sop.knd,
  aes( score.sop, fill = SEX ) ) +
  geom_bar( binwidth = 1 ) +
  facet_grid( . ~ SEX ) +
  theme_bw( ) +
  scale_fill_brewer( type = "qual", palette = 6, direction = -1 ) +
  labs( title = "Kinder SDQ Probleme mit Gleichaltrigen")


tbl.20.sop.knd <- tbl.sdq.ges.knd [tbl.sdq.ges.knd$score.ges >= 20 &
                                     tbl.sdq.sop.knd$score.sop >=6,]

table( tbl.20.sop.knd$score.ges, tbl.20.sop.knd$SEX)

summary(tbl.20.sop.knd$score.ges)

ggplot( 
  tbl.20.sop.knd,
  aes( score.ges, fill = SEX ) ) +
  geom_bar( binwidth = 1 ) +
  facet_grid( . ~ SEX ) +
  theme_bw( ) +
  scale_fill_brewer( type = "qual", palette = 6, direction = -1 ) +
  labs( title = "Kinder SDQ-Gesamtscore >= 20 & SDQ Probleme Peers >= 6" )
