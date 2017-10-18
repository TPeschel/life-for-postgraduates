setwd("../Doktorarbeit/")
source("join.R")
library(dplyr)
library(lubridate)
library(ggplot2)
library(readxl)

##Wie viele Besuche nehmen an freiwilliger Sport-AG teil?
tbl.ag <- tbl[ !is.na(tbl$FB_FV_CH_SCHUL_F),]
table( tbl.ag$FB_FV_CH_SCHUL_F, tbl.ag$SEX)
summary(tbl.ag$FB_FV_CH_SCHUL_F)

##Was mache ich hier falsch? Würde gerne im Plot die 0 und 1 in ja und
#nein umbenennen...
tbl.ag$FB_FV_CH_SCHUL_F.jn <- factor(tbl.ag$FB_FV_CH_SCHUL_F, levels = c(0,1),
                                     labels = c("nein","ja"))


ggplot( 
  tbl.ag$FB_FV_CH_SCHUL_F.jn,
  aes( FB_FV_CH_SCHUL_F.jn, fill = SEX ) ) +
  geom_bar( binwidth = 1 ) +
  facet_grid( . ~ SEX ) +
  theme_bw( ) +
  scale_fill_brewer( type = "qual", palette = 6, direction = -1 ) +
  labs( title = "Besuche Freiwillige Sport-AG" )


##Wie viele Besuche machen Sport im Verein?
tbl.sp.verein.ch <- tbl[!is.na(tbl$FB_FV_CH_F0010_A),]
table(tbl.sp.verein.ch$FB_FV_CH_F0010_A, tbl.sp.verein.ch$SEX)
summary(tbl.sp.verein.ch$FB_FV_CH_F0010_A)
ggplot( 
  tbl.sp.verein.ch,
  aes( FB_FV_CH_F0010_A, fill = SEX ) ) +
  geom_bar( binwidth = 1 ) +
  facet_grid( . ~ SEX ) +
  theme_bw( ) +
  scale_fill_brewer( type = "qual", palette = 6, direction = -1 ) +
  labs( title = "Besuche Sport im Verein" )


##Wie viele Besuche machen mindestens 1x pro Woche Sport im Verein?
tbl.sp.verein.ch.2 <- tbl.sp.verein.ch[tbl.sp.verein.ch$FB_FV_CH_F0010_A >= 2,]
table(tbl.sp.verein.ch.2$FB_FV_CH_F0010_A, tbl.sp.verein.ch.2$SEX)
summary(tbl.sp.verein.ch.2$FB_FV_CH_F0010_A)
ggplot( 
  tbl.sp.verein.ch.2,
  aes( FB_FV_CH_F0010_A, fill = SEX ) ) +
  geom_bar( binwidth = 1 ) +
  facet_grid( . ~ SEX ) +
  theme_bw( ) +
  scale_fill_brewer( type = "qual", palette = 6, direction = -1 ) +
  labs( title = "Besuche Sport im Verein >=2" )


##Wie viele Besuche machen Sport außerhalb von Verein?
tbl.sp.kverein.ch <- tbl[!is.na(tbl$FB_FV_CH_F0010_B),]
table(tbl.sp.kverein.ch$FB_FV_CH_F0010_B, tbl.sp.verein.ch$SEX)
summary(tbl.sp.kverein.ch$FB_FV_CH_F0010_B)
ggplot( 
  tbl.sp.kverein.ch,
  aes( FB_FV_CH_F0010_B, fill = SEX ) ) +
  geom_bar( binwidth = 1 ) +
  facet_grid( . ~ SEX ) +
  theme_bw( ) +
  scale_fill_brewer( type = "qual", palette = 6, direction = -1 ) +
  labs( title = "Besuche Sport ohne Verein" )


##Wie viele Besuche machen mindestens 1x pro Woche Sport außerhalb 
#vom Verein?
tbl.sp.kverein.ch.2 <- tbl.sp.kverein.ch[tbl.sp.kverein.ch$FB_FV_CH_F0010_B >=2,]
table(tbl.sp.kverein.ch.2$FB_FV_CH_F0010_B, tbl.sp.kverein.ch.2$SEX)
summary(tbl.sp.kverein.ch.2$FB_FV_CH_F0010_B)
ggplot( 
  tbl.sp.kverein.ch.2,
  aes( FB_FV_CH_F0010_B, fill = SEX ) ) +
  geom_bar( binwidth = 1 ) +
  facet_grid( . ~ SEX ) +
  theme_bw( ) +
  scale_fill_brewer( type = "qual", palette = 6, direction = -1 ) +
  labs( title = "Besuche Sport ohne Verein >=2" )

##Wie viele Besuche spielen im Freien?
tbl.spiel.frei.ch <- tbl[!is.na(tbl$FB_FV_CH_F0009),]
table(tbl.spiel.frei.ch$FB_FV_CH_F0009, tbl.spiel.frei.ch$SEX)
summary(tbl.spiel.frei.ch$FB_FV_CH_F0009)
ggplot( 
  tbl.spiel.frei.ch,
  aes( FB_FV_CH_F0009, fill = SEX ) ) +
  geom_bar( binwidth = 1 ) +
  facet_grid( . ~ SEX ) +
  theme_bw( ) +
  scale_fill_brewer( type = "qual", palette = 6, direction = -1 ) +
  labs( title = "Besuche Spielen im Freien" )

##Wie viele Besuche spielen mindestens 1x pro Woche im Freien?
tbl.spiel.frei.ch.2 <- tbl.spiel.frei.ch[tbl.spiel.frei.ch$FB_FV_CH_F0009>=2,]
table(tbl.spiel.frei.ch.2$FB_FV_CH_F0009, tbl.spiel.frei.ch.2$SEX)
summary(tbl.spiel.frei.ch.2$FB_FV_CH_F0009)
ggplot( 
  tbl.spiel.frei.ch.2,
  aes( FB_FV_CH_F0009, fill = SEX ) ) +
  geom_bar( binwidth = 1 ) +
  facet_grid( . ~ SEX ) +
  theme_bw( ) +
  scale_fill_brewer( type = "qual", palette = 6, direction = -1 ) +
  labs( title = "Besuche Spielen im Freien >=2" )

##Wie viele Kinder nehmen an freiwilliger Sport AG teil?
tbl.ag <- tbl[ !is.na(tbl$FB_FV_CH_SCHUL_F),]
tbl.ag.knd <- tbl.ag %>% group_by(SIC, SEX) %>%
  summarise( score.ges = max(FB_FV_CH_SCHUL_F))

table(tbl.ag.knd$score.ges, tbl.ag.knd$SEX)

summary(tbl.ag.knd$score.ges)

ggplot( 
  tbl.ag.knd,
  aes( score.ges, fill = SEX ) ) +
  geom_bar( binwidth = 1 ) +
  facet_grid( . ~ SEX ) +
  theme_bw( ) +
  scale_fill_brewer( type = "qual", palette = 6, direction = -1 ) +
  labs( title = "Kinder Sport-AG" )

##Wie viele Kinder machen Sport im Verein?

tbl.sp.verein.ch.knd <- tbl.sp.verein.ch %>% group_by(SIC, SEX) %>%
                  summarise( score.ges = mean(FB_FV_CH_F0010_A))

table(tbl.sp.verein.ch.knd$score.ges, tbl.sp.verein.ch.knd$SEX)

summary(tbl.sp.verein.ch.knd$score.ges)

ggplot( 
  tbl.sp.verein.ch.knd,
  aes( score.ges, fill = SEX ) ) +
  geom_bar( binwidth = 1 ) +
  facet_grid( . ~ SEX ) +
  theme_bw( ) +
  scale_fill_brewer( type = "qual", palette = 6, direction = -1 ) +
  labs( title = "Kinder Sport im Verein" )

##Wie viele Kinder machen mind. 1x pro Woche Sport im Verein?

tbl.sp.verein.ch.2.knd <- tbl.sp.verein.ch.knd[tbl.sp.verein.ch.knd$score.ges 
                                               >=2,]
table(tbl.sp.verein.ch.2.knd$score.ges, tbl.sp.verein.ch.2.knd$SEX)

summary(tbl.sp.verein.ch.2.knd$score.ges)

ggplot( 
  tbl.sp.verein.ch.2.knd,
  aes( score.ges, fill = SEX ) ) +
  geom_bar( binwidth = 1 ) +
  facet_grid( . ~ SEX ) +
  theme_bw( ) +
  scale_fill_brewer( type = "qual", palette = 6, direction = -1 ) +
  labs( title = "Kinder Sport im Verein >=2" )

##Wie viele Kinder machen Sport außerhalb vom Verein?

tbl.sp.kverein.ch.knd <- tbl.sp.kverein.ch %>% group_by(SIC, SEX) %>%
  summarise( score.ges = mean(FB_FV_CH_F0010_B))

table(tbl.sp.kverein.ch.knd$score.ges, tbl.sp.kverein.ch.knd$SEX)

summary(tbl.sp.kverein.ch.knd$score.ges)

ggplot( 
  tbl.sp.kverein.ch.knd,
  aes( score.ges, fill = SEX ) ) +
  geom_bar( binwidth = 1 ) +
  facet_grid( . ~ SEX ) +
  theme_bw( ) +
  scale_fill_brewer( type = "qual", palette = 6, direction = -1 ) +
  labs( title = "Kinder Sport ohne Verein" )

##Wie viele Kinder machen mind. 1x pro Woche Sport außerhalb vom Verein?

tbl.sp.kverein.ch.2.knd <- tbl.sp.kverein.ch.knd[tbl.sp.kverein.ch.knd$score.ges
                                                 >=2,]

table(tbl.sp.kverein.ch.2.knd$score.ges, tbl.sp.kverein.ch.2.knd$SEX)

summary(tbl.sp.kverein.ch.2.knd$score.ges)

ggplot( 
  tbl.sp.kverein.ch.2.knd,
  aes( score.ges, fill = SEX ) ) +
  geom_bar( binwidth = 1 ) +
  facet_grid( . ~ SEX ) +
  theme_bw( ) +
  scale_fill_brewer( type = "qual", palette = 6, direction = -1 ) +
  labs( title = "Kinder Sport ohne Verein >=2" )

##Wie viele Kinder spielen im Freien?

tbl.spiel.frei.ch.knd <- tbl.spiel.frei.ch %>% group_by(SIC, SEX) %>%
  summarise( score.ges = mean(FB_FV_CH_F0009))

table(tbl.spiel.frei.ch.knd$score.ges, tbl.spiel.frei.ch.knd$SEX)

summary(tbl.spiel.frei.ch.knd$score.ges)

ggplot( 
  tbl.spiel.frei.ch.knd,
  aes( score.ges, fill = SEX ) ) +
  geom_bar( binwidth = 1 ) +
  facet_grid( . ~ SEX ) +
  theme_bw( ) +
  scale_fill_brewer( type = "qual", palette = 6, direction = -1 ) +
  labs( title = "Kinder Spielen im Freien" )

##Wie viele Kinder spielen mind. 1x pro Woche im Freien?

tbl.spiel.frei.ch.2.knd <- tbl.spiel.frei.ch.knd[tbl.spiel.frei.ch.knd$
                                                   score.ges >=2,]

table(tbl.spiel.frei.ch.2.knd$score.ges, tbl.spiel.frei.ch.2.knd$SEX)

summary(tbl.spiel.frei.ch.2.knd$score.ges)

ggplot( 
  tbl.spiel.frei.ch.2.knd,
  aes( score.ges, fill = SEX ) ) +
  geom_bar( binwidth = 1 ) +
  facet_grid( . ~ SEX ) +
  theme_bw( ) +
  scale_fill_brewer( type = "qual", palette = 6, direction = -1 ) +
  labs( title = "Kinder Spielen im Freien >=2" )
