setwd("../Doktorarbeit/")
source("join.R")
library(dplyr)
library(lubridate)
library(ggplot2)
library(readxl)

##Wie viele Besuche machen Sport im Verein?
tbl.sp.verein.bp1 <- tbl[ !is.na(tbl$FB_FV_BP1_SP_VEREIN_K),]
table(tbl.sp.verein.bp1$FB_FV_BP1_SP_VEREIN_K, tbl.sp.verein.bp1$sex)
summary(tbl.sp.verein.bp1$FB_FV_BP1_SP_VEREIN_K)
ggplot( 
  tbl.sp.verein.bp1,
  aes( FB_FV_BP1_SP_VEREIN_K, fill = sex ) ) +
  geom_bar( binwidth = 1 ) +
  facet_grid( . ~ sex ) +
  theme_bw( ) +
  scale_fill_brewer( type = "qual", palette = 6, direction = -1 ) +
  labs( title = "Besuche Sport im Verein" )

##Wie viele Besuche machen mindestens 1x pro Woche Sport im Verein?
tbl.sp.verein.bp1.2 <- tbl.sp.verein.bp1[tbl.sp.verein.bp1$FB_FV_BP1_SP_VEREIN_K >=2,]
addmargins(table(tbl.sp.verein.bp1.2$FB_FV_BP1_SP_VEREIN_K, tbl.sp.verein.bp1.2$sex))
summary(tbl.sp.verein.bp1.2$FB_FV_BP1_SP_VEREIN_K)
ggplot( 
  tbl.sp.verein.bp1.2,
  aes( FB_FV_BP1_SP_VEREIN_K, fill = SEX ) ) +
  geom_bar( binwidth = 1 ) +
  facet_grid( . ~ SEX ) +
  theme_bw( ) +
  scale_fill_brewer( type = "qual", palette = 6, direction = -1 ) +
  labs( title = "Besuche Sport im Verein >=2" )

##Wie viele Besuche machen Sport außerhalb von Verein?
tbl.sp.kverein.bp1 <- tbl[ !is.na(tbl$FB_FV_BP1_SPORT_FREIZ_K),]
table(tbl.sp.kverein.bp1$FB_FV_BP1_SPORT_FREIZ_K, tbl.sp.kverein.bp1$SEX)
summary(tbl.sp.kverein.bp1$FB_FV_BP1_SPORT_FREIZ_K)
ggplot( 
  tbl.sp.kverein.bp1,
  aes( FB_FV_BP1_SPORT_FREIZ_K, fill = SEX ) ) +
  geom_bar( binwidth = 1 ) +
  facet_grid( . ~ SEX ) +
  theme_bw( ) +
  scale_fill_brewer( type = "qual", palette = 6, direction = -1 ) +
  labs( title = "Besuche Sport ohne Verein" )


##Wie viele Besuche machen mindestens 1x pro Woche Sport außerhalb 
#vom Verein?
tbl.sp.kverein.bp1.2 <- tbl.sp.kverein.bp1[tbl.sp.kverein.bp1$FB_FV_BP1_SPORT_FREIZ_K >= 2,]
table(tbl.sp.kverein.bp1.2$FB_FV_BP1_SPORT_FREIZ_K, tbl.sp.kverein.bp1.2$SEX)
summary(tbl.sp.kverein.bp1.2$FB_FV_BP1_SPORT_FREIZ_K)
ggplot( 
  tbl.sp.kverein.bp1.2,
  aes( FB_FV_BP1_SPORT_FREIZ_K, fill = SEX ) ) +
  geom_bar( binwidth = 1 ) +
  facet_grid( . ~ SEX ) +
  theme_bw( ) +
  scale_fill_brewer( type = "qual", palette = 6, direction = -1 ) +
  labs( title = "Besuche Sport ohne Verein >=2" )

##Wie viele Besuche spielen im Freien?
tbl.spiel.frei.bp1 <- tbl[ !is.na(tbl$FB_FV_BP1_SPIEL_FREI_K),]
table(tbl.spiel.frei.bp1$FB_FV_BP1_SPIEL_FREI_K, tbl.spiel.frei.bp1$SEX)
summary(tbl.spiel.frei.bp1$FB_FV_BP1_SPIEL_FREI_K)
ggplot( 
  tbl.spiel.frei.bp1,
  aes( FB_FV_BP1_SPIEL_FREI_K, fill = SEX ) ) +
  geom_bar( binwidth = 1 ) +
  facet_grid( . ~ SEX ) +
  theme_bw( ) +
  scale_fill_brewer( type = "qual", palette = 6, direction = -1 ) +
  labs( title = "Besuche Spielen im Freien" )


##Wie viele Besuche spielen mindestens 1x pro Woche im Freien?
tbl.spiel.frei.bp1.2 <- tbl.spiel.frei.bp1[tbl.spiel.frei.bp1$FB_FV_BP1_SPIEL_FREI_K >=2,]
table(tbl.spiel.frei.bp1.2$FB_FV_BP1_SPIEL_FREI_K, tbl.spiel.frei.bp1.2$SEX)
summary(tbl.spiel.frei.bp1.2$FB_FV_BP1_SPIEL_FREI_K)
ggplot( 
  tbl.spiel.frei.bp1.2,
  aes( FB_FV_BP1_SPIEL_FREI_K, fill = SEX ) ) +
  geom_bar( binwidth = 1 ) +
  facet_grid( . ~ SEX ) +
  theme_bw( ) +
  scale_fill_brewer( type = "qual", palette = 6, direction = -1 ) +
  labs( title = "Besuche Spielen im Freien >=2" )

##Wie viele Kinder machen Sport im Verein?
tbl.sp.verein.bp1.knd <- tbl.sp.verein.bp1 %>% group_by(SIC,SEX) %>% summarise(
  score.ges = mean(FB_FV_BP1_SP_VEREIN_K))

table(tbl.sp.verein.bp1.knd$score.ges, tbl.sp.verein.bp1.knd$SEX)

summary(tbl.sp.verein.bp1.knd$score.ges)

ggplot( 
  tbl.sp.verein.bp1.knd,
  aes( score.ges, fill = SEX ) ) +
  geom_bar( binwidth = 1 ) +
  facet_grid( . ~ SEX ) +
  theme_bw( ) +
  scale_fill_brewer( type = "qual", palette = 6, direction = -1 ) +
  labs( title = "Kinder Sport im Verein" )

##Wie viele Kinder machen mindestens 1x pro Woche Sport im Verein?
tbl.sp.verein.bp1.2.knd <- tbl.sp.verein.bp1.knd[tbl.sp.verein.bp1.knd$
                                                   score.ges >=2,]
    
table(tbl.sp.verein.bp1.2.knd$score.ges, tbl.sp.verein.bp1.2.knd$SEX)

summary(tbl.sp.verein.bp1.2.knd$score.ges)

ggplot( 
  tbl.sp.verein.bp1.2.knd,
  aes( score.ges, fill = SEX ) ) +
  geom_bar( binwidth = 1 ) +
  facet_grid( . ~ SEX ) +
  theme_bw( ) +
  scale_fill_brewer( type = "qual", palette = 6, direction = -1 ) +
  labs( title = "Kinder Sport im Verein >= 2" )


##Wie viele Kinder machen außerhalb von einem Verein Sport?
tbl.sp.kverein.bp1.knd <- tbl.sp.kverein.bp1 %>% group_by(SIC,SEX) %>% summarise(
  score.ges = mean(FB_FV_BP1_SPORT_FREIZ_K))

table(tbl.sp.kverein.bp1.knd$score.ges, tbl.sp.kverein.bp1.knd$SEX)

summary(tbl.sp.kverein.bp1.knd$score.ges)

ggplot( 
  tbl.sp.kverein.bp1.knd,
  aes( score.ges, fill = SEX ) ) +
  geom_bar( binwidth = 1 ) +
  facet_grid( . ~ SEX ) +
  theme_bw( ) +
  scale_fill_brewer( type = "qual", palette = 6, direction = -1 ) +
  labs( title = "Kinder Sport ohne Verein" )

##Wie viele Kinder machen mindestens 1x pro Woche Sport außerhalb vom Verein?
tbl.sp.kverein.bp1.2.knd <- tbl.sp.kverein.bp1.knd[tbl.sp.kverein.bp1.knd$
                                                   score.ges >=2,]

table(tbl.sp.kverein.bp1.2.knd$score.ges, tbl.sp.kverein.bp1.2.knd$SEX)

summary(tbl.sp.kverein.bp1.2.knd$score.ges)

ggplot( 
  tbl.sp.kverein.bp1.2.knd,
  aes( score.ges, fill = SEX ) ) +
  geom_bar( binwidth = 1 ) +
  facet_grid( . ~ SEX ) +
  theme_bw( ) +
  scale_fill_brewer( type = "qual", palette = 6, direction = -1 ) +
  labs( title = "Kinder Sport ohne Verein >= 2" )

##Wie viele Kinder spielen im Freien?
tbl.spiel.frei.bp1.knd <- tbl.spiel.frei.bp1 %>% group_by(SIC,SEX) %>% summarise(
  score.ges = mean(FB_FV_BP1_SPIEL_FREI_K))

table(tbl.spiel.frei.bp1.knd$score.ges, tbl.spiel.frei.bp1.knd$SEX)

summary(tbl.spiel.frei.bp1.knd$score.ges)

ggplot( 
  tbl.spiel.frei.bp1.knd,
  aes( score.ges, fill = SEX ) ) +
  geom_bar( binwidth = 1 ) +
  facet_grid( . ~ SEX ) +
  theme_bw( ) +
  scale_fill_brewer( type = "qual", palette = 6, direction = -1 ) +
  labs( title = "Kinder Spielen im Freien" )

##Wie viele Kinder spielen mind. 1x pro Woche im Freien?
tbl.spiel.frei.bp1.2.knd <- tbl.spiel.frei.bp1.knd[tbl.spiel.frei.bp1.knd$
                                                   score.ges >=2,]

table(tbl.spiel.frei.bp1.2.knd$score.ges, tbl.spiel.frei.bp1.2.knd$SEX)

summary(tbl.spiel.frei.bp1.2.knd$score.ges)

ggplot( 
  tbl.spiel.frei.bp1.2.knd,
  aes( score.ges, fill = SEX ) ) +
  geom_bar( binwidth = 1 ) +
  facet_grid( . ~ SEX ) +
  theme_bw( ) +
  scale_fill_brewer( type = "qual", palette = 6, direction = -1 ) +
  labs( title = "Kinder Spielen im Freien >= 2" )
