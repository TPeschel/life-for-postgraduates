setwd("../Doktorarbeit/")

source( "join.R" )
library( ggplot2 )
library(readxl)
library(lubridate)
library(dplyr)

## Bei wievielen Besuchen wurde eine laufende Nase diagnostiziert?

tbl.nase.ch <- tbl[ !is.na(tbl$S010293_F0012),]

table( tbl.nase.ch$S010293_F0012, tbl.nase.ch$SEX)

summary(tbl.nase.ch$S010293_F0012)

ggplot( 
  tbl.nase.ch,
  aes( S010293_F0012, fill = SEX ) ) +
  geom_bar( binwidth = 1 ) +
  facet_grid( . ~ SEX ) +
  theme_bw( ) +
  scale_fill_brewer( type = "qual", palette = 6, direction = -1 ) +
  labs( title = "Besuche Laufende Nase" )

## Bei wievielen Besuchen wurde Heuschnupfen diagnostiziert?

tbl.hs.ch <- tbl[ !is.na(tbl$S010293_F0027),]

table( tbl.hs.ch$S010293_F0027, tbl.hs.ch$SEX)

summary(tbl.hs.ch$S010293_F0027)

ggplot( 
  tbl.hs.ch,
  aes( S010293_F0027, fill = SEX ) ) +
  geom_bar( binwidth = 1 ) +
  facet_grid( . ~ SEX ) +
  theme_bw( ) +
  scale_fill_brewer( type = "qual", palette = 6, direction = -1 ) +
  labs( title = "Besuche Heuschnupfen" )

## Bei wievielen Besuchen wurden Atembeschwerden diagnostiziert?

tbl.ab.ch <- tbl[ !is.na(tbl$S010293_F0039),]

table( tbl.ab.ch$S010293_F0039, tbl.ab.ch$SEX)

summary(tbl.ab.ch$S010293_F0039)

ggplot( 
  tbl.ab.ch,
  aes( S010293_F0039, fill = SEX ) ) +
  geom_bar( binwidth = 1 ) +
  facet_grid( . ~ SEX ) +
  theme_bw( ) +
  scale_fill_brewer( type = "qual", palette = 6, direction = -1 ) +
  labs( title = "Besuche Atembeschwerden" )

## Bei wievielen Besuchen wurde Asthma diagnostiziert?

tbl.asthma.ch <- tbl[ !is.na(tbl$S010293_F0041),]

table( tbl.asthma.ch$S010293_F0041, tbl.asthma.ch$SEX)

summary(tbl.asthma.ch$S010293_F0041)

ggplot( 
  tbl.asthma.ch,
  aes( S010293_F0041, fill = SEX ) ) +
  geom_bar( binwidth = 1 ) +
  facet_grid( . ~ SEX ) +
  theme_bw( ) +
  scale_fill_brewer( type = "qual", palette = 6, direction = -1 ) +
  labs( title = "Besuche Asthma" )

##Bei wie vielen Kindern wurde eine laufende Nase diagnostiziert?

tbl.nase.ch.knd <- tbl.nase.ch %>% group_by(SIC,SEX) %>% summarise(
  score.ges = mean(S010293_F0012))

table( tbl.nase.ch.knd$score.ges, tbl.nase.ch.knd$SEX)

summary(tbl.nase.ch.knd$score.ges)

ggplot( 
  tbl.nase.ch.knd,
  aes( score.ges, fill = SEX ) ) +
  geom_bar( binwidth = 1 ) +
  facet_grid( . ~ SEX ) +
  theme_bw( ) +
  scale_fill_brewer( type = "qual", palette = 6, direction = -1 ) +
  labs( title = "Kinder Laufende Nase" )

##Bei wie vielen Kindern wurde Heuschnupfen diagnostiziert?

tbl.hs.ch.knd <- tbl.hs.ch %>% group_by(SIC,SEX) %>% summarise(
  score.ges = mean(S010293_F0027))

table( tbl.hs.ch.knd$score.ges, tbl.hs.ch.knd$SEX)

summary(tbl.hs.ch.knd$score.ges)

ggplot( 
  tbl.hs.ch.knd,
  aes( score.ges, fill = SEX ) ) +
  geom_bar( binwidth = 1 ) +
  facet_grid( . ~ SEX ) +
  theme_bw( ) +
  scale_fill_brewer( type = "qual", palette = 6, direction = -1 ) +
  labs( title = "Kinder Heuschnupfen" )

##Bei wie vielen Kindern wurden Atembeschwerden diagnostiziert?

tbl.ab.ch.knd <- tbl.ab.ch %>% group_by(SIC,SEX) %>% summarise(
  score.ges = mean(S010293_F0039))

table( tbl.ab.ch.knd$score.ges, tbl.ab.ch.knd$SEX)

summary(tbl.ab.ch.knd$score.ges)

ggplot( 
  tbl.ab.ch.knd,
  aes( score.ges, fill = SEX ) ) +
  geom_bar( binwidth = 1 ) +
  facet_grid( . ~ SEX ) +
  theme_bw( ) +
  scale_fill_brewer( type = "qual", palette = 6, direction = -1 ) +
  labs( title = "Kinder Atembeschwerden" )

##Bei wie vielen Kindern wurde Asthma diagnostiziert?

tbl.asthma.ch.knd <- tbl.asthma.ch %>% group_by(SIC,SEX) %>% summarise(
  score.ges = mean(S010293_F0041))

table( tbl.asthma.ch.knd$score.ges, tbl.asthma.ch.knd$SEX)

summary(tbl.asthma.ch.knd$score.ges)

ggplot( 
  tbl.asthma.ch.knd,
  aes( score.ges, fill = SEX ) ) +
  geom_bar( binwidth = 1 ) +
  facet_grid( . ~ SEX ) +
  theme_bw( ) +
  scale_fill_brewer( type = "qual", palette = 6, direction = -1 ) +
  labs( title = "Kinder Asthma" )
