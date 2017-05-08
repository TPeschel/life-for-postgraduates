setwd("../Doktorarbeit/")

source( "join.R" )
library( ggplot2 )
library(readxl)
library(lubridate)
library(dplyr)

## Bei wievielen Besuchen wurde eine laufende Nase diagnostiziert
tbl.nase.bp1 <- tbl[ !is.na(tbl$FB_ALLERGY_BP1_F0014),]

table( tbl.nase.bp1$FB_ALLERGY_BP1_F0014, tbl.nase.bp1$SEX)

summary(tbl.nase.bp1$FB_ALLERGY_BP1_F0014)

ggplot( 
  tbl.nase.bp1,
  aes( FB_ALLERGY_BP1_F0014, fill = SEX ) ) +
  geom_bar( binwidth = 1 ) +
  facet_grid( . ~ SEX ) +
  theme_bw( ) +
  scale_fill_brewer( type = "qual", palette = 6, direction = -1 ) +
  labs( title = "Besuche Laufende Nase" )

## Bei wievielen Besuchen wurde Heuschnupfen diagnostiziert
tbl.hs.bp1 <- tbl[ !is.na(tbl$FB_ALLERGY_BP1_F0029),]

table( tbl.hs.bp1$FB_ALLERGY_BP1_F0029, tbl.hs.bp1$SEX)

summary(tbl.hs.bp1$FB_ALLERGY_BP1_F0029)

ggplot( 
  tbl.hs.bp1,
  aes( FB_ALLERGY_BP1_F0029, fill = SEX ) ) +
  geom_bar( binwidth = 1 ) +
  facet_grid( . ~ SEX ) +
  theme_bw( ) +
  scale_fill_brewer( type = "qual", palette = 6, direction = -1 ) +
  labs( title = "Besuche Heuschnupfen" )

## Bei wievielen Besuchen wurden Atembeschwerden diagnostiziert
tbl.ab.bp1 <- tbl[ !is.na(tbl$FB_ALLERGY_BP1_F0041),]

table( tbl.ab.bp1$FB_ALLERGY_BP1_F0041, tbl.ab.bp1$SEX)

summary(tbl.ab.bp1$FB_ALLERGY_BP1_F0041)

ggplot( 
  tbl.ab.bp1,
  aes( FB_ALLERGY_BP1_F0041, fill = SEX ) ) +
  geom_bar( binwidth = 1 ) +
  facet_grid( . ~ SEX ) +
  theme_bw( ) +
  scale_fill_brewer( type = "qual", palette = 6, direction = -1 ) +
  labs( title = "Besuche Atembeschwerden" )


## Bei wievielen Besuchen wurde Asthma diagnostiziert?

tbl.asthma.bp1 <- tbl[ !is.na(tbl$FB_ALLERGY_BP1_F0043),]

table( tbl.asthma.bp1$FB_ALLERGY_BP1_F0043, tbl.asthma.bp1$SEX)

summary(tbl.asthma.bp1$FB_ALLERGY_BP1_F0043)

ggplot( 
  tbl.asthma.bp1,
  aes( FB_ALLERGY_BP1_F0043, fill = SEX ) ) +
  geom_bar( binwidth = 1 ) +
  facet_grid( . ~ SEX ) +
  theme_bw( ) +
  scale_fill_brewer( type = "qual", palette = 6, direction = -1 ) +
  labs( title = "Besuche Asthma" )

##Bei wie vielen Kindern wurde eine laufende Nase diagnostiziert?

tbl.nase.bp1.knd <- tbl.nase.bp1 %>% group_by(SIC,SEX) %>% summarise(
  score.ges = mean(FB_ALLERGY_BP1_F0014))

table( tbl.nase.bp1.knd$score.ges, tbl.nase.bp1.knd$SEX)

summary(tbl.nase.bp1.knd$score.ges)

ggplot( 
  tbl.nase.bp1.knd,
  aes( score.ges, fill = SEX ) ) +
  geom_bar( binwidth = 1 ) +
  facet_grid( . ~ SEX ) +
  theme_bw( ) +
  scale_fill_brewer( type = "qual", palette = 6, direction = -1 ) +
  labs( title = "Kinder Laufende Nase" )

##Bei wie vielen Kindern wurde Heuschnupfen diagnostiziert?

tbl.hs.bp1.knd <- tbl.hs.bp1 %>% group_by(SIC,SEX) %>% summarise(
  score.ges = mean(FB_ALLERGY_BP1_F0029))

table( tbl.hs.bp1.knd$score.ges, tbl.hs.bp1.knd$SEX)

summary(tbl.hs.bp1.knd$score.ges)

ggplot( 
  tbl.hs.bp1.knd,
  aes( score.ges, fill = SEX ) ) +
  geom_bar( binwidth = 1 ) +
  facet_grid( . ~ SEX ) +
  theme_bw( ) +
  scale_fill_brewer( type = "qual", palette = 6, direction = -1 ) +
  labs( title = "Kinder Heuschnupfen" )

##Bei wie vielen Kindern wurden Atembeschwerden diagnostiziert?

tbl.ab.bp1.knd <- tbl.ab.bp1 %>% group_by(SIC,SEX) %>% summarise(
  score.ges = mean(FB_ALLERGY_BP1_F0041))

table( tbl.ab.bp1.knd$score.ges, tbl.ab.bp1.knd$SEX)

summary(tbl.ab.bp1.knd$score.ges)

ggplot( 
  tbl.ab.bp1.knd,
  aes( score.ges, fill = SEX ) ) +
  geom_bar( binwidth = 1 ) +
  facet_grid( . ~ SEX ) +
  theme_bw( ) +
  scale_fill_brewer( type = "qual", palette = 6, direction = -1 ) +
  labs( title = "Kinder Atembeschwerden" )

##Bei wie vielen Kindern wurde Asthma diagnostiziert?

tbl.asthma.bp1.knd <- tbl.asthma.bp1 %>% group_by(SIC,SEX) %>% summarise(
  score.ges = mean(FB_ALLERGY_BP1_F0043))

table( tbl.asthma.bp1.knd$score.ges, tbl.asthma.bp1.knd$SEX)

summary(tbl.asthma.bp1.knd$score.ges)

ggplot( 
  tbl.asthma.bp1.knd,
  aes( score.ges, fill = SEX ) ) +
  geom_bar( binwidth = 1 ) +
  facet_grid( . ~ SEX ) +
  theme_bw( ) +
  scale_fill_brewer( type = "qual", palette = 6, direction = -1 ) +
  labs( title = "Kinder Asthma" )
