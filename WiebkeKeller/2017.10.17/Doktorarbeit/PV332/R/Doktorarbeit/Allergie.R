setwd("../Doktorarbeit/")

source( "join.R" )

library( dplyr )
library( ggplot2 )
library( readxl )
library( lubridate )

##Wie viele Besuche haben Asthma?

tbl.asthma <- tbl[!is.na(tbl$C_DISEASE_TX_ASTHMA),]

table(tbl.asthma$C_DISEASE_TX_ASTHMA, tbl.asthma$SEX)

summary(tbl.asthma$C_DISEASE_TX_ASTHMA)

ggplot( 
  tbl.asthma,
  aes( C_DISEASE_TX_ASTHMA, fill = SEX ) ) +
  geom_bar( binwidth = 1 ) +
  facet_grid( . ~ SEX ) +
  theme_bw( ) +
  scale_fill_brewer( type = "qual", palette = 6, direction = -1 ) +
  labs( title = "Besuche Asthma" )

##Wie viele Besuche haben Allergien?

tbl.allerg <- tbl[!is.na(tbl$C_DISEASE_TX_ALLERG),]

table(tbl.allerg$C_DISEASE_TX_ALLERG, tbl.allerg$SEX)

summary(tbl.allerg$C_DISEASE_TX_ALLERG)

ggplot( 
  tbl.allerg,
  aes( C_DISEASE_TX_ALLERG, fill = SEX ) ) +
  geom_bar( binwidth = 1 ) +
  facet_grid( . ~ SEX ) +
  theme_bw( ) +
  scale_fill_brewer( type = "qual", palette = 6, direction = -1 ) +
  labs( title = "Besuche Allergie" )

##Wie viele Besuche haben Neurodermitis?

tbl.neuroder <- tbl[!is.na(tbl$C_DISEASE_TX_NEURODER),]

table(tbl.neuroder$C_DISEASE_TX_NEURODER, tbl.neuroder$SEX)

summary(tbl.neuroder$C_DISEASE_TX_NEURODER)

ggplot( 
  tbl.neuroder,
  aes( C_DISEASE_TX_NEURODER, fill = SEX ) ) +
  geom_bar( binwidth = 1 ) +
  facet_grid( . ~ SEX ) +
  theme_bw( ) +
  scale_fill_brewer( type = "qual", palette = 6, direction = -1 ) +
  labs( title = "Besuche Neurodermitis" )
 
##Wie viele Kinder haben Asthma?

tbl.asthma.knd <- tbl.asthma %>%
  group_by( SIC, SEX ) %>%
  summarise( score.asthma = max( C_DISEASE_TX_ASTHMA ) )

table(tbl.asthma.knd$score.asthma, tbl.asthma.knd$SEX)

summary(tbl.asthma.knd$score.asthma)

ggplot( 
  tbl.asthma.knd,
  aes( score.asthma, fill = SEX ) ) +
  geom_bar( binwidth = 1 ) +
  facet_grid( . ~ SEX ) +
  theme_bw( ) +
  scale_fill_brewer( type = "qual", palette = 6, direction = -1 ) +
  labs( title = "Kinder Asthma" )

##Wie viele Kinder haben Allergien?

tbl.allerg.knd <- tbl.allerg %>%
  group_by( SIC, SEX ) %>%
  summarise( score.allerg = max( C_DISEASE_TX_ALLERG ) )

table(tbl.allerg.knd$score.allerg, tbl.allerg.knd$SEX)

summary(tbl.allerg.knd$score.allerg)

ggplot( 
  tbl.allerg.knd,
  aes( score.allerg, fill = SEX ) ) +
  geom_bar( binwidth = 1 ) +
  facet_grid( . ~ SEX ) +
  theme_bw( ) +
  scale_fill_brewer( type = "qual", palette = 6, direction = -1 ) +
  labs( title = "Kinder Allergie" )

##Wie viele Kinder haben Neurodermitis?

tbl.neuroder.knd <- tbl.neuroder %>%
  group_by( SIC, SEX ) %>%
  summarise( score.neuroder = max( C_DISEASE_TX_NEURODER ) )

table(tbl.neuroder.knd$score.neuroder, tbl.neuroder.knd$SEX)

summary(tbl.neuroder.knd$score.neuroder)

ggplot( 
  tbl.neuroder.knd,
  aes( score.neuroder, fill = SEX ) ) +
  geom_bar( binwidth = 1 ) +
  facet_grid( . ~ SEX ) +
  theme_bw( ) +
  scale_fill_brewer( type = "qual", palette = 6, direction = -1 ) +
  labs( title = "Kinder Neurodermitis" )
