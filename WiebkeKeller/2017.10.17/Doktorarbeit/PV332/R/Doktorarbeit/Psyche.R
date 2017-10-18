setwd("../Doktorarbeit/")

source( "join.R" )

library( dplyr )
library( ggplot2 )
library( readxl )
library( lubridate )

##Wie viele Besuche haben ADHS?

tbl.adhs <- tbl[!is.na(tbl$C_DISEASE_TX_ADHS),]

table(tbl.adhs$C_DISEASE_TX_ADHS, tbl.adhs$SEX)

summary(tbl.adhs$C_DISEASE_TX_ADHS)

ggplot( 
  tbl.adhs,
  aes( C_DISEASE_TX_ADHS, fill = SEX ) ) +
  geom_bar( binwidth = 1 ) +
  facet_grid( . ~ SEX ) +
  theme_bw( ) +
  scale_fill_brewer( type = "qual", palette = 6, direction = -1 ) +
  labs( title = "Besuche ADHS" )


##Wie viele Besuche haben Depression?

tbl.depres <- tbl[!is.na(tbl$C_DISEASE_TX_DEPRES),]

table(tbl.depres$C_DISEASE_TX_DEPRES, tbl.depres$SEX)

summary(tbl.depres$C_DISEASE_TX_DEPRES)

ggplot( 
  tbl.depres,
  aes( C_DISEASE_TX_DEPRES, fill = SEX ) ) +
  geom_bar( binwidth = 1 ) +
  facet_grid( . ~ SEX ) +
  theme_bw( ) +
  scale_fill_brewer( type = "qual", palette = 6, direction = -1 ) +
  labs( title = "Besuche Depression" )

##Kann man das vielleicht noch anders einstellen, damit man die Erkrankten
#auch im Plot erkennt?

##Wie viele Besuche haben psychische Erkrankungen?

tbl.psy <- tbl[!is.na(tbl$C_DISEASE_TX_PSY),]

table(tbl.psy$C_DISEASE_TX_PSY, tbl.psy$SEX)

summary(tbl.psy$C_DISEASE_TX_PSY)

ggplot( 
  tbl.psy,
  aes( C_DISEASE_TX_PSY, fill = SEX ) ) +
  geom_bar( binwidth = 1 ) +
  facet_grid( . ~ SEX ) +
  theme_bw( ) +
  scale_fill_brewer( type = "qual", palette = 6, direction = -1 ) +
  labs( title = "Besuche Psychische Erkrankung" )

##Wie viele Kinder haben ADHS?

tbl.adhs.knd <- tbl.adhs %>%
  group_by( SIC, SEX ) %>%
  summarise( score.adhs = max( C_DISEASE_TX_ADHS ) )

table(tbl.adhs.knd$score.adhs, tbl.adhs.knd$SEX)

summary(tbl.adhs.knd$score.adhs)

ggplot( 
  tbl.adhs.knd,
  aes( score.adhs, fill = SEX ) ) +
  geom_bar( binwidth = 1 ) +
  facet_grid( . ~ SEX ) +
  theme_bw( ) +
  scale_fill_brewer( type = "qual", palette = 6, direction = -1 ) +
  labs( title = "Kinder ADHS" )

##Wie viele Kinder haben Depressionen?

tbl.depres.knd <- tbl.depres %>%
  group_by( SIC, SEX ) %>%
  summarise( score.depres = max( C_DISEASE_TX_DEPRES ) )

table(tbl.depres.knd$score.depres, tbl.depres.knd$SEX)

summary(tbl.depres.knd$score.depres)

ggplot( 
  tbl.depres.knd,
  aes( score.depres, fill = SEX ) ) +
  geom_bar( binwidth = 1 ) +
  facet_grid( . ~ SEX ) +
  theme_bw( ) +
  scale_fill_brewer( type = "qual", palette = 6, direction = -1 ) +
  labs( title = "Kinder Depression" )

##Wie viele Kinder haben psychische Erkrankungen?

tbl.psy.knd <- tbl.psy %>%
  group_by( SIC, SEX ) %>%
  summarise( score.psy = max( C_DISEASE_TX_PSY ) )

table(tbl.psy.knd$score.psy, tbl.psy.knd$SEX)

summary(tbl.psy.knd$score.psy)

ggplot( 
  tbl.psy.knd,
  aes( score.psy, fill = SEX ) ) +
  geom_bar( binwidth = 1 ) +
  facet_grid( . ~ SEX ) +
  theme_bw( ) +
  scale_fill_brewer( type = "qual", palette = 6, direction = -1 ) +
  labs( title = "Kinder Psychische Erkrankung" )

