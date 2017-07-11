#setwd( "../R/Doktorarbeit/" )

setwd( "~/LIFE/life-for-postgraduates/WiebkeKeller/data/" )

#source( "join.R" )

library( dplyr )
library( ggplot2 )
library( readxl )
library( lubridate )

load( "join.Rd" )

tbl.psy <- tbl[ !is.na( tbl$C_DISEASE_TX_PSY ), ]

tbl.psy.knd <- tbl.psy %>%
  group_by( SIC, SEX ) %>%
  summarise( score.psy = max( C_DISEASE_TX_PSY ) )

table(tbl.psy.knd$score.psy, tbl.psy.knd$SEX)

summary(tbl.psy.knd$score.psy)

tbl.allerg <- tbl[!is.na(tbl$C_DISEASE_TX_ALLERG),]

tbl.allerg.knd <- tbl.allerg %>%
  group_by( SIC, SEX ) %>%
  summarise( score.allerg = max( C_DISEASE_TX_ALLERG ) )

table(tbl.allerg.knd$score.allerg, tbl.allerg.knd$SEX)

summary(tbl.allerg.knd$score.allerg)
summary(tbl.psy.knd)

Psyche <- tbl.psy.knd$score.psy

Allergie <- tbl.allerg.knd$score.allerg

a <- glm(Psyche ~ Allergie, family = binomial( "logit" ) )

names(tbl)









tbl.psy.allerg <- tbl[ !is.na( tbl$C_DISEASE_TX_PSY ) & !is.na( tbl$C_DISEASE_TX_ALLERG ), ]

tbl.psy.allerg %<>%
    group_by( SIC, SEX ) %>%
    summarise( 
        score.psy = max( C_DISEASE_TX_PSY ),
        score.allerg = max( C_DISEASE_TX_ALLERG ) )

glm.male   <- glm( data = tbl.psy.allerg[ SEX == "male", ], score.psy ~ score.allerg, family = binomial( "logit" ) )
glm.female <- glm( data = tbl.psy.allerg[ SEX == "female", ], score.psy ~ score.allerg, family = binomial( "logit" ) )
