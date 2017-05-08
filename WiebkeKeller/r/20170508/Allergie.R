setwd("../../data/")

library( dplyr )
library( ggplot2 )
library( readxl )
library( lubridate )

my.theme <-
  theme_bw( )

tbl.all <- read_excel("PV0332_Gesamt_Join.xlsx")

tbl.disease <- read_excel("PV0332_D00127_NODUP.xlsx")

tbl.all$JAHR <-
  year( tbl.all$E_SDQ_EDAT )

tbl <- merge (
  tbl.all,
  tbl.disease,
  by.x = c("TEILNEHMER_SIC"),
  by.y = c("C_DISEASE_TX_SIC" ))


tbl$sex <- factor(tbl$TEILNEHMER_GESCHLECHT, levels = c(1,2),
                  labels = c("male","female"))

##Wie viele Besuche haben Asthma?

tbl.asthma <- tbl[!is.na(tbl$C_DISEASE_TX_ASTHMA),]

table(tbl.asthma$C_DISEASE_TX_ASTHMA, tbl.asthma$sex)

summary(tbl.asthma$C_DISEASE_TX_ASTHMA)

ggplot( 
  tbl.asthma, 
  aes( C_DISEASE_TX_ASTHMA, fill = sex ) ) + 
  geom_bar( stat = "count" ) +
  facet_grid( . ~ sex ) +
  my.theme +
  scale_fill_brewer( type = "qual", palette = 6, direction = -1 ) +
  labs( title = "Besuche Asthma" )


##Wie viele Besuche haben Allergien?

tbl.allerg <- tbl[!is.na(tbl$C_DISEASE_TX_ALLERG),]

table(tbl.allerg$C_DISEASE_TX_ALLERG, tbl.allerg$sex)

summary(tbl.allerg$C_DISEASE_TX_ALLERG)

ggplot( 
  tbl.allerg, 
  aes( C_DISEASE_TX_ALLERG, fill = sex ) ) + 
  geom_bar( stat = "count" ) +
  facet_grid( . ~ sex ) +
  my.theme +
  scale_fill_brewer( type = "qual", palette = 6, direction = -1 ) +
  labs( title = "Besuche Allergie" )

##Wie viele Besuche haben Neurodermitis?

tbl.neuroder <- tbl[!is.na(tbl$C_DISEASE_TX_NEURODER),]

table(tbl.neuroder$C_DISEASE_TX_NEURODER, tbl.neuroder$sex)

summary(tbl.neuroder$C_DISEASE_TX_NEURODER)

ggplot( 
  tbl.neuroder, 
  aes( C_DISEASE_TX_NEURODER, fill = sex ) ) + 
  geom_bar( stat = "count" ) +
  facet_grid( . ~ sex ) +
  my.theme +
  scale_fill_brewer( type = "qual", palette = 6, direction = -1 ) +
  labs( title = "Besuche Neurodermitis" )
 
##Wie viele Kinder haben Asthma?

tbl.asthma.knd <- tbl.asthma %>%
  group_by( TEILNEHMER_SIC, sex ) %>%
  summarise( score.asthma = mean( C_DISEASE_TX_ASTHMA ) )

table(tbl.asthma.knd$score.asthma, tbl.asthma.knd$sex)

summary(tbl.asthma.knd$score.asthma)

ggplot( 
  tbl.asthma.knd, 
  aes( score.asthma, fill = sex ) ) + 
  geom_bar( stat = "count" ) +
  facet_grid( . ~ sex ) +
  my.theme +
  scale_fill_brewer( type = "qual", palette = 6, direction = -1 ) +
  labs( title = "Kinder Asthma" )

##Wie viele Kinder haben Allergien?

tbl.allerg.knd <- tbl.allerg %>%
  group_by( TEILNEHMER_SIC, sex ) %>%
  summarise( score.allerg = mean( C_DISEASE_TX_ALLERG ) )

table(tbl.allerg.knd$score.allerg, tbl.allerg.knd$sex)

summary(tbl.allerg.knd$score.allerg)

ggplot( 
  tbl.allerg.knd, 
  aes( score.allerg, fill = sex ) ) + 
  geom_bar( stat = "count" ) +
  facet_grid( . ~ sex ) +
  my.theme +
  scale_fill_brewer( type = "qual", palette = 6, direction = -1 ) +
  labs( title = "Kinder Allergie" )

##Wie viele Kinder haben Neurodermitis?

tbl.neuroder.knd <- tbl.neuroder %>%
  group_by( TEILNEHMER_SIC, sex ) %>%
  summarise( score.neuroder = mean( C_DISEASE_TX_NEURODER ) )

table(tbl.neuroder.knd$score.neuroder, tbl.neuroder.knd$sex)

summary(tbl.neuroder.knd$score.neuroder)

ggplot( 
  tbl.neuroder.knd, 
  aes( score.neuroder, fill = sex ) ) + 
  geom_bar( stat = "count" ) +
  facet_grid( . ~ sex ) +
  my.theme +
  scale_fill_brewer( type = "qual", palette = 6, direction = -1 ) +
  labs( title = "Kinder Neurodermitis" )

