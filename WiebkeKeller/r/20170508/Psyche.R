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

##Wie viele Besuche haben ADHS?

tbl.adhs <- tbl[!is.na(tbl$C_DISEASE_TX_ADHS),]

table(tbl.adhs$C_DISEASE_TX_ADHS, tbl.adhs$sex)

summary(tbl.adhs$C_DISEASE_TX_ADHS)

ggplot( 
  tbl.adhs, 
  aes( C_DISEASE_TX_ADHS, fill = sex ) ) + 
  geom_bar( stat = "count" ) +
  facet_grid( . ~ sex ) +
  my.theme +
  scale_fill_brewer( type = "qual", palette = 6, direction = -1 ) +
  labs( title = "Besuche ADHS" )


##Wie viele Besuche haben Depression?

tbl.depres <- tbl[!is.na(tbl$C_DISEASE_TX_DEPRES),]

table(tbl.depres$C_DISEASE_TX_DEPRES, tbl.depres$sex)

summary(tbl.depres$C_DISEASE_TX_DEPRES)

ggplot( 
  tbl.depres, 
  aes( C_DISEASE_TX_DEPRES, fill = sex ) ) + 
  geom_bar( stat = "count" ) +
  facet_grid( . ~ sex ) +
  my.theme +
  scale_fill_brewer( type = "qual", palette = 6, direction = -1 ) +
  labs( title = "Besuche Depression" )
##Kann man das vielleicht noch anders einstellen, damit man die Erkrankten
#auch im Plot erkennt?

##Wie viele Besuche haben psychische Erkrankungen?

tbl.psy <- tbl[!is.na(tbl$C_DISEASE_TX_PSY),]

table(tbl.psy$C_DISEASE_TX_PSY, tbl.psy$sex)

summary(tbl.psy$C_DISEASE_TX_PSY)

ggplot( 
  tbl.psy, 
  aes( C_DISEASE_TX_PSY, fill = sex ) ) + 
  geom_bar( stat = "count" ) +
  facet_grid( . ~ sex ) +
  my.theme +
  scale_fill_brewer( type = "qual", palette = 6, direction = -1 ) +
  labs( title = "Besuche Psychische Erkrankungen" )

##Wie viele Kinder haben ADHS?

tbl.adhs.knd <- tbl.adhs %>%
  group_by( TEILNEHMER_SIC, sex ) %>%
  summarise( score.adhs = mean( C_DISEASE_TX_ADHS ) )

table(tbl.adhs.knd$score.adhs, tbl.adhs.knd$sex)

summary(tbl.adhs.knd$score.adhs)

ggplot( 
  tbl.adhs.knd, 
  aes( score.adhs, fill = sex ) ) + 
  geom_bar( stat = "count" ) +
  facet_grid( . ~ sex ) +
  my.theme +
  scale_fill_brewer( type = "qual", palette = 6, direction = -1 ) +
  labs( title = "Kinder ADHS" )

##Wie viele Kinder haben Depressionen?

tbl.depres.knd <- tbl.depres %>%
  group_by( TEILNEHMER_SIC, sex ) %>%
  summarise( score.depres = mean( C_DISEASE_TX_DEPRES ) )

table(tbl.depres.knd$score.depres, tbl.depres.knd$sex)

summary(tbl.depres.knd$score.depres)

ggplot( 
  tbl.depres.knd, 
  aes( score.depres, fill = sex ) ) + 
  geom_bar( stat = "count" ) +
  facet_grid( . ~ sex ) +
  my.theme +
  scale_fill_brewer( type = "qual", palette = 6, direction = -1 ) +
  labs( title = "Kinder Depression" )

##Wie viele Kinder haben psychische Erkrankungen?

tbl.psy.knd <- tbl.psy %>%
  group_by( TEILNEHMER_SIC, sex ) %>%
  summarise( score.psy = mean( C_DISEASE_TX_PSY ) )

table(tbl.psy.knd$score.psy, tbl.psy.knd$sex)

summary(tbl.psy.knd$score.psy)

ggplot( 
  tbl.psy.knd, 
  aes( score.psy, fill = sex ) ) + 
  geom_bar( stat = "count" ) +
  facet_grid( . ~ sex ) +
  my.theme +
  scale_fill_brewer( type = "qual", palette = 6, direction = -1 ) +
  labs( title = "Kinder Psychische Erkrankungen" )
