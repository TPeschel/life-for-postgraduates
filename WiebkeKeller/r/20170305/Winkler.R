#ibrary(helpR)
library(lubridate)
library(ggplot2)



tbls <-
  read.all.xlsx.tables( "../../data/" )

t.n <-
  all.xlsx.tables.names( tbls )

t.c.n <- 
  all.xlsx.tables.row.names( tbls )

t.c.n

t.c.n[7]

t.n[7]

d00177 <- get.tbl("D00177", all.tables=tbls)

##Verteilung Gesamt-Score Winkler Index
table(d00177$SCORE_FAM)
summary(d00177$SCORE_FAM)
ggplot(d00177, aes(x= SCORE_FAM))+geom_bar()

##Wie viele Familien gehören zur "Unterschicht" (3-8)?
d00177[d00177$SCORE_FAM < 8.1,]
d00177.8 <- d00177[d00177$SCORE_FAM < 8.1,]
table(d00177.8$SCORE_FAM)
summary(d00177.8$SCORE_FAM)
ggplot(d00177.8, aes(x= SCORE_FAM))+geom_bar()

##Wie viele Familien gehören zur "Mittelschicht" (9-14)?
d00177[d00177$SCORE_FAM >9 & d00177$SCORE_FAM <14,]
d00177.14 <- d00177[d00177$SCORE_FAM > 9 & d00177$SCORE_FAM < 14,]
table(d00177.14$SCORE_FAM)
summary(d00177.14$SCORE_FAM)
ggplot(d00177.14, aes(x=SCORE_FAM))+geom_bar()

##Wie viele Familien gehören zur "Oberschicht" (15-21)?
d00177[d00177$SCORE_FAM >= 15,]
d00177.21 <- d00177[d00177$SCORE_FAM >= 15,]
table(d00177.21$SCORE_FAM)
summary(d00177.21$SCORE_FAM)
ggplot(d00177.21, aes(x=SCORE_FAM))+geom_bar()
