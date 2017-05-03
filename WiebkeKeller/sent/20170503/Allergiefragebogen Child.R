library(helpR)
library(lubridate)
library(ggplot2)

tbls <-
  read.all.xlsx.tables( "../../data/" )

t.n <-
  all.xlsx.tables.names( tbls )

t.c.n <- 
  all.xlsx.tables.row.names( tbls )

t.c.n

t.c.n[15]

t.n[15]

t00214 <- get.tbl("T00214", all.tables=tbls)

##Wie viele Kinder haben laufende Nase?
table(t00214$S010293_F0012)
summary(t00214$S010293_F0012)
#auch wieder schwierige Darstellung 

##Wie viele Kinder haben diagnostizierten Heuschnupfen?
table(t00214$S010293_F0027)
summary(t00214$S010293_F0027)

##Wie viele Kinder haben Atembeschwerden?
table(t00214$S010293_F0039)
summary(t00214$S010293_F0039)

##Wie viele Kinder haben diagnostiziertes Asthma?
table(t00214$S010293_F0041)
summary(t00214$S010293_F0041)
