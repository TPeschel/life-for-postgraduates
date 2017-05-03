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

t.c.n[14]

t.n[14]

t00213 <- get.tbl("T00213", all.tables=tbls)

##Wie viele Kinder haben laufende Nase?
table(t00213$FB_ALLERGY_BP1_F0014)
summary(t00213$FB_ALLERGY_BP1_F0014)
#auch wieder schwierige Darstellung 

##Wie viele Kinder haben einen diagnostizierten Heuschnupfen?
table(t00213$FB_ALLERGY_BP1_F0029)
summary(t00213$FB_ALLERGY_BP1_F0029)

##Wie viele Kinder haben Atembeschwerden?
table(t00213$FB_ALLERGY_BP1_F0041)
summary(t00213$FB_ALLERGY_BP1_F0041)

##Wie viele Kinder haben diagnostiziertes Asthma?
table(t00213$FB_ALLERGY_BP1_F0043)
summary(t00213$FB_ALLERGY_BP1_F0043)
