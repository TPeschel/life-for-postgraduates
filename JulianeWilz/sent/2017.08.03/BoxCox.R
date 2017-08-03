# Loesche Speicher
rm( list =  ls( ) )

##
# installiere devtools, falls noch nicht geschehen
##
if( !"devtools" %in% rownames( installed.packages( ) ) ) install.packages( "devtools" )

##
# installiere helper for life 
##
devtools::install_github( "TPeschel/hlpr4life" )

##
# lade helper for life
##
library( hlpr4life )

load.pkgs(
    c(
        "readxl",
        "dplyr",
        "ggplot2",
        "reshape2",
        "effects",
        "car" ) )


#setwd( "~/Desktop/pv0116_neu/")
setwd( "~/LIFE/life-for-postgraduates/JulianeWilz/sent/2017.08.03/" )

##
# lade Tabelle und benenne Spalten sofort um, (dafuer ist hlpr4life noetig)
# kannst natuerlich noch weitere Spalten umbennen, wenn Du willst, kannst es aber auch lassen (O;
# macht halt die Zeilen etwas kuerzer
# einfach, alte Spaltennamen hinzufuegen und in der zweiten Liste den entsprechenden neuen Namen hinzufuegen
##
daten <- 
    rename.columns( 
        read_excel( "AktuelleTabelle190517excel.xlsx" ), 
        c( "AGE_Calcitionin", "TEILNEHMER_GESCHLECHT", "CT_S_1_NUM_VALUE", "P1NP_S_NUM_VALUE" ),
        c( "AGE", "SEX", "CALCITONIN", "P1NP" ) )

d <- daten[ !is.na( daten$AGE ) & !is.na( daten$CALCITONIN ), ]

d$AGE <- as.numeric( d$AGE )

d.m <- d[ d$SEX == "male", ]
d.f <- d[ d$SEX == "female", ]

d01 <- d[ between( d$AGE, 0, 1 ), ]
d01m <- d.m[ between(d.m$AGE, 0, 1 ), ]
d01f <- d.f[ between(d.f$AGE, 0, 1 ), ]


##
# lin reg kalzitonin gegen alter 
##
lin.reg.01.m.CALCITONIN.AGE <- lm(d01m$CALCITONIN ~ d01m$AGE )
summary( lin.reg.01.m.CALCITONIN.AGE )

##
# qqplot anschauen, um zu sehen, ob Kalzitonin normalverteilt ist
##
qqnorm( d01m$CALCITONIN )
qqline( d01m$CALCITONIN )
##
# eher nicht
##

##
# ermittle Box-Cox-Transformations Parameter (Exponent)
##
box.cox.01.m.CALCITONIN.AGE <- boxCox( lin.reg.01.m.CALCITONIN.AGE )

##
# lambda (Exponent) bei maximum von log-likelyhood
##
( lambda <- box.cox.01.m.CALCITONIN.AGE$x[ which.max( box.cox.01.m.CALCITONIN.AGE$y ) ] )

##
# transformiere Kalzitonin mit dem ermittelten Lambda
##
calcitonin.trans <- car::bcPower( d01m$CALCITONIN, lambda )

##
# nochmal qqplot anschauen, jz aba das transformierte Kalzitonin
##
qqnorm( calcitonin.trans )
qqline( calcitonin.trans )
##
# viel besser!
# also lin reg jz mit transformiertem Kalzitonin
##
lin.reg.01.m.CALCITONIN_TRANSF.AGE <- lm( calcitonin.trans ~ d01m$AGE )
summary( lin.reg.01.m.CALCITONIN_TRANSF.AGE )
#P1NP

#lineare Regression von Calcitonin mit dem Alter und P1NP
( lin.reg.01.m.CALCITONIN.AGE_P1NP <- lm( d01m$CALCITONIN ~ d01m$AGE + d01m$P1NP ) )
summary( lin.reg.01.m.CALCITONIN.AGE_P1NP )

box.cox.01.m.CALCITONIN.AGE_P1NP <- boxCox( lin.reg.01.m.CALCITONIN.AGE_P1NP )
( lambda <- box.cox.01.m.CALCITONIN.AGE_P1NP$x[ which.max( box.cox.01.m.CALCITONIN.AGE_P1NP$y ) ] )
# lambda ist jetzt anders, dh es muss wieder transformiert werden
calcitonin.trans <- car::bcPower( d01m$CALCITONIN, lambda )

##
# nochmal qqplot anschauen, jz aba das transformierte Kalzitonin
##
qqnorm( calcitonin.trans )
qqline( calcitonin.trans )
##
# sieht auch gut aus!
# also lin reg jz wieder mit transformiertem Kalzitonin
##
lin.reg.01.m.CALCITONIN_TRANSF.AGE_P1NP <- lm( calcitonin.trans ~ d01m$AGE + d01m$P1NP )
summary( lin.reg.01.m.CALCITONIN_TRANSF.AGE_P1NP )

