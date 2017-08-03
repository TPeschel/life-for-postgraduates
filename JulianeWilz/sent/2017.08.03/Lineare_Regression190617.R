# Loesche Speicher
rm( list =  ls( ) )

# Lade benoetigte Pakete
if( !"devtools" %in% rownames( installed.packages( ) ) ) install.packages( "devtools" )

devtools::install_github( "TPeschel/hlpr4life" )

library( hlpr4life )

load.pkgs(
    c(
        "readxl",
        "dplyr",
        "ggplot2",
        "reshape2",
        "effects" ) )


#setwd( "~/Desktop/pv0116_neu/")
setwd( "~/LIFE/life-for-postgraduates/JulianeWilz/sent/2017.08.03/" )

# daten <- read_excel( "Tabellen/AktuelleTabelle190517excel.xlsx" )

##
# lade Tabelle und benenne Spalten um
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

# ich betrachte Jungen und Mädchen getrennt
# ich betrachte bei beiden nur das erste Lebensjahr
d01 <- d[ between( d$AGE, 0, 1 ), ]
d01m <- d.m[ between(d.m$AGE, 0, 1 ), ]
d01f <- d.f[ between(d.f$AGE, 0, 1 ), ]

# dann habe ich eine lineare Regression durchgeführt und mir den Zusammenhang
# von Calcitonin und dem Alter angesehen
# ich betrachte ersteinmal nur die Jungen

lin.reg.01.m.CALCITONIN.AGE <- lm(d01m$CALCITONIN ~ d01m$AGE )
box.cox.01.m.CALCITONIN.AGE <- boxCox( lin.reg.01.m.CALCITONIN.AGE )
lambda <- box.cox.01.m.CALCITONIN.AGE$x[ which.max( box.cox.01.m.CALCITONIN.AGE$y ) ]

summary( lin.reg.01.m.CALCITONIN.AGE )
# Alter ist signifikant

#P1NP

#lineare Regression von Calcitonin mit dem Alter und P1NP
( lin.reg.01.m.CALCITONIN.AGE_P1NP <- lm(d01m$CALCITONIN ~ d01m$AGE * d01m$P1NP ) )
box.cox.01.m.CALCITONIN.AGE_P1NP <- boxCox( lin.reg.01.m.CALCITONIN.AGE_P1NP )
( lambda <- box.cox.01.m.CALCITONIN.AGE_P1NP$x[ which.max( box.cox.01.m.CALCITONIN.AGE_P1NP$y ) ] )
y.transf <- car::bcPower(d01m$CALCITONIN,lambda)



ggqqplot <-
    function( x ) {
        ggplot( ) +
            theme_bw( ) +
            geom_point( 
                aes(
                    quantile( rnorm( length( x ) ), seq( 0, 1, length.out = length( x ) ) ),
                    quantile( x, seq( 0, 1, length.out = length( x ) ) ) ) ) }

ggqqplot.standardized <-
    function( x ) {
        ggplot( ) +
            theme_bw( ) +
            geom_point( 
                aes(
                    quantile( rnorm( length( x ) ), seq( 0, 1, length.out = length( x ) ) ),
                    quantile( ( x - mean( x, na.rm = T ) ) / sd( x, na.rm = T ), seq( 0, 1, length.out = length( x ) ) ) ) ) }

ggqqplot( d01m$CALCITONIN )
qqnorm( d01m$CALCITONIN )
qqnorm( ( d01m$CALCITONIN - mean( d01m$CALCITONIN ) ) / sd( d01m$CALCITONIN ) )
ggqqplot.standardized( d01m$CALCITONIN )

ggplot( ) +
    geom_point( 
        aes(
            quantile( rnorm( length( d01m$CALCITONIN ) ), seq( 0, 1, length.out = length( d01m$CALCITONIN ) ) ),
            quantile( ( d01m$CALCITONIN - mean( d01m$CALCITONIN ) ) / sd( d01m$CALCITONIN ), seq( 0, 1, length.out = length( d01m$CALCITONIN ) ) ) ) ) #+
    #geom_line( aes(x,y),data.frame(x=c(-10,10),y=c(-10,10)))

ggplot( ) +
    geom_point( 
        aes(
            quantile( 
                rnorm( 
                    length( d01m$CALCITONIN ), 
                    mean( d01m$CALCITONIN ), 
                    sd(d01m$CALCITONIN ) ),  
                seq( 0, 1, length.out = length( d01m$CALCITONIN ) ) ),
            quantile( d01m$CALCITONIN, seq( 0, 1, length.out = length( d01m$CALCITONIN ) ) ) ) ) #+
    #geom_line( aes(x,y),data.frame(x=c(-10,10),y=c(-10,10)))

ggplot( ) +
    geom_point( 
        aes(
            quantile( ( y.transf - mean( y.transf ) ) / sd( y.transf ), seq( 0, 1, length.out = length( y.transf ) ) ),
            quantile( rnorm( length( y.transf ), mean( y.transf ), sd( y.transf ) ), seq( 0, 1, length.out = length( y.transf ) ) ) ) ) 

qqnorm( d01m$CALCITONIN )
qqnorm( ( d01m$CALCITONIN - mean( d01m$CALCITONIN ) ) / sd( d01m$CALCITONIN ) )
qqnorm( y.transf )
qqline( y.transf )

lin.reg.01.m.CALCITONIN.AGE_P1NP <- lm( y.transf ~ d01m$AGE * d01m$P1NP )

summary( lin.reg.01.m.CALCITONIN.AGE_P1NP )

#P1NP und Alter sind signifikant, wenn auch Zusammenhang mit von P1NP Calcitonin klein

#ich schließe einen vermeintlichen Ausreißer aus
d01m.p.aus <- d01m[ d01m$P1NP_S_NUM_VALUE != 5515, ]

linreg <- lm(d01m.p.aus$CT_S_1_NUM_VALUE ~ d01m.p.aus$AGE_Calcitionin +
               d01m.p.aus$P1NP_S_NUM_VALUE)
summary(linreg)
#Zusammenhang stärker

#zum Vergleich: P1NP alleine
linreg <- lm(d01m.p.aus$CT_S_1_NUM_VALUE ~ d01m.p.aus$P1NP_S_NUM_VALUE)
summary(linreg)


#lineare Regression von Calcitonin mit dem Alter und P1NP und Osteocalcin
linreg <- lm(d01m.p.aus$CT_S_1_NUM_VALUE ~ d01m.p.aus$AGE_Calcitionin +
               d01m.p.aus$P1NP_S_NUM_VALUE + d01m.p.aus$OSTEO_S_NUM_VALUE)
summary(linreg)
# keine Signifikanz von Osteocalcin

#Osteocalcin
#lineare Regression von Calcitonin mit dem Alter und Osteocalcin

linreg <- lm(d01m$CT_S_1_NUM_VALUE ~ d01m$AGE_Calcitionin +
               d01m$OSTEO_S_NUM_VALUE)
summary(linreg)
# ohne P1NP ist Osteocalcin signifikant

linreg <- lm(d01m$CT_S_1_NUM_VALUE ~ d01m$AGE_Calcitionin +
               d01m$OSTEO_S_NUM_VALUE*d01m$P1NP_S_NUM_VALUE)
summary(linreg)
##P1NP und Osteocalcin zusammen nicht signifikant

#Phosphat

#lineare Regression von Calcitonin mit dem Alter und Phosphat
linreg <- lm(d01m$CT_S_1_NUM_VALUE ~ d01m$AGE_Calcitionin + d01m$P_S_NUM_VALUE)

summary(linreg)
#Phosphat ist nicht signifikant

linreg <- lm(d01m$CT_S_1_NUM_VALUE ~ d01m$AGE_Calcitionin +
               d01m$P1NP_S_NUM_VALUE + d01m$P_S_NUM_VALUE)
summary(linreg)
#auch nicht, wenn P1NP dabei ist

#Calcium

#lineare Regression von Calcitonin mit dem Alter und Calcium
linreg <- lm(d01m$CT_S_1_NUM_VALUE ~ d01m$AGE_Calcitionin + d01m$CA_S_NUM_VALUE)

summary(linreg)
#Calcium ist nicht signifikant

d01m.c.aus <- d01m[ d01m$CA_S_NUM_VALUE != 1.93, ]

linreg <- lm(d01m.c.aus$CT_S_1_NUM_VALUE ~ d01m.c.aus$AGE_Calcitionin + d01m.c.aus$CA_S_NUM_VALUE)

summary(linreg)

#auch nicht ohne Ausreißer signifikant

#Gewicht_SDS

#lineare Regression von Calcitonin mit dem Alter und Gewicht_SDS
linreg <- lm(d01m$CT_S_1_NUM_VALUE ~ d01m$AGE_Calcitionin + d01m$C_ANTHRO_KH_WEIGHT_ADJ)

summary(linreg)

#Gewicht SDS nicht signifikant

#BMI

linreg <- lm(d01m$CT_S_1_NUM_VALUE ~ d01m$AGE_Calcitionin + d01m$C_ANTHRO_KH_BMI_ADJ)

summary(linreg)
#BMI knapp nicht signifikant


#BMI_SDS


#lineare Regression von Calcitonin mit dem Alter und BMI_SDS
linreg <- lm(d01m$CT_S_1_NUM_VALUE ~ d01m$AGE_Calcitionin + d01m$C_ANTHRO_KH_BMI_ADJ)

summary(linreg)
#BMI_SDS signifikant

linreg <- lm(d01m$CT_S_1_NUM_VALUE ~ d01m$AGE_Calcitionin * d01m$C_ANTHRO_KH_BMI_ADJ)

summary(linreg)
#einzeln beide signifikant, zusammen nicht

#lineare Regression von Calcitonin mit dem Alter und P1NP und BMI_SDS

d01m.p.aus <- d01m[ d01m$P1NP_S_NUM_VALUE != 5515, ]

linreg <- lm(d01m.p.aus$CT_S_1_NUM_VALUE ~ d01m.p.aus$AGE_Calcitionin + d01m.p.aus$P1NP_S_NUM_VALUE + d01m.p.aus$C_ANTHRO_KH_BMI_ADJ)
summary(linreg)

#Zusammenhang von Calcitonin mit Alter, P1NP und BMI_SDS signifikant

#lineare Regression von Calcitonin mit dem Alter und Osteocalcin und BMI_SDS
linreg <- lm(d01m$CT_S_1_NUM_VALUE ~ d01m$AGE_Calcitionin + d01m$OSTEO_S_NUM_VALUE + d01m$C_ANTHRO_KH_BMI_ADJ)
summary (linreg)
#Zusammenhang von Calcitonin mit Alter, Osteocalcin und BMI_SDS signifikant

#IGF-1

#lineare Regression von Calcitonin mit dem Alter und IGF-1
linreg <- lm(d01m$CT_S_1_NUM_VALUE ~ d01m$AGE_Calcitionin + d01m$IGF1_S_2_NUM_VALUE)

summary(linreg)

#IGF1 signifikant

#lineare Regression von Calcitonin mit dem Alter und P1NP und IGF-1
d01m.p.aus <- d01m[ d01m$P1NP_S_NUM_VALUE != 5515, ]
linreg <- lm(d01m$CT_S_1_NUM_VALUE ~ d01m$AGE_Calcitionin + d01m$P1NP_S_NUM_VALUE + d01m$IGF1_S_2_NUM_VALUE)
summary(linreg)
#nur noch Alter signifikant

#lineare Regression von Calcitonin mit dem Alter und BMI_SDS, IGF-1 und P1NP

linreg <- lm(d01m.p.aus$CT_S_1_NUM_VALUE ~ d01m.p.aus$AGE_Calcitionin + d01m.p.aus$C_ANTHRO_KH_BMI_ADJ  + d01m.p.aus$IGF1_S_2_NUM_VALUE + d01m.p.aus$P1NP_S_NUM_VALUE)
summary(linreg)
#BMI und IGF-1 nicht signifikant

#Alter, BMI-SDS, IGF-1
linreg <- lm(d01m$CT_S_1_NUM_VALUE ~ d01m$AGE_Calcitionin + d01m$C_ANTHRO_KH_BMI_ADJ  + d01m$IGF1_S_2_NUM_VALUE)
summary(linreg)
#alle signifikant, nur BMI-SDS nicht

# Alter, P1NP, BMI, IGF1
d01m.p.aus <- d01m[ d01m$P1NP_S_NUM_VALUE != 5515, ]
linreg <- lm(d01m.p.aus$CT_S_1_NUM_VALUE ~ d01m.p.aus$AGE_Calcitionin + d01m.p.aus$C_ANTHRO_KH_BMI_ORIG  + d01m.p.aus$IGF1_S_2_NUM_VALUE + d01m.p.aus$P1NP_S_NUM_VALUE )
summary(linreg)
#BMI und IGF-1 nicht signifikant

#Alter, IGF-1, P1NP, Osteocalcin
linreg <- lm(d01m.p.aus$CT_S_1_NUM_VALUE ~ d01m.p.aus$AGE_Calcitionin + d01m.p.aus$C_ANTHRO_KH_BMI_ADJ  + d01m.p.aus$IGF1_S_2_NUM_VALUE + d01m.p.aus$P1NP_S_NUM_VALUE + d01m.p.aus$OSTEO_S_NUM_VALUE  )
summary(linreg)
#nur Alter signifikant

#IGF-1-SDS

linreg <- lm(d01m$CT_S_1_NUM_VALUE ~ d01m$AGE_Calcitionin + d01m$ZIGF_S_2_NUM_VALUE)
summary(linreg)

#IGF1-SDS signifikant

#lineare Regression von Calcitonin mit dem Alter und P1NP und IGF-1-SDS
d01m.p.aus <- d01m[ d01m$P1NP_S_NUM_VALUE != 5515, ]
linreg <- lm(d01m.p.aus$CT_S_1_NUM_VALUE ~ d01m.p.aus$AGE_Calcitionin + d01m.p.aus$P1NP_S_NUM_VALUE + d01m.p.aus$ZIGF_S_2_NUM_VALUE)
summary(linreg)
#nur noch Alter signifikant

#lineare Regression von Calcitonin mit dem Alter und BMI_SDS und IGF-1-SDS

linreg <- lm(d01m$CT_S_1_NUM_VALUE ~ d01m$AGE_Calcitionin + d01m$C_ANTHRO_KH_BMI_ADJ  + d01m$ZIGF_S_2_NUM_VALUE)
summary(linreg)

#Alter und IGF-1-SDS signifikant. BMI SDS nicht

#Alter, BMI-SDS, P1NP, IGF-1-SDS
linreg <- lm(d01m.p.aus$CT_S_1_NUM_VALUE ~ d01m.p.aus$AGE_Calcitionin + d01m.p.aus$C_ANTHRO_KH_BMI_ADJ  + d01m.p.aus$ZIGF_S_2_NUM_VALUE + d01m.p.aus$P1NP_S_NUM_VALUE )
summary(linreg)
#IGF-1-SDS  und BMI-SDS nicht signifikant

#Alter, BMI-SDS, P1NP, Osteocalcin, IGF-1-SDS
linreg <- lm(d01m.p.aus$CT_S_1_NUM_VALUE ~ d01m.p.aus$AGE_Calcitionin + d01m.p.aus$C_ANTHRO_KH_BMI_ADJ  + d01m.p.aus$ZIGF_S_2_NUM_VALUE + d01m.p.aus$P1NP_S_NUM_VALUE + d01m.p.aus$OSTEO_S_NUM_VALUE  )
summary(linreg)
# nur noch Altr signifikant

#Alter, P1NP, BMI

linreg <- lm(d01m.p.aus$CT_S_1_NUM_VALUE ~ d01m.p.aus$AGE_Calcitionin + d01m.p.aus$C_ANTHRO_KH_BMI_ORIG  + d01m.p.aus$P1NP_S_NUM_VALUE )
summary(linreg)

#Alter, BMI-SDS, IGF1-SDS
linreg <- lm(d01m$CT_S_1_NUM_VALUE ~ d01m$AGE_Calcitionin + d01m$C_ANTHRO_KH_BMI_ADJ  + d01m$ZIGF_S_2_NUM_VALUE )
summary(linreg)

#Alter, P1NP, IGF-1-SDS
linreg <- lm(d01m.p.aus$CT_S_1_NUM_VALUE ~ d01m.p.aus$AGE_Calcitionin + d01m.p.aus$P1NP_S_NUM_VALUE  + d01m.p.aus$ZIGF_S_2_NUM_VALUE )
summary(linreg)

#Alter, P1NP, IGF-1-SDS, BMI-SDS
linreg <- lm(d01m.p.aus$CT_S_1_NUM_VALUE ~ d01m.p.aus$AGE_Calcitionin + d01m.p.aus$P1NP_S_NUM_VALUE  + d01m.p.aus$C_ANTHRO_KH_BMI_ADJ + d01m.p.aus$ZIGF_S_2_NUM_VALUE )
summary(linreg)

#Alter, P1NP, IGF-1, BMI
linreg <- lm(d01m.p.aus$CT_S_1_NUM_VALUE ~ d01m.p.aus$AGE_Calcitionin + d01m.p.aus$P1NP_S_NUM_VALUE  + d01m.p.aus$C_ANTHRO_KH_BMI_ORIG + d01m.p.aus$IGF1_S_2_NUM_VALUE )
summary(linreg)

#Alter, Osteocalcin, BMI

linreg <- lm(d01m$CT_S_1_NUM_VALUE ~ d01m$AGE_Calcitionin + d01m$C_ANTHRO_KH_BMI_ORIG  + d01m$OSTEO_S_NUM_VALUE )
summary(linreg)

#Alter, BMI, Calcium
d01m.c.aus <- d01m[ d01m$CA_S_NUM_VALUE != 1.93, ]
linreg <- lm(d01m.c.aus$CT_S_1_NUM_VALUE ~ d01m.c.aus$AGE_Calcitionin + d01m.c.aus$C_ANTHRO_KH_BMI_ORIG  + d01m.c.aus$CA_S_NUM_VALUE )
summary(linreg)
#nur Alter signifikant, der Rest knapp nicht 

#Alter, BMI-SDS, Calcium
linreg <- lm(d01m.c.aus$CT_S_1_NUM_VALUE ~ d01m.c.aus$AGE_Calcitionin + d01m.c.aus$C_ANTHRO_KH_BMI_ADJ  + d01m.c.aus$CA_S_NUM_VALUE )
summary(linreg)
#Alter und BMI-SDS signifikant, Ca knapp nicht


#Alter, Calcium, IGF-1
d01m.c.aus <- d01m[ d01m$CA_S_NUM_VALUE != 1.93, ]
linreg <- lm(d01m.c.aus$CT_S_1_NUM_VALUE ~ d01m.c.aus$AGE_Calcitionin + d01m.c.aus$IGF1_S_2_NUM_VALUE  + d01m.c.aus$CA_S_NUM_VALUE )
summary(linreg)
#Alter, Ca, IGF-1 signifikant

#Alter,Calcium, IGF-1-SDS
d01m.c.aus <- d01m[ d01m$CA_S_NUM_VALUE != 1.93, ]
linreg <- lm(d01m.c.aus$CT_S_1_NUM_VALUE ~ d01m.c.aus$AGE_Calcitionin + d01m.c.aus$ZIGF_S_2_NUM_VALUE  + d01m.c.aus$CA_S_NUM_VALUE )
summary(linreg)
#Alter, Ca, IGF-1-SDS signifikant

#Alter,Calcium, Phosphat
d01m.aus <- d01m[ d01m$CA_S_NUM_VALUE != 1.93, ]
linreg <- lm(d01m.c.aus$CT_S_1_NUM_VALUE ~ d01m.c.aus$AGE_Calcitionin + d01m.c.aus$P_S_NUM_VALUE  + d01m.c.aus$CA_S_NUM_VALUE )
summary(linreg)
#Alter, Ca signifikant, Phosphat deutlich nicht

#Alter, P1NP, Calcium
d01m.pc.aus <- d01m[ d01m$P1NP_S_NUM_VALUE != 5515 & d01m$CA_S_NUM_VALUE != 1.93, ]
linreg <- lm(d01m.pc.aus$CT_S_1_NUM_VALUE ~ d01m.pc.aus$AGE_Calcitionin + d01m.pc.aus$P1NP_S_NUM_VALUE  + d01m.pc.aus$CA_S_NUM_VALUE )
summary(linreg)
#signifikant

#Alter, Calcium, IGF-1-SDS, P1NP
d01m.pc.aus <- d01m[ d01m$P1NP_S_NUM_VALUE != 5515 & d01m$CA_S_NUM_VALUE != 1.93, ]
linreg <- lm(d01m.pc.aus$CT_S_1_NUM_VALUE ~ d01m.pc.aus$AGE_Calcitionin + d01m.pc.aus$ZIGF_S_2_NUM_VALUE  + d01m.pc.aus$CA_S_NUM_VALUE + d01m.pc.aus$P1NP_S_NUM_VALUE)
summary(linreg)
#Ca und P1NP signifikant, Alter und IGF-1-SDS deutlich nicht

#Alter, Calcium, IGF-1, P1NP
d01m.pc.aus <- d01m[ d01m$P1NP_S_NUM_VALUE != 5515 & d01m$CA_S_NUM_VALUE != 1.93, ]
linreg <- lm(d01m.pc.aus$CT_S_1_NUM_VALUE ~ d01m.pc.aus$AGE_Calcitionin + d01m.pc.aus$IGF1_S_2_NUM_VALUE  + d01m.pc.aus$CA_S_NUM_VALUE + d01m.pc.aus$P1NP_S_NUM_VALUE)
summary(linreg)
#Ca und P1NP signifikant, Alter und IGF-1 deutlich nicht



#Alter, P1NP, Calcium, Osteocalcin
d01m.pc.aus <- d01m[ d01m$P1NP_S_NUM_VALUE != 5515 & d01m$CA_S_NUM_VALUE != 1.93, ]
linreg <- lm(d01m.pc.aus$CT_S_1_NUM_VALUE ~ d01m.pc.aus$AGE_Calcitionin + d01m.pc.aus$P1NP_S_NUM_VALUE  + d01m.pc.aus$CA_S_NUM_VALUE + d01m.pc.aus$OSTEO_S_NUM_VALUE)
summary(linreg)
#alle bis auf Osteocalcin sind signifikant


#n-Zahlen

rm( list =  ls( ) )

setwd( "~/Desktop/pv0116_neu/")

daten <-
  read_excel( "Tabellen/AktuelleTabelle190517excel.xlsx" )



d <-
  daten[ !is.na( daten$AGE_Calcitionin ) & !is.na( daten$CT_S_1_NUM_VALUE )  &  !is.na( daten$IGF1_S_2_NUM_VALUE) &  !is.na( daten$P1NP_S_NUM_VALUE) , ] 




d$AGE_Calcitionin <- as.numeric( d$AGE_Calcitionin )

d.m <- d[ d$TEILNEHMER_GESCHLECHT=="male", ]


d01m <- d.m[between(d.m$AGE_Calcitionin, 0,1),]

d01m.aus <- d01m[ d01m$P1NP_S_NUM_VALUE != 5515, ]
d01m.aus <- d01m[ d01m$CA_S_NUM_VALUE != 1.93, ]
d01m.pc.aus <- d01m[ d01m$P1NP_S_NUM_VALUE != 5515 & d01m$CA_S_NUM_VALUE != 1.93, ]
