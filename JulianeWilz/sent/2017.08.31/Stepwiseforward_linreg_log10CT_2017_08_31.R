########################################################################
# Loesche Speicher
rm( list =  ls( ) )
########################################################################
# lade alle noetigen Pakete 
hlpr4life::load.pkgs( 
    c( 
        "hlpr4life",   # load.pkgs, ggsubplot
        "dplyr",
        "readxl",      # read_excel
        "ggplot2",     # ggplot
        "lsr",         # etaSquared
        "Hmisc" ) )    # rcorr
########################################################################

#setwd( "~/Desktop/pv0116_neu/")
setwd( "~/LIFE/life-for-postgraduates/JulianeWilz/data/" )

# daten <-
#   read_excel( "Tabellen/AktuelleTabelle190517excel.xlsx" )

# wieso is meine tabelle neuer als deine?
daten <-
  read_excel( "../sent/data.2017.08.31/AktuelleTabelle190517excel.xlsx" )

d <-
    rename.columns( 
        daten,
        c( 
            "AGE_Calcitionin",
            "TEILNEHMER_GESCHLECHT",
            "C_ANTHRO_KH_HEIGHT_ORIG",
            "C_ANTHRO_KH_WEIGHT_ORIG",
            "C_ANTHRO_KH_BMI_ORIG",
            "C_ANTHRO_KH_HEIGHT_ADJ",
            "C_ANTHRO_KH_WEIGHT_ADJ",
            "C_ANTHRO_KH_BMI_ADJ",
            "CT_S_1_NUM_VALUE" ),
        c(
            "age",
            "sex",
            "height",
            "weight",
            "bmi",
            "height.sds",
            "weight.sds",
            "bmi.sds",
            "CT" ) )

d <-
  d[ !is.na( d$age ) & !is.na( d$CT ), ]

d$age <- 
    as.numeric( d$age )

d$log10CT <-
    log10( d$CT )

# ich betrachte Jungen und Mädchen getrennt
# ich betrachte bei beiden nur das erste Lebensjahr
d01 <-
    d[ between( d$age, 0, 1 ), ]

d01m <-
    d01[ d01$sex == "male", ]

d01f <-
    d01[ d01$sex == "female", ]


##
# muss an der Stelle leider feststellen, 
# dass die sds-Werte alle eine Altersabhaengigkeit zeigen
# muessen wir mal drueber sprechen, wie man damit umgeht
##
ggsubplot(
    ggplot( d, aes( age, weight.sds ) ) + geom_point( ) + geom_smooth( method = "lm" ) + theme_bw( ) + facet_grid( sex ~ . ),
    ggplot( d, aes( age, height.sds ) ) + geom_point( ) + geom_smooth( method = "lm" ) + theme_bw( ) + facet_grid( sex ~ . ),
    ggplot( d, aes( age, bmi.sds ) ) + geom_point( ) + geom_smooth( method = "lm" ) + theme_bw( ) + facet_grid( sex ~ . ),
    cols = 3 )

# missings raus
d <-
    d[ !is.na( d$weight.sds ) & !is.na( d$height.sds ) & !is.na( d$bmi.sds ), ]

ggsubplot(
    ggplot( d, aes( age, weight.sds ) ) + geom_point( ) + geom_smooth( method = "lm" ) + theme_bw( ) + facet_grid( sex ~ . ),
    ggplot( d, aes( age, height.sds ) ) + geom_point( ) + geom_smooth( method = "lm" ) + theme_bw( ) + facet_grid( sex ~ . ),
    ggplot( d, aes( age, bmi.sds ) ) + geom_point( ) + geom_smooth( method = "lm" ) + theme_bw( ) + facet_grid( sex ~ . ),
    cols = 3 )

summary( lm( weight.sds ~ age * sex, d ) )
summary( lm( height.sds ~ age * sex, d ) )
summary( lm( bmi.sds ~ age * sex, d ) )
########################################################################

# Hier mal Deine Box-Plots und wie sie richtig sein sollten
# Du hast die Achsen verwechselt in Deiner Mehl
# die oberen 2 sind die richtigen
# die unteren 2 Deine (O;

ggsubplot(
    ggplot( d01m ) + theme_bw( ) + geom_boxplot( aes( age, CT ) ) + ggtitle( "kein Log10" ),
    ggplot( d01m ) + theme_bw( ) + geom_boxplot( aes( CT, age ) ) + ggtitle( "Achsen vertauscht" ),
    ggplot( d01m ) + theme_bw( ) + geom_boxplot( aes( age, log10CT ) ) + ggtitle( "Log10" ),
    ggplot( d01m ) + theme_bw( ) + geom_boxplot( aes( log10CT, age ) ) + ggtitle( "Achsen vertauscht" ),
    cols = 2 )

# fuer boxplots immer Kategorien fuer die x-Achse bilden
# deswegen die 4 Warnungen
d01m$age.cat <-
    cut( x = d01m$age, breaks = c( -Inf, Inf ), labels = "ages between 0 and 1 years" )

ggsubplot(
    ggplot( d01m ) + theme_bw( ) + geom_boxplot( aes( age.cat, CT ) ) + ggtitle( "kein Log10" ),
    ggplot( d01m ) + theme_bw( ) + geom_boxplot( aes( age.cat, log10CT ) ) + ggtitle( "Log10" ),
    cols = 2 )

#####################################

# dann habe ich eine lineare Regression durchgeführt und mir den Zusammenhang
# von Calcitonin und dem Alter angesehen
# ich betrachte ersteinmal nur die Jungen

d01m <-
    d01m[ !is.na( d01m$height ) & !is.na( d01m$weight ) & !is.na( d01m$bmi ), ]

lm.log10CT.age <-
    lm( log10CT ~ age, d01m )

summary( lm.log10CT.age ) 

my.summary( rename.columns( d01m, c( "CT", "sex" ), c( "Y", "GROUP" ) ) )

sum.of.squares( d01m$log10CT )
sum.of.squares( d01m$log10CT[ d01m$age.cat == 1 ] )

etaSquared( lm.log10CT.age )
#Alter ist signifikant

#####################################

lm.log10CT.age.plus.height <-
    lm( log10CT ~ age + height, d01m )

summary( lm.log10CT.age.plus.height )

etaSquared( lm.log10CT.age.plus.height ) 

rcorr( d01m$age, d01m$height )
# Alter und Groesse sind fast dasselbe! Korrelation von 0.9

ggsubplot(
    ggplot( d01m, aes( age, height ) ) + geom_point( ) + geom_smooth( method = "lm" ) + theme_bw( ),
    ggplot( d01m, aes( age, log10CT ) ) + geom_point( ) + geom_smooth( method = "lm" ) + theme_bw( ),
    ggplot( d01m, aes( height, log10CT ) ) + geom_point( ) + geom_smooth( method = "lm" ) + theme_bw( ),
    cols = 3 )

#####################################
# TESTE auch immer die Interaktion zweier Variablen, hier mal Alter und Hoehe!!!
# meist ist keine da, aber manchmal eben schon
# hier nur sehr schwach und nicht signifikant
lm.log10CT.age.times.height <-
    lm( log10CT ~ age * height, d01m )
# Sinnvoll sind meist nur Interaktionen mit einer metrischen Variable und einer kategoriellen
# z.B. log10CT ~ weight * C_PUB_STAT_G

summary( lm.log10CT.age.times.height )

etaSquared( lm.log10CT.age.times.height ) 
#Größe_Orig knapp nicht signifikant -> rausschmeißen?

anova( lm.log10CT.age, lm.log10CT.age.plus.height, lm.log10CT.age.times.height )
#####################################

lm.log10CT.age.plus.weight <- 
    lm( log10CT ~ age + weight, d01m )

summary( lm.log10CT.age.plus.weight )

etaSquared( lm.log10CT.age.plus.weight )
#Gewicht_orig nicht signifikant -> rausschmeißen

#####################################

lm.log10CT.age.plus.weight.sds <-
    lm( log10CT ~ age + weight.sds, d01m )

summary( lm.log10CT.age.plus.weight.sds )

etaSquared( lm.log10CT.age.plus.weight.sds )
#Gewicht_adj nicht signifikant -> rausschmeißen

#####################################

lm.log10CT.age.plus.bmi.sds <-
    lm( log10CT ~ age + bmi.sds, d01m )

summary( lm.log10CT.age.plus.bmi.sds )

etaSquared( lm.log10CT.age.plus.bmi.sds )
#BMI-adj signifikant R-Quadrat 0.2965 adj R-Quadrat 0.2908

#####################################

lm.log10CT.age.bmi <-
    lm( log10CT ~ age + bmi, d01m )

summary( lm.log10CT.age.bmi )

etaSquared( lm.log10CT.age.bmi )
#BMI signifikant, adjusted R-squared bei BMI-SDS besser

#####################################

lm.log10CT.age.plus.height.sds <- 
    lm( log10CT ~ age + height.sds, d01m )

summary( lm.log10CT.age.plus.height.sds )

etaSquared( lm.log10CT.age.plus.height.sds )
#Größe-adj nicht signifikant

#####################################






##
# THEORETISCH
# SO WEITER
##



# Jetzt adjustieren wir mal fuer das Alter, wie es Prof. Kratzsch vorschlug
# Was er meinte, findest Du hier in dem sehr guten pdf zur Ancova
# https://www.tu-chemnitz.de/hsw/psychologie/professuren/method/homepages/ts/methodenlehre/meth3.pdf

summary( lm. <- lm( log10CT ~ age, d01m ) )

d01m$log10CT <- lm.$residuals

summary( lm. <- lm( log10CT ~ age, d01m ) )

# nochmal den Plot von Oben
# Du siehst keine Altersabhaengigkeit mehr zu log10CT
# Also braucht diese auch nicht mehr in Deiner linearen Regression vorzukommen
ggsubplot(
    ggplot( d01m, aes( age, height ) ) + geom_point( ) + geom_smooth( method = "lm" ) + theme_bw( ),
    ggplot( d01m, aes( age, log10CT ) ) + geom_point( ) + geom_smooth( method = "lm" ) + theme_bw( ),
    ggplot( d01m, aes( height, log10CT ) ) + geom_point( ) + geom_smooth( method = "lm" ) + theme_bw( ),
    cols = 3 )

#####################################
# nur noch Groesse assoziieren
lm.log10CT.height <-
    lm( log10CT ~ height, d01m )

summary( lm.log10CT.height )

etaSquared( lm.log10CT.height ) 

rcorr( d01m$age, d01m$height )
# Die Groesse korreliert stark mit dem Alter

# keine Signifikanz, sieht man auch im Plot
ggsubplot(
    ggplot( d01m, aes( age, height ) ) + geom_point( ) + geom_smooth( method = "lm" ) + theme_bw( ),
    ggplot( d01m, aes( height, log10CT ) ) + geom_point( ) + geom_smooth( method = "lm" ) + theme_bw( ),
    cols = 2 )

#####################################

summary( 
    lm.log10CT.weight <- 
        lm( log10CT ~ weight, d01m ) )

etaSquared( lm.log10CT.weight )

rcorr( d01m$age, d01m$weight ) # das Gewicht korreliert auch stark mit dem Alter
# keine signifikante Abhaengigkeit, sieht man auch
ggsubplot(
    ggplot( d01m, aes( age, weight ) ) + geom_point( ) + geom_smooth( method = "lm" ) + theme_bw( ),
    ggplot( d01m, aes( weight, log10CT ) ) + geom_point( ) + geom_smooth( method = "lm" ) + theme_bw( ),
    cols = 2 )

#####################################

summary( 
    lm.log10CT.weight.sds <-
        lm( log10CT ~ weight.sds, d01m ) )

etaSquared( lm.log10CT.weight.sds )

rcorr( d01m$age, d01m$weight.sds ) # der Gewichts-SDS korreliert stark mit dem Alter, das sollte er nicht
#Gewicht_adj nicht signifikant -> rausschmeißen
ggsubplot(
    ggplot( d01m, aes( age, weight.sds ) ) + geom_point( ) + geom_smooth( method = "lm" ) + theme_bw( ),
    ggplot( d01m, aes( weight.sds, log10CT ) ) + geom_point( ) + geom_smooth( method = "lm" ) + theme_bw( ),
    cols = 2 )

#####################################

summary( 
    lm.log10CT.bmi.sds <-
        lm( log10CT ~ bmi.sds, d01m ) )

etaSquared( lm.log10CT.bmi.sds )

rcorr( d01m$age, d01m$bmi.sds ) # BMI-SDS korreliert nur schwach mit dem Alter

ggsubplot(
    ggplot( d01m, aes( age, bmi.sds ) ) + geom_point( ) + geom_smooth( method = "lm" ) + theme_bw( ),
    ggplot( d01m, aes( bmi.sds, log10CT ) ) + geom_point( ) + geom_smooth( method = "lm" ) + theme_bw( ),
    cols = 2 )

ggsubplot(
    ggplot( d01m, aes( age, bmi ) ) + geom_point( ) + geom_smooth( method = "lm" ) + theme_bw( ),
    ggplot( d01m, aes( bmi, log10CT ) ) + geom_point( ) + geom_smooth( method = "lm" ) + theme_bw( ),
    cols = 2 )

# Bauen wir unsere eigenen SDS-Werte
d01m$bmi_sds <-
    ( lm( bmi ~ age, d01m ) )$residuals

d01m$bmi_sds <-
    d01m$bmi_sds / sd( d01m$bmi_sds )

ggsubplot(
    ggplot( d01m, aes( age, bmi.sds ) ) + geom_point( ) + geom_smooth( method = "lm" ) + theme_bw( ) + ggtitle( "altersabhaengig" ),
    ggplot( d01m, aes( age, bmi_sds ) ) + geom_point( ) + geom_smooth( method = "lm" ) + theme_bw( ) + ggtitle( "nicht altersabhaengig" ),
    ggplot( d01m, aes( bmi.sds, log10CT ) ) + geom_point( ) + geom_smooth( method = "lm" ) + theme_bw( ) + ggtitle( "altersabhaengig" ),
    ggplot( d01m, aes( bmi_sds, log10CT ) ) + geom_point( ) + geom_smooth( method = "lm" ) + theme_bw( ) + ggtitle( "nicht altersabhaengig" ),
    cols = 2 )

summary( 
    lm.log10CT.bmi.sds <-
        lm( log10CT ~ bmi.sds, d01m ) )
summary( 
    lm.log10CT.bmi_sds <-
        lm( log10CT ~ bmi_sds, d01m ) )

etaSquared( lm.log10CT.bmi.sds )
etaSquared( lm.log10CT.bmi_sds )

rcorr( d01m$age, d01m$bmi.sds ) # BMI.SDS korreliert nur schwach mit dem Alter
rcorr( d01m$age, d01m$bmi_sds ) # BMI_SDS korreliert gar nicht mit dem Alter


#####################################

summary( 
    lm.log10CT.bmi <-
        lm( log10CT ~ bmi, d01m ) )

etaSquared( lm.log10CT.bmi )

rcorr( d01m$age, d01m$bmi ) # BMI korreliert schwach mit dem Alter

ggsubplot(
    ggplot( d01m, aes( age, bmi ) ) + geom_point( ) + geom_smooth( method = "lm" ) + theme_bw( ),
    ggplot( d01m, aes( bmi, log10CT ) ) + geom_point( ) + geom_smooth( method = "lm" ) + theme_bw( ),
    cols = 2 )

#####################################

summary( 
    lm.log10CT.age.plus.height.sds <- 
        lm( log10CT ~ age + height.sds, d01m ) )

etaSquared( lm.log10CT.age.plus.height.sds )
#Größe-adj nicht signifikant

#####################################
# eigene Height-SDS bauen
d01m$height_sds <-
    ( lm( height ~ age, d01m ) )$residuals

d01m$height_sds <-
    d01m$height_sds / sd( d01m$height_sds )

ggsubplot(
    ggplot( d01m, aes( age, height.sds ) ) + geom_point( ) + geom_smooth( method = "lm" ) + theme_bw( ) + ggtitle( "altersabhaengig" ),
    ggplot( d01m, aes( age, height_sds ) ) + geom_point( ) + geom_smooth( method = "lm" ) + theme_bw( ) + ggtitle( "nicht altersabhaengig" ),
    ggplot( d01m, aes( height.sds, log10CT ) ) + geom_point( ) + geom_smooth( method = "lm" ) + theme_bw( ) + ggtitle( "altersabhaengig" ),
    ggplot( d01m, aes( height_sds, log10CT ) ) + geom_point( ) + geom_smooth( method = "lm" ) + theme_bw( ) + ggtitle( "nicht altersabhaengig" ),
    cols = 2 )

summary( 
    lm.log10CT.height.sds <-
        lm( log10CT ~ height.sds, d01m ) )

summary( 
    lm.log10CT.height_sds <-
        lm( log10CT ~ height_sds, d01m ) )

etaSquared( lm.log10CT.height.sds )
etaSquared( lm.log10CT.height_sds )

rcorr( d01m$age, d01m$height.sds ) # HEIGHT.SDS korreliert schwach mit dem Alter
rcorr( d01m$age, d01m$height_sds ) # HEIGHT_SDS korreliert gar nicht mit dem Alter













#also weiter mit Alter und BMI-SDS


linreg7 <- lm(d01m$logCT_S_1_NUM_VALUE ~ d01m$AGE_Calcitionin + d01m$C_ANTHRO_KH_BMI_ADJ + d01m$PTH_S_NUM_VALUE)
summary(linreg7)
#PTH nicht signifikant -> rausschmeißen


d01m.c.aus <- d01m[ d01m$CA_S_NUM_VALUE != 1.93, ]
linreg8 <- lm(d01m.c.aus$logCT_S_1_NUM_VALUE ~ d01m.c.aus$AGE_Calcitionin + d01m.c.aus$C_ANTHRO_KH_BMI_ADJ + d01m.c.aus$CA_S_NUM_VALUE)
summary(linreg8)
#Calcium ist signifikant, R-squared: 0.2987, adjusted R-suared 0.2909

linreg8 <- lm(d01m$logCT_S_1_NUM_VALUE ~ d01m$AGE_Calcitionin + d01m$C_ANTHRO_KH_BMI_ADJ + d01m$VDT_OH_S_NUM_VALUE)
summary(linreg8)
#Vitamin D ist nicht signifikant

# mit Calcium weitermachen
d01m.pc.aus <- d01m[ d01m$P1NP_S_NUM_VALUE != 5515 & d01m$CA_S_NUM_VALUE != 1.93, ]
d01m.p.aus <- d01m[ d01m$P1NP_S_NUM_VALUE != 5515, ]

linreg9 <- lm(d01m.pc.aus$logCT_S_1_NUM_VALUE ~ d01m.pc.aus$AGE_Calcitionin + d01m.pc.aus$C_ANTHRO_KH_BMI_ADJ + d01m.pc.aus$P1NP_S_NUM_VALUE + d01m.pc.aus$CA_S_NUM_VALUE)
summary(linreg9)
#P1NP ist signifikant, R-squared: 0.3074, adjusted R-squared 0.2967

#P1NP und Osteocalcin
linreg9.5 <- lm(d01m.pc.aus$logCT_S_1_NUM_VALUE ~ d01m.pc.aus$AGE_Calcitionin + d01m.pc.aus$C_ANTHRO_KH_BMI_ADJ + d01m.pc.aus$P1NP_S_NUM_VALUE + d01m.pc.aus$CA_S_NUM_VALUE + d01m.pc.aus$OSTEO_S_NUM_VALUE)
summary(linreg9.5)
#nicht signifikant


#ohne den Ausreißer bei P1NP
linreg10 <- lm(d01m.c.aus$logCT_S_1_NUM_VALUE ~ d01m.c.aus$AGE_Calcitionin + d01m.c.aus$C_ANTHRO_KH_BMI_ADJ + d01m.c.aus$P1NP_S_NUM_VALUE + d01m.c.aus$CA_S_NUM_VALUE)
summary(linreg10)
# immer noch signifikant, R-squared: 0.3074, adjusted R-squared 0.2933

linreg11 <- lm(d01m.c.aus$logCT_S_1_NUM_VALUE ~ d01m.c.aus$AGE_Calcitionin + d01m.c.aus$C_ANTHRO_KH_BMI_ADJ + d01m.c.aus$OSTEO_S_NUM_VALUE + d01m.c.aus$CA_S_NUM_VALUE)
summary(linreg11)
#Osteocalcin ist ganz knapp nicht signifikant (0.0504), R-squared: 0.3073, adjusted R-squared 0.2968

linreg12 <- lm(d01m.pc.aus$logCT_S_1_NUM_VALUE ~ d01m.pc.aus$AGE_Calcitionin + d01m.pc.aus$C_ANTHRO_KH_BMI_ADJ + d01m.pc.aus$P1NP_S_NUM_VALUE + d01m.pc.aus$ZIGF_S_2_NUM_VALUE + d01m.pc.aus$CA_S_NUM_VALUE )
summary(linreg12)
# nicht signifikant

linreg13 <- lm(d01m.pc.aus$logCT_S_1_NUM_VALUE ~ d01m.pc.aus$AGE_Calcitionin + d01m.pc.aus$C_ANTHRO_KH_BMI_ADJ + d01m.pc.aus$P1NP_S_NUM_VALUE + d01m.pc.aus$IGF1_S_2_NUM_VALUE + d01m.pc.aus$CA_S_NUM_VALUE)
summary(linreg13)
# nicht signifikant



linreg16 <- lm(d01m.c.aus$logCT_S_1_NUM_VALUE ~ d01m.c.aus$AGE_Calcitionin + d01m.c.aus$C_ANTHRO_KH_BMI_ADJ + d01m.c.aus$CA_S_NUM_VALUE + d01m.c.aus$OSTEO_S_NUM_VALUE + d01m.c.aus$ZIGF_S_2_NUM_VALUE)
summary(linreg16)
# auch bei Osteocalcin keine Signifikanz wenn IGF1/-SDS dazukommen


#bestes Modell: Alter, BMI-SDS?? oder da dann doch lieber BMI nehmen??, Calcium, P1NP oder Osteocalcin?
#vllt nochmal die einzelnen R-Quadrate anzeigen lassen