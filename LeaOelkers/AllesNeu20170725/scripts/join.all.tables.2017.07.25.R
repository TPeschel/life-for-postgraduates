##
# loesche speicher
##
rm( list = ls( ) )

# installiere devtools, falls noch nicht geschehen
if( !"devtools" %in% rownames( installed.packages( ) ) ) install.packages( "devtools" )

# installiere neueste Version von helperForLife, falls noch nicht geschehen
devtools::install_github( "TPeschel/hlpr4life" )

# lade hlpr4life
library( hlpr4life )

load.pkgs(
    c(
        "dplyr",
        "reshape",
        "readxl",
        "openxlsx" ) )


# setze Zeitzobe auf Berlin Mean Time
Sys.setenv( TZ = "BMT" ) 

##
# setze Pfad zu aktuellem R-Arbeitsverzeichnis, Pfad zum Laden
setwd( "~/LIFE/life-for-postgraduates/LeaOelkers/AllesNeu20170725/data/original/" )
#setwd( "c:/Users/Lea/Desktop/DA/LIFE-Daten/data/" )

##
# liste Verzeichnis auf, um zu schauen, welche Tabellen vorhanden sind
dir( )

##
# Derivattabelle: C_ANTHRO_KH (D00040) Letzte Änderung: 09.12.2013
# DQP-ID: Druckdatum: 05.08.2016
# DQP_22_0027_20131128
# Beschreibung: Das Assessment enthält Anthropometriedaten (Körperhöhe, Gewicht, BMI) für Säuglinge und
# Kinder zusammen mit SDS-Werten. Dazu werden die SDS-Tabellen von Kromeyer-Hauschild
# (2001) verwendet.
##
d040 <-
    read_excel( "PV0298_D00040_NODUP.xlsx" )

##
# Derivattabelle: C_ANTHRO (D00043) Letzte Änderung: 05.03.2014
# DQP-ID: Druckdatum: 05.08.2016
# DQP_22_0033_20140305
# Beschreibung: SDS-Transformationen für verschiedene anthropometrische Skalen (A2)
##
# d043 <-
#     read_excel( "PV0298_D00043_NODUP.xlsx" )
    
##
# Derivattabelle: C_ANT_S (D00066) Letzte Änderung: 14.03.2014
# DQP-ID: Druckdatum: 05.08.2016
# DQP_22_0044_20140317
# Beschreibung: SDS-Transformationen zu anthropometrischen Maßen für Säuglinge (A2)
##
# d066 <-
#     read_excel( "PV0298_D00066_NODUP.xlsx" )
    
##
# Derivattabelle: C_PUB_STAT (D00077) Letzte Änderung: 06.05.2014
# DQP-ID: Druckdatum: 05.08.2016
# DQP_22_0037_20140430
# Beschreibung: Einteilung des Geschlechts in männlich und weiblich.
# Beschreibung der Pubertätsentwicklung gemessen an der Entwicklung des äußeren Genitale der
# Probanden. Das angewandte Instrument sind die Tanner-Stadien. Bei männlichen Probanden wird
# die Entwicklung des äußeren Genitale in 5 Stufen unterteilt (G1-G5). Bei weiblichen Probanden
# erfolgt die Einteilung der Brustentwicklung ebenfalls in 5 Stufen (B1-B5). Bei beiden
# Geschlechtern wird die Ausprägung der Schambehaarung durch 6 Entwicklungsstufen (P1-P6)
# charakterisiert. Bei Jungen Angabe der Hodenvolumen rechts und links in ml. Bei Mädchen wird
# das Einsetzen der Regelblutung abgefragt. Informant: Kinder, Altersbereich: 0-20,5 Jahre
##
d077 <-
    read_excel( "PV0298_D00077_NODUP.xlsx" )
    
##
# Derivattabelle: C_SOZDEM (D00078) Letzte Änderung: 02.06.2014
# DQP-ID: Druckdatum: 05.08.2016
# DQP_22_0058_20140602
# Beschreibung: Soziodemographie (A2)
##
## WIRD VERMUTLICH DURCH D00177 ERSETZT
##
# d078 <-
#     read_excel( "PV0298_D00078_NODUP.xlsx" )

##   
# Derivattabelle: C_DISEASE_TX (D00127) Letzte Änderung: 09.04.2015
# DQP-ID: Druckdatum: 05.08.2016
# DQP_22_0106_20150408
# Beschreibung: Krankheiten von Kindern (Vorhandensein von Freitextangaben), die Beschreibung der
# Krankheitsklassen erfolgt mit ausgewählten Freitexten
##
d127 <-
    read_excel( "PV0298_D00127_NODUP.xlsx" )
    
##
# Derivattabelle: C_SOZ_WI (D00128) Letzte Änderung: 09.04.2015
# DQP-ID: Druckdatum: 05.08.2016
# DQP_22_0107_20150409
# Beschreibung: Winkler-Index auf Grundlage der Soziodemographie.
# Winkler, J., Stolzenberg, H., 2009. Adjustierung des Sozialen-Schicht-Index für die Anwendung im
# Kinder-und Jugendgesundheitssurvey (KiGGS). Wismarer Diskussionspapiere.
# Boyce, W., Torsheim, T., Currie, C., Zambon, A., 2006. The Family Affluence Scale as a Measure
# of National Wealth: Validation of an Adolescent Self-Report Measure. Social Indicators Research
# 78, 473–487. doi:10.1007/s11205-005-1607-6
##
# d128 <-
#     read_excel( "PV0298_D00128_NODUP.xlsx" )
    
##
# Derivattabelle: CHILD_MED_H (D00129) Letzte Änderung: 16.04.2015
# DQP-ID: Druckdatum: 05.08.2016
# DQP_22_0108_20150420
# Beschreibung: Medikamentenanamnese (A2) horizontal
# Hinweis: Im Feld D00129_ATC_CODE bedeutet der Eintrag "Z" eine Angabe, die mittels ATC-Code
# nicht zugeordnet werden konnte.
# Code Beschreibung Datentyp
##
d129 <-
    read_excel( "PV0298_D00129_NODUP.xlsx" )

##
# Derivattabelle: C_WINKLER (D00177) Letzte Änderung: 07.02.2017
# DQP-ID: Druckdatum: 08.06.2017
# DQP_22_0160_20161121
# Beschreibung: Winkler-Index, basierend auf D00204. Es wurde pro Jahr und pro Familie ein Score ermittelt. Alle
# SICs eine Familie und einen Score zugewiesen.
# Lampert, Thomas, et al. "Messung des sozioökonomischen Status in der Studie zur Gesundheit
# Erwachsener in Deutschland (DEGS1)." Bundesgesundheitsblatt-Gesundheitsforschung-
# Gesundheitsschutz 56.5-6 (2013): 631-636.
##
d177 <-
    read_excel( "PV0298_D00177.xlsx" )

##
# Zieltabelle:
# Teilnehmer (R00001)
# Letzte Änderung: 29.02.2016
# Druckdatum: 05.08.2016
# Beschreibung: Das Assessment enthält Daten zum Teilnehmer, die bei Aufnahme in die LIFE-Studie festgestellt
# werden.
##
r001 <-
    read_excel( "PV0298_R00001.xlsx" )
    
##
# Zieltabelle:
# FSH_S (T00487)
# Letzte Änderung: 07.06.2016
# Druckdatum: 05.08.2016
# Beschreibung: Bestimmung von Follikelstimulierendem Hormon im Serum von Kindern und Erwachsenen im
# Altersbereich 0-79 Jahre.
# Maßeinheit: U/l
# Anwendungszweck: Reproduktion
# Messprinzip: ECLIA
# Gerät / Firma: Cobas / Roche
##
t487 <-
    read_excel( "PV0298_T00487_NODUP.xlsx" )
    
##
# Zieltabelle:
# E2_S (T00488)
# Letzte Änderung: 24.05.2016
# Druckdatum: 05.08.2016
# Beschreibung: Bestimmung von Estradiol im Serum von Kindern und Erwachsenen im Altersbereich 0-79 Jahre.
# Maßeinheit: pmol/l
# Anwendungszweck: Reproduktion
# Messprinzip: ECLIA
# Gerät / Firma: Cobas / Roche
# Bemerkungen: Generationswechsel zum 14.07.2015 in T00894
##
t488 <-
    read_excel( "PV0298_T00488_NODUP.xlsx" )

##
# Zieltabelle:
# SHBG_S (T00489)
# Letzte Änderung: 07.06.2016
# Druckdatum: 05.08.2016
# Beschreibung: Bestimmung von SHBG im Serum von Kindern und Erwachsenen im Altersbereich 0-79 Jahre.
# Maßeinheit: nmol/l
# Anwendungszweck: Reproduktion
# Messprinzip: ECLIA
# Gerät / Firma: Cobas / Roche
##
t489 <-
    read_excel( "PV0298_T00489_NODUP.xlsx" )
    
##
# Zieltabelle:
# DHEAS_S (T00490)
# Letzte Änderung: 03.06.2016
# Druckdatum: 05.08.2016
# Beschreibung: Bestimmung von DHEAS-S im Serum von Kindern und Erwachsenen im Altersbereich 0-79 Jahre.
# Maßeinheit: μmol/l
# Anwendungszweck: Nebennierendiagnostik
# Messprinzip: ECLIA
# Gerät / Firma: Cobas / Roche
# Bemerkungen: 1. Generation bis 31.03.2016
##
t490 <-
    read_excel( "PV0298_T00490_NODUP.xlsx" )
    
##
# Zieltabelle:
# LH_S (T00508)
# Letzte Änderung: 16.12.2015
# Druckdatum: 05.08.2016
# Beschreibung: Bestimmung von LH im Serum von Kindern und Erwachsenen im Altersbereich 0-79 Jahre.
# Maßeinheit: U/l
# Anwendungszweck: Reproduktionsdiagnostik
# Messprinzip: ECLIA
# Gerät / Firma: Cobas / Roche
##
t508 <-
    read_excel( "PV0298_T00508_NODUP.xlsx" )
    
##
# Zieltabelle:
# TESTO_S (T00509)
# Letzte Änderung: 16.12.2015
# Druckdatum: 05.08.2016
# Beschreibung: Bestimmung von Testosteron im Serum von Kindern und Erwachsenen im Altersbereich 0-79
# Jahre.
# Maßeinheit: nmol/l
# Anwendungszweck: Reproduktionsdiagnostik
# Messprinzip: ECLIA
# Gerät / Firma: Cobas / Roche
##
t509 <-
    read_excel( "PV0298_T00509_NODUP.xlsx" )


# t508(LH), t487(FSH)
tbl <-
    merge(
        t487,
        t508,
        by.x = c( "FSH_S_SIC", "FSH_S_GRUPPE" ),
        by.y = c( "LH_S_SIC", "LH_S_GRUPPE" ),
        all = T )

sapply( tbl, function( col ) sum( is.na( col ) ) )
sapply( tbl, function( col ) sum( !is.na( col ) ) )

tbl <-
    rename.columns( tbl, c( "FSH_S_SIC", "FSH_S_GRUPPE", "FSH_S_DATUM" ), c( "SIC", "SCI_GROUP", "EDAT" ) )
        
table( tbl$SCI_GROUP )

# depressive Kinder fliegen erstmal raus    
tbl <-
    tbl[ tbl$SCI_GROUP != "Z00", ]


# t508(LH), t487(FSH), d040(ANTHRO)
tbl <-
    merge( 
        tbl,
        d040,
        by.x = c( "SIC", "SCI_GROUP" ),
        by.y = c( "C_ANTHRO_KH_SIC", "C_ANTHRO_KH_GRP" ),
        all.x = T )

sapply( tbl, function( col ) sum( is.na( col ) ) )
sapply( tbl, function( col ) sum( !is.na( col ) ) )


# t508(LH), t487(FSH), d040(ANTHRO), r001(Stammdaten)
tbl <-
    merge(
        tbl,
        r001,
        by.x = c( "SIC" ),
        by.y = c( "TEILNEHMER_SIC" ),
        all.x = T )

sapply( tbl, function( col ) sum( is.na( col ) ) )
sapply( tbl, function( col ) sum( !is.na( col ) ) )


# t508(LH), t487(FSH), d040(ANTHRO), r001(Stammdaten), d077(PubStat)
tbl <-
    merge(
        tbl,
        d077,
        by.x = c( "SIC", "SCI_GROUP" ),
        by.y = c( "C_PUB_STAT_SIC", "C_PUB_STAT_GRUPPE" ),
        all.x = T)

sapply( tbl, function( col ) sum( is.na( col ) ) )
sapply( tbl, function( col ) sum( !is.na( col ) ) )

# zu 9 sics gibt es keinen Winkler
sum( !tbl$SIC %in% d177$PSEUDONYM )

# fuege Spalte hinzu, die nur das Jahr des EDATs enthaelt 
tbl$Y <-
    year( tbl$EDAT )

# t508(LH), t487(FSH), d040(ANTHRO), r001(Stammdaten), d077(PubStat), d177(Winkler)
tbl <-
    merge(
        tbl,
        d177,
        by.x = c( "SIC", "Y" ),
        by.y = c( "PSEUDONYM", "D00177_JAHR" ),
        all.x = T )

sapply( tbl, function( col ) sum( is.na( col ) ) )
sapply( tbl, function( col ) sum( !is.na( col ) ) )


# t508(LH), t487(FSH), d040(ANTHRO), r001(Stammdaten), d077(PubStat), d177(Winkler), d127(Krankheiten)
tbl <-
    merge(
        tbl,
        d127,
        by.x = c( "SIC", "SCI_GROUP" ),
        by.y = c( "C_DISEASE_TX_SIC", "C_DISEASE_TX_SCI_GROUP" ),
        all.x = T )

sapply( tbl, function( col ) sum( is.na( col ) ) )
sapply( tbl, function( col ) sum( !is.na( col ) ) )


# t508(LH), t487(FSH), d040(ANTHRO), r001(Stammdaten), d077(PubStat), d177(Winkler), d127(Krankheiten), d129(Medikamente)
tbl <-
    merge(
        tbl,
        d129,
        by.x = c( "SIC", "SCI_GROUP" ),
        by.y = c( "CHILD_MED_H_SIC", "CHILD_MED_H_SCI_GROUP" ),
        all.x = T )

sapply( tbl, function( col ) sum( is.na( col ) ) )
sapply( tbl, function( col ) sum( !is.na( col ) ) )


# t508(LH), t487(FSH), d040(ANTHRO), r001(Stammdaten), d077(PubStat), d177(Winkler), d127(Krankheiten), d129(Medikamente), t509(Testosteron)
tbl <-
    merge(
        tbl,
        t509,
        by.x = c( "SIC", "SCI_GROUP" ),
        by.y = c( "TESTO_S_SIC", "TESTO_S_GRUPPE" ),
        all.x = T )

sapply( tbl, function( col ) sum( is.na( col ) ) )
sapply( tbl, function( col ) sum( !is.na( col ) ) )


# t508(LH), t487(FSH), d040(ANTHRO), r001(Stammdaten), d077(PubStat), d177(Winkler), d127(Krankheiten), d129(Medikamente), t509(Testosteron), t488(Estradiol)
tbl <-
    merge(
        tbl,
        t488,
        by.x = c( "SIC", "SCI_GROUP" ),
        by.y = c( "E2_S_SIC", "E2_S_GRUPPE" ),
        all.x = T )

sapply( tbl, function( col ) sum( is.na( col ) ) )
sapply( tbl, function( col ) sum( !is.na( col ) ) )

# t508(LH), t487(FSH), d040(ANTHRO), r001(Stammdaten), d077(PubStat), d177(Winkler), d127(Krankheiten), d129(Medikamente), t509(Testosteron), t488(Estradiol), t489(SHBG)
tbl <-
    merge(
        tbl,
        t489,
        by.x = c( "SIC", "SCI_GROUP" ),
        by.y = c( "SHBG_S_SIC", "SHBG_S_GRUPPE" ),
        all.x = T )

sapply( tbl, function( col ) sum( is.na( col ) ) )
sapply( tbl, function( col ) sum( !is.na( col ) ) )


# t508(LH), t487(FSH), d040(ANTHRO), r001(Stammdaten), d077(PubStat), d177(Winkler), d127(Krankheiten), d129(Medikamente), t509(Testosteron), t488(Estradiol), t489(SHBG), t490(DHEAS)
tbl <-
    merge(
        tbl,
        t490,
        by.x = c( "SIC", "SCI_GROUP" ),
        by.y = c( "DHEAS_S_SIC", "DHEAS_S_GRUPPE" ),
        all.x = T )

sapply( tbl, function( col ) sum( is.na( col ) ) )
sapply( tbl, function( col ) sum( !is.na( col ) ) )

dates <-
    names( tbl )[ grep( "EDAT|DATUM", names( tbl ) ) ]

dates <-
    dates[ dates != "EDAT.x" ]

sci_grps <-
    names( tbl )[ grep( "SCI|GROUP", names( tbl ) ) ]

sci_grps <-
    sci_grps[ sci_grps != "SCI_GROUP" ]

tbl <-
    tbl[ , setdiff( names( tbl ), c( "Y", dates, sci_grps ) ) ]

tbl <-
    rename.columns( 
        tbl,
        c( "EDAT.x", "TEILNEHMER_GESCHLECHT", "TEILNEHMER_GEB_JJJJMM", "C_ANTHRO_KH_AGE" ),
        c( "EDAT", "SEX", "AGE" ) )

sapply( tbl, function( col ) sum( is.na( col ) ) )
sapply( tbl, function( col ) sum( !is.na( col ) ) )

setwd( "~/LIFE/life-for-postgraduates/LeaOelkers/AllesNeu20170725/data/generated/" )

save( tbl, file = "main.table.Rd" )
openxlsx::write.xlsx( x = tbl, file = "main.table.xlsx" )
