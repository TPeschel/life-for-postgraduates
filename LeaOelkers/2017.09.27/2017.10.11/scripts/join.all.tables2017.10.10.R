##
# loesche speicher
##
rm( list = ls( ) )

# installiere devtools, falls noch nicht geschehen
if( !"devtools" %in% rownames( installed.packages( ) ) ) install.packages( "devtools" )

# installiere neueste Version von helperForLife, falls noch nicht geschehen
devtools::install_github( "TPeschel/hlpr4life" )

hlpr4life::load.pkgs(
    c(
        "hlpr4life",
        "dplyr",
        "reshape2",
        "readxl",
        "openxlsx",
        "lubridate") )   ##openxlsx ist viel schneller als xlsx, WriteXLS ist nicht gut


# setze Zeitzone auf Berlin Mean Time
Sys.setenv( TZ = "Europe/Berlin" ) 

# mehr infos bei ausgabe
options(max.print = 100000)

##
# setze Pfad zu aktuellem R-Arbeitsverzeichnis, Pfad zum Laden
setwd( "~/LIFE/life-for-postgraduates/LeaOelkers/2017.09.27/data/original/" )
# setwd( "c:/Users/Lea/Desktop/AllesNeu20170725/data/original/" )  ##Pfad in dem die original Excel tabl stehen

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


# Stimmbruchdaten
t976 <-
    read_excel( "PV0298_T00976_NODUP.xlsx")

t109 <-
    read_excel( "PV0298_T00109_NODUP.xlsx")

#  Geburtsgewicht
t139 <-
  read_excel( "PV0298_D00139U1.xlsx")

## Mergen

# t508(LH), t487(FSH)
tbl <-
    merge(
        t487,
        t508,
        by.x = c( "FSH_S_SIC", "FSH_S_GRUPPE" ),
        by.y = c( "LH_S_SIC", "LH_S_GRUPPE" ),
        all = T )

t( table.df( tbl ) ) ##zeigt anzahl Nas und availables an

tbl <-
    rename.columns( tbl, c( "FSH_S_SIC", "FSH_S_GRUPPE", "LH_S_DATUM" ), c( "SIC", "SCI_GROUP", "EDAT" ) )
        
table( tbl$SCI_GROUP )

# jetzt ueberfluessig
# depressive Kinder fliegen erstmal raus    
# sind eigentlich gar keine mehr drin (O;)
# tbl <-
#     tbl[ tbl$SCI_GROUP != "Z00", ]

# t508(LH), t487(FSH), d040(ANTHRO)
tbl <-
    merge( 
        tbl,
        d040,
        by.x = c( "SIC", "SCI_GROUP" ),
        by.y = c( "C_ANTHRO_KH_SIC", "C_ANTHRO_KH_GRP" ),
        all.x = T )
#4722
table.df( tbl )

# t508(LH), t487(FSH), d040(ANTHRO), r001(Stammdaten)
tbl <-
    merge(
        tbl,
        r001,
        by.x = c( "SIC" ),
        by.y = c( "TEILNEHMER_SIC" ),
        all.x = T )
#4722
table.df( tbl )

tbl <-
    rename.columns( 
        tbl, 
        c( 
            "C_ANTHRO_KH_AGE",
            "C_ANTHRO_KH_HEIGHT_ORIG",
            "C_ANTHRO_KH_WEIGHT_ORIG",
            "C_ANTHRO_KH_BMI_ORIG",
            "C_ANTHRO_KH_HEIGHT_ADJ",
            "C_ANTHRO_KH_WEIGHT_ADJ",
            "C_ANTHRO_KH_BMI_ADJ",
            "TEILNEHMER_GESCHLECHT",
            "TEILNEHMER_GEB_JJJJMM" ),
        c( 
            "AGE",
            "HEIGHT",
            "WEIGHT",
            "BMI",
            "HEIGHT.ADJ",
            "WEIGHT.ADJ",
            "BMI.ADJ",
            "SEX",
            "BIRTHDAY") )

table.df( tbl )

# schmeisse alle ohne alter und groesse und gewicht raus, 20 leute
tbl <-
    tbl[ !is.na( tbl$AGE ) & !is.na( tbl$HEIGHT ) & !is.na( tbl$WEIGHT ) & !is.na( tbl$BMI ), ]
# 4702

table.df( tbl )

# t508(LH), t487(FSH), d040(ANTHRO), r001(Stammdaten), d077(PubStat)
tbl <-
    merge(
        tbl,
        d077,
        by.x = c( "SIC", "SCI_GROUP" ),
        by.y = c( "C_PUB_STAT_SIC", "C_PUB_STAT_GRUPPE" ),
        all.x = T)
# 4702

table.df( tbl )

# fuege Spalte hinzu, die nur das Jahr des EDATs enthaelt 
tbl$Y <-
    year( tbl$EDAT )

# haben wir zu jedem ein edat?
sum( is.na( tbl$EDAT ) ) 
# ja

# t508(LH), t487(FSH), d040(ANTHRO), r001(Stammdaten), d077(PubStat), d177(Winkler)

# zu 1 sic gibt es keinen Winkler
sum( !tbl$SIC %in% d177$PSEUDONYM )

tbl <-
    merge(
        tbl,
        d177,
        by.x = c( "SIC", "Y" ),
        by.y = c( "PSEUDONYM", "D00177_JAHR" ),
        all.x = T ) ##4707 = F
# 4702
sics <-
    tbl$SIC[ is.na( tbl$FAM_PSEUDO ) ] ##15 sics ohne fam-ID

tbl[ is.na( tbl$FAM_PSEUDO ), ] 

sci.grp <-
    tbl$SCI_GROUP[ is.na( tbl$FAM_PSEUDO ) ] 

# 15 kinder haben keinen besuch in der d177, alle sind unterschiedlich
length( unique( sics ) )

# wirf den einen raus, den es in der d177 nicht gibt!
sics <-
    intersect( sics, d177$PSEUDONYM )

# 14 kinder haben keinen besuch in der d177
length( unique( sics ) )

# hole alle eintraege zu diesen sics aus der d177
d177.sub <-
    d177[ d177$PSEUDONYM %in% sics, ]

length( unique( d177.sub$PSEUDONYM ) )

# zaehle missings pro zeilen und vermerke dies in der Spalte Num.Of.Missings, bei welchem besuch sind die meisten Eintraege
d177.sub$Num.Of.Missings <-
  sapply( 1 : nrow( d177.sub ), function( z ) sum( is.na( d177.sub[ z , ] ) ) )

# suche Zeile mit det geringsten Anzahl an Missings und 
# vemerke das dazugehoerige Jahr in der Spalte "J"
d177.sub %<>%
    group_by( PSEUDONYM ) %>%
    mutate( J = D00177_JAHR[ which.min( Num.Of.Missings ) ] ) 

# sollten 14 sein
nrow( d177.sub <- d177.sub[ d177.sub$D00177_JAHR == d177.sub$J, ] )

# ermittle Zeilen in (original)tbl, zu fehlenden Werten in tbl
tbl.rows <-
    which( tbl$SIC %in% sics & is.na( tbl$D00177_SCORE_FAM ) )

# ermittle Zeilen in d177.sub, zu fehlenden Werten in tbl, Zeieln des Winkler die in original fehlen, werden jetzt zur original tbl zugefügt
d177.rows <-
    which( d177.sub$PSEUDONYM %in% sics )

# ermittle namen der fehlenden Werte: diese zeilen werden aus der 177 geholt und in original tbl zugefügt
names.d177 <-
    intersect( names( tbl ), names( d177.sub ) )

# ersetze fehlende Werte in tbl
for( i in 1 : length( tbl.rows ) ) {
    
    tbl[ tbl.rows[ i ], names.d177 ] <-
        d177.sub[ d177.rows[ i ], names.d177 ]
}

# schmeiss den einen raus, der keine  FamPseudo hat
tbl <-
    tbl[ !is.na( tbl$FAM_PSEUDO ), ]

# 4701

# zeige resultat
t( table.df( tbl ) )

# benenne EDAT.x in EDAT um
tbl<- 
    rename.columns( tbl, "EDAT.x", "EDAT" ) #EDAT.x wird in EDAT umbenannt

# loesche EDAT.y
tbl <-
    remove.columns( tbl, "EDAT.y" )

# t508(LH), t487(FSH), d040(ANTHRO), r001(Stammdaten), d077(PubStat), d177(Winkler), d127(Krankheiten)
tbl <-
    merge(
        tbl,
        d127,
        by.x = c( "SIC", "SCI_GROUP" ),
        by.y = c( "C_DISEASE_TX_SIC", "C_DISEASE_TX_SCI_GROUP" ),
        all.x = T )
# 4701
t( table.df( tbl ) )

# t508(LH), t487(FSH), d040(ANTHRO), r001(Stammdaten), d077(PubStat), d177(Winkler), d127(Krankheiten), d129(Medikamente)
tbl <-
    merge(
        tbl,
        d129,
        by.x = c( "SIC", "SCI_GROUP" ),
        by.y = c( "CHILD_MED_H_SIC", "CHILD_MED_H_SCI_GROUP" ),
        all.x = T )
# 4701
t( table.df( tbl ) )

# t508(LH), t487(FSH), d040(ANTHRO), r001(Stammdaten), d077(PubStat), d177(Winkler), d127(Krankheiten), d129(Medikamente), t509(Testosteron)
tbl <-
    merge(
        tbl,
        t509,
        by.x = c( "SIC", "SCI_GROUP" ),
        by.y = c( "TESTO_S_SIC", "TESTO_S_GRUPPE" ),
        all.x = T )
# 4701

table.df( tbl )

# t508(LH), t487(FSH), d040(ANTHRO), r001(Stammdaten), d077(PubStat), d177(Winkler), d127(Krankheiten), d129(Medikamente), t509(Testosteron), t488(Estradiol)
tbl <-
    merge(
        tbl,
        t488,
        by.x = c( "SIC", "SCI_GROUP" ),
        by.y = c( "E2_S_SIC", "E2_S_GRUPPE" ),
        all.x = T )

# 4701
table.df( tbl )

# t508(LH), t487(FSH), d040(ANTHRO), r001(Stammdaten), d077(PubStat), d177(Winkler), d127(Krankheiten), d129(Medikamente), t509(Testosteron), t488(Estradiol), t489(SHBG)
tbl <-
    merge(
        tbl,
        t489,
        by.x = c( "SIC", "SCI_GROUP" ),
        by.y = c( "SHBG_S_SIC", "SHBG_S_GRUPPE" ),
        all.x = T )
# 4701

table.df( tbl )

# t508(LH), t487(FSH), d040(ANTHRO), r001(Stammdaten), d077(PubStat), d177(Winkler), d127(Krankheiten), d129(Medikamente), t509(Testosteron), t488(Estradiol), t489(SHBG), t490(DHEAS)
tbl <-
    merge(
        tbl,
        t490,
        by.x = c( "SIC", "SCI_GROUP" ),
        by.y = c( "DHEAS_S_SIC", "DHEAS_S_GRUPPE" ),
        all.x = T )

# 4701

table.df( tbl )

# t508(LH), t487(FSH), d040(ANTHRO), r001(Stammdaten), d077(PubStat), d177(Winkler), d127(Krankheiten),
# d129(Medikamente), t509(Testosteron), t488(Estradiol), t489(SHBG), t490(DHEAS), t976(Stimmbruch)
tbl <-
    merge(
        tbl,
        t976[, c( "FB_KS_SIC", "FB_KS_GRUPPE", "FB_KS_STIMMBRUCH", "FB_KS_STIMM_ALTER" )],
        by.x = c( "SIC", "SCI_GROUP" ),
        by.y = c( "FB_KS_SIC", "FB_KS_GRUPPE" ),
        all.x = T )

# 4701

table.df( tbl )
table( tbl[ , get.columns( tbl, "stimm" ) ] )

# t508(LH), t487(FSH), d040(ANTHRO), r001(Stammdaten), d077(PubStat), d177(Winkler), d127(Krankheiten),
# d129(Medikamente), t509(Testosteron), t488(Estradiol), t489(SHBG), t490(DHEAS), t976(Stimmbruch), t109(stimmbruch)
tbl <-
    merge(
        tbl,
        t109[, c( "FB_SK_CH_SIC", "FB_SK_CH_GRUPPE", "FB_SK_CH_F0011", "FB_SK_CH_F0012" )],  ##nur bestimmte Spalte von Stimmbruch ranhängen
        by.x = c( "SIC", "SCI_GROUP" ),
        by.y = c( "FB_SK_CH_SIC", "FB_SK_CH_GRUPPE" ),
        all.x = T )

# 4701

table.df( tbl )

# Merge mit geburtsgewicht
# Wir nehemn nur bestimmete Spalten mit rein.(EDAT raus, damit es nicht gedoppelt wird.)
tbl <-
  merge(
    tbl,
    t139[ , c( "PSEUDONYM", "D00139_U1_GEW", "D00139_U1_GROE", "D00139_SSW_W", "D00139_SSW_T" ) ],
    by.x = c( "SIC" ),
    by.y = c ("PSEUDONYM" ),
    all.x = T )

# 4701
table.df( tbl )

# Geburtsgewichts- und Stimmbruchspalten umbenennen
tbl <-
  rename.columns( 
    tbl,
    c( "D00139_U1_GEW", "D00139_U1_GROE", "FB_KS_STIMMBRUCH", "FB_KS_STIMM_ALTER", "FB_SK_CH_F0011", "FB_SK_CH_F0012" ),
    c( "Geburtsgewicht", "Geburtsgroesse", "StimmbruchGehabt1", "StimmbruchAlter1","StimmbruchGehabt2", "StimmbruchAlter2" ) )
 
# 4701

table.df( tbl )
#4496 Geburtsgewicht
#4442 Geburtsgroesse
#918  C_PUB_STAT_MENARCHE_WANN
#229  StimmbruchGehabt1 = FB_KS_STIMMBRUCH 
#117  Stimmbruchalter1 = FB_KS_STIMM_ALTER
#1402 StimmbruchGehabt2 = FB_SK_CH_F0011
#754  Stimmbruchalter2 = FB_SK_CH_F0012


table( tbl$StimmbruchAlter1 + 6 )
table( floor( tbl$StimmbruchAlter2 ) )

##Stimmbruchdaten: zusammenpacken, unn?tige Spalten aussortieren
#beide Stimmbruchdaten zusammenpacken
tbl$STIMMBRUCH_GEHABT <-
    tbl$StimmbruchGehabt1

tbl$STIMMBRUCH_GEHABT[ is.na( tbl$STIMMBRUCH_GEHABT ) ] <-
    tbl$StimmbruchGehabt2[ is.na( tbl$STIMMBRUCH_GEHABT ) ]  ##wenn in STIMMBRUCH_GEHABT nichts drin ist, werden die Werte aus StimmbruchGehabt2 genommen

tbl$STIMMBRUCH_ALTER <-
    tbl$StimmbruchAlter1 + 6

tbl$STIMMBRUCH_ALTER[ is.na( tbl$STIMMBRUCH_ALTER ) ] <-
    tbl$StimmbruchAlter2[ is.na( tbl$STIMMBRUCH_ALTER ) ]

table( tbl$STIMMBRUCH_ALTER )

sics <-
    tbl$SIC[ !is.na( tbl$STIMMBRUCH_ALTER ) & ( tbl$STIMMBRUCH_ALTER < 10 | 20 < tbl$STIMMBRUCH_ALTER ) ] ##Cut offs für Stimmbruch

nms <-
    c( "SIC", "SCI_GROUP", "AGE", "STIMMBRUCH_ALTER", "STIMMBRUCH_GEHABT", "StimmbruchAlter1", "StimmbruchAlter2", "StimmbruchGehabt1", "StimmbruchGehabt2" )

View( tbl[ tbl$SIC %in% sics, nms ] )  ##einige machen keinen Sinn, also alles einzeln durchgehen udn kurieren

# zu jung
tbl[ tbl$SIC == sics[ 1 ], nms ] ##alle sics die außerhalb der Range liegen (10-20y)
tbl$STIMMBRUCH_ALTER[ tbl$SIC == sics[ 1 ] ] <- NA
tbl$STIMMBRUCH_GEHABT[ tbl$SIC == sics[ 1 ] ] <- NA

# wahrscheinlich mit 15
tbl[ tbl$SIC == sics[ 2 ], nms ]
tbl$STIMMBRUCH_ALTER[ tbl$SIC == sics[ 2 ] ] <- 15    
tbl$STIMMBRUCH_GEHABT[ tbl$SIC == sics[ 2 ] ] <- 1  ##kuriert

# zu jung
tbl[ tbl$SIC == sics[ 3 ], nms ]
tbl$STIMMBRUCH_ALTER[ tbl$SIC == sics[ 3 ] ] <- NA
tbl$STIMMBRUCH_GEHABT[ tbl$SIC == sics[ 3 ] ] <- NA  

# sagen wir 13.5
tbl[ tbl$SIC == sics[ 4 ], nms ]
tbl$STIMMBRUCH_ALTER[ tbl$SIC == sics[ 4 ] ] <- 13.5
tbl$STIMMBRUCH_GEHABT[ tbl$SIC == sics[ 4 ] ] <- 1

# zu jung
tbl[ tbl$SIC == sics[ 5 ], nms ]
tbl$STIMMBRUCH_ALTER[ tbl$SIC == sics[ 5 ] ] <- NA
tbl$STIMMBRUCH_GEHABT[ tbl$SIC == sics[ 5 ] ] <- NA

# keine Ahnung
tbl[ tbl$SIC == sics[ 6 ], nms ]
tbl$STIMMBRUCH_ALTER[ tbl$SIC == sics[ 6 ] ] <- NA
tbl$STIMMBRUCH_GEHABT[ tbl$SIC == sics[ 6 ] ] <- NA

# keine Ahnung
tbl[ tbl$SIC == sics[ 7 ], nms ]
tbl$STIMMBRUCH_ALTER[ tbl$SIC == sics[ 7 ] ] <- NA
tbl$STIMMBRUCH_GEHABT[ tbl$SIC == sics[ 7 ] ] <- NA

# wahrscheinlich 14.1
tbl[ tbl$SIC == sics[ 8 ], nms ]
tbl$STIMMBRUCH_ALTER[ tbl$SIC == sics[ 8 ] ] <- 14.1
tbl$STIMMBRUCH_GEHABT[ tbl$SIC == sics[ 8 ] ] <- 1

# keine Ahnung
tbl[ tbl$SIC == sics[ 9 ], nms ]
tbl$STIMMBRUCH_ALTER[ tbl$SIC == sics[ 9 ] ] <- NA
tbl$STIMMBRUCH_GEHABT[ tbl$SIC == sics[ 9 ] ] <- NA

# ist erst 10 und wird mit 24.7 seinen Stimmbruch bekommen
tbl[ tbl$SIC == sics[ 10 ], nms ]
tbl$STIMMBRUCH_ALTER[ tbl$SIC == sics[ 10 ] ] <- NA
tbl$STIMMBRUCH_GEHABT[ tbl$SIC == sics[ 10 ] ] <- NA

# zu jung
tbl[ tbl$SIC == sics[ 11 ], nms ]
tbl$STIMMBRUCH_ALTER[ tbl$SIC == sics[ 11 ] ] <- NA
tbl$STIMMBRUCH_GEHABT[ tbl$SIC == sics[ 11 ] ] <- NA

# zu jung
tbl[ tbl$SIC == sics[ 12 ], nms ]
tbl$STIMMBRUCH_ALTER[ tbl$SIC == sics[ 12 ] ] <- NA
tbl$STIMMBRUCH_GEHABT[ tbl$SIC == sics[ 12 ] ] <- NA

# zu jung
tbl[ tbl$SIC == sics[ 13 ], nms ]
tbl$STIMMBRUCH_ALTER[ tbl$SIC == sics[ 13 ] ] <- NA
tbl$STIMMBRUCH_GEHABT[ tbl$SIC == sics[ 13 ] ] <- NA

# nur den letzten Besuch korrigieren
tbl[ tbl$SIC == sics[ 14 ], nms ]
tbl$STIMMBRUCH_ALTER[ tbl$SIC == sics[ 14 ] & tbl$AGE < 14 ] <- NA
tbl$STIMMBRUCH_GEHABT[ tbl$SIC == sics[ 14 ] & tbl$AGE < 14 ] <- NA

# keine Ahnung
tbl[ tbl$SIC == sics[ 15 ], nms ]
tbl$STIMMBRUCH_ALTER[ tbl$SIC == sics[ 15 ] ] <- NA
tbl$STIMMBRUCH_GEHABT[ tbl$SIC == sics[ 15 ] ] <- NA

# nehme, was zuerst angegeben wurde, passe auf Alter auf
tbl[ tbl$SIC == sics[ 16 ], nms ]
tbl$STIMMBRUCH_ALTER[ tbl$SIC == sics[ 16 ] & 12.5 < tbl$AGE ] <- 12.5
tbl$STIMMBRUCH_ALTER[ tbl$SIC == sics[ 16 ] & tbl$AGE < 12.5 ] <- NA
tbl$STIMMBRUCH_GEHABT[ tbl$SIC == sics[ 16 ] & 12.5 < tbl$AGE ] <- 1
tbl$STIMMBRUCH_GEHABT[ tbl$SIC == sics[ 16 ] & tbl$AGE < 12.5 ] <- 0

# nehme nur den letzten Besuch, damit Altersangaben zu Stimmbruch Sinn machen
tbl[ tbl$SIC == sics[ 17 ], nms ]
tbl$STIMMBRUCH_ALTER[ tbl$SIC == sics[ 17 ] & 13 < tbl$AGE ] <- 13.1
tbl$STIMMBRUCH_ALTER[ tbl$SIC == sics[ 17 ] & tbl$AGE < 13 ] <- NA
tbl$STIMMBRUCH_GEHABT[ tbl$SIC == sics[ 17 ] & 13 < tbl$AGE ] <- 1
tbl$STIMMBRUCH_GEHABT[ tbl$SIC == sics[ 17 ] & tbl$AGE < 13 ] <- 0

# nehme nur den letzten Besuch
tbl[ tbl$SIC == sics[ 18 ], nms ]
tbl$STIMMBRUCH_ALTER[ tbl$SIC == sics[ 18 ] & 15 < tbl$AGE ] <- 15
tbl$STIMMBRUCH_ALTER[ tbl$SIC == sics[ 18 ] & tbl$AGE < 15 ] <- NA
tbl$STIMMBRUCH_GEHABT[ tbl$SIC == sics[ 18 ] & 15 < tbl$AGE ] <- 1
tbl$STIMMBRUCH_GEHABT[ tbl$SIC == sics[ 18 ] & tbl$AGE < 15 ] <- 0

# keine Ahnung
tbl[ tbl$SIC == sics[ 19 ], nms ]
tbl$STIMMBRUCH_ALTER[ tbl$SIC == sics[ 19 ] ] <- NA
tbl$STIMMBRUCH_GEHABT[ tbl$SIC == sics[ 19 ] ] <- NA

# keine Ahnung
tbl[ tbl$SIC == sics[ 20 ], nms ]
tbl$STIMMBRUCH_ALTER[ tbl$SIC == sics[ 20 ] ] <- NA
tbl$STIMMBRUCH_GEHABT[ tbl$SIC == sics[ 20 ] ] <- NA

# kann nicht sein
tbl[ tbl$SIC == sics[ 21 ], nms ]
tbl$STIMMBRUCH_ALTER[ tbl$SIC == sics[ 21 ] ] <- NA
tbl$STIMMBRUCH_GEHABT[ tbl$SIC == sics[ 21 ] ] <- NA

# kann nicht sein
tbl[ tbl$SIC == sics[ 22 ], nms ]
tbl$STIMMBRUCH_ALTER[ tbl$SIC == sics[ 22 ] ] <- 13
tbl$STIMMBRUCH_GEHABT[ tbl$SIC == sics[ 22 ] ] <- 1

# kann nicht sein
tbl[ tbl$SIC == sics[ 23 ], nms ]
tbl$STIMMBRUCH_ALTER[ tbl$SIC == sics[ 23 ] ] <- NA
tbl$STIMMBRUCH_GEHABT[ tbl$SIC == sics[ 23 ] ] <- NA

tbl$STIMMBRUCH_ALTER[ !is.na( tbl$STIMMBRUCH_ALTER ) & ( tbl$STIMMBRUCH_ALTER < 10 | 20 < tbl$STIMMBRUCH_ALTER ) ] <-
    NA ##alle anderen die jetzt nicht kuriert wurden wird NA zugewiesen, da außerhalb der range, nur zur Sicherheit, weil alle eigentlich angeguckt wurden die außerhalb der range lagen

# loesche unnoetige Spalten
tbl <-
    remove.columns( tbl, names( tbl )[ grep( "Stimmbruch", names( tbl ) ) ] )

addmargins(table( tbl$STIMMBRUCH_ALTER ))  ##range stimmbruch: 10-16.8y, 742

# zeige alle Datumsspalten an
table.df( tbl[ , get.date.columns( tbl ) ] )

# loesche alle ausser EDAT 
tbl <-
    remove.columns( tbl, setdiff( get.date.columns( tbl ), "EDAT" ) )

# Kind und FamSics und Gruppen
table.df( tbl[ , get.sic.columns( tbl ) ] ) ##es gibt eine SIC für kind und eine FAM_PSEUDO für Fam.zugehoerigkeit
table.df( tbl[ , get.scigroup.columns( tbl ) ] ) ##das selbe fuer SCIgroup

# loesche Jahresspalte
tbl <-
    remove.columns( tbl, "Y" )

# kodiere Spalte SEX um
# heisst alles schon richtig
# tbl <-
#     rename.columns( 
#         tbl,
#         c( "EDAT.x", "TEILNEHMER_GESCHLECHT", "TEILNEHMER_GEB_JJJJMM", "C_ANTHRO_KH_AGE" ),
#         c( "EDAT", "SEX", "BIRTHDAY", "AGE" ) )

# SEX noch umkodieren
tbl$SEX <-
    as.factor( c( "male", "female" )[ match( tbl$SEX, c( 1, 2 ) ) ] )


#4701
nrow( tbl )   

##Underweights raus: ab BMI(SDS)< - 1.88
tbl <-
    tbl[ is.na( tbl$BMI.ADJ ) | ( !is.na( tbl$BMI.ADJ ) & ( tbl$BMI.ADJ >= -1.88 ) ), ]
 ##24 leute werden im GG zu -2.0 mehr ausgeschlossen

sum( is.na( tbl$WEIGHT ) ) 

# jetzt 4600, wenn bmi ausschluss bei -2.0 wären es 4624
nrow(tbl)    





# WEIGHT-GROUPS
tbl$WGHT_GRPS <-
    cut( 
        tbl$BMI.ADJ,
        c( -Inf, +1.28, Inf ),
        c(  "normalweight", "overweight.and.obese" ) ) ##normalweigth m?ssen noch umbenannt werden,da auch die underweights bis -2.0 mit drin sind

# HV: neue spalte mit mean von HV_LI und HV_RE zu HV

li <- tbl$C_PUB_STAT_HV_LI
re <- tbl$C_PUB_STAT_HV_RE

li[ is.na( li ) ] <- 0
re[ is.na( re ) ] <- 0

tbl$HV <-
    ( li + re ) / ifelse( li * re == 0, 1, 2 )

table( tbl[ c( "C_PUB_STAT_HV_LI", "C_PUB_STAT_HV_RE", "HV" ) ] )  ##angucken wie die zahlen zustande kommen

## WINKLER INDEX
# low: 3 <= SCORE_FAM <= 8.4
# mid: 8.4 < SCORE_FAM <= 15.4
# high: 15.4 < SCORE_FAM <= 21 ##nach Kiggs-Quintilen:  "Messung des sozio?konomischen Status in der Kiggs Studie",
##wir richten uns nach Kiggs, die Quintilen von LIFE w?rden anders aussehen...:
quantile( tbl$D00177_SCORE_FAM, na.rm = T, c( .2, .8 ) ) ##angucken, wie wir es eigentlich trennen würden bei quitilen betrachtung

tbl$SES <-
    cut(
        tbl$D00177_SCORE_FAM,
        c( 2, 8.4, 15.4, 22 ),
        c( "low", "mid", "high" ) )

table.df( tbl )

# Zeige alle Spaltennamen der Tabelle tbl an!
# names( tbl ) 

##Pfad in dem die generierten Tabellen stehen
# setwd( "~/LIFE/life-for-postgraduates/LeaOelkers/AllesNeu20170725/data/generated/" )
# setwd( "c:/Users/Lea/Desktop/AllesNeu20170725/data/generated/" )  

setwd( "../generated/" )

save( tbl, file = "main.table.Rd" )

openxlsx::write.xlsx( x = tbl, file = "main.table.xlsx" )

names( tbl)

nrow(tbl)   ##main table mit aussortierten underweights
#4600



sum(is.na(tbl$SES))
t( table.df( tbl ) ) ##zeigt anzahl Nas und availables von allen Spalten an
#spalte "Stimmbruch_Gehabt macht keinen sinn, da viele keine altersangabe gemacht haben, deshalb nur spalte  STIMMBRUCH_ALTER angucken

