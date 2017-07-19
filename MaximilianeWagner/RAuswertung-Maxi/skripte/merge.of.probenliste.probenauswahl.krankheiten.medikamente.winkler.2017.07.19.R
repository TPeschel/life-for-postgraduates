rm( list = ls( ) )

library( "hlpr4life" )

load.pkgs(
    c(
        "readxl",
        "xlsx",
        "dplyr",
        "ggplot2",
        "ggthemes",
        "lubridate" ) )

# hier Deinen Pfad eintragen
setwd( "~/LIFE/life-for-postgraduates/MaximilianeWagner/RAuswertung-Maxi/" )

probenliste <-
    read_excel( "daten/original/20170608_Probenliste_AGa.xlsx" )

probenauswahl <-
    read_excel( "daten/original/PV208_Probauswahl2_20160929.xlsx" )

krankheiten <-
    read_excel( "daten/original/PV0208_D00127_Krankheiten.xlsx" )

medikamente <-
    read_excel( "daten/original/PV0208_D00129_Medikamente.xlsx" )

winkler <-
    read_excel( "daten/original/PV0208_D00177.xlsx" )

pl.pa <-
    merge(
        probenauswahl,
        probenliste,
        by = "Materialnummer",
        all.x = F )

pl.pa.kr <-
    merge(
        pl.pa,
        krankheiten,
        by.x = c( "TEILNEHMER_SIC", "C_AUFKL_SCI_GROUP" ),
        by.y = c( "C_DISEASE_TX_SIC", "C_DISEASE_TX_SCI_GROUP" ),
        all.x = T )

pl.pa.kr.me <-
    merge(
        pl.pa.kr,
        medikamente,
        by.x = c( "TEILNEHMER_SIC", "C_AUFKL_SCI_GROUP" ),
        by.y = c( "CHILD_MED_H_SIC", "CHILD_MED_H_SCI_GROUP" ),
        all.x = T )

pl.pa.kr.me$YEAR.EDAT <-
    year( pl.pa.kr.me$ENTNAHME_EDAT )

pl.pa.kr.me.wi <-
    merge(
        pl.pa.kr.me,
        winkler,
        by.x = c( "TEILNEHMER_SIC", "YEAR.EDAT" ),
        by.y = c( "PSEUDONYM", "D00177_JAHR" ),
        all.x = T )

pl.pa.kr.me.wi$Cortisol

##
# Kodiere Kortisol um
# noch nicht gemessen: -2
#  < LLOQ:             -1
##
pl.pa.kr.me.wi$Cortisol[ pl.pa.kr.me.wi$Cortisol == "noch nicht gemessen" ] <- 
    -2

pl.pa.kr.me.wi$Cortisol[ pl.pa.kr.me.wi$Cortisol == "<LLOQ" ] <- 
    -1

pl.pa.kr.me.wi$Cortisol <-
    as.numeric( pl.pa.kr.me.wi$Cortisol )

pl.pa.kr.me.wi <-
    rename.column(
        pl.pa.kr.me.wi,
        "Wie oft gewaschen.x",
        'HaarwaschFrequenz' )

pl.pa.kr.me.wi$HaarwaschFrequenzGruppen <-
    c(
        "selten", "selten",
        "normal",
        "haeufig", "haeufig" 
    )[ match(
        pl.pa.kr.me.wi$HaarwaschFrequenz,
        c( 
            1, 3,
            2,
            4, 5 ) ) ]

relevante.spalten <-
    c( 
        "Materialnummer",
        "TEILNEHMER_SIC",
        "C_AUFKL_DATUM",
        "lfd. Nr.x",
        "Cortisol",
        "C_BP_SDS_BP_DIA_3",
        "C_BP_SDS_BP_SYS_3", 
        "C_PUB_STAT_PUB_STATUS",
        "C_ANTHRO_AGE",
        "C_AUFKL_GENDER",
        "C_ANTHRO_KH_BMI_ORIG",
        "C_ANTHRO_KH_BMI_ADJ",
        "C_ANTHRO_KH_HEIGHT_ADJ",
        "HaarwaschFrequenz",
        "HaarwaschFrequenzGruppen",
        "CHILD_MED_H_GLUCO_CORT",
        "CHILD_MED_H_MINERALOCORT",
        "CHILD_MED_H_SEX_STEROIDE",
        "D00177_SCORE_FAM" )

neue.spaltennamen <-
    c( 
        "MAT_NUM",
        "SIC",
        "EDAT",
        "lfdNr",
        "CORTISOL",
        "SDS_DIA_3",
        "SDS_SYS_3", 
        "TANNER",
        "AGE",
        "SEX",
        "BMI_ORIG",
        "BMI_ADJ",
        "HEIGHT_ADJ",
        "HAARWASCH_FREQ",
        "HAARWASCH_FREQ_GRPS",
        "GLUCO_CORT",
        "MINERALOCORT",
        "SEX_STEROIDE",
        "FAM.SCORE" )

pl.pa.kr.me.wi <-
    pl.pa.kr.me.wi[ , relevante.spalten ]

pl.pa.kr.me.wi <-
    rename.columns(
        pl.pa.kr.me.wi,
        relevante.spalten,
        neue.spaltennamen )

# probenliste.probenauswahl.krankheiten.medikamente.winkler <-
#     pl.pa.kr.me.wi

# Arbeitstabelle
main.table <-
    pl.pa.kr.me.wi

# wieviele NAs in den Spalten?
sapply( main.table, function( col ) { sum( is.na( col ) ) } )

# wieviele Datem in den Spalten?
sapply( main.table, function( col ) { sum( !is.na( col ) ) } )

# zumindest SIC und EDAT sollte es geben
main.table <-
    main.table[ !is.na( main.table$SIC ) &!is.na( main.table$EDAT ), ]

# wieviele NAs in den Spalten?
sapply( main.table, function( col ) { sum( is.na( col ) ) } )

# wieviele Datem in den Spalten?
sapply( main.table, function( col ) { sum( !is.na( col ) ) } )


# entferne alle NAs und lege minimale vollstaendige Tabelle an
main.table.minimal <-
    na.omit( main.table )

# keine NAs mehr? Logan!
sapply( main.table.minimal, function( col ) {  sum( is.na( col ) ) } )

# wieviele Datem in den Spalten? 326 (O;)
sapply( main.table.minimal, function( col ) { sum( !is.na( col ) ) } )

# entferne alle unnoetigen Tabellen
rm( 
    list = c(
        "relevante.spalten",
        "neue.spaltennamen",
        "probenliste",
        "probenauswahl",
        "krankheiten",
        "medikamente",
        "winkler",
        "pl.pa",
        "pl.pa.kr",
        "pl.pa.kr.me",
        "pl.pa.kr.me.wi",
        "main.table.minimal" ) )

main.table$SEX <-
    c( "male", "female" )[ match( main.table$SEX, c( 1, 2 ) ) ] 

main.table <- 
    main.table[ main.table$CORTISOL < 35, ]

save(
    list = "main.table",
    file = "daten/main.table.Rd" )

write.xlsx(
    x =  main.table,
    file = "daten/main.table.xlsx",
    sheetName = "pl.pa.kr.me.wi" )

main.table$AGE.CAT <-
    cut(
        main.table$AGE,
        breaks = c( 0 : 20 ) )

p1 <-
    ggplot( 
        main.table, 
        aes( 
            cut( 
                AGE, 
                breaks = c( 0 : 20 ) ), 
            fill = SEX ) ) +
        theme_classic( ) +
        scale_fill_manual( values = c( "deeppink", "deepskyblue" ) ) +
        geom_histogram( stat = "count" ) +
        geom_hline( yintercept = 30, linetype = 2 ) +
        facet_grid( SEX ~ . ) +
        labs( title = "AGE", x = "AGE [y]" ) +
        theme( axis.text.x = element_text( angle = 90 ) )


p2 <-
    ggplot( main.table[ 0 < main.table$CORTISOL, ]  ) +
        theme_classic( ) +
        scale_fill_manual( values = c( "deeppink", "deepskyblue" ) ) +
        geom_boxplot( aes( AGE.CAT, CORTISOL, fill = SEX ) ) +
        facet_grid( SEX ~ . ) +
        labs( title = "CORTISOL BOXPLOT", x = "AGE [y]" ) +
        theme( axis.text.x = element_text( angle = 90 ) ) +
        scale_y_log10( )

p3 <-
    ggplot( main.table[ !is.na( main.table$CORTISOL ) & 0 < main.table$CORTISOL, ] %>% group_by( AGE.CAT, SEX ) %>% summarise( N.CORTISOL = n( ) ) ) +
    theme_classic( ) +
    scale_fill_manual( values = c( "deeppink", "deepskyblue" ) ) +
    geom_histogram( aes( AGE.CAT, N.CORTISOL, fill = SEX ), stat = "identity" ) +
    geom_hline( yintercept = 30, linetype = 2 ) +
    facet_grid( SEX ~ . ) +
    labs( title = "NUM OF CORTISOL DATA PER AGE", x = "AGE [y]" ) +
    theme( axis.text.x = element_text( angle = 90 ) )

p4 <-
    ggplot( main.table[ !is.na( main.table$AGE.CAT ), ] ) +
    theme_bw( ) +
    scale_fill_manual( values = c( "deeppink", "deepskyblue" ) ) +
    geom_histogram( aes( AGE.CAT, fill = SEX ), stat = "count" ) +
    facet_grid( SEX ~ TANNER ) +
    labs( title = "TANNER", x = "PUB STAT" ) +
    theme( axis.text.x = element_text( angle = 90 ) )

##
# !!! ZOOM !!!
##
ggsubplot( p1, p2, p3, p4, cols = 2 )
