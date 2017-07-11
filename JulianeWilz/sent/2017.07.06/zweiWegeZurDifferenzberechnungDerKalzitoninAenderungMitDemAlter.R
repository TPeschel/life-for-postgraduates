rm( list = ls( ) )
library( dplyr )
library( readxl )
library( ggplot2 )
library(reshape2)

# setwd( "~/Desktop/pv0116_neu/")
d <-
    read_excel( "../sent/2017.06.19/AktuelleTabelle190517excel.xlsx" )

# d <-
#   read_excel( "Tabellen/AktuelleTabelle190517excel.xlsx" )

d$AGE_Calcitionin <-
    as.numeric( d$AGE_Calcitionin )

d$age.cat <-
    cut( d$AGE_Calcitionin, breaks = c( 0 : 20 ), labels = c( 1 : 20 ) )

# in welchem Alter wird wie vielen Kindern Vitamin D zugeführt
d$CHILD_MED_H_VITAMIN_D[ is.na( d$CHILD_MED_H_VITAMIN_D ) ] <- 
    -1

ggplot( d ) + 
    geom_histogram( aes( as.factor( CHILD_MED_H_VITAMIN_D ), fill = as.factor( CHILD_MED_H_VITAMIN_D ) ), stat = "count", position = "identity" ) +
    scale_y_continuous( name = "count" ) + 
    scale_x_discrete( name = "vitamin D administered", breaks = c( -1 : +1 ), labels = c( "no data", "no", "yes" ) ) + 
    scale_fill_manual( name = "count", values = c( "black", "red", "green" ), breaks = c( -1 : +1 ), labels = c( "no data", "no", "yes" ) ) + 
    theme_bw( ) +
    facet_grid( TEILNEHMER_GESCHLECHT ~ age.cat ) +
    theme( axis.text.x = element_text( angle = 90 ) ) +
    ggtitle( "Administration of vitamin D vs age [y]" )

## -1 sind NAs
table( d$CHILD_MED_H_VITAMIN_D, d$age.cat ) 


## jetzt mal ohne NAs
ggplot( d[ d$CHILD_MED_H_VITAMIN_D > -1, ] ) + 
    geom_histogram( aes( as.factor( CHILD_MED_H_VITAMIN_D ), fill = as.factor( CHILD_MED_H_VITAMIN_D ) ), stat = "count", position = "identity" ) +
    scale_y_continuous( name = "count" ) + 
    scale_x_discrete( name = "vitamin D administered", breaks = c( 0 : +1 ), labels = c( "no", "yes" ) ) + 
    scale_fill_manual( name = "count", values = c( "red", "green" ), breaks = c( 0 : +1 ), labels = c( "no", "yes" ) ) + 
    theme_bw( ) +
    facet_grid( TEILNEHMER_GESCHLECHT ~ age.cat ) +
    theme( axis.text.x = element_text( angle = 90 ) ) +
    ggtitle( "Administration of vitamin D vs age [y]" )

# ok ab 3 Jahren is nix mehr los
# vielen Kindern im Alter von 0-2.5 Jahren
d$age.cat <-
    cut( d$AGE_Calcitionin, breaks = .25 * c( 0 : 12 ) )

ggplot( d[ d$AGE_Calcitionin <= 3, ] ) + 
    geom_histogram( aes( as.factor( CHILD_MED_H_VITAMIN_D ), fill = as.factor( CHILD_MED_H_VITAMIN_D ) ), stat = "count", position = "identity" ) +
    scale_y_continuous( name = "count" ) + 
    scale_x_discrete( name = "vitamine D administered", breaks = c( -1 : +1 ), labels = c( "no data", "no", "yes" ) ) + 
    theme_bw( ) +
    facet_grid( TEILNEHMER_GESCHLECHT ~ age.cat ) +
    theme( axis.text.x = element_text( angle = 90 ) ) +
    scale_fill_manual( name = "count", values = c( "black", "red", "green" ), breaks = c( -1 : +1 ), labels = c( "no data", "no", "yes" ) ) + 
    ggtitle( "Administration of Vitamine D vs age [y]" )
# vielen Kindern im Alter von 0-2.5 Jahren

( d.tbl <-
    table( d[ , c( "CHILD_MED_H_VITAMIN_D", "age.cat", "TEILNEHMER_GESCHLECHT" ) ] ) )

d.tbl <-
    data.frame( d.tbl )

ggplot( d[ d$AGE_Calcitionin <= 3 & d$CHILD_MED_H_VITAMIN_D > -1, ] ) + 
    geom_histogram( aes( as.factor( CHILD_MED_H_VITAMIN_D ), fill = as.factor( CHILD_MED_H_VITAMIN_D ) ), stat = "count", position = "identity" ) +
    scale_y_continuous( name = "count" ) + 
    scale_x_discrete( name = "vitamine D administered", breaks = c( 0 : +1 ), labels = c( "no", "yes" ) ) + 
    theme_bw( ) +
    facet_grid( TEILNEHMER_GESCHLECHT ~ age.cat ) +
    theme( axis.text.x = element_text( angle = 90 ) ) +
    scale_fill_manual( name = "count", values = c( "red", "green" ), breaks = c( 0 : +1 ), labels = c( "no", "yes" ) ) + 
    #scale_color_manual( name = "count", values = c( "red", "green" ), breaks = c( 0 : +1 ), labels = c( "no", "yes" ), guide = F ) + 
    geom_text( data = d.tbl[ d.tbl$CHILD_MED_H_VITAMIN_D != -1, ], aes( x = as.factor( CHILD_MED_H_VITAMIN_D ), y = Freq, label = Freq ), nudge_y = 5, check_overlap = F, size = 5 ) +
    ggtitle( "Administration of Vitamine D vs age [y]" )

# Alter in Altersgruppen aufteilen
d$AGE <-
    cut(
        d$AGE_Calcitionin,
        c( 0 : 20 ) )

# nach Geschlecht und Alter gruppiert den Mittelwert berechnen
s <- 
    d %>%
        group_by( TEILNEHMER_GESCHLECHT, AGE ) %>%
        summarise( calcitonin = mean( CT_S_1_NUM_VALUE ) )

# tp: sortiere zum Ansehen nach Geschlecht und Alter
arrange( s, TEILNEHMER_GESCHLECHT, AGE )

# die Mittelwertdifferenzen des Calcitonins nach Alter und Geschlecht aufgetrennt
s$dff.calcitonin <- s$calcitonin - lag( s$calcitonin )

# in der Altersgruppe 0-1 Jahre kann es noch keine Mittelwertdifferenz geben, da dies der erste Wert ist
s$dff.calcitonin[ s$AGE == "(0,1]" ] <- NA

if( !require( latex2exp ) ) { install.packages( "latex2exp" ) require( latex2exp ) }
# Graph, der die Mittelwertdifferenzen der einzelnen Altersgruppen nach Geschlecht getrennt darstellt
ggplot( s ) + 
    geom_point( aes( AGE, dff.calcitonin, col  = TEILNEHMER_GESCHLECHT ) ) +
    geom_path( aes( AGE, dff.calcitonin, group = TEILNEHMER_GESCHLECHT, col  = TEILNEHMER_GESCHLECHT ) ) +
    scale_color_manual( values = c( "deeppink", "deepskyblue" ) ) +
    geom_hline( yintercept = 0, linetype = 2 ) +
    ylab( TeX( "$\\Delta calcitonin \\[EINHEIT rausfinden\\]$" ) ) +
    theme_bw( )
# man sieht, dass die Mittelwertdifferenzen zwischen dem dritten und vierten Lebensjahr nicht mehr so groß sind
# man kann diese Altersgruppen ggf zusammen betrachten


rm( list = ls( ) )
library( dplyr )
library( readxl )
library( ggplot2 )
library(reshape2)

# d <-
#   read_excel( "Tabellen/AktuelleTabelle190517excel.xlsx" )
d <-
    read_excel( "../sent/2017.06.19/AktuelleTabelle190517excel.xlsx" )

d$AGE_Calcitionin <-
    as.numeric( d$AGE_Calcitionin )

# Altersgruppen einteilen
d$AGE <-
    cut(
        d$AGE_Calcitionin,
        c( 0 : 20 ) )
# erste Frage: statt 325.25 365.25?
# !!! GUT AUFGEPASST!!!!
# die Differenz der CT-Werte zwischen zwei Besuchen geteilt durch die Anzahl der Tage, die zwischen den beiden Besuchen liegt

d <-
    arrange( d, CT_S_1_SIC ) %>%
        group_by( CT_S_1_SIC, TEILNEHMER_GESCHLECHT ) %>%
        mutate( 
            diff.visit = as.numeric( difftime( CT_S_1_DATUM, lag( CT_S_1_DATUM ), units = "days" ) ) / 365.25,
            diff.ct_val = CT_S_1_NUM_VALUE - lag( CT_S_1_NUM_VALUE ),
            rat.ct_val.vis = diff.ct_val / diff.visit )

# von dem oben genannten Quotienten wird für jede Altersgruppe der Mittelwert gebildet
s <- 
    d %>%
        group_by( AGE, TEILNEHMER_GESCHLECHT ) %>%
        summarise( m = mean( rat.ct_val.vis, na.rm = T ) )

ggplot( s ) +
    geom_path( aes( AGE, m, col = TEILNEHMER_GESCHLECHT, group = TEILNEHMER_GESCHLECHT ) ) +
    geom_point( aes( AGE, m, col = TEILNEHMER_GESCHLECHT ) ) +
    scale_color_manual( values = c( "deeppink", "deepskyblue" ) ) +
    geom_hline( yintercept = 0, linetype = 2 ) +
    ylab( TeX( "$\\Delta calcitonin \\[EINHEIT rausfinden\\]$" ) ) +
    theme_bw( )

# ab dem vierten Lebensjahr ist die Differenz der CT-Werte sehr klein, ändert sich kaum noch zwischen den Besuchen
# spricht auch dafür, dass man diese Lebensjahre zusammenfassen kann
# nochmal fragen, wie genau ich die Formel anwende


#Meine Überlegungen:
#Gibt es noch weitere interessante Variablen, auf die ich das beziehen kann?
#Größe, SSW



# d <-
#     d[ !is.na( d$CHILD_MED_H_VITAMIN_D ) & !is.na( d$diff.visit ) & !is.na( d$diff.ct_val ), ]
# 
# s <-
#     d %>%
#         group_by( TEILNEHMER_GESCHLECHT ) %>%
#         summarise( 
#             sum.vit.d     = sum( CHILD_MED_H_VITAMIN_D == 1 ),
#             sum.no.vit.d  = sum( CHILD_MED_H_VITAMIN_D == 0
