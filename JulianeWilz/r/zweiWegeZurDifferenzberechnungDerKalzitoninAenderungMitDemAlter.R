rm( list = ls( ) )
library( dplyr )
library( readxl )
library( ggplot2 )
library(reshape2)

d <-
    read_excel( "../sent/2017.06.19/AktuelleTabelle190517excel.xlsx" )

d$AGE_Calcitionin <-
    as.numeric( d$AGE_Calcitionin )

ggplot( d, aes( AGE_Calcitionin, CHILD_MED_H_VITAMIN_D, col = TEILNEHMER_GESCHLECHT ) ) + 
    geom_point( )

d$AGE <-
    cut(
        d$AGE_Calcitionin,
        c( 0 : 20 ) )

s <- 
    d %>%
        group_by( TEILNEHMER_GESCHLECHT, AGE ) %>%
        summarise( calcitonin = mean( CT_S_1_NUM_VALUE ) )

arrange( s, TEILNEHMER_GESCHLECHT, AGE )

s$dff.calcitonin <- s$calcitonin - lag( s$calcitonin )

s$dff.calcitonin[ s$AGE == "(0,1]" ] <- NA

ggplot( s ) + 
    geom_point( aes( AGE, dff.calcitonin, col  = TEILNEHMER_GESCHLECHT ) ) +
    geom_path( aes( AGE, dff.calcitonin, group = TEILNEHMER_GESCHLECHT, col  = TEILNEHMER_GESCHLECHT ) )




rm( list = ls( ) )
library( dplyr )
library( readxl )
library( ggplot2 )
library(reshape2)

d <-
    read_excel( "../sent/2017.06.19/AktuelleTabelle190517excel.xlsx" )

d$AGE_Calcitionin <-
    as.numeric( d$AGE_Calcitionin )

d$AGE <-
    cut(
        d$AGE_Calcitionin,
        c( 0 : 20 ) )
d <-
    arrange( d, CT_S_1_SIC ) %>%
        group_by( CT_S_1_SIC, TEILNEHMER_GESCHLECHT ) %>%
        mutate( 
            diff.visit = as.numeric( difftime( CT_S_1_DATUM, lag( CT_S_1_DATUM ), units = "days" ) ) / 325.25,
            diff.ct_val = CT_S_1_NUM_VALUE - lag( CT_S_1_NUM_VALUE ),
            rat.ct_val.vis = diff.ct_val / diff.visit )

s <- 
    d %>%
        group_by( AGE, TEILNEHMER_GESCHLECHT ) %>%
        summarise( m = mean( rat.ct_val.vis, na.rm = T ) )

ggplot( s ) +
    geom_path( aes( AGE, m, col = TEILNEHMER_GESCHLECHT, group = TEILNEHMER_GESCHLECHT ) ) +
    geom_point( aes( AGE, m, col = TEILNEHMER_GESCHLECHT ) )



# d <-
#     d[ !is.na( d$CHILD_MED_H_VITAMIN_D ) & !is.na( d$diff.visit ) & !is.na( d$diff.ct_val ), ]
# 
# s <-
#     d %>%
#         group_by( TEILNEHMER_GESCHLECHT ) %>%
#         summarise( 
#             sum.vit.d     = sum( CHILD_MED_H_VITAMIN_D == 1 ),
#             sum.no.vit.d  = sum( CHILD_MED_H_VITAMIN_D == 0 ) )

