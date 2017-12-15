## delete all data
rm( list = ls( ) )

setwd( "~/LIFE/life-for-postgraduates/ThomasBerger/gfx/mutationRate")

load( "../../results/data_sprech.Rda" )

library( latex2exp )
library( reshape2 )
library( dplyr )
library( ggplot2 )
library( life.helper )

WDTH <-
    9

HGHT <-
    6

ENDING <-
    "pdf"

COL <-
    F

d <-
    data.sprech[ , c( "SIC", "sex", "age", "EDAT.x", "F0_SPRECH_1", "F0_SPRECH_2", "F0_SPRECH_3", "F0_SPRECH_4", "SPL_SPRECH_1", "SPL_SPRECH_2", "SPL_SPRECH_3", "SPL_SPRECH_4" ) ]

d <- 
    na.omit( d )

months <-
    18

table( as.integer( d$age ) )

d <- 
    d[ 6 <= d$age & d$age <= 18, ]

breaks_ <-
    seq( 6, 19, months / 12 )

labels_ <-
    as.factor( as.numeric( as.character( breaks_[ 1 : length( breaks_ ) -1 ] ) ) )

d$age.cat <-
    cut(
        d$age,
        breaks_,
        labels_ )

d.df <-
    d %>%
        group_by( sex, age.cat ) %>%
        summarise( 
            m.f0.1 = mean( F0_SPRECH_1 ),
            m.f0.2 = mean( F0_SPRECH_2 ),
            m.f0.3 = mean( F0_SPRECH_3 ),
            m.f0.4 = mean( F0_SPRECH_4 ),
            cnt    = n( ) )

d.df <-
    d.df %>%
        arrange( sex, age.cat ) %>%
        group_by( sex ) %>%
        mutate( 
            d.m.f0.1 = ( m.f0.1 - lag( m.f0.1 ) ) / months,
            d.m.f0.2 = ( m.f0.2 - lag( m.f0.2 ) ) / months,
            d.m.f0.3 = ( m.f0.3 - lag( m.f0.3 ) ) / months,
            d.m.f0.4 = ( m.f0.4 - lag( m.f0.4 ) ) / months,
            d.cnt    = ( cnt + lag( cnt ) ) / 2 )

d.df[ , c( "m.f0.1", "m.f0.2", "m.f0.3", "m.f0.4", "cnt" ) ] <- 
    NULL

d.df <-
    melt( 
        d.df,
        c( "sex", "age.cat", "d.cnt" ) )


d.df$variable <- 
    c( "I", "II", "III", "IV" )[ match( d.df$variable, c( "d.m.f0.1", "d.m.f0.2", "d.m.f0.3", "d.m.f0.4" ) ) ]

if( COL == T ) {
    ggplot( 
        d.df, 
        aes( as.numeric( as.character( age.cat ) ), value ) ) +
        geom_point( aes( size = d.cnt ), alpha = .25 ) +
        geom_path( aes( size = 1 ), alpha = .25 ) +
        geom_smooth( method = "loess", alpha = .2 ) +
        facet_grid( variable ~ sex ) +
        labs( title = TeX( "monthly change of fundamental frequency of voice levels I-IV measured in steps of 18 months" ) ) +
        scale_size( name = "count of\nmeasurements" ) +
        scale_x_continuous( name = "age [y]", breaks = c( 6 : 18 ), limits = c( 6.5, 17.5 ) ) +
        scale_y_continuous( name = TeX( "$\\frac{\\Delta f_0}{\\Delta t}\\,\\left[\\frac{Hz}{month}\\right]$" ), breaks = c( -6 : 4 ) ) +
        geom_text( aes( label = as.character( round( value, 1 ) ) ), col = "#8030a0", nudge_y = -2 ) +
        # geom_text( aes( label = as.character( round( d.cnt ) ) ), col = "#80a0c0", nudge_y = +2 ) +
        #annotate( "text", label = "change in f0", col = "#8030a0", x = 12, y = -6 ) +
        # annotate( "text", label = "number of measurements", col = "#80a0c0", x = 12, y = 4 ) +
        geom_hline( yintercept = 0 ) +
        theme_bw( )
}else{
    ggplot( 
        d.df, 
        aes( as.numeric( as.character( age.cat ) ), value ) ) +
        geom_point( aes( size = d.cnt ), alpha = .25 ) +
        geom_path( aes( size = 1 ), alpha = .25 ) +
        geom_smooth( method = "loess", alpha = .2, col = "#404040" ) +
        facet_grid( variable ~ sex ) +
        labs( title = TeX( "monthly change of fundamental frequency of voice levels I-IV measured in steps of 18 months" ) ) +
        scale_size( name = "count of\nmeasurements" ) +
        scale_x_continuous( name = "age [y]", breaks = c( 6 : 18 ), limits = c( 6.5, 17.5 ) ) +
        scale_y_continuous( name = TeX( "$\\frac{\\Delta f_0}{\\Delta t}\\,\\left[\\frac{Hz}{month}\\right]$" ), breaks = c( -6 : 4 ) ) +
        geom_text( aes( label = as.character( round( value, 1 ) ) ), col = "#808080", nudge_y = -2 ) +
#        geom_text( aes( label = as.character( round( d.cnt ) ) ), col = "#808080", nudge_y = +2 ) +
        #annotate( "text", label = "change in f0", col = "#606060", x = 12, y = -6 ) +
#        annotate( "text", label = "number of measurements", col = "#606060", x = 12, y = 4 ) +
        geom_hline( yintercept = 0 ) +
        theme_bw( ) }

WDTH <-
    11

HGHT <-
    9

ggsave( filename = paste( "changesOfMeansOfFreqEvery18MonthsForAllVoiceLevelsBW.", ENDING ), device = ENDING, width = WDTH, height = HGHT )

d.df.2 <-
    d.df[ d.df$variable == "II", ]

d.df.2 <-
    d.df.2[ !is.na( d.df.2$value ) | !is.na( d.df.2$d.cnt ), ]

if( COL == T ) {
    ggplot( 
        d.df.2, 
        aes( as.numeric( as.character( age.cat ) ), value ) ) +
        geom_point( aes( size = d.cnt ), alpha = .25 ) +
        geom_path( aes( size = 1 ), alpha = .25 ) +
        geom_smooth( method = "loess", alpha = .2 ) +
        facet_grid( . ~ sex ) +
        labs( title = TeX( "monthly change of fundamental frequency of voice level II measured in steps of 18 months" ) ) +
        scale_size( name = "count of\nmeasurements" ) +
        scale_x_continuous( name = "age [y]", breaks = c( 5 : 18 ) ) +
        scale_y_continuous( name = TeX( "$\\frac{\\Delta f_0}{\\Delta t}=\\left[\\frac{Hz}{month}\\right]$" ), breaks = c( -5 : 3 ) ) +
        geom_text( aes( label = as.character( round( value, 1 ) ) ), col = "#8030a0", nudge_y = -1 ) +
        #geom_text( aes( label = as.character( round( d.cnt ) ) ), col = "#80a0c0", nudge_y = +1 ) +
        #annotate( "text", label = "change in f0", col = "#8030a0", x = 12, y = -5 ) +
        #annotate( "text", label = "number of measurements", col = "#80a0c0", x = 12, y = 2 ) +
        geom_hline( yintercept = 0 ) +
        theme_bw( )
}else{
    ggplot( 
        d.df.2, 
        aes( as.numeric( as.character( age.cat ) ), value ) ) +
        geom_point( aes( size = d.cnt ), alpha = .25 ) +
        geom_path( aes( size = 1 ), alpha = .25 ) +
        geom_smooth( method = "loess", alpha = .2, col = "#404040" ) +
        facet_grid( . ~ sex ) +
        labs( title = TeX( "monthly change of fundamental frequency of voice level II measured in steps of 18 months" ) ) +
        scale_size( name = "count of\nmeasurements" ) +
        scale_x_continuous( name = "age [y]", breaks = c( 5 : 18 ) ) +
        scale_y_continuous( name = TeX( "$\\frac{\\Delta f_0}{\\Delta t}=\\left[\\frac{Hz}{month}\\right]$" ), breaks = c( -5 : 3 ) ) +
        geom_text( aes( label = as.character( round( value, 1 ) ) ), col = "#808080", nudge_y = -1 ) +
        #geom_text( aes( label = as.character( round( d.cnt ) ) ), col = "#808080", nudge_y = +1 ) +
        #annotate( "text", label = "change in f0", col = "#606060", x = 12, y = -5 ) +
        #annotate( "text", label = "number of measurements", col = "#606060", x = 12, y = 2 ) +
        geom_hline( yintercept = 0 ) +
        theme_bw( ) }

WDTH <-
    11

HGHT <-
    5

ggsave( filename = paste( "changesOfMeansOfFreqEvery18MonthsForVoiceLevelIIBW.", ENDING ), device = ENDING, width = WDTH, height = HGHT )
