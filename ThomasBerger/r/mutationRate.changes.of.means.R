## delete all data
rm( list = ls( ) )

load( "../results/data_sprech.Rda" )

library( latex2exp )
library( reshape2 )
library( dplyr )
library( ggplot2 )

d <-
    data.sprech[ , c( "SIC", "sex", "age", "EDAT.x", "F0_SPRECH_1", "F0_SPRECH_2", "F0_SPRECH_3", "F0_SPRECH_4", "SPL_SPRECH_1", "SPL_SPRECH_2", "SPL_SPRECH_3", "SPL_SPRECH_4" ) ]

d <- 
    na.omit( d )

months <-
    6

table( as.integer( data.sprech$age ) )

data.sprech <- 
    data.sprech[ 6 <= data.sprech$age, ]

breaks_ <-
    seq( 6, 18, months / 12 )

labels_ <-
    as.factor( as.numeric( as.character( breaks_[ -1 ] ) ) - months / 24 )

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

ggplot( d.df, aes( as.numeric( as.character( age.cat ) ), value ) ) +
    geom_point( aes( size = 2 ), alpha = .25 ) +
    geom_path( aes( size = 1 ), alpha = .25 ) +
    geom_smooth( method = "loess", alpha = .2 ) +
    facet_grid( variable ~ sex ) +
    ylim( -6, 6 ) +
    scale_x_continuous( breaks = c( 6 : 17 ) ) +
    geom_text( aes( label = as.character( round( value, 1 ) ) ), col = "#4080a0", nudge_y = -1 ) +
    geom_text( aes( label = as.character( d.cnt ) ), col = "#80a0c0", nudge_y = +1 ) +
    geom_hline( yintercept = 0 ) +
    theme_bw( )

ggsave( filename = "changesOfMeans12Months.pdf", width = 30, height = 19 )
