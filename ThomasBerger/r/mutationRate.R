#load( "../results/data_sprech.Rda" )

library( reshape2 )
library( dplyr )
library( ggplot2 )

d <-
    data.sprech[ , c( "SIC", "sex", "age", "EDAT.x", "F0_SPRECH_1", "F0_SPRECH_2", "F0_SPRECH_3", "F0_SPRECH_4", "SPL_SPRECH_1", "SPL_SPRECH_2", "SPL_SPRECH_3", "SPL_SPRECH_4" ) ]
    
d$EDAT <- d$EDAT.x
d$EDAT.x <- NULL

d <-
    arrange( d, SIC, EDAT )


d %<>%
    group_by( SIC ) %>%
    mutate( 
        age    = age,
        d.t    = as.numeric( difftime( EDAT, lag( EDAT ), units = "d" ) ) *12 / 365.25,
        d.f0.1 = F0_SPRECH_1 - lag( F0_SPRECH_1 ),
        d.f0.2 = F0_SPRECH_2 - lag( F0_SPRECH_2 ),
        d.f0.3 = F0_SPRECH_3 - lag( F0_SPRECH_3 ),
        d.f0.4 = F0_SPRECH_4 - lag( F0_SPRECH_4 ),
        d.spl.1 = SPL_SPRECH_1 - lag( SPL_SPRECH_1 ),
        d.spl.2 = SPL_SPRECH_2 - lag( SPL_SPRECH_2 ),
        d.spl.3 = SPL_SPRECH_3 - lag( SPL_SPRECH_3 ),
        d.spl.4 = SPL_SPRECH_4 - lag( SPL_SPRECH_4 ) )

# d$F0_SPRECH_1 <- NULL
# d$F0_SPRECH_2 <- NULL
# d$F0_SPRECH_3 <- NULL
# d$F0_SPRECH_4 <- NULL
# d$SPL_SPRECH_1 <- NULL
# d$SPL_SPRECH_2 <- NULL
# d$SPL_SPRECH_3 <- NULL
# d$SPL_SPRECH_4 <- NULL
# d$EDAT <- NULL
d <- 
#    d[ , c( "SIC", "sex", "age", "d.t", "d.f0.1", "d.f0.4" ) ]
    d[ , c( "SIC", "sex", "age", "d.t", "d.f0.1", "d.f0.2", "d.f0.3", "d.f0.4", "d.spl.1",  "d.spl.2",  "d.spl.3",  "d.spl.4" ) ]

d <-
    d[ !is.na( d$d.t ), ]

d <-
    melt(
        data = d,
        c( "SIC", "sex", "age", "d.t" ) ) 

d$value <- d$value / d$d.t

ggplot( d, aes( age, value ) ) +
    geom_line( aes( col = sex ) ) +
    geom_smooth( method = "loess", aes( col = sex ) ) +
    facet_grid( variable ~ sex )

