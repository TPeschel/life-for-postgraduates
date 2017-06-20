\name{ggsubplot}
\alias{ggsubplot}
\title{GGSubPlot}
\usage{
ggsubplot( ggplot1, ggplot2, ggplot3, cols = 3 )
}
\description{
gives the opportunity to plot several ggplots at one page
it is nothing but the function multiplot from Cookbook for R
}
\examples{
library(ggthemes)
ggsubplot(
    ggplot( ) +
        geom_path( aes( c( 1 : 100 ), rnorm( 100 ), col = factor( sample( c( 1 : 5 ), 100, T ) ) ) ) +
        scale_color_brewer( name = "STOCKS", type = "qual", palette = 2 ) +
        theme_bw( ),
    ggplot( ) +
        geom_path( aes( c( 1 : 100 ), rnorm( 100 ), col = factor( sample( c( 0 : 3 ), 100, T ) ) ) ) +
        scale_color_brewer( name = "NUMS", type = "seq", palette = 2 ) +
        labs( title = "numbers", x = "x", y = "dev" ) +
        theme_dark( ),
    ggplot( ) +
        geom_point( aes( rnorm( 100, 10, 1 ), rnorm( 100, 1, 10 ) ) ) +
        theme_light( ),
    ggplot( ) +
        geom_histogram( aes( rnorm( 300 ), fill = sample( LETTERS[ c( 1, 2, 4, 8, 16 ) ], 300, T ) ) ) +
        scale_fill_brewer( "Letters", type = "qual", palette = 2 ) +
        theme_linedraw( ),
    ggplot(
        data.frame(
            height = rnorm( 200, 170, 10 ),
            grp1 = sample( c( "male", "female" ), 200, T ),
            grp2 = factor(
                x = sample( c( "black", "brown", "blonde" ), 200, T ),
                levels = c( "blonde", "brown", "black" ) )
        ) ) +
        geom_histogram( aes( height, fill = grp2 ) ) +
        scale_fill_manual( "SEX", values = c(  "#ffc060", "#806000", "#101010" ) ) +
        facet_grid( . ~ grp1 ) +
        theme_gdocs( ),
    ggplot(
        polyfy(
            x = x <- .01 * c( -100 : 100),
            y = exp( -5 * x * x ),
            g = c( rep( "A", 70 ), rep( "B", 61 ), rep( "C", 70 ) ) ) ) +
        geom_polygon( aes( X, Y, fill = G ) ) +
        theme_foundation( ),
    layout = t( matrix( c( 1, 1, 4, 4,
                           3, 2, 2, 2,
                           5, 5, 6, 6 ), nrow = 4 ) ) )
