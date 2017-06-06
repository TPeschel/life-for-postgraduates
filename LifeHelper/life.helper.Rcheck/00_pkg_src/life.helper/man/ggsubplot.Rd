\name{ggsubplot}
\alias{ggsubplot}
\title{Multi-Plots}
\usage{
ggsubplot( ggplot1, ggplot2, ggplot3, cols = 3 )
}
\description{
gives the opportunity to plot several ggplots at one page
}
\examples{
p1 <- ggplot() + geom_point( aes( c( 1:100), rnorm( 100 ) ) )
p2 <- ggplot() + geom_histogramm( aes( rnorm( 100 ) ) )
ggsubplot( p1, p2, cols = 1 )
}
