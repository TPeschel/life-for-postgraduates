##
# loesche speicher
##
rm( list = ls( ) )

source( "beta.plot.R" )

d <-
    data.frame(
        x1 = x1<-rnorm( 100 ),
        x2 = x2<-rnorm( 100 ),
        x3 = x3<-sample( c( "low", "normal", "high" ), 100, T ),
        y = 10 * x1 - 5 * x2 + rnorm( 100, 0, 10 ) + c( -3, 1, 7 )[ match( x3, c( "low", "normal", "high" ) ) ],
        sex = sample( c( "f", "m" ) ) )


# immer dieselben Zahlen,
# signifikanz und werte findest Du in den Plots
( d.lm <- lm( y ~ ( x1 + x2 + x3 ) * sex, d ) )
summary( d.lm )
confint( d.lm )

( d.lm.m <- lm( y ~ x1 + x2 + x3, d[ d$sex == "m", ] ) )
summary( d.lm.m )
confint( d.lm.m )

( d.lm.f <- lm( y ~ x1 + x2 + x3, d[ d$sex == "f", ] ) )
summary( d.lm.f )
confint( d.lm.f )


##
# d muss eine Spalte sex haben und diese muss mit "m" und "f" fuer male and female enthalten.
# kannste einfach so anlegen
# tabelle$sex <- c( "m", "f" )[ match( tablle$SpaltennameGender, c( 1, 2 ) ) ] ## wenns 1 fuer maennlich und 2 fuer weiblich im original kodiert ist
# tabelle$sex <- c( "m", "f" )[ match( tablle$SpaltennameGender, c( "male", "female" ) ) ] ## wenns "male" fuer maennlich und "female" fuer weiblich im original kodiert ist

( p <-
    plot.lm.coefficients.with.errorbars( 
        dataframe = d,
        formula   = "y ~ x1 + x2 + x3",
        title     = "Beispielplot",
        xlab      = "Name der unabhaengigen Variable der linearen Regression",
        ylab      = "Groesse des Koeffizienten der unabhaengigen Variable der linearen Regression mit 95% Konfidenzintervallen",
        col       = T ) )   # wenn die Variablen verschiedene Farben haben soll.
                            # Sinnvoll bei Faktoren mit mehreren Levels
                            # bedarf aber unter Umstaenden eine kleine Aenderung in beta.plot.R

p + ggplot2::ggtitle( "Der Plot kann im Nachhinein noch modifiziert werden." )

