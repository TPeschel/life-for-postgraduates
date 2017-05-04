# Loesche Speicher
rm( list =  ls( ) )

# Lade benoetigte Pakete
library( readxl )
library( dplyr )
library( ggplot2 )
library( GGally )
library( Hmisc )

setwd( "~/LIFE/github-tpeschel/life-for-postgraduates/JulianeWilz/data/")

daten <-
    read_excel( "PV0116_GesamtJoin.xlsx" )

d <-
    daten[ !is.na( daten$AGE_Calcitionin ) & !is.na( daten$CA_S_NUM_VALUE ) & !is.na( daten$CT_S_1_NUM_VALUE ), ]

d$age <- as.numeric( d$AGE_Calcitionin )
d$AGE_Calcitionin <- NULL

d$EDAT <- d$CT_S_1_DATUM
d$CT_S_1_DATUM <- NULL

d$SCIGROUP <- d$CT_S_1_GRUPPE
d$CT_S_1_GRUPPE <- NULL

d$SIC <- d$CT_S_1_SIC
d$CT_S_1_GRUPPE <- NULL

d$sex <- d$TEILNEHMER_GESCHLECHT
d$TEILNEHMER_GESCHLECHT <- NULL

d <-
    d[ , c( "SIC", "SCIGROUP", "EDAT", "age", "sex", "CT_S_1_NUM_VALUE", "CA_S_NUM_VALUE" ) ]

d <-
    d %>%
    group_by( SIC ) %>%
    mutate( visits = as.factor( n( ) ), visit = as.factor( dense_rank( EDAT ) ) )

d$ages <- 
    cut( 
        d$age,
        breaks = c( c(0, .375, .75 ), seq( 1, 20, by = 1 ) ) )

ggplot( d, aes( age, CT_S_1_NUM_VALUE, col = visit ) ) +
    geom_point( ) +
    facet_grid( . ~ sex )

ggplot( d[ d$age < 3, ], aes( age, CT_S_1_NUM_VALUE, col = visit ) ) +
    geom_point( aes( size = visits ), alpha = .3 )  +
    scale_color_manual( values = c( "#FF0000", "#00FF00", "#0000FF", "#00a0a0", "#a000a0", "#a000a0", "#808080", "#404040" ) ) +
    #    guides( col = FALSE ) +
    facet_grid( . ~ sex )

ggplot( d, aes( age, CT_S_1_NUM_VALUE, col = SIC ) ) +
    geom_point( alpha = .3 )  +
    geom_line( aes( alpha = visits ) ) +
    guides( col = FALSE ) +
    facet_grid( . ~ sex )

ggplot( d, aes( age, CT_S_1_NUM_VALUE, col = visit ) ) +
    geom_point( aes( alpha = visits ) )  +
    scale_color_manual( values = c( "#FF0000", "#00FF00", "#0000FF", "#00a0a0", "#a000a0", "#a0a0a0", "#808080", "#404040" ) ) +
    guides( col = FALSE ) +
    facet_grid( . ~ sex )

ggplot( d[ d$age < 3, ], aes( age, CT_S_1_NUM_VALUE, col = SIC ) ) +
    geom_point( aes( alpha = visit ) )  +
    geom_line( alpha = .2 ) +
    guides( col = FALSE ) +
    facet_grid( . ~ sex )

ggplot( d[ d$age < 25, ], aes( ages, CT_S_1_NUM_VALUE, col = SIC ) ) +
    geom_point( aes( alpha = visits ) )  +
    geom_line( aes( as.numeric( ages ), alpha = visits ) ) +
    guides( col = FALSE ) +
    facet_grid( . ~ sex ) +
    theme_bw( )

ggplot( d[ d$age < 2.5, ], aes( age, CT_S_1_NUM_VALUE, col = SIC ) ) +
    geom_point( aes( alpha = visit ) )  +
    geom_line( aes( alpha = visits ) ) +
    guides( col = FALSE ) +
    facet_grid( . ~ sex ) +
    theme_bw( )

d <-
    d[ d$age <= 2.5, ]

d$ages <- 
    cut( 
        d$age,
        breaks = c(0, .375, .75, 1.25, 1.75, 25 ) )

d <-
    d %>%
    group_by( SIC ) %>%
    mutate( visits = as.factor( n( ) ), visit = as.factor( dense_rank( EDAT ) ) )

table( d$ages, d$visits, d$sex )

ggplot( d, aes( ages, fill = visit ) ) +
    geom_bar( stat = "count", position = "stack" ) +
    facet_grid( . ~ sex )

ggplot( d, aes( ages, fill = visits ) ) +
    geom_bar( stat = "count", position = "stack" ) +
    facet_grid( . ~ sex )

# an dieser Stelle erkennen, dass eine einfache Einteilung
# in Kinder mit hohen Kalzitoninwerten und niedrige ein Problem
# darstellen koennte
d.sum <-
    d %>%
    group_by( sex, ages ) %>%
    summarise( n = n( ) )

ggplot( d, aes( CT_S_1_NUM_VALUE, fill = sex ) ) +
    geom_bar( ) +
    facet_grid( ages ~ sex ) +
    ylim( 0, 25 )

ggplot( d, aes( CT_S_1_NUM_VALUE, fill = visit ) ) +
    geom_bar( position = "stack" ) +
    facet_grid( ages ~ sex ) +
    ylim( 0, 25 )

d.sum <-
    d %>%
    group_by( sex, ages, visit ) %>%
    summarise( CT_S_1_NUM_VALUE.mean = mean( CT_S_1_NUM_VALUE ), n = n( ) )

ggplot( d, aes( ages, CT_S_1_NUM_VALUE, col = visit ) ) +
    geom_boxplot( outlier.size = .2 ) +
    geom_text( data = d.sum, aes( ages, CT_S_1_NUM_VALUE.mean, label = n ) ) +
    facet_grid( sex ~ visit )

d.02 <-
    d[ d$age <= 2.5, ]

d.02$ages <- 
    cut( 
        d.02$age,
        breaks = c( 0, .375, .625, 1.25, 2.5 ) )

d.02.sum <-
    d.02 %>%
    group_by( sex, ages, visit ) %>%
    summarise( CT_S_1_NUM_VALUE.mean = mean( CT_S_1_NUM_VALUE ), n = n( ) )

ggplot( d.02, aes( ages, CT_S_1_NUM_VALUE, col = visit ) ) +
    geom_boxplot( outlier.size = .2 ) +
    geom_point( aes( ages, CT_S_1_NUM_VALUE, col = visit ), alpha = .4 ) +
    geom_text( data = d.02.sum, aes( ages, CT_S_1_NUM_VALUE.mean, label = n ) ) +
    facet_grid( sex ~ visit )

d.sum <-
    d %>%
    group_by( sex, ages, visit ) %>%
    summarise( CT_S_1_NUM_VALUE.mean = mean( CT_S_1_NUM_VALUE ), n = n( ) )

ggplot( d, aes( ages, CT_S_1_NUM_VALUE, col = visit ) ) +
    geom_boxplot( outlier.size = .2, fill = "gray" ) +
    geom_point( aes( ages, CT_S_1_NUM_VALUE, col = visit ), position = "jitter" ) +
    geom_text( data = d.sum, aes( ages, CT_S_1_NUM_VALUE.mean, label = n ) ) +
    facet_grid( sex ~ visit ) +
    theme_bw( )

d.1 <-
    d[ d$ages == levels( d$ages )[ 2 ], ]

head( d.1 )

ggplot( d.1, aes( ages, CT_S_1_NUM_VALUE, col = visit ) ) +
    geom_boxplot( outlier.size = .2, fill = "gray" ) +
    geom_point( aes( ages, CT_S_1_NUM_VALUE, col = visit ), position = "jitter" ) +
    geom_text( data = d.sum, aes( ages, CT_S_1_NUM_VALUE.mean, label = n ), col = "black" ) +
    facet_grid( sex ~ visit ) +
    theme_bw( )

ggplot( d.1, aes( ages, CT_S_1_NUM_VALUE, col = visit ) ) +
    geom_boxplot( outlier.size = .2, fill = "gray" ) +
    geom_point( aes( ages, CT_S_1_NUM_VALUE, col = visit ), position = "jitter" ) +
    geom_text( data = d.sum, aes( ages, CT_S_1_NUM_VALUE.mean, label = n ), col = "black" ) +
    facet_grid( sex ~ . ) +
    theme_bw( )

n <- nrow( d.1 )

table( d.1$sex )

ggplot( d.1, aes( CT_S_1_NUM_VALUE, col = sex ) ) +
    geom_bar( ) +
    facet_grid( . ~ sex )

d.m <- d.1[ d.1$sex=="male", ]
d.f <- d.1[ d.1$sex=="female", ]

d.m.low  <- d.m[ d.m$CT_S_1_NUM_VALUE < median( d.m$CT_S_1_NUM_VALUE ), ]
d.m.high <- d.m[ d.m$CT_S_1_NUM_VALUE >= median( d.m$CT_S_1_NUM_VALUE ), ]
d.f.low  <- d.f[ d.f$CT_S_1_NUM_VALUE < median( d.f$CT_S_1_NUM_VALUE ), ]
d.f.high <- d.f[ d.f$CT_S_1_NUM_VALUE >= median( d.f$CT_S_1_NUM_VALUE ), ]

n <-4000

d.m <- sample( d.m.high$CA_S_NUM_VALUE, n, replace = T ) - sample( d.m.low$CA_S_NUM_VALUE, n, replace = T )
d.f <- sample( d.f.high$CA_S_NUM_VALUE, n, replace = T ) - sample( d.f.low$CA_S_NUM_VALUE, n, replace = T )

nrow( d.m.high )

nrow( d.f.high )

summary( d.f.low$CA_S_NUM_VALUE )
hist( d.f.low$CA_S_NUM_VALUE, breaks = 30 )

summary( d.f.high$CA_S_NUM_VALUE )
hist( d.f.high$CA_S_NUM_VALUE, breaks = 30 )


hist( d.m.low$CA_S_NUM_VALUE, breaks = 30 )
hist( d.m.high$CA_S_NUM_VALUE, breaks = 30 )
hist( d.m, breaks = 30 )

hist( d.f.low$CA_S_NUM_VALUE, breaks = 30 )
hist( d.f.high$CA_S_NUM_VALUE, breaks = 30 )
hist( d.f, breaks = 30 )

shapiro.test( d.m )
shapiro.test( d.f )

mean( sapply( c( 1 : 1000 ), function( d ) shapiro.test( rnorm( n ) )$statistic ) )
mean( sapply( c( 1 : 1000 ), function( d ) shapiro.test( rnorm( n ) )$p ) )
mean( sapply( c( 1 : 1000 ), function( d ) shapiro.test( sample( d.m.high$CA_S_NUM_VALUE, 1000, replace = T ) - sample( d.m.low$CA_S_NUM_VALUE, 1000, replace = T ) )$statistic ) )
mean( sapply( c( 1 : 1000 ), function( d ) shapiro.test( sample( d.m.high$CA_S_NUM_VALUE, 1000, replace = T ) - sample( d.m.low$CA_S_NUM_VALUE, 1000, replace = T ) )$p ) )

mean( sapply( c( 1 : 1000 ), function( d ) shapiro.test( sample( d.f.high$CA_S_NUM_VALUE, 1000, replace = T ) - sample( d.f.low$CA_S_NUM_VALUE, 1000, replace = T ) )$statistic ) )
mean( sapply( c( 1 : 1000 ), function( d ) shapiro.test( sample( d.f.high$CA_S_NUM_VALUE, 1000, replace = T ) - sample( d.f.low$CA_S_NUM_VALUE, 1000, replace = T ) )$p ) )

t.test( d.m.high$CA_S_NUM_VALUE, d.m.low$CA_S_NUM_VALUE )
t.test( d.f.high$CA_S_NUM_VALUE, d.f.low$CA_S_NUM_VALUE )


