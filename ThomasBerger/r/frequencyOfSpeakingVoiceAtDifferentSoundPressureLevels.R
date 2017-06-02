#load( "~/LIFE/github-tpeschel/R/ThomasBerger/original/Stimme/Stimme-Dateien/save/voice_clean.Rda" )

## delete all data
rm( list = ls( ) )

library( directlabels )
library( dplyr )
library( gamlss )
library( ggplot2 )
library( lifecuration )
library( lubridate )
library( readxl )
library( reshape2 )
library( svglite )
library( xtable )

## connection to data base
source( "~/connection/connection.r" )

## change working dir to data/results
setwd( "~/LIFE/life-for-postgraduates/ThomasBerger/results/" )

## metadata of all "Aufklärungsgespräche" (A2) 
d88 <- get.data( ldb, "D00088" )

## personal data
persdat <- get.persdat( ldb )

## sing and speech voices with aliases
t865 <- get.data.with.aliases( ldb, "T00865" )

## glue persdat to t865
t865 <- add.persdat.age( persdat = persdat, t865 )

## we are interested only in kid's age between 5.5 and 18
t865 <- t865[ 5.5 < t865$age & t865$age < 18, ]

## families
## SozDem
d177 <- get.data( ldb, "D00177", remove.D.name = T )
d177 <- unique( d177[, c( "SIC", "FAM_ID" ) ] )

## sing and speech voices KLASSAK
t328 <- get.data.with.aliases( ldb, "T00328" )

## merge t865 and d177
d <- merge( t865, d177, by = c( "SIC" ), all.x = T )

nrow( d )

d <- merge( d, t328, by = c( "SIC", "SGROUP" ), all.x = F )

nrow( d )

d <-
    d[ !( is.na( d$Stimme.F0_SPRECH_1 ) + is.na( d$Stimme.F0_SPRECH_2 ) + is.na( d$Stimme.F0_SPRECH_3 ) + is.na( d$Stimme.F0_SPRECH_4 ) + is.na( d$Stimme.SPL_SPRECH_1 ) + is.na( d$Stimme.SPL_SPRECH_2 ) + is.na( d$Stimme.SPL_SPRECH_3 ) + is.na( d$Stimme.SPL_SPRECH_4 ) ), ]

nrow( d )

head( d )

months <-
    12

breaks_ <-
    seq( 5.5, 18, months / 12 )

labels_ <-
    breaks_[ -1 ]

d$age.cat <-
    cut(
        d$age,
        breaks_,
        labels_ )

f0 <-
    d %>%
    group_by( sex, age.cat ) %>%
    summarise( 
        I    = mean( Stimme.F0_SPRECH_1 ),
        II   = mean( Stimme.F0_SPRECH_2 ),
        III  = mean( Stimme.F0_SPRECH_3 ),
        IV   = mean( Stimme.F0_SPRECH_4 ) ) %>%
    ungroup( ) %>%
    melt( c( "sex", "age.cat" ) )

spl <-
    d %>%
    group_by( sex, age.cat ) %>%
    summarise( 
        I   = mean( Stimme.SPL_SPRECH_1 ),
        II  = mean( Stimme.SPL_SPRECH_2 ),
        III = mean( Stimme.SPL_SPRECH_3 ),
        IV  = mean( Stimme.SPL_SPRECH_4 ) ) %>%
    ungroup( ) %>%
    melt( c( "sex", "age.cat" ) )

f0.spl <- 
    merge( 
        f0,
        spl,
        by = c( "sex", "age.cat", "variable" ) )

f0.spl <- 
    na.omit( f0.spl )

ggplot( 
    f0.spl, 
    aes( x = value.x, y = value.y, alpha = age.cat, col = variable ) ) +
    geom_path( aes( group = age.cat ) ) +
    geom_point( shape = 3, size = 5 ) +
    geom_text( aes( label = age.cat ) ) +
    facet_grid( . ~ sex  ) +
    scale_color_brewer( type = "div", palette = 1 ) +
    theme_light( )
        
f0.spl[ f0.spl$age.cat == "18",]
