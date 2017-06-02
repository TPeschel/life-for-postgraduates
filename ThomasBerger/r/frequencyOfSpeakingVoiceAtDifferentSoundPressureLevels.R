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
t865 <- t865[ 6 < t865$age & t865$age < 18, ]

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

pal <-
    rep(
        rgb( seq( 0, .8, length.out = 3 ), seq( 0, .7, length.out = 3 ), seq( 0, .6, length.out = 3 ) ), 4 )
    #rgb( runif( 32, 0, 1 ), runif( 32, 0, 1 ), runif( 32, 0, 1 )  )

txt.pos <-
    f0.spl %>%
    group_by( variable, sex ) %>%
    summarise(
        y = mean( value.y ),
        x = min( ifelse( sex == "male", max( value.x ) + 15, min( value.x ) - 15 ) ) )

ggplot( 
    f0.spl, 
    aes( x = value.x, y = value.y, col = age.cat ) ) +
    geom_path( aes( group = age.cat ) ) +
    geom_point( shape = 3, size = 3 ) +
    facet_grid( . ~ sex ) +
    scale_color_discrete( "age [y]" ) +
    labs( 
        title = "sound pressure vs fundamental frequency",
        x = "fundamental frequency [Hz]", 
        y = "sound pressure [dB]",
        subtitle = "voice levels: I: softest speaking   II: conversational   III: classroom   IV: shouting" ) +
    theme_bw( ) +
    geom_text( inherit.aes = F, data = txt.pos, aes( label = variable, x = x, y = y ) ) 

ggsave( filename = "ageRelatedFrequenciesOfSpeakingVoiceAtDifferentSoundPressureLevels.png", width = 15, height = 9 )