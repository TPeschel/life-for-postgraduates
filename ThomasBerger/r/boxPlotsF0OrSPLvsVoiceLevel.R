## delete all data
rm( list = ls( ) )

library( life.helper )

WDTH <-
    6
HGHT <-
    6
ENDING <-
    "pdf"

## change working dir to data/gfx/betaPlots
setwd( "~/LIFE/life-for-postgraduates/ThomasBerger/gfx/boxPlotsFRQ_SPL" )

load( "~/LIFE/life-for-postgraduates/ThomasBerger/original/Stimme/Stimme-Dateien/save/voice_clean.Rda" )

profiles <-
    c( 1 : 5 )

endings <-
    c( "I", "II", "III", "IV", "V" )

names( voice_speak)

d <-
    voice_speak[ , c( "age", "geschlecht", paste0( "f0_sprech_", profiles ), paste0( "spl_sprech_", profiles ) ) ]

d$geschlecht <-
    c( "male", "female" )[ match( d$geschlecht, c( "m", "f" ) ) ] 

d$age.cat <-
    cut(
        d$age,
        breaks = c( 5.5, 10.5, 14.5, 17.5 ),
        labels = c( "6-10 years", "11-14 years", "15-17 years" ) )
###################
old.f0 <-
    paste0( "f0_sprech_", profiles )

d.f0 <-
    melt( 
        na.omit(
            d[ , c( "age.cat", "geschlecht", old.f0 ) ],
            c( "age.cat", "geschlecht" ) ) )

d.f0$variable <-
    endings[ match( d.f0$variable, old.f0 ) ]

old.spl <-
    paste0( "spl_sprech_", profiles )

d.spl <-
    melt( 
        na.omit(
            d[ , c( "age.cat", "geschlecht", old.spl ) ],
            c( "age.cat", "geschlecht" ) ) )

d.spl$variable <-
    endings[ match( d.spl$variable, old.spl ) ]

ggsubplot(
    ggplot( d.f0 ) +
        geom_boxplot( aes( variable, value ) ) +
        facet_grid( geschlecht ~ age.cat ) +
        labs( title = "fundamental frequency", x = "voice level", y = "frequency [Hz]" ) +
        theme_base( ),
    ggplot( d.spl ) +
        geom_boxplot( aes( variable, value ) ) +
        facet_grid( geschlecht ~ age.cat ) +
        labs( title = "sound pegel", x = "voice level", y = "sound pegel [dB]" ) +
        theme_base( ),
        layout = matrix( c( 1, 2 ), ncol = 2 ) )

ggplot( d.f0 ) +
    geom_boxplot( aes( variable, value ) ) +
    facet_grid( geschlecht ~ age.cat ) +
    labs( title = "fundamental frequency", x = "voice level", y = "fundamental frequency [Hz]" ) +
    theme_base( )

ggsave( 
    filename = paste0( "boxPlotF0.", ENDING ), 
    width = WDTH, 
    height = HGHT )

ggplot( d.spl ) +
    geom_boxplot( aes( variable, value ) ) +
    facet_grid( geschlecht ~ age.cat ) +
    labs( title = "sound pegel", x = "voice level", y = "sound pegel [dB]" ) +
    theme_base( )

ggsave(
    filename = paste0( "boxPlotSPL.", ENDING ), 
    width = WDTH, 
    height = HGHT )

