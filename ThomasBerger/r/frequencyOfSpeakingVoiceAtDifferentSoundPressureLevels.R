#load( "~/LIFE/life-for-postgraduates/ThomasBerger/original/Stimme/Stimme-Dateien/save/voice_clean.Rda" )

## delete all data
rm( list = ls( ) )

library( hlpr4life )

load.pkgs( 
    c( "directlabels",
        "dplyr",
        "gamlss",
        "ggplot2",
        "lifecuration",
        "lubridate",
        "readxl", 
        "reshape2",
        "svglite",
        "xtable",
        "latex2exp" ) )

WDTH <-
    8
HGHT <-
    4
ENDING <-
    "png"

## function for quick renaming columns
# re.name <-
#     function( dat.frm, orig.name, new.name ) {
#         names( dat.frm )[ names( dat.frm ) %in% orig.name ] <-
#             new.name
#         names( dat.frm )
#     }

## connection to data base
source( "~/connection/connection.r" )

## change working dir to data/results
setwd( "~/LIFE/life-for-postgraduates/ThomasBerger/gfx/SPLvsFRQ_AGECATS" )

## metadata of all "Aufklärungsgespräche" (A2) 
d88 <-
    get.data( ldb, "D00088" )

## personal data
persdat <-
    get.persdat( ldb )

## sing and speech voices with aliases
t865 <-
    get.data.with.aliases( 
        ldb, 
        "T00865" )

## glue persdat to t865
t865 <-
    add.persdat.age( 
        persdat = persdat, 
        t865 )

## we are interested only in kid's age between 5.5 and 18
t865 <-
    t865[ 6 < t865$age & t865$age < 18, ]

## families
## SozDem
d177 <-
    get.data( 
        ldb, 
        "D00177",
        remove.D.name = T )

d177 <-
    unique( d177[, c( "SIC", "FAM_ID" ) ] )

## sing and speech voices KLASSAK
t328 <-
    get.data.with.aliases( 
        ldb,
        "T00328" )

## merge t865 and d177
d <-
    merge( 
        t865, 
        d177, 
        by = c( "SIC" ), 
        all.x = T )

nrow( d )

d <-
    merge( 
        d, 
        t328, 
        by = c( "SIC", "SGROUP" ), 
        all.x = F )

nrow( d )

d <-
    d[ !( is.na( d$Stimme.F0_SPRECH_1 ) + is.na( d$Stimme.F0_SPRECH_2 ) + is.na( d$Stimme.F0_SPRECH_3 ) + is.na( d$Stimme.F0_SPRECH_4 ) + is.na( d$Stimme.SPL_SPRECH_1 ) + is.na( d$Stimme.SPL_SPRECH_2 ) + is.na( d$Stimme.SPL_SPRECH_3 ) + is.na( d$Stimme.SPL_SPRECH_4 ) ), ]

# nrow( d )
# 
# head( d )
# 
# months <-
#     12

# breaks_ <-
#     seq( 
#         5.5, 
#         19, 
#         months / 12 )
# 
# labels_ <-
#     breaks_[ -1 ] - .5
d <-
    d[ d$age <= 17.5, ]

breaks_ <-
    c(
        5.5, 
        10.5,
        14.5,
        17.5 )

# d$age.cat <-
#     cut(
#         d$age,
#         breaks_,
#         labels_ )

d$age.cat <-
    cut(
        d$age,
        breaks_ )

f0.mean <-
    d %>%
    group_by( sex, age.cat ) %>%
    summarise( 
        I    = mean( Stimme.F0_SPRECH_1 ),
        II   = mean( Stimme.F0_SPRECH_2 ),
        III  = mean( Stimme.F0_SPRECH_3 ),
        IV   = mean( Stimme.F0_SPRECH_4 ) ) %>%
    ungroup( ) %>%
    melt( c( "sex", "age.cat" ) )

names( f0.mean )[ names( f0.mean ) == "value" ] <-
    "f0.mean"

f0.sd <-
    d %>%
    group_by( sex, age.cat ) %>%
    summarise( 
        I    = sd( Stimme.F0_SPRECH_1 ),
        II   = sd( Stimme.F0_SPRECH_2 ),
        III  = sd( Stimme.F0_SPRECH_3 ),
        IV   = sd( Stimme.F0_SPRECH_4 ) ) %>%
    ungroup( ) %>%
    melt( c( "sex", "age.cat" ) )

names( f0.sd )[ names( f0.sd ) == "value" ] <-
    "f0.sd"

spl.mean <-
    d %>%
    group_by( sex, age.cat ) %>%
    summarise( 
        I   = mean( Stimme.SPL_SPRECH_1 ),
        II  = mean( Stimme.SPL_SPRECH_2 ),
        III = mean( Stimme.SPL_SPRECH_3 ),
        IV  = mean( Stimme.SPL_SPRECH_4 ) ) %>%
    ungroup( ) %>%
    melt( c( "sex", "age.cat" ) )

names( spl.mean )[ names( spl.mean ) == "value" ] <-
    "spl.mean"

spl.sd <-
    d %>%
    group_by( sex, age.cat ) %>%
    summarise( 
        I   = sd( Stimme.SPL_SPRECH_1 ),
        II  = sd( Stimme.SPL_SPRECH_2 ),
        III = sd( Stimme.SPL_SPRECH_3 ),
        IV  = sd( Stimme.SPL_SPRECH_4 ) ) %>%
    ungroup( ) %>%
    melt( c( "sex", "age.cat" ) )

names( spl.sd )[ names( spl.sd ) == "value" ] <-
    "spl.sd"

n <-
    d %>%
    group_by( sex, age.cat ) %>%
    summarise( 
        I   = sum( !is.na( Stimme.SPL_SPRECH_1 ) ),
        II  = sum( !is.na( Stimme.SPL_SPRECH_2 ) ),
        III = sum( !is.na( Stimme.SPL_SPRECH_3 ) ),
        IV  = sum( !is.na( Stimme.SPL_SPRECH_4 ) ) ) %>%
    ungroup( ) %>%
    melt( c( "sex", "age.cat" ) )

names( n )[ names( n ) == "value" ] <-
    "n"

f0.spl <- 
    merge( 
        f0.mean,
        spl.mean,
        by = c( "sex", "age.cat", "variable" ) )
# 
# names( f0.spl ) <-
#     re.name( f0.spl, c( "value.x", "value.y" ), c( "f0.mean", "spl.mean" ) )

f0.spl <- 
    merge( 
        f0.spl,
        f0.sd,
        by = c( "sex", "age.cat", "variable" ) )

# names( f0.spl ) <-
#     re.name( f0.spl, c( "value.x", "value.y" ), c( "f0.mean", "spl.mean" ) )

f0.spl <- 
    merge( 
        f0.spl,
        spl.sd,
        by = c( "sex", "age.cat", "variable" ) )

# n <-
#     d %>%
#     group_by( sex, age.cat ) %>%
#     summarise( n = n( ) )

# f0.spl$n <-
#     n$n[ n$sex %in% f0.spl$sex & n$age.cat %in% f0.spl$age.cat ]

f0.spl <-
    merge(
        f0.spl,
        n,
        by = c( "sex", "age.cat", "variable" ) )
    
f0.spl <- 
    na.omit( f0.spl )

# pal <- c(
#     "#102030", "#39de82", "#9010f0", "#00ff21", "#320077", "#8e7f20",
#     "#f07030", "#094e82", "#ff0f10", "#a04f21", "#326037", "#ae7f20",
#     "#f00090", "#800e82", "#90f000", "#d0ff61", "#520037", "#8ebf20" )
#    rgb( runif( 32, 0, 1 ), runif( 32, 0, 1 ), runif( 32, 0, 1 )  )

pal <-
    rgb( 
        seq( 0, .9, length.out = 3 ) + runif( 3, 0, .1 ), 
        seq( 0, .8, length.out = 3 ) + runif( 3, 0, .2 ), 
        seq( 0, .8, length.out = 3 ) + runif( 3, 0, .2 ) )

txt.pos <-
    f0.spl %>%
    group_by( variable, sex ) %>%
    summarise(
        y = mean( spl.mean ) + 2,
        x = min( ifelse( sex == "male", max( f0.mean ) + 50, min( f0.mean ) - 25 ) ) )

notes.1 <- 
    data.frame(
        frq  = f <- 10 ** seq( log10( 100 ), log10( 375 ), by = .05 ),
        note = sapply( f, function( f ) note.of.freq( f ) ) )

notes.2 <- 
    data.frame(
        frq  = f <- c( 100, 150, 200, 250, 300, 350, 400 ),
        note = sapply( f, function( f ) note.of.freq( f ) ) )

ggplot(
    f0.spl,
    aes( x = f0.mean, y = spl.mean ) ) +
    geom_line( aes( linetype = age.cat ) ) +
    geom_point( shape = 1 ) +
    geom_errorbar( aes( ymin = spl.mean - spl.sd, ymax = spl.mean + spl.sd, group = age.cat ), size = .2, alpha = .75, width = 8 ) +
    geom_errorbarh( inherit.aes = T, aes( xmin = f0.mean - f0.sd, xmax = f0.mean + f0.sd, group = age.cat ), size = .2, alpha = .75, height = 2 ) +
    facet_grid( ~ sex, scales = "free" ) +
    scale_color_discrete( guide = F, "age [y]" ) +
    scale_linetype( "age [y]" ) +
    labs(
        title = "sound pressure vs fundamental frequency",
        x = "fundamental frequency [Hz]",
        y = "sound pressure [dB]",
        subtitle = "voice levels: I: softest speaking   II: conversational   III: classroom   IV: shouting" ) +
    theme_bw( ) +
    geom_text( inherit.aes = F, data = notes.2, aes( label = note, x = frq, y = 94 ), check_overlap = T, col = "#606060", size = 2.5 ) +
    geom_text( inherit.aes = F, data = txt.pos, aes( label = variable, x = x, y = y ), alpha = 1, size = 4, col = "black" ) +
    scale_x_continuous( breaks = notes.2$frq, labels = notes.2$frq ) +
    ylim( 44, 96 )

ggsave( filename = paste0( "ageRelatedFrequenciesOfSpeakingVoiceAtDifferentSoundPressureLevels.", ENDING ), width = WDTH, height = HGHT )

ggplot(
    f0.spl,
    aes( x = f0.mean, y = spl.mean ) ) +
    geom_point( data = d, inherit.aes = F, aes( Stimme.F0_SPRECH_1, Stimme.SPL_SPRECH_1, col = age.cat ), alpha = .05 ) +
    geom_point( data = d, inherit.aes = F, aes( Stimme.F0_SPRECH_2, Stimme.SPL_SPRECH_2, col = age.cat ), alpha = .05 ) +
    geom_point( data = d, inherit.aes = F, aes( Stimme.F0_SPRECH_3, Stimme.SPL_SPRECH_3, col = age.cat ), alpha = .05 ) +
    geom_point( data = d, inherit.aes = F, aes( Stimme.F0_SPRECH_4, Stimme.SPL_SPRECH_4, col = age.cat ), alpha = .05 ) +
    geom_line( aes( linetype = age.cat ) ) +
    geom_point( shape = 1 ) +
    geom_errorbar( aes( ymin = spl.mean - spl.sd, ymax = spl.mean + spl.sd, group = age.cat ), size = .2, alpha = .75, width = 8 ) +
    geom_errorbarh( inherit.aes = T, aes( xmin = f0.mean - f0.sd, xmax = f0.mean + f0.sd, group = age.cat ), size = .2, alpha = .75, height = 2 ) +
    facet_grid( ~ sex, scales = "free" ) +
    scale_color_discrete( guide = F, "age [y]" ) +
    scale_linetype( "age [y]" ) +
    labs(
        title = "sound pressure vs fundamental frequency",
        x = "fundamental frequency [Hz]",
        y = "sound pressure [dB]",
        subtitle = "voice levels: I: softest speaking   II: conversational   III: classroom   IV: shouting" ) +
    theme_bw( ) +
    geom_text( inherit.aes = F, data = notes.2, aes( label = note, x = frq, y = 94 ), check_overlap = T, col = "#606060", size = 2.5 ) +
    geom_text( inherit.aes = F, data = txt.pos, aes( label = variable, x = x, y = y ), alpha = 1, size = 4, col = "black" ) +
    scale_x_continuous( breaks = notes.2$frq, labels = notes.2$frq ) +
    ylim( 44, 96 )

ggsave( filename = paste0( "ageRelatedFrequenciesOfSpeakingVoiceAtDifferentSoundPressureLevelsPoints.", ENDING ), width = WDTH, height = HGHT )

ggplot(
    f0.spl,
    aes( x = f0.mean, y = spl.mean ) ) +
    geom_line( aes( group = age.cat, linetype = age.cat ) ) +
    geom_errorbar( aes( ymin = spl.mean - spl.sd, ymax = spl.mean + spl.sd, group = age.cat ), size = .2, alpha = .75, width = .02 ) +
    geom_errorbarh( inherit.aes = T, aes( xmin = f0.mean - f0.sd, xmax = f0.mean + f0.sd, group = age.cat ), size = .2, alpha = .75, height = 2 ) +
    geom_point( shape = 1 ) +
    facet_grid( ~ sex, scales = "free" ) +
    scale_color_discrete( guide = F, "age [y]" ) +
    scale_linetype( "age [y]" ) +
    labs(
        title = "sound pressure vs semi tones",
        x = "semi tones",
        y = "sound pressure [dB]",
        subtitle = "voice levels    I: softest speaking   II: conversational   III: classroom   IV: shouting" ) +
    theme_bw( ) +
    geom_text( inherit.aes = F, data = notes.1, aes( label = paste0( round( frq ), "Hz" ), x = frq, y = 94 ), check_overlap = T, col = "#606060", size = 2.5 ) +
    geom_text( inherit.aes = F, data = txt.pos, aes( label = variable, x = x, y = y ), alpha = 1, size = 4, col = "black" ) +
    scale_x_log10( breaks = notes.1$frq, labels = notes.1$note ) +
    ylim( 44, 96 )

ggsave( filename = paste0( "ageRelatedFrequenciesOfSpeakingVoiceAtDifferentSoundPressureLevelsLog2.", ENDING ), width = WDTH, height = HGHT )

ggplot(
    f0.spl,
    aes( x = f0.mean, y = spl.mean ) ) +
    geom_point( data = d, inherit.aes = F, aes( Stimme.F0_SPRECH_1, Stimme.SPL_SPRECH_1, col = age.cat ), alpha = .03 ) +
    geom_point( data = d, inherit.aes = F, aes( Stimme.F0_SPRECH_2, Stimme.SPL_SPRECH_2, col = age.cat ), alpha = .03 ) +
    geom_point( data = d, inherit.aes = F, aes( Stimme.F0_SPRECH_3, Stimme.SPL_SPRECH_3, col = age.cat ), alpha = .03 ) +
    geom_point( data = d, inherit.aes = F, aes( Stimme.F0_SPRECH_4, Stimme.SPL_SPRECH_4, col = age.cat ), alpha = .03 ) +
    geom_line( aes( group = age.cat, linetype = age.cat ) ) +
    geom_errorbar( aes( ymin = spl.mean - spl.sd, ymax = spl.mean + spl.sd, group = age.cat ), size = .2, alpha = .75, width = .02 ) +
    geom_errorbarh( inherit.aes = T, aes( xmin = f0.mean - f0.sd, xmax = f0.mean + f0.sd, group = age.cat ), size = .2, alpha = .75, height = 2 ) +
    geom_point( shape = 1 ) +
    facet_grid( ~ sex, scales = "free" ) +
    scale_color_discrete( guide = F, "age [y]" ) +
    scale_linetype( "age [y]" ) +
    labs(
        title = "sound pressure vs semi tones",
        x = "semi tones",
        y = "sound pressure [dB]",
        subtitle = "voice levels    I: softest speaking   II: conversational   III: classroom   IV: shouting" ) +
    theme_bw( ) +
    geom_text( inherit.aes = F, data = notes.1, aes( label = paste0( round( frq ), "Hz" ), x = frq, y = 94 ), check_overlap = T, col = "#606060", size = 2.5 ) +
    geom_text( inherit.aes = F, data = txt.pos, aes( label = variable, x = x, y = y ), alpha = 1, size = 4, col = "black" ) +
    scale_x_log10( breaks = notes.1$frq, labels = notes.1$note ) +
    ylim( 44, 96 )

ggsave( filename = paste0( "ageRelatedFrequenciesOfSpeakingVoiceAtDifferentSoundPressureLevelsLog2Points.", ENDING ), width = WDTH, height = HGHT )
