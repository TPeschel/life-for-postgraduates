## delete all data
rm( list = ls( ) )

library( hlpr4life )

load.pkgs( 
    c( 
        "ggplot2",
        "dplyr",
        "reshape2",
        "lme4",
        "lifecuration" ) )

WDTH <-
    8

HGHT <-
    4

ENDING <-
    "png"

COL <-
    F

plot.lmer.coefficients.with.errorbars <-
    function( 
        dataframe, 
        formula, 
        title = "coefficients of linear regression of speaking voice II",
        xlab = "dependent variables", 
        ylab = "independent variables" ) {
        # dataframe <- voice_speak.lmer
        # formula = model#st_sprech_1 ~ (1|sic) + (1|fam.id2 )
        library( ggplot2 )
        library( lme4 )
        ## males
        lmer.male.sum <-
            summary( 
                lmer.male <-
                    lmer( 
                        data = dataframe[ dataframe$geschlecht == "m", ], 
                        formula = formula ) )
        lmer.male.sum.confint <- 
            as.data.frame( 
                confint( 
                    lmer.male, 
                    method = "Wald" ) )
        lmer.male <-
            data.frame( 
                x    = rownames( lmer.male.sum$coefficients ),
                y    = lmer.male.sum$coefficients[ , 1 ],
                ymin = ymn <- lmer.male.sum.confint[ 4 : nrow( lmer.male.sum.confint ), 1 ],
                ymax = ymx <- lmer.male.sum.confint[ 4 : nrow( lmer.male.sum.confint ), 2 ],
                p    = c("-", "*")[ match( ymx * ymn < 0, c( T, F ) ) ] )
        lmer.male$sex <- "male"
        # females
        lmer.female.sum <-
            summary( 
                lmer.female <-
                    lmer( 
                        data = dataframe[ dataframe$geschlecht == "f", ], 
                        formula = formula ) )
        lmer.female.sum.confint <- 
            as.data.frame( 
                confint( 
                    lmer.female, 
                    method = "Wald" ) )
        lmer.female <- 
            data.frame( 
                x    = rownames( lmer.female.sum$coefficients ),
                y    = lmer.female.sum$coefficients[ , 1 ],
                ymin = ymn <- lmer.female.sum.confint[ 4 : nrow( lmer.female.sum.confint ), 1 ],
                ymax = ymx <- lmer.female.sum.confint[ 4 : nrow( lmer.female.sum.confint ), 2 ],
                p    = c("-", "*")[ match( ymx * ymn < 0, c( T, F ) ) ] )
        lmer.female$sex <- "female"
        # both together
        lmer.fm <- 
            rbind.data.frame( 
                lmer.female, 
                lmer.male )
        lmer.fm <-
            lmer.fm[ lmer.fm$x != "(Intercept)", ]
        print( lmer.fm )
        if( COL ) {
            ggplot( data = lmer.fm ) +
                geom_point(
                    aes( 
                        x, 
                        y, 
                        col = substr( x, 1, 6 ) ), 
                    size = 2 ) +
                geom_errorbar( 
                    aes( 
                        x, 
                        ymin = ymin, 
                        ymax = ymax, 
                        col = substr( x, 1, 6 ) 
                        ), 
                    width = .3, 
                    alpha = .75 ) + 
                coord_flip( ) +
                facet_grid( . ~ sex ) +
                scale_color_discrete( guide = F ) +
                scale_y_continuous( breaks = seq( -8, 6, by = 2 ) ) +
                theme_bw( ) +
                geom_hline( 
                    yintercept = 0, 
                    linetype = 2 ) +
                labs( 
                    title = title, 
                    x = xlab, 
                    y = ylab ) +
                geom_text( 
                    aes( 
                        x, 
                        y, 
                        label = paste0( round( y, 2 ), " (", p, ")" ) ), 
                    size = 3, 
                    nudge_x = .3 ) } else {
            ggplot( data = lmer.fm ) +
                geom_point(
                    aes( 
                        x, 
                        y ), 
                    size = 2 ) +
                geom_errorbar( 
                    aes( 
                        x, 
                        ymin = ymin, 
                        ymax = ymax ), 
                    width = .3, 
                    alpha = .75 ) + 
                coord_flip( ) +
                facet_grid( . ~ sex ) +
                scale_color_discrete( guide = F ) +
                scale_y_continuous( breaks = seq( -8, 6, by = 2 ) ) +
                theme_bw( ) +
                geom_hline( 
                    yintercept = 0, 
                    linetype = 2 ) +
                labs( 
                    title = title, 
                    x = xlab, 
                    y = ylab ) +
                geom_text( 
                    aes( 
                        x, 
                        y, 
                        label = paste0( round( y, 2 ), " (", p, ")" ) ), 
                    size = 3, 
                    nudge_x = .3 ) } }

## change working dir to data/gfx/betaPlots
setwd( "~/LIFE/life-for-postgraduates/ThomasBerger/gfx/betaPlots" )

load( "~/LIFE/life-for-postgraduates/ThomasBerger/original/Stimme/Stimme-Dateien/save/voice_clean.Rda" )

ldb <-
    connect.db( "user" )

d177 <-
    get.data.with.aliases( ldb, "D00177" )

dbDisconnect( ldb )

voice_speak <-
    merge( 
        voice_speak,
        d177,
        by.x = c( "sic" ),
        by.y = c( "SIC" ),
        all.x = T,
        all.y = F )

voice_speak <-
    voice_speak[ !is.na( voice_speak$datum ) & !is.na( voice_speak$EDAT ) & year( voice_speak$datum ) == year( voice_speak$EDAT ), ]

profiles <-
    c( 1 : 4 )

endings <-
    c( "I", "II", "III", "IV" )

voice_speak$bmi.sds.cat <-
    relevel( voice_speak$bmi.sds.cat, "normal" )

voice_speak$tanner.cat <-
    c( "I-prepubertal", paste0( "II-IV-", rep( "pubertal", 3 ) ), "V-adult" )[ match( voice_speak$tanner, c( 1 : 5 ) ) ]

voice_speak.lmer <-
    voice_speak[ , c( "sic", "gruppe", "fam.id2", "datum", "geschlecht", "bmi", "bmi.sds.cat", "tanner", "tanner.cat", "u_sing_stimmbelastg", "u_sing_stimmbelastg_v", "u_sing_stimmtraining", "u_sing_stimmtraining_v", "u_sing_singen_mot", "u_sing_zaehl_mot", paste0( "u_sing_instrument_", c( 1 : 3 ) ), paste0( "u_sing_instr_v_", c( 1 : 3 ) ), paste0( "st_sprech_", profiles ), paste0( "spl_sprech_", profiles ), names( d177 )[ !names( d177 ) %in% c( "SIC", "SGROUP" ) ] ) ]

voice_speak.lmer$u_sing_stimmbelastg <-
    as.factor( voice_speak.lmer$u_sing_stimmbelastg )

voice_speak.lmer$u_sing_stimmbelastg_v <- 
    as.factor( voice_speak.lmer$u_sing_stimmbelastg_v )

voice_speak.lmer$u_sing_stimmtraining <- 
    as.factor( voice_speak.lmer$u_sing_stimmtraining )

voice_speak.lmer$u_sing_stimmtraining_v <-
    as.factor( voice_speak.lmer$u_sing_stimmtraining_v )

voice_speak.lmer$wind_instrument <-
    c( "no", "yes" )[ match( voice_speak.lmer$u_sing_instrument_2, c( 0, 1 ) ) ]

voice_speak.lmer$wind_instrument_past <-
    c( "no", "yes" )[ match( voice_speak.lmer$u_sing_instr_v_2, c( 0, 1 ) ) ]

addmargins( table( voice_speak.lmer$u_sing_stimmbelastg ) )
addmargins( table( voice_speak.lmer$u_sing_stimmbelastg_v ) )
addmargins( table( voice_speak.lmer$u_sing_stimmtraining ) )
addmargins( table( voice_speak.lmer$u_sing_stimmtraining_v ) )
addmargins( table( voice_speak.lmer$wind_instrument ) )
addmargins( table( voice_speak.lmer$wind_instrument_past ) )

voice_speak.lmer$strain <-
    factor( 
        c( "sometimes", "sometimes", "routinely", "routinely" )[ match( voice_speak.lmer$u_sing_stimmbelastg, c( 1 : 4 ) ) ] )

voice_speak.lmer$strain <-
    relevel( voice_speak.lmer$strain, "sometimes" )

voice_speak.lmer$strain_past <-
    factor( 
        c( "sometimes", "sometimes", "routinely", "routinely" )[ match( voice_speak.lmer$u_sing_stimmbelastg_v, c( 1 : 4 ) ) ] )

voice_speak.lmer$strain_past <-
    relevel( voice_speak.lmer$strain_past, "sometimes" )
        
voice_speak.lmer$training <-
    factor(
        c( "unmaintained", "maintained", "maintained" )[ match( voice_speak.lmer$u_sing_stimmtraining, c( 1 : 3 ) ) ] )
        
voice_speak.lmer$training <-
    relevel( voice_speak.lmer$training, "unmaintained" )

voice_speak.lmer$training_past <-
    factor(
        c( "unmaintained", "maintained", "maintained" )[ match( voice_speak.lmer$u_sing_stimmtraining_v, c( 1 : 3 ) ) ] )
        
voice_speak.lmer$training_past <-
    relevel( voice_speak.lmer$training_past, "unmaintained" )

addmargins( table( voice_speak.lmer$u_sing_stimmbelastg ) )
addmargins( table( voice_speak.lmer$u_sing_stimmbelastg_v ) )
addmargins( table( voice_speak.lmer$u_sing_stimmtraining ) )
addmargins( table( voice_speak.lmer$u_sing_stimmtraining_v ) )

addmargins( table( voice_speak.lmer$bmi.sds.cat ) )
addmargins( table( voice_speak.lmer$wind_instrument ) )
addmargins( table( voice_speak.lmer$wind_instrument_past ) )

sum( is.na( voice_speak.lmer$C_WINKLER.SCORE_FAM ) )

addmargins( table( voice_speak.lmer$u_sing_stimmbelastg ) )
addmargins( table( voice_speak.lmer$u_sing_stimmbelastg_v ) )
addmargins( table( voice_speak.lmer$u_sing_stimmtraining ) )
addmargins( table( voice_speak.lmer$u_sing_stimmtraining_v ) )

addmargins( table( voice_speak.lmer$bmi.sds.cat ) )
addmargins( table( voice_speak.lmer$wind_instrument ) )
addmargins( table( voice_speak.lmer$wind_instrument_past ) )

# voice_speak.lmer <-
#     na.omit( voice_speak.lmer )

names( voice_speak.lmer )[ grep( "mot", names( voice_speak.lmer ) ) ]

voice_speak.lmer$SES <-
    cut( 
        voice_speak.lmer$C_WINKLER.SCORE_FAM,
        c( 3., 8.4, 15.4, 21. ),
        c( "LOW", "MIDDLE", "HIGH" ) )

i <- 
    4

model <-
        paste0( "st_sprech_", i, " ~ spl_sprech_", i, " + tanner + bmi.sds.cat + strain_past + training_past + wind_instrument_past + I( u_sing_singen_mot * u_sing_zaehl_mot ) + SES + ( 1 | sic ) + ( 1 | fam.id2 )" )

lmer.m <-
    lmerTest::lmer( model, voice_speak.lmer[ voice_speak.lmer$geschlecht == "m", ] )

lmer.f.1 <-
    lmerTest::lmer( st_sprech_1 ~ spl_sprech_2 + ( 1 | sic ) + ( 1 | fam.id2 ), voice_speak.lmer[ voice_speak.lmer$geschlecht == "f", ], REML = F )

lmer.f.2 <-
    lmerTest::lmer( st_sprech_1 ~ spl_sprech_2 + tanner + ( 1 | sic ) + ( 1 | fam.id2 ), voice_speak.lmer[ voice_speak.lmer$geschlecht == "f", ], REML = F )

for( i in profiles ) {
    
    model <-
        paste0( "st_sprech_", i, " ~ spl_sprech_", i, " + tanner + bmi.sds.cat + strain_past + training + wind_instrument + SES + ( 1 | sic ) + ( 1 | fam.id2 )" )
    
    print( 
        plot.lmer.coefficients.with.errorbars( 
            voice_speak.lmer, 
            model,
            title = paste0( "coefficients of linear regression of speaking voice ", endings[ i ] ),
            xlab  = "coefficients", 
            ylab  = "semi tones" ) )
    
    # ggsave( 
    #     filename = paste0( "betaPlotsLineareRegressionSpeakingVoice", ifelse( COL, "_Col_", "_BW_" ), endings[ i ], ".", ENDING ), 
    #     width = WDTH, 
    #     height = HGHT )
}

plts <-
    list( NA, NA, NA, NA )

for( i in profiles ) {
    
    model <-
        paste0( "st_sprech_", i, " ~ spl_sprech_", i, " + u_sing_singen_mot + strain_past + tanner + SES + wind_instrument + ( 1 | sic ) + ( 1 | fam.id2 )" )
    
    plts[[ i ]] <-
        plot.lmer.coefficients.with.errorbars( 
            voice_speak.lmer, 
            model,
            title = paste0( "coefficients of linear regression of speaking voice ", endings[ i ] ),
            xlab  = "coefficients", 
            ylab  = "semi tones" )

    # ggsave( 
    #     filename = paste0( "betaPlotsLineareRegressionSpeakingVoice", ifelse( COL, "_Col_", "_BW_" ), endings[ i ], ".", ENDING ), 
    #     width = WDTH, 
    #     height = HGHT )
}

ggsubplot( 
    plts[[ 1 ]],
    plts[[ 2 ]],
    plts[[ 3 ]],
    plts[[ 4 ]],
    cols = 2 )

i <- 
    2

model <-
        paste0( "st_sprech_", i, " ~ spl_sprech_", i, " + bmi.sds.cat + u_sing_singen_mot + tanner + wind_instrument + SES + ( 1 | sic ) + ( 1 | fam.id2 )" )

lmer.m <-
    lmerTest::lmer( model, data = voice_speak.lmer[ voice_speak.lmer$geschlecht == "m", ] )

anova( lmer.m )

summary( lmer.m )

lmer.f <-
    lmerTest::lmer( model, data = voice_speak.lmer[ voice_speak.lmer$geschlecht == "f", ] )

anova( lmer.f.1, lmer.f.2 )

summary( lmer.f )


View( voice_speak.lmer[ ,c( "sic", "gruppe", "datum", "wind_instrument", "u_sing_instr_v_1", "u_sing_instr_v_2", "u_sing_instr_v_3", "u_sing_instr_v_1", "u_sing_instr_v_2", "u_sing_instr_v_3" ) ] )


table( voice_speak.lmer[ , c( "training" ) ] )
table( voice_speak.lmer[ , c( "training_past" ) ] )

v <-
    data.frame( 
        table( 
            voice_speak.lmer[ , c( "u_sing_zaehl_mot", "u_sing_singen_mot" ) ] ) )

ggplot( v ) +
    geom_histogram( aes( u_sing_zaehl_mot, Freq, fill = u_sing_singen_mot ), stat = "identity", position = "dodge" ) +
    facet_grid( ~ u_sing_singen_mot )
    
ggplot( v ) +
    geom_point( aes( u_sing_zaehl_mot, u_sing_singen_mot, Freq, size = Freq, col = Freq ) ) +
    theme_bw( )

