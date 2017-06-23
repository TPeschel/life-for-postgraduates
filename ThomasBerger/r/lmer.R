## delete all data
rm( list = ls( ) )

## change working dir to data/results
setwd( "~/LIFE/life-for-postgraduates/ThomasBerger/results/" )

load( "~/LIFE/life-for-postgraduates/ThomasBerger/original/Stimme/Stimme-Dateien/save/voice_clean.Rda" )

plot.lmer.coefficients.with.errorbars <-
    function( dataframe, formula ) {
        library( ggplot2 )
        library( lme4 )
        lmer.male <-
            summary( lmer( data = dataframe[ dataframe$geschlecht == "m", ], formula = formula ) )
        lmer.male <-
            data.frame( 
                x    = rownames( lmer.male$coefficients ),
                y    = lmer.male$coefficients[ , 1 ],
                ymin = lmer.male$coefficients[ , 1 ] - lmer.male$coefficients[ , 2 ],
                ymax = lmer.male$coefficients[ , 1 ] + lmer.male$coefficients[ , 2 ],
                p    = lmer.male$coefficients[ , 3 ] )
        lmer.female <-
            summary( lmer( data = dataframe[ dataframe$geschlecht == "f", ], formula = formula ) )
        lmer.female <- 
            data.frame( 
                x    = rownames( lmer.female$coefficients ),
                y    = lmer.female$coefficients[ , 1 ],
                ymin = lmer.female$coefficients[ , 1 ] - lmer.female$coefficients[ , 2 ],
                ymax = lmer.female$coefficients[ , 1 ] + lmer.female$coefficients[ , 2 ],
                p    = lmer.female$coefficients[ , 3 ] )
        lmer.female$sex <- "female"
        lmer.male$sex <- "male"
        lmer.fm <- rbind.data.frame( lmer.male, lmer.female )
        lmer.fm <-
            lmer.fm[ lmer.fm$x != "(Intercept)", ]
        ggplot( data = lmer.fm ) +
            geom_point( aes( x, y, col = substr( x, 1, 5 ) ) ) +
            geom_errorbar( aes( x, ymin = ymin, ymax = ymax, col = substr( x, 1, 5 ) ), width = .3, alpha = .5 ) + 
            coord_flip( ) +
            facet_grid( . ~ sex ) +
            scale_color_discrete( guide = F ) +
            scale_y_continuous( breaks = seq( -8, 6, by = 2 ) ) +
            theme_bw( ) +
            geom_hline( yintercept = 0, linetype = 2 ) }

library( life.helper )

ggsubplot( 
    plot.lmer.coefficients.with.errorbars( voice, st_sprech_1 ~ tanner + spl_sprech_1 + ses.cat + bmi.sds + bmi.sds.cat + ( 1 | sic ) + ( 1 | fam.id2 ) ),
    plot.lmer.coefficients.with.errorbars( voice, st_sprech_2 ~ tanner + spl_sprech_2 + ses.cat + bmi.sds + bmi.sds.cat + ( 1 | sic ) + ( 1 | fam.id2 ) ),
    plot.lmer.coefficients.with.errorbars( voice, st_sprech_3 ~ tanner + spl_sprech_3 + ses.cat + bmi.sds + bmi.sds.cat + ( 1 | sic ) + ( 1 | fam.id2 ) ),
    plot.lmer.coefficients.with.errorbars( voice, st_sprech_4 ~ tanner + spl_sprech_4 + ses.cat + bmi.sds + bmi.sds.cat + ( 1 | sic ) + ( 1 | fam.id2 ) ),
    cols = 2 )
    
voice$st_s
