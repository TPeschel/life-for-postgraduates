## delete all data
rm( list = ls( ) )
library( life.helper )

## change working dir to data/results
setwd( "~/LIFE/life-for-postgraduates/ThomasBerger/results/" )

load( "~/LIFE/life-for-postgraduates/ThomasBerger/original/Stimme/Stimme-Dateien/save/voice_clean.Rda" )

voice_speak$bmi.sds.cat <-
    relevel( voice_speak$bmi.sds.cat, "normal" )

voice$bmi.sds.cat <-
    relevel( voice$bmi.sds.cat, "normal" )

plot.lmer.coefficients.with.errorbars <-
    function( dataframe, formula, title = "coefficients of linear regression of speaking voice II", xlab = "dependent variables", ylab = "independent variables" ) {
        dataframe <- voice_speak
        formula = st_sprech_1 ~ spl_sprech_1 + tanner + (1|sic) + (1|fam.id2 )
        library( ggplot2 )
        library( lme4 )
        ## males
        lmer.male.sum <-
            summary( 
                lmer.male <-
                    lmer( data = dataframe[ dataframe$geschlecht == "m", ], formula = formula ) )
        lmer.male.sum.confint <- 
            as.data.frame( confint( lmer.male, method = "Wald" ) )
        lmer.male <-
            data.frame( 
                x    = rownames( lmer.male.sum$coefficients ),
                y    = lmer.male.sum$coefficients[ , 1 ],
                ymin = lmer.male.sum.confint[ 4 : nrow( lmer.male.sum.confint ), 1 ],
                ymax = lmer.male.sum.confint[ 4 : nrow( lmer.male.sum.confint ), 2 ],
                p    = round( lmer.male.sum$coefficients[ , 3 ], 2 ) )
        lmer.male$sex <- "male"
        # females
        lmer.female.sum <-
            summary( 
                lmer.female <-
                    lmer( data = dataframe[ dataframe$geschlecht == "f", ], formula = formula ) )
        lmer.female.sum.confint <- 
            as.data.frame( confint( lmer.female, method = "Wald" ) )
        lmer.female <- 
            data.frame( 
                x    = rownames( lmer.female.sum$coefficients ),
                y    = lmer.female.sum$coefficients[ , 1 ],
                ymin = lmer.female.sum.confint[ 4 : nrow( lmer.female.sum.confint ), 1 ],
                ymax = lmer.female.sum.confint[ 4 : nrow( lmer.female.sum.confint ), 2 ],
                p    = round( lmer.female.sum$coefficients[ , 3 ], 2 ) )
        lmer.female$sex <- "female"
        # both together
        lmer.fm <- 
            rbind.data.frame( lmer.female, lmer.male )
        lmer.fm <-
            lmer.fm[ lmer.fm$x != "(Intercept)", ]
        print( lmer.fm )
        ggplot( data = lmer.fm ) +
            geom_point( aes( x, y, col = substr( x, 1, 6 ) ), size = 2 ) +
            geom_errorbar( aes( x, ymin = ymin, ymax = ymax, col = substr( x, 1, 6 ) ), width = .3, alpha = .75 ) + 
            coord_flip( ) +
            facet_grid( . ~ sex ) +
            scale_color_discrete( guide = F ) +
            scale_y_continuous( breaks = seq( -8, 6, by = 2 ) ) +
            theme_bw( ) +
            geom_hline( yintercept = 0, linetype = 2 ) +
            labs( title = title, x = xlab, y = ylab ) +
            geom_text( aes( x, y, label = paste0( round( y, 2 ), " (p:", p, ")" ) ), size = 3, nudge_x = .3 ) }

voice_speak$tanner <- 
    factor(
        voice_speak$tanner,
        levels = c( 1: 5 ) )
ggsubplot( 
    plot.lmer.coefficients.with.errorbars( voice_speak, 
        st_sprech_1 ~ tanner + spl_sprech_1 + ses.cat + bmi.sds.cat + stimmerfahrung + u_sing_stimmbelastg +  u_sing_stimmbelastg_v + ( 1 | sic ) + ( 1 | fam.id2 ),
        title = "coefficients of linear regression of speaking voice I",
        xlab = "coefficients", 
        ylab = "speaking voice I" ),
    plot.lmer.coefficients.with.errorbars( voice_speak, 
        st_sprech_2 ~ tanner + spl_sprech_2 + ses.cat + bmi.sds.cat + stimmerfahrung + u_sing_stimmbelastg +  u_sing_stimmbelastg_v +( 1 | sic ) + ( 1 | fam.id2 ),
        title = "coefficients of linear regression of speaking voice II",
        xlab = "coefficients", 
        ylab = "speaking voice II " ),
    cols = 1 )

l.null <-
    lmer( data = voice_speak, st_sprech_1 ~ + ( 1 | sic ) + ( 1 | fam.id2 ), REML = F )

l.model <-
    lmer( data = voice_speak, st_sprech_1 ~ tanner + spl_sprech_1 + ses.cat + bmi.sds.cat + stimmerfahrung + u_sing_stimmbelastg +  u_sing_stimmbelastg_v + ( 1 | sic ) + ( 1 | fam.id2 ), REML = F )

stats::anova( l.null, l.model )
