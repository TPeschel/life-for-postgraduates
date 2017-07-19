plot.lm.coefficients.with.errorbars <-
    function( 
        dataframe, 
        formula, 
        title = "coefficients of linear regression",
        xlab = "dependent variable", 
        ylab = "independent variable",
        col = T ) {
        ## males
        lm.male.sum <-
            summary( 
                lm.male <-
                    lm( 
                        data = dataframe[ dataframe$sex == "m", ], 
                        formula = formula ) )
        lm.male.sum.confint <- 
            as.data.frame( 
                confint( 
                    lm.male, 
                    method = "Wald" ) )
        lm.male <-
            data.frame( 
                x    = rownames( lm.male.sum$coefficients ),
                y    = lm.male.sum$coefficients[ , 1 ],
                ymin = ymn <- lm.male.sum.confint[ 1 : nrow( lm.male.sum.confint ), 1 ],
                ymax = ymx <- lm.male.sum.confint[ 1 : nrow( lm.male.sum.confint ), 2 ],
                p    = c("-", "*")[ match( ymx * ymn < 0, c( T, F ) ) ] )
        lm.male$sex <- "male"
        # females
        lm.female.sum <-
            summary( 
                lm.female <-
                    lm( 
                        data = dataframe[ dataframe$sex == "f", ], 
                        formula = formula ) )
        lm.female.sum.confint <- 
            as.data.frame( 
                confint( 
                    lm.female, 
                    method = "Wald" ) )
        lm.female <- 
            data.frame( 
                x    = rownames( lm.female.sum$coefficients ),
                y    = lm.female.sum$coefficients[ , 1 ],
                ymin = ymn <- lm.female.sum.confint[ 1 : nrow( lm.female.sum.confint ), 1 ],
                ymax = ymx <- lm.female.sum.confint[ 1 : nrow( lm.female.sum.confint ), 2 ],
                p    = c("-", "*")[ match( ymx * ymn < 0, c( T, F ) ) ] )
        lm.female$sex <- "female"
        # both together
        lm.fm <- 
            rbind.data.frame( 
                lm.female, 
                lm.male )
        lm.fm <-
            lm.fm[ lm.fm$x != "(Intercept)", ]
        print( lm.fm )
        if( col ) {
            ggplot2::ggplot( data = lm.fm ) +
                ggplot2::geom_point(
                    ggplot2::aes( 
                        x, 
                        y, 
                        col = substr( x, 1, 2 ) ), 
                    size = 2 ) +
                ggplot2::geom_errorbar( 
                    ggplot2::aes( 
                        x, 
                        ymin = ymin, 
                        ymax = ymax, 
                        col = substr( x, 1, 2 ) 
                    ), 
                    width = .3, 
                    alpha = .75 ) + 
                ggplot2::coord_flip( ) +
                ggplot2::facet_grid( . ~ sex ) +
                ggplot2::scale_color_discrete( guide = F ) +
                #ggplot2::scale_y_continuous( breaks = seq( -8, 6, by = 2 ) ) +
                ggplot2::theme_bw( ) +
                ggplot2::geom_hline( 
                    yintercept = 0, 
                    linetype = 2 ) +
                ggplot2::labs( 
                    title = title, 
                    x = xlab, 
                    y = ylab ) +
                ggplot2::geom_text( 
                    ggplot2::aes( 
                        x, 
                        y, 
                        label = paste0( round( y, 2 ), " (", p, ")" ) ), 
                    size = 3, 
                    nudge_x = .3 ) } else {
                        ggplot2::ggplot( data = lm.fm ) +
                            ggplot2::geom_point(
                                ggplot2::aes( 
                                    x, 
                                    y ), 
                                size = 2 ) +
                            ggplot2::geom_errorbar( 
                                ggplot2::aes( 
                                    x, 
                                    ymin = ymin, 
                                    ymax = ymax ), 
                                width = .3, 
                                alpha = .75 ) + 
                            ggplot2::coord_flip( ) +
                            ggplot2::facet_grid( . ~ sex ) +
                            ggplot2::scale_color_discrete( guide = F ) +
                            #ggplot2::scale_y_continuous( breaks = seq( -8, 6, by = 2 ) ) +
                            ggplot2::theme_bw( ) +
                            ggplot2::geom_hline( 
                                yintercept = 0, 
                                linetype = 2 ) +
                            ggplot2::labs( 
                                title = title, 
                                x = xlab, 
                                y = ylab ) +
                            ggplot2::geom_text( 
                                ggplot2::aes( 
                                    x, 
                                    y, 
                                    label = paste0( round( y, 2 ), " (", p, ")" ) ), 
                                size = 3, 
                                nudge_x = .3 ) } }

