library( "foreign" )
library( "ggplot2" )

setwd( "~/LIFE/life-for-postgraduates/TanjaPoulain/" )

mt <-
    as.data.frame( read.spss( "data/fuer_thomas_datei.sav" ) )

mt <-
    na.omit(
        mt[ , c( "Alter", "sex", "SCORE_FAM", "FB_SSSC_BP_NOVELTY_1" ) ] )

ggplot( mt, aes( Alter, SCORE_FAM, col = sex ) ) + 
    geom_point( ) + 
    geom_smooth( method = "lm" ) + 
    theme_bw( ) + 
    facet_grid( sex ~ . )

mt$SCORE_FAM_CNTRL.for.Alter.and.sex <-
    ( lm( SCORE_FAM ~ Alter * sex, mt ) )$residuals

ggplot( mt, aes( Alter, SCORE_FAM_CNTRL.for.Alter.and.sex, col = sex ) ) + 
    geom_point( ) + 
    geom_smooth( method = "lm" ) + 
    theme_bw( ) + 
    facet_grid( sex ~ . )

ggplot( mt, aes( Alter, FB_SSSC_BP_NOVELTY_1, col = sex ) ) + 
    geom_point( ) + 
    geom_smooth( method = "lm" ) + 
    theme_bw( ) + 
    facet_grid( sex ~ . )

mt$FB_SSSC_BP_NOVELTY_1_CNTRL.for.Alter.and.sex <-
    ( lm( FB_SSSC_BP_NOVELTY_1 ~ Alter * sex, mt ) )$residuals

ggplot( mt, aes( Alter, FB_SSSC_BP_NOVELTY_1_CNTRL.for.Alter.and.sex, col = sex ) ) + 
    geom_point( ) + 
    geom_smooth( method = "lm" ) + 
    theme_bw( ) + 
    facet_grid( sex ~ . )
