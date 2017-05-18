## delete all data
#rm( list = ls( ) )
warning = F

library( directlabels )
library( dplyr )
library( gamlss )
library( ggplot2 )
library( lifecuration )
library( lubridate )
library( readxl )
library( reshape2 )
library( svglite )
library(lme4)
library(effects)
library(multcomp)

load( file = "~/LIFE/github-tpeschel/life-for-postgraduates/ThomasBerger/results/refs.Rda" )
load( file = "~/LIFE/github-tpeschel/life-for-postgraduates/ThomasBerger/results/data.sprech.Rda" )
#load( file = "~/LIFE/github-tpeschel/R/ThomasBerger/results/refs.Rda" )
#load( file = "~/LIFE/github-tpeschel/R/ThomasBerger/results/data.sprech.Rda" )

d <- data.sprech

## -WINKLER ######################################################################################################################
d$Winkler <-
    cut( 
        d$SCORE_FAM,
        breaks = c( -Inf, 8.4, 15.4, Inf ),
        labels = c( "low", "mid", "high" ) )
## WINKLER- ######################################################################################################################



## -WINKLER F0-1 LIN REG #########################################################################################################
mm1.w <- lmer( sprech1_sds ~ sex/Winkler + (1|FAM_ID/SIC), data = d )

summary( glht( mm1.w ) )

plot( allEffects( mm1.w ) )

suppressWarnings(
    print(
        ggplot( d, aes( Winkler, sprech1_sds, col = sex ) ) +
            geom_point( alpha = .3, na.rm = T ) +
            facet_grid( sex ~ . ) +
            scale_color_manual( values =  c( "male" = "midnightblue", "female" = "deeppink4" ) ) +
            labs( title = "f0 Sprech 1 SDS vs. SES" , x = "SES", y = "f0-1 SDS [Hz]" ) +
            theme_bw( ) ) )

ggsave( "linreg.sprech1.f0.sds.against.winkler.points.png", width = 10, height = 8 )

suppressWarnings(
    print(
        ggplot( d, aes( Winkler, sprech1_sds, col = sex ) ) +
            geom_boxplot( alpha = .3, na.rm = T, notch = T ) +
            facet_grid( sex ~ . ) +
            scale_color_manual( values =  c( "male" = "midnightblue", "female" = "deeppink4" ) ) +
            labs( title = "f0 Sprech 1 SDS vs. SES" , x = "SES", y = "f0-1 SDS [Hz]" ) +
            theme_bw( ) ) )

ggsave( "linreg.sprech1.f0.sds.against.winkler.boxplot.png", width = 10, height = 8 )
## WINKLER F0-1 LIN REG- #########################################################################################################


## -SES F0-1 LIN REG #############################################################################################################
mm1 <- lmer( sprech1_sds ~ sex/SCORE_FAM + (1|FAM_ID/SIC), data = data.sprech )

summary( glht( mm1 ) )

plot( allEffects( mm1 ) )

suppressWarnings(
    print(
        ggplot( data.sprech, aes( SCORE_FAM, sprech1_sds, col = sex ) ) +
            geom_point( alpha = .3, na.rm = T ) +
            geom_smooth( method = "gam", na.rm = T ) +
            facet_grid( sex ~ . ) +
            scale_color_manual( values =  c( "male" = "midnightblue", "female" = "deeppink4" ) ) +
            labs( title = "f0 Sprech 1 SDS vs. SES" , x = "SES", y = "f0-1 SDS [Hz]" ) +
            theme_bw( ) ) )

ggsave( "linreg.sprech1.f0.sds.against.famscore.png", width = 10, height = 8 )
## SES F0-1 LIN REG- #############################################################################################################


## -WINKLER F0-2 LIN REG #########################################################################################################
mm2.w <- lmer( sprech2_sds ~ sex/Winkler + (1|FAM_ID/SIC), data = d )

summary( glht( mm2.w ) )

plot( allEffects( mm2.w ) )

suppressWarnings(
    print(
        ggplot( d, aes( Winkler, sprech2_sds, col = sex ) ) +
            geom_point( alpha = .3, na.rm = T ) +
            facet_grid( sex ~ . ) +
            scale_color_manual( values =  c( "male" = "midnightblue", "female" = "deeppink4" ) ) +
            labs( title = "f0 Sprech 2 SDS vs. SES" , x = "SES", y = "f0-2 SDS [Hz]" ) +
            theme_bw( ) ) )

ggsave( "linreg.sprech2.f0.sds.against.winkler.points.png", width = 10, height = 8 )

suppressWarnings(
    print(
        ggplot( d, aes( Winkler, sprech2_sds, col = sex ) ) +
            geom_boxplot( alpha = .3, na.rm = T, notch = T ) +
            facet_grid( sex ~ . ) +
            scale_color_manual( values =  c( "male" = "midnightblue", "female" = "deeppink4" ) ) +
            labs( title = "f0 Sprech 2 SDS vs. SES" , x = "SES", y = "f0-2 SDS [Hz]" ) +
            theme_bw( ) ) )

ggsave( "linreg.sprech2.f0.sds.against.winkler.boxplot.png", width = 10, height = 8 )
## WINKLER F0-2 LIN REG- #########################################################################################################

## -SES F0-2 LIN REG #############################################################################################################
mm2 <- lmer( sprech2_sds ~ sex/SCORE_FAM + (1|FAM_ID/SIC), data = data.sprech )

summary( glht( mm2 ) )

plot( allEffects( mm2 ) )

suppressWarnings(
    print(
        ggplot( data.sprech, aes( SCORE_FAM, sprech2_sds, col = sex ) ) +
            geom_point( alpha = .3, na.rm = T ) +
            geom_smooth( method = "gam", na.rm = T ) +
            facet_grid( sex ~ . ) +
            scale_color_manual( values =  c( "male" = "midnightblue", "female" = "deeppink4" ) ) +
            labs( title = "f0 Sprech 2 SDS vs. SES" , x = "SES", y = "f0-2 SDS [Hz]" ) +
            theme_bw( ) ) )

ggsave( "linreg.sprech2.f0.sds.against.famscore.png", width = 10, height = 8 )
## SES F0-2 LIN REG- #############################################################################################################



## -WINKLER F0-3 LIN REG #########################################################################################################
mm3.w <- lmer( sprech3_sds ~ sex/Winkler + (1|FAM_ID/SIC), data = d )

summary( glht( mm3.w ) )

plot( allEffects( mm3.w ) )

suppressWarnings(
    print(
        ggplot( d, aes( Winkler, sprech3_sds, col = sex ) ) +
            geom_point( alpha = .3, na.rm = T ) +
            facet_grid( sex ~ . ) +
            scale_color_manual( values =  c( "male" = "midnightblue", "female" = "deeppink4" ) ) +
            labs( title = "f0 Sprech 3 SDS vs. SES" , x = "SES", y = "f0-3 SDS [Hz]" ) +
            theme_bw( ) ) )

ggsave( "linreg.sprech3.f0.sds.against.winkler.points.png", width = 10, height = 8 )

suppressWarnings(
    print(
        ggplot( d, aes( Winkler, sprech3_sds, col = sex ) ) +
            geom_boxplot( alpha = .3, na.rm = T, notch = T ) +
            facet_grid( sex ~ . ) +
            scale_color_manual( values =  c( "male" = "midnightblue", "female" = "deeppink4" ) ) +
            labs( title = "f0 Sprech 3 SDS vs. SES" , x = "SES", y = "f0-3 SDS [Hz]" ) +
            theme_bw( ) ) )

ggsave( "linreg.sprech3.f0.sds.against.winkler.boxplot.png", width = 10, height = 8 )
## WINKLER F0-3 LIN REG- #########################################################################################################


## -SES F0-3 LIN REG #############################################################################################################
mm3 <- lmer( sprech3_sds ~ sex/SCORE_FAM + (1|FAM_ID/SIC), data = data.sprech )

summary( glht( mm3 ) )

plot( allEffects( mm3 ) )

suppressWarnings(
    print(
        ggplot( data.sprech, aes( SCORE_FAM, sprech3_sds, col = sex ) ) +
            geom_point( alpha = .3, na.rm = T ) +
            geom_smooth( method = "gam", na.rm = T ) +
            facet_grid( sex ~ . ) +
            scale_color_manual( values =  c( "male" = "midnightblue", "female" = "deeppink4" ) ) +
            labs( title = "f0 Sprech 3 SDS vs. SES" , x = "SES", y = "f0-3 SDS [Hz]" ) +
            theme_bw( ) ) )

ggsave( "linreg.sprech3.f0.sds.against.famscore.png", width = 10, height = 8 )
## SES F0-3 LIN REG- #############################################################################################################


## -WINKLER F0-4 LIN REG #########################################################################################################
mm4.w <- lmer( sprech4_sds ~ sex/Winkler + (1|FAM_ID/SIC), data = d )

summary( glht( mm4.w ) )

plot( allEffects( mm4.w ) )

suppressWarnings(
    print(
        ggplot( d, aes( Winkler, sprech4_sds, col = sex ) ) +
            geom_point( alpha = .3, na.rm = T ) +
            facet_grid( sex ~ . ) +
            scale_color_manual( values =  c( "male" = "midnightblue", "female" = "deeppink4" ) ) +
            labs( title = "f0 Sprech 4 SDS vs. SES" , x = "SES", y = "f0-4 SDS [Hz]" ) +
            theme_bw( ) ) )

ggsave( "linreg.sprech4.f0.sds.against.winkler.points.png", width = 10, height = 8 )

suppressWarnings(
    print(
        ggplot( d, aes( Winkler, sprech4_sds, col = sex ) ) +
            geom_boxplot( alpha = .3, na.rm = T, notch = T ) +
            facet_grid( sex ~ . ) +
            scale_color_manual( values =  c( "male" = "midnightblue", "female" = "deeppink4" ) ) +
            labs( title = "f0 Sprech 4 SDS vs. SES" , x = "SES", y = "f0-4 SDS [Hz]" ) +
            theme_bw( ) ) )

ggsave( "linreg.sprech4.f0.sds.against.winkler.boxplot.png", width = 10, height = 8 )
## WINKLER F0-4 LIN REG- #########################################################################################################


## -SES F0-4 LIN REG #############################################################################################################
mm4 <- lmer( sprech4_sds ~ sex/SCORE_FAM + (1|FAM_ID/SIC), data = data.sprech )

summary( glht( mm4 ) )

plot( allEffects( mm4 ) )

suppressWarnings(
    print(
        ggplot( data.sprech, aes( SCORE_FAM, sprech4_sds, col = sex ) ) +
            geom_point( alpha = .3, na.rm = T ) +
            geom_smooth( method = "gam", na.rm = T ) +
            facet_grid( sex ~ . ) +
            scale_color_manual( values =  c( "male" = "midnightblue", "female" = "deeppink4" ) ) +
            labs( title = "f0 Sprech 4 SDS vs. SES" , x = "SES", y = "f0-4 SDS [Hz]" ) +
            theme_bw( ) ) )

ggsave( "linreg.sprech4.f0.sds.against.famscore.png", width = 10, height = 8 )
## SES F0-4 LIN REG- #############################################################################################################

