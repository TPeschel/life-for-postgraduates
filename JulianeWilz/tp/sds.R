##
# clear memory
##
rm( list = ls( ) ) #

##
# install devtools if necessary
##
if( ! "devtools" %in% rownames( installed.packages( ) ) )
    install.packages( "devtools" )

##
# install helper for life
##
devtools::install_github( "TPeschel/hlpr4life" )

##
# load necessary r-packages
##
hlpr4life::load.pkgs(
    c(
        "hlpr4life",
        "readxl", 
        "WriteXLS",   
        "ggplot2",    
        "lsr",        
        "dplyr",      
        "Hmisc",
        "MASS",
        "gamlss" ) )   
##
# set working directory
##
setwd( "~/LIFE/life-for-postgraduates/JulianeWilz/" )

##
# read in main table: mt
##
mt <-
    read_excel( "data/NeueTabelle170517excel.xlsx" )

##
# show warnings
##
warnings( )

# 5425 obs

##
# rename ugly column names
##
mt <-
    rename.columns( 
        mt,
        c( 
            "TEILNEHMER_GESCHLECHT",
            "AGE_Calcitionin",
            "CT_S_1_NUM_VALUE",
            "C_ANTHRO_KH_HEIGHT_ORIG",
            "C_ANTHRO_KH_WEIGHT_ORIG",
            "C_ANTHRO_KH_BMI_ORIG",
            "C_ANTHRO_KH_HEIGHT_ADJ",
            "C_ANTHRO_KH_WEIGHT_ADJ",
            "C_ANTHRO_KH_BMI_ADJ" ),
        c( 
            "SEX",
            "AGE",
            "CALCITONIN",
            "HEIGHT",
            "WEIGHT",
            "BMI",
            "HEIGHT.ADJ",
            "WEIGHT.ADJ",
            "BMI.ADJ" ) )

##
# show structure of mt
##
str( mt )

##
# CALCITONIN should be numeric
# SEX should be a factor
##
mt$CALCITONIN <-
    as.numeric( mt$CALCITONIN )


##
# only complete observations of
# all thinkable and possible variables as:
# sex, age, height, weight, calcitonin
##
mt <-
    na.omit(
        mt[ , c(
            "SEX",
            "AGE",
            "CALCITONIN",
            "HEIGHT",
            "WEIGHT",
            "BMI",
            "HEIGHT.ADJ",
            "WEIGHT.ADJ",
            "BMI.ADJ" ) ] )
    
# 5387 obs left

mt$AGE <-
    as.numeric( mt$AGE )

mt$SEX <-
    factor( as.character( mt$SEX ) )

##
# plot problems withs sds values
##
ggsubplot(
    ggplot( mt, aes( AGE, HEIGHT.ADJ ) ) + geom_point( alpha = .2 ) + geom_smooth( method = "lm" ) + theme_bw( ),
    ggplot( mt, aes( AGE, HEIGHT.ADJ ) ) + geom_point( alpha = .2) + geom_smooth( method = "lm" ) + theme_bw( ) + facet_grid( SEX ~ . ),
    cols = 2 )

ggsubplot(
    ggplot( mt, aes( AGE, WEIGHT.ADJ ) ) + geom_point( alpha = .2 ) + geom_smooth( method = "lm" ) + theme_bw( ),
    ggplot( mt, aes( AGE, WEIGHT.ADJ ) ) + geom_point( alpha = .2) + geom_smooth( method = "lm" ) + theme_bw( ) + facet_grid( SEX ~ . ),
    cols = 2 )

ggsubplot(
    ggplot( mt, aes( AGE, BMI.ADJ ) ) + geom_point( alpha = .2 ) + geom_smooth( method = "lm" ) + theme_bw( ),
    ggplot( mt, aes( AGE, BMI.ADJ ) ) + geom_point( alpha = .2) + geom_smooth( method = "lm" ) + theme_bw( ) + facet_grid( SEX ~ . ),
    cols = 2 )

##
# try to build our own sds values
##

##
# height sds
##
mt$HEIGHT.SDS <-
    ( lm( HEIGHT ~ AGE * SEX, mt ) )$residuals

mt$HEIGHT.SDS <-
    mt$HEIGHT.SDS / sd( mt$HEIGHT.SDS )

ggsubplot(
    ggplot( mt, aes( AGE, HEIGHT ) ) + geom_point( alpha = .2 ) + geom_smooth( method = "lm" ) + theme_bw( ),
    ggplot( mt, aes( AGE, HEIGHT.ADJ ) ) + geom_point( alpha = .2 ) + geom_smooth( method = "lm" ) + theme_bw( ),
    ggplot( mt, aes( AGE, HEIGHT.SDS ) ) + geom_point( alpha = .2 ) + geom_smooth( method = "lm" ) + theme_bw( ),
    ggplot( mt, aes( AGE, HEIGHT ) ) + geom_point( alpha = .2 ) + geom_smooth( method = "lm" ) + theme_bw( ) + facet_grid( SEX ~ . ),
    ggplot( mt, aes( AGE, HEIGHT.ADJ ) ) + geom_point( alpha = .2) + geom_smooth( method = "lm" ) + theme_bw( ) + facet_grid( SEX ~ . ),
    ggplot( mt, aes( AGE, HEIGHT.SDS ) ) + geom_point( alpha = .2) + geom_smooth( method = "lm" ) + theme_bw( ) + facet_grid( SEX ~ . ),
    cols = 2 )

##
# box cox for height
##
bc.height <-
    boxcox( lm( HEIGHT ~ AGE * SEX, mt ) )

xp <-
    bc.height$x[ which.max( bc.height$y ) ]

powerTransform <- 
    function( y, lambda1, lambda2 = NULL, method = "boxcox" ) {
    
    boxcoxTrans <-
        function( x, lam1, lam2 = NULL ) {
        
        # if we set lambda2 to zero, it becomes the one parameter transformation
        lam2 <- 
            ifelse( is.null( lam2 ), 0, lam2 )
        
        if( lam1 == 0L ) {
            log( y + lam2 )
        } else {
            ( ( ( y + lam2 ) ^ lam1 ) - 1 ) / lam1
        }
    }
    
    switch(
        method,
        boxcox = boxcoxTrans( y, lambda1, lambda2 ),
        tukey = y ^ lambda1 ) }

mt$HEIGHT.BOX.COX <-
    powerTransform( mt$HEIGHT, xp )

ggsubplot(
    ggplot( mt, aes( AGE, HEIGHT ) ) + geom_point( alpha = .2 ) + geom_smooth( method = "lm" ) + theme_bw( ),
    ggplot( mt, aes( AGE, HEIGHT ) ) + geom_point( alpha = .2 ) + geom_smooth( method = "lm" ) + theme_bw( ) + facet_grid( SEX ~ . ),
    ggplot( mt, aes( AGE, HEIGHT.BOX.COX ) ) + geom_point( alpha = .2 ) + geom_smooth( method = "lm" ) + theme_bw( ),
    ggplot( mt, aes( AGE, HEIGHT.BOX.COX ) ) + geom_point( alpha = .2 ) + geom_smooth( method = "lm" ) + theme_bw( ) + facet_grid( SEX ~ . ),
    cols = 2 )

ggsubplot(
    ggplot( mt, aes( AGE, HEIGHT ) ) + geom_point( alpha = .2 ) + geom_smooth( ) + theme_bw( ),
    ggplot( mt, aes( AGE, HEIGHT ) ) + geom_point( alpha = .2 ) + geom_smooth( ) + theme_bw( ) + facet_grid( SEX ~ . ),
    ggplot( mt, aes( AGE, HEIGHT.BOX.COX ) ) + geom_point( alpha = .2 ) + geom_smooth( ) + theme_bw( ),
    ggplot( mt, aes( AGE, HEIGHT.BOX.COX ) ) + geom_point( alpha = .2 ) + geom_smooth( ) + theme_bw( ) + facet_grid( SEX ~ . ),
    cols = 2 )

##
# Solution is Discrete-Fourier-Trafo of GAM Fits
# Then build for all Oscillations the Taylor-Function 
# up to a certain degree
# add them and use the resilting polynomial for
# creating real sds values from gam
##

ggsubplot(
    ggplot( mt, aes( AGE, HEIGHT ) ) + geom_point( alpha = .2 ) + geom_smooth( ) + theme_bw( ),
    ggplot( mt ) + geom_histogram( aes( AGE ), alpha = .2 ) + geom_point( aes( AGE, HEIGHT ), alpha = .2 ) + geom_smooth( aes( AGE, HEIGHT ) ) + theme_bw( ) + facet_grid( SEX ~ . ),
    ggplot( mt, aes( AGE, HEIGHT.BOX.COX ) ) + geom_point( alpha = .2 ) + geom_smooth( ) + theme_bw( ),
    ggplot( mt  ) + geom_histogram( aes( AGE ), alpha = .2 ) + geom_point( aes( AGE, HEIGHT.BOX.COX ), alpha = .2 ) + geom_smooth( aes( AGE, HEIGHT.BOX.COX ) ) + theme_bw( ) + facet_grid( SEX ~ . ),
    cols = 2 )






g <- 
    gamlss( HEIGHT ~ AGE * SEX, data = mt,family = "BCCG" )
str( g )
plot( mt$AGE, g$y )

fft(  )
