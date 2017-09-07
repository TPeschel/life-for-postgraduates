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
<<<<<<< HEAD
        "reshape2",
        "gamlss") )   

sds <-
    function( y, x, f = c( "NO", "BCPEo", "BCCGo", "BCTo" ) ) {
        lms. <-
            lms( y, x, families = f, cent = c( ), points = F  )
        lms.
    }

=======
        "gamlss" ) )   
>>>>>>> 50b2678b61db20ea6ddf312751609970334e3a2d
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


mt.f <-
    mt[ mt$SEX == "female", ]

mt.m <-
    mt[ mt$SEX == "male", ]

sds.f <-
    sds( mt.f$HEIGHT, mt.f$AGE, "BCTo" )

sds.m <-
    sds( mt.m$HEIGHT, mt.m$AGE, "BCTo" )

mt.f$HGHT.SDS <-
    sds.f$residuals

mt.m$HGHT.SDS <-
    sds.m$residuals

mt. <-
    rbind( mt.f, mt.m )

ggsubplot(
    ggplot( mt., aes( AGE, HGHT.SDS, col = SEX ) ) + 
        theme_bw( ) + 
        scale_color_manual( values = c( "deeppink", "deepskyblue" ) ) +
        geom_point( ) +
        geom_smooth( ) +
        facet_grid( . ~ SEX ),
    ggplot( mt., aes( AGE, HEIGHT.ADJ, col = SEX ) ) + 
        theme_bw( ) + 
        scale_color_manual( values = c( "deeppink", "deepskyblue" ) ) +
        geom_point( ) +
        geom_smooth( ) +
        facet_grid( . ~ SEX ),
    cols = 2 )

ggsubplot(
    ggplot( mt., aes( AGE, HGHT.SDS, col = SEX ) ) + 
        theme_bw( ) + 
        scale_color_manual( values = c( "deeppink", "deepskyblue" ) ) +
        geom_point( ) +
        geom_smooth( ) +
        facet_grid( . ~ SEX ),
    ggplot( mt., aes( AGE, HEIGHT.ADJ, col = SEX ) ) + 
        theme_bw( ) + 
        scale_color_manual( values = c( "deeppink", "deepskyblue" ) ) +
        geom_point( ) +
        geom_smooth( ) +
        facet_grid( . ~ SEX ),
    cols = 2 )

ages <-
    seq( from = 0, to = 20, by = 1 / 12 )

<<<<<<< HEAD
prms.f <-
    as.data.frame( predict( sds.f, newdata = ages ) )

prms.f$sex <-
    "female"

prms.m <-
    as.data.frame( predict( sds.m, newdata = ages ) )

prms.m$sex <-
    "male"

prms <-
    rbind( prms.f, prms.m )

prms$age <-
    rep( ages, 2 )

ggsubplot(
    ggplot( prms ) + theme_bw( ) + scale_color_manual( values = sex.colors, guide = F ) +
        geom_line( aes( age, mu, col = sex ) ),
    ggplot( prms ) + theme_bw( ) + scale_color_manual( values = sex.colors, guide = F ) +
        geom_line( aes( age, sigma, col = sex ) ),
    ggplot( prms ) + theme_bw( ) + scale_color_manual( values = sex.colors, guide = F ) +
        geom_line( aes( age, nu, col = sex ) ),
    ggplot( prms ) + theme_bw( ) + scale_color_manual( values = sex.colors, guide = F ) +
        geom_line( aes( age, tau, col = sex ) ),
    cols = 2 )
    
mt.f$HGHT.SDS <-
    ( sds( y = mt.f$HEIGHT[ mt.f$AGE < 1.25 ], x = mt.f$AGE[ mt.f$AGE < 1.25 ] ) )$residuals    

mt.m$HGHT.SDS <-
    ( sds( y = mt.m$HEIGHT[ mt.m$AGE < 1.25 ], x = mt.m$AGE[ mt.m$AGE < 1.25 ] ) )$residuals

mt. <-
    rbind(
        mt.f,
        mt.m )

ggsubplot(
    ggplot( mt., aes( AGE, HEIGHT.ADJ, col = SEX ) ) + theme_bw( ) + scale_color_manual( values = sex.colors, guide = F ) + facet_grid( . ~ SEX ) +
        geom_point( ) + geom_smooth( ),
    ggplot( mt., aes( AGE, HGHT.SDS, col = SEX ) ) + theme_bw( ) + scale_color_manual( values = sex.colors, guide = F ) + facet_grid( . ~ SEX ) +
        geom_point(  ) + geom_smooth( ),
    cols = 2 )


ggsubplot(
    ggplot( mt., aes( HEIGHT.ADJ, log10CT, col = SEX ) ) + theme_bw( ) + scale_color_manual( values = sex.colors, guide = F ) + facet_grid( . ~ SEX ) +
        geom_point( ) + geom_smooth( ),
    ggplot( mt., aes( HGHT.SDS, log10CT, col = SEX ) ) + theme_bw( ) + scale_color_manual( values = sex.colors, guide = F ) + facet_grid( . ~ SEX ) +
        geom_point(  ) + geom_smooth( ),
    cols = 1 )

ggsubplot(
    ggplot( mt., aes( HEIGHT.ADJ, log10CT, col = SEX ) ) + theme_bw( ) + scale_color_manual( values = sex.colors, guide = F ) + facet_grid( . ~ SEX ) +
        geom_point( ) + geom_smooth( method = "lm" ),
    ggplot( mt., aes( HGHT.SDS, log10CT, col = SEX ) ) + theme_bw( ) + scale_color_manual( values = sex.colors, guide = F ) + facet_grid( . ~ SEX ) +
        geom_point(  ) + geom_smooth( method = "lm" ),
    cols = 1 )

ggsubplot(
    ggplot( mt., aes( HEIGHT.ADJ, log10CT, col = SEX ) ) + theme_bw( ) + scale_color_manual( values = sex.colors, guide = F ) +
        geom_point( ) + geom_smooth( method = "lm" ),
    ggplot( mt., aes( HGHT.SDS, log10CT, col = SEX ) ) + theme_bw( ) + scale_color_manual( values = sex.colors, guide = F ) + 
        geom_point(  ) + geom_smooth( method = "lm" ),
    cols = 2 )

summary(
    lm.sds <-
        lm( log10CT ~ HGHT.SDS * SEX, mt. ) )

summary(
    lm.adj <-
        lm( log10CT ~ HEIGHT.ADJ * SEX, mt. ) )

summary(
    lm.sds <-
        lm( log10CT ~ HGHT.SDS + SEX, mt. ) )

summary(
    lm.adj <-
        lm( log10CT ~ HEIGHT.ADJ + SEX, mt. ) )


mt.f$log10CT.adj.for.AGE <-
    ( sds( mt.f$log10CT, mt.f$AGE ) )$residuals

mt.m$log10CT.adj.for.AGE <-
    ( sds( mt.f$log10CT, mt.f$AGE ) )$residuals
