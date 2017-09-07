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
        "reshape2",
        "gamlss") )   

l.m.s <-
    function( y, x, f = c( "NO", "BCPEo", "BCCGo", "BCTo" ) ) {
        
        lms( y, x, families = f, cent = c( 1, 2.5, 5, 10, 25, 50, 75, 90, 95, 97.5, 99 )  ) }

sds <-
    function( y, x, f = c( "NO", "BCPEo", "BCCGo", "BCTo" ) ) {
        
        ( l.m.s( y, x, f ) )$residuals }

##
# set working directory
##
setwd( "~/LIFE/life-for-postgraduates/JulianeWilz/" )

##
# read in main table: mt
##
mt <-
    read_excel( "sent/data.2017.08.31/AktuelleTabelle190517excel.xlsx" )

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
            "C_ANTHRO_KH_BMI_ADJ",
            "OSTEO_S_NUM_VALUE"),
        c( 
            "SEX",
            "AGE",
            "CALCITONIN",
            "HEIGHT",
            "WEIGHT",
            "BMI",
            "HEIGHT.ADJ",
            "WEIGHT.ADJ",
            "BMI.ADJ",
            "OSTEO" ) )

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

mt$SEX <-
    as.factor( mt$SEX )

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
            "BMI.ADJ",
            "OSTEO" ) ] )
    
# 5387 obs left

mt$AGE <-
    as.numeric( mt$AGE )

mt$log10CT <-
    log10( mt$CALCITONIN )

##
# plot problems withs sds values
##

sex.colors <-
    c( "deeppink", "deepskyblue" )

sex.min.colors <-
    c( "deeppink", "green", "deepskyblue", "red" )

##
# our population is not representative
##
ggsubplot(
    ggplot( mt, aes( AGE, HEIGHT.ADJ ) ) + theme_bw( ) +
        geom_point( alpha = .1 ) + geom_smooth( method = "lm" ),
    ggplot( mt, aes( AGE, HEIGHT.ADJ, col = SEX ) ) + theme_bw( ) + facet_grid( SEX ~ . ) + scale_color_manual( values = sex.colors, guide = F ) +
        geom_point( alpha = .1) + geom_smooth( method = "lm" ) ,
    ggplot( mt, aes( AGE, WEIGHT.ADJ ) ) + theme_bw( ) +
        geom_point( alpha = .1 ) + geom_smooth( method = "lm" ),
    ggplot( mt, aes( AGE, WEIGHT.ADJ, col = SEX ) ) + theme_bw( ) + facet_grid( SEX ~ . ) + scale_color_manual( values = sex.colors, guide = F ) +
        geom_point( alpha = .1) + geom_smooth( method = "lm" ),
    ggplot( mt, aes( AGE, BMI.ADJ ) ) + theme_bw( ) + 
        geom_point( alpha = .1 ) + geom_smooth( method = "lm" ),
    ggplot( mt, aes( AGE, BMI.ADJ, col = SEX ) ) + theme_bw( ) + facet_grid( SEX ~ . ) + scale_color_manual( values = sex.colors, guide = F ) +
        geom_point( alpha = .1) + geom_smooth( method = "lm" ),
    cols = 3 )

mtf <-
    mt[ mt$SEX == "female", ]

mtm <-
    mt[ mt$SEX == "male", ]

mtf$CT.adj.for.AGE.and.SEX <-
    sds( mtf$CALCITONIN, mtf$AGE )

mtm$CT.adj.for.AGE.and.SEX <-
    sds( mtm$CALCITONIN, mtm$AGE )

mtf$log10CT.adj.for.AGE.and.SEX <-
    sds( mtf$log10CT, mtf$AGE )

mtm$log10CT.adj.for.AGE.and.SEX <-
    sds( mtm$log10CT, mtm$AGE )

mt. <-
    rbind(
        mtf,
        mtm )

mt.$min <-
    ifelse( mt.$CALCITONIN == min( mt.$CALCITONIN ), 1, 0 )

ggsubplot(
    ggplot( mt., aes( AGE, CALCITONIN, col = paste0( SEX, min ) ) ) + theme_bw( ) + scale_color_manual( values = sex.min.colors, guide = F ) + facet_grid( . ~ SEX ) +
        geom_point( alpha = .2 ),
    ggplot( mt., aes( AGE, log10CT, col = paste0( SEX, min ) ) ) + theme_bw( ) + scale_color_manual( values = sex.min.colors, guide = F ) + facet_grid( . ~ SEX ) +
        geom_point( alpha = .2 ),
    ggplot( mt., aes( AGE, log10CT.adj.for.AGE.and.SEX, col = paste0( SEX, min ) ) ) + theme_bw( ) + scale_color_manual( values = sex.min.colors, guide = F ) + facet_grid( . ~ SEX ) +
        geom_point( alpha = .2 ),
    ggplot( mt., aes( AGE, CT.adj.for.AGE.and.SEX, col = paste0( SEX, min ) ) ) + theme_bw( ) + scale_color_manual( values = sex.min.colors, guide = F ) + facet_grid( . ~ SEX ) +
        geom_point( alpha = .2 ),
    ggplot( mt., aes( AGE, log10CT.adj.for.AGE.and.SEX, col = paste0( SEX, min ) ) ) + theme_bw( ) + scale_color_manual( values = sex.min.colors, guide = F ) + facet_grid( . ~ SEX ) +
        geom_point( alpha = .2 ),
    ggplot( mt., aes( AGE, exp( log10CT.adj.for.AGE.and.SEX ), col = paste0( SEX, min ) ) ) + theme_bw( ) + scale_color_manual( values = sex.min.colors, guide = F ) + facet_grid( . ~ SEX ) +
        geom_point( alpha = .2 ),
    layout = t(
        matrix(
            c( 
                1, 2, 3, 
                4, 5, 6 ),
            nrow = 3 ) ) )

ggsubplot(
    ggplot( mt., aes( AGE, CALCITONIN, col = SEX ) ) + theme_bw( ) + scale_color_manual( values = sex.colors, guide = F ) + facet_grid( . ~ SEX ) +
        geom_point( alpha = .1 ) + geom_smooth( ),
    ggplot( mt., aes( AGE, log10CT, col = SEX ) ) + theme_bw( ) + scale_color_manual( values = sex.colors, guide = F ) + facet_grid( . ~ SEX ) +
        geom_point( alpha = .1 ) + geom_smooth( ),
    ggplot( mt., aes( AGE, exp( log10CT ), col = SEX ) ) + theme_bw( ) + scale_color_manual( values = sex.colors, guide = F ) + facet_grid( . ~ SEX ) +
        geom_point( alpha = .1 ) + geom_smooth( ),
    ggplot( mt., aes( AGE, CT.adj.for.AGE.and.SEX, col = SEX ) ) + theme_bw( ) + scale_color_manual( values = sex.colors, guide = F ) + facet_grid( . ~ SEX ) +
        geom_point( alpha = .1 ) + geom_smooth( ),
    ggplot( mt., aes( AGE, log10CT.adj.for.AGE.and.SEX, col = SEX ) ) + theme_bw( ) + scale_color_manual( values = sex.colors, guide = F ) + facet_grid( . ~ SEX ) +
        geom_point( alpha = .1 ) + geom_smooth( ),
    ggplot( mt., aes( AGE, exp( log10CT.adj.for.AGE.and.SEX ), col = SEX ) ) + theme_bw( ) + scale_color_manual( values = sex.colors, guide = F ) + facet_grid( . ~ SEX ) +
        geom_point( alpha = .1 ) + geom_smooth( ),
    layout = t(
        matrix(
            c( 
                1, 2, 3, 
                4, 5, 6 ),
            nrow = 3 ) ) )

ggplot( mt., aes( AGE, CALCITONIN, col = SEX ) ) + theme_bw( ) + scale_color_manual( values = sex.colors, guide = F ) + facet_grid( . ~ SEX ) +
    geom_point( alpha = .2 ) + geom_smooth( method = "lm" )

d <-
    mt.[ mt.$AGE >= 12 & mt.$AGE <= 14, ]

hist( log10( mt.$CALCITONIN ), breaks = 100 )

mt.$age.cat <-
    cut( mt.$AGE, breaks = seq( 0, 20, by = 1 ) )

ggsubplot(
    ggplot( mt., aes( log10CT, fill = SEX ) ) + theme_bw( ) + scale_fill_manual( values = sex.colors, guide = F ) + geom_histogram( ) + facet_grid( age.cat ~ SEX ) + ggtitle( "floor effect" ),
    ggplot( mt., aes( CALCITONIN, fill = SEX ) ) + theme_bw( ) + scale_fill_manual( values = sex.colors, guide = F ) + geom_histogram( ) + facet_grid( age.cat ~ SEX ) + ggtitle( "floor effect" ),
    cols = 2 )

ggsubplot(
    ggplot( mt., aes( log10CT, fill = SEX ) ) + theme_bw( ) + scale_fill_manual( values = sex.colors, guide = F ) + geom_density( ) + facet_grid( age.cat ~ SEX ) + ggtitle( "floor effect" ),
    ggplot( mt., aes( CALCITONIN, fill = SEX ) ) + theme_bw( ) + scale_fill_manual( values = sex.colors, guide = F ) + geom_density( ) + facet_grid( age.cat ~ SEX ) + ggtitle( "floor effect" ),
    cols = 2 )

CALCITONIN <-
    seq( min( mt.$CALCITONIN ), max( mt.$CALCITONIN ), by = .1 )

DENSITY <-
    sapply(
        CALCITONIN,
        function( ct ) { round( sum( mt.$CALCITONIN == ct ) / length( !is.na( mt.$CALCITONIN ) ), 10 )  } )

FREQUENCY <-
    sapply(
        CALCITONIN,
        function( ct ) { sum( mt.$CALCITONIN == ct ) } )

m <-
    data.frame( CALCITONIN, FREQUENCY, DENSITY )

ggplot( m ) + theme_bw( ) + geom_line( aes( CALCITONIN, DENSITY ) ) + geom_point( aes( CALCITONIN, DENSITY ) ) + geom_text( aes( CALCITONIN, DENSITY, label = paste0( FREQUENCY, "\n", round( 100 * DENSITY, 2 ), "%" ) ), nudge_y = .005, check_overlap = T ) + ggtitle( "floor effect" )

ggplot( m[ m$CALCITONIN < 2.5, ] ) + theme_bw( ) + geom_line( aes( CALCITONIN, DENSITY ) ) + geom_point( aes( CALCITONIN, DENSITY ) ) + geom_text( aes( CALCITONIN, DENSITY, label = paste0( FREQUENCY, "\n", round( 100 * DENSITY, 2 ), "%" ) ), nudge_y = .005 ) + ggtitle( "floor effect" )


ggplot( mt, aes( AGE, CALCITONIN ) ) + theme_bw( ) +
    geom_point( alpha = .2 ) + geom_smooth( method = "lm" )

mt01 <-
    mt[ mt$AGE < 1.25, ]

mt01f <-
    mt01[ mt01$SEX == "female", ]

mt01m <-
    mt01[ mt01$SEX == "male", ]

mt01f$log10CT.adj.for.AGE <-
    sds( mt01f$log10CT, mt01f$AGE )

mt01m$log10CT.adj.for.AGE <-
    sds( mt01m$log10CT, mt01m$AGE )

mt01f$HEIGHT.adj.for.AGE <-
    sds( mt01f$HEIGHT, mt01f$AGE )

mt01m$HEIGHT.adj.for.AGE <-
    sds( mt01m$HEIGHT, mt01m$AGE )

mt01f$WEIGHT.adj.for.AGE <-
    sds( mt01f$WEIGHT, mt01f$AGE )

mt01m$WEIGHT.adj.for.AGE <-
    sds( mt01m$WEIGHT, mt01m$AGE )

mt01f$OSTEO.adj.for.AGE <-
    sds( mt01f$OSTEO, mt01f$AGE )

mt01m$OSTEO.adj.for.AGE <-
    sds( mt01m$OSTEO, mt01m$AGE )

mt01 <-
    rbind( mt01f, mt01m )

ggsubplot(
    ggplot( mt01, aes( AGE, HEIGHT.ADJ ) ) + theme_bw( ) +
        geom_point( alpha = .2 ) + geom_smooth( method = "lm" ),
    ggplot( mt01, aes( AGE, HEIGHT.ADJ, col = SEX ) ) + theme_bw( ) + facet_grid( SEX ~ . ) + scale_color_manual( values = sex.colors, guide = F ) +
        geom_point( alpha = .2) + geom_smooth( method = "lm" ),
    ggplot( mt01, aes( AGE, HEIGHT.adj.for.AGE ) ) + theme_bw( ) +
        geom_point( alpha = .2 ) + geom_smooth( method = "lm" ),
    ggplot( mt01, aes( AGE, HEIGHT.adj.for.AGE, col = SEX ) ) + theme_bw( ) + facet_grid( SEX ~ . ) + scale_color_manual( values = sex.colors, guide = F ) +
        geom_point( alpha = .2) + geom_smooth( method = "lm" ),
    ggplot( mt01, aes( AGE, WEIGHT.ADJ ) ) + theme_bw( ) +
        geom_point( alpha = .2 ) + geom_smooth( method = "lm" ),
    ggplot( mt01, aes( AGE, WEIGHT.ADJ, col = SEX ) ) + theme_bw( ) + facet_grid( SEX ~ . ) + scale_color_manual( values = sex.colors, guide = F ) +
        geom_point( alpha = .2) + geom_smooth( method = "lm" ),
    ggplot( mt01, aes( AGE, WEIGHT.adj.for.AGE ) ) + theme_bw( ) +
        geom_point( alpha = .2 ) + geom_smooth( method = "lm" ),
    ggplot( mt01, aes( AGE, WEIGHT.adj.for.AGE, col = SEX ) ) + theme_bw( ) + facet_grid( SEX ~ . ) + scale_color_manual( values = sex.colors, guide = F ) +
        geom_point( alpha = .2) + geom_smooth( method = "lm" ),
    cols = 4 )

ggsubplot(
    ggplot( mt01, aes( HEIGHT, log10CT, col = SEX ) ) + theme_bw( ) + scale_color_manual( values = sex.colors, guide = F ) +
        geom_point( alpha = .2 ) + geom_smooth( method = "lm" ),
    ggplot( mt01, aes( HEIGHT, log10CT, col = SEX ) ) + theme_bw( ) + facet_grid( SEX ~ . ) + scale_color_manual( values = sex.colors, guide = F ) +
        geom_point( alpha = .2 ) + geom_smooth( method = "lm" ),
    ggplot( mt01, aes( HEIGHT.ADJ, log10CT, col = SEX ) ) + theme_bw( ) + scale_color_manual( values = sex.colors, guide = F ) +
        geom_point( alpha = .2 ) + geom_smooth( method = "lm" ),
    ggplot( mt01, aes( HEIGHT.ADJ, log10CT, col = SEX ) ) + theme_bw( ) + facet_grid( SEX ~ . ) + scale_color_manual( values = sex.colors, guide = F ) +
        geom_point( alpha = .2 ) + geom_smooth( method = "lm" ),
    ggplot( mt01, aes( HEIGHT, log10CT.adj.for.AGE, col = SEX ) ) + theme_bw( ) + scale_color_manual( values = sex.colors, guide = F ) +
        geom_point( alpha = .2 ) + geom_smooth( method = "lm" ),
    ggplot( mt01, aes( HEIGHT, log10CT.adj.for.AGE, col = SEX ) ) + theme_bw( ) + facet_grid( SEX ~ . ) + scale_color_manual( values = sex.colors, guide = F ) +
        geom_point( alpha = .2 ) + geom_smooth( method = "lm" ),
    ggplot( mt01, aes( HEIGHT.adj.for.AGE, log10CT, col = SEX ) ) + theme_bw( ) + scale_color_manual( values = sex.colors, guide = F ) +
        geom_point( alpha = .2 ) + geom_smooth( method = "lm" ),
    ggplot( mt01, aes( HEIGHT.adj.for.AGE, log10CT, col = SEX ) ) + theme_bw( ) + facet_grid( SEX ~ . ) + scale_color_manual( values = sex.colors, guide = F ) +
        geom_point( alpha = .2 ) + geom_smooth( method = "lm" ),
    ggplot( mt01, aes( HEIGHT.adj.for.AGE, log10CT.adj.for.AGE, col = SEX ) ) + theme_bw( ) + scale_color_manual( values = sex.colors, guide = F ) +
        geom_point( alpha = .2 ) + geom_smooth( method = "lm" ),
    ggplot( mt01, aes( HEIGHT.adj.for.AGE, log10CT.adj.for.AGE, col = SEX ) ) + theme_bw( ) + facet_grid( SEX ~ . ) + scale_color_manual( values = sex.colors, guide = F ) +
        geom_point( alpha = .2 ) + geom_smooth( method = "lm" ),
    cols = 5 )

summary( lm( log10CT ~ HEIGHT * SEX, mt01 ) )
summary( lm( log10CT ~ HEIGHT.ADJ * SEX, mt01 ) )
summary( lm( log10CT ~ HEIGHT.adj.for.AGE * SEX, mt01 ) )
summary( lm( log10CT.adj.for.AGE ~ HEIGHT.adj.for.AGE * SEX, mt01 ) )

ggsubplot(
    ggplot( mt01, aes( WEIGHT, log10CT, col = SEX ) ) + theme_bw( ) + scale_color_manual( values = sex.colors, guide = F ) +
        geom_point( alpha = .2 ) + geom_smooth( method = "lm" ),
    ggplot( mt01, aes( WEIGHT, log10CT, col = SEX ) ) + theme_bw( ) + facet_grid( SEX ~ . ) + scale_color_manual( values = sex.colors, guide = F ) +
        geom_point( alpha = .2) + geom_smooth( method = "lm" ),
    ggplot( mt01, aes( WEIGHT.ADJ, log10CT, col = SEX ) ) + theme_bw( ) + scale_color_manual( values = sex.colors, guide = F ) +
        geom_point( alpha = .2 ) + geom_smooth( method = "lm" ),
    ggplot( mt01, aes( WEIGHT.ADJ, log10CT, col = SEX ) ) + theme_bw( ) + facet_grid( SEX ~ . ) + scale_color_manual( values = sex.colors, guide = F ) +
        geom_point( alpha = .2) + geom_smooth( method = "lm" ),
    ggplot( mt01, aes( WEIGHT, log10CT.adj.for.AGE, col = SEX ) ) + theme_bw( ) + scale_color_manual( values = sex.colors, guide = F ) +
        geom_point( alpha = .2 ) + geom_smooth( method = "lm" ),
    ggplot( mt01, aes( WEIGHT, log10CT.adj.for.AGE, col = SEX ) ) + theme_bw( ) + facet_grid( SEX ~ . ) + scale_color_manual( values = sex.colors, guide = F ) +
        geom_point( alpha = .2) + geom_smooth( method = "lm" ),
    ggplot( mt01, aes( WEIGHT.adj.for.AGE, log10CT, col = SEX ) ) + theme_bw( ) + scale_color_manual( values = sex.colors, guide = F ) +
        geom_point( alpha = .2 ) + geom_smooth( method = "lm" ),
    ggplot( mt01, aes( WEIGHT.adj.for.AGE, log10CT, col = SEX ) ) + theme_bw( ) + facet_grid( SEX ~ . ) + scale_color_manual( values = sex.colors, guide = F ) +
        geom_point( alpha = .2) + geom_smooth( method = "lm" ),
    ggplot( mt01, aes( WEIGHT.adj.for.AGE, log10CT.adj.for.AGE, col = SEX ) ) + theme_bw( ) + scale_color_manual( values = sex.colors, guide = F ) +
        geom_point( alpha = .2 ) + geom_smooth( method = "lm" ),
    ggplot( mt01, aes( WEIGHT.adj.for.AGE, log10CT.adj.for.AGE, col = SEX ) ) + theme_bw( ) + facet_grid( SEX ~ . ) + scale_color_manual( values = sex.colors, guide = F ) +
        geom_point( alpha = .2) + geom_smooth( method = "lm" ),
    cols = 5 )

summary( lm( log10CT ~ WEIGHT * SEX, mt01 ) )
summary( lm( log10CT ~ WEIGHT.ADJ * SEX, mt01 ) )
summary( lm( log10CT ~ WEIGHT.adj.for.AGE * SEX, mt01 ) )

summary( lm( log10CT.adj.for.AGE ~ WEIGHT.adj.for.AGE * SEX, mt01 ) )
summary( lm( log10CT.adj.for.AGE ~ WEIGHT.adj.for.AGE, mt01f ) )
summary( lm( log10CT.adj.for.AGE ~ WEIGHT.adj.for.AGE, mt01m ) )

summary( lm( log10CT ~ WEIGHT * SEX, mt01 ) )
summary( lm( log10CT ~ WEIGHT.ADJ * SEX, mt01 ) )
summary( lm( log10CT ~ WEIGHT.adj.for.AGE * SEX, mt01 ) )

summary( lm( log10CT.adj.for.AGE ~ WEIGHT.adj.for.AGE * SEX, mt01 ) )
summary( lm( log10CT.adj.for.AGE ~ WEIGHT.adj.for.AGE, mt01f ) )
summary( lm( log10CT.adj.for.AGE ~ WEIGHT.adj.for.AGE, mt01m ) )

ggsubplot(
    ggplot( mt01, aes( AGE, OSTEO, col = SEX ) ) + theme_bw( ) + scale_color_manual( values = sex.colors, guide = F ) +
        geom_point( ) + geom_smooth( method = "lm" ),
    ggplot( mt01, aes( AGE, OSTEO.adj.for.AGE, col = SEX ) ) + theme_bw( ) + scale_color_manual( values = sex.colors, guide = F ) +
        geom_point( ) + geom_smooth( method = "lm" ),
    cols = 2 )

ggsubplot(
    ggplot( mt01, aes( OSTEO, log10CT, col = SEX ) ) + theme_bw( ) + scale_color_manual( values = sex.colors, guide = F ) +
        geom_point( ) + geom_smooth( method = "lm" ),
    ggplot( mt01, aes( OSTEO.adj.for.AGE, log10CT, col = SEX ) ) + theme_bw( ) + scale_color_manual( values = sex.colors, guide = F ) +
        geom_point( ) + geom_smooth( method = "lm" ),
    ggplot( mt01, aes( OSTEO, log10CT.adj.for.AGE, col = SEX ) ) + theme_bw( ) + scale_color_manual( values = sex.colors, guide = F ) +
        geom_point( ) + geom_smooth( method = "lm" ),
    ggplot( mt01, aes( OSTEO.adj.for.AGE, log10CT.adj.for.AGE, col = SEX ) ) + theme_bw( ) + scale_color_manual( values = sex.colors, guide = F ) +
        geom_point( ) + geom_smooth( method = "lm" ),
    cols = 4 )

summary( lm( log10CT ~ OSTEO * SEX, mt01 ) )
summary( lm( log10CT.adj.for.AGE ~ OSTEO * SEX, mt01 ) )
summary( lm( log10CT ~ OSTEO.adj.for.AGE * SEX, mt01 ) )
summary( lm( log10CT.adj.for.AGE ~ OSTEO.adj.for.AGE * SEX, mt01 ) )

lms( mt$CALCITONIN[ mt$SEX == "male" ], mt$AGE[ mt$SEX == "male" ] )

l.m.s( mt$CALCITONIN[ mt$SEX == "male" ], mt$AGE[ mt$SEX == "male" ], f = "BCTo" )
