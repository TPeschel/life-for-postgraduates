###############################################

rm( list = ls( ) )

###############################################

hlpr4life::load.pkgs( 
    c( 
        "hlpr4life",
        "readxl",
        "dplyr",
        "reshape2",
        "ggplot2" ) )

###############################################

setwd( "~/LIFE/life-for-postgraduates/TobiasDienerowitz/" )

###############################################

main <-
    read_excel( "data/PV0278_datajoin20161205.xlsx" )

main$age.cat.1 <- 
    cut(
        main$age,
        floor( min( main$age, na.rm = T ) - 1 ) : ceiling( max( main$age, na.rm = T ) ) ) 

not.use <-
    read_excel( "sent/2017.08.17/Prom_non_usable.xlsx" )

head( not.use )

main.use <-
    main[ !paste0( main$PSEUDONYM,"XYLOP", main$C_ANTHRO_KH_GRP ) %in% paste0( not.use$PSEUDONYM, "XYLOP", not.use$C_ANTHRO_KH_GRP ), ]

main.not.use <-
    main[ paste0( main$PSEUDONYM,"XYLOP", main$C_ANTHRO_KH_GRP ) %in% paste0( not.use$PSEUDONYM, "XYLOP", not.use$C_ANTHRO_KH_GRP ), ]

ggplot( main.not.use ) + theme_bw( ) + scale_fill_manual( values = c( "deeppink", "deepskyblue" ) ) +
    geom_histogram( aes( age.cat.1, fill = sex ), stat = "count" ) +
    geom_text( aes( age.cat.1, n,label = n ), summarise( group_by( main.not.use, age.cat.1, sex ), n = n( ) ), nudge_y = 1.5 ) +
    facet_grid( sex ~ . )

View( not.use[ , c( "age", "sex", "NA.KOMMENTAR_MESSUNG" ) ] )

d<-data.frame(x=x<-runif(100,0,1),y=y<-rnorm(100,0,1),z=rnorm(100,0,2)+2*x+3*y)

(lm.z.yx<-lm(z~y+x,d))
(lm.z.yx.s <- summary( lm.z.yx ))
aov(lm.z.yx)
anova(lm.z.yx)

ggplot(d)+geom_point(aes(x,z))+geom_smooth(aes(x,z), method="lm")
ggplot(d)+geom_point(aes(y,z))+geom_smooth(aes(y,z), method="lm")


d$r <- lm.z.yx.s$residuals
d$f <- fitted( lm.z.yx )

ggplot(d)+geom_point(aes(f,r))
plot(lm.z.yx)


d <-
    data.frame(
        x1 = x1<-c( 1 : 10 )+rnorm(10),
        x2 = x2<-c( 1 : 10 )+rnorm(10),
        y = x1 + x2 )

( lm.d <- lm( y ~ x2 + x1, d ) )
summary( lm.d )
aov( lm.d )
anova( lm.d )


ggsubplot(
    ggplot( d ) + theme_bw( ) + #scale_x_continuous( breaks = c( 0 : 10 ) ) + scale_y_continuous( breaks = c( 0 : 10 ) ) + 
        geom_point( aes( x1, y ) ) +
        geom_smooth( aes( x1, y ), method = "lm" ),
    ggplot( d ) + theme_bw( ) + #scale_x_continuous( breaks = c( 0 : 10 ) ) + scale_y_continuous( breaks = c( 0 : 10 ) ) + 
        geom_point( aes( x2, y ) ) +
        geom_smooth( aes( x2, y ), method = "lm" ),
    cols = 1 )

                    