##
# rm
##
rm( list = ls( ) )

##
# Harris-Boyd-Critria
##
harris.boyd.test <-
    function( X.group1, X.group2 ) {

        set <-
            function( result., recommendation.value = +1, recommendation = "partitioning", reason = "not available" ) {
                
                result.$recommendation.value[ 1 ] <- 
                    recommendation.value
                
                result.$recommendation[ 1 ] <-
                    recommendation
                
                result.$reason[ 1 ] <- 
                    reason
                
                result. }
        
        x1 <-
            X.group1[ !is.na( X.group1 ) ]
        
        x2 <-
            X.group2[ !is.na( X.group2 ) ]
        
        n1 <- 
            length( x1 )
        
        n2 <- 
            length( x2 )
        
        zCrit <- 
            sqrt( ( n1 + n2 ) / 240 )

        t.t <-
            t.test( x1, x2 )
        
        result <-
            data.frame(
                stringsAsFactors = F,
                size.total.1 = length( X.group1 ),
                size.total.2 = length( X.group2 ),
                size.complete.1 = n1,
                size.complete.2 = n2,
                mean.1 = mean( x1 ),
                mean.2 = mean( x2 ),
                std.dev.1 = s1<-sd( x1 ),
                std.dev.2 = s2<-sd( x2 ),
                R = s1 / s2,
                t.test.t.val  = t.t$statistic,
                t.test.p.val  = t.t$p.value,
                zCrit3 = 3 * zCrit,
                t.test.t.val.abs  = abs( t.t$statistic ),
                zCrit5 = 5 * zCrit,
                reason = "too less data",
                recommendation = "partitioning",
                recommendation.value = +1,
                rec.val.set = list(
                    partitioning    = -1,
                    marginal        = 0,
                    no.partitioning = +1 ) )

        if( n1 < 120 ) {
            
            result <-
                set( result, 1, "partitioning", "number of X.group1 is too small. it should be at least 120." )
            
            return( result ) }

        if( n2 < 120 ) {
            
            result <-
                set( result, 1, "partitioning", "number of X.group2 is too small. it should be at least 120." )

            return( result ) }

        if( result$std.dev.2 < 1e-20 ) {
            
            result <-
                set( result, 1, "partitioning", "variance of X.group.2 is near to zero." )
            
            return( result ) } #Yes
        
        if( result$R < .7 ) {
            
            result <-
                set( result, -1, "no partitioning", "ratio of standard deviations R = s1 / s2 = ", result$R, " is less than 0.7" )
            
            return( result ) } #No 
        
        if( 1.5 < result$R ) {
            
            result <-
                set( result, +1, "partitioning", "the ratio of standard deviations R = s1 / s2 = ", result$R, " is greater than 1.5" )
            
            return( result ) } #Yes
        
        if( result$t.test.t.val.abs < result$zCrit3 ) {
            
            result <-
                set( result, -1, "no partitioning", paste0( "the ratio of standard deviations R = s1 / s2 = ", result$R, " is between 0.7 and 1.5 and |t| = ", result$t.test.t.val.abs, " is less than ", result$zCrit3 ) )
            
            return( result ) } #Maybe
        
        if( result$zCrit5 < result$t.test.t.val.abs ) {
            
            result <-
                set( result, 1, "partitioning", paste0( "ratio of standard deviations R = s1 / s2 = ", result$R, " is between 0.7 and 1.5 and |t| = ", result$t.test.t.val.abs, " is greater than ", result$zCrit5 ) )
            
            return( result ) } #Maybe
        
        result <-
                set( result, 0, "marginal", paste0( "ratio of standard deviations R = s1 / s2 = ", result$R, " is between 0.7 and 1.5 and |t| = ", result$t.test.t.val.abs, " is between ", result$zCrit3, " and ", result$zCrit5 ) )
        
        return( result ) }

# examples

cnt <-
    5000

sex.groups <-
    c( "female", "male" )

dat <-
    data.frame( 
        time = tm<-runif( cnt, 0, 100 ),
        sex  = sx<-sample( sex.groups, cnt, T ),
        y = rnorm( cnt, c( -10, +10 )[ match( sx, sex.groups ) ], c( +10, +10 )[ match( sx, sex.groups ) ] ) +
            tm * c( +.2, -.2 )[ match( sx, sex.groups ) ] )

plts <- 
    list(  )

i <- 
    0

for( tm in c( 1, 2, 4, 5, 10, 20, 30, 40, 60, 70, 80, 90, 95, 96, 98, 99 ) ) {

    i <-
        i + 1
    
    dat.t1 <- dat[ dat$time < tm & dat$sex == "female", ]
    dat.t2 <- dat[ dat$time < tm & dat$sex == "male", ]

    h.b.t <-
        harris.boyd.test( dat.t1$y, dat.t2$y )
    
    print( tm )
    print( h.b.t$recommendation )
    print( h.b.t$reason )
    
    plts[[ i ]] <-
        ggplot2::ggplot( ) + ggplot2::theme_bw( ) +
            ggplot2::geom_point( ggplot2::aes( time, y ), dat.t1, col = "green", alpha = .2 ) +
            ggplot2::geom_point( ggplot2::aes( time, y ), dat.t2, col = "red", alpha = .2 ) +
            ggplot2::geom_text( ggplot2::aes( 50, 0, label = h.b.t$reason ), size = 2 ) +
            ggplot2::ggtitle( label = h.b.t$recommendation ) }

hlpr4life::ggsubplot(
    plotlist = plts,
    cols = ceiling( sqrt( length( plts ) ) ) )
