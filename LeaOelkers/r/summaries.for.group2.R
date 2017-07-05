rm( list = ls( ) )

if( !require( ggplot2 ) ) install.packages( "ggplot2" )
if( !require( ggthemes ) ) install.packages( "ggthemes" )

t4 <-
    read_excel( "~/LIFE/life-for-postgraduates/LeaOelkers/sent/2017.07.04/DA/LIFE-Daten/R-Projekte/r20170601/data/Gruppe2/Gruppe2_fertigerDatensatz.xlsx" )

t4 <-
    t4[ !is.na( t4$C_ANTHRO_KH_BMI_ADJ ), ]

t4 <-
    t4[ 4 < t4$AGE, ] ##den 3 j?hrigen ausschlie?en

##spalten wieder zuf?gen zu gr??erer t4 (235 Spalten): 
load( "~/LIFE/life-for-postgraduates/LeaOelkers/sent/2017.07.04/DA/LIFE-Daten/data/tabelleGesamtJoinAlleSpalten.Rd" )

# nms <-
#     intersect( names( t4 ), names( gesamt.Join.alle.spalten ) )

nms <-
    intersect( names( t4 ), names( tbl ) )

# t4 <-
#     merge( t4, gesamt.Join.alle.spalten, by=nms,all.x = TRUE, all.y=F)
t4 <-
    merge( t4, tbl, by = nms, all.x = TRUE, all.y = F )

# t4 <- t4[ ,c( "alle spalten die ich will") ]  wenn ich wieder nur bestimmet spalten betrachten will

# t4$AGE.CATEGORIE <-
#   cut(
#     x      = t4$AGE,
#     breaks = c(3 : 18 ),
#     labels = c(4 : 18 ) )

nrow( t4 )

##Age by gender
table( t4$SEX )

addmargins( table( t4$SEX, t4$AGE.CATEGORIE ) )

t4$AGE.CAT <-
    cut( 
        x = t4$AGE,
        breaks = seq( 4.5, 19.5, by = 1 ),
        labels = seq( 5, 19, by = 1 ) )

##
# age by gender
# for category n stands for an age in this interval .5 < 1 <= 1.5
##

addmargins( table( t4[ , c( "SEX", "AGE.CAT" ) ] ) )

ggplot( t4 ) +
    geom_histogram( aes( AGE.CAT, fill = SEX ), stat = "count", position = "dodge" ) +
    scale_fill_manual( values = c( "deeppink", "deepskyblue" ) ) + 
    facet_grid( . ~ SEX ) +
    theme_bw( )

##
# summary age by sex
##
d <-
    as.data.frame( rbind( t( sapply( unique( t4$SEX ), function( d ) summary( t4$AGE[ t4$SEX == d ] ) ) ), summary( t4$AGE ) ) )
    
rownames( d )[ 3 ] <- 
    "total"

d    

table( d )

df.mean.age <-
    data.frame(
        SEX  = c( "male", "female", "total" ),
        MEAN = c( mean( t4$AGE[ t4$SEX == "male" ] ), mean( t4$AGE[ t4$SEX == "female" ] ), mean( t4$AGE ) ),
        STDV = c( sd(   t4$AGE[ t4$SEX == "male" ] ), sd(   t4$AGE[ t4$SEX == "female" ] ), sd(   t4$AGE ) ),
        MIN  = c( min(  t4$AGE[ t4$SEX == "male" ] ), min(  t4$AGE[ t4$SEX == "female" ] ), min(  t4$AGE ) ),
        MAX  = c( max(  t4$AGE[ t4$SEX == "male" ] ), max(  t4$AGE[ t4$SEX == "female" ] ), max(  t4$AGE ) ) )

df.mean.age
