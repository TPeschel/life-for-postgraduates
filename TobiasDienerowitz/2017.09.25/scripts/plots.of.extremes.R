##
# gib speicher frei
##
rm( list = ls( ) )

##
# lade alle noetigen Pakete
##
hlpr4life::load.pkgs(
    c(
        "hlpr4life",
        "ggplot2",
        "dplyr",
        "reshape2",
        "knitr" ) )

##
# setze Pfad auf Projektverzeichnis
##
setwd( "~/LIFE/life-for-postgraduates/TobiasDienerowitz/2017.09.25/" )

##
# lade join mit extremen werten
##
load( "data/generated/main.1205.only.usable.with.extremes.Rd" )
m <- main.1205.only.usable.with.extremes
rm( main.1205.only.usable.with.extremes )

##
# lade Rohdaten
##
load( "data/original/raw.dat.Rd" )
r <- res
rm( res )

load( "data/original/20170922sicpseudoliste.rdata" ) 

r$PSEUDONYM <-
    sicpseudo$PSEUDONYM[ match( r$sic, sicpseudo$SIC ) ]
    
rm( sicpseudo )

r$proftype[ 255 < r$proftype ] <- 
    3

nams <- c(
    "singing silent",                  ## 0
    "singing loud",                    ## 1
    "speaking point cloud maximum",    ## 2
    "speaking type point",             ## 3
    "singing formant loud",            ## 5
    "speaking point cloud average" )   ## 6

r$pt <-
    nams[ match( r$proftype, c( 0, 1, 2, 3, 5, 6 ) ) ]

e <-
    m[ m$EXTREME, ]

re <-
    r[ r$sic %in% e$SIC, ]

re <-
    merge( 
        re, 
        rename.columns( 
            m[ , c( "PSEUDONYM",  "C_ANTHRO_KH_AGE", "C_ANTHRO_KH_GRP", "C_ANTHRO_KH_EDAT" ) ],
            c( "PSEUDONYM",  "C_ANTHRO_KH_AGE", "C_ANTHRO_KH_GRP", "C_ANTHRO_KH_EDAT" ),
            c( "PSEUDONYM",  "AGE", "SCI_GROUP", "EDAT" ) ),
        by = "PSEUDONYM" ) 

re$time.diff <-
    difftime( re$create_at, re$EDAT, units = "day" )

re <-
    re[ abs( re$time.diff ) < 30, ]

hist( abs( as.numeric( re$time.diff ) ) )

limits.freq <-
    floor( key.of.freq( range( re$frequ, na.rm = T ) ) )

limits.freq[ 2 ] <-
    limits.freq[ 2 ] + 1

re$st <-
    note.of.freq( re$frequ )

re$piano.key <-
    key.of.freq( re$frequ )


# ggplot( re[ re$sic == sic & re$proftype %in% c( 0, 1 ), ], aes( piano.key, energy, col = paste0( proftype ), group = paste0( sessid, pt ) ) ) +
#             theme_bw( ) +
#             geom_point( ) +
#             geom_line( ) +
#             facet_grid( create_at ~ . ) +
#             scale_x_continuous( name = "semitone", breaks = seq( 1, 100, by = 3 ), labels = note.of.key( seq( 1, 100, by = 3 ) ), sec.axis = sec_axis( ~ ( freq.of.key( . ) ), name = "f [Hz]",  breaks = c( 50, 100, 200, 400, 800, 1600, 3200 ) ) ) +
#             scale_y_continuous( name = "sound pressure level [dB]" ) +
#             scale_color_manual( "sound profiles", values = c( "blue", "red" ), labels = nams[ c( 1, 2 ) ] )

my.plot <-
    ggplot( NULL, aes( piano.key, energy, col = paste0( proftype ), group = paste0( sessid, pt ) ) ) +
    theme_bw( ) +
    geom_point( ) +
    geom_line( ) +
    facet_grid( create_at ~ . ) +
    scale_x_continuous( name = "semitone", breaks = seq( 1, 100, by = 3 ), labels = note.of.key( seq( 1, 100, by = 3 ) ), sec.axis = sec_axis( ~ ( freq.of.key( . ) ), name = "f [Hz]",  breaks = c( 12.5, 25, 50, 100, 200, 400, 800, 1600, 3200 ) ), limits = c( 11, 72 ) ) +
    scale_y_continuous( name = "sound pressure level [dB]", limits = c( .9 * min( re$energy ), 1.1 * max( re$energy ) ) ) +
    scale_color_manual( "sound profiles", values = c( "blue", "red" ), labels = nams[ c( 1, 2 ) ] ) +
    geom_label( aes( x = 66, y = 90, label = paste0( SCI_GROUP, "\n", AGE ) ), inherit.aes = F, data = NULL, stat = "identity", col = "black" )

plot.list <-
    re[ re$proftype %in% c( 0, 1 ), ] %>%
    filter( pt %in% nams ) %>%
    group_by( sic ) %>%
    do( plot = my.plot %+% . + ggtitle( paste0( .$sex, " ", .$sic, "(", .$PSEUDONYM, ")-[ ", .$commentsess, "]" ) ) )

setwd( "pdf/")

today <-
    gsub( "-", "", Sys.Date( ) )

outname <-
    paste0( "plot.of.extremes[", today, "].tex" )

knit2pdf( "../scripts/plots.of.extremes.Rnw", outname )

#options( warn = oldw )
