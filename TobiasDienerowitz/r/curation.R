rm( list = ls( ) )

hlpr4life::load.pkgs(
    c(
        "hlpr4life",
        "lifecuration",
        "ggplot2",
        "dplyr",
        "reshape2" ) )

load( "sent/2017.09.14/raw.dat.Rd" )
load( "data/main/main.table.2017.09.29.Rd" ) 
load( "")
names( main.1205 )

obs.non.usable <-
    read.csv( "sent/2017.09.14/non_usable_obs." )

names( obs.non.usable )

names( obs.non.usable ) <-
    gsub( "NA\\.", "T00865\\.", names( obs.non.usable ) )

out <-
    paste0( obs.non.usable$PSEUDONYM, obs.non.usable$C_ANTHRO_KH_GRP )

main.1205.only.usable <-
    main.1205[ !paste0( main.1205$PSEUDONYM, main.1205$C_ANTHRO_KH_GRP ) %in% out, ]

obs.extr <-
    read.csv( "sent/2017.09.14/obs_extremes" )

names( obs.extr )

names( obs.extr ) <-
    gsub( "NA\\.", "T00865\\.", names( obs.extr ) )

res$proftype[ 255 < res$proftype ] <- 
    3

nams <- c(
    "singing silent",                  ## 0
    "singing loud",                    ## 1
    "speaking point cloud maximum",    ## 2
    "speaking type point",             ## 3
    "singing formant loud",            ## 5
    "speaking point cloud average" )   ## 6

res$pt <-
    nams[ match( res$proftype, c( 0, 1, 2, 3, 5, 6 ) ) ]


main.1205.only.usable$PSEUDONYM

# WriteXLS( main.1205, ExcelFileName = "data/main/PV0278_datajoin_20170929.xlsx" ) #doesn't save correctly pseudonyms
write.xlsx( main.1205, file = "sent/2017.09.14/main.1205.only.usable.xlsx" )

save( main.1205.only.usable, file = "sent/2017.09.14/main.1205.only.usable.Rd" )

