##
# gib speicher frei
##
rm( list = ls( ) )

devtools::install_github( "TPeschel/hlpr4life" )

##
# lade alle noetigen Pakete
##
hlpr4life::load.pkgs(
    c(
        "hlpr4life",
        "ggplot2",
        "dplyr",
        "reshape2",
        "knitr",
        "db.access",
        "lifecuration" ) )

##
# setze Pfad auf Projektverzeichnis
##
setwd( "~/LIFE/life-for-postgraduates/TobiasDienerowitz/2017.09.25/" )

##
# lade join mit extremen werten
##
load( "data/generated/main.table.only.usable.with.extremes.Rd" )
load( "data/generated/main.table.Rd" )

t( table.df( main.1205.only.usable.with.extremes ) )

source( "~/LIFE/.secret.R" )

con <-
    db.open( Sys.getenv( "DB_USER" ), Sys.getenv( "DB_PASS" ) )

t00865 <-
    get.data.with.aliases( con, "T00865" )

db.close( con )

# 81 sics sind nicht in der datenbank
sum( !main.1205$SIC %in% t00865$SIC )

# ohne nonusables:
# 72 sics sind nicht in der DB 
sum( !main.1205.only.usable.with.extremes$SIC %in% t00865$SIC )

load( "data/original/20170922sicpseudoliste.rdata" )

# alle in der maintable haben einen zuordnung
main.1205$SIC[ !main.1205$SIC %in% sicpseudo$SIC ]

# es gibt aber 4 in der sic-pseudo-map, die es nicht in der main table gibt 
sicpseudo$SIC[ !sicpseudo$SIC %in% main.1205$SIC ]

# In der sic.pseudo-map gibt es 72, die es in der T00865 nicht gibt
length( unique( sicpseudo$SIC[ !sicpseudo$SIC %in% t00865$SIC ] ) )

# In der main table gibt es 70, die nicht in der t865 sind
length( unique( main.1205$SIC[ !main.1205$SIC %in% t00865$SIC ] ) )

( sic.not.in.t865 <-
    unique( main.1205$SIC[ !main.1205$SIC %in% t00865$SIC ] ) )

t( table.df( main.1205[ main.1205$SIC %in% sic.not.in.t865, ] ) )

main.1205[ main.1205$SIC %in% sic.not.in.t865, grep( "U_Sing", names( main.1205 ) ) ]

main.1205[ is.na( main.1205$T00865.EDAT ) & main.1205$SIC %in% sic.not.in.t865, c( "SIC", "sex" ) ] 

ni865 <-
    main.1205$SIC[ !is.na( main.1205$T00865.EDAT ) & main.1205$SIC %in% sic.not.in.t865 ] 

sum( t00865$SIC %in% ni865 )
