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
load( "data/generated/main.table.only.usable.without.extremes.Rd" )
#load( "data/generated/main.table.only.usable.with.extremes.Rd" )
#load( "data/generated/main.table.Rd" )

table.df( mt, F )

source( "~/LIFE/.secret.R" )

con <-
    db.open( Sys.getenv( "DB_USER" ), Sys.getenv( "DB_PASS" ) )

t00865 <-
    get.data.with.aliases( con, "T00865" )

db.close( con )

# 72 sics sind nicht in der DB 
sum( !mt$SIC %in% t00865$SIC )

load( "data/original/20171030sicpseudoliste.rdata" )

# alle in der maintable haben eine zuordnung
mt$SIC[ !mt$SIC %in% sicpseudo$SIC ]

# es gibt aber 5 in der sic-pseudo-map, die es nicht in der main table gibt 
sicpseudo$SIC[ !sicpseudo$SIC %in% mt$SIC ]

# In der sic.pseudo-map gibt es 70, die es in der T00865 nicht gibt
length( unique( sicpseudo$SIC[ !sicpseudo$SIC %in% t00865$SIC ] ) )

# In der main table gibt es 61, die nicht in der t865 sind
length( unique( mt$SIC[ !mt$SIC %in% t00865$SIC ] ) )

( sic.not.in.t865 <-
    unique( mt$SIC[ !mt$SIC %in% t00865$SIC ] ) )

# moeglicherweise muessen die auch noch raus
table.df( mt[ mt$SIC %in% sic.not.in.t865, ], F )
