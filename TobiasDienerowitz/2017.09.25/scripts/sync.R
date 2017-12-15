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
# lade join ohne extreme werte
##
# load( "data/generated/main.table.only.usable.without.extremes.Rd" )
load( "data/generated/main.table.only.usable.with.extremes.Rd" )

mt <-
    main.1205.only.usable.with.extremes

rm( main.1205.only.usable.with.extremes )

load( "data/original/raw.dat.Rd" )

table.df( mt, F )
table.df( res, F )

# alle in Rohdatentabelle
length( unique( res$sic ) ) # 2240 Probanden

# alle in Rohdatentabelle, die nicht im join sind
length( unique( res$sic[ !res$sic %in% mt$SIC ] ) ) # 673 sind nicht im join
length( unique( res$sic[ res$sic %in% mt$SIC ] ) ) # 1567 sind nicht im join

# alle in Rohdatentabelle, die im join sind
length( unique( res$sic[ res$sic %in% mt$SIC ] ) ) # 1567 sind im join

# alle im join #1586
length( unique( mt$SIC ) )

# alle im join, die nicht in der Rohdatentabelle sind
length( unique( mt$SIC[ !mt$SIC %in% res$sic ] ) ) # 19

sics.not.in.raw <-
    unique( mt$SIC[ !mt$SIC %in% res$sic ] )

mt[ mt$SIC %in% sics.not.in.raw, ]

# alle im join, die in der Rohdatentabelle sind
length( unique( mt$SIC[ mt$SIC %in% res$sic ] ) )

res$ed <- lubridate::as_date( res$create_at )
mt$ed <- lubridate::as_date( mt$U_Sing.EDAT )

res.sync <-
    res[ key.df( res, c( "sic", "ed" ) ) %in% key.df( mt, c( "SIC", "ed" ) ), ]

mt.sync <-
    mt

save( mt.sync, file = "data/generated/main.table.sync.Rd" )
WriteXLS::WriteXLS( mt.sync, ExcelFileName = "data/generated/main.table.sync.xlsx" )

save( res.sync, file = "data/generated/raw.sync.Rd" )
WriteXLS::WriteXLS( res.sync, ExcelFileName = "data/generated/raw.sync.xlsx" )

