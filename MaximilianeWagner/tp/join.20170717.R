rm( list = ls( ) )

if( !"devtools" %in% installed.packages( ) ) {
    install.packages( "devtools" ) }

devtools::install_github( "TPeschel/hlpr4life" )

library( "hlpr4life" )

load.pkgs(
    c(
        "dplyr",
        "readxl",
        "WriteXLS" ) )

setwd( "~/LIFE/life-for-postgraduates/MaximilianeWagner/tp/" )

load( "T6.Rd")

names( t6 )

tbl.pv208 <-
    read_excel( "PV208_DataJoin20160929.xlsx" )



dis <- 
    read_excel( "PV0208_D00127_Krankheiten.xlsx" )

med <- 
    read_excel( "PV0208_D00129_Medikamente.xlsx" )

dis <- 
    read_excel( "PV0208_D00177.xlsx" )

merge(
    t6,
    
)