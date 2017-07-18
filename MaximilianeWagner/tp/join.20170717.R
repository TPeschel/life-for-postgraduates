rm( list = ls( ) )

<<<<<<< HEAD
Sys.setenv( TZ = "BMT")

if( !"devtools" %in% installed.packages( ) ) {
    install.packages( "devtools" )
}
=======
if( !"devtools" %in% installed.packages( ) ) {
    install.packages( "devtools" ) }
>>>>>>> daa1ab05c758e77c20694327fec76f89ab9c1a1f

devtools::install_github( "TPeschel/hlpr4life" )

library( "hlpr4life" )

<<<<<<< HEAD
load.pkgs( c( "readxl" ) )

setwd( "~/LIFE/life-for-postgraduates/MaximilianeWagner/tp/" )

load( "T6.Rd" )

d177 <-
    read_excel( "PV0208_D00177.xlsx" )

t6 <-
    merge.likely(
        t6,
        d177,
        by.x = "TEILNEHMER_SIC",
        by.y = "PSEUDONYM",
        by.lk.x = "Entnahmedatum.x",
        by.lk.y = "EDAT",
        min = c( -183 ),
        max = c( +182 ),
        all.x = T,
        all.y = F )

d129 <-
    read_excel( "PV0208_D00129_Medikamente.xlsx" )

t6 <-
    merge.likely( 
        d1 = t6, 
        d2 = d129, 
        by.x = c( "TEILNEHMER_SIC" ), 
        by.y = c( "CHILD_MED_H_SIC" ),
        by.lk.x = c( "Entnahmedatum.x" ),
        by.lk.y = c( "CHILD_MED_H_EDAT" ),
        min = c( -32 ),
        max = c( +31 ),
        all.x = T,
        all.y = F )

t6 <-
    t6[ , c(
        "Materialnummer",
        "TEILNEHMER_SIC",
        "Entnahmedatum.x",
        "lfd. Nr.x",
        "Cortisol",
        "C_BP_SDS_BP_DIA_3",
        "C_BP_SDS_BP_SYS_3",
        "C_PUB_STAT_PUB_STATUS",
        "C_ANTHRO_AGE",
        "C_AUFKL_GENDER",
        "C_ANTHRO_KH_BMI_ORIG",
        "C_ANTHRO_KH_BMI_ADJ",
        "C_ANTHRO_KH_HEIGHT_ADJ",
#        "Haarwaschfrequenz",
        "HaarwaschfrequenzGruppen",
        "CHILD_MED_H_GLUCO_CORT",
        "CHILD_MED_H_MINERALOCORT",
        "CHILD_MED_H_SEX_STEROIDE",
        "D00177_SCORE_FAM" ) ]


save( "t6", file = "AnthroWinklerMedik.Rd" )
=======
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
>>>>>>> daa1ab05c758e77c20694327fec76f89ab9c1a1f
