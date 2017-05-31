##
## Data.Join Kuration
## 

require( "readxl" )
require( "lubridate" )
require( "dplyr" )
require( "ggplot2" )

##
## lade tabelle Probenauswahl
##
data.join <-
    read_excel( "PV208_DataJoin20160929.xlsx" )

##
## betrachte Struktur
##
str( data.join, list.len = ncol( data.join ) )

##
## ermittle Spaltennamen
##
( names.pa <-
    names( data.join ) )

##
## aendere die wichtigsten Namen
##
names( data.join )[ names.pa == "TEILNEHMER_SIC" ] <- "SIC"
names( data.join )[ names.pa == "C_AUFKL_SCI_GROUP" ] <- "SCIGROUP"
names( data.join )[ names.pa == "TEILNEHMER_GESCHLECHT" ] <- "SEX"
names( data.join )[ names.pa == "TEILNEHMER_GEB_JJJJMM" ] <- "BIRTHDAY"

## 
## EDATS
##
( edats <-
    apply( is.na( data.join[ grep( x = names( data.join ), pattern = "EDAT" ) ] ), 2, sum ) )

##
## ermittle EDAT-Spalte mit den wenigsten Missings
##
( best.edat <-
    which.min( edats ) )

##
## erklaere diese Spalte zur offiziellen EDAT-Spalte
##
names( data.join )[ names( data.join ) == names( best.edat ) ] <-
    "EDAT"

##
## AGES
##
( ages <-
    apply( is.na( data.join[ grep( x = names( data.join ), pattern = "AGE" ) ] ), 2, sum ) )

##
## ermittle Alter-Spalte mit den wenigsten Missings
##
( best.age <-
    which.min( ages ) )

##
## erklaere diese Spalte zur offiziellen EDAT-Spalte
##
names( data.join )[ names( data.join ) == names( best.age ) ] <-
    "AGE"

##
## finde notwendige Spalten
necessary.columns <-
    c( "SIC", "SCIGROUP", "EDAT", "SEX", "BIRTHDAY", "AGE", 
        "TEILNEHMER_STADTTEIL", "TEILNEHMER_MERKMAL_1", "TEILNEHMER_MERKMAL_2", "TEILNEHMER_MERKMAL_3",
        "C_AUFKL_DQP_ID", 
        "C_ANTHRO_KH_UID", "C_ANTHRO_KH_HEIGHT_ORIG", "C_ANTHRO_KH_HEIGHT_ADJ", "C_ANTHRO_KH_WEIGHT_ORIG", "C_ANTHRO_KH_WEIGHT_ADJ",
        "C_ANTHRO_KH_BMI_ORIG", "C_ANTHRO_KH_BMI_ADJ",
        "C_ANTHRO_VERSION", "C_ANTHRO_UID",
        "C_ANTHRO_H_KOERPER", "C_ANTHRO_SDS_H_KOERPER", "C_ANTHRO_GEWICHT", "C_ANTHRO_SDS_GEWICHT",
        "C_ANTHRO_H_UMFANG", "C_ANTHRO_SDS_H_UMFANG",
        "C_ANTHRO_HIP", "C_ANTHRO_SDS_HIP",
        "C_ANTHRO_K_UMFANG", "C_ANTHRO_SDS_K_UMFANG",
        "C_ANTHRO_OA_UMFANG", "C_ANTHRO_SDS_OA_UMFANG",
        "C_ANTHRO_OS_UMFANG", "C_ANTHRO_SDS_OS_UMFANG",
        "C_ANTHRO_SITZHOEHE", "C_ANTHRO_SDS_SITZHOEHE",
        "C_ANTHRO_TRIZEPS_1", "C_ANTHRO_SDS_TRIZEPS_1", "C_ANTHRO_TRIZEPS_2", "C_ANTHRO_SDS_TRIZEPS_2", "C_ANTHRO_TRIZEPS_3",
        "C_ANTHRO_SDS_TRIZEPS_3",
        "C_ANTHRO_WA_UMFANG", "C_ANTHRO_SDS_WA_UMFANG",
        "C_ANTHRO_WAIST", "C_ANTHRO_SDS_WAIST",
        "C_BP_VERSION", "C_BP_UID",
        "C_BP_BP_SYS_1", "C_BP_SDS_BP_SYS_1",
        "C_BP_BP_SYS_2", "C_BP_SDS_BP_SYS_2",
        "C_BP_BP_SYS_3", "C_BP_SDS_BP_SYS_3",
        "C_BP_BP_DIA_1", "C_BP_SDS_BP_DIA_1",
        "C_BP_BP_DIA_2", "C_BP_SDS_BP_DIA_2",
        "C_BP_BP_DIA_3", "C_BP_SDS_BP_DIA_3",
        "C_PUB_STAT_PH", "C_PUB_STAT_G", "C_PUB_STAT_B",
        "C_PUB_STAT_HV_LI", "C_PUB_STAT_HV_RE", 
        "C_PUB_STAT_MENARCHE", "C_PUB_STAT_PUB_STATUS", "C_PUB_STAT_MENARCHE_WANN",
        "C_SOZDEM_KS_RELIGION", "C_SOZDEM_GEBLAND_DTL", "C_SOZDEM_GEBLAND_OTH", "C_SOZDEM_KE_RELIGION",
        "C_SOZDEM_K_SCHULE", 
        "C_SOZDEM_M_SCHULAB", "C_SOZDEM_V_SCHULAB", "C_SOZDEM_M_BERUFAUSB", "C_SOZDEM_V_BERUFAUSB",
        "C_SOZDEM_M_BERUFSTELL", "C_SOZDEM_V_BERUFSTELL",
        "C_SOZDEM_M_BERUFSTELL_ARB", "C_SOZDEM_V_BERUFSTELL_ARB",
        "C_SOZDEM_M_BERUFSTELL_ANG", "C_SOZDEM_V_BERUFSTELL_ANG", 
        "C_SOZDEM_M_BERUFSTELL_BEAM", "C_SOZDEM_V_BERUFSTELL_BEAM",
        "C_SOZDEM_M_BERUFSTELL_SELB", "C_SOZDEM_V_BERUFSTELL_SELB",
        "C_SOZDEM_M_BERUFSTELL_SONS", "C_SOZDEM_V_BERUFSTELL_SONS",
        "C_SOZDEM_M_BERUFSTAET", "C_SOZDEM_V_BERUFSTAET",
        "C_SOZDEM_M_ZUWANDGR", "C_SOZDEM_V_ZUWANDGR", 
        "C_SOZDEM_FAMSTAND", "C_SOZDEM_WOHNGEG", "C_SOZDEM_N_COMP", "C_SOZDEM_N_AUTO",
        "C_SOZDEM_EINZELKIND",
        "C_SOZDEM_N_URLAUB", "C_SOZDEM_N_GSW_ALT", "C_SOZDEM_N_HAUSHALT", "C_SOZDEM_N_GSW_JUNG",
        "C_SOZDEM_EINKOMMEN", "C_SOZDEM_N_GSW_GA", "C_SOZDEM_EIGZIMM", "C_SOZDEM_HAUSTIER", "C_SOZDEM_ZWEITZUHAUS", 
        "C_SOZDEM_ZH_VATER", "C_SOZDEM_ZH_MUTTER", "C_SOZDEM_ZH_STIEFV", "C_SOZDEM_ZH_STIEFM",
        "C_SOZDEM_ZH_OPA", "C_SOZDEM_ZH_OMA",
        "C_SOZDEM_ZH_BRUED_N", "C_SOZDEM_ZH_SCHW_N",
        "C_SOZDEM_WHGGROESSE",
        "C_SOZDEM_HT_KATZE", "C_SOZDEM_HT_HUND", "C_SOZDEM_HT_VOEGEL", "C_SOZDEM_HT_FISCH",
        "C_SOZDEM_HT_KS", "C_SOZDEM_HT_OTH", "C_SOZ_WI_FASSUMME", "C_SOZ_WI_FAS", "C_SOZ_WI_EINKOMMEN",
        "C_SOZ_WI_SCHULAB_M", "C_SOZ_WI_SCHULAB_V",
        "C_SOZ_WI_BERUFSTELL_M", "C_SOZ_WI_BERUFSTELL_V",
        "C_SOZ_WI_SOZSTAT_M", "C_SOZ_WI_SOZSTAT_V",
        "C_SOZ_WI_SOZIO_M", "C_SOZ_WI_SOZIO_V", 
        "C_SOZ_WI_SCHICHT_M", "C_SOZ_WI_SCHICHT_V",
        "C_SOZ_WI_SOZIO_FAM" )
    
##
## verwirf unnoetige Spalten
## 
data.join <-
    data.join[ , necessary.columns ]

##
## zeige Struktur
##
str( data.join, list.len = ncol( data.join ) )

##
## wandle geburtstag in vernuenftiges Datum um
data.join$BIRTHDAY <- 
    as.Date( ymd( paste0( data.join$BIRTHDAY, "15" ) ) )

##
## zeige Struktur
##
str( data.join, list.len = ncol( data.join ) )
