rm( list = ls( ) )

if( !"devtools" %in% rownames( installed.packages( ) ) ) {
    install.packages( "devtools" ) }

devtools::install_github( "TPeschel/hlpr4life" )

hlpr4life::load.pkgs(
    c(
        "hlpr4life",
        "readxl",
        "dplyr" )
)

setwd( "~/LIFE/life-for-postgraduates/HansSurup/sent/20171005/" )

# ich nenne die Tabelle mt fuer main table
# is nich so lang (O;
mt <-
    read_excel( "PV0365_Gesamt_Join.xlsx" )

table.df( mt )

mt <-
    rename.columns( 
        mt,
        c(
            "TEILNEHMER_SIC",
            "TEILNEHMER_GESCHLECHT",
            "TEILNEHMER_GEB_JJJJMM",
            "C_ANTHRO_KH_AGE",
            "C_ANTHRO_KH_HEIGHT_ORIG",
            "C_ANTHRO_KH_HEIGHT_ADJ",
            "C_ANTHRO_KH_WEIGHT_ORIG",
            "C_ANTHRO_KH_WEIGHT_ADJ",
            "C_ANTHRO_KH_BMI_ORIG",
            "C_ANTHRO_KH_BMI_ADJ" ),
        c(
            "SIC",
            "SEX",
            "BIRTHDAY",
            "AGE",
            "HEIGHT",
            "WEIGHT",
            "BMI",
            "HEIGHT.ADJ",
            "WEIGHT.ADJ",
            "BMI.ADJ" ) )

as.data.frame( t( table.df( mt ) ) )

# erstmal Besuche mit fehlenden TSH-, fT3-, fT4- Werten ausschließen 
# so schliesst Du alle Zeilen aus, die nicht in allen 3 Spalten einen Wert aufweisen
# mt <-
#     mt[ !is.na( mt$TSH_S_NUM_VALUE ) | !is.na( mt$FT3_S_NUM_VALUE ) | !is.na( mt$FT4_S_NUM_VALUE ), ]
# 7711 uebrig


# so schliesst Du alle Zeilen aus, die nicht in allen 3 Spalten 3 Werte aufweisen
mt <-
    mt[ !is.na( mt$TSH_S_NUM_VALUE ) & !is.na( mt$FT3_S_NUM_VALUE ) & !is.na( mt$FT4_S_NUM_VALUE ), ]
#7406 uebrig

# Sex umkodieren 
mt$SEX <-
    c( "male", "female" )[ match( mt$SEX, c( 1, 2 ) ) ]

get.info( mt )

# sex UND alter muessen beide angegeben sein
mt <-
    mt[ !is.na( mt$SEX ) & !is.na( mt$AGE ), ]
# 7378 uebrig


##1.1 Alle NAs auf 0 setzen
mt$C_DISEASE_TX_SD_ALLG[ is.na( mt$C_DISEASE_TX_SD_ALLG ) ] <- 0
mt$C_DISEASE_TX_SD_HYPO[ is.na( mt$C_DISEASE_TX_SD_HYPO ) ] <- 0
mt$C_DISEASE_TX_SD_HYPER[ is.na( mt$C_DISEASE_TX_SD_HYPER ) ] <- 0
mt$C_DISEASE_TX_FRUEHGEB[ is.na( mt$C_DISEASE_TX_FRUEHGEB ) ] <- 0
mt$C_DISEASE_TX_ENDOKR[ is.na( mt$C_DISEASE_TX_ENDOKR ) ] <- 0
mt$C_DISEASE_TX_DM1[ is.na( mt$C_DISEASE_TX_DM1 ) ] <- 0
mt$C_DISEASE_TX_DM2[ is.na( mt$C_DISEASE_TX_DM2 ) ] <- 0
mt$C_DISEASE_TX_NGSCREEN[ is.na( mt$C_DISEASE_TX_NGSCREEN ) ] <- 0
mt$C_DISEASE_TX_BLUT[ is.na( mt$C_DISEASE_TX_BLUT ) ] <- 0
mt$C_DISEASE_TX_GERIN[ is.na(mt$C_DISEASE_TX_GERIN ) ] <- 0
mt$C_DISEASE_TX_GIT[ is.na( mt$C_DISEASE_TX_GIT ) ] <- 0
mt$C_DISEASE_TX_NIERENFEHL[ is.na( mt$C_DISEASE_TX_NIERENFEHL ) ] <- 0
mt$C_DISEASE_TX_KARDIO_RHYTH[ is.na( mt$C_DISEASE_TX_KARDIO_RHYTH ) ] <- 0
mt$C_DISEASE_TX_KARDIO[ is.na( mt$C_DISEASE_TX_KARDIO ) ] <- 0
mt$C_DISEASE_TX_EPIKRAMPF[ is.na( mt$C_DISEASE_TX_EPIKRAMPF ) ] <- 0
mt$C_DISEASE_TX_PSY[ is.na( mt$C_DISEASE_TX_PSY ) ] <- 0
mt$C_DISEASE_TX_SUCHT[ is.na( mt$C_DISEASE_TX_SUCHT ) ] <- 0
mt$C_DISEASE_TX_SUCHT[ is.na( mt$C_DISEASE_TX_NEPHRO ) ] <- 0


# 1.2 Flags zählen und protokollieren
sum( mt$C_DISEASE_TX_SD_ALLG == 1 ) # 29

sum( mt$C_DISEASE_TX_NEPHRO == 1 ) # hier gibt es offensichtlich noch NAs
sum( is.na( mt$C_DISEASE_TX_NEPHRO ) ) # 261 Missings
sum( !is.na( mt$C_DISEASE_TX_NEPHRO ) & mt$C_DISEASE_TX_NEPHRO == 1 ) #7

# Schmeisse die Missings noch raus
mt <-
    mt[ !is.na( mt$C_DISEASE_TX_NEPHRO ), ]
# 7117 uebrig

sum( mt$C_DISEASE_TX_SD_HYPER == 1 )  # 5
sum( mt$C_DISEASE_TX_SD_HYPO == 1 )  # 36
sum( mt$C_DISEASE_TX_ENDOKR == 1 ) #28
sum( mt$C_DISEASE_TX_DM1 == 1 ) #5
sum( mt$C_DISEASE_TX_DM2 == 1 ) # 13
sum( mt$C_DISEASE_TX_BLUT == 1 ) #37
sum( mt$C_DISEASE_TX_GIT == 1 ) #80 hier fehlt jetzt einer
sum( mt$C_DISEASE_TX_EPIKRAMPF == 1 ) #54
sum( mt$C_DISEASE_TX_SUCHT == 1 ) #20

get.info( mt )

#So nur, wenn Du sicher bist, dass die Spalten keine Missings enthalten
mt <- mt[ mt$C_DISEASE_TX_SD_ALLG == 0, ] 
mt <- mt[ mt$C_DISEASE_TX_SD_HYPO == 0, ] #7341
mt <- mt[ mt$C_DISEASE_TX_SD_HYPER == 0, ] #7377
mt <- mt[ mt$C_DISEASE_TX_ENDOKR == 0, ] #7293
mt <- mt[ mt$C_DISEASE_TX_EPIKRAMPF == 0, ] #6919

