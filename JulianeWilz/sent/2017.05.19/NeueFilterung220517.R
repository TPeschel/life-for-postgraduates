# Loesche ALLES
rm( list = ls( ) )

#setwd( "Juliane/Desktop/pv0116_neu/" )
setwd( "~/LIFE/life-for-postgraduates/JulianeWilz/data/" )

library(readxl)
library( dplyr )
library( ggplot2 )
library( GGally )
library( Hmisc )
library(dplyr)
library( openxlsx )

#daten <- read_excel( "PV0116_Arbeitsversion.xlsx" )

daten <- read_excel( "PV0116_GesamtJoin.xlsx" )
df <- daten[ !is.na( daten$CT_S_1_NUM_VALUE ), ]

diseases <- c( "C_DISEASE_TX_SD_ALLG", "C_DISEASE_TX_SD_HYPER",
               "C_DISEASE_TX_SD_HYPO", "C_DISEASE_TX_VITD", "C_DISEASE_TX_DM1", "C_DISEASE_TX_DM2", 
               "C_DISEASE_TX_BLUT", "C_DISEASE_TX_GERIN", 
               "C_DISEASE_TX_NEPHRO", "C_DISEASE_TX_NIERENFEHL",
               "C_DISEASE_TX_ANGIO", "C_DISEASE_TX_DEPRES", "C_DISEASE_TX_SUCHT", 
               "C_DISEASE_TX_MUSKEL", "C_DISEASE_TX_GIT", "C_DISEASE_TX_KARDIO",  
               "C_DISEASE_TX_KARDIO_RHYTH")

#diseases1 <- c( "C_DISEASE_TX_SD_ALLG", "C_DISEASE_TX_SD_HYPER",
#"C_DISEASE_TX_SD_HYPO", "C_DISEASE_TX_VITD", "C_DISEASE_TX_DM1", "C_DISEASE_TX_DM2", 
#"C_DISEASE_TX_BLUT", "C_DISEASE_TX_GERIN", 
#"C_DISEASE_TX_NEPHRO", "C_DISEASE_TX_NIERENFEHL",
#"C_DISEASE_TX_ANGIO", "C_DISEASE_TX_DEPRES", "C_DISEASE_TX_SUCHT", 
#"C_DISEASE_TX_MUSKEL")

medik <- c( "CHILD_MED_H_METFORMIN", "CHILD_MED_H_INSULIN", "CHILD_MED_H_LTHYROX",
            "CHILD_MED_H_HORMONE", "CHILD_MED_H_SEX_STEROIDE", "CHILD_MED_H_WACHSTUM",
            "CHILD_MED_H_DESMOPRESS",  "CHILD_MED_H_GLUCO_CORT",
            "CHILD_MED_H_TESTO", "CHILD_MED_H_ASTHMA")


# Das ist die neue Funktion, die Dir alle Mediks und Diseases rausfiltert und die NAs zu auf 0 setzt
filter.for.reasonable.values <-
    function( dat.frm, interesting.cols ) {
        na.flags <-
            as.data.frame(
                apply(
                    df[ , interesting.cols ],
                    2.,
                    is.na
                )
            )
        
        df[ , interesting.cols ] <-
            as.data.frame(
                mapply(
                    df[ , interesting.cols ],
                    na.flags,
                    FUN = function( a, b ) ifelse( b, 0, a ) ) )
        
        df$sum.of.ones <-
            apply(
                df[ , interesting.cols ],
                1,
                sum
            )
        
        df <- 
            df[ df$sum.of.ones < 1, ]
        
        df$sum.of.ones <- 
            NULL
        
        df
    }

mdk.dss <-
    c( medik, diseases )

df <- 
    filter.for.reasonable.values( df, mdk.dss )

View( df[ c( "CT_S_1_SIC", "CT_S_1_GRUPPE", mdk.dss ) ] )

# ab hier haste nur noch "gesunde" Leute, die "keine Medikamente" eingenommen haben

nichtausgeschlossen#Hüftdysplasie <- c("7D32247AE1A2_3M","5E726880B3A2_3M","9455D30FA4A2_3M","6A4A9D2DE4A2_3M", "9455D30FA4A2_01","07E300BEFAA2_01"
#"4293018CFFA2_6M","57145BC607A2_01","8D7AB20F9CA2_3M","C4280BA5C4A2_3M","63B6F4042BA2_3M","57145BC607A2_6M")



Ikterus <- c("F253BA9FDCA2-SK_01", "40EF9053CDA2_3M","27F3AAFDF8A2-SK_01","4C1C477A09A2_3M","72AC41C68FA2_3M","2D14EEAF9EA2_3M",
"7F5A47DEFAA2_01", "B6FC81B271A2_3M","38CF322948A2_3M","8641A70968A2_3M")

Niere <- c("CAA1484CF0A2_04", "2DC3B0ADE2A2_11","B5286CD3F9A2_15","7179E63ED5A2_06","081940C4D5A2_10","BF1ADED87CA2_3M","69FEBB9A07A2_08",
"DFEA8BF395A2_09","DADED57804A2_07","3E0513BAA2A2_16","FBC647BF53A2_01","ED731EADAEA2_11", "E16FC4DFB2A2_3M")

GIT <- c("3FB83E76EDA2-SK_05")

Ausreisser <- c("FC11C38221A2_3M")


Ritalin <- c("3234E27F42B1_16","3F37D4C832A2_11","5F56BA3A4AB1_12","8504D6FB2EA2_13","E72CDACCBAA2_15","8504D6FB2EA2_12","03419A0DEAA2-SK_01",
"8504D6FB2EA2_14" )


MuskErkrankung <- c("4F267F4F54A2_16","BF6CDD9F6FA2_10","B984F164F8A2_01")


Pantoprazol <- c("32A73968C0A2_15", "F5936AD536A2_15", "840D327FDAA2_14", "9C3E0A5C7AA2_14", "E7E6C4D7F9A2_15")


SICGROUP <- c(  GIT, Ikterus, Ritalin, MuskErkrankung, Pantoprazol, Ausreisser )

df1 <- df[!paste0( df$CT_S_1_SIC, df$CT_S_1_GRUPPE) %in% SICGROUP, ]


ASD <- c("F4D84480C7" )

VSD <- c("19BD6A6AAC","37A2BC8419","63820C4B8E","8641A70968","8E72CCA696","C4C48F737A")

Mitral <- c("E7B754719C","1D1327997B","072C7C1912","27CC9E6D64", "46CB460261",
            "90F008C440", "B383040016")

Aorten <- c("28CCC778A4", "3F3B78A16D", "E8FA497455")

mehrereHerzkrankheiten <- c("28CCC778A4")

MMeulengracht<- c("E60A583B4A", "D1E1792725","228CA1C40E", "EACDCF372D" )

Niere <- c("CAA1484CF0", "2DC3B0ADE2","B5286CD3F9","7179E63ED5","081940C4D5","BF1ADED87C","69FEBB9A07",
           "DFEA8BF395","DADED57804","3E0513BAA2","FBC647BF53","ED731EADAE", "E16FC4DFB2")

Knochen <- c("7BF6EFE2CD", "87691882D4","9C4A6A31B5","D22C464F28","353A8B3870","7D09FB4586","4B58B02900",
             "6C0AC81AE7","76C48EF23B","53225E8183","C0A44D788A","5D17742E68","9E7C2CF446","7F8C935C88","8D37A6E155",
             "2B0240A66D","0FB7E114F6","5BC70742CE","29E70F2C61","6634694A7C","20D4416103" ,"710EA2D8A3","DDAF42DD68",
             "0EE9B0EFD2","6622F5E746","2B9F681AB3","F9F266BDB8", "A5543137CC")

Gehirnschädigung <- c("08BC3D640D", "0A7F789EFB","08BC3D640D")

Sonstiges <- c("278D8061B2", "DC395B3CC8","B03385C1E3","2542D1625C","7B4E0CDAE8","A0EC5EB277","F3BE3660F0",
               "AC2DB1F248","FDC849B4BF","238AF1D07C","9249323B02","79FD6D1603","86CCE3AE09", "3495EE330C","3495EE330C",
               "0677B362C6","4880CB93E3","674CF37825","D4C93A0446","713D6B6534","35D5BA9E5E","7B54C4FAEE",
               "BC26147BEC","0A7F789EFB","844E26F4B0", "6553E9B0FF")

Hormone <- c("A32701D433","1708D266B1","FA8AD8BD1D",
             "EED92F1F55","28EFA1AD5A","B357E9BB61","A290507BB8","2D0EF351A6","31851D333F",
             "EB9B84517C","99F6DA6C37","9BB155D7D0","16A74EFEF4","65BBC9638A","3BFB8F8246")

maligneErkrankung <-  c("58E7E02A9E", "3870E289B1")

Wachstumsretardierung <- c("BF00616007","E1A9F2741D")

BipolareStoerung <- c("4F267F4F54")

SICS.alle <- c(ASD, VSD, Mitral, Aorten, mehrereHerzkrankheiten, MMeulengracht, Knochen, Niere, Gehirnschädigung, Sonstiges, Hormone, Wachstumsretardierung, maligneErkrankung, BipolareStoerung)

neue.Tabelle <- df1[ !df1$CT_S_1_SIC %in%  SICS.alle, ]


write.xlsx( x = neue.Tabelle, file = "AktuelleTabelle220517excel.xlsx" )
