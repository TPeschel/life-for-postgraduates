# Loesche ALLES
rm( list = ls( ) )

setwd( "~/LIFE/life-for-postgraduates/JulianeWilz/data" )

library(readxl)
library( dplyr )
library( ggplot2 )
library( GGally )
library( Hmisc )
library(dplyr)

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

# So testest Du, ob alle durch diseases ausgewaehlten Spalten keine Missings enthalten.
# Ich empfehle aber, diese Zeile nicht auszufuehren, da Dir so viele Werte verloren gehen.
# Geht man davon aus, dass der Proband nur krank ist, wenn tatsaechlich irgendwo eine 1 steht,
# dann kannst Du die Missings uebersehen. In diesem Fall empfehle ich sogar Missings auf 0 zu setzen
# das waere Variante C

# Variante A
# Variante A.1
# df <- df[ complete.cases( df[ , diseases ] ), ]

# Dasselbe mit den Medikamenten
# Variante A.2
# df <- df[ complete.cases( df[ , medik ] ), ]

# in einem Rutsch
# Variante B
# df <- df[ complete.cases( df[ , c( diseases, medik ) ] ), ]

# aber da bleibt ja so gut, wie nix uebrig
# deshalb rate ich dazu nur nach den 1en zu suchen und die NAs zu uebersehen


# Variante C
View( df[ ,c( "CT_S_1_SIC", "CT_S_1_GRUPPE", medik, diseases ) ] ) # vorher
for( i in c( medik, diseases ) ) {
    
    df[ is.na( df[ , i ] ), i ] <- 0
}
View( df[ ,c( "CT_S_1_SIC", "CT_S_1_GRUPPE", medik, diseases ) ] ) # nachher

# Die Funktion "filter.for.certain.values" filtert alle Zeilen, die bei irgendeiner Krankheit oder Medikation eine 1 stehen haben, aus.
# NAs werden nicht beachtet. die gibt es wahrscheinlich ja auch gar nicht mehr
# Moechtest Du diese aber auch raushaben, dann fuehre vorher Variante A oder B aus
# Das ganze dauert einen kleinen Moment
filter.for.certain.values <-
function( d, cols ) {
    
    j = 1
    
    for( i in 1 : nrow( d ) ) {
    
        if( !any( !is.na( d[ i , cols ] ) & d[ i , cols ] == 1 ) ) {
            
            d[ j, ] <- d[ i, ]
            
            j <- j + 1
        }
    }
    
    ( d <- d[ 1 : ( j - 1 ), ] )
}

# df <- filter.for.certain.values( df, diseases )
# df <- filter.for.certain.values( df, medik )

# Weils so lange dauert, ist es wohl besser diseases und medik in einem Rutsch zu erledigen.
df <- filter.for.certain.values( df, c( medik, diseases ) )

# ab hier haste nur noch gesunde Leute, die keine Medikamente eingenommen haben

library( WriteXLS )

WriteXLS( x = df , ExcelFileName = "NeueTabelle180517excel.xlsx" )

