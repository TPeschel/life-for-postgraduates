rm( list = ls())

library( "readxl" )
library("WriteXLS")
library("xlsx")
library("dplyr")
library("ggplot2")


rename.column <-
  function( d, old.column.name, new.column.name ) {
    names( d )[ names( d ) == old.column.name ] <-
      new.column.name
    d }

rename.columns <-
  function( d, old.column.names, new.column.names ) {
    
    for( i in c( 1 : length( old.column.names ) ) ) {
      
      d <-
        rename.column( d, old.column.names[ i ], 
                       new.column.names[ i ] ) }
    d }



## Setze hier den Pfad zum Verzeichnis Deiner Dateien
setwd( "C:/Users/Anwender/Documents/Dissertation/RAuswertung/" )

## Lade beide Tabellen
t1 <- read_excel( "daten/original/20170608_Probenliste_AGa.xlsx" )
t2 <- read_excel( "daten/original/PV208_Probauswahl2_20160929.xlsx" )
names(t1)
names(t2)


nms <- intersect(names(t1),names(t2))
nms <- nms[-5]
## Zusammenfuegen beider Tabellen ueber die Materialnummer
t3 <- merge( t1, t2, by = "Materialnummer" )
data.frame(h1 = t3$`Wie oft gewaschen.x`, h2 = t3$`Wie oft gewaschen.y`)
## Zeige alle Namen, die die auf .x enden sind aus t1 und .y aus t2
names( t3 )

## Zeige Tabelle t3
#View( t3 )

t4 <- t3[ !is.na( as.numeric( t3$Cortisol ) ), ]

#View( t4 )

t6 <- t4[ , c(
  "Materialnummer",
  "TEILNEHMER_SIC",
  "C_AUFKL_SCI_GROUP",
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
  "Wie oft gewaschen.x") ]

# t6 <-
#   rename.column( t6,"Wie oft gewaschen.x","Haarwaschfrequenz" )

t6 <- t6[ !is.na( t6$C_PUB_STAT_PUB_STATUS ), ]

t6$Cortisol <- as.numeric( t6$Cortisol )
#hist(t6$Haarwaschfrequenz)
t6$HaarwaschfrequenzGruppen <- c("selten","selten","normal","haeufig","haeufig")[match(t6$'Wie oft gewaschen.x',c(1,3,2,4,5))]
#t6$HaarwaschfrequenzGruppen <- c("selten","selten","normal","haeufig","haeufig")[match(t6$Haarwaschfrequenz,c(1,3,2,4,5))]
##WriteXLS(t6,"JoinmitCortisol.xlsx")
##write.csv2(x = t6,file = "JoinmitCortisol.csv")

#write.xlsx( x=t6, file ="JoinmitCortisol.xlsx" )

#t6 <- t6[ t6$Cortisol < 35, ]

save( t6, file = "daten/T6.Rd")


d177 <- read_excel("PV0208_D00177.xlsx")

t6 <- merge( t6, d177, by.x = c( "TEILNEHMER_SIC", "Entnahmedatum.x" ), by.y = c("PSEUDONYM","EDAT" ),all.x = T,all.y = F)

dmedikamente <- read_excel("PV0208_D00129_Medikamente.xlsx")

t6. <- merge(
  t6,
  dmedikamente,
  by.x  = c( "TEILNEHMER_SIC", "Entnahmedatum.x" ), 
  by.y  = c("CHILD_MED_H_SIC","CHILD_MED_H_EDAT"),
  all.x = )

t6 <- t6[,c("Materialnummer",
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
            "Haarwaschfrequenz",
            "HaarwaschfrequenzGruppen",
            "CHILD_MED_H_GLUCO_CORT",
            "CHILD_MED_H_MINERALOCORT",
            "CHILD_MED_H_SEX_STEROIDE",
            "D00177_SCORE_FAM"  )]

t6.male   <- t6[ t6$C_AUFKL_GENDER == 1, ]
t6.female <- t6[ t6$C_AUFKL_GENDER == 2, ]

save(t6, t6.male, t6.female,file = "Haupttabellen.Rd" )

# load( "geschlechter.Rd")

#library( WriteXLS)

write.xlsx( x = t6.male, file = "t6Male.xlsx" )
write.xlsx( x = t6.female, file = "t6Female.xlsx" )

ggplot( t6.male, aes( x = factor( C_PUB_STAT_PUB_STATUS ), y = log( Cortisol ) ) ) +
  geom_boxplot( )

ggplot( t6.female, aes( x = factor( C_PUB_STAT_PUB_STATUS ), y = log( Cortisol ) ) ) +
  geom_boxplot( )

##
# ANOVA 
##
t6.aov <- aov( data = t6.male, log( Cortisol ) ~ C_PUB_STAT_PUB_STATUS )
summary( t6.aov )

t6.aov <- aov( data = t6.female, log( Cortisol ) ~ C_PUB_STAT_PUB_STATUS )
summary( t6.aov )

tanner.male.1 <- t6.male[ t6$C_PUB_STAT_PUB_STATUS == 1, ]
tanner.male.2 <- t6.male[ t6$C_PUB_STAT_PUB_STATUS == 2, ]
tanner.male.3 <- t6.male[ t6$C_PUB_STAT_PUB_STATUS == 3, ]
tanner.male.4 <- t6.male[ t6$C_PUB_STAT_PUB_STATUS == 4, ]
tanner.male.5 <- t6.male[ t6$C_PUB_STAT_PUB_STATUS == 5, ]
tanner.male.234 <- t6.male[ between( t6$C_PUB_STAT_PUB_STATUS, 2, 4 ), ]

tanner.female.1 <- t6.female[ t6$C_PUB_STAT_PUB_STATUS == 1, ]
tanner.female.2 <- t6.female[ t6$C_PUB_STAT_PUB_STATUS == 2, ]
tanner.female.3 <- t6.female[ t6$C_PUB_STAT_PUB_STATUS == 3, ]
tanner.female.4 <- t6.female[ t6$C_PUB_STAT_PUB_STATUS == 4, ]
tanner.female.5 <- t6.female[ t6$C_PUB_STAT_PUB_STATUS == 5, ]
tanner.female.234 <- t6.female[ between( t6$C_PUB_STAT_PUB_STATUS, 2, 4 ), ]

summary( tanner.male.1$Cortisol )
summary( tanner.male.2$Cortisol )
summary( tanner.male.3$Cortisol )
summary( tanner.male.4 )
summary( tanner.male.5 )
summary( tanner.male.234 )

summary( tanner.female.1 )
summary( tanner.female.2 )
summary( tanner.female.3 )
summary( tanner.female.4 )
summary( tanner.female.5 )
summary( tanner.female.234 )

rbind( tanner.1, tanner.234 )

ggplot( t6, aes( x=C_ANTHRO_AGE, y=Cortisol, color=factor( C_PUB_STAT_PUB_STATUS ) ) ) +
  geom_point( ) +
  geom_smooth( )

ggplot( t6, aes(x=C_ANTHRO_KH_BMI_ADJ, y=Cortisol, color=factor(C_AUFKL_GENDER ) ) ) +
geom_point()
geom_smooth()


ggplot(t6, aes(x=C_BP_SDS_BP_SYS_3, y=Cortisol)) +
geom_point()

ggplot(t6, aes(x=C_BP_SDS_BP_DIA_3, y=Cortisol)) +
  geom_point()

ggplot(t6, aes(x=C_SOZ_WI_SOZIO_FAM, y=Cortisol, color=factor(C_AUFKL_GENDER))) +
  geom_point()+
  geom_smooth()

ggplot(t6, aes(x=C_PUB_STAT_PUB_STATUS, y=Cortisol))+
geom_point()

class(t6$geom_point()_STAT_PUB_STATUS)
