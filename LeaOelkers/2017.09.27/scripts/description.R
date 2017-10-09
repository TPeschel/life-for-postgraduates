rm( list = ls( ) )

# installiere devtools, falls noch nicht geschehen
if( !"devtools" %in% rownames( installed.packages( ) ) ) install.packages( "devtools" )

# installiere neueste Version von helperForLife, falls noch nicht geschehen
devtools::install_github( "TPeschel/hlpr4life" )

hlpr4life::load.pkgs(
    c(
        "hlpr4life",
        "dplyr",
        "reshape2",
        "readxl",
        "openxlsx",    ##openxlsx ist viel schneller als xlsx, WriteXLS ist nicht gut
        "lubridate",
        "ggplot2",
        "psych") )  

# setze Zeitzone auf Berlin Mean Time
Sys.setenv( TZ = "Europe/Berlin" ) 

##
# setze Pfad zu aktuellem R-Arbeitsverzeichnis, Pfad zum Laden
# setwd( "~/LIFE/life-for-postgraduates/LeaOelkers/AllesNeu20170725/data/original/" )
# setwd( "c:/Users/Lea/Desktop/AllesNeu20170725/data/generated/" )  
setwd( "~/LIFE/life-for-postgraduates/LeaOelkers/2017.09.27/")

load( "data/generated/main.table.curated_2017-10-06.Rd" )

##Themes f?r Plots, um nicht jedes mal alles neu zu definieren: ohne Gitter, da Kiess dies preferiert
theme.histo <-
  list(
    theme_bw( ),
    scale_color_manual( values = c( "deeppink", "blue" ) ),
    scale_fill_manual( values = c( "deeppink", "blue" ) ),
    theme( panel.grid = element_blank( ) ) )

theme.histo.facet <-
  list(
    theme_bw( ),
    scale_color_manual( values = c( "deeppink", "blue" ), guide = F ),
    scale_fill_manual( values = c( "deeppink", "blue" ), guide = F ),
    theme( panel.grid = element_blank( ) ),
    facet_grid( SEX ~ . ) )    ##theme ohne Legende bei Facet grid

theme.scatter <-
  list(
    theme_bw( ),
    scale_color_manual( values = c( "deeppink", "deepskyblue" ) ),
    scale_fill_manual( values = c( "deeppink", "deepskyblue" ) ) )

##Hilfe
    # list(colours())
    # View(tbl[, c( "C_ANTHRO_KH_BMI_ADJ", "C_ANTHRO_KH_BMI_ORIG", "AGE", "WGHT_GRPS_BMI","WGHT_GRPS"  )]) ##einzelne spalten angucken
    # sum(is.na(tbl$C_ANTHRO_KH_BMI_ORIG))  ##NAs zählen


##CNT : um n=xx in die Plots reinzuschreiben
tbl.add <-
    as.data.frame( table( tbl$SEX ) )

tbl$CNT <- 
    NA     ##CNT als spalte angezeigt

tbl$CNT[ tbl$SEX == "male" ] <-
    tbl.add$Freq[ tbl.add$Var1=="male" ]

tbl$CNT[ tbl$SEX == "female" ] <-
    tbl.add$Freq[ tbl.add$Var1 =="female"]

nrow( tbl )

#Alterskathegorien bilden

# tbl$AGE.CAT1<- 
#     cut(tbl$AGE,
#         breaks = c(-1:21),   
#         labels = c(-1:20))  ##ODER:

tbl$AGE.CAT1<- 
    cut(
        tbl$AGE,
        breaks = seq( 4, 18, by = 1 ),   
        labels = seq( 4, 17, by = 1 ) )

tbl$AGE.CAT2<- 
    cut(
        tbl$AGE,
        breaks = seq( 4, 18, by = 2 ),   
        labels = seq( 5, 18, by = 2 ) )


##Age by gender
table( tbl$SEX )
addmargins( table( tbl[ , c( "SEX", "AGE.CAT1" ) ] ) )

##Mittelwert etc des ALters
describeBy( tbl$AGE, tbl$SEX )

summary( tbl$AGE[ tbl$SEX == "male" ] )  

summary( tbl$AGE[ tbl$SEX == "female" ] ) 

#Plot: frequency of age by sex

# setwd("c:/Users/Lea/Desktop/AllesNeu20170725/results/plots" )

ggplot(tbl)+
    geom_histogram(aes( AGE.CAT2, fill=SEX),stat="count", position = "dodge")  # oder stack

nrow(tbl)

ggplot( tbl ) +
    geom_histogram( aes( AGE.CAT1, fill = SEX ), stat = "count" ) + 
    # theme_bw( ) +
    theme.histo.facet +
    ylab( "frequency" ) +
    scale_fill_manual( "sex", values = c( "deeppink", "blue" ), guide = F ) +  ##"grey85", "grey65"
    scale_x_discrete( "age [y]" )
    # geom_text( x=12, y=50, aes( label=paste0( "n = ",CNT ), group = "SEX" ), check_overlap = T )      #--> ?ndern; individuell n= eintragen


ggplot( tbl ) +
    geom_histogram( aes( AGE.CAT1, fill = SEX ), stat =  "count" ) + 
    facet_grid( .~ SEX ) +   ##nebeneinander, wenn umgedreht: ?berienander
    theme_classic( ) +
    ylab( "frequency" ) +
    scale_fill_manual( "sex", values = c( "grey85", "grey65" ) ) +
    scale_x_discrete( "age [y]" )
# geom_text( x=12, y=50, aes( label=paste0( "n = ",CNT ), group = "SEX" ), check_overlap = T )  


##Boxplot mean age by gender
    
# data frame schaffen mit Alter-Mean/MAX/MIN nach SEX (f?r die n?chsten Befehle wichtig)
df.mean.age <-
    data.frame(
        SEX=c("male","female"),
        MEAN = c( mean( tbl$AGE[tbl$SEX=="male"]),mean( tbl$AGE[tbl$SEX=="female"])),
        MIN = c( min( tbl$AGE[tbl$SEX=="male"]),min( tbl$AGE[tbl$SEX=="female"])),
        MAX = c( max( tbl$AGE[tbl$SEX=="male"]),max( tbl$AGE[tbl$SEX=="female"]))
    )

df.mean.age



##mit jitter (verstreuten Punkten)
# ggplot(tbl)+
#   geom_boxplot(aes(x=SEX, y=AGE), fill="gray")+  ##boxplot, grau f?rben
#   geom_errorbar(data=df.mean.age,aes(x=SEX, ymin=MIN,ymax=MAX),size=.3,width=.5)+ ##size=liniendicke, witth=breite der min/MAx werte (=staples)
#   geom_point(aes(x=SEX,y=AGE), position="jitter",alpha=.6)+ ##punktverteilung zus?tzlich darstellen, alpha=tranparenz, position=jitter: punktverteilung weit um Linie herum
#   geom_point(data = df.mean.age, aes(x=SEX,y=MEAN), col="red")+  ##mean als Punkt draufsetzen
#   theme_bw()  ##in schwarz-wei?

#das gleiche ohne jitter: age by gender --> am besten
ggplot( tbl )+
    geom_boxplot(aes(x=SEX, y=AGE, fill=SEX), width=.5)+ #
    geom_errorbar(data=df.mean.age,aes(x=SEX, ymin=MIN,ymax=MAX),size=.3,width=.5)+ ##size=liniendicke, witth=breite der min/MAx werte
    geom_jitter(aes(x=SEX,y=AGE),alpha=.1,width = .01)+ ##punktverteilung zus?tzlich darstellen, alpha=tranparenz
    geom_point(data = df.mean.age, aes(x=SEX,y=MEAN), col="green", size=2)+
    scale_fill_manual( "sex", values = c( "deeppink", "blue" ) ) +
    ylab("age [y]")+
    xlab("sex")+
    theme_bw()
# ggsave(filename =  "boxplot.ageBySex.pdf" )


# # das Gleiche alles in grau
# ggplot(tbl)+
#   geom_errorbar(aes(x=SEX, ymin=min(AGE),ymax=max(AGE)),size=.3,width=.5)+ ##size=liniendicke, witth=breite der whisker
#   geom_point(aes(x=SEX,y=AGE),alpha=.1)+ ##punktverteilung zus?tzlich darstellen, alpha=tranparenz
#   geom_boxplot(aes(x=SEX, y=AGE),alpha=.5)+ ##
#   geom_point(data = df.mean.age, aes(x=SEX,y=MEAN), col="red")





##Puberty Groups

addmargins(table(tbl$SEX, tbl$C_PUB_STAT_PUB_STATUS))
describeBy(tbl$C_PUB_STAT_PUB_STATUS, tbl$SEX)
summary(tbl$C_PUB_STAT_PUB_STATUS)


table(tbl$C_PUB_STAT_PUB_STATUS, tbl$AGE.CAT1, tbl$SEX=="female") 
table(tbl$C_PUB_STAT_PUB_STATUS,tbl$SEX)  
# describeBy(tbl$AGE, tbl$C_PUB_STAT_PUB_STATUS, tbl$SEX=="female") geht nicht...wie k?rner S.9?


##frequency of PG all together
ggplot(tbl) +
  geom_histogram(aes(C_PUB_STAT_PUB_STATUS, fill = SEX  ), stat = "count") +
  theme.histo.facet +
  facet_grid(. ~ SEX)+
  xlab("PG") +
  ylab("frequency") +
  scale_fill_manual( "sex", values = c( "deeppink", "blue" ) , guide = F) 
  

ggplot(tbl) +
  theme.histo +
  geom_histogram(aes(C_PUB_STAT_PUB_STATUS, fill = SEX  ), stat = "count") +
  xlab("PG") +
  ylab("frequency")

ggplot(tbl) +
  theme.histo.facet +
  geom_histogram(aes(C_PUB_STAT_PUB_STATUS, fill = SEX  ), stat = "count") +
  xlab("PG") +
  ylab("frequency")
# ggsave( filename = "frequency.PG.sexTogether.pdf" )

#getrennt nach sex 
ggplot(tbl) +
    geom_histogram(aes(C_PUB_STAT_PUB_STATUS, fill = SEX  ), stat = "count") +
    facet_grid(. ~ SEX)+
    theme_bw() +
    xlab("PG") +
    ylab("frequency") +
    scale_fill_manual( "sex", values = c( "deeppink", "blue" ) ) 
# ggsave( filename = "frequency.PG.sex.pdf" )

#oder in sw
# ggplot(tbl) +
#   my.theme +
#   # scale_fill_manual(name = "Geschlecht", values = c("red", "blue")) +
#   xlab("PubStat") +
#   ylab("Anzahl der Probanden") +
#   geom_histogram(aes(x = as.factor( C_PUB_STAT_PUB_STATUS ) ), stat = "count", position = "stack") +
#   facet_grid(. ~ SEX)+
#   scale_fill_manual( "sex", values = c( "deeppink", "blue" ) ) 
#   ##?bereinander: position="stack", nebeneinander w?re: "dodge", ##stat= count: z?hlt anzhal pro pubstat

##ohne sex aufteilung
# ggplot(tbl) +
#   my.theme +
#   xlab("PubStat") +
#   ylab("Anzahl der Probanden") +
#   geom_histogram(aes(x = as.factor( C_PUB_STAT_PUB_STATUS ) ), stat = "count", position = "stack")


#Boxplots
# df.mean.age neu definieren:mean/min/max nach sex und PG
foo <- function( tbl ) {
    
    #  df.mean.age <-
    data.frame(
        SEX= c(rep("male",5),rep("female",5)),
        MEAN = c( 
            mean( tbl$AGE[tbl$SEX=="male" & tbl$C_PUB_STAT_PUB_STATUS==1] ),  #1. mean vom age, von male in PG1
            mean( tbl$AGE[tbl$SEX=="male" & tbl$C_PUB_STAT_PUB_STATUS==2] ),  # mean vom age, von male in PG2  usw.
            mean( tbl$AGE[tbl$SEX=="male" & tbl$C_PUB_STAT_PUB_STATUS==3] ),
            mean( tbl$AGE[tbl$SEX=="male" & tbl$C_PUB_STAT_PUB_STATUS==4] ),
            mean( tbl$AGE[tbl$SEX=="male" & tbl$C_PUB_STAT_PUB_STATUS==5] ),
            mean( tbl$AGE[tbl$SEX=="female" & tbl$C_PUB_STAT_PUB_STATUS==1] ),
            mean( tbl$AGE[tbl$SEX=="female" & tbl$C_PUB_STAT_PUB_STATUS==2] ),
            mean( tbl$AGE[tbl$SEX=="female" & tbl$C_PUB_STAT_PUB_STATUS==3] ),
            mean( tbl$AGE[tbl$SEX=="female" & tbl$C_PUB_STAT_PUB_STATUS==4] ),
            mean( tbl$AGE[tbl$SEX=="female" & tbl$C_PUB_STAT_PUB_STATUS==5] ) ),
        SD = c( 
            sd( tbl$AGE[tbl$SEX=="male" & tbl$C_PUB_STAT_PUB_STATUS==1] ),
            sd( tbl$AGE[tbl$SEX=="male" & tbl$C_PUB_STAT_PUB_STATUS==2] ),
            sd( tbl$AGE[tbl$SEX=="male" & tbl$C_PUB_STAT_PUB_STATUS==3] ),
            sd( tbl$AGE[tbl$SEX=="male" & tbl$C_PUB_STAT_PUB_STATUS==4] ),
            sd( tbl$AGE[tbl$SEX=="male" & tbl$C_PUB_STAT_PUB_STATUS==5] ),
            sd( tbl$AGE[tbl$SEX=="female" & tbl$C_PUB_STAT_PUB_STATUS==1] ),
            sd( tbl$AGE[tbl$SEX=="female" & tbl$C_PUB_STAT_PUB_STATUS==2] ),
            sd( tbl$AGE[tbl$SEX=="female" & tbl$C_PUB_STAT_PUB_STATUS==3] ),
            sd( tbl$AGE[tbl$SEX=="female" & tbl$C_PUB_STAT_PUB_STATUS==4] ),
            sd( tbl$AGE[tbl$SEX=="female" & tbl$C_PUB_STAT_PUB_STATUS==5] ) ),
        
        MEDIAN = c( 
            median( tbl$AGE[tbl$SEX=="male" & tbl$C_PUB_STAT_PUB_STATUS==1] ),
            median( tbl$AGE[tbl$SEX=="male" & tbl$C_PUB_STAT_PUB_STATUS==2] ),
            median( tbl$AGE[tbl$SEX=="male" & tbl$C_PUB_STAT_PUB_STATUS==3] ),
            median( tbl$AGE[tbl$SEX=="male" & tbl$C_PUB_STAT_PUB_STATUS==4] ),
            median( tbl$AGE[tbl$SEX=="male" & tbl$C_PUB_STAT_PUB_STATUS==5] ),
            median( tbl$AGE[tbl$SEX=="female" & tbl$C_PUB_STAT_PUB_STATUS==1] ),
            median( tbl$AGE[tbl$SEX=="female" & tbl$C_PUB_STAT_PUB_STATUS==2] ),
            median( tbl$AGE[tbl$SEX=="female" & tbl$C_PUB_STAT_PUB_STATUS==3] ),
            median( tbl$AGE[tbl$SEX=="female" & tbl$C_PUB_STAT_PUB_STATUS==4] ),
            median( tbl$AGE[tbl$SEX=="female" & tbl$C_PUB_STAT_PUB_STATUS==5] ) ),
        
        MIN = c( 
            min( tbl$AGE[tbl$SEX=="male" & tbl$C_PUB_STAT_PUB_STATUS==1] ),
            min( tbl$AGE[tbl$SEX=="male" & tbl$C_PUB_STAT_PUB_STATUS==2] ),
            min( tbl$AGE[tbl$SEX=="male" & tbl$C_PUB_STAT_PUB_STATUS==3] ),
            min( tbl$AGE[tbl$SEX=="male" & tbl$C_PUB_STAT_PUB_STATUS==4] ),
            min( tbl$AGE[tbl$SEX=="male" & tbl$C_PUB_STAT_PUB_STATUS==5] ),
            min( tbl$AGE[tbl$SEX=="female" & tbl$C_PUB_STAT_PUB_STATUS==1] ),
            min( tbl$AGE[tbl$SEX=="female" & tbl$C_PUB_STAT_PUB_STATUS==2] ),
            min( tbl$AGE[tbl$SEX=="female" & tbl$C_PUB_STAT_PUB_STATUS==3] ),
            min( tbl$AGE[tbl$SEX=="female" & tbl$C_PUB_STAT_PUB_STATUS==4] ),
            min( tbl$AGE[tbl$SEX=="female" & tbl$C_PUB_STAT_PUB_STATUS==5] ) ),
        MAX = c( 
            max( tbl$AGE[tbl$SEX=="male" & tbl$C_PUB_STAT_PUB_STATUS==1] ),
            max( tbl$AGE[tbl$SEX=="male" & tbl$C_PUB_STAT_PUB_STATUS==2] ),
            max( tbl$AGE[tbl$SEX=="male" & tbl$C_PUB_STAT_PUB_STATUS==3] ),
            max( tbl$AGE[tbl$SEX=="male" & tbl$C_PUB_STAT_PUB_STATUS==4] ),
            max( tbl$AGE[tbl$SEX=="male" & tbl$C_PUB_STAT_PUB_STATUS==5] ),
            max( tbl$AGE[tbl$SEX=="female" & tbl$C_PUB_STAT_PUB_STATUS==1] ),
            max( tbl$AGE[tbl$SEX=="female" & tbl$C_PUB_STAT_PUB_STATUS==2] ),
            max( tbl$AGE[tbl$SEX=="female" & tbl$C_PUB_STAT_PUB_STATUS==3] ),
            max( tbl$AGE[tbl$SEX=="female" & tbl$C_PUB_STAT_PUB_STATUS==4] ),
            max( tbl$AGE[tbl$SEX=="female" & tbl$C_PUB_STAT_PUB_STATUS==5] ) ),
        P25 = c( 
            quantile( tbl$AGE[tbl$SEX=="male" & tbl$C_PUB_STAT_PUB_STATUS==1],c(.25)),
            quantile( tbl$AGE[tbl$SEX=="male" & tbl$C_PUB_STAT_PUB_STATUS==2],c(.25)),
            quantile( tbl$AGE[tbl$SEX=="male" & tbl$C_PUB_STAT_PUB_STATUS==3],c(.25)),
            quantile( tbl$AGE[tbl$SEX=="male" & tbl$C_PUB_STAT_PUB_STATUS==4],c(.25)),
            quantile( tbl$AGE[tbl$SEX=="male" & tbl$C_PUB_STAT_PUB_STATUS==5],c(.25)),
            quantile( tbl$AGE[tbl$SEX=="female" & tbl$C_PUB_STAT_PUB_STATUS==1],c(.25)),
            quantile( tbl$AGE[tbl$SEX=="female" & tbl$C_PUB_STAT_PUB_STATUS==2],c(.25)),
            quantile( tbl$AGE[tbl$SEX=="female" & tbl$C_PUB_STAT_PUB_STATUS==3],c(.25)),
            quantile( tbl$AGE[tbl$SEX=="female" & tbl$C_PUB_STAT_PUB_STATUS==4],c(.25)),
            quantile( tbl$AGE[tbl$SEX=="female" & tbl$C_PUB_STAT_PUB_STATUS==5],c(.25))),
        
        P75 = c( 
            quantile( tbl$AGE[tbl$SEX=="male" & tbl$C_PUB_STAT_PUB_STATUS==1],c(.75)),
            quantile( tbl$AGE[tbl$SEX=="male" & tbl$C_PUB_STAT_PUB_STATUS==2],c(.75)),
            quantile( tbl$AGE[tbl$SEX=="male" & tbl$C_PUB_STAT_PUB_STATUS==3],c(.75)),
            quantile( tbl$AGE[tbl$SEX=="male" & tbl$C_PUB_STAT_PUB_STATUS==4],c(.75)),
            quantile( tbl$AGE[tbl$SEX=="male" & tbl$C_PUB_STAT_PUB_STATUS==5],c(.75)),
            quantile( tbl$AGE[tbl$SEX=="female" & tbl$C_PUB_STAT_PUB_STATUS==1],c(.75)),
            quantile( tbl$AGE[tbl$SEX=="female" & tbl$C_PUB_STAT_PUB_STATUS==2],c(.75)),
            quantile( tbl$AGE[tbl$SEX=="female" & tbl$C_PUB_STAT_PUB_STATUS==3],c(.75)),
            quantile( tbl$AGE[tbl$SEX=="female" & tbl$C_PUB_STAT_PUB_STATUS==4],c(.75)),
            quantile( tbl$AGE[tbl$SEX=="female" & tbl$C_PUB_STAT_PUB_STATUS==5],c(.75))),
        
        
        PG = c( c(1:5),c(1:5))  #f?r male/female
    ) }

df.mean.age<- foo( tbl )

(df.mean.age.normalweight<- foo( tbl[ tbl$WGHT_GRPS == "normalweight", ] ))
( df.mean.age.overweight<- foo( tbl[ tbl$WGHT_GRPS == "overweight.and.obese", ])) ##mit klammern wirds gleich geplottet



##t5: neue tbl mit neuen Spalten (mean/min/Max vom alter, nach SEX und PG) 
t5 <-
    merge(
        tbl,
        df.mean.age,
        by.x = c( "SEX", "C_PUB_STAT_PUB_STATUS" ),
        by.y = c( "SEX", "PG" )
    )

# # Boxplot: age by PG and gender  -->mit jitter
# ggplot(t5)+
#   geom_boxplot(aes(x=SEX, y=AGE), fill="gray")+ #
#   geom_errorbar(aes(x=SEX, ymin=MIN,ymax=MAX),size=.3,width=.5)+ ##size=liniendicke, width=breite der min/MAx werte
#   geom_point(aes(x=SEX,y=AGE), position="jitter",alpha=.9)+ ##punktverteilung zus?tzlich darstellen, alpha=tranparenz
#   geom_point(aes(x=SEX,y=MEAN), col="red")+
#   theme_bw() +
#   facet_grid(~C_PUB_STAT_PUB_STATUS)


# --> ohne jitter! (besser finde ich)
ggplot(t5)+
    geom_boxplot(aes(x=SEX, y=AGE), fill="gray")+ #
    geom_errorbar(aes(x=SEX, ymin=MIN,ymax=MAX),size=.3,width=.5)+ ##size=liniendicke, witth=breite der min/MAx werte
    geom_jitter(aes(x=SEX,y=AGE), width = .02,alpha=.2)+ ##punktverteilung zus?tzlich darstellen, alpha=tranparenz
    geom_point(aes(x=SEX,y=MEAN), col="red")+
    xlab("sex")+
    ylab("age[y]")+
    theme_bw() +
    facet_grid(~C_PUB_STAT_PUB_STATUS)+
    ggtitle("Age by puberty groups and sex ")



#1-5 in PG1-5 umbenennen
t5$PG<- c("PG1","PG2", "PG3", "PG4", "PG5")[match(t5$C_PUB_STAT_PUB_STATUS,c(1:5))]

ggplot(t5)+
    geom_boxplot(aes(x=SEX, y=AGE), fill="gray")+ #
    geom_errorbar(aes(x=SEX, ymin=MIN,ymax=MAX),size=.3,width=.5)+ ##size=liniendicke, witth=breite der min/MAx werte
    geom_jitter(aes(x=SEX,y=AGE), width = .02,alpha=.9)+ ##punktverteilung zus?tzlich darstellen, alpha=tranparenz
    geom_point(aes(x=SEX,y=MEAN), col="red")+
    xlab("sex")+
    ylab("age[y]")+
    theme_bw() +
    facet_grid(~PG)
   # ggtitle("Age by puberty groups and sex",subtitle = "wasauch immer" )

##jetzt noch nach weigth groups getrennt
##ersmtal weight groups k?rzer benennen

t5$wc2<- c("NW","OW")[match(t5$WGHT_GRPS, c("normalweight", "overweight.and.obese"))]
ggplot(t5)+
    geom_boxplot(aes(x=wc2, y=AGE), fill="gray")+ #
    geom_errorbar(aes(x=wc2, ymin=MIN,ymax=MAX),size=.3,width=.5)+ ##size=liniendicke, witth=breite der min/MAx werte
    geom_jitter(aes(x=wc2,y=AGE), width = .02,alpha=.9)+ ##punktverteilung zus?tzlich darstellen, alpha=tranparenz
    geom_point(aes(x=wc2,y=MEAN), col="red")+
    xlab("weight group")+
    ylab("age[y]")+
    theme_bw() +
    facet_grid(SEX~PG)+
    labs(title="Age by puberty groups, weight groups and sex",caption = "NW: normalweight\nOW:overweight and obese" )
    # ggtitle("Age by puberty groups, weight groups and sex" )
#   theme(axis.text.x = element_text(angle=90))

# ggsave( filename = "Age_by_puberty_groups_weightGroupsAndSex.png", width=7, height=6)

##Menarchealter
table(tbl$C_PUB_STAT_MENARCHE_WANN, tbl$SEX)
describeBy(tbl$C_PUB_STAT_MENARCHE_WANN)
summary(tbl$C_PUB_STAT_MENARCHE_WANN)

    #outlier angucken: wenn evtl. NAs drin immer check mit !is.na
tbl[!is.na( tbl$C_PUB_STAT_MENARCHE_WANN ) & tbl$C_PUB_STAT_MENARCHE_WANN>15, c("C_DISEASE_TX_FREITEXT_ANGABE", "SEX", "C_ANTHRO_KH_BMI_ORIG", "AGE", "SIC", "CHILD_MED_H_ATC_NAME" )]

    #outlier 16 raus
tbl<- tbl[is.na(tbl$C_PUB_STAT_MENARCHE_WANN)|(!is.na( tbl$C_PUB_STAT_MENARCHE_WANN ) & tbl$C_PUB_STAT_MENARCHE_WANN<16), ]

# female <- subset(tbl, tbl$SEX == "female")    ##darstellung: frequency direkt ?ber Alter?Titel? Table runterschreiben?
# menarchal_age <-  female$C_PUB_STAT_MENARCHE_WANN
# hist(menarchal_age, xlim = c(9, 16), ylim = c(0, 70), 
#      col = "blue", breaks = 10, main = "menarchal age")

ggplot( tbl ) +
    geom_histogram( aes(C_PUB_STAT_MENARCHE_WANN, fill=SEX), stat =  "count" ) + 
    theme_bw( ) + 
    ylab( "frequency" ) +
    scale_fill_manual(values="deeppink", guide=F)+
    scale_x_continuous( "menarchal age [y]", breaks=c(9:16), labels=c(9:16) )+ ##--> wie besser mit alterseineteilung?
    annotate(geom="text",x=15, y=50, label="n=210" ) +
    theme( panel.grid = element_blank())#+
# facet_grid(.~WGHT_GRPS)

##das gleiche f?r Stimmbruch:
tbl$mutation.age.cat <-
  cut(tbl$FB_SK_CH_F0012+.001,breaks = c(0:21), labels= (0:20))

View( tbl[ ,c( "mutation.age.cat", "FB_SK_CH_F0012" )])

range( tbl$FB_SK_CH_F0012, na.rm = T )


max(tbl$FB_SK_CH_F0012,na.rm = T)

table(round(tbl$FB_SK_CH_F0012))


ggplot( tbl[ !is.na( tbl$FB_SK_CH_F0012 ), ] ) +
  geom_histogram( aes(mutation.age.cat), stat =  "count" , fill= "blue") + 
  theme.histo +
  ylab( "frequency" ) +
 # scale_fill_manual(values="blue", guide=F)+
 scale_x_discrete( "age of voice break [y]" )+ ##--> wie besser mit alterseineteilung?
  annotate(geom="text",x=8, y=40, label="n=125" )
  
ggsave("Frequencies_of_voicebreak.png", width = 7, height = 5)

##getrennt nach weight group 
(tbl.cnt <-
        tbl[ !is.na( tbl$C_PUB_STAT_MENARCHE_WANN ), ] %>% 
        group_by(WGHT_GRPS ) %>% 
        summarise( cnt.wg2 = n( ), 
                   mean.menarchalage.wg2= mean(C_PUB_STAT_MENARCHE_WANN),
                   sd.menarchalage.wg2= sd(C_PUB_STAT_MENARCHE_WANN),
                   median.menarchalage.wg2= median(C_PUB_STAT_MENARCHE_WANN)))

ggplot( tbl ) +
    geom_histogram( aes(C_PUB_STAT_MENARCHE_WANN, fill=SEX), stat =  "count" ) + 
    theme_bw( ) + 
    ylab( "frequency" ) +
    scale_fill_manual(values="deeppink", guide=F)+
    scale_x_continuous( "menarchal age [y]", breaks=c(9:16), labels=c(9:16) )+ ##--> wie besser mit alterseineteilung?
    geom_text( data= tbl.cnt,x=15, y=40, aes(label=paste0("n = ",cnt.wg2)), check_overlap = T )+
    facet_grid(.~WGHT_GRPS)

# ggsave(filename = "menarchalage_weightGroups.png", width=6, height = 5)


##boxplot menarchal age 

# ggplot(tbl)+
#   geom_boxplot(aes(x=female, y=C_PUB_STAT_MENARCHE_WANN), fill="gray")+ 
#   geom_errorbar(data=tbl,aes(x=female, y=C_PUB_STAT_MENARCHE_WANN),size=.3,width=.5)+
#   # geom_errorbar(data=df.mean.age,aes(x=SEX, ymin=MIN,ymax=MAX),size=.3,width=.5)+ ##size=liniendicke, witth=breite der min/MAx werte
#   geom_jitter(aes(x=female,y=C_PUB_STAT_MENARCHE_WANN),alpha=.1,width = .01)+ ##punktverteilung zus?tzlich darstellen, alpha=tranparenz
#   # geom_point(data = tbl, aes(x=SEX,y=MEAN), col="red")+
#   theme_bw()

## Plot: mittleres Menarchealter --> eher nicht verwenden
# female <- subset(tbl, tbl$SEX == "female")                   ##wie nur female, wie median dicker?mean einzeichnen?
# p1<- ggplot(tbl, aes(x = SEX, y = C_PUB_STAT_MENARCHE_WANN)) +
#   geom_boxplot()
# p1 ##wie mache ich es nur f?r female?
# p1 <- p1 + scale_x_discrete(name = "female") +
#   scale_y_continuous(name = "menarchal age")   ##?ndert die Achsenbezeichnungen
# p1 <- p1 + ggtitle("menarchal age")    ##titel hinzuf?gen
# p1


##BMI by age & gender

describeBy(tbl$C_ANTHRO_KH_BMI_ORIG, tbl$SEX)
summary(tbl$C_ANTHRO_KH_BMI_ORIG[tbl$SEX=="male"])
summary(tbl$C_ANTHRO_KH_BMI_ORIG[tbl$SEX=="female"]) ##wie kann ich hier noch "all"/sum berechnen (k?rner s. 10)

describeBy(tbl$C_ANTHRO_KH_BMI_ADJ, tbl$SEX)
summary(tbl$C_ANTHRO_KH_BMI_ADJ[tbl$SEX=="male"])
summary(tbl$C_ANTHRO_KH_BMI_ADJ[tbl$SEX=="female"])



# tbl$WGHT_GRPS_BMI <-
#  cut(
#    x = tbl$C_ANTHRO_KH_BMI_ORIG,
#    breaks =  c( -Inf, 18.5,  25, 30, +Inf ),   ##stimmt das so? Es werden viel zu viele underweights angezeigt!!!
#    labels = c( "underweight", "normalweight", "overweight", "obese" ) )


##Deskriptive Statistik BMI 2 weight groups
df.BMI.descriptiv <-
  data.frame(
    SEX= c(rep("male",2),rep("female",2)),
    MEAN = c( 
      mean( tbl$C_ANTHRO_KH_BMI_ORIG [tbl$SEX=="male" & tbl$WGHT_GRPS=="normalweight"] ), 
      mean( tbl$C_ANTHRO_KH_BMI_ORIG [tbl$SEX=="male" & tbl$WGHT_GRPS=="overweight.and.obese"] ), 
      mean( tbl$C_ANTHRO_KH_BMI_ORIG [tbl$SEX=="female" & tbl$WGHT_GRPS=="normalweight"] ), 
      mean( tbl$C_ANTHRO_KH_BMI_ORIG [tbl$SEX=="female" & tbl$WGHT_GRPS=="overweight.and.obese"] )), 
    SD = c( 
      sd( tbl$C_ANTHRO_KH_BMI_ORIG [tbl$SEX=="male" & tbl$WGHT_GRPS=="normalweight"] ), 
      sd( tbl$C_ANTHRO_KH_BMI_ORIG [tbl$SEX=="male" & tbl$WGHT_GRPS=="overweight.and.obese"] ), 
      sd( tbl$C_ANTHRO_KH_BMI_ORIG [tbl$SEX=="female" & tbl$WGHT_GRPS=="normalweight"] ), 
      sd( tbl$C_ANTHRO_KH_BMI_ORIG [tbl$SEX=="female" & tbl$WGHT_GRPS=="overweight.and.obese"] )), 
    MEDIAN = c( 
      median( tbl$C_ANTHRO_KH_BMI_ORIG [tbl$SEX=="male" & tbl$WGHT_GRPS=="normalweight"] ), 
      median( tbl$C_ANTHRO_KH_BMI_ORIG [tbl$SEX=="male" & tbl$WGHT_GRPS=="overweight.and.obese"] ), 
      median( tbl$C_ANTHRO_KH_BMI_ORIG [tbl$SEX=="female" & tbl$WGHT_GRPS=="normalweight"] ), 
      median( tbl$C_ANTHRO_KH_BMI_ORIG [tbl$SEX=="female" & tbl$WGHT_GRPS=="overweight.and.obese"] )), 
    MIN = c( 
      min( tbl$C_ANTHRO_KH_BMI_ORIG [tbl$SEX=="male" & tbl$WGHT_GRPS=="normalweight"] ), 
      min( tbl$C_ANTHRO_KH_BMI_ORIG [tbl$SEX=="male" & tbl$WGHT_GRPS=="overweight.and.obese"] ), 
      min( tbl$C_ANTHRO_KH_BMI_ORIG [tbl$SEX=="female" & tbl$WGHT_GRPS=="normalweight"] ), 
      min( tbl$C_ANTHRO_KH_BMI_ORIG [tbl$SEX=="female" & tbl$WGHT_GRPS=="overweight.and.obese"] )), 
    MAX = c( 
      max( tbl$C_ANTHRO_KH_BMI_ORIG [tbl$SEX=="male" & tbl$WGHT_GRPS=="normalweight"] ), 
      max( tbl$C_ANTHRO_KH_BMI_ORIG [tbl$SEX=="male" & tbl$WGHT_GRPS=="overweight.and.obese"] ), 
      max( tbl$C_ANTHRO_KH_BMI_ORIG [tbl$SEX=="female" & tbl$WGHT_GRPS=="normalweight"] ), 
      max( tbl$C_ANTHRO_KH_BMI_ORIG [tbl$SEX=="female" & tbl$WGHT_GRPS=="overweight.and.obese"] )), 
    
    P25 = c( 
      quantile( tbl$C_ANTHRO_KH_BMI_ORIG [tbl$SEX=="male" & tbl$WGHT_GRPS=="normalweight"],c(.25)), 
      quantile( tbl$C_ANTHRO_KH_BMI_ORIG [tbl$SEX=="male" & tbl$WGHT_GRPS=="overweight.and.obese"],c(.25)), 
      quantile( tbl$C_ANTHRO_KH_BMI_ORIG [tbl$SEX=="female" & tbl$WGHT_GRPS=="normalweight"],c(.25)), 
      quantile( tbl$C_ANTHRO_KH_BMI_ORIG [tbl$SEX=="female" & tbl$WGHT_GRPS=="overweight.and.obese"],c(.25))), 
    
    P75 = c( 
      quantile( tbl$C_ANTHRO_KH_BMI_ORIG [tbl$SEX=="male" & tbl$WGHT_GRPS=="normalweight"],c(.75)), 
      quantile( tbl$C_ANTHRO_KH_BMI_ORIG [tbl$SEX=="male" & tbl$WGHT_GRPS=="overweight.and.obese"],c(.75)), 
      quantile( tbl$C_ANTHRO_KH_BMI_ORIG [tbl$SEX=="female" & tbl$WGHT_GRPS=="normalweight"],c(.75)), 
      quantile( tbl$C_ANTHRO_KH_BMI_ORIG [tbl$SEX=="female" & tbl$WGHT_GRPS=="overweight.and.obese"],c(.75))), 
    
    weight.groups = c( c(1:2),c(1:2))  #f?r male/female
  )

df.BMI.descriptiv  

addmargins(table(tbl$WGHT_GRPS,tbl$SEX))

##Boxplot: mittlerer BMIweight groups
ggplot(tbl)+
  geom_boxplot(aes(x=SEX, y=C_ANTHRO_KH_BMI_ORIG), fill="gray")+ #
  geom_jitter(aes(x=SEX,y=C_ANTHRO_KH_BMI_ORIG), width = .02,alpha=.01)+ ##punktverteilung zus?tzlich darstellen, alpha=tranparenz
  xlab("sex")+
  ylab("BMI")+
  theme_bw() +
  ylim(10,40)+
  facet_grid(~WGHT_GRPS) 

##wie farbig machen? mit       scale_fill_manual( "sex", values = c( "deeppink", "blue" ) ) ghets nicht...


##2WG:scatterplot mit mean
ggplot(tbl,aes( AGE, C_ANTHRO_KH_BMI_ADJ, 
               col = WGHT_GRPS ))+scale_x_continuous(name = "age [y]", seq( 3,17,by = 2))+
    geom_jitter( )+
    ylab("BMI (SDS)")+
    geom_smooth(method = "lm")

#2WG: mit gleitendem mean
ggplot(tbl,aes( AGE, C_ANTHRO_KH_BMI_ADJ, 
               col =  WGHT_GRPS ))+scale_x_continuous(name = "age [y]", seq( 3,17,by = 2))+
    geom_jitter(alpha=.5 )+
    ylab("BMI (SDS)")+
  geom_hline(yintercept = 0, linetype=2) + 
  geom_smooth(method = "lm")+
    theme_bw()+
    theme( panel.grid = element_blank( ) )+
    scale_color_manual(name="weight groups", values = c("forestgreen", "indianred") )
  

# ggsave(filename = "Scatterplot_weightGroups.pdf")

##2WG:ohne means
ggplot(tbl,aes( AGE, C_ANTHRO_KH_BMI_ADJ, 
               col =  WGHT_GRPS ))+scale_x_continuous(name = "Age", seq( 3,17,by = 2))+
    geom_jitter( )+
    ylab("BMI (SDS)")


##Frequency weight groups nach alter
addmargins(table(tbl$WGHT_GRPS,tbl$AGE.CAT1))


##Histogramm: frequencies of weight groups
ggplot(tbl) +
    geom_histogram(aes(WGHT_GRPS, fill = SEX  ), stat = "count", position = "dodge") +
    # facet_grid(. ~ SEX)+
    theme_bw() +
    xlab("weight groups") +
    ylab("frequency") +
    scale_fill_manual( "sex", values = c( "deeppink", "blue" ) ) 

    

  

##Height by age&gender
describeBy(tbl$C_ANTHRO_KH_HEIGHT_ORIG, tbl$SEX)
summary(tbl$C_ANTHRO_KH_HEIGHT_ORIG[tbl$SEX=="male"])
summary(tbl$C_ANTHRO_KH_HEIGHT_ORIG[tbl$SEX=="female"])

ggplot(tbl) +
    geom_point(aes(x = AGE, y = C_ANTHRO_KH_HEIGHT_ORIG, col=SEX))+ 
    scale_x_continuous(name = "age [y]", seq( 3,17,by = 2))+
    scale_color_manual( "sex", values = c( "deeppink", "blue" ) ) +
    xlab("age [y]") +
    ylab("height [cm]")+
    facet_grid(.~SEX)+
    theme.histo.facet
   

##Weight by age&gender

describeBy(tbl$C_ANTHRO_KH_WEIGHT_ORIG, tbl$SEX)
summary(tbl$C_ANTHRO_KH_WEIGHT_ORIG[tbl$SEX=="male"])
summary(tbl$C_ANTHRO_KH_WEIGHT_ORIG[tbl$SEX=="female"])

ggplot(tbl) +
    geom_point(aes(x = AGE, y = C_ANTHRO_KH_WEIGHT_ORIG, col=SEX))+
    scale_x_continuous(name = "age [y]", seq( 3,17,by = 2))+
    scale_color_manual( "sex", values = c( "deeppink", "blue" ) ) +
    xlab("age [y]") +
    ylab("weight [kg]")+
    facet_grid(.~SEX)+
    theme.histo.facet
#outlier 180kg angucken:
# tbl[!is.na( tbl$C_ANTHRO_KH_WEIGHT_ORIG ) & tbl$C_ANTHRO_KH_WEIGHT_ORIG>150, c("C_DISEASE_TX_FREITEXT_ANGABE", "SEX", "C_ANTHRO_KH_BMI_ORIG", "AGE", "SIC", "CHILD_MED_H_ATC_NAME" )]




##Winkler
table(tbl$SES, tbl$SEX)

range(tbl$D00177_SCORE_FAM, na.rm=T)


ggplot(tbl) +
    geom_histogram(aes(tbl$SES, fill = SEX  ), stat = "count", position = "dodge") +
    # facet_grid(. ~ SEX)+
    theme_bw() +
    theme( panel.grid = element_blank( ) )+
    xlab("socioeconomic status") +
    ylab("frequency") +
    scale_fill_manual( "sex", values = c( "deeppink", "blue" ) )

# ggplot(tbl) +
#     geom_histogram(aes(tbl$SES, fill = SEX  ), stat = "count", position = "dodge") +
#     # facet_grid(. ~ SEX)+
#     theme_bw() +
#     xlab("socioeconomic status") +
#     ylab("frequency") +
#     scale_fill_manual( "sex", values = c( "deeppink", "blue" ) )+
#     facet_grid(.~WGHT_GRPS)+
#   theme( panel.grid = element_blank( ) )


scale_color_manual( values = c( "deeppink", "blue" ), guide = F ),
scale_fill_manual( values = c( "deeppink", "blue" ), guide = F ),
theme( panel.grid = element_blank( ) ),
facet_grid( SEX ~ . ) )   

ggplot(tbl) +
    geom_histogram(aes(tbl$SES, fill = SEX  ), stat = "count", position = "dodge") +
    # facet_grid(. ~ SEX)+
    theme_bw() +
    xlab("socioeconomic status") +
    ylab("frequency") +
    scale_fill_manual( "sex", values = c( "deeppink", "blue" ) )+
    facet_grid(WGHT_GRPS~.)

ggplot(tbl[!is.na(tbl$D00177_SCORE_FAM),])+
    geom_point(aes(x=D00177_SCORE_FAM, y= C_ANTHRO_KH_BMI_ADJ , col=SES))+ ##col=scatterplot, fill bei boxplot/hist (hier w?re col der Rand)
    geom_smooth(aes(x=D00177_SCORE_FAM, y= C_ANTHRO_KH_BMI_ADJ), method = "lm")+
    xlab("socioecenomic score")+
    ylab("BMI (SDS)")+
    scale_color_discrete(name="socioeconomic status")  ##brewer bietet farben an


tbl. <- tbl[!is.na(tbl$SES),]

##Deskriptive Statistik BMI nach SES
df.BMI.descriptiv.SES <-
    data.frame (
                SEX= c(rep("male",3),rep("female",3)),
                MEAN = c( 
                    mean( tbl.$C_ANTHRO_KH_BMI_ORIG [tbl.$SEX=="male" & tbl.$SES=="low"], na.rm=T ), 
                    mean( tbl.$C_ANTHRO_KH_BMI_ORIG [tbl.$SEX=="male" & tbl.$SES=="mid"] , na.rm=T), 
                    mean( tbl.$C_ANTHRO_KH_BMI_ORIG [tbl.$SEX=="male" & tbl.$SES=="high"], na.rm=T ), 
                    mean( tbl.$C_ANTHRO_KH_BMI_ORIG [tbl.$SEX=="female" & tbl.$SES=="low"] , na.rm=T), 
                    mean( tbl.$C_ANTHRO_KH_BMI_ORIG [tbl.$SEX=="female" & tbl.$SES=="mid"], na.rm=T ), 
                    mean( tbl.$C_ANTHRO_KH_BMI_ORIG [tbl.$SEX=="female" & tbl.$SES=="high"], na.rm=T )), 
                SD = c(   
                    sd( tbl.$C_ANTHRO_KH_BMI_ORIG [tbl.$SEX=="male" & tbl.$SES=="low"] , na.rm=T), 
                    sd( tbl.$C_ANTHRO_KH_BMI_ORIG [tbl.$SEX=="male" & tbl.$SES=="mid"], na.rm=T ), 
                    sd( tbl.$C_ANTHRO_KH_BMI_ORIG [tbl.$SEX=="male" & tbl.$SES=="high"], na.rm=T ), 
                    sd( tbl.$C_ANTHRO_KH_BMI_ORIG [tbl.$SEX=="female" & tbl.$SES=="low"] , na.rm=T), 
                    sd( tbl.$C_ANTHRO_KH_BMI_ORIG [tbl.$SEX=="female" & tbl.$SES=="mid"] , na.rm=T), 
                    sd( tbl.$C_ANTHRO_KH_BMI_ORIG [tbl.$SEX=="female" & tbl.$SES=="high"] , na.rm=T)), 
                MEDIAN = c( 
                    median( tbl.$C_ANTHRO_KH_BMI_ORIG [tbl.$SEX=="male" & tbl.$SES=="low"], na.rm=T ), 
                    median( tbl.$C_ANTHRO_KH_BMI_ORIG [tbl.$SEX=="male" & tbl.$SES=="mid"], na.rm=T ), 
                    median( tbl.$C_ANTHRO_KH_BMI_ORIG [tbl.$SEX=="male" & tbl.$SES=="high"] , na.rm=T), 
                    median( tbl.$C_ANTHRO_KH_BMI_ORIG [tbl.$SEX=="female" & tbl.$SES=="low"] , na.rm=T), 
                    median( tbl.$C_ANTHRO_KH_BMI_ORIG [tbl.$SEX=="female" & tbl.$SES=="mid"] , na.rm=T), 
                    median( tbl.$C_ANTHRO_KH_BMI_ORIG [tbl.$SEX=="female" & tbl.$SES=="high"], na.rm=T )),
                MIN = c( 
                    min( tbl.$C_ANTHRO_KH_BMI_ORIG [tbl.$SEX=="male" & tbl.$SES=="low"] , na.rm=T), 
                    min( tbl.$C_ANTHRO_KH_BMI_ORIG[tbl.$SEX=="male" & tbl.$SES=="mid"], na.rm=T ), 
                    min( tbl.$C_ANTHRO_KH_BMI_ORIG [tbl.$SEX=="male" & tbl.$SES=="high"], na.rm=T ), 
                    min( tbl.$C_ANTHRO_KH_BMI_ORIG [tbl.$SEX=="female" & tbl.$SES=="low"] , na.rm=T), 
                    min( tbl.$C_ANTHRO_KH_BMI_ORIG [tbl.$SEX=="female" & tbl.$SES=="mid"] , na.rm=T), 
                    min( tbl.$C_ANTHRO_KH_BMI_ORIG [tbl.$SEX=="female" & tbl.$SES=="high"] , na.rm=T)),
                MAX = c(
                    max( tbl.$C_ANTHRO_KH_BMI_ORIG [tbl.$SEX=="male" & tbl.$SES=="low"] , na.rm=T), 
                    max( tbl.$C_ANTHRO_KH_BMI_ORIG [tbl.$SEX=="male" & tbl.$SES=="mid"], na.rm=T ), 
                    max( tbl.$C_ANTHRO_KH_BMI_ORIG [tbl.$SEX=="male" & tbl.$SES=="high"], na.rm=T ), 
                    max( tbl.$C_ANTHRO_KH_BMI_ORIG [tbl.$SEX=="female" & tbl.$SES=="low"] , na.rm=T), 
                    max( tbl.$C_ANTHRO_KH_BMI_ORIG [tbl.$SEX=="female" & tbl.$SES=="mid"] , na.rm=T), 
                    max( tbl.$C_ANTHRO_KH_BMI_ORIG [tbl.$SEX=="female" & tbl.$SES=="high"], na.rm=T )),
                P25 = c( 
                    quantile( tbl.$C_ANTHRO_KH_BMI_ORIG [tbl.$SEX=="male" & tbl.$SES=="low"], c(.25), na.rm=T ), 
                    quantile( tbl.$C_ANTHRO_KH_BMI_ORIG [tbl.$SEX=="male" & tbl.$SES=="mid"], c(.25), na.rm=T ), 
                    quantile( tbl.$C_ANTHRO_KH_BMI_ORIG [tbl.$SEX=="male" & tbl.$SES=="high"], c(.25) , na.rm=T), 
                    quantile( tbl.$C_ANTHRO_KH_BMI_ORIG [tbl.$SEX=="female" & tbl.$SES=="low"], c(.25), na.rm=T ), 
                    quantile( tbl.$C_ANTHRO_KH_BMI_ORIG [tbl.$SEX=="female" & tbl.$SES=="mid"], c(.25), na.rm=T ), 
                    quantile( tbl.$C_ANTHRO_KH_BMI_ORIG [tbl.$SEX=="female" & tbl.$SES=="high"], c(.25), na.rm=T )),
                P75 = c( 
                    quantile( tbl.$C_ANTHRO_KH_BMI_ORIG [tbl.$SEX=="male" & tbl.$SES=="low"], c(.75), na.rm=T ), 
                    quantile( tbl.$C_ANTHRO_KH_BMI_ORIG [tbl.$SEX=="male" & tbl.$SES=="mid"], c(.75), na.rm=T ), 
                    quantile( tbl.$C_ANTHRO_KH_BMI_ORIG [tbl.$SEX=="male" & tbl.$SES=="high"], c(.75) , na.rm=T), 
                    quantile( tbl.$C_ANTHRO_KH_BMI_ORIG [tbl.$SEX=="female" & tbl.$SES=="low"], c(.75) , na.rm=T), 
                    quantile( tbl.$C_ANTHRO_KH_BMI_ORIG [tbl.$SEX=="female" & tbl.$SES=="mid"], c(.75), na.rm=T ), 
                    quantile( tbl.$C_ANTHRO_KH_BMI_ORIG [tbl.$SEX=="female" & tbl.$SES=="high"], c(.75), na.rm=T )),
                
                socioeconomic.status = c( c(1:3),c(1:3))  #f?r male/female
    )

df.BMI.descriptiv.SES




# LH
describeBy(tbl$LH_S_NUM_VALUE, tbl$SEX) 
summary(tbl$LH_S_NUM_VALUE[tbl$SEX=="male"])
summary(tbl$LH_S_NUM_VALUE[tbl$SEX=="female"]) ##wie kann ich hier noch "all"/sum berechnen (k?rner s. 10)


# FSH
describeBy(tbl$FSH_S_NUM_VALUE, tbl$SEX) 
summary(tbl$FSH_S_NUM_VALUE[tbl$SEX=="male"])
summary(tbl$FSH_S_NUM_VALUE[tbl$SEX=="female"])

##Bodeneffekt testen
# tbl$z <-
    # ifelse( tbl$LH_S_NUM_VALUE > .1, 1,0)  ##neue Spalte um werten zahl zuzuweisen: alle werte>0,1 =1, die anderen=0

#Plot: LH~age nach SEX
# ggplot(tbl)+
#     geom_point( aes( x=AGE, y=LH_S_NUM_VALUE, col=z))+
#     scale_x_continuous(name = "age", seq( 3,17,by = 2))+
#     ylab("LH(IU/L")+
#     facet_grid(.~SEX)+
#     scale_fill_manual( "sex", values = c( "deeppink", "blue" ) )

#Plot: LH~age nach SEX
ggplot(tbl)+
    geom_point( aes( x=AGE, y=LH_S_NUM_VALUE, col=SEX))+
    scale_x_continuous(name = "age [y]", seq( 3,17,by = 2))+
    ylab("LH [IU/L]")+
    scale_color_manual( "sex", values = c( "deeppink", "blue" ) )+
  theme.histo.facet+
  facet_grid(.~SEX)


#Plot: FSH~age nach SEX
ggplot(tbl) +
    geom_point(aes(x = AGE, y = FSH_S_NUM_VALUE, col=SEX))+
    scale_x_continuous(name = "age [y]", seq( 3,17,by = 2))+
    ylab("FSH [IU/L]")+
    scale_color_manual( "sex", values = c( "deeppink", "blue" ) ) +
    theme.histo.facet+
    facet_grid(.~SEX)

# #Plot mean- LH in beiden BMI-Groups
ggplot(tbl)+
    geom_boxplot(aes(x= SEX, y=LH_S_NUM_VALUE), fill="gray")+
    # geom_errorbar(aes(x=SEX, ymin=LH_S_NUM_VALUE, ),size=.3,width=.5)+ ##size=liniendicke, witth=breite der min/MAx werte
    # geom_jitter(aes(x=SEX,y=LH_S_NUM_VALUE), width = .02,alpha=.01)+ ##punktverteilung zus?tzlich darstellen, alpha=tranparenz
    theme_bw() +
    xlab("sex")+
    ylab("LH")+
    facet_grid(WGHT_GRPS~C_PUB_STAT_PUB_STATUS)+ 
    scale_y_log10()

ggplot(tbl)+
    geom_boxplot(aes(x= tbl$WGHT_GRPS, y=LH_S_NUM_VALUE), fill="gray")+
    # geom_errorbar(aes(x=SEX, ymin=LH_S_NUM_VALUE, ),size=.3,width=.5)+ ##size=liniendicke, witth=breite der min/MAx werte
    # geom_jitter(aes(x=SEX,y=LH_S_NUM_VALUE), width = .02,alpha=.01)+ ##punktverteilung zus?tzlich darstellen, alpha=tranparenz
    theme_bw() +
    xlab("weight groups")+
    ylab("LH [IU/L]")+
    facet_grid(SEX~C_PUB_STAT_PUB_STATUS)+
    # scale_y_log10()
    ylim(0,8)+   ##f?r diesen Plot wurden die Ausrei?er weggelassen
    theme(panel.grid = element_blank())  ##ohne gitter

ggplot(tbl)+
    geom_boxplot(aes(x= tbl$WGHT_GRPS, y=FSH_S_NUM_VALUE), fill="gray")+
    theme_bw() +
    xlab("weight groups")+
    ylab("FSH [IU/L]")+
    facet_grid(SEX~C_PUB_STAT_PUB_STATUS)+
    # scale_y_log10()
    ylim(0,8)   ##f?r diesen Plot wurden die Ausrei?er weggelassen


t5$wc2<- c("NW","OW")[match(t5$WGHT_GRPS, c("normalweight", "overweight.and.obese"))]
ggplot(t5)+
  geom_boxplot(aes(x= t5$wc2, y=LH_S_NUM_VALUE, fill= wc2))+
  scale_fill_manual(values=c("forestgreen", "indianred"), guide = F)+
  xlab("weight group")+
  ylab("LH [IU/L]")+
  theme_bw() +
  theme( panel.grid = element_blank( ) )+
  ylim(0,8)+
  facet_grid(SEX~C_PUB_STAT_PUB_STATUS)+
  labs(title="LH by puberty groups, weight groups and sex",caption = "NW: normalweight\nOW:overweight and obese" )
# ggtitle("Age by puberty groups, weight groups and sex" )
#   theme(axis.text.x = element_text(angle=90))




##LH mit gleitendem mean weight groups
ggplot(tbl,aes( AGE, LH_S_NUM_VALUE, 
               col = WGHT_GRPS ))+scale_x_continuous(name = "age [y]", seq( 3,17,by = 2))+
    geom_jitter( alpha=.3 )+
   scale_color_manual(name="weight groups", values= c("forestgreen", "indianred","forestgreen", "indianred"))+
    ylab("LH [IU/L]")+
    ylim(0,20)+
  facet_grid(.~SEX)+
    geom_smooth(method = "lm")+theme_bw()+
  theme( panel.grid = element_blank( ) )
    # scale_color_discrete(name="weight groups")
    # ggtitle("LH by age and weight groups")

##LH mit mean nach SES
ggplot(tbl,aes( AGE, LH_S_NUM_VALUE, 
               col = SES ))+scale_x_continuous(name = "age [y]", seq( 3,17,by = 2))+
    geom_jitter( )+
    ylab("LH [IU/L]")+
    ylim(0,20)+
    geom_smooth(method = "loess")+theme_bw()+
    scale_color_discrete(name="socioeconomic status")+
    ggtitle("LH by age and socioeconomic status")

#LH/FSH-ratio 
tbl$LH.FSH.ratio<-
    tbl$LH_S_NUM_VALUE/tbl$FSH_S_NUM_VALUE
##
(lf.ratio.summary <-
        tbl%>% 
        group_by(SEX, WGHT_GRPS, C_PUB_STAT_PUB_STATUS ) %>%
        summarise(
            LF.ratio.mean=mean(LH.FSH.ratio, na.rm=T),
            LF.ratio.median=median(LH.FSH.ratio, na.rm=T),
            LF.ratio.sd=sd(LH.FSH.ratio, na.rm=T)
        )) ##spalte in der ich das reinschreibe=LF.ratio.mean
# View(lf.ratio.summary)

ggplot(lf.ratio.summary)+
    geom_point(aes(WGHT_GRPS, LF.ratio.mean))+
    facet_grid(C_PUB_STAT_PUB_STATUS~SEX)

##boxplot menarchealter nach weight groups--> nicht gut
# ggplot(tbl)+
#   geom_boxplot(aes(x=(SEX=="female"), y=C_PUB_STAT_MENARCHE_WANN), fill="gray")+ 
#   geom_jitter(aes(x=(SEX=="female"),y=C_PUB_STAT_MENARCHE_WANN), width = .02,alpha=.01)+ ##punktverteilung zus?tzlich darstellen, alpha=tranparenz
#   xlab("female")+
#   ylab("menarchal age")+
#   theme_bw() +
#   ylim(10,40)+
#   facet_grid(~WGHT_GRPS)


##LH ~ Alter, nach Sex und PubStat
# ggplot(tbl) +
#   geom_point(aes(x = AGE, y = LH_S_NUM_VALUE, col=SEX))+
#   facet_grid(C_PUB_STAT_PUB_STATUS~SEX )

# LH~Alter nach SEX und PubStat mit limitierterer Y-Achse:
# ggplot(tbl) +
#   geom_point(aes(x = AGE, y = LH_S_NUM_VALUE, col=SEX))+
#   facet_grid(C_PUB_STAT_PUB_STATUS~SEX ) +
#   ylim( c(0, 10))
# ggsave( filename = "LH~Alter_nachSEXundPubstat.pdf" )

# ##im selben Plot wie vorher die Adip?sen-Falgs markieren
# tbl$C_DISEASE_TX_ADIP[ is.na( tbl$C_DISEASE_TX_ADIP ) ] <- 2
#
# ggplot(tbl) +
#   geom_point(aes(x = AGE, y = LH_S_NUM_VALUE, col=as.factor(C_DISEASE_TX_ADIP)))+ #as factor: da adipostitas flag 0 oder 1
#   facet_grid(C_PUB_STAT_PUB_STATUS~SEX ) +
#   scale_color_manual( name = "ADI", values = c( "red", "green", "blue") )+
#   ylim( c(0, 10))
# # ggsave( filename = "LH~Alter_nachSEXundPubstatAdip?seMarkiert.pdf" )
#
#
# # # neue spalte f?r kohorten zuweisung anlegen
# tbl$Kohorte <- NA
# tbl$Kohorte[grepl( "A", tbl$SCIGROUP, perl = T) ] <- "A-Kohorte"
# tbl$Kohorte[grepl( "B", tbl$SCIGROUP, perl = T) ] <- "B-Kohorte"
#
# ggplot(tbl) +
#   geom_point(aes(x = AGE, y = LH_S_NUM_VALUE, col=Kohorte))+ #as factor: da adipostitas flag 0 oder 1
#   facet_grid(C_PUB_STAT_PUB_STATUS~SEX )
# #ggsave( filename = "LH~Alter_nachSEXundPubstat.pdf" )




# ##lineare Regression LH
# plot(tbl$C_ANTHRO_KH_BMI_ORIG, tbl$LH_S_NUM_VALUE, xlim = c(0, 50))
# #Regressionsgerade
# abline(lm(tbl$C_ANTHRO_KH_BMI_ORIG~tbl$LH_S_NUM_VALUE), col="red")
#
# # ggplot:
# ggplot (daten, aes (x = C_ANTHRO_KH_BMI_ORIG, y = LH_S_NUM_VALUE, colour = GESCHLECHT)) + geom_point() + geom_smooth(method=loess) + ylim(0, 10)
#
# ## Boxplot: mittlerer LH wert in Abhaengigkeit vom Geschlecht,
# # boxplot(tbl$LH_S_NUM_VALUE ~ SEX, data = tbl) #sagt kaum was aus
#
#
# ##Säulendiagramm LH wert in Abh. vom geschlecht
# histogram( ~  LH_S_NUM_VALUE | SEX, data = tbl)
#
# addmargins(table(tbl$SEX, tbl$AGE.CATEGORIE))
# summary(tbl$AGE[tbl$SEX=="male"])
# table(tbl$SEX) ##in table zusammenfassen
# fivenum(tbl$AGE)
# ?fivenum
# quantile(x = tbl$AGE, c(.1,.9))



##Menarchealter nach weight groups
sum(!is.na(tbl$C_PUB_STAT_MENARCHE_WANN[tbl$SEX=="female"& tbl$WGHT_GRPS=="normalweigt"]), na.rm = T)
sum(!is.na(tbl$C_PUB_STAT_MENARCHE_WANN[tbl$SEX=="female"& tbl$WGHT_GRPS=="overweight.and.obese"]), na.rm = T)
addmargins(table(tbl$C_PUB_STAT_MENARCHE_WANN, tbl$WGHT_GRPS))


mean(tbl$C_PUB_STAT_MENARCHE_WANN[tbl$SEX=="female"& tbl$WGHT_GRPS=="overweight.and.obese"], na.rm = T)
mean(tbl$C_PUB_STAT_MENARCHE_WANN[tbl$SEX=="female"& tbl$WGHT_GRPS=="normalweight"], na.rm = T)
sd(tbl$C_PUB_STAT_MENARCHE_WANN[tbl$SEX=="female"& tbl$WGHT_GRPS=="normalweight"], na.rm = T)
sd(tbl$C_PUB_STAT_MENARCHE_WANN[tbl$SEX=="female"& tbl$WGHT_GRPS=="overweight.and.obese"], na.rm = T)
median(tbl$C_PUB_STAT_MENARCHE_WANN[tbl$SEX=="female"& tbl$WGHT_GRPS=="normalweight"], na.rm = T)
median(tbl$C_PUB_STAT_MENARCHE_WANN[tbl$SEX=="female"& tbl$WGHT_GRPS=="overweight.and.obese"], na.rm = T)


##F?r Jungen Hodenvolumen oder Genitalstatus?

##HV
tbl$HV[is.na(tbl$HV)] <- 0
sum(tbl$HV == 0)  ##710HV
addmargins(table(tbl$HV)) 

##HV li/HV re

# tbl$C_PUB_STAT_HV_LI[is.na(tbl$C_PUB_STAT_HV_LI)] <- 0
# sum(tbl$C_PUB_STAT_HV_LI == 0) 
# addmargins(table(tbl$C_PUB_STAT_HV_LI)) #--> HV li
# 
# tbl$C_PUB_STAT_HV_RE[is.na(tbl$C_PUB_STAT_HV_RE)] <- 0
# sum(tbl$C_PUB_STAT_HV_RE == 0) 
# addmargins(table(tbl$C_PUB_STAT_HV_RE)) #--> 59HV re, aber nur ca. 9 3-4ml

tbl$C_PUB_STAT_G
tbl$C_PUB_STAT_G[is.na(tbl$C_PUB_STAT_G)] <- 0
sum(tbl$C_PUB_STAT_G == 0) 
addmargins(table(tbl$C_PUB_STAT_G)) ##709 G

tbl$C_PUB_STAT_PH[is.na(tbl$C_PUB_STAT_PH)] <- 0
sum(tbl$C_PUB_STAT_PH == 0) 
addmargins(table(tbl$C_PUB_STAT_PH, tbl$SEX)) ##PH bei allen 710 Jungen

##Stimmbruch (FB_SK_CH_F0012)
sum(!is.na(tbl$FB_SK_CH_F0012)) ##134Stimmbrüche
sum(!is.na(tbl$C_PUB_STAT_MENARCHE_WANN)) ##210Menarchen
sum(!is.na(tbl$HV)&tbl$HV>0) # 710 HV

mean(tbl$FB_SK_CH_F0012[tbl$SEX=="male"], na.rm = T)

mean(tbl$FB_SK_CH_F0012[tbl$SEX=="male"& tbl$WGHT_GRPS=="overweight.and.obese"], na.rm = T)
mean(tbl$FB_SK_CH_F0012[tbl$SEX=="male"& tbl$WGHT_GRPS=="normalweight"], na.rm = T)
sd(tbl$FB_SK_CH_F0012[tbl$SEX=="male"& tbl$WGHT_GRPS=="normalweight"], na.rm = T)
sd(tbl$FB_SK_CH_F0012[tbl$SEX=="male"& tbl$WGHT_GRPS=="overweight.and.obese"], na.rm = T)
median(tbl$FB_SK_CH_F0012[tbl$SEX=="male"& tbl$WGHT_GRPS=="normalweight"], na.rm = T)
median(tbl$FB_SK_CH_F0012[tbl$SEX=="male"& tbl$WGHT_GRPS=="overweight.and.obese"], na.rm = T)

addmargins(table(tbl$FB_SK_CH_F0012, tbl$WGHT_GRPS))


##frequencies of mutation age
ggplot( tbl ) +
  geom_histogram( aes(FB_SK_CH_F0012, fill=SEX), stat =  "count" ) + 
  theme_bw( ) + 
  ylab( "frequency" ) +
  scale_fill_manual(values="blue", guide=F)+
  scale_x_continuous( "mutation age [y]", breaks=c(9:16), labels=c(9:16) )+ ##--> wie besser mit alterseineteilung?
  annotate(geom="text",x=15, y=50, label="n=134" )


##Wachstumsgeschwindigkeit
##kleinere Age groups erstellen
months<- 12
tbl$AGE.CAT.height <-
    cut(
        tbl$AGE,
        breaks= c(4.5, seq(5,18,by=months/12)),##halbjeahresschritte f?r gr??e
        labels = seq(5,18,by=months/12)-months/24) 

    ##mean height fuer age und sex
mean.height.age <-
    tbl %>%
    group_by(
        AGE.CAT.height, 
        SEX) %>% ##tbl gruppieren und mean angucken: neue tbl, mit mean height fpr jedes Alter und sex
    summarise(
        height.mean=mean( C_ANTHRO_KH_HEIGHT_ORIG ) )

mean.height.age.m <-mean.height.age[ mean.height.age$SEX == "male", ]
mean.height.age.f <-mean.height.age[ mean.height.age$SEX == "female", ]

mean.height.age.m$growth.vel <-
    (( mean.height.age.m$height.mean)-lag( mean.height.age.m$height.mean))/  
    (as.numeric( as.character(mean.height.age.m$AGE.CAT.height))-lag( as.numeric(as.character(mean.height.age.m$AGE.CAT.height)) ) )

##velocity pro jahr f?r halbes jahr
mean.height.age.m

#Wachstumsgeschwindigkeit: größendiff. / Zeit
mean.height.age.f$growth.vel <-
    (( mean.height.age.f$height.mean)-lag( mean.height.age.f$height.mean))/  
    (as.numeric( as.character(mean.height.age.f$AGE.CAT.height))-lag( as.numeric(as.character(mean.height.age.f$AGE.CAT.height)) ) )

as.numeric(as.character(mean.height.age.f$AGE.CAT.height))-lag( as.numeric(as.character(mean.height.age.f$AGE.CAT.height)))  

##Peak height velocity: 1 (growth.rate= größendiff./Größe), dann peak davon

mean.height.age.m$growth.rate <-
    ( ( mean.height.age.m$height.mean)-lag( mean.height.age.m$height.mean) ) / mean.height.age.m$height.mean


mean.height.age.m

mean.height.age.f$growth.rate <-
    ( ( mean.height.age.f$height.mean )-lag( mean.height.age.f$height.mean ) ) / mean.height.age.f$height.mean;

##wieder einen gemeinsamen dataframe erstellen
mean.height.age <- 
    rbind.data.frame(mean.height.age.m,mean.height.age.f )

##wachstumsgeschwindigkeit
ggsubplot(
    ggplot(mean.height.age)+
        geom_line(aes(AGE.CAT.height, growth.vel, col=SEX, group=SEX))+
        facet_grid(.~SEX)+
        theme_bw()+
        #xlab("")+
        ylab("growth vel [cm/y]")+
        geom_hline(yintercept = 0, linetype=2)+
        scale_colour_manual(values=c("deeppink", "deepskyblue"))+
        theme(axis.text.x = element_blank(),axis.title.x = element_blank())+
        theme(axis.text.x = element_text(angle=90)),
    
    ggplot(mean.height.age)+
        geom_line(aes(AGE.CAT.height, height.mean, col=SEX, group=SEX))+
        facet_grid(.~SEX)+
        theme_bw()+
        xlab("age [y]")+
        ylab("height[cm]")+
        scale_colour_manual(values=c("deeppink", "deepskyblue"))+
        theme(axis.text.x = element_text(angle=90)),
    cols = 1) ##bilder in 1 spalte

ggsubplot(
    ggplot(mean.height.age)+
        geom_line(aes(AGE.CAT.height, growth.rate, col=SEX, group=SEX))+
        facet_grid(.~SEX)+
        theme_bw()+
        #xlab("")+
        ylab("growth rate [cm/y]")+
        geom_hline(yintercept = 0, linetype=2)+
        scale_colour_manual(values=c("deeppink", "deepskyblue"))+
        theme(axis.text.x = element_blank(),axis.title.x = element_blank())+
        theme(axis.text.x = element_text(angle=90)),
    ggplot(mean.height.age)+
        geom_line(aes(AGE.CAT.height, growth.vel, col=SEX, group=SEX))+
        facet_grid(.~SEX)+
        theme_bw()+
        #xlab("")+
        ylab("growth vel [cm/y]")+
        geom_hline(yintercept = 0, linetype=2)+
        scale_colour_manual(values=c("deeppink", "deepskyblue"))+
        theme(axis.text.x = element_blank(),axis.title.x = element_blank())+
        theme(axis.text.x = element_text(angle=90)),
    
    ##höhe übers alter
    ggplot(mean.height.age)+
        geom_line(aes(AGE.CAT.height, height.mean, col=SEX, group=SEX))+
        facet_grid(.~SEX)+
        theme_bw()+
        xlab("age [y]")+
        ylab("height[cm]")+
        scale_colour_manual(values=c("deeppink", "deepskyblue"))+
        theme(axis.text.x = element_text(angle=90)),
    cols = 1) ##bilder in 1 spalte


##PHV berechnen
mean.height.age.f$AGE.CAT.height[which.max(mean.height.age.f$growth.rate)]  #PHV für female=11,5
mean.height.age.m$AGE.CAT.height[which.max(mean.height.age.m$growth.rate)]  #PHV für male= 13,5
mean.height.age

##jetzt mit gleitendem Mean, alterskat. evtl. verändern, vielleicht geom_smooth plotten











##PHV nach weight groups, SES


##mean height fuer age und sex und weight group
mean.height.age.wg <-
    tbl %>%
    group_by(
        AGE.CAT.height, 
        SEX,
        WGHT_GRPS) %>% ##tbl gruppieren und mean angucken: neue tbl, mit mean height fpr jedes Alter und sex
    summarise(
        height.mean=mean( C_ANTHRO_KH_HEIGHT_ORIG ) )

##datensatz in 4 Gruppen unterteilen
mean.height.age.m.nw <-mean.height.age.wg[ mean.height.age.wg$SEX == "male"& mean.height.age.wg$WGHT_GRPS== "normalweight", ]
mean.height.age.f.nw <-mean.height.age.wg[ mean.height.age.wg$SEX == "female"&mean.height.age.wg$WGHT_GRPS== "normalweight", ]
mean.height.age.m.ow <-mean.height.age.wg[ mean.height.age.wg$SEX == "male" & mean.height.age.wg$WGHT_GRPS== "overweight.and.obese", ]
mean.height.age.f.ow <-mean.height.age.wg[ mean.height.age.wg$SEX == "female"& mean.height.age.wg$WGHT_GRPS== "overweight.and.obese", ]

##wachstumsgeschw. berechnen
mean.height.age.m.nw$growth.vel <-
    (( mean.height.age.m.nw$height.mean)-lag( mean.height.age.m.nw$height.mean))/  
    (as.numeric( as.character(mean.height.age.m.nw$AGE.CAT.height))-lag( as.numeric(as.character(mean.height.age.m.nw$AGE.CAT.height)) ) )
##...jetzt für alle 4
##erstmal Verfahren für PHV verfeinern...




# ##velocity pro jahr f?r halbes jahr
# mean.height.age.m
# 
# #Wachstumsgeschwindigkeit: größendiff. / Zeit
# mean.height.age.f$growth.vel <-
#     (( mean.height.age.f$height.mean)-lag( mean.height.age.f$height.mean))/  
#     (as.numeric( as.character(mean.height.age.f$AGE.CAT.height))-lag( as.numeric(as.character(mean.height.age.f$AGE.CAT.height)) ) )
# 
# as.numeric(as.character(mean.height.age.f$AGE.CAT.height))-lag( as.numeric(as.character(mean.height.age.f$AGE.CAT.height)))  
# 
# ##Peak height velocity: 1 (growth.rate= größendiff./Größe), dann peak davon
# 
# mean.height.age.m$growth.rate <-
#     ( ( mean.height.age.m$height.mean)-lag( mean.height.age.m$height.mean) ) / mean.height.age.m$height.mean
# 
# 
# mean.height.age.m
# 
# mean.height.age.f$growth.rate <-
#     ( ( mean.height.age.f$height.mean )-lag( mean.height.age.f$height.mean ) ) / mean.height.age.f$height.mean;
# 
# ##wieder einen gemeinsamen dataframe erstellen
# mean.height.age <- 
#     rbind.data.frame(mean.height.age.m,mean.height.age.f )
# 
# ##wachstumsgeschwindigkeit
# ggsubplot(
#     ggplot(mean.height.age)+
#         geom_line(aes(AGE.CAT.height, growth.vel, col=SEX, group=SEX))+
#         facet_grid(.~SEX)+
#         theme_bw()+
#         #xlab("")+
#         ylab("growth vel [cm/y]")+
#         geom_hline(yintercept = 0, linetype=2)+
#         scale_colour_manual(values=c("deeppink", "deepskyblue"))+
#         theme(axis.text.x = element_blank(),axis.title.x = element_blank())+
#         theme(axis.text.x = element_text(angle=90)),
#     
#     ggplot(mean.height.age)+
#         geom_line(aes(AGE.CAT.height, height.mean, col=SEX, group=SEX))+
#         facet_grid(.~SEX)+
#         theme_bw()+
#         xlab("age [y]")+
#         ylab("height[cm]")+
#         scale_colour_manual(values=c("deeppink", "deepskyblue"))+
#         theme(axis.text.x = element_text(angle=90)),
#     cols = 1) ##bilder in 1 spalte
# 
# ggsubplot(
#     ggplot(mean.height.age)+
#         geom_line(aes(AGE.CAT.height, growth.rate, col=SEX, group=SEX))+
#         facet_grid(.~SEX)+
#         theme_bw()+
#         #xlab("")+
#         ylab("growth rate [cm/y]")+
#         geom_hline(yintercept = 0, linetype=2)+
#         scale_colour_manual(values=c("deeppink", "deepskyblue"))+
#         theme(axis.text.x = element_blank(),axis.title.x = element_blank())+
#         theme(axis.text.x = element_text(angle=90)),
#     ggplot(mean.height.age)+
#         geom_line(aes(AGE.CAT.height, growth.vel, col=SEX, group=SEX))+
#         facet_grid(.~SEX)+
#         theme_bw()+
#         #xlab("")+
#         ylab("growth vel [cm/y]")+
#         geom_hline(yintercept = 0, linetype=2)+
#         scale_colour_manual(values=c("deeppink", "deepskyblue"))+
#         theme(axis.text.x = element_blank(),axis.title.x = element_blank())+
#         theme(axis.text.x = element_text(angle=90)),
#     
#     ##höhe übers alter
#     ggplot(mean.height.age)+
#         geom_line(aes(AGE.CAT.height, height.mean, col=SEX, group=SEX))+
#         facet_grid(.~SEX)+
#         theme_bw()+
#         xlab("age [y]")+
#         ylab("height[cm]")+
#         scale_colour_manual(values=c("deeppink", "deepskyblue"))+
#         theme(axis.text.x = element_text(angle=90)),
#     cols = 1) ##bilder in 1 spalte
# 
# 
# ##PHV berechnen
# mean.height.age.f$AGE.CAT.height[which.max(mean.height.age.f$growth.rate)]  #PHV für female=11,5
# mean.height.age.m$AGE.CAT.height[which.max(mean.height.age.m$growth.rate)]  #PHV für male= 13,5
# mean.height.age

##jetzt mit gleitendem Mean, alterskat. evtl. verändern, vielleicht geom_smooth plotten

##PHV nach weight groups, SES

