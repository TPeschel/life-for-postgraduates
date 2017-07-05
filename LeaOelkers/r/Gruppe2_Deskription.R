##Deskriptive Statistik Gruppe 2, n=1229.

rm( list = ls( ) ) ##Speicher lÃ¶schenren
library(ggplot2)
library(reshape2) #u.a. melt funktion
library(readxl) ##standart excel tbl lesen
library( dplyr ) ##group by befehl, summarise,...
library( openxlsx ) ##lesen und schreiben
#library(xlsx) ##lesen und schreiben
library(WriteXLS) ##standart zum schreiben
library(psych) ##für describeBY
my.theme <-  # bevorzugtes Schema fuer Grafiken
  theme_bw( )

# t4$Kohorte <- NULL ##spalte wieder löschen wenn ich sie nicht mehr brauche

setwd("c:/Users/Lea/Desktop/DA/LIFE-Daten/R-Projekte/r20170601/") ##Pfad setzen
t4 <- read_excel("C:/Users/Lea/Desktop/DA/LIFE-Daten/R-Projekte/r20170601/data/Gruppe2/Gruppe2_fertigerDatensatz.xlsx")

t4 <- t4[ !is.na( t4$C_ANTHRO_KH_BMI_ADJ ), ]
t4 <- t4[ 4 < t4$AGE, ] ##den 3 jährigen ausschließen

##spalten wieder zufügen zu größerer t4 (235 Spalten): 
load("data/tabelleGesamtJoinAlleSpalten.Rd")
nms <- intersect(names(t4),names(gesamt.Join.alle.spalten))
t4 <- merge( t4, gesamt.Join.alle.spalten, by=nms,all.x = TRUE, all.y=F)
# t4 <- t4[ ,c( "alle spalten die ich will") ]  wenn ich wieder nur bestimmet spalten betrachten will

nrow(t4) ##1195

# t4$AGE.CATEGORIE <-
#   cut(
#     x      = t4$AGE,
#     breaks = c(3 : 18 ),
#     labels = c(4 : 18 ) )


##Age by gender
table(t4$SEX)
addmargins(table(t4$SEX, t4$AGE.CATEGORIE))

##Mittelwert etc des ALters
describeBy(t4$AGE, t4$SEX)
summary(t4$AGE[t4$SEX=="male"])  ##-->mean= 10,38 ##zeigt uns mean, median, min, max etc an von den male an
summary(t4$AGE[t4$SEX=="female"]) ##--> 10,783= mean female

##Histogramm Atersverteilung:

# table(t4$SEX)
# male <- subset(t4, t4$SEX == "male")
# Age_Boys <-  male$AGE
# hist(Age_Boys, xlim = c(3, 20), ylim = c(0, 80), col = "blue", breaks = 20, main = "boys") ##Überschrift andern? in girls boys??
#         #siehe Körner: wie kann ich es nach altersgurppen (jedes Jahr einzeln) anzeigen lassen?
# 
# table(t4$SEX)
# female <- subset(t4, t4$SEX == "female")
# Age_Girls <-  female$AGE
# hist(Age_Girls, xlim = c(3, 20), ylim = c(0, 80), col = "blue", breaks = 20, main = "girls") ##Überschrift andern? in girls boys??

t4$AGE.CATEGORIE <-   ##alterskategorie in 1 yr breaks
  cut(
    x      = t4$AGE,
    breaks = seq( 2, 18, by = 1 ),
    labels = seq( 3, 18, by = 1 ) )


ggplot( t4 ) +
  geom_histogram( aes( AGE.CATEGORIE, fill = SEX ), stat =  "count" ) + 
  facet_grid( SEX ~ . ) +
  theme_bw( ) + 
  ylab( "frequency" ) +
  scale_fill_manual( "sex", values = c( "deeppink", "blue" ) ) +
  scale_x_discrete( "age [y]" )

# t4$AGE.CATEGORIE <-   ##alter in 2yr breaks
#   cut(
#     x      = t4$AGE,
#     breaks = seq( 2, 18, by = 2 ),
#     labels = seq( 3, 17, by = 2 ) )

##Boxplot age by gender
# data frame schaffen mit Alter-Mean/MAX/MIN nach SEX (für die nächsten Befehle wichtig)
df.mean.age <-
  data.frame(
    SEX=c("male","female"),
    MEAN = c( mean( t4$AGE[t4$SEX=="male"]),mean( t4$AGE[t4$SEX=="female"])),
    MIN = c( min( t4$AGE[t4$SEX=="male"]),min( t4$AGE[t4$SEX=="female"])),
    MAX = c( max( t4$AGE[t4$SEX=="male"]),max( t4$AGE[t4$SEX=="female"]))
  )
df.mean.age

##zunächst nicht nach PG getrennt: Boxplot age by gender, weiß mit grauen Plots
# ggplot(t4)+
#   geom_boxplot(aes(x=SEX, y=AGE), fill="gray")+  ##boxplot, grau färben
#   geom_errorbar(data=df.mean.age,aes(x=SEX, ymin=MIN,ymax=MAX),size=.3,width=.5)+ ##size=liniendicke, witth=breite der min/MAx werte (=staples)
#   geom_point(aes(x=SEX,y=AGE), position="jitter",alpha=.6)+ ##punktverteilung zusätzlich darstellen, alpha=tranparenz, position=jitter: punktverteilung weit um Linie herum
#   geom_point(data = df.mean.age, aes(x=SEX,y=MEAN), col="red")+  ##mean als Punkt draufsetzen
#   theme_bw()  ##in schwarz-weiß

#das gleiche ohne jitter: age by gender --> am besten
ggplot(t4)+
  geom_boxplot(aes(x=SEX, y=AGE), fill="gray")+ #
  geom_errorbar(data=df.mean.age,aes(x=SEX, ymin=MIN,ymax=MAX),size=.3,width=.5)+ ##size=liniendicke, witth=breite der min/MAx werte
  geom_jitter(aes(x=SEX,y=AGE),alpha=.1,width = .01)+ ##punktverteilung zusätzlich darstellen, alpha=tranparenz
  geom_point(data = df.mean.age, aes(x=SEX,y=MEAN), col="red")+
  ylab("age")+
  xlab("sex")+
  theme_bw()
  
  # title(main = list("age by gender", cex=1.5, col="blue", font=2)
        ##wie kann man die Schriftgröße der Achsenbezeichnungen(ylab) ändern? Titel einfügen?

#das Gleiche mit weiß+weißen Plots
# ggplot(t4)+
#   geom_errorbar(data=df.mean.age,aes(x=SEX, ymin=MIN,ymax=MAX),size=.3,width=.5)+ ##size=liniendicke, witth=breite der min/MAx werte
#   geom_point(aes(x=SEX,y=AGE),alpha=.5)+ ##punktverteilung zusätzlich darstellen, alpha=tranparenz
#   geom_boxplot(aes(x=SEX, y=AGE),alpha=.5)+ ##
#   geom_point(data = df.mean.age, aes(x=SEX,y=MEAN), col="red")+
#   theme_bw() ##hintergrund in SW
# 
# # da Gleiche alles in grau
# ggplot(t4)+
#   geom_errorbar(aes(x=SEX, ymin=min(AGE),ymax=max(AGE)),size=.3,width=.5)+ ##size=liniendicke, witth=breite der whisker
#   geom_point(aes(x=SEX,y=AGE),alpha=.1)+ ##punktverteilung zusätzlich darstellen, alpha=tranparenz
#   geom_boxplot(aes(x=SEX, y=AGE),alpha=.5)+ ##
#   geom_point(data = df.mean.age, aes(x=SEX,y=MEAN), col="red")

# mean etc. anzeigen lassen
df.mean.age
summary(t4$AGE[t4$SEX=="male"])  ##-->mean= 10,38 ##zeigt uns mean, median, min, max etc an von den male an
summary(t4$AGE[t4$SEX=="female"]) ##--> 10,783= mean female



##Puberty Groups by gender
table(t4$SEX, t4$C_PUB_STAT_PUB_STATUS)
describe.by(t4$C_PUB_STAT_PUB_STATUS, t4$SEX)

##frequency of PG
ggplot(t4) +
  geom_histogram(aes(C_PUB_STAT_PUB_STATUS, fill = SEX  ), stat = "count") +
  facet_grid(. ~ SEX)+
  theme_bw() +
  xlab("PG") +
  ylab("frequency") +
  scale_fill_manual( "sex", values = c( "deeppink", "blue" ) ) 
# ggsave( filename = "AnzahlProbanden~PubStat.pdf" )
  
  #oder in sw
ggplot(t4) +
  my.theme +
  # scale_fill_manual(name = "Geschlecht", values = c("red", "blue")) +
  xlab("PubStat") +
  ylab("Anzahl der Probanden") +
  geom_histogram(aes(x = as.factor( C_PUB_STAT_PUB_STATUS ) ), stat = "count", position = "stack") +
  facet_grid(. ~ SEX)+
  scale_fill_manual( "sex", values = c( "deeppink", "blue" ) ) 
  ##übereinander: position="stack", nebeneinander wäre: "dodge", ##stat= count: zählt anzhal pro pubstat

# ggsave( filename = "AnzahlProbanden~PubStat.pdf" )

##ohne sex aufteilung
ggplot(t4) +
  my.theme +
  xlab("PubStat") +
  ylab("Anzahl der Probanden") +
  geom_histogram(aes(x = as.factor( C_PUB_STAT_PUB_STATUS ) ), stat = "count", position = "stack")
  
# ggsave( filename = "AnzahlBesuche~PubStatOhneSEX.pdf" )



##Puberty groups by age&gender ??
table(t4$C_PUB_STAT_PUB_STATUS, t4$AGE.CATEGORIE, t4$SEX=="female") 
          ##wie stelle ich es besser dar? auch mit margins? tables checken!
 ##wie mache ich despkriptive statistik (S.9)?
 
#Boxplots
 # df.mean.age neu definieren:mean/min/max nach sex und PG
df.mean.age <-
  data.frame(
    SEX= c(rep("male",5),rep("female",5)),
    MEAN = c( 
      mean( t4$AGE[t4$SEX=="male" & t4$C_PUB_STAT_PUB_STATUS==1] ),  #1. mean vom age, von male in PG1
      mean( t4$AGE[t4$SEX=="male" & t4$C_PUB_STAT_PUB_STATUS==2] ),  # mean vom age, von male in PG2  usw.
      mean( t4$AGE[t4$SEX=="male" & t4$C_PUB_STAT_PUB_STATUS==3] ),
      mean( t4$AGE[t4$SEX=="male" & t4$C_PUB_STAT_PUB_STATUS==4] ),
      mean( t4$AGE[t4$SEX=="male" & t4$C_PUB_STAT_PUB_STATUS==5] ),
      mean( t4$AGE[t4$SEX=="female" & t4$C_PUB_STAT_PUB_STATUS==1] ),
      mean( t4$AGE[t4$SEX=="female" & t4$C_PUB_STAT_PUB_STATUS==2] ),
      mean( t4$AGE[t4$SEX=="female" & t4$C_PUB_STAT_PUB_STATUS==3] ),
      mean( t4$AGE[t4$SEX=="female" & t4$C_PUB_STAT_PUB_STATUS==4] ),
      mean( t4$AGE[t4$SEX=="female" & t4$C_PUB_STAT_PUB_STATUS==5] ) ),
    MIN = c( 
      min( t4$AGE[t4$SEX=="male" & t4$C_PUB_STAT_PUB_STATUS==1] ),
      min( t4$AGE[t4$SEX=="male" & t4$C_PUB_STAT_PUB_STATUS==2] ),
      min( t4$AGE[t4$SEX=="male" & t4$C_PUB_STAT_PUB_STATUS==3] ),
      min( t4$AGE[t4$SEX=="male" & t4$C_PUB_STAT_PUB_STATUS==4] ),
      min( t4$AGE[t4$SEX=="male" & t4$C_PUB_STAT_PUB_STATUS==5] ),
      min( t4$AGE[t4$SEX=="female" & t4$C_PUB_STAT_PUB_STATUS==1] ),
      min( t4$AGE[t4$SEX=="female" & t4$C_PUB_STAT_PUB_STATUS==2] ),
      min( t4$AGE[t4$SEX=="female" & t4$C_PUB_STAT_PUB_STATUS==3] ),
      min( t4$AGE[t4$SEX=="female" & t4$C_PUB_STAT_PUB_STATUS==4] ),
      min( t4$AGE[t4$SEX=="female" & t4$C_PUB_STAT_PUB_STATUS==5] ) ),
    MAX = c( 
      max( t4$AGE[t4$SEX=="male" & t4$C_PUB_STAT_PUB_STATUS==1] ),
      max( t4$AGE[t4$SEX=="male" & t4$C_PUB_STAT_PUB_STATUS==2] ),
      max( t4$AGE[t4$SEX=="male" & t4$C_PUB_STAT_PUB_STATUS==3] ),
      max( t4$AGE[t4$SEX=="male" & t4$C_PUB_STAT_PUB_STATUS==4] ),
      max( t4$AGE[t4$SEX=="male" & t4$C_PUB_STAT_PUB_STATUS==5] ),
      max( t4$AGE[t4$SEX=="female" & t4$C_PUB_STAT_PUB_STATUS==1] ),
      max( t4$AGE[t4$SEX=="female" & t4$C_PUB_STAT_PUB_STATUS==2] ),
      max( t4$AGE[t4$SEX=="female" & t4$C_PUB_STAT_PUB_STATUS==3] ),
      max( t4$AGE[t4$SEX=="female" & t4$C_PUB_STAT_PUB_STATUS==4] ),
      max( t4$AGE[t4$SEX=="female" & t4$C_PUB_STAT_PUB_STATUS==5] ) ),
    PG = c( c(1:5),c(1:5))  #für male/female
  )
df.mean.age

##t5: neue tbl mit neuen Spalten (mean/min/Max vom alter, nach SEX und PG) 
t5 <-
  merge(
    t4,
    df.mean.age,
    by.x = c( "SEX", "C_PUB_STAT_PUB_STATUS" ),
    by.y = c( "SEX", "PG" )
  )

# Boxplot: age by PG and gender  -->mit jitter
ggplot(t5)+
  geom_boxplot(aes(x=SEX, y=AGE), fill="gray")+ #
  geom_errorbar(aes(x=SEX, ymin=MIN,ymax=MAX),size=.3,width=.5)+ ##size=liniendicke, width=breite der min/MAx werte
  geom_point(aes(x=SEX,y=AGE), position="jitter",alpha=.9)+ ##punktverteilung zusätzlich darstellen, alpha=tranparenz
  geom_point(aes(x=SEX,y=MEAN), col="red")+
  theme_bw() +
  facet_grid(~C_PUB_STAT_PUB_STATUS)


# --> ohne jitter! (besser finde ich)
ggplot(t5)+
  geom_boxplot(aes(x=SEX, y=AGE), fill="gray")+ #
  geom_errorbar(aes(x=SEX, ymin=MIN,ymax=MAX),size=.3,width=.5)+ ##size=liniendicke, witth=breite der min/MAx werte
  geom_jitter(aes(x=SEX,y=AGE), width = .02,alpha=.9)+ ##punktverteilung zusätzlich darstellen, alpha=tranparenz
  geom_point(aes(x=SEX,y=MEAN), col="red")+
  theme_bw() +
  facet_grid(~C_PUB_STAT_PUB_STATUS)



##Menarchealter
table(t4$C_PUB_STAT_MENARCHE_WANN, t4$SEX)
describe.by(t4$C_PUB_STAT_MENARCHE_WANN) 

female <- subset(t4, t4$SEX == "female")    ##darstellung: frequency direkt über Alter?Titel? Table runterschreiben?
menarchal_age <-  female$C_PUB_STAT_MENARCHE_WANN
hist(menarchal_age, xlim = c(9, 16), ylim = c(0, 70), col = "blue", breaks = 10, main = "menarchal age")+
 # xlab("menarchal age")+
 #  ylab("frequency")  ##achsenbeschriftung ändern??
 #oder vielleicht mit:  scale_x_discrete(name = "female") +
 #  scale_y_continuous(name = "menarchal age") 
  
##boxplot menarchal age 

# ggplot(t4)+
#   geom_boxplot(aes(x=female, y=C_PUB_STAT_MENARCHE_WANN), fill="gray")+ 
#   geom_errorbar(data=t4,aes(x=female, y=C_PUB_STAT_MENARCHE_WANN),size=.3,width=.5)+
#   # geom_errorbar(data=df.mean.age,aes(x=SEX, ymin=MIN,ymax=MAX),size=.3,width=.5)+ ##size=liniendicke, witth=breite der min/MAx werte
#   geom_jitter(aes(x=female,y=C_PUB_STAT_MENARCHE_WANN),alpha=.1,width = .01)+ ##punktverteilung zusätzlich darstellen, alpha=tranparenz
#   # geom_point(data = t4, aes(x=SEX,y=MEAN), col="red")+
#   theme_bw()

## Plot: mittleres Menarchealter --> eher nicht verwenden
# female <- subset(t4, t4$SEX == "female")                   ##wie nur female, wie median dicker?mean einzeichnen?
# p1<- ggplot(t4, aes(x = SEX, y = C_PUB_STAT_MENARCHE_WANN)) +
#   geom_boxplot()
# p1 ##wie mache ich es nur für female?
# p1 <- p1 + scale_x_discrete(name = "female") +
#   scale_y_continuous(name = "menarchal age")   ##ändert die Achsenbezeichnungen
# p1 <- p1 + ggtitle("menarchal age")    ##titel hinzufügen
# p1



##BMI by age & gender
describeBy(t4$C_ANTHRO_KH_BMI_ORIG, t4$SEX)
summary(t4$C_ANTHRO_KH_BMI_ORIG[t4$SEX=="male"])
summary(t4$C_ANTHRO_KH_BMI_ORIG[t4$SEX=="female"]) ##wie kann ich hier noch "all"/sum berechnen (körner s. 10)

describeBy(t4$C_ANTHRO_KH_BMI_ADJ, t4$SEX)
summary(t4$C_ANTHRO_KH_BMI_ADJ[t4$SEX=="male"])
summary(t4$C_ANTHRO_KH_BMI_ADJ[t4$SEX=="female"])

t4$weight.cut <-
  cut( 
    x = t4$C_ANTHRO_KH_BMI_ADJ, 
    breaks =  c( -Inf, -1.28,  1.28, 1.88, +Inf ),
    labels = c( "underweight", "normalweight", "overweight", "obese" ) )

t4$weight.cut_BMI <-
 cut( 
   x = t4$C_ANTHRO_KH_BMI_ORIG, 
   breaks =  c( -Inf, 18.5,  25, 30, +Inf ),   ##stimmt das so? Es werden viel zu viele underweights angezeigt!!!
   labels = c( "underweight", "normalweight", "overweight", "obese" ) )

ggplot(t4)+geom_jitter( aes( t4$AGE, C_ANTHRO_KH_BMI_ADJ, 
   col = weight.cut ))+scale_x_continuous(name = "Age", seq( 3,17,by = 2))+
  ylab("BMI (SDS)")

ggplot(t4)+geom_jitter( aes( t4$AGE, C_ANTHRO_KH_BMI_ORIG, 
    col = weight.cut_BMI ))+scale_x_continuous(name = "age", seq( 3,17,by = 2))+
  ylab("BMI")

  # und nach sex
ggplot(t4)+geom_jitter( aes( t4$AGE, C_ANTHRO_KH_BMI_ORIG, 
   col = weight.cut_BMI ))+scale_x_continuous(name = "age", seq( 3,17,by = 2))+
  ylab("BMI")+
  facet_grid(. ~ SEX)
  # legend("topright", inset=c(-0.2,0), legend=c("underweight", "normalweight", "overweight", "obese"), pch=c(1,3), title="Group")
  ##wie kann ich den Legenden-Titel ändern?
#ggsave("Test.png",width = 10, height = 8)
#ggsave("Test.pdf") ##besser ist pdf

addmargins(table(t4$weight.cut,t4$AGE.CATEGORIE))
addmargins(table(t4$weight.cut_BMI,t4$AGE.CATEGORIE))

##2 groups:underweight and normalweight zusammen, overweight und obese
 
 #Für BMI_SDS
t4$weight.cut.2 <-
  c( "normal.and.underweight", "normal.and.underweight", "overweight.and.obese", "overweight.and.obese" )[ match( t4$weight.cut, c( "underweight", "normalweight", "overweight", "obese" ) ) ]
  
#für BMI --> macht keinen Sinn bei Kindern!!
t4$weight.cut.2BMI <-
  c( "normal.and.underweight", "normal.and.underweight", "overweight.and.obese", "overweight.and.obese" )[ match( t4$weight.cut_BMI, c( "underweight", "normalweight", "overweight", "obese" ) ) ]
# View(t4[,c("C_ANTHRO_KH_BMI_ORIG", "weight.cat_BMI", "weight.cat.2BMI")])
addmargins(table(t4$weight.cut.2,t4$AGE.CATEGORIE))
addmargins(table(t4$weight.cut.2BMI,t4$AGE.CATEGORIE)) ## tabellen stimmen nciht überein...??

ggplot(t4) +
  geom_histogram(aes(t4$weight.cut, fill = SEX  ), stat = "count", position = "dodge") +
  # facet_grid(. ~ SEX)+
  theme_bw() +
  xlab("BMI (SDS)") +
  ylab("frequency") +
  scale_fill_manual( "sex", values = c( "deeppink", "blue" ) ) 

ggplot(t4) +
  geom_histogram(aes(t4$weight.cut_BMI, fill = SEX  ), stat = "count", position = "dodge") +
  # facet_grid(. ~ SEX)+
  theme_bw() +
  xlab("BMI") +
  ylab("frequency") +
  scale_fill_manual( "sex", values = c( "deeppink", "blue" ) ) 

ggplot(t4) +
  geom_histogram(aes(t4$weight.cut.2, fill = SEX  ), stat = "count", position = "dodge") +
  # facet_grid(. ~ SEX)+
  theme_bw() +
  xlab("BMI (SDS)") +
  ylab("frequency") +
  scale_fill_manual( "sex", values = c( "deeppink", "blue" ) ) 

ggplot(t4) +
  geom_histogram(aes(t4$weight.cut.2BMI, fill = SEX  ), stat = "count", position = "dodge") +
  # facet_grid(. ~ SEX)+
  theme_bw() +
  xlab("BMI (SDS)") +
  ylab("frequency") +
  scale_fill_manual( "sex", values = c( "deeppink", "blue" ) ) 

##Boxplot mit mittleren BMIs der gruppen darstellen?


##Height by age&gender
describeBy(t4$C_ANTHRO_KH_HEIGHT_ORIG, t4$SEX) ##auch Height sds wichtig??
summary(t4$C_ANTHRO_KH_BMI_ORIG[t4$SEX=="male"])
summary(t4$C_ANTHRO_KH_BMI_ORIG[t4$SEX=="female"]) ##wie kann ich hier noch "all"/sum berechnen (körner s. 10)

ggplot(t4) +
  geom_point(aes(x = AGE, y = C_ANTHRO_KH_HEIGHT_ORIG, col=SEX))+ 
  scale_x_continuous(name = "age", seq( 3,17,by = 2))+
  xlab("age") +
  ylab("height (cm)")+    
  facet_grid(.~SEX)

##Weight by age&gender
ggplot(t4) +
  geom_point(aes(x = AGE, y = C_ANTHRO_KH_WEIGHT_ORIG, col=SEX))+ 
  scale_x_continuous(name = "age", seq( 3,17,by = 2))+
  scale_fill_manual( "sex", values = c( "deeppink", "blue" ) ) +
  xlab("age") +
  ylab("weight (kg)")+    
  facet_grid(.~SEX)  ##farben ändern??
 



##Winkler
t4$Winkler.cut <-
  cut( 
    x = t4$D00177_SCORE_FAM, 
    breaks =  c( 3, 8, 14,21 ),
    labels = c( "low", "intermediate", "high" ) ) 

ggplot(t4) +
  geom_histogram(aes(t4$Winkler.cut, fill = SEX  ), stat = "count", position = "dodge") +
  # facet_grid(. ~ SEX)+
  theme_bw() +
  xlab("socioeconomic status") +
  ylab("frequency") +
  scale_fill_manual( "sex", values = c( "deeppink", "blue" ) ) 





## LH 
describeBy(t4$LH_S_NUM_VALUE, t4$SEX) ##auch Height sds wichtig??
summary(t4$LH_S_NUM_VALUE[t4$SEX=="male"])
summary(t4$LH_S_NUM_VALUE[t4$SEX=="female"]) ##wie kann ich hier noch "all"/sum berechnen (körner s. 10)


## FSH
describeBy(t4$FSH_S_NUM_VALUE, t4$SEX) ##auch Height sds wichtig??
summary(t4$FSH_S_NUM_VALUE[t4$SEX=="male"])
summary(t4$FSH_S_NUM_VALUE[t4$SEX=="female"])

 #Plot: LH~age nach SEX
ggplot(t4)+
  geom_point( aes( x=AGE, y=LH_S_NUM_VALUE, col=SEX))+
  scale_x_continuous(name = "age", seq( 3,17,by = 2))+
  ylab("LH(IU/L")+
  facet_grid(.~SEX)+
  scale_fill_manual( "sex", values = c( "deeppink", "blue" ) ) 
  

#Plot: FSH~age nach SEX
ggplot(t4) +
  geom_point(aes(x = AGE, y = FSH_S_NUM_VALUE, col=SEX))+  
  scale_x_continuous(name = "age", seq( 3,17,by = 2))+
  ylab("FSH(IU/L)")+
  xlab("age")+
  facet_grid(.~SEX)

#Plot mean- LH in beiden BMI-Groups
ggplot(t4)+
  geom_boxplot(aes(x=SEX, y=LH_S_NUM_VALUE), fill="gray")+ 
  # geom_errorbar(aes(x=SEX, ymin=LH_S_NUM_VALUE, ),size=.3,width=.5)+ ##size=liniendicke, witth=breite der min/MAx werte
  # geom_jitter(aes(x=SEX,y=LH_S_NUM_VALUE), width = .02,alpha=.01)+ ##punktverteilung zusätzlich darstellen, alpha=tranparenz
  theme_bw() +
  xlab("LH (IU/L)")+
  ylab("sex")+
  facet_grid(~t4$weight.cut.2) ##wie besser darstellen? Körner S. 13.



# ##LH ~ Alter, nach Sex und PubStat
# ggplot(t4) +
#   geom_point(aes(x = AGE, y = LH_S_NUM_VALUE, col=SEX))+
#   facet_grid(C_PUB_STAT_PUB_STATUS~SEX )
# 
# # LH~Alter nach SEX und PubStat mit limitierterer Y-Achse:
# ggplot(t4) +
#   geom_point(aes(x = AGE, y = LH_S_NUM_VALUE, col=SEX))+
#   facet_grid(C_PUB_STAT_PUB_STATUS~SEX ) +
#   ylim( c(0, 10))
# # ggsave( filename = "LH~Alter_nachSEXundPubstat.pdf" )
# 
# ##im selben Plot wie vorher die Adipösen-Falgs markieren
# t4$C_DISEASE_TX_ADIP[ is.na( t4$C_DISEASE_TX_ADIP ) ] <- 2
# 
# ggplot(t4) +
#   geom_point(aes(x = AGE, y = LH_S_NUM_VALUE, col=as.factor(C_DISEASE_TX_ADIP)))+ #as factor: da adipostitas flag 0 oder 1
#   facet_grid(C_PUB_STAT_PUB_STATUS~SEX ) +
#   scale_color_manual( name = "ADI", values = c( "red", "green", "blue") )+
#   ylim( c(0, 10))
# # ggsave( filename = "LH~Alter_nachSEXundPubstatAdipöseMarkiert.pdf" )
# 
# 
# # # neue spalte für kohorten zuweisung anlegen
# t4$Kohorte <- NA
# t4$Kohorte[grepl( "A", t4$SCIGROUP, perl = T) ] <- "A-Kohorte"
# t4$Kohorte[grepl( "B", t4$SCIGROUP, perl = T) ] <- "B-Kohorte"
# 
# ggplot(t4) +
#   geom_point(aes(x = AGE, y = LH_S_NUM_VALUE, col=Kohorte))+ #as factor: da adipostitas flag 0 oder 1
#   facet_grid(C_PUB_STAT_PUB_STATUS~SEX )
# #ggsave( filename = "LH~Alter_nachSEXundPubstat.pdf" )

 


# ##lineare Regression LH 
# plot(t4$C_ANTHRO_KH_BMI_ORIG, t4$LH_S_NUM_VALUE, xlim = c(0, 50))
# #Regressionsgerade
# abline(lm(t4$C_ANTHRO_KH_BMI_ORIG~t4$LH_S_NUM_VALUE), col="red")
# 
# # ggplot:
# ggplot (daten, aes (x = C_ANTHRO_KH_BMI_ORIG, y = LH_S_NUM_VALUE, colour = GESCHLECHT)) + geom_point() + geom_smooth(method=loess) + ylim(0, 10)
# 
# ## Boxplot: mittlerer LH wert in Abhaengigkeit vom Geschlecht, 
# # boxplot(t4$LH_S_NUM_VALUE ~ SEX, data = t4) #sagt kaum was aus
# 
# 
# ##SÃ¤ulendiagramm LH wert in Abh. vom geschlecht
# histogram( ~  LH_S_NUM_VALUE | SEX, data = t4)
# 
# addmargins(table(t4$SEX, t4$AGE.CATEGORIE))
# summary(t4$AGE[t4$SEX=="male"])
# table(t4$SEX) ##in table zusammenfassen
# fivenum(t4$AGE)
# ?fivenum
# quantile(x = t4$AGE, c(.1,.9))
# 
