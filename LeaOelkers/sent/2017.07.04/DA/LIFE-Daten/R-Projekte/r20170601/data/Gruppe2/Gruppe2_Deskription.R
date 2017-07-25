##Deskriptive Statistik Gruppe 2, n=1229.

rm( list = ls( ) ) ##Speicher löschenren
library(ggplot2)
library(reshape2) #u.a. melt funktion
library(readxl) ##standart excel tbl lesen
library( dplyr ) ##group by befehl, summarise,...
#library( openxlsx ) ##lesen und schreiben
#library(xlsx) ##lesen und schreiben
library(WriteXLS) ##standart zum schreiben
library(psych) ##f?r describeBY

my.theme <-  # bevorzugtes Schema fuer Grafiken
  theme_bw( )

#setwd("c:/Users/Lea/Desktop/DA/LIFE-Daten/R-Projekte/r20170601/") ##Pfad setzen

t4 <-
    read_excel( "~/LIFE/life-for-postgraduates/LeaOelkers/sent/2017.07.04/DA/LIFE-Daten/R-Projekte/r20170601/data/Gruppe2/Gruppe2_fertigerDatensatz.xlsx" )

t4 <-
    t4[ !is.na( t4$C_ANTHRO_KH_BMI_ADJ ), ]

t4 <-
    t4[ 4 < t4$AGE, ] ##den 3 j?hrigen ausschlie?en

##spalten wieder zuf?gen zu gr??erer t4 (235 Spalten): 
load( "~/LIFE/life-for-postgraduates/LeaOelkers/sent/2017.07.04/DA/LIFE-Daten/data/tabelleGesamtJoinAlleSpalten.Rd" )

# nms <-
#     intersect( names( t4 ), names( gesamt.Join.alle.spalten ) )

nms <-
    intersect( names( t4 ), names( tbl ) )

# t4 <-
#     merge( t4, gesamt.Join.alle.spalten, by=nms,all.x = TRUE, all.y=F)
t4 <-
    merge( t4, tbl, by = nms, all.x = TRUE, all.y = F )

# t4 <- t4[ ,c( "alle spalten die ich will") ]  wenn ich wieder nur bestimmet spalten betrachten will

# t4$AGE.CATEGORIE <-
#   cut(
#     x      = t4$AGE,
#     breaks = c(3 : 18 ),
#     labels = c(4 : 18 ) )

nrow( t4 )

##Age by gender
table( t4$SEX )

addmargins( table( t4[ ,c( "SEX", "AGE.CAT" ) ] ) )

##Mittelwert etc des ALters
describeBy(t4$AGE, t4$SEX)

summary(t4$AGE[t4$SEX=="male"])  ##-->mean= 10,38 ##zeigt uns mean, median, min, max etc an von den male an
summary(t4$AGE[t4$SEX=="female"]) ##--> 10,783= mean female

##Histogramm Atersverteilung: 
table(t4$SEX)
male <- subset(t4, t4$SEX == "male")
Age_Boys <-  male$AGE
hist(Age_Boys, xlim = c(3, 20), ylim = c(0, 80), col = "blue", breaks = 20, main = "boys") ##?berschrift andern? in girls boys??
        #siehe K?rner: wie kann ich es nach altersgurppen (jedes Jahr einzeln) anzeigen lassen?

table(t4$SEX)
female <- subset(t4, t4$SEX == "female")
Age_Girls <-  female$AGE
hist(Age_Girls, xlim = c(3, 20), ylim = c(0, 80), col = "blue", breaks = 20, main = "girls") ##?berschrift andern? in girls boys??

t4$AGE.CATEGORIE <-
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

# t4$AGE.CATEGORIE <-
#   cut(
#     x      = t4$AGE,
#     breaks = seq( 2, 18, by = 2 ),
#     labels = seq( 3, 17, by = 2 ) )

##Boxplot age by gender
# data frame schaffen mit Alter-Mean/MAX/MIN nach SEX (f?r die n?chsten Befehle wichtig)
df.mean.age <-
    data.frame(
        SEX  = c( "male", "female", "total" ),
        MEAN = c( mean( t4$AGE[ t4$SEX == "male" ] ), mean( t4$AGE[ t4$SEX == "female" ] ), mean( t4$AGE ) ),
        STDV = c( sd(   t4$AGE[ t4$SEX == "male" ] ), sd(   t4$AGE[ t4$SEX == "female" ] ), sd(   t4$AGE ) ),
        MIN  = c( min(  t4$AGE[ t4$SEX == "male" ] ), min(  t4$AGE[ t4$SEX == "female" ] ), min(  t4$AGE ) ),
        MAX  = c( max(  t4$AGE[ t4$SEX == "male" ] ), max(  t4$AGE[ t4$SEX == "female" ] ), max(  t4$AGE ) ) )

df.mean.age

##zun?chst nicht nach PG getrennt: Boxplot age by gender, wei? mit grauen Plots
# ggplot(t4)+
#   geom_boxplot(aes(x=SEX, y=AGE), fill="gray")+  ##boxplot, grau f?rben
#   geom_errorbar(data=df.mean.age,aes(x=SEX, ymin=MIN,ymax=MAX),size=.3,width=.5)+ ##size=liniendicke, witth=breite der min/MAx werte (=staples)
#   geom_point(aes(x=SEX,y=AGE), position="jitter",alpha=.6)+ ##punktverteilung zus?tzlich darstellen, alpha=tranparenz, position=jitter: punktverteilung weit um Linie herum
#   geom_point(data = df.mean.age, aes(x=SEX,y=MEAN), col="red")+  ##mean als Punkt draufsetzen
#   theme_bw()  ##in schwarz-wei?

#das gleiche ohne jitter: age by gender --> am besten
ggplot(t4)+
  geom_boxplot(aes(x=SEX, y=AGE), fill="gray")+ #
  geom_errorbar(data=df.mean.age,aes(x=SEX, ymin=MIN,ymax=MAX),size=.3,width=.5)+ ##size=liniendicke, witth=breite der min/MAx werte
  geom_jitter(aes(x=SEX,y=AGE),alpha=.1,width = .01)+ ##punktverteilung zus?tzlich darstellen, alpha=tranparenz
  geom_point(data = df.mean.age, aes(x=SEX,y=MEAN), col="red")+
  theme_bw()
  
  # title(main = list("age by gender", cex=1.5, col="blue", font=2)
        ##wie kann man die Schriftgr??e der Achsenbezeichnungen(ylab) ?ndern? Titel einf?gen?

#das Gleiche mit wei?+wei?en Plots
# ggplot(t4)+
#   geom_errorbar(data=df.mean.age,aes(x=SEX, ymin=MIN,ymax=MAX),size=.3,width=.5)+ ##size=liniendicke, witth=breite der min/MAx werte
#   geom_point(aes(x=SEX,y=AGE),alpha=.5)+ ##punktverteilung zus?tzlich darstellen, alpha=tranparenz
#   geom_boxplot(aes(x=SEX, y=AGE),alpha=.5)+ ##
#   geom_point(data = df.mean.age, aes(x=SEX,y=MEAN), col="red")+
#   theme_bw() ##hintergrund in SW
# 
# # da Gleiche alles in grau
# ggplot(t4)+
#   geom_errorbar(aes(x=SEX, ymin=min(AGE),ymax=max(AGE)),size=.3,width=.5)+ ##size=liniendicke, witth=breite der whisker
#   geom_point(aes(x=SEX,y=AGE),alpha=.1)+ ##punktverteilung zus?tzlich darstellen, alpha=tranparenz
#   geom_boxplot(aes(x=SEX, y=AGE),alpha=.5)+ ##
#   geom_point(data = df.mean.age, aes(x=SEX,y=MEAN), col="red")

# mean etc. anzeigen lassen
df.mean.age
summary(t4$AGE[t4$SEX=="male"])  ##-->mean= 10,38 ##zeigt uns mean, median, min, max etc an von den male an
summary(t4$AGE[t4$SEX=="female"]) ##--> 10,783= mean female



##Puberty Groups by gender
table(t4$SEX, t4$C_PUB_STAT_PUB_STATUS)
describe.by(t4$C_PUB_STAT_PUB_STATUS, t4$SEX)

#Anzahl PG
ggplot(t4) +
  my.theme +
  #scale_fill_manual(name = "Geschlecht", values = c("red", "blue")) +
  xlab("PubStat") +
  ylab("Anzahl der Probanden") +
  geom_histogram(aes(x = as.factor( C_PUB_STAT_PUB_STATUS ) ), stat = "count", position = "stack") +
  facet_grid(. ~ SEX)  ##?bereinander: position="stack", nebeneinander w?re: "dodge", ##stat= count: z?hlt anzhal pro pubstat
facet_grid(. ~ SEX) #teilt Z?hlung nach sex
# ggsave( filename = "AnzahlProbanden~PubStat.pdf" )

##ohne sex aufteilung
ggplot(t4) +
  my.theme +
  #scale_fill_manual(name = "Geschlecht", values = c("red", "blue")) +
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
    PG = c( c(1:5),c(1:5))  #f?r male/female
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
  geom_point(aes(x=SEX,y=AGE), position="jitter",alpha=.9)+ ##punktverteilung zus?tzlich darstellen, alpha=tranparenz
  geom_point(aes(x=SEX,y=MEAN), col="red")+
  theme_bw() +
  facet_grid(~C_PUB_STAT_PUB_STATUS)


# --> ohne jitter! (besser finde ich)
ggplot(t5)+
  geom_boxplot(aes(x=SEX, y=AGE), fill="gray")+ #
  geom_errorbar(aes(x=SEX, ymin=MIN,ymax=MAX),size=.3,width=.5)+ ##size=liniendicke, witth=breite der min/MAx werte
  geom_jitter(aes(x=SEX,y=AGE), width = .02,alpha=.9)+ ##punktverteilung zus?tzlich darstellen, alpha=tranparenz
  geom_point(aes(x=SEX,y=MEAN), col="red")+
  theme_bw() +
  facet_grid(~C_PUB_STAT_PUB_STATUS)



##Menarchealter
table(t4$C_PUB_STAT_MENARCHE_WANN, t4$SEX)
describe.by(t4$C_PUB_STAT_MENARCHE_WANN) 

female <- subset(t4, t4$SEX == "female")    ##darstellung: frequency direkt ?ber Alter?Titel? Table runterschreiben?
menarchal_age <-  female$C_PUB_STAT_MENARCHE_WANN
hist(menarchal_age, xlim = c(9, 16), ylim = c(0, 70), col = "blue", breaks = 10, main = "menarchal age")+
 # xlab("menarchal age")+
 #  ylab("frequency")  ##achsenbeschriftung ?ndern??
 #oder vielleicht mit:  scale_x_discrete(name = "female") +
 #  scale_y_continuous(name = "menarchal age") 
  
##boxplot menarchal age 

# ggplot(t4)+
#   geom_boxplot(aes(x=female, y=C_PUB_STAT_MENARCHE_WANN), fill="gray")+ 
#   geom_errorbar(data=t4,aes(x=female, y=C_PUB_STAT_MENARCHE_WANN),size=.3,width=.5)+
#   # geom_errorbar(data=df.mean.age,aes(x=SEX, ymin=MIN,ymax=MAX),size=.3,width=.5)+ ##size=liniendicke, witth=breite der min/MAx werte
#   geom_jitter(aes(x=female,y=C_PUB_STAT_MENARCHE_WANN),alpha=.1,width = .01)+ ##punktverteilung zus?tzlich darstellen, alpha=tranparenz
#   # geom_point(data = t4, aes(x=SEX,y=MEAN), col="red")+
#   theme_bw()

female <- subset(t4, t4$SEX == "female")                   ##wie nur female, wie median dicker?mean einzeichnen?
p1<- ggplot(t4, aes(x = SEX, y = C_PUB_STAT_MENARCHE_WANN)) +
  geom_boxplot()
p1 ##wie mache ich es nur f?r female?
p1 <- p1 + scale_x_discrete(name = "female") +
  scale_y_continuous(name = "menarchal age")   ##?ndert die Achsenbezeichnungen
p1 <- p1 + ggtitle("menarchal age")    ##titel hinzuf?gen
p1



##BMI by age & gender
describeBy(t4$C_ANTHRO_KH_BMI_ORIG, t4$SEX)
summary(t4$C_ANTHRO_KH_BMI_ORIG[t4$SEX=="male"])
summary(t4$C_ANTHRO_KH_BMI_ORIG[t4$SEX=="female"]) ##wie kann ich hier noch "all"/sum berechnen (k?rner s. 10)

describeBy(t4$C_ANTHRO_KH_BMI_ADJ, t4$SEX)
summary(t4$C_ANTHRO_KH_BMI_ADJ[t4$SEX=="male"])
summary(t4$C_ANTHRO_KH_BMI_ADJ[t4$SEX=="female"])

t4$weight.cat <-
  cut( 
    x = t4$C_ANTHRO_KH_BMI_ADJ, 
    breaks =  c( -Inf, -1.28,  1.28, 1.88, +Inf ),
    labels = c( "underweight", "normalweight", "overweight", "obese" ) )

ggplot(t4)+geom_jitter( aes( t4$AGE, C_ANTHRO_KH_BMI_ORIG, col = weight.cat.2 ))+scale_x_continuous(name = "Alter", seq( 3,17,by = 2))
#ggsave("Test.png",width = 10, height = 8)
#ggsave("Test.pdf") ##besser ist pdf
addmargins(table(t4$weight.cat,t4$AGE.CATEGORIE))
addmargins(table(t4$weight.cat.2,t4$AGE.CATEGORIE))

t4$weight.cat.2 <-
  c( "normal.and.underweight", "normal.and.underweight", "overweight.and.obese", "overweight.and.obese" )[ match( t4$weight.cat, c( "underweight", "normalweight", "overweight", "obese" ) ) ]


View(t4[,c("C_ANTHRO_KH_BMI_ADJ", "weight.cat", "weight.cat.2")])
##BMI in Kohorten teilen
underweight <- t4[t4$C_ANTHRO_KH_BMI_ADJ <= -1.28,]  ## <= less than
normalweight <-t4[(t4$C_ANTHRO_KH_BMI_ADJ >= -1.28) & (t4$C_ANTHRO_KH_BMI_ADJ < 1.28),]
overweight <-t4[(t4$C_ANTHRO_KH_BMI_ADJ >= 1.28) & (t4$C_ANTHRO_KH_BMI_ADJ < 1.88),]
obese <-t4[t4$C_ANTHRO_KH_BMI_ADJ >= 1.88,]

underweight%>%summary  ##table f?r alle?
overweight%>%table
obese%>%table
normalweight%>%summary  ##873   ##kiess fragen: zusammenlegen, bei LIFE: wie kann ich mehr overweight bekommen? es ist schon gefiltert...
                          ##fehler: zeigt nicht die richtige zahl an??

all.3 <- filter(t4, C_ANTHRO_KH_BMI_ADJ>=-1.28 & C_ANTHRO_KH_BMI_ADJ <1.28)
all.3%>%table
nrow(all.3$C_ANTHRO_KH_BMI_ADJ)



  #plot mit Age.Categorie
ggplot(t4) +  ##wie kann ich einen text (N=xx) in den Scatterplot einf?gen
    geom_point(aes(x = t4$AGE.CATEGORIE, y = t4$C_ANTHRO_KH_BMI_ORIG, col=SEX))+ #geom_point()= setzt punkte
    facet_grid(.~SEX)+
  scale_color_manual( "sex", values = c( "deeppink", "blue" ) ) +
    xlab("Age") +
    ylab("BMI")
 
#Plot mit AGE
ggplot(t4) +
  geom_point(aes(x = AGE, y = t4$C_ANTHRO_KH_BMI_ADJ, col=SEX))+                
  facet_grid(.~SEX)+
  scale_color_manual( "sex", values = c( "deeppink", "blue" ) ) +
  theme_bw() +
  xlab("Age") +
  ylab("BMI (SDS)")   ##Achsenbezeichnungen
  
  
##Height by age&gender
describeBy(t4$C_ANTHRO_KH_HEIGHT_ORIG, t4$SEX) ##auch Height sds wichtig??
summary(t4$C_ANTHRO_KH_BMI_ORIG[t4$SEX=="male"])
summary(t4$C_ANTHRO_KH_BMI_ORIG[t4$SEX=="female"]) ##wie kann ich hier noch "all"/sum berechnen (k?rner s. 10)

   #plot mit Age categorie
ggplot(t4) +  ##wie kann ich einen text (N=xx) in den Scatterplot einf?gen
  geom_point(aes(x = t4$AGE.CATEGORIE, y = t4$C_ANTHRO_KH_HEIGHT_ORIG, col=SEX))+ #geom_point()= setzt punkte
  facet_grid(.~SEX)+
  xlab("Age") +
  ylab("height")

  #plot mit AGE  #wie altersskalierung ?ndern?wie einzelnen Plot nach geschlecht? wie n=xxx hinzuf?gen in Plots?
ggplot(t4) +
  geom_point(aes(x = t4$AGE, y = t4$C_ANTHRO_KH_HEIGHT_ORIG, col=SEX))+                
  facet_grid(.~SEX)+
  xlab("age") +
  ylab("height (cm)")   

ggplot(t4)+
  geom_boxplot(aes(x=t4$AGE.CATEGORIE, y=C_ANTHRO_KH_HEIGHT_ORIG), fill="gray")+ #
  # geom_errorbar(aes(x=SEX, ymin=MIN,ymax=MAX),size=.3,width=.5)+ ##size=liniendicke, witth=breite der min/MAx werte
  geom_jitter(aes(x=AGE.CATEGORIE,y=C_ANTHRO_KH_HEIGHT_ORIG), width = .02,alpha=.9)+ ##punktverteilung zus?tzlich darstellen, alpha=tranparenz
  theme_bw() +
  ylab("height(cm)")+
  xlab("age")+
  facet_grid(~SEX)

## LH by age & gender
describeBy(t4$LH_S_NUM_VALUE, t4$SEX) ##auch Height sds wichtig??
summary(t4$LH_S_NUM_VALUE[t4$SEX=="male"])
summary(t4$LH_S_NUM_VALUE[t4$SEX=="female"]) ##wie kann ich hier noch "all"/sum berechnen (k?rner s. 10)
   ##warum male n=562??

## FSH by age & gender
describeBy(t4$FSH_S_NUM_VALUE, t4$SEX) ##auch Height sds wichtig??
summary(t4$FSH_S_NUM_VALUE[t4$SEX=="male"])
summary(t4$FSH_S_NUM_VALUE[t4$SEX=="female"])

 #Plot: LH~age nach SEX
ggplot(t4) +
  geom_point(aes(x = AGE, y = LH_S_NUM_VALUE, col=SEX))+   
  ylab("LH(IU/L)")+
  xlab("age")+
  facet_grid(.~SEX)
   ##wie kann ich die achsenskalierung begrenzen?alterseinteilung in 6,8,10.etc?

#Plot: FSH~age nach SEX
ggplot(t4) +
  geom_point(aes(x = AGE, y = FSH_S_NUM_VALUE, col=SEX))+   
  ylab("FSH(IU/L)")+
  xlab("age")+
  facet_grid(.~SEX)
##wie kann ich die achsenskalierung begrenzen?alterseinteilung in 6,8,10.etc?


# # oder beide sex zusammen:
# ggplot(t4) +
#   geom_point(aes(x = AGE, y = LH_S_NUM_VALUE, col=SEX))
# # ggsave( filename = "LH~Alter_SexZusammen.pdf" )
# 
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
# ##im selben Plot wie vorher die Adip?sen-Falgs markieren
# t4$C_DISEASE_TX_ADIP[ is.na( t4$C_DISEASE_TX_ADIP ) ] <- 2
# 
# ggplot(t4) +
#   geom_point(aes(x = AGE, y = LH_S_NUM_VALUE, col=as.factor(C_DISEASE_TX_ADIP)))+ #as factor: da adipostitas flag 0 oder 1
#   facet_grid(C_PUB_STAT_PUB_STATUS~SEX ) +
#   scale_color_manual( name = "ADI", values = c( "red", "green", "blue") )+
#   ylim( c(0, 10))
# # ggsave( filename = "LH~Alter_nachSEXundPubstatAdip?seMarkiert.pdf" )
# 
# 
# # # neue spalte f?r kohorten zuweisung anlegen
# t4$Kohorte <- NA
# t4$Kohorte[grepl( "A", t4$SCIGROUP, perl = T) ] <- "A-Kohorte"
# t4$Kohorte[grepl( "B", t4$SCIGROUP, perl = T) ] <- "B-Kohorte"
# 
# ggplot(t4) +
#   geom_point(aes(x = AGE, y = LH_S_NUM_VALUE, col=Kohorte))+ #as factor: da adipostitas flag 0 oder 1
#   facet_grid(C_PUB_STAT_PUB_STATUS~SEX )
# #ggsave( filename = "LH~Alter_nachSEXundPubstat.pdf" )

 
t4$Kohorte <- NULL ##spalte wieder l?schen wenn ich sie nicht mehr brauche
unique( t4$SCIGROUP)

##Winkler
t4$D00177_SCORE_FAM <- cut(t4$D00177_SCORE_FAM, breaks = c(3, 8, 14, 21)) ##breaks beizeihcnen?hoher status etc
table(t4$D00177_SCORE_FAM) ## nachz?hlen: NAs? (68?)

hist(t4$D00177_SCORE_FAM, breaks=0:22, col="darkseagreen3", main="")

 #Barplot in 3 Winklerkategorien
barplot(table(t4$D00177_SCORE_FAM), main = "frequencies of winkler categories")  ##wie achsenbezeichnung ?ndern? Titel?

                               
#Plot: Anzahl der Probanden nach Winkler
ggplot(t4) +
  my.theme +
  xlab("Winkler-Kategorie") +
  ylab("Anzahl der Probanden") +
  geom_histogram(aes(x = as.factor( D00177_SCORE_FAM ) ), stat = "count", position = "stack")
# ggsave( filename = "AnzahlBesuche~Winkler.pdf" )

# 
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
# ##Säulendiagramm LH wert in Abh. vom geschlecht
# histogram( ~  LH_S_NUM_VALUE | SEX, data = t4)
# 
# addmargins(table(t4$SEX, t4$AGE.CATEGORIE))
# summary(t4$AGE[t4$SEX=="male"])
# table(t4$SEX) ##in table zusammenfassen
# fivenum(t4$AGE)
# ?fivenum
# quantile(x = t4$AGE, c(.1,.9))
# 
