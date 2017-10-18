# source( "c:/Users/Lea/Desktop/AllesNeu20170725/scripts/join.all.tables.2017.07.25.R" )
# source( "c:/Users/Lea/Desktop/AllesNeu20170725/scripts/curation.R" )
# source( "c:/Users/Lea/Desktop/AllesNeu20170725/scripts/visits.R" )
# source( "c:/Users/Lea/Desktop/AllesNeu20170725/scripts/description.R" )


if( !"devtools" %in% rownames( installed.packages( ) ) ) install.packages( "devtools" )

# installiere neueste Version von helperForLife, falls noch nicht geschehen
devtools::install_github( "TPeschel/hlpr4life" )

# lade hlpr4life
library( hlpr4life )

load.pkgs(
  c(
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
setwd( "c:/Users/Lea/Desktop/AllesNeu20170725/data/generated/" )  

# load( "main.table.curated.first.visit.Rd" )












## Die LH- und FSH- Werte korrelieren mit den Pubert?tsstadien (und dem Geburtsgewicht). 
  
    ## 1. Plots: Boxplots?
    ## LH~PG (Scatterplot LH ?ber das Alter, PG eingef?rbt? oder Boxplot nach PG getrennt)
    ## 2. Tabelle: LH/FSH (means) jeweils zu B1-5 etc.
    

    ## 3. Korrelationen: LH ~ B/G/HV/PH

  ## Einfache Korrelation: PG/Parameter/Geburtsgewicht mit LH/FSH korrelieren??? was zeigt die gr??te Korrelation?
      
      ## 1. Korr.:PG ~ LH  #Frage: Pearson oder Spearman? Reicht PG insgesamt oder einzeln in 1-5?

cor.test(tbl$C_PUB_STAT_PUB_STATUS,tbl$LH_S_NUM_VALUE)

                # Pearson's product-moment correlation
                # 
                # data:  tbl$C_PUB_STAT_PUB_STATUS and tbl$LH_S_NUM_VALUE
                # t = 27.496, df = 1531, p-value < 2.2e-16
                # alternative hypothesis: true correlation is not equal to 0
                # 95 percent confidence interval:
                # 0.5404465 0.6075325
                # sample estimates:
                # cor 
                # 0.574955        #Korrelationskoeffizzient nach Pearson

cor(tbl$C_PUB_STAT_PUB_STATUS,tbl$LH_S_NUM_VALUE,
    method = "spearman")          #0.8443385 Korrelationskoeffizzient nach Spearman

      ## 1. Korr.:B ~ LH


cor.test(tbl$C_PUB_STAT_B, tbl$LH_S_NUM_VALUE)

                # Pearson's product-moment correlation
                # 
                # data:  tbl$C_PUB_STAT_B and tbl$LH_S_NUM_VALUE
                # t = 19.156, df = 776, p-value < 2.2e-16
                # alternative hypothesis: true correlation is not equal to 0
                # 95 percent confidence interval:
                # 0.5169231 0.6125169
                # sample estimates:
                # cor 
                # 0.5666236 

cor(tbl$C_PUB_STAT_B,tbl$LH_S_NUM_VALUE,
    method = "spearman", use = "complete.obs" ) ##NAs werden gel?scht, 0.8583518

cor.test(tbl$C_PUB_STAT_B, tbl$Geburtsgewicht)
                    # 
                    # Pearson's product-moment correlation
                    # 
                    # data:  tbl$C_PUB_STAT_B and tbl$Geburtsgewicht
                    # t = -1.6678, df = 2036, p-value = 0.09552
                    # alternative hypothesis: true correlation is not equal to 0
                    # 95 percent confidence interval:
                    #  -0.080227525  0.006494807
                    # sample estimates:
                    #        cor 
                    # -0.0369359 

##Zus?tzlich lineares Modell: Einflussgr??e LH/FSH/Geburtsgewicht, Zielgr??e: PG/Parameter. Oder umgekehrt?da zielgr??e stetig sein muss
 # --> Zielgr??e LH/F

##LH~AGE
(lm.age.LH <-
    lm(LH_S_NUM_VALUE ~ AGE, data = tbl))

tidy( summary( lm.age.LH ) )



##LH~B
(lm.tbl.LH.PG <-
    lm(LH_S_NUM_VALUE ~ C_PUB_STAT_B, data = tbl ) )

tidy( summary( lm.tbl.LH.PG ) )

##LH ~ age+sex+B
(lm.LH.age.B <-
    lm(LH_S_NUM_VALUE ~ AGE + C_PUB_STAT_B, data = tbl ) )

tidy( summary( lm.LH.age.PG ) )



###########################

##Vorhersage: mit steigenden LH-Werten steigt die Wahrscheinlichkeit der Menarche /Hypothese: kann man anhand der Hormonwerte Pubert?tsstadien/etc. vorhersehen? 


#log. Regression: Zielgr??e: Menarche ja/nein, Einfluss LH/FSH  -->Funktionskurve f?r Wahrscheinlichkeit, Regressionskoeff., OR. 

log.test <- glm(tbl$C_PUB_STAT_MENARCHE ~ tbl$LH_S_NUM_VALUE,
                family = binomial (link="logit"), data = tbl)
log.test

plot(glm(tbl$C_PUB_STAT_MENARCHE ~ tbl
         $LH_S_NUM_VALUE, binomial))


#################################

## ?bergewichtige und adip?se Kinder treten fr?her als normalgewichtige Kinder in die Pubert?t ein. 
  #frage: was spiegelt am besten den Pubert?tseintritt wieder?--> P 1 oder >1

##oder: mean age des Eintrittsalters berechnen für weight groups und mit t-test  vergleichen

##2 gruppen
tbl.OW<- tbl[tbl$WGHT_GRPS=="overweight.and.obese",]
tbl.NW<- tbl[tbl$WGHT_GRPS=="normalweight",]

##mean age in PG2 für sex und weight groups
tbl.sum.PG2 <- tbl %>% group_by( SEX, WGHT_GRPS, C_PUB_STAT_PUB_STATUS ) %>% summarise( AGE.MEAN = mean( AGE ), AGE.VAR = var( AGE ) )
tbl.sum.PG2[tbl.sum.PG2$C_PUB_STAT_PUB_STATUS==2,]

##jetzt wahrscheinlichkeit für jeweilige gurppe ausrechnen, in die PG2 zu kommen oder (OR)
##t test: varianzen der alter in PG2 vergleichen auf sign. Unterschiede



##lin.Regression: Zielgr??e: z.B. PG2 (mean age), Einfluss: BMI(SDS)? 


##log. Regression: table: OR aus Vierfeldertafel (p1/p>1 und OW/NW), dann glm
  #Bsp: glm(P ~ OW + age + SES.f, data =d)  --> ?-->e^? = OR -->CI
