#############################################################################
##################MEDIEN, SDQ, KIDSCREEN, FREIZEIT############################
##############################################################################

#####################################################################################################################
##################Vorbereitung##########################################
########################################################################

source("H:/R/connectionWIN.r")
#verbindung mit lifedatnbank (?ber mandys account)

#source( "~/LIFE/.secret.R" )

#con <-
#    db.access::db.open( Sys.getenv( "DB_USER" ), Sys.getenv( "DB_PASS" ) )

library( lifecuration )
#l?dte pakete, die fur datenbank geschrieben wurden


##################################################################
####medien umkodieren###########################################
#################################################################

library("dplyr")

load( "20170405_medien_noten.rdata" )

medien$T00156_F0009 <- dplyr::recode(medien$T00156_F0009,
                              `1` = 0, `2` = 0.5, `3` = 1.5, `4` = 3.5, `5` = 5) 
medien$T00156_F0010 <- dplyr::recode(medien$T00156_F0010,
                              `1` = 0, `2` = 0.5, `3` = 1.5, `4` = 3.5, `5` = 5) 
medien$T00156_F0011 <- dplyr::recode(medien$T00156_F0011,
                              `1` = 0, `2` = 0.5, `3` = 1.5, `4` = 3.5, `5` = 5) 
medien$T00156_F0012 <- dplyr::recode(medien$T00156_F0012,
                              `1` = 0, `2` = 0.5, `3` = 1.5, `4` = 3.5, `5` = 5) 
medien$T00156_F0013 <- dplyr::recode(medien$T00156_F0013,
                              `1` = 0, `2` = 0.5, `3` = 1.5, `4` = 3.5, `5` = 5)

medien$medien_all <- medien$T00156_F0009 + medien$T00156_F0010 + medien$T00156_F0011 + medien$T00156_F0012 + medien$T00156_F0013

medien$medien_bild <- medien$T00156_F0009 + medien$T00156_F0010 + medien$T00156_F0011 + medien$T00156_F0013

##################################################################
####freizeit umkodieren###########################################
#################################################################

freizeit_sb$T00159_F0016 <- dplyr::recode(freizeit_sb$T00159_F0016, 
                                   `0` = 0, `1` = 0.5, `2` = 1.5, `3` = 4, `4` = 6)
freizeit_sb$T00159_F0017 <- dplyr::recode(freizeit_sb$T00159_F0017, 
                                   `0` = 0, `1` = 0.5, `2` = 1.5, `3` = 4, `4` = 6)
freizeit_sb$frei_sport <- freizeit_sb$T00159_F0016 + freizeit_sb$T00159_F0017

#####################################################################################################################
#####noten umkodieren#################
######################################


noten$T00152_F0024 <- dplyr::recode(noten$T00152_F0024,
                             `0` = 6, `1` = 5, `2` = 5, `3` = 5, `4` = 4, `5` = 4, `6` = 4, `7` = 3, `8` = 3, `9` = 3, `10` = 2, `11` = 2, `12` = 2, `13` = 1, `14` = 1, `15` = 1) 
noten$T00152_F0025 <- dplyr::recode(noten$T00152_F0025,
                             `0` = 6, `1` = 5, `2` = 5, `3` = 5, `4` = 4, `5` = 4, `6` = 4, `7` = 3, `8` = 3, `9` = 3, `10` = 2, `11` = 2, `12` = 2, `13` = 1, `14` = 1, `15` = 1) 
noten$T00152_F0026 <- dplyr::recode(noten$T00152_F0026,
                             `0` = 6, `1` = 5, `2` = 5, `3` = 5, `4` = 4, `5` = 4, `6` = 4, `7` = 3, `8` = 3, `9` = 3, `10` = 2, `11` = 2, `12` = 2, `13` = 1, `14` = 1, `15` = 1, `16` = 7)
noten$T00152_F0027 <- dplyr::recode(noten$T00152_F0027,
                             `0` = 6, `1` = 5, `2` = 5, `3` = 5, `4` = 4, `5` = 4, `6` = 4, `7` = 3, `8` = 3, `9` = 3, `10` = 2, `11` = 2, `12` = 2, `13` = 1, `14` = 1, `15` = 1, `16` = 7) 

#############datenaufbereitung

#######noten- und punktsystem zusammenfassen

ms3 <- is.na(noten$T00152_F0020)
noten$T00152_F0020 [ms3] <- noten$T00152_F0024 [ms3]

ms4 <- is.na(noten$T00152_F0021)
noten$T00152_F0021 [ms4] <- noten$T00152_F0025 [ms4]

ms5 <- is.na(noten$T00152_F0022)
noten$T00152_F0022 [ms5] <- noten$T00152_F0026 [ms5]


ms6 <- is.na(noten$T00152_F0023)
noten$T00152_F0023 [ms6] <- noten$T00152_F0027 [ms6]

noten$T00152_F0020[noten$T00152_F0020 == 7] <- NA
noten$T00152_F0021[noten$T00152_F0021 == 7] <- NA
noten$T00152_F0022[noten$T00152_F0022 == 7] <- NA
noten$T00152_F0023[noten$T00152_F0023 == 7] <- NA

table(noten$T00152_F0020)
table(noten$T00152_F0021)
table(noten$T00152_F0022)

########################################
##noten zusammenfassen
######################################

noten$T00152_F0020 <- dplyr::recode(noten$T00152_F0020,
                             `1` = 1, `2` = 2, `3` = 3, `4` = 4, `5` = 4, `6` = 4) 
noten$T00152_F0021 <- dplyr::recode(noten$T00152_F0021,
                             `1` = 1, `2` = 2, `3` = 3, `4` = 4, `5` = 4, `6` = 4) 
noten$T00152_F0022 <- dplyr::recode(noten$T00152_F0022,
                             `1` = 1, `2` = 2, `3` = 3, `4` = 4, `5` = 4, `6` = 4) 

#noten <- subset(noten, T00152_F0022 <6)
#noten <- subset(noten, T00152_F0021 <6)
#noten <- subset(noten, T00152_F0020 <6)

############################################
#bestimmte Schulformen rausschmei?en
###########################

table(schulform$D00175_K_SCHULE)
#noch-nicht-sch?ler und grundsch?ler raus
schulform <- subset(schulform, D00175_K_SCHULE >1)
#nicht-merh-sch?ler raus:
schulform <- subset(schulform, D00175_K_SCHULE <8)

table(schulform$D00175_K_SCHULE)
#gesamtsch?ler und f?rdersch?ler und hauptsch?ler mit realsch?lern verbinden
schulform$D00175_K_SCHULE <- dplyr::recode(schulform$D00175_K_SCHULE,
                                       `2` = 2, `3` = 2, `4` = 2, `5` = 2, `6` = 1, `7` = 2) 
#schulform als faktor
schulform$D00175_K_SCHULE <- as.factor(schulform$D00175_K_SCHULE)
#1 entspricht ymnasium, 2 entspricht Nicht-Gymnasium

#####################################
#####zusammenf?gen##################
####################################

med_frei <- merge(medien, freizeit_sb,
                      by = c("SIC","SGROUP"),
                      all.x = T)

med_frei_noten <- merge(med_frei, noten,
                              by = c("SIC","SGROUP"),
                              all.x = T)

med_frei_noten_bmi <- merge(med_frei_noten, bmi,
                            by = c("SIC","SGROUP"),
                            all.x = T)

med_frei_noten_bmi_schulform <- merge(med_frei_noten_bmi, schulform,
                            by = c("SIC","SGROUP"),
                            all.x = T)

med_frei_noten_fams <- merge(med_frei_noten_bmi_schulform, fams,
                             by = c("SIC"),
                             all.x = T)
med_frei_noten_fams$EDAT.YEAR <- year(med_frei_noten_fams$EDAT.x)
ses$EDAT.YEAR <- year(ses$EDAT)


med_frei_ses_noten_bmi <- merge (med_frei_noten_fams, ses,
                             by = c("SIC","EDAT.YEAR"),
                             all.x = T)

#######################################
#####f?llebeschr?nkung#################
#######################################

#Alternative: schneidet nach komma ab
med_frei_ses_noten_bmi$age2 <- floor(med_frei_ses_noten_bmi$age)

#beschr?nkung auf 10 bis 17 j?hrige
mfqs_noten <- med_frei_ses_noten_bmi[between(med_frei_ses_noten_bmi$age, 10.0000, 17.9999),]
mfqs_noten <- med_frei_ses_noten_bmi[med_frei_ses_noten_bmi$age >= 10.0000 & med_frei_ses_noten_bmi$age < 17.9999,]
mfqs_noten <- mfqs_noten[mfqs_noten$age2 >= 10.0000 & mfqs_noten$age2 < 17.9999,]

summary(mfqs_noten$age)
hist(mfqs_noten$age)

#Variable "Jahr" berechnen
mfqs_noten$year <- substr(mfqs_noten$EDAT.x, 3,4)
mfqs_noten$year <- as.numeric(mfqs_noten$year)

#beschr?nkung auf ersten besuch
mfqs_noten <- mfqs_noten[order(mfqs_noten$SIC, mfqs_noten$age),]
mfqs_1_noten <- mfqs_noten[!duplicated(mfqs_noten$SIC),]

#Geschwister rausschmei?en
sum(duplicated(mfqs_1_noten$D00202_FAM_ID))
mfqs_1_noten <- mfqs_1_noten[!duplicated(mfqs_1_noten$D00202_FAM_ID),]

#Kinder mit vollst?ndigen daten zu T1
mfqs_1_noten <- subset(mfqs_1_noten, D00177_SCORE_FAM >0)
mfqs_1_noten <- subset(mfqs_1_noten, !is.na(D00175_K_SCHULE))
mfqs_1_noten <- subset(mfqs_1_noten, !is.na(T00152_F0022))

####################################
#zusammenh?nge########################
#####################################

#MATHE:
(lm.noten_m_bild.all <- lm(T00152_F0020 ~ frei_sport
                      + medien_bild
                      + age
                     + sex
                      + D00177_SCORE_FAM
                      + year
                      + D00040_BMI_SDS
                      + D00175_K_SCHULE, data = mfqs_1_noten))
summary(lm.noten_m_bild.all)
library(lm.beta)
library(car)
lm.beta(lm.noten_m_bild.all)
#medien
#keine interaktion 
vif(lm.noten_m_bild.all)
sqrt(vif(lm.noten_m_bild.all))
#DEUTSCH
(lm.noten_d_bild.all <- lm(T00152_F0021 ~ frei_sport
                      + medien_bild
                     + age
                      + sex
                      + D00177_SCORE_FAM
                      + year
                      + D00040_BMI_SDS
                      + D00175_K_SCHULE, data = mfqs_1_noten))
summary(lm.noten_d_bild.all)
lm.beta(lm.noten_d_bild.all)
#medien
#keine interaktionen

#SPORT
(lm.noten_s_bild.all <- lm(T00152_F0022 ~ frei_sport
                      + medien_bild
                      + age
                      + sex
                      + D00177_SCORE_FAM
                      + year
                      + D00040_BMI_SDS
                      + D00175_K_SCHULE, data = mfqs_1_noten))
summary(lm.noten_s_bild.all)
lm.beta(lm.noten_s_bild.all)
plot(effects::allEffects(lm.noten_s_bild.all))
#sport
#interaktion sport_ses: negativer zusammenhang note-sport, der mit zunahme des ses schw?cher wird
#aber: vif
plot(effects::Effect(c("medien_bild","D00040_BMI_SDS"),lm.noten_s_bild.all, xlevels = list(D00040_BMI_SDS = -1:1)))

vif(lm.noten_s_bild.all)

##############################
#l?ngsschnitt
#################################

mfqs_noten$EDAT.x = NULL
mfqs_noten$EDAT.y = NULL
mfqs_noten$ID.x = NULL
mfqs_noten$ID.y = NULL
mfqs_noten$VERSION.x = NULL
mfqs_noten$VERSION.y = NULL
mfqs_noten$EDAT.x = NULL
mfqs_noten$EDAT.y = NULL

mfqs_noten1.2 <- mfqs_noten %>% group_by(SIC) %>% mutate(
  visnr = dense_rank(age),
  max.vis = max(visnr)
) %>% filter(visnr %in% c(1,2) & max.vis > 1) %>%
  select(SIC, year, D00175_K_SCHULE, D00040_BMI_SDS, D00040_BMI_ORIG, T00159_F0016, T00159_F0017, T00156_F0009, T00156_F0010, T00156_F0011, T00156_F0012, T00156_F0013, age,sex, D00177_SCORE_FAM, medien_all, medien_bild, frei_sport, visnr, max.vis, T00152_F0020, T00152_F0021, T00152_F0022, T00152_F0023, D00202_FAM_ID)


mfqs_noten1.2 <- mfqs_noten1.2 %>% group_by(SIC) %>% mutate(
  diff.time = abs(diff(age))
)

mfqs_noten1.2 <- as.data.frame(mfqs_noten1.2)

mfqs_noten1.2 <- reshape(mfqs_noten1.2, 
                         idvar = c("SIC","sex"),
                         timevar = "visnr",
                         direction = "wide")

mfqs_noten1.2 <- mfqs_noten1.2[between(mfqs_noten1.2$diff.time.1, 0.5, 1.5),]

mfqs_noten1.2[mfqs_noten1.2$diff.time.1 > 1.5,"SIC"]
mfqs_noten1.2[mfqs_noten1.2$diff.time.1 < 0.5,"SIC"]

#Geschwister rausschmei?en
sum(duplicated(mfqs_noten1.2$D00202_FAM_ID.1))
mfqs_noten1.2 <- mfqs_noten1.2[!duplicated(mfqs_noten1.2$D00202_FAM_ID.1),]

#vollst?ndige daten
mfqs_noten1.2 <- subset(mfqs_noten1.2, D00177_SCORE_FAM.1 >0)
mfqs_noten1.2 <- subset(mfqs_noten1.2, !is.na(D00175_K_SCHULE.1))
mfqs_noten1.2 <- subset(mfqs_noten1.2, !is.na(T00152_F0022.1))
#mfqs_noten1.2 <- subset(mfqs_noten1.2, !is.na(T00152_F0022.2))

table(mfqs_noten1.2$sex)
summary(mfqs_noten1.2$age.1)

#######################

table(mfqs_noten1.2$T00152_F0020.2)
table(mfqs_noten1.2$T00152_F0021.2)
table(mfqs_noten1.2$T00152_F0022.2)

table(mfqs_noten1.2$T00152_F0023.2, mfqs_noten1.2$D00175_K_SCHULE.1)

#MATHE
mfqs_noten1.2$T00152_F0020.2 <- as.numeric(mfqs_noten1.2$T00152_F0020.2)
(lm.noten_m.bild.x <- lm(T00152_F0020.2 ~ frei_sport.1
                        + medien_bild.1
                        + age.2
                        + sex
                        + D00177_SCORE_FAM.1
                        + T00152_F0020.1
                        + year.1
                        + D00040_BMI_SDS.1
                        + D00175_K_SCHULE.1, data = mfqs_noten1.2))
summary(lm.noten_m.bild.x)
lm.beta(lm.noten_m.bild.x)
#medien (pos)
#keine interaktion


##############################################################################
##############################################################################
#######bei folgendem modell bitte 1 plot f?r beide Geschlechter #############
#################################################################################
#################################################################################

mfqs_noten1.2$T00152_F0020.2 <- as.numeric(mfqs_noten1.2$T00152_F0020.2)

(lm.noten_m.einzeln.x <- lm(T00152_F0020.2 ~ T00159_F0016.1
                            + T00159_F0017.1
                            + T00156_F0009.1
                            + T00156_F0010.1
                            + T00156_F0011.1
                            + T00156_F0013.1
                            + age.2
                            + sex
                            + D00177_SCORE_FAM.1
                            + year.1
                            + T00152_F0020.1
                            + D00040_BMI_SDS.1
                            + D00175_K_SCHULE.1, data = mfqs_noten1.2))

library(effects)
library( ggplot2 )
library( ggthemes )

d <-
    as.data.frame( Effect( "T00156_F0011.1", lm.noten_m.einzeln.x ) )

ggplot( d ) +
    theme_bw( ) + 
    geom_ribbon( aes( x = T00156_F0011.1, ymin = lower, ymax = upper ), col = "gray", fill = "grey", alpha = .5 ) +
    geom_line( aes( T00156_F0011.1, y = fit ), d ) +
    xlab( "use of media [h]" ) +
    ylab( "mathematical education [mark]" ) +
    theme( panel.grid = element_blank( ) )

ggsave( "mathe_medien.pdf" )    
    
ggplot( ) +
    theme_bw( ) + 
    #geom_jitter( aes( T00156_F0011.1f, T00152_F0020.2 ), mfqs_noten1.2, alpha = .5) +
    geom_boxplot( aes( T00152_F0020.2, T00156_F0011.1f ), mfqs_noten1.2, alpha = .5 ) #+ 
    #geom_line( aes( i, y ), d.y0.m ) +
    #geom_point( aes( i, y, size = n ), d.y0.m, col = "red" ) +
    #geom_line( aes( i, m), d.y0.m.1 ) +
    #geom_point( aes( i, m, size = n ), d.y0.m.1, col = "green" ) 
    
ggsave( "unschoen.pdf")

summary(lm.noten_m.einzeln.x)
#PC (pos)
#keine interaktionen
confint(lm.noten_m.einzeln.x)

library( effects )
plot(Effect(c("T00156_F0011.1"),lm.noten_m.einzeln.x))


# ohne ia
mfqs_noten1.2$T00156_F0011.1_neu <- as.factor(mfqs_noten1.2$T00156_F0011.1)
plot(mfqs_noten1.2$T00156_F0011.1_neu, mfqs_noten1.2$T00152_F0020.2) # mathe

#DEUTSCH

mfqs_noten1.2$T00152_F0021.2 <- as.numeric(mfqs_noten1.2$T00152_F0021.2)
(lm.noten_d.bild.x <- lm(T00152_F0021.2 ~ frei_sport.1
                        + medien_bild.1
                        + age.2
                        + sex
                        + D00177_SCORE_FAM.1
                        + T00152_F0021.1
                        + year.1
                        + D00040_BMI_SDS.1
                        + D00175_K_SCHULE.1, data = mfqs_noten1.2))

summary(lm.noten_d.bild.x)
vif(lm.noten_d.bild.x)
lm.beta(lm.noten_d.bild.x)
#ns
#interaktion sport_ses!!!
#aber: vif
confint(lm.noten_d.bild.x)
library(effects)
plot(Effect(c("frei_sport.1","D00177_SCORE_FAM.1"),lm.noten_d.bild.x))
#niedriger SES: schlechtere noten bei weniger sport, h?herer SES: umgekehrt (aber weniger steil)

Effect(c("frei_sport.1","D00177_SCORE_FAM.1"),lm.noten_d.bild.x)

(lm.noten_d.einzeln.x <- lm(T00152_F0021.2 ~ T00159_F0016.1
                            + T00159_F0017.1*D00177_SCORE_FAM.1 + T00159_F0017.1*age.2
                            + T00156_F0009.1
                            + T00156_F0010.1
                            + T00156_F0011.1
                            + T00156_F0013.1
                            + age.2
                            + sex
                            + D00177_SCORE_FAM.1
                            + year.1
                            + T00152_F0021.1
                            + D00040_BMI_SDS.1
                            + D00175_K_SCHULE.1, data = mfqs_noten1.2))
summary(lm.noten_d.einzeln.x)
vif(lm.noten_d.einzeln.x)
#
#interaktion T00159_F0017.1*D00177_SCORE_FAM.1: niedriger ses: neg zusm, hoher ses: pos. zus.
#(interaktion T00156_F0011.1*D00177_SCORE_FAM.1)
#interaktion T00159_F0017.1*age.2: erst bei ?lteren kindern zus (neg) sport-deutschnote
#aber: vif

plot(allEffects(lm.noten_d.einzeln.x))
plot(Effect(c("T00159_F0017.1","D00177_SCORE_FAM.1"),lm.noten_d.einzeln.x))
plot(Effect(c("T00159_F0017.1","age.2"),lm.noten_d.einzeln.x))

#SPORT

mfqs_noten1.2$T00152_F0022.2 <- as.numeric(mfqs_noten1.2$T00152_F0022.2)
(lm.noten_s.bild.x <- lm(T00152_F0022.2 ~ frei_sport.1
                        + medien_bild.1
                        + age.2
                        + sex
                        + D00177_SCORE_FAM.1
                        + T00152_F0022.1
                        + year.1
                        + D00040_BMI_SDS.1
                        + D00175_K_SCHULE.1, data = mfqs_noten1.2))
summary(lm.noten_s.bild.x)
lm.beta(lm.noten_s.bild.x)
#sport
#keine interaktionen
confint(lm.noten_s.bild.x)


##############################################################################
##############################################################################
#######bei folgendem modell bitte plot por Geschlecht ########################
#########bzw. beide geschlechter in einer grafik farblich trennen############
#################################################################################
#################################################################################

#einzelne medien
(lm.noten_s.einzeln.x <- lm(T00152_F0022.2 ~ T00159_F0016.1
                            + T00159_F0017.1*sex
                            + T00156_F0009.1
                            + T00156_F0010.1
                            + T00156_F0011.1
                            + T00156_F0013.1
                            + age.2
                            + D00177_SCORE_FAM.1
                            + year.1
                            + T00152_F0022.1
                            + D00040_BMI_SDS.1
                            + D00175_K_SCHULE.1, data = mfqs_noten1.2))
summary(lm.noten_s.einzeln.x)
confint(lm.noten_s.einzeln.x)
vif(lm.noten_s.einzeln.x)
#(sport mit verein), sport ohne verein, TV 
#interaktion T00156_F0009.1*D00177_SCORE_FAM.1: nur bei niedrigerem SES: pos. zusm (also mehr TV --> schlechterer sportnote)
#interaktion T00159_F0017.1*sex: nur neg. zusammenhang bei m?dchen
#vif! au?er f?r interaktion mit sex! diese sollte man also interpretieren!
plot(allEffects(lm.noten_s.einzeln.x))
plot(Effect(c("T00156_F0009.1","D00177_SCORE_FAM.1"),lm.noten_s.einzeln.x))
plot(Effect(c("T00159_F0017.1","sex"),lm.noten_s.einzeln.x))

d <-
    as.data.frame( Effect(c("T00159_F0017.1","sex"),lm.noten_s.einzeln.x) )

d
library( hlpr4life )

d$sex <-
    c( "girls", "boys" )[ match( d$sex, c( "female", "male" ) ) ]

ggplot( d ) +
    theme_bw( ) +
    geom_ribbon( aes( x = T00159_F0017.1, ymin = lower, ymax = upper ), col = "gray", fill = "grey", alpha = .5 ) +
    geom_line( aes( T00159_F0017.1, y = fit ), d ) +
    xlab( "physical activity [h]" ) +
    ylab( "physical education [mark]" ) +
    theme( panel.grid = element_blank( ) ) +
    facet_grid( . ~ sex )

ggsave( "interaction.png", width = 8, height = 6 )
    

ggplot( mfqs_noten1.2 ) +
    geom_boxplot( aes( as.factor( T00152_F0022.2 ), T00159_F0017.1 ))

table( mfqs_noten1.2[ , c( "T00159_F0017.1", "T00152_F0022.2", "sex" ) ] )



# mit interaktion sex
mfqs_noten1.2$T00159_F0017.1_neu <- as.factor(mfqs_noten1.2$T00159_F0017.1)
plot(mfqs_noten1.2$T00159_F0017.1_neu, mfqs_noten1.2$T00152_F0022.2)  # sport




