#############################################################################
##################MEDIEN, SDQ, KIDSCREEN, FREIZEIT############################
##############################################################################

#####################################################################################################################
##################Vorbereitung##########################################
########################################################################

#source("H:/R/connectionWIN.r")
source( "~/connection/connection.r" )
#verbindung mit lifedatnbank (?ber mandys account)

library(lifecuration)
#l?dte pakete, die fur datenbank geschrieben wurden

persdat <- get.persdat(ldb)
#holt datei "persdat" (infos zu geschlecht, alter, geburtstag) aus der datenbank (ldb) und nennt sie persdat

medien <- get.data( ldb, "T00156" )
medien <- add.persdat.age( persdat, medien )
show.dups( medien, sic = "SIC", zp = "SGROUP" )
medien <- remove.duplicates( medien, sic = "SIC", zp = "SGROUP" )
table( medien$SGROUP )

freizeit_sb <- get.data(ldb, "T00159")
show.dups(freizeit_sb, sic = "SIC", zp = "SGROUP")
freizeit_sb <- remove.duplicates(freizeit_sb, sic = "SIC", zp = "SGROUP")
table(freizeit_sb$SGROUP)

ses <- get.data(ldb, "D00177")
show.dups(ses, sic = "SIC", zp = "SGROUP")
ses <- remove.duplicates(ses, sic = "SIC", zp = "SGROUP")

bmi <- get.data(ldb, "D00040")
show.dups(bmi, sic = "SIC", zp = "SGROUP")
bmi <- remove.duplicates(bmi, sic = "SIC", zp = "SGROUP")

noten <- get.data(ldb, "T00152")
show.dups(noten, sic = "SIC", zp = "SGROUP")
noten <- remove.duplicates(noten, sic = "SIC", zp = "SGROUP")

fams <- get.fams(ldb)

schulform <- get.data(ldb, "D00175")
show.dups(schulform, sic = "SIC", zp = "SGROUP")
schulform <- remove.duplicates(schulform, sic = "SIC", zp = "SGROUP")

save(bmi, schulform, fams, freizeit_sb, medien, persdat, ses, noten, file = "20170405_medien_noten.rdata")

##################################################################
####medien umkodieren###########################################
#################################################################

library("dplyr")

medien$T00156_F0009 <- recode(medien$T00156_F0009,
                              `1` = 0, `2` = 0.5, `3` = 1.5, `4` = 3.5, `5` = 5)
medien$T00156_F0010 <- recode(medien$T00156_F0010,
                              `1` = 0, `2` = 0.5, `3` = 1.5, `4` = 3.5, `5` = 5)
medien$T00156_F0011 <- recode(medien$T00156_F0011,
                              `1` = 0, `2` = 0.5, `3` = 1.5, `4` = 3.5, `5` = 5)
medien$T00156_F0012 <- recode(medien$T00156_F0012,
                              `1` = 0, `2` = 0.5, `3` = 1.5, `4` = 3.5, `5` = 5)
medien$T00156_F0013 <- recode(medien$T00156_F0013,
                              `1` = 0, `2` = 0.5, `3` = 1.5, `4` = 3.5, `5` = 5)

medien$medien_all <- medien$T00156_F0009 + medien$T00156_F0010 + medien$T00156_F0011 + medien$T00156_F0012 + medien$T00156_F0013

medien$medien_bild <- medien$T00156_F0009 + medien$T00156_F0010 + medien$T00156_F0011 + medien$T00156_F0013

##################################################################
####freizeit umkodieren###########################################
#################################################################

freizeit_sb$T00159_F0016 <- recode(freizeit_sb$T00159_F0016, 
                                   `0` = 0, `1` = 0.5, `2` = 1.5, `3` = 4, `4` = 6)
freizeit_sb$T00159_F0017 <- recode(freizeit_sb$T00159_F0017, 
                                   `0` = 0, `1` = 0.5, `2` = 1.5, `3` = 4, `4` = 6)
freizeit_sb$frei_sport <- freizeit_sb$T00159_F0016 + freizeit_sb$T00159_F0017  ## sicher? nicht lieber durch 2 teilen?

#####################################################################################################################
#####noten umkodieren#################
######################################


noten$T00152_F0024 <- recode(noten$T00152_F0024,
                             `0` = 6, `1` = 5, `2` = 5, `3` = 5, `4` = 4, `5` = 4, `6` = 4, `7` = 3, `8` = 3, `9` = 3, `10` = 2, `11` = 2, `12` = 2, `13` = 1, `14` = 1, `15` = 1) 
noten$T00152_F0025 <- recode(noten$T00152_F0025,
                             `0` = 6, `1` = 5, `2` = 5, `3` = 5, `4` = 4, `5` = 4, `6` = 4, `7` = 3, `8` = 3, `9` = 3, `10` = 2, `11` = 2, `12` = 2, `13` = 1, `14` = 1, `15` = 1) 
noten$T00152_F0026 <- recode(noten$T00152_F0026,
                             `0` = 6, `1` = 5, `2` = 5, `3` = 5, `4` = 4, `5` = 4, `6` = 4, `7` = 3, `8` = 3, `9` = 3, `10` = 2, `11` = 2, `12` = 2, `13` = 1, `14` = 1, `15` = 1, `16` = 7)
noten$T00152_F0027 <- recode(noten$T00152_F0027,
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

#noten <- subset(noten, T00152_F0022 <6)
#noten <- subset(noten, T00152_F0021 <6)
#noten <- subset(noten, T00152_F0020 <6)

############################################
#bestimmte Schulformen rausschmei?en
###########################

table(schulform$D00175_K_SCHULE)
#noch-nicht-sch?ler und grundsch?ler raus
#nicht-merh-sch?ler raus:
schulform <- subset( schulform, D00175_K_SCHULE > 1 & D00175_K_SCHULE < 8 )
table(schulform$D00175_K_SCHULE)

#gesamtsch?ler und f?rdersch?ler und hauptsch?ler mit realsch?lern verbinden
schulform$D00175_K_SCHULE <- recode(schulform$D00175_K_SCHULE,
                                       `2` = 2, `3` = 2, `4` = 2, `5` = 2, `6` = 1, `7` = 2) 
#schulform als faktor
schulform$D00175_K_SCHULE <- as.factor(schulform$D00175_K_SCHULE)
#1 entspricht ymnasium, 2 entspricht Nicht-Gymnasium

#####################################
#####zusammenf?gen##################
####################################

med_frei <- merge( medien, freizeit_sb[ , names( freizeit_sb ) != "EDAT" ],
                      by = c( "SIC", "SGROUP" ),
                      all.x = T )

med_frei_noten <- merge( med_frei, noten[ , names( noten ) != "EDAT" ],
                              by = c("SIC","SGROUP"),
                              all.x = T)

med_frei_noten_bmi <- merge(med_frei_noten, bmi[ , names( bmi ) != "EDAT" ],
                            by = c("SIC","SGROUP"),
                            all.x = T)

med_frei_noten_bmi_schulform <- merge(med_frei_noten_bmi, schulform[ , names( schulform ) != "EDAT" ],
                            by = c("SIC","SGROUP"),
                            all.x = T)

med_frei_noten_fams <- merge( med_frei_noten_bmi_schulform, fams,
                             by = c("SIC"),
                             all.x = T)

med_frei_noten_fams$EDAT.YEAR <- year( med_frei_noten_fams$EDAT )

ses$EDAT.YEAR <- year( ses$EDAT )

med_frei_ses_noten_bmi <- merge (med_frei_noten_fams, ses[ , names( ses ) != "EDAT" ],
                             by = c("SIC","EDAT.YEAR"),
                             all.x = T)

#######################################
#####f?llebeschr?nkung#################
#######################################

#Alternative: schneidet nach komma ab
med_frei_ses_noten_bmi$age2 <- floor(med_frei_ses_noten_bmi$age)

#beschr?nkung auf 10 bis 17 j?hrige
mfqs_noten <- med_frei_ses_noten_bmi[ med_frei_ses_noten_bmi$age >= 10 & med_frei_ses_noten_bmi$age < 18, ]

summary( mfqs_noten$age )

hist( mfqs_noten$age )

#Variable "Jahr" berechnen
mfqs_noten$year <- as.numeric( substr( mfqs_noten$EDAT, 3, 4 ) )

#beschr?nkung auf ersten besuch
mfqs_noten <- mfqs_noten[ order( mfqs_noten$SIC, mfqs_noten$age ),]
mfqs_1_noten <- mfqs_noten[!duplicated(mfqs_noten$SIC),]

# ich wuerde es so machen, da man nicht weiss, ob duplicated in Zukunft auch den ersten behaelt und alle nachkommenden Zeilen rauswirft 
# mfqs_2_noten <-
#     mfqs_noten %>%
#     group_by( SIC ) %>%
#     mutate( min.age = min( age ) ) %>%
#     ungroup( ) %>%
#     subset( min.age == age )
    
#Geschwister rausschmei?en
sum(duplicated(mfqs_1_noten$D00202_FAM_ID))
mfqs_1_noten <- mfqs_1_noten[!duplicated(mfqs_1_noten$D00202_FAM_ID),]

#Kinder mit vollst?ndigen daten zu T1
mfqs_1_noten_neu <- subset(mfqs_1_noten, D00177_SCORE_FAM >0)   ## verstehe ich nicht SCORE_FAM geht doch erst bei 3 los

summary( mfqs_1_noten$D00177_SCORE_FAM) ## willst Du nicht vll die NA's rausschmeissen?

table(mfqs_1_noten$age2, mfqs_1_noten$D00175_K_SCHULE)

summary(mfqs_1_noten_neu$age)

prop.table(table(mfqs_1_noten_neu$sex))
prop.table(table(mfqs_1_noten_neu$T00152_F0020))
prop.table(table(mfqs_1_noten_neu$T00152_F0021))
prop.table(table(mfqs_1_noten_neu$T00152_F0022))

prop.table(table(mfqs_1_noten_neu$T00156_F0009))
prop.table(table(mfqs_1_noten_neu$T00156_F0010))
prop.table(table(mfqs_1_noten_neu$T00156_F0011))
prop.table(table(mfqs_1_noten_neu$T00156_F0012))
prop.table(table(mfqs_1_noten_neu$T00156_F0013))
prop.table(table(mfqs_1_noten_neu$T00159_F0016))
prop.table(table(mfqs_1_noten_neu$T00159_F0017))

summary(mfqs_1_noten_neu$medien_all)
sd(mfqs_1_noten_neu$medien_all, na.rm=TRUE)
hist(mfqs_1_noten_neu$medien_all)

summary(mfqs_1_noten_neu$medien_bild)
sd(mfqs_1_noten_neu$medien_bild, na.rm=TRUE)
hist(mfqs_1_noten_neu$medien_bild)

summary(mfqs_1_noten_neu$frei_sport)
sd(mfqs_1_noten_neu$frei_sport, na.rm=TRUE)
hist(mfqs_1_noten_neu$frei_sport)

mean(mfqs_1_noten_neu$T00156_F0009, na.rm=TRUE)
sd(mfqs_1_noten_neu$T00156_F0009, na.rm=TRUE)
summary(mfqs_1_noten_neu$T00156_F0009, na.rm=TRUE)

prop.table(table(mfqs_1_noten_neu$D00175_K_SCHULE))

mfqs_1_noten$ses_group <- cut(mfqs_1_noten$D00177_SCORE_FAM, breaks = c(-Inf, 8.99, 14.99, Inf),
                         labels = c("niedrig", "mittel", "hoch"))

prop.table(table(mfqs_1_noten$ses_group))
table(mfqs_1_noten$ses_group)

summary(mfqs_1_noten$D00175_K_SCHULE)

########################

prop.table(table(mfqs_1_noten$T00152_F0020))
prop.table(table(mfqs_1_noten$T00152_F0021))
prop.table(table(mfqs_1_noten$T00152_F0022))

table(mfqs_1_noten$T00152_F0022, mfqs_1_noten$D00175_K_SCHULE)
table(mfqs_1_noten$D00175_K_SCHULE)

#####################################################################################################################
#####zusammenhang medien - altern, geschlecht, ses##
#####################################################

####################################################
#####bez?glich einzelner medien#####################
#####################################################

(lm.tv.ageetc <- lm(mfqs_1_noten$T00156_F0009 ~ mfqs_1_noten$age + mfqs_1_noten$sex + mfqs_1_noten$D00177_SCORE_FAM + mfqs_1_noten$year + mfqs_1_noten$D00175_K_SCHULE))
summary(lm.tv.ageetc)
confint(lm.tv.ageetc, level = 0.95)
#age, ses, schulform

## Bin mir nicht sicher, ob das koscher ist,
table(mfqs_1_noten$T00156_F0010)
## da es so aussieht, als haetten viele Kinder ueberhaupt keine Konsole,
## haetten sie eine, spielten sie wahrscheinlich auch damit.
## moeglicherweise, falls noch nicht geschehen und ueberhaupt moeglich,
## sollte man nur die auswaehlen, die im Besitz einer Konsole sind.

(lm.kons.ageetc <- lm(mfqs_1_noten$T00156_F0010 ~ mfqs_1_noten$age + mfqs_1_noten$sex + mfqs_1_noten$D00177_SCORE_FAM + mfqs_1_noten$year + mfqs_1_noten$D00175_K_SCHULE))
summary(lm.kons.ageetc)
confint(lm.kons.ageetc, level = 0.95)
#Sex, ses, schulform

## Hier scheinen alle Zugang zu einem PC zu haben ##########
table(mfqs_1_noten$T00156_F0011)
############################################################

(lm.pc.ageetc <- lm(mfqs_1_noten$T00156_F0011 ~ mfqs_1_noten$age + mfqs_1_noten$sex + mfqs_1_noten$D00177_SCORE_FAM + mfqs_1_noten$year + mfqs_1_noten$D00175_K_SCHULE))
summary(lm.pc.ageetc)
confint(lm.pc.ageetc, level = 0.95)
#age, sex, ses

## Was immer Beschaeftigen mit Musik bedeutet, es scheinen alle Zugang zu haben ###
table(mfqs_1_noten$T00156_F0012)
###################################################################################

(lm.mus.ageetc <- lm(mfqs_1_noten$T00156_F0012 ~ mfqs_1_noten$age + mfqs_1_noten$sex + mfqs_1_noten$D00177_SCORE_FAM + mfqs_1_noten$year + mfqs_1_noten$D00175_K_SCHULE))
summary(lm.mus.ageetc)
confint(lm.mus.ageetc, level = 0.95)
#age, sex, schulform

## Handy ist dasselbe Problem wie beim PC
table(mfqs_1_noten$T00156_F0013)

(lm.han.ageetc <- lm(mfqs_1_noten$T00156_F0013 ~ mfqs_1_noten$age + mfqs_1_noten$sex + mfqs_1_noten$D00177_SCORE_FAM + mfqs_1_noten$year + mfqs_1_noten$D00175_K_SCHULE))
summary(lm.han.ageetc)
confint(lm.han.ageetc, level = 0.95)
#age, sex, ses, year, schulform

mfqs_2_noten <- 
    mfqs_1_noten[!is.na( mfqs_1_noten$age ) &!is.na( mfqs_1_noten$sex ) &!is.na( mfqs_1_noten$D00177_SCORE_FAM ) &!is.na( mfqs_1_noten$medien_bild ) &!is.na( mfqs_1_noten$frei_sport ) &!is.na( mfqs_1_noten$D00040_BMI_SDS ) &!is.na( mfqs_1_noten$year ) &!is.na( mfqs_1_noten$D00175_K_SCHULE ) , ]

ggplot(mfqs_2_noten, aes( frei_sport, D00175_K_SCHULE ) ) +
    geom_bar( stat = "identity", position = "fill", aes( fill = D00175_K_SCHULE) )
    
cdplot( D00175_K_SCHULE ~ frei_sport, data = mfqs_1_noten)
( xx <- glm(D00175_K_SCHULE ~ age * sex * D00177_SCORE_FAM * year * medien_bild * frei_sport * D00040_BMI_SDS, family = binomial, data = mfqs_1_noten) )
( xx <- glm(D00175_K_SCHULE ~ age + sex + D00177_SCORE_FAM + year + medien_bild + frei_sport + D00040_BMI_SDS, family = binomial, data = mfqs_1_noten) )
summary(xx)


library(ggplot2)

ggplot( mfqs_2_noten, aes( frei_sport, f.v ) ) + 
    geom_point( )

yy <- glm(D00175_K_SCHULE ~ age + sex + D00177_SCORE_FAM + year + D00040_BMI_SDS
          + T00156_F0009
          + T00156_F0010
          + T00156_F0011
          + T00156_F0013
          + T00159_F0016
          + T00159_F0017
          + frei_sport, family = binomial, data = mfqs_1_noten)

summary(yy)

####################################################
#####bez?glich aller medien#####################
#####################################################
(lm.med_all.ses.age.sex <- lm(mfqs_1_noten$medien_all ~ mfqs_1_noten$age + mfqs_1_noten$sex + mfqs_1_noten$D00177_SCORE_FAM + mfqs_1_noten$year + mfqs_1_noten$D00175_K_SCHULE))
summary(lm.med_all.ses.age.sex)
#age, ses, schulform
(lm.med_bild.ses.age.sex <- lm(mfqs_1_noten$medien_bild ~ mfqs_1_noten$age + mfqs_1_noten$sex + mfqs_1_noten$D00177_SCORE_FAM + mfqs_1_noten$year + mfqs_1_noten$D00175_K_SCHULE))
summary(lm.med_all.ses.age.sex)

plot(mfqs_1_noten$D00175_K_SCHULE, mfqs_1_noten$medien_bild)

##########################################
#bez?glich anderer freizeitgruppen########
###########################################

(lm.sport.ageetc <- lm(mfqs_1_noten$frei_sport ~ mfqs_1_noten$age + mfqs_1_noten$sex + mfqs_1_noten$D00177_SCORE_FAM + mfqs_1_noten$year + mfqs_1_noten$D00175_K_SCHULE))
summary(lm.sport.ageetc)
#age, sex, ses, schulform

(lm.sport_verein.ageetc <- lm(mfqs_1_noten$T00159_F0016 ~ mfqs_1_noten$age + mfqs_1_noten$sex + mfqs_1_noten$D00177_SCORE_FAM + mfqs_1_noten$year + mfqs_1_noten$D00175_K_SCHULE))
summary(lm.sport_verein.ageetc)
confint(lm.sport_verein.ageetc, level = 0.95)
#sex, ses, schulform

(lm.sport_ohneverein.ageetc <- lm(mfqs_1_noten$T00159_F0017 ~ mfqs_1_noten$age + mfqs_1_noten$sex + mfqs_1_noten$D00177_SCORE_FAM + mfqs_1_noten$year + mfqs_1_noten$D00175_K_SCHULE))
summary(lm.sport_ohneverein.ageetc)
confint(lm.sport_ohneverein.ageetc, level = 0.95)
#age, sex



####################################
#zusammenh?nge########################
#####################################

#MATHE:
(lm.noten_m.all <- lm(mfqs_1_noten$T00152_F0020 ~ mfqs_1_noten$frei_sport
                      + mfqs_1_noten$medien_all
                      + mfqs_1_noten$age
                      + mfqs_1_noten$sex
                      + mfqs_1_noten$D00177_SCORE_FAM
                      + mfqs_1_noten$year
                      + mfqs_1_noten$D00040_BMI_SDS
                      + mfqs_1_noten$D00175_K_SCHULE))
summary(lm.noten_m.all)
#medien (pos)

#bildschirmmedien
(lm.noten_m_bild.all <- lm(mfqs_1_noten$T00152_F0020 ~ mfqs_1_noten$frei_sport
                      + mfqs_1_noten$medien_bild
                      + mfqs_1_noten$age
                      + mfqs_1_noten$sex
                      + mfqs_1_noten$D00177_SCORE_FAM
                      + mfqs_1_noten$year
                      + mfqs_1_noten$D00040_BMI_SDS
                      + mfqs_1_noten$D00175_K_SCHULE))
summary(lm.noten_m_bild.all)
library(lm.beta)
lm.beta(lm.noten_m_bild.all)
#medien

(lm.noten_m.einzeln <- lm(mfqs_1_noten$T00152_F0020 ~ mfqs_1_noten$T00159_F0016
                          + mfqs_1_noten$T00159_F0017
                          + mfqs_1_noten$T00156_F0009
                          + mfqs_1_noten$T00156_F0010
                          + mfqs_1_noten$T00156_F0011
                          + mfqs_1_noten$T00156_F0013
                          + mfqs_1_noten$age
                          + mfqs_1_noten$sex
                          + mfqs_1_noten$D00177_SCORE_FAM
                          + mfqs_1_noten$year
                          + mfqs_1_noten$D00040_BMI_SDS
                          + mfqs_1_noten$D00175_K_SCHULE))
summary(lm.noten_m.einzeln)
#sign zusammenhang mit mathenote: handy (pos)


#DEUTSCH
(lm.noten_d.all <- lm(mfqs_1_noten$T00152_F0021 ~ mfqs_1_noten$frei_sport
                      + mfqs_1_noten$medien_all
                      + mfqs_1_noten$age
                      + mfqs_1_noten$sex
                      + mfqs_1_noten$D00177_SCORE_FAM
                      + mfqs_1_noten$year
                      + mfqs_1_noten$D00040_BMI_SDS
                      + mfqs_1_noten$D00175_K_SCHULE))
summary(lm.noten_d.all)
#sign zusammenhang mit deutschnote: medien (pos)

(lm.noten_d_bild.all <- lm(mfqs_1_noten$T00152_F0021 ~ mfqs_1_noten$frei_sport
                      + mfqs_1_noten$medien_bild
                      + mfqs_1_noten$age
                      + mfqs_1_noten$sex
                      + mfqs_1_noten$D00177_SCORE_FAM
                      + mfqs_1_noten$year
                      + mfqs_1_noten$D00040_BMI_SDS
                      + mfqs_1_noten$D00175_K_SCHULE))
summary(lm.noten_d_bild.all)
lm.beta(lm.noten_d_bild.all)
#medien


(lm.noten_d.einzeln <- lm(mfqs_1_noten$T00152_F0021 ~ mfqs_1_noten$T00159_F0016
                          + mfqs_1_noten$T00159_F0017
                          + mfqs_1_noten$T00156_F0009
                          + mfqs_1_noten$T00156_F0010
                          + mfqs_1_noten$T00156_F0011
                          + mfqs_1_noten$T00156_F0013
                          + mfqs_1_noten$age
                          + mfqs_1_noten$sex
                          + mfqs_1_noten$D00177_SCORE_FAM
                          + mfqs_1_noten$year
                          + mfqs_1_noten$D00040_BMI_SDS
                          + mfqs_1_noten$D00175_K_SCHULE))
summary(lm.noten_d.einzeln)
#

#SPORT
(lm.noten_s.all <- lm(mfqs_1_noten$T00152_F0022 ~ mfqs_1_noten$frei_sport
                      + mfqs_1_noten$medien_all
                      + mfqs_1_noten$age
                      + mfqs_1_noten$sex
                      + mfqs_1_noten$D00177_SCORE_FAM
                      + mfqs_1_noten$year
                      + mfqs_1_noten$D00040_BMI_SDS
                      + mfqs_1_noten$D00175_K_SCHULE))
summary(lm.noten_s.all)
#sign zusammenhang mit sportnote: sport (neg)

(lm.noten_s_bild.all <- lm(mfqs_1_noten$T00152_F0022 ~ mfqs_1_noten$frei_sport
                      + mfqs_1_noten$medien_bild
                      + mfqs_1_noten$age
                      + mfqs_1_noten$sex
                      + mfqs_1_noten$D00177_SCORE_FAM
                      + mfqs_1_noten$year
                      + mfqs_1_noten$D00040_BMI_SDS
                      + mfqs_1_noten$D00175_K_SCHULE))
summary(lm.noten_s_bild.all)
lm.beta(lm.noten_s_bild.all)
#sport

(lm.noten_s.einzeln <- lm(mfqs_1_noten$T00152_F0022 ~ mfqs_1_noten$T00159_F0016
                          + mfqs_1_noten$T00159_F0017
                          + mfqs_1_noten$T00156_F0009
                          + mfqs_1_noten$T00156_F0010
                          + mfqs_1_noten$T00156_F0011
                          + mfqs_1_noten$T00156_F0013
                          + mfqs_1_noten$age
                          + mfqs_1_noten$sex
                          + mfqs_1_noten$D00177_SCORE_FAM
                          + mfqs_1_noten$year
                          + mfqs_1_noten$D00040_BMI_SDS
                          + mfqs_1_noten$D00175_K_SCHULE))
summary(lm.noten_s.einzeln)
#sign zusammenhang mit sportnote: sport__verein (neg), sport ohne verein (neg), konsole (pos)


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

mfqs_noten1.2_neu <- subset(mfqs_noten1.2, D00177_SCORE_FAM.1 >0)
summary(mfqs_noten1.2_neu$age.1)
table(mfqs_noten1.2_neu$sex)


  
#######################

table(mfqs_noten1.2$T00152_F0020.2)
table(mfqs_noten1.2$T00152_F0021.2)
table(mfqs_noten1.2$T00152_F0022.2)

table(mfqs_noten1.2$T00152_F0023.2, mfqs_noten1.2$D00175_K_SCHULE.1)

#MATHE
(lm.noten_m.all.x <- lm(mfqs_noten1.2$T00152_F0020.2 ~ mfqs_noten1.2$frei_sport.1
                        + mfqs_noten1.2$medien_all.1
                        + mfqs_noten1.2$age.2
                        + mfqs_noten1.2$sex
                        + mfqs_noten1.2$D00177_SCORE_FAM.1
                        + mfqs_noten1.2$T00152_F0020.1
                        + mfqs_noten1.2$year.1
                        + mfqs_noten1.2$D00040_BMI_SDS.1
                        + mfqs_noten1.2$D00175_K_SCHULE.1))
summary(lm.noten_m.all.x)
#medien (pos)

(lm.noten_m.bild.x <- lm(mfqs_noten1.2$T00152_F0020.2 ~ mfqs_noten1.2$frei_sport.1
                        + mfqs_noten1.2$medien_bild.1
                        + mfqs_noten1.2$age.2
                        + mfqs_noten1.2$sex
                        + mfqs_noten1.2$D00177_SCORE_FAM.1
                        + mfqs_noten1.2$T00152_F0020.1
                        + mfqs_noten1.2$year.1
                        + mfqs_noten1.2$D00040_BMI_SDS.1
                        + mfqs_noten1.2$D00175_K_SCHULE.1))
summary(lm.noten_m.bild.x)
lm.beta(lm.noten_m.bild.x)
#medien (pos), 

(lm.noten_m.einzeln.x <- lm(mfqs_noten1.2$T00152_F0020.2 ~ mfqs_noten1.2$T00159_F0016.1
                            + mfqs_noten1.2$T00159_F0017.1
                            + mfqs_noten1.2$T00156_F0009.1
                            + mfqs_noten1.2$T00156_F0010.1
                            + mfqs_noten1.2$T00156_F0011.1
                            + mfqs_noten1.2$T00156_F0013.1
                            + mfqs_noten1.2$age.2
                            + mfqs_noten1.2$sex
                            + mfqs_noten1.2$D00177_SCORE_FAM.1
                            + mfqs_noten1.2$year.1
                            + mfqs_noten1.2$T00152_F0020.1
                            + mfqs_noten1.2$D00040_BMI_SDS.1
                            + mfqs_noten1.2$D00175_K_SCHULE.1))
summary(lm.noten_m.einzeln.x)
#PC (pos)


#DEUTSCH
(lm.noten_d.all.x <- lm(mfqs_noten1.2$T00152_F0021.2 ~ mfqs_noten1.2$frei_sport.1
                        + mfqs_noten1.2$medien_all.1
                        + mfqs_noten1.2$age.2
                        + mfqs_noten1.2$sex
                        + mfqs_noten1.2$D00177_SCORE_FAM.1
                        + mfqs_noten1.2$T00152_F0021.1
                        + mfqs_noten1.2$year.1
                        + mfqs_noten1.2$D00040_BMI_SDS.1
                        + mfqs_noten1.2$D00175_K_SCHULE.1))
summary(lm.noten_d.all.x)
#sign sind (medien)


#bildschirmmedien
(lm.noten_d.bild.x <- lm(mfqs_noten1.2$T00152_F0021.2 ~ mfqs_noten1.2$frei_sport.1
                        + mfqs_noten1.2$medien_bild.1
                        + mfqs_noten1.2$age.2
                        + mfqs_noten1.2$sex
                        + mfqs_noten1.2$D00177_SCORE_FAM.1
                        + mfqs_noten1.2$T00152_F0021.1
                        + mfqs_noten1.2$year.1
                        + mfqs_noten1.2$D00040_BMI_SDS.1
                        + mfqs_noten1.2$D00175_K_SCHULE.1))
summary(lm.noten_d.bild.x)
lm.beta(lm.noten_d.bild.x)
#

(lm.noten_d.einzeln.x <- lm(mfqs_noten1.2$T00152_F0021.2 ~ mfqs_noten1.2$T00159_F0016.1
                            + mfqs_noten1.2$T00159_F0017.1
                            + mfqs_noten1.2$T00156_F0009.1
                            + mfqs_noten1.2$T00156_F0010.1
                            + mfqs_noten1.2$T00156_F0011.1
                            + mfqs_noten1.2$T00156_F0013.1
                            + mfqs_noten1.2$age.2
                            + mfqs_noten1.2$sex
                            + mfqs_noten1.2$D00177_SCORE_FAM.1
                            + mfqs_noten1.2$year.1
                            + mfqs_noten1.2$T00152_F0021.1
                            + mfqs_noten1.2$D00040_BMI_SDS.1
                            + mfqs_noten1.2$D00175_K_SCHULE.1))
summary(lm.noten_d.einzeln.x)
#


#SPORT
(lm.noten_s.all.x <- lm(mfqs_noten1.2$T00152_F0022.2 ~ mfqs_noten1.2$frei_sport.1
                        + mfqs_noten1.2$medien_all.1
                        + mfqs_noten1.2$age.2
                        + mfqs_noten1.2$sex
                        + mfqs_noten1.2$D00177_SCORE_FAM.1
                        + mfqs_noten1.2$T00152_F0022.1
                        + mfqs_noten1.2$year.1
                        + mfqs_noten1.2$D00040_BMI_SDS.1
                        + mfqs_noten1.2$D00175_K_SCHULE.1))
summary(lm.noten_s.all.x)
#sport


#nur bildschirmmedien
(lm.noten_s.bild.x <- lm(mfqs_noten1.2$T00152_F0022.2 ~ mfqs_noten1.2$frei_sport.1
                        + mfqs_noten1.2$medien_bild.1
                        + mfqs_noten1.2$age.2
                        + mfqs_noten1.2$sex
                        + mfqs_noten1.2$D00177_SCORE_FAM.1
                        + mfqs_noten1.2$T00152_F0022.1
                        + mfqs_noten1.2$year.1
                        + mfqs_noten1.2$D00040_BMI_SDS.1
                        + mfqs_noten1.2$D00175_K_SCHULE.1))
summary(lm.noten_s.bild.x)
lm.beta(lm.noten_s.bild.x)
#sport

#einzelne medien
(lm.noten_s.einzeln.x <- lm(mfqs_noten1.2$T00152_F0022.2 ~ mfqs_noten1.2$T00159_F0016.1
                            + mfqs_noten1.2$T00159_F0017.1
                            + mfqs_noten1.2$T00156_F0009.1
                            + mfqs_noten1.2$T00156_F0010.1
                            + mfqs_noten1.2$T00156_F0011.1
                            + mfqs_noten1.2$T00156_F0013.1
                            + mfqs_noten1.2$age.2
                            + mfqs_noten1.2$sex
                            + mfqs_noten1.2$D00177_SCORE_FAM.1
                            + mfqs_noten1.2$year.1
                            + mfqs_noten1.2$T00152_F0022.1
                            + mfqs_noten1.2$D00040_BMI_SDS.1
                            + mfqs_noten1.2$D00175_K_SCHULE.1))
summary(lm.noten_s.einzeln.x)
#(sport mit verein), sport ohne verein, TV 


################################
####umgedreht###################
###############################

#vorhersage medien_all
(lm.medien <- lm(mfqs_noten1.2$medien_all.2 ~ mfqs_noten1.2$T00152_F0020.1
                 + mfqs_noten1.2$T00152_F0021.1
                 + mfqs_noten1.2$T00152_F0022.1
                 + mfqs_noten1.2$age.2
                 + mfqs_noten1.2$sex
                 + mfqs_noten1.2$D00177_SCORE_FAM.1
                 + mfqs_noten1.2$year.1
                 + mfqs_noten1.2$medien_all.1
                 + mfqs_noten1.2$D00040_BMI_SDS.1
                 + mfqs_noten1.2$D00175_K_SCHULE.1))
summary(lm.medien)
#deutschnote, sportnote

#vorhersage bildschirmmedien
(lm.medien_bild <- lm(mfqs_noten1.2$medien_bild.2 ~ mfqs_noten1.2$T00152_F0020.1
                      + mfqs_noten1.2$T00152_F0021.1
                      + mfqs_noten1.2$T00152_F0022.1
                      + mfqs_noten1.2$age.1
                      + mfqs_noten1.2$sex
                      + mfqs_noten1.2$D00177_SCORE_FAM.1
                      + mfqs_noten1.2$year.1
                      + mfqs_noten1.2$medien_bild.1
                      + mfqs_noten1.2$D00040_BMI_SDS.1
                      + mfqs_noten1.2$D00175_K_SCHULE.1))
summary(lm.medien_bild)
lm.beta(lm.medien_bild)
#sportnote



(lm.medien_bild <- lm(mfqs_noten1.2$medien_bild.2 ~ mfqs_noten1.2$T00152_F0022.1))
summary(lm.medien_bild)


(lm.medien_bild <- lm(mfqs_noten1.2$medien_bild.2 ~ mfqs_noten1.2$T00152_F0020.1
                      + mfqs_noten1.2$T00152_F0021.1
                      + mfqs_noten1.2$T00152_F0022.1))
summary(lm.medien_bild)


(lm.medien_bild <- lm(mfqs_noten1.2$medien_bild.2 ~ mfqs_noten1.2$T00152_F0020.1
                      + mfqs_noten1.2$T00152_F0021.1
                      + mfqs_noten1.2$T00152_F0022.1
                      + mfqs_noten1.2$age.1
                      + mfqs_noten1.2$sex
                      + mfqs_noten1.2$D00177_SCORE_FAM.1
                      + mfqs_noten1.2$year.1
                      + mfqs_noten1.2$medien_bild.1
                      + mfqs_noten1.2$D00040_BMI_SDS.1
                      + mfqs_noten1.2$D00175_K_SCHULE.1))
summary(lm.medien_bild)


plot(mfqs_noten1.2$T00152_F0022.1, mfqs_noten1.2$medien_bild.2)

#vorhersage sportliche aktivit?t
(lm.sport <- lm(mfqs_noten1.2$frei_sport.2 ~ mfqs_noten1.2$T00152_F0020.1
                 + mfqs_noten1.2$T00152_F0021.1
                 + mfqs_noten1.2$T00152_F0022.1
                 + mfqs_noten1.2$age.2
                 + mfqs_noten1.2$sex
                 + mfqs_noten1.2$D00177_SCORE_FAM.1
                 + mfqs_noten1.2$year.1
                 + mfqs_noten1.2$frei_sport.1
                 + mfqs_noten1.2$D00040_BMI_SDS.1
                 + mfqs_noten1.2$D00175_K_SCHULE.1))
summary(lm.sport)
lm.beta(lm.sport)
#sportnote 

##############
#korrelationen/stabilit?t
############################


cor.test(mfqs_noten1.2$medien_all.1, mfqs_noten1.2$medien_all.2)
cor.test(mfqs_noten1.2$T00152_F0020.1, mfqs_noten1.2$T00152_F0020.2)
cor.test(mfqs_noten1.2$T00152_F0021.1, mfqs_noten1.2$T00152_F0021.2)
cor.test(mfqs_noten1.2$T00152_F0022.1, mfqs_noten1.2$T00152_F0022.2)

###############Grafiken

library(ggplot2)

summary(mfqs_noten1.2$medien_bild.1)
hist(mfqs_noten1.2$medien_bild.1)
mfqs_noten1.2$medien_bild_group.1 <- cut(mfqs_noten1.2$medien_bild.1, breaks = c(0, 2, 4, 20),
                              labels = c("niedrig", "mittel", "hoch"))


fd <-
  mfqs_noten1.2 %>%
  group_by( medien_bild_group.1 ) %>%
  mutate( m = mean( T00152_F0020.2, na.rm = T ) )

ggplot( fd, aes( medien_bild_group.1, T00152_F0020.2 ) ) +
  geom_boxplot( ) +
  geom_point( aes( medien_bild_group.1, m ), col = "red" )

plot(mfqs_noten1.2$medien_bild_group.1, mfqs_noten1.2$T00152_F0020.2)
summary(mfqs_noten1.2$medien_bild_group.1)

#######################

mfqs_1_noten$T00152_F0020 <- as.factor(mfqs_1_noten$T00152_F0020)
mfqs_1_noten$T00152_F0021 <- as.factor(mfqs_1_noten$T00152_F0021)
mfqs_1_noten$T00152_F0022 <- as.factor(mfqs_1_noten$T00152_F0022)

mfqs_noten1.2$T00152_F0020.2 <- as.factor(mfqs_noten1.2$T00152_F0020.2)
mfqs_noten1.2$T00152_F0021.2 <- as.factor(mfqs_noten1.2$T00152_F0021.2)
mfqs_noten1.2$T00152_F0022.2 <- as.factor(mfqs_noten1.2$T00152_F0022.2)
mfqs_noten1.2$T00152_F0023.2 <- as.factor(mfqs_noten1.2$T00152_F0023.2)

tmp <- mfqs_noten1.2[!is.na(mfqs_noten1.2$T00152_F0020.2) & !is.na(mfqs_noten1.2$medien_bild.1),]

ggplot(tmp, aes(T00152_F0020.2, medien_bild.1)) +
  geom_boxplot()+
  coord_flip()

tmp1 <- mfqs_noten1.2[!is.na(mfqs_noten1.2$T00152_F0022.2) & !is.na(mfqs_noten1.2$frei_sport.1),]
ggplot(tmp1, aes(T00152_F0022.2, frei_sport.1)) +
  geom_boxplot()+
  coord_flip()






plot(mfqs_1_noten$T00152_F0020, mfqs_1_noten$medien_bild)
plot(mfqs_1_noten$T00152_F0021, mfqs_1_noten$medien_bild)
plot(mfqs_1_noten$T00152_F0022, mfqs_1_noten$frei_sport)

table(mfqs_1_noten$T00152_F0021)

plot(mfqs_noten1.2$medien_all.1, mfqs_noten1.2$T00152_F0020.2)
plot(mfqs_noten1.2$medien_all.1, mfqs_noten1.2$T00152_F0021.2)
plot(mfqs_noten1.2$medien_all.1, mfqs_noten1.2$T00152_F0022.2)
plot(mfqs_noten1.2$medien_all.1, mfqs_noten1.2$T00152_F0023.2)

mfqs_noten1.2$T00152_F0020.2 <- as.factor(mfqs_noten1.2$T00152_F0020.2)
mfqs_noten1.2$T00152_F0021.2 <- as.factor(mfqs_noten1.2$T00152_F0021.2)
mfqs_noten1.2$T00152_F0022.2 <- as.factor(mfqs_noten1.2$T00152_F0022.2)
mfqs_noten1.2$T00152_F0023.2 <- as.factor(mfqs_noten1.2$T00152_F0023.2)
plot(mfqs_noten1.2$T00152_F0020.2, mfqs_noten1.2$medien_bild.1)
plot(mfqs_noten1.2$T00152_F0021.2, mfqs_noten1.2$medien_bild.1)
plot(mfqs_noten1.2$T00152_F0022.2, mfqs_noten1.2$frei_sport.1)

#umgedreht

mfqs_noten1.2$T00152_F0020.1 <- as.factor(mfqs_noten1.2$T00152_F0020.1)
mfqs_noten1.2$T00152_F0021.1 <- as.factor(mfqs_noten1.2$T00152_F0021.1)
mfqs_noten1.2$T00152_F0022.1 <- as.factor(mfqs_noten1.2$T00152_F0022.1)
mfqs_noten1.2$T00152_F0023.1 <- as.factor(mfqs_noten1.2$T00152_F0023.1)

plot(mfqs_noten1.2$T00152_F0022.1, mfqs_noten1.2$frei_sport.2)
plot(mfqs_noten1.2$T00152_F0020.1, mfqs_noten1.2$frei_sport.2)
plot(mfqs_noten1.2$T00152_F0022.1, mfqs_noten1.2$medien_bild.2)
