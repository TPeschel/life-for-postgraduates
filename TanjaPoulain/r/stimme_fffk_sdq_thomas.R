##
# thomas
##
rm( list = ls( ) )

# if( !"devtools" %in% rownames( installed.packages( ) ) ) install.packages( "devtools" )
# devtools::install_github( "TPeschel/hlpr4life" )
##
# Hilfe: https://tpeschel.github.io/hlpr4life/
##

hlpr4life::load.pkgs( c( "hlpr4life", "lifecuration", "db.access", "ggplot2", "effects", "Hmisc" ) )

overview <-
    function( df, lhs, rhs, methods = c( "pearson", "spearman" ), sexes = c( "female", "male" ) ) {
        Reduce(
            dplyr::bind_rows,
            lapply(
                methods,
                function( tp ) {
                    Reduce( 
                        dplyr::bind_rows, 
                        lapply( 
                            sexes,
                            function( s ) {
                                Reduce( 
                                    dplyr::bind_rows, 
                                    lapply( 
                                        lhs,
                                        function ( l ) {
                                            Reduce( 
                                                dplyr::bind_rows, 
                                                lapply(
                                                    rhs, 
                                                    function ( r ) {
                                                        t <- unlist( Hmisc::rcorr( unlist( df[ df$sex == s, l ] ), unlist( df[ df$sex == s, r ] ), type = tp ) )
                                                        d <- data.frame( sex = s, x = l, y = r, corr = t[ 2 ], n = t[ 6 ], p = t[ 10 ], significance = ifelse( t[ 10 ] <= .05, "yes", "no" ), type = tp )
                                                        }
                                                    )
                                            )
                                            }
                                        )
                                )
                                }
                            )
                    )
                    }
                )
        )
    }


# source( "~/LIFE/.secret.R" )
# ldb <- db.open( user = Sys.getenv( "DB_USER" ), password = Sys.getenv( "DB_PASS" ) )
###
# samoht
##


# source("H:/R/connectionWIN.r")
#verbindung mit lifedatnbank (?ber mandys account)

# library(lifecuration)
#l?dte pakete, die fur datenbank geschrieben wurden

persdat <- get.persdat(ldb)
#holt datei "persdat" (infos zu geschlecht, alter, geburtstag) aus der datenbank (ldb) und nennt sie persdat

stimme <- get.data(ldb, "T00865")
stimme <- add.persdat.age(persdat, stimme)
show.dups(stimme, sic = "SIC", zp = "SGROUP")
stimme <- remove.duplicates( stimme, sic = "SIC", zp = "SGROUP" )
table( stimme$SGROUP )

pers <- get.data( ldb, "D00154" )
show.dups(pers, sic = "SIC", zp = "SGROUP")
pers <- remove.duplicates(pers, sic = "SIC", zp = "SGROUP")
table(pers$SGROUP)

sdq <- get.data(ldb, "D00149")
show.dups(sdq, sic = "SIC", zp = "SGROUP")
sdq <- remove.duplicates(sdq, sic = "SIC", zp = "SGROUP")
table(sdq$SGROUP)

ses <- get.data(ldb, "D00177")
show.dups(ses, sic = "SIC", zp = "SGROUP")
ses <- remove.duplicates(ses, sic = "SIC", zp = "SGROUP")

bmi <- get.data(ldb, "D00040")
show.dups(bmi, sic = "SIC", zp = "SGROUP")
bmi <- remove.duplicates(bmi, sic = "SIC", zp = "SGROUP")

fams <- get.fams(ldb)

##
# thomas
##
# db.close( ldb )
###
# samoht
##

############################################################################
#zusammenfuegen##############
###############################

stimme_pers <- merge( stimme, pers, by = c("SIC","SGROUP" ) )

table.df( stimme_pers, F )

stimme_pers[ 30 < abs( as.numeric( difftime( stimme_pers$EDAT.x, stimme_pers$EDAT.y, units = "days" ) ) ), c( "SIC", "SGROUP", "EDAT.x", "EDAT.y", setdiff( names( stimme_pers ), c( "SIC", "SGROUP", "EDAT.x", "EDAT.y" ) ) ) ]

stimme_pers <- rename.columns( stimme_pers, c( "EDAT.x", "EDAT.y" ), c( "EDAT.T865", "EDAT.D154" ) )

stimme_pers_sdq <- merge( stimme_pers, sdq, by = c( "SIC", "SGROUP" ) )

table.df( stimme_pers_sdq, F )

stimme_pers_sdq_fams <- merge( stimme_pers_sdq, fams, by = c( "SIC" ), all.x = T)

stimme_pers_sdq_fams <- rename.columns( stimme_pers_sdq_fams, c( "SGROUP.x", "SGROUP.y" ), c( "SGROUP", "SGROUP.FAM") )

stimme_pers_sdq_fams$EDAT.YEAR <- year( stimme_pers_sdq_fams$EDAT )

ses$EDAT.YEAR <- year( ses$EDAT )

table.df( stimme_pers_sdq_fams, F )

stimme_pers_sdq_fams_ses <- merge( stimme_pers_sdq_fams, ses, by = c( "SIC", "EDAT.YEAR" ), all.x = T )

table.df( stimme_pers_sdq_fams_ses, F )

stimme_pers_sdq_fams_ses <- rename.columns( stimme_pers_sdq_fams_ses, c( "SGROUP.x", "SGROUP.y", "EDAT.x", "EDAT.y" ), c( "SGROUP", "SGROUP.SES", "EDAT", "EDAT.SES" ) )

table.df( stimme_pers_sdq_fams_ses, F )

##############################################################################
######nur erstbesuch#########
############################

xxx <-
    stimme_pers_sdq_fams_ses %>%
    group_by( SIC ) %>%
    arrange( age ) %>%
    mutate( num.of.visit = dense_rank( age ), last.visit = n( ), rand.visit = round( runif( 1, min = 1, max = last.visit ) ) )
    
xxx.first.visit <- xxx[ xxx$num.of.visit == 1, ]
xxx.last.visit  <- xxx[ xxx$num.of.visit == xxx$last.visit, ]
xxx.rand.visit  <- xxx[ xxx$num.of.visit == xxx$rand.visit, ]

xxx_1 <- xxx.first.visit

# Das verstehe ich nicht
# xxx <- stimme_pers_sdq_fams_ses[order(stimme_pers_sdq_fams_ses$SIC, stimme_pers_sdq_fams_ses$age),]
# xxx_1 <- stimme_pers_sdq_fams_ses[!duplicated(stimme_pers_sdq_fams_ses$SIC),]

##################################################################################
#####deskriptiv##############
##############################

summary( xxx_1$age )
table( xxx_1$sex )
hist( xxx_1$age )

#####################################################################################
####altersbeschraenkung########
##############################

#7 bis 14
#xxx_1$age2 <- floor( xxx_1$age )
xxx_1 <- xxx_1[ between( xxx_1$age, 7.0000, 13.9999 ), ]

##
# thomas
# hier vielleicht lieber Facktoren bauen
##
xxx_1$age.cat <- cut( xxx_1$age, c( 7 : 14 ), c( 7 : 13 ) )
xxx_1[ , c( "age", "age.cat" ) ]
##
# samoht
##

################################################################################
#####datenvolumen##########
###########################
##
# alles auf einen blick
##
table.df( xxx_1, F, T )
##
# wenn du dich nur fuer die missings interessierst
##
sum.na( xxx_1 )
##
# wenn du dich nur fuer die availables interessierst
##
sum.av( xxx_1 )

summary( xxx_1$D00154_EXTRAV )
summary( xxx_1$D00154_EMOTIO_ST )
summary( xxx_1$D00154_VERTRAEGL )
summary( xxx_1$D00154_GEWISSENH )
summary( xxx_1$D00154_KULTUR )
#keine fehlwerte beim fffk

summary( xxx_1$D00149_PRO_SUM ) 
summary( xxx_1$D00149_EMO_SUM )
summary( xxx_1$D00149_HYP_SUM )
summary( xxx_1$D00149_SOP_SUM )
summary( xxx_1$D00149_VER_SUM )
#keine fehlwerte beim sdq

summary( xxx_1$D00177_SCORE_FAM )
#31 fehlwerte

summary( xxx_1$T00865_F0015 )
summary( xxx_1$T00865_F0016 )
summary( xxx_1$T00865_F0017 )
summary( xxx_1$T00865_F0018 )
summary( xxx_1$T00865_F0019 )
#kaum fehlwerte

#ausschluss der kinder im stimmbruch
xxx_1_neu <- xxx_1[ xxx_1$T00865_F0016  >=  170.00, ]


summary( xxx_1_neu$T00865_F0016 )
#842 kinder bleiben ?brig

summary( xxx_1$T00865_F0020 )
summary( xxx_1$T00865_F0021 )
summary( xxx_1$T00865_F0022 )
summary( xxx_1$T00865_F0023 )
summary( xxx_1$T00865_F0024 )
#kaum fehlwerte

#############################################
#verteilungen###############
############################

hist(xxx_1_neu$T00865_F0015[between(xxx_1_neu$age, 10, 13)])
hist(xxx_1_neu$T00865_F0016)
hist(xxx_1_neu$T00865_F0017,breaks = 30)
hist(xxx_1_neu$T00865_F0018)
hist(xxx_1_neu$T00865_F0019)
#frequenzen sind normalverteilt!

hist(xxx_1_neu$T00865_F0020)
hist(xxx_1_neu$T00865_F0021)
hist(xxx_1_neu$T00865_F0022)
hist(xxx_1_neu$T00865_F0023)
hist(xxx_1_neu$T00865_F0024)
#lautstaerke normalverteilt, nur einzelne ausrei?er

hist(xxx_1_neu$T00865_F0025)
#phonation time normalverteilt, nur ausrei?er

hist(log(xxx_1_neu$T00865_F0026))
#Jitter rechtsschief bzw. Ausrei?er, die richtig weit weg sind
summary(xxx_1_neu$T00865_F0026)

hist(xxx_1_neu$D00149_EMO_SUM)
hist(xxx_1_neu$D00149_PRO_SUM)
hist(xxx_1_neu$D00149_HYP_SUM)
hist(xxx_1_neu$D00149_VER_SUM)
hist(xxx_1_neu$D00149_SOP_SUM)


#source( "overview.R" )

lhs <- c( "age", "T00865_F0007",  "D00149_EMO_SUM" )
rhs <- c( "D00149_GES_SCORE", "D00177_SCORE_FAM" )

overview( xxx_1, lhs, rhs )

plot(xxx_1_neu$D00149_EMO_SUM[xxx_1_neu$sex == "female"], xxx_1_neu$T00865_F0016[xxx_1_neu$sex == "female"])

table.df( xxx_1_neu, F )

ggplot( xxx_1_neu,aes(age,T00865_F0016,col=as.factor(D00149_EMO_SUM)))+geom_point()+facet_grid(as.factor(D00149_EMO_SUM)~sex)

##################################
##ses#############################
#################################

xxx_1_neu$ses_group <- cut(xxx_1_neu$D00177_SCORE_FAM, breaks = c(-Inf, 8.99, 14.99, Inf),
                           labels = c("niedrig", "mittel", "hoch"))
prop.table(table(xxx_1_neu$ses_group, round( xxx_1_neu$D00177_SCORE_FAM ) ) )

####################################
#zusammenh?nge sdq-fffk##############
####################################

(extraversion <- lm(D00154_EXTRAV ~  D00149_EMO_SUM + D00149_HYP_SUM + D00149_VER_SUM 
                          + D00149_SOP_SUM + D00149_PRO_SUM, data = xxx_1_neu))
summary(extraversion)


(emot_stabil <- lm(D00154_EMOTIO_ST ~  D00149_EMO_SUM + D00149_HYP_SUM + D00149_VER_SUM 
                    + D00149_SOP_SUM + D00149_PRO_SUM, data = xxx_1_neu))
summary(emot_stabil)

(gewissenhaftigkeit <- lm(D00154_GEWISSENH ~  D00149_EMO_SUM + D00149_HYP_SUM + D00149_VER_SUM 
                   + D00149_SOP_SUM + D00149_PRO_SUM, data = xxx_1_neu))
summary(gewissenhaftigkeit)

(vertraeglichkeit <- lm(D00154_VERTRAEGL ~  D00149_EMO_SUM + D00149_HYP_SUM + D00149_VER_SUM 
                          + D00149_SOP_SUM + D00149_PRO_SUM, data = xxx_1_neu))
summary(vertraeglichkeit)

(kultur <- lm(D00154_KULTUR ~  D00149_EMO_SUM + D00149_HYP_SUM + D00149_VER_SUM 
                        + D00149_SOP_SUM + D00149_PRO_SUM, data = xxx_1_neu))
summary(kultur)
#insgesamt sehr hohe inter-korrelationen


#####################################
####messungen###############
#############################

###################################
#####lautstaerke#############
##########################

#fluesternde stimme

(sprechstimme_leise <- lm(T00865_F0020 ~ D00154_EXTRAV + D00154_EMOTIO_ST
                          + D00154_VERTRAEGL + D00154_GEWISSENH + D00154_KULTUR
                          + D00149_EMO_SUM + D00149_HYP_SUM + D00149_VER_SUM 
                          + D00149_SOP_SUM + D00149_PRO_SUM
                          + D00177_SCORE_FAM + age + sex, data = xxx_1_neu))

vif( sprechstimme_leise )
summary(sprechstimme_leise)
#extraversion (pos***), hyperaktiv (pos)**, soz_prob (pos)*, age (neg)*
plot(allEffects(sprechstimme_leise))
( xxx_1_neu$T00865_F0020 )
#normale sprechstimme

(sprechstimme_normal <- lm(T00865_F0021 ~ D00154_EXTRAV + D00154_EMOTIO_ST
                           + D00154_VERTRAEGL + D00154_GEWISSENH + D00154_KULTUR
                           + D00149_EMO_SUM + D00149_HYP_SUM + D00149_VER_SUM 
                           + D00149_SOP_SUM + D00149_PRO_SUM
                           + D00177_SCORE_FAM + age + sex, data = xxx_1_neu))
summary(sprechstimme_normal)
#extraversion (pos**), soz_prob (pos)**
library(effects)
plot(allEffects(sprechstimme_normal))

#laute stimme

(sprechstimme_laut <- lm(T00865_F0022 ~ D00154_EXTRAV + D00154_EMOTIO_ST
                         + D00154_VERTRAEGL + D00154_GEWISSENH + D00154_KULTUR
                         + D00149_EMO_SUM + D00149_HYP_SUM + D00149_VER_SUM 
                         + D00149_SOP_SUM + D00149_PRO_SUM
                         + D00177_SCORE_FAM + age + sex, data = xxx_1_neu))
summary(sprechstimme_laut)
#extraversion (pos***), soz_prob (pos)***, sex (boys > girls)**
plot(allEffects(sprechstimme_laut))

#rufende stimme

(sprechstimme_ruf <- lm(T00865_F0023 ~ D00154_EXTRAV* + D00154_EMOTIO_ST
                        + D00154_VERTRAEGL + D00154_GEWISSENH + D00154_KULTUR
                        + D00149_EMO_SUM + D00149_HYP_SUM + D00149_VER_SUM 
                        + D00149_SOP_SUM + D00149_PRO_SUM
                        + D00177_SCORE_FAM + age + sex, data = xxx_1_neu))
summary(sprechstimme_ruf)
#extraversion (pos***), emot_stab (neg**), gewissen (neg)*, kultur (pos*), hyp (neg!)*, soz_prob (pos)*, ses (pos***), age (pos**), sex (boys>girls***)
plot(allEffects(sprechstimme_ruf))
library(car)

sqrt( vif( sprechstimme_laut ) )

#mittel aus allen messungen

xxx_1_neu$average_laut <- (xxx_1_neu$T00865_F0020 + xxx_1_neu$T00865_F0021 + xxx_1_neu$T00865_F0022 + xxx_1_neu$T00865_F0023)/4

(sprechstimme_average_laut <- lm(average_laut ~ D00154_EXTRAV + D00154_EMOTIO_ST
                                 + D00154_VERTRAEGL + D00154_GEWISSENH + D00154_KULTUR
                                 + D00149_EMO_SUM + D00149_HYP_SUM + D00149_VER_SUM 
                                 + D00149_SOP_SUM + D00149_PRO_SUM
                                 + D00177_SCORE_FAM + age + sex, data = xxx_1_neu))
summary(sprechstimme_average_laut)
#extraversion (pos***), soz_propb (pos)***, ses (pos)**, sex (boys>girls***)
plot(allEffects(sprechstimme_average_laut))

##################################################
########frequenz#######################
######################################

#fluesternde stimme

(sprechstimme_leise_fr <- lm(T00865_F0015 ~ D00154_EXTRAV + D00154_EMOTIO_ST
                             + D00154_VERTRAEGL + D00154_GEWISSENH + D00154_KULTUR
                             + D00149_EMO_SUM + D00149_HYP_SUM + D00149_VER_SUM 
                             + D00149_SOP_SUM + D00149_PRO_SUM
                             + D00177_SCORE_FAM + age + sex, data = xxx_1_neu))
summary(sprechstimme_leise_fr)
#emot_probleme (pos)*, age (neg***), sex (girls > boys)***
plot(allEffects(sprechstimme_leise_fr))

#normale sprechstimme

(sprechstimme_normal_fr <- lm(T00865_F0016 ~ D00154_EXTRAV + D00154_EMOTIO_ST
                              + D00154_VERTRAEGL + D00154_GEWISSENH + D00154_KULTUR
                              + D00149_EMO_SUM + D00149_HYP_SUM + D00149_VER_SUM 
                              + D00149_SOP_SUM + D00149_PRO_SUM
                              + D00177_SCORE_FAM + age + sex, data = xxx_1_neu))
summary(sprechstimme_normal_fr)
#soz_probleme (pos)*, age (neg***), sex (girls > boys)***
library(effects)
plot(allEffects(sprechstimme_normal_fr))

#laute stimme

(sprechstimme_laut_fr <- lm(T00865_F0017 ~ D00154_EXTRAV + D00154_EMOTIO_ST
                            + D00154_VERTRAEGL + D00154_GEWISSENH + D00154_KULTUR
                            + D00149_EMO_SUM + D00149_HYP_SUM + D00149_VER_SUM 
                            + D00149_SOP_SUM + D00149_PRO_SUM
                            + D00177_SCORE_FAM + age + sex, data = xxx_1_neu))
summary(sprechstimme_laut_fr)
#extraversion (pos***), age (neg)***
plot(allEffects(sprechstimme_laut_fr))

#rufende stimme

(sprechstimme_ruf_fr <- lm(T00865_F0018 ~ D00154_EXTRAV + D00154_EMOTIO_ST
                           + D00154_VERTRAEGL + D00154_GEWISSENH + D00154_KULTUR
                           + D00149_EMO_SUM + D00149_HYP_SUM + D00149_VER_SUM 
                           + D00149_SOP_SUM + D00149_PRO_SUM
                           + D00177_SCORE_FAM + age + sex, data = xxx_1_neu))
summary(sprechstimme_ruf_fr)
#ns
plot(allEffects(sprechstimme_ruf_fr))


#mittel aus allen messungen

xxx_1_neu$average_fr <- (xxx_1_neu$T00865_F0015 + xxx_1_neu$T00865_F0016 + xxx_1_neu$T00865_F0017 + xxx_1_neu$T00865_F0018)/4

(sprechstimme_average_fr <- lm(average_fr ~ D00154_EXTRAV + D00154_EMOTIO_ST
                               + D00154_VERTRAEGL + D00154_GEWISSENH + D00154_KULTUR
                               + D00149_EMO_SUM + D00149_HYP_SUM + D00149_VER_SUM 
                               + D00149_SOP_SUM + D00149_PRO_SUM
                               + D00177_SCORE_FAM + age + sex, data = xxx_1_neu))
summary(sprechstimme_average_fr)
#extraversion (pos)*, age (neg)***, sex (girls < boys)*
plot(allEffects(sprechstimme_average_fr))

################################################
#####max.Tonhalten#################
##################################

(MPT <- lm(T00865_F0025 ~ D00154_EXTRAV + D00154_EMOTIO_ST
           + D00154_VERTRAEGL + D00154_GEWISSENH + D00154_KULTUR
           + D00149_EMO_SUM + D00149_HYP_SUM + D00149_VER_SUM 
           + D00149_SOP_SUM + D00149_PRO_SUM
           + D00177_SCORE_FAM + age + sex, data = xxx_1_neu))
summary(MPT)
#Kultur (pos)**, age (pos)***, sex (boys > girls)**
plot(allEffects(MPT))

##########################################
######Jitter####################
###################################

(Jitter <- lm(T00865_F0026 ~ D00154_EXTRAV + D00154_EMOTIO_ST
              + D00154_VERTRAEGL + D00154_GEWISSENH + D00154_KULTUR
              + D00149_EMO_SUM + D00149_HYP_SUM + D00149_VER_SUM 
              + D00149_SOP_SUM + D00149_PRO_SUM
              + D00177_SCORE_FAM + age + sex, data = xxx_1_neu))
summary(Jitter)
#ses (pos)*
plot(allEffects(Jitter))









