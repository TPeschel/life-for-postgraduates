# Untersuchng der SDQ Werte 

rm( list = ls( ) )

# Alternativ: Speichern als R.data- file
# save( tbl.thyroid.complete2508, file= "GesamtJoin_Bereinigt2908.Rd" )

# Methode zum Speichern als Excel- Tabelle

# openxlsx::write.xlsx(x = tbl.thyroid.complete2508, file = "GesamtJoin_Bereinigt2908A.xlsx" )

#Verzeichniswahl

#setwd("~/Documents/Schule und Uni/Uni/Doktorarbeit/DieArbeitII/LIfE/Eigen Erstelltes/")
setwd("~/LIFE/life-for-postgraduates/HansSurup/data/")
dir()
# Laden des R.datafiles

load("GesamtJoin_Bereinigt2908.Rd")

# Laden des Paketverbundes

hlpr4life::load.pkgs(
  c( "hlpr4life", "ggplot2", "readxl", "openxlsx", "xlsx", "dplyr"))

# Laden der persönlichen ggplot- Einstellungen

my.histo.theme <-
  list(
    theme_bw(),
    scale_color_manual( "SEX", values = c( "red", "blue" ) ),
    scale_fill_manual( "SEX", values = c( "red", "blue" ) ),
    facet_grid( .  ~  SEX ))

at <- tbl.thyroid.complete2508


# Namensänderung

at <-
  rename.columns(at, c("C_ANTHRO_KH_AGE", "C_ANTHRO_KH_EDAT", "C_ANTHRO_KH_GRP", "TEILNEHMER_SIC", "TEILNEHMER_GESCHLECHT",
                     "TEILNEHMER_GEB_JJJJMM"), c("AGE", "EDAT", "SCIGROUP", "SIC", "SEX", "BIRTH"))

# Änderung der Kodierung für Sex

at$SEX <- c( "male", "female")[ match( at$SEX, c( 1, 2 ) ) ]

# Rundung von Kommawerten bei dem Hyperaktivitätssummenscore

at$E_SDQ_HYP_SUM <- round(at$E_SDQ_HYP_SUM)

# Erstellung eines Faktors

at$E_SDQ_HYP_SUM_FACT <- as.factor(at$E_SDQ_HYP_SUM)

table(at$E_SDQ_HYP_SUM)

# Laden des persönlichen ggplots mit Faktor

ggplot( at) + geom_histogram( aes( E_SDQ_HYP_SUM_FACT, fill = SEX ), stat = "count") +
  xlab( "Punktwert der SDQ- Hyperaktivitätssubskala" ) +
  ylab( "Häufigkeit des Punktwerteswertes" ) +
  my.histo.theme

# Zum Speichern immer diesen Befehl nutzen!

setwd("/Users/Hans/Documents/Schule und Uni/Uni/Doktorarbeit/DieArbeitII/LIfE/Grafiken//")

ggsave("SDQ_HYP_Countbig.png",width = 6, height=5) # desto größer das Bild, desto kleiner die Schrift

ggsave("R/Grafiken/SDQ_HYP_Countbig.png",width = 12, height=10) # gleiche Formate beibehalten

# Ausschluss der nicht vorhandenen Punktwerte zur Verhältnisauswahl

at1 <- at[ !is.na( at$E_SDQ_HYP_SUM ), ]

# Verhältnisberechnung

paste0(round(sum(at1$E_SDQ_HYP_SUM[at1$SEX=="female"] < 6) / length(at1$E_SDQ_HYP_SUM[at1$SEX=="female"]),6)*100,"%")

paste0(round(sum(at1$E_SDQ_HYP_SUM[at1$SEX=="female"] > 6) / length(at1$E_SDQ_HYP_SUM[at1$SEX=="female"]),4)*100,"%")


paste0(round(sum(at1$E_SDQ_HYP_SUM[at1$SEX=="male"] < 6) / length(at1$E_SDQ_HYP_SUM[at1$SEX=="male"]),6)*100,"%")

paste0(round(sum(at1$E_SDQ_HYP_SUM[at1$SEX=="male"] > 6) / length(at1$E_SDQ_HYP_SUM[at1$SEX=="male"]),6)*100,"%")













