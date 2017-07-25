    
    ##Struktur zeigt den Datensatz aufgelistet
    str(daten)
    ##in Tabellenform wird jede Spalte dargestellt
    ## Für mich:Werte damit auflisten, aber nach sex getrennt
    head(daten)
    
    ##Fehlwerte zählen 
    colSums(is.na(daten)) ##warum so viele NA?
    
    ##wieviel NA (missing values) von FSH num. values fuer jedes Geschlecht
    table(daten$sex[is.na(daten$FSH_S_NUM_VALUE)])
    
    ##Boxplot: LH ~ Alter, ylab=Achsenbezeichnung
    boxplot(LH_S_NUM_VALUE ~ C_ANTHRO_KH_AGE, data=daten, ylab="LH (IU/L)")
    ## wie kann ich LH~Alter&Geschlecht eingeben??
    
    ##Scatterplot LH~Alter, PG~Alter
    scatter.smooth(daten$C_ANTHRO_KH_AGE, daten$LH_S_NUM_VALUE)
    scatter.smooth(daten$C_ANTHRO_KH_AGE, PG$C_PUB_STAT_PUB_STATUS) ##geht nicht...
    
    ##Winkler
    ##S?ulendiagramm Häufigkeit des sozStat (Geamtscore der Familie)
    Winkler$sozstat <- cut(Winkler$C_SOZ_WI_SOZIO_FAM, breaks = c(3, 8, 14, 21))
    ##Winkler, Histogramm(teilt automatisch ein)
    hist(Winkler$C_SOZ_WI_SOZIO_FAM, breaks=0:22, col="darkseagreen3", main="")
    table(Winkler$sozstat)
    
    ##Barplot in 3 Winklerkategorien
    barplot(table(Winkler$sozstat))
    
    ##ggplot2=paket f?r Graphiken ,aes=welche Variable woher
    library(ggplot2)
    ggplot(Winkler, aes(x = sozstat)) +
      geom_bar()
    
    ##Winkler nach A2 und B1 
    Winkler$koh <- substr(Winkler$C_SOZ_WI_GRUPPE,1,2)
    
    ggplot(Winkler, aes(x = koh)) +
      geom_bar()
    
    ggplot(Winkler, aes(x = sozstat)) +
      geom_bar() +
      facet_wrap( ~ koh, nrow = 3)
    
    ##kohorte mit anteiligem sozstat
    ggplot(Winkler, aes(x = koh, fill = sozstat)) +
      geom_bar()
    
    ##nach Prozent: von A2 snd x% niedriger sozStat
    ggplot(Winkler, aes(x = koh, fill = sozstat)) +
      geom_bar(position = position_fill())
    
    ##andersrum: z.B. niedriger sozstat: 20% sind B1
    ggplot(Winkler, aes(fill = koh, x = sozstat)) +
      geom_bar(position = position_fill())
    
    ## altersgruppen
    daten$ag <- cut(daten$C_ANTHRO_KH_AGE, breaks = c(5.5,12,16, Inf))
    
    table(!is.na(daten$LH_S_NUM_VALUE), daten$ag)
    
    ##Histogramm: LH werte~Alter und Prozent der Gesamtheit --> 3 Paramter!!
    histogram( ~ daten$LH_S_NUM_VALUE | daten$sex + daten$ag, data = daten)
    
    ##histogramme nach sex und altersgruppen: LH
    ggplot(daten, aes(x = LH_S_NUM_VALUE)) +
      geom_histogram() +
      facet_wrap(~ ag + sex , ncol = 2, scales = "free")
    
    ##boxplot LH nach alter und sex!!
    ggplot(daten, aes(x = factor(floor(C_ANTHRO_KH_AGE)), y = LH_S_NUM_VALUE, colour = sex)) +
      geom_boxplot()
    
    ##tapply funktion f?r bedingte Anwendung n?tig?
    tapply(daten$LH_S_NUM_VALUE, daten$C_ANTHRO_KH_AGE, daten$sex)
    ##Mittelwert berechnen jeder Spezies
    tapply(daten$LH_S_NUM_VALUE, daten$C_ANTHRO_KH_AGE, mean)## warum f?r jedes Alter nur NA als mean angegeben
    
    ## lineare regression
    ##lineare regression Scatterplot
    scatter.smooth(daten$C_ANTHRO_KH_AGE, daten$LH_S_NUM_VALUE)
    abline(hd_lm_1, col="red") 
    ##wie kann ich die Achseneinteilung ?ndern und nach geschlecht aufteilen?
    
