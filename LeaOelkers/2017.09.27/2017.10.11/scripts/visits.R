rm( list = ls( ) )

hlpr4life::load.pkgs(
    c(
        "hlpr4life",
        "dplyr",
        "ggplot2" ) )

# setze Zeitzone auf Berlin Mean Time
Sys.setenv( TZ = "Europe/Berlin" ) 

##
# setze Pfad zu aktuellem R-Arbeitsverzeichnis, Pfad zum Laden
setwd( "~/LIFE/life-for-postgraduates/LeaOelkers/2017.09.27/data/generated/" )
# setwd( "c:/Users/Lea/Desktop/AllesNeu20170725/data/generated/" )  
dir( )

load( "main.table.curated.2017-10-18.Rd" )  #3058

range( tbl$AGE, na.rm = T )


##wir erzeugen 3 tables: Erst-, Letzt- und Zufallsbesuch--> dann daruas plots erzeugen und Verteilung angucken
##Gruppierte Tbl (nach SIC) in mutate geschickt, dort neue Spalten erzeugen (hier:v=visit (1-4), s=Gr??e der Gruppe/ANzahl der Besuche, r=ein zufallswert
## von Besuch 1-4). Nur f?r diesen nplot wird eine neue tabelle "tb" erzeugt


tb <-
    tbl %>% group_by( SIC ) %>%  mutate( v = dense_rank( EDAT ), s = n( ), r = sample( v, 1 ) )  ##3Spalten:(v=1-5), s(=summe also 5), r=Zufallszahl von 1-4

#Besuchsverteilung
table(tb$v, tb$SEX) ##z.b. 453 M?dchen aren 2x da, sind aber bei 1 auch aufgef?hrt, deshalb:
tb.v <- tb%>%group_by(SIC,SEX)%>%summarise(visits = n())

table( tb.v$visits,tb.v$SEX)

tb$AGE.CAT1<-   ##AGE.CATs erzeugen an tb (eigentlich erst in description, dann aber an tbl)
  cut(tb$AGE+0.001,
      breaks = seq(4,18, by =1),   
      labels = seq(4,17, by=1))

#View(tb[, c("AGE", "AGE.CAT1")])

#jetzt filtern:
tb.fst <-
    tb[ tb$v == 1, ] ##nur Erstbesuch

tb.lst <-
    tb[ tb$v == tb$s, ] ##last besuch

tb.rnd <-
    tb[ tb$v == tb$r, ] ##Zufallsbesuch


ggsubplot(
    ggplot( tb ) + facet_grid( . ~ SEX ) + ylim( 0, 180 ) +
      theme_bw()+
        geom_histogram( aes( AGE.CAT1, fill = as.factor( v ) ), stat = "count" ) +
        geom_hline( yintercept = 50 )+
        ggtitle("all visits"),
    ggplot( tb.fst ) + facet_grid( . ~ SEX ) + ylim( 0, 180 ) +
      theme_bw()+
      geom_histogram( aes( AGE.CAT1, fill = as.factor( v ) ), stat = "count" ) +
        geom_hline( yintercept = 50 )+
       ggtitle("first visit"),
    ggplot( tb.rnd ) + facet_grid( . ~ SEX ) + ylim( 0, 180 ) +
      theme_bw()+
      geom_histogram( aes( AGE.CAT1, fill = as.factor( v ) ), stat = "count" ) +
        geom_hline( yintercept = 50 )+
        ggtitle("Zufall"),
    ggplot( tb.lst ) + facet_grid( . ~ SEX ) + ylim( 0, 180 ) +
      theme_bw()+
      geom_histogram( aes( AGE.CAT1, fill = as.factor( v ) ), stat = "count" ) +
        geom_hline( yintercept = 50 )+
        ggtitle("last visit"),
    cols = 1 )
##--> beim last visit-Besuch scheint die Verteilung am besten zu sein

##F?r ausgew?hlte Verteilung bestimmte Merkmale betrachen, um zu gucken, ob die Verteilung Sinn macht

sum(!is.na(tb.lst$C_PUB_STAT_MENARCHE_WANN)) ##331Menarchen in last visit  --> spricht auch daf?r, dass ich last visit nehme
sum(!is.na(tb.fst$C_PUB_STAT_MENARCHE_WANN)) ##220 Menarchen in first visit 
sum(!is.na(tb.rnd$C_PUB_STAT_MENARCHE_WANN)) ##280 Menarchen in random

sum(!is.na(tb.lst$STIMMBRUCH_ALTER)) ##206 Mutation in last visit
sum(!is.na(tb.fst$STIMMBRUCH_ALTER)) ##129 Mutation in first
sum(!is.na(tb.rnd$STIMMBRUCH_ALTER)) ##random

##Menarche plotten (mehrfachbesucher)
n.menarche <- sum(!is.na(tb.lst$C_PUB_STAT_MENARCHE_WANN))

ggplot( tb.lst ) +
  geom_histogram( aes(C_PUB_STAT_MENARCHE_WANN, fill=SEX), stat =  "count" ) + 
  theme_bw( ) + 
  ylab( "frequency" ) +
  scale_fill_manual(values="deeppink", guide=F)+
  scale_x_continuous( "menarchal age [y]", breaks=c(9:16), labels=c(9:16) )+ ##--> wie besser mit alterseineteilung?
  annotate(geom="text",x=15, y=60, label=paste0( "n = ",n.menarche ) ) +
  theme( panel.grid = element_blank())

save(list = c( "tb.fst", "tb.lst", "tb.rnd" ), file = "tbl.ein.Besuch.Rd" )

mean( tb.lst$C_PUB_STAT_MENARCHE_WANN, na.rm = T )

tb.lst.menarch.OB <-
    tb.lst[ tb.lst$SEX == "female" & !is.na( tb.lst$C_PUB_STAT_MENARCHE ), ] %>% group_by( AGE.CAT1, C_PUB_STAT_MENARCHE ) %>% summarise( N = n( ) )

tb.lst.menarch.JA <-
    tb.lst.menarch.OB[ tb.lst.menarch.OB$C_PUB_STAT_MENARCHE == 1, ]

tb.lst.menarch.JA$sum <-
    cumsum( tb.lst.menarch.JA$N )

tb.lst.menarch.JA$sum.rel <-
    tb.lst.menarch.JA$sum / sum( tb.lst.menarch.JA$N )
