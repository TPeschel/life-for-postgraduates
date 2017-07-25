rm( list = ls( ) )

library( "hlpr4life" )

load.pkgs(
	c(
		"readxl",
		"xlsx",
		"dplyr",
		"ggplot2",
		"ggthemes",
		"lubridate" ) )

# hier Deinen Pfad eintragen
setwd( "~/LIFE/life-for-postgraduates/MaximilianeWagner/RAuswertung/daten/erzeugt/" )
#setwd( "C:/Users/Anwender/Documents/Dissertation/RAuswertung/daten/erzeugt" )

load( "main.table.Rd" )

# windsorize Kortisol at 35
# schau zunaechst einmal, welche Werte groesser 35 sind
main.table[ !is.na( main.table$CORTISOL ) & main.table$CORTISOL > 35, ]

# entferne diese
main.table.windsorized <-
	main.table[ is.na( main.table$CORTISOL ) | ( !is.na( main.table$CORTISOL ) & main.table$CORTISOL < 35 ), ]

# erstelle vollstaendige Kortisol-Tabelle daraus
main.table.complete.cortisol <-
	main.table.windsorized[ !is.na( main.table.windsorized$CORTISOL ), ]

save( main.table, main.table.windsorized, main.table.complete.cortisol, file = "allData.windsorized.and.curated.2017.07.25.Rd" )
