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
setwd( "~/LIFE/life-for-postgraduates/MaximilianeWagner/RAuswertung/daten" )
#setwd( "C:/Users/Anwender/Documents/Dissertation/RAuswertung/" )

load( "main.table" )

#zuerst betrachte ich die gesamte datenlage 
#wie ist das durchschnittliche alter aller kinder
summary( main.table$AGE )
summary( main.table$AGE[ main.table$SEX == "male" ] )
summary( main.table$AGE[ main.table$SEX == "female" ] )

#wie ist das durchschnittliche alter des subsamples
main.table.complete <-
  main.table[ !is.na( main.table$CORTISOL ), ]

summary( main.table.complete$AGE )
summary( main.table.complete$AGE[ main.table.complete$SEX == "male" ] )
summary( main.table.complete$AGE[ main.table.complete$SEX == "female" ] )

##
# das musst du doch nicht nochmal machen, 
# siehe zeilen 27, 28
##
# main.table.complete <-
#   main.table[ !is.na( main.table$CORTISOL ), ]

#wie sind die geschlechter verteilt im subsample
sum( !is.na( main.table.complete$SEX ) )
sum( !is.na( main.table.complete$SEX [ main.table.complete$SEX == "male" ] ) )
sum( !is.na( main.table.complete$SEX[ main.table.complete$SEX == "female" ] ) )


#wie sind die geschlechter verteilt gesamte studie
sum( !is.na( main.table$SEX[ main.table$SEX == "male" ] ) )
sum( !is.na( main.table$SEX[ main.table$SEX == "female" ] ) )

#wieviele kinder gibt es zu jedem tanner stadium im subsample
sum( !is.na( main.table.complete$TANNER ) )
##
# Das funktioniert so nicht.
# Du fragst, ob es in der Spalte Tanner in den Zeilen kein Missing steht, wenn da ne 1 steht
##
sum( !is.na( main.table.complete$TANNER[ main.table.complete$TANNER == 1 ] ) )
sum( !is.na( main.table.complete$TANNER[ main.table.complete$TANNER == 2 ] ) )
sum( !is.na( main.table.complete$TANNER[ main.table.complete$TANNER == 3 ] ) )
sum( !is.na( main.table.complete$TANNER[ main.table.complete$TANNER == 4 ] ) )
sum( !is.na( main.table.complete$TANNER[ main.table.complete$TANNER == 5 ] ) )

#wieviele kinder gibt es zu jedem tanner nach geschlechtern getrennt im subsample
sum( !is.na( main.table.complete$TANNER ))
sum( !is.na( main.table.complete$TANNER[ main.table.complete$TANNER == 1 ] [main.table.complete$SEX == "male"] ) )
sum( !is.na( main.table.complete$TANNER[ main.table.complete$TANNER == 2 ] [main.table.complete$SEX == "male"] ) )
sum( !is.na( main.table.complete$TANNER[ main.table.complete$TANNER == 3 ] [main.table.complete$SEX == "male"] ) )
sum( !is.na( main.table.complete$TANNER[ main.table.complete$TANNER == 4 ] [main.table.complete$SEX == "male"] ) )
sum( !is.na( main.table.complete$TANNER[ main.table.complete$TANNER == 5 ] [main.table.complete$SEX == "male"] ) )

# wieviele kinder gibt es zu jedem tanner in der gesamten studie
sum( !is.na( main.table$TANNER ) )
sum( !is.na( main.table$TANNER[ main.table$TANNER == 1 ] ) )
sum( !is.na( main.table$TANNER[ main.table$TANNER == 2 ] ) )
sum( !is.na( main.table$TANNER[ main.table$TANNER == 3 ] ) )
sum( !is.na( main.table$TANNER[ main.table$TANNER == 4 ] ) )
sum( !is.na( main.table$TANNER[ main.table$TANNER == 5 ] ) )

# wieviele kinder gibt es zu jeder haarwaschfrequenz nach geschlechtern getrennt im subsample

sum( !is.na( main.table.complete$HAARWASCH_FREQ_GRPS ))
sum( !is.na( main.table.complete$HAARWASCH_FREQ_GRPS[ main.table.complete$HAARWASCH_FREQ_GRPS == "haeufig" ] [main.table.complete$SEX == "male"] ) )
sum( !is.na( main.table.complete$HAARWASCH_FREQ_GRPS[ main.table.complete$HAARWASCH_FREQ_GRPS == "normal" ] [main.table.complete$SEX == "male"] ) )
sum( !is.na( main.table.complete$HAARWASCH_FREQ_GRPS[ main.table.complete$HAARWASCH_FREQ_GRPS == "selten"] [main.table.complete$SEX == "male"] ) )
sum( !is.na( main.table.complete$HAARWASCH_FREQ_GRPS[ main.table.complete$HAARWASCH_FREQ_GRPS == "haeufig" ] [main.table.complete$SEX == "female"] ) )
sum( !is.na( main.table.complete$HAARWASCH_FREQ_GRPS[ main.table.complete$HAARWASCH_FREQ_GRPS == "normal" ] [main.table.complete$SEX == "female"] ) )
sum( !is.na( main.table.complete$HAARWASCH_FREQ_GRPS[ main.table.complete$HAARWASCH_FREQ_GRPS == "selten"] [main.table.complete$SEX == "female"] ) )

# wieviele kinder gibt es zu jeder haarwaschfrequenz nach geschlechtern getrennt in life
sum( !is.na( main.table$HAARWASCH_FREQ_GRPS[ main.table.complete$HAARWASCH_FREQ_GRPS == "haeufig" ] ) )
sum( !is.na( main.table$HAARWASCH_FREQ_GRPS[ main.table.complete$HAARWASCH_FREQ_GRPS == "normal" ]  ) )
sum( !is.na( main.table$HAARWASCH_FREQ_GRPS[ main.table.complete$HAARWASCH_FREQ_GRPS == "selten"] ) )

# wie ist der bmi im subsample 
sum( !is.na( main.table.complete$bmigroup[ main.table.complete$bmigroup == "normalweight" ] [main.table.complete$SEX == "male"] ) )
sum( !is.na( main.table.complete$bmigroup[ main.table.complete$bmigroup == "overweight" ] [main.table.complete$SEX == "male"] ) )
sum( !is.na( main.table.complete$bmigroup[ main.table.complete$bmigroup == "obese"] [main.table.complete$SEX == "male"] ) )
sum( !is.na( main.table.complete$bmigroup[ main.table.complete$bmigroup == "normalweight" ] [main.table.complete$SEX == "female"] ) )
sum( !is.na( main.table.complete$bmigroup[ main.table.complete$bmigroup == "overweight" ] [main.table.complete$SEX == "female"] ) )
sum( !is.na( main.table.complete$bmigroup[ main.table.complete$bmigroup == "obese"] [main.table.complete$SEX == "female"] ) )

# wie ist der bmi bei life 
sum( !is.na( main.table$bmigroup[ main.table.complete$bmigroup == "normalweight" ] ) )
sum( !is.na( main.table$bmigroup[ main.table.complete$bmigroup == "overweight" ]  ) )
sum( !is.na( main.table$bmigroup[ main.table.complete$bmigroup == "obese"] ) )

main.table$AGE.CAT <-
	cut(
		main.table$AGE,
		breaks = c( 0 : 20 ) )

plt <-
	function( tbl ) {
		
		p1 <-
			ggplot( 
				tbl, 
				aes( 
					cut( 
						AGE, 
						breaks = c( 0 : 20 ) ), 
					fill = SEX ) ) +
			theme_classic( ) +
			scale_fill_manual( values = c( "deeppink", "deepskyblue" ), guide = F ) +
			geom_histogram( stat = "count" ) +
			geom_hline( yintercept = 30, linetype = 2 ) +
			facet_grid( SEX ~ . ) +
			labs( title = "AGE", x = "AGE [y]" ) +
			theme( axis.text.x = element_text( angle = 90 ) )
		
		p2 <-
			ggplot( tbl[ 0 < tbl$CORTISOL, ]  ) +
			theme_classic( ) +
			scale_fill_manual( values = c( "deeppink", "deepskyblue" ), guide = F ) +
			geom_boxplot( aes( AGE.CAT, CORTISOL, fill = SEX ) ) +
			facet_grid( SEX ~ . ) +
			labs( title = "LOG CORTISOL BOXPLOT", x = "AGE [y]", y = "log10 cortisol" ) +
			theme( axis.text.x = element_text( angle = 90 ) ) +
			scale_y_log10( )
		
		p3 <-
			ggplot( tbl %>% group_by( AGE.CAT, SEX, PUBSTAT ) %>% summarise( n = n( ) ) ) +
			theme_classic( ) +
			scale_fill_manual( 
				"PUBERTY STATES\n PER SEX",
				labels = c( paste0( "female ", c( 1 : 5 ) ), paste0( "male ", c( 1 : 5 ) ) ),
				values = rev( c( "#0000ff", "#4040ff", "#8080ff", "#c0c0ff", "#f0f0ff", "#ff0000", "#ff4040", "#ff8080", "#ffc0c0", "#fff0f0" ) ) ) +
			geom_histogram( aes( AGE.CAT, n, fill = paste0( SEX, PUBSTAT ) ), stat = "identity", col = "black" ) +
			geom_hline( yintercept = 30, linetype = 2 ) +
			facet_grid( SEX ~ . ) +
			labs( title = "PUBERTY STATES PER AGE", x = "AGE [y]" ) +
			theme( axis.text.x = element_text( angle = 90 ) )
		
		p4 <-
			ggplot( tbl[ !is.na( tbl$AGE.CAT ), ] ) +
			theme_bw( ) +
			scale_fill_manual( values = c( "deeppink", "deepskyblue" ) ) +
			geom_histogram( aes( AGE.CAT, fill = SEX ), stat = "count" ) +
			facet_grid( SEX ~ PUBSTAT ) +
			labs( title = "PUBSTAT", x = "PUB STAT" ) +
			theme( axis.text.x = element_text( angle = 90 ) )
		
		##
		# !!! ZOOM !!!
		##
		# ggsubplot( p1, p2, p3, p4, cols = 2 )
		
		ggsubplot( 
			p1, p2, p3, p4,
			layout = t(
				matrix(
					c( 
						1, 2, 3,
						4, 4, 4 ),
					ncol= 2 ) ) ) }

mt <-
	main.table[ !is.na( main.table$CORTISOL ) & !is.na( main.table$AGE ) & !is.na( main.table$PUBSTAT ), ]

nrow( mt )

sapply( mt, function( col ) { sum( !is.na( col ) ) } )

plt( mt )

#plt( na.omit( main.table ) )

# wieviele Datem in den Spalten?
sapply( main.table, function( col ) { sum( is.na( col ) ) } )
sapply( main.table, function( col ) { sum( !is.na( col ) ) } )
sapply( main.table, function( col ) { if( is.numeric( col ) ) mean( col, na.rm = T ) } )

mean( main.table$BMI_ADJ, na.rm = T )
sd( main.table$BMI_ADJ, na.rm = T )

