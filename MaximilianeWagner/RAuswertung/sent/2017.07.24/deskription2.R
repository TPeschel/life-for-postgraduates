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
setwd( "~/LIFE/life-for-postgraduates/MaximilianeWagner/RAuswertung/" )
#setwd( "C:/Users/Anwender/Documents/Dissertation/RAuswertung/" )

load( "daten/main.table.Rd" )

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
