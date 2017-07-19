## delete all data
rm( list = ls( ) )

library( hlpr4life )

load.pkgs( 
    c( 
        "reshape2",
        "ggthemes",
        "WriteXLS" ) )

load( "~/LIFE/life-for-postgraduates/ThomasBerger/original/Stimme/Stimme-Dateien/save/voice_clean.Rda" )

vp <-
    voice_speak

## change working dir to results
setwd( "~/LIFE/life-for-postgraduates/ThomasBerger/results/" )

summary.of.voice_speak <-
    function( vp ) {
        vp %>%
            group_by( sex, agecat ) %>%
            summarise(
                n               = n( ),
                age.mean        = round( mean( age, na.rm = T ), 2 ),
                age.sd          = round( sd( age, na.rm = T ), 2 ),
                thin            = sum( bmi.sds.cat == "thin", na.rm = T ),
                thin.perc       = round( 100 * thin / n, 2 ),
                normal          = sum( bmi.sds.cat == "normal", na.rm = T ),
                normal.perc     = round( 100 * normal / n, 2 ),
                overweight      = sum( bmi.sds.cat == "overwt", na.rm = T ),
                overweight.perc = round( 100 * overweight / n, 2 ),
                obese           = sum( bmi.sds.cat == "obese", na.rm = T ),
                obese.perc      = round( 100 * obese / n, 2 ),
                bmi.sds.mean    = round( mean( bmi.sds, na.rm = T ), 2 ),
                bmi.sds.sd      = round( sd( bmi.sds, na.rm = T ), 2 ),
                weight.mean     = round( mean( weight, na.rm = T ), 2 ),
                weight.sd       = round( sd( weight, na.rm = T ), 2 ),
                tanner1         = sum( tanner == 1, na.rm = T ), 
                tanner1.perc    = round( 100 * tanner1 / n, 2 ),
                tanner2         = sum( tanner == 2, na.rm = T ), 
                tanner2.perc    = round( 100 * tanner2 / n, 2 ),
                tanner3         = sum( tanner == 3, na.rm = T ), 
                tanner3.perc    = round( 100 * tanner3 / n, 2 ),
                tanner4         = sum( tanner == 4, na.rm = T ), 
                tanner4.perc    = round( 100 * tanner4 / n, 2 ),
                tanner5         = sum( tanner == 5, na.rm = T ), 
                tanner5.perc    = round( 100 * tanner5 / n, 2 ),
                klasakA         = sum( u_sing_stimmbelastg == 1, na.rm = T ),
                klasakA.perc    = round( 100 * klasakA / n, 2 ),
                klasakB         = sum( u_sing_stimmbelastg == 2, na.rm = T ),
                klasakB.perc    = round( 100 * klasakB / n, 2 ),
                klasakC         = sum( u_sing_stimmbelastg == 3, na.rm = T ),
                klasakC.perc    = round( 100 * klasakC / n, 2 ),
                klasakD         = sum( u_sing_stimmbelastg == 4, na.rm = T ),
                klasakD.perc    = round( 100 * klasakD / n, 2 ) )
    }

vp.without.nas.summary <-
    summary.of.voice_speak( vp[ !is.na( vp$agecat ) & !is.na( vp$bmi.sds.cat )& !is.na( vp$weight ) & !is.na( vp$sex ) & !is.na( vp$tanner ) & !is.na( vp$u_sing_stimmbelastg ), ] )

vp.with.nas.summary <-
    summary.of.voice_speak( vp )

WriteXLS( x = list( allData = vp.with.nas.summary, onlyCompleteData = vp.without.nas.summary ), ExcelFileName = "voice_speak_summary.xlsx" )

WriteXLS( x = list( allData = as.data.frame( t( vp.with.nas.summary ) ), onlyCompleteData = as.data.frame( t( vp.without.nas.summary ) ) ), ExcelFileName = "voice_speak_summary_transposed.xlsx" )
