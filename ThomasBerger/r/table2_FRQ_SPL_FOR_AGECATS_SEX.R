## delete all data
rm( list = ls( ) )

hlpr4life::load.pkgs(
    c(
        "hlpr4life",
        "reshape2",
        "WriteXLS",
        "dplyr",
        "magrittr" )
)

## change working dir to data/results
setwd( "~/LIFE/life-for-postgraduates/ThomasBerger/results/" )

load( "~/LIFE/life-for-postgraduates/ThomasBerger/original/Stimme/Stimme-Dateien/save/voice_clean.Rda" )

voice_speak.summary <-
    as.data.frame(
        voice_speak %>%
            group_by( sex, agecat ) %>%
            summarise(
                count = n( ),
                mean.age = mean( age, na.rm = T ),
                sd.age = sd( age, na.rm = T ),
                
                mean.F1 = mean( f0_sprech_1, na.rm = T ),
                mean.F2 = mean( f0_sprech_2, na.rm = T ),
                mean.F3 = mean( f0_sprech_3, na.rm = T ),
                mean.F4 = mean( f0_sprech_4, na.rm = T ),
                mean.F5 = mean( f0_sprech_5, na.rm = T ),
                sd.F1 = sd( f0_sprech_1, na.rm = T ),
                sd.F2 = sd( f0_sprech_2, na.rm = T ),
                sd.F3 = sd( f0_sprech_3, na.rm = T ),
                sd.F4 = sd( f0_sprech_4, na.rm = T ),
                sd.F5 = sd( f0_sprech_5, na.rm = T ),
        
                mean.SPL1 = mean( spl_sprech_1, na.rm = T ),
                mean.SPL2 = mean( spl_sprech_2, na.rm = T ),
                mean.SPL3 = mean( spl_sprech_3, na.rm = T ),
                mean.SPL4 = mean( spl_sprech_4, na.rm = T ),
                mean.SPL5 = mean( spl_sprech_5, na.rm = T ),
                sd.SPL1 = sd( spl_sprech_1, na.rm = T ),
                sd.SPL2 = sd( spl_sprech_2, na.rm = T ),
                sd.SPL3 = sd( spl_sprech_3, na.rm = T ),
                sd.SPL4 = sd( spl_sprech_4, na.rm = T ),
                sd.SPL5 = sd( spl_sprech_5, na.rm = T ) ) )

voice_speak.summary[ ,-c( 1, 2 ) ] <-
    round( voice_speak.summary[ , -c( 1, 2 ) ], 2 )

( vss <-
    melt( voice_speak.summary, id.vars = c( "sex", "agecat" ) ) )

table2 <-
    data.frame(
        SEX = c( rep( "male", 10 ), rep( "female", 10 ) ),
        VALUE = rep( c( rep( "FRQ", 5 ), rep( "SPL", 5 ) ), 2 ),
        LEVEL = c( rep( 1 : 5, 4 ) ),
        'AGE_5.5_10.5' = c( 
            paste0(
                voice_speak.summary$mean.F1[ voice_speak.summary$sex == "male" & voice_speak.summary$agecat == "[5.5,10.5]" ],
                " (", voice_speak.summary$sd.F1[ voice_speak.summary$sex == "male" & voice_speak.summary$agecat == "[5.5,10.5]" ], ")" ),
            paste0(
                voice_speak.summary$mean.F2[ voice_speak.summary$sex == "male" & voice_speak.summary$agecat == "[5.5,10.5]" ],
                " (", voice_speak.summary$sd.F2[ voice_speak.summary$sex == "male" & voice_speak.summary$agecat == "[5.5,10.5]" ], ")" ),
            paste0(
                voice_speak.summary$mean.F3[ voice_speak.summary$sex == "male" & voice_speak.summary$agecat == "[5.5,10.5]" ],
                " (", voice_speak.summary$sd.F3[ voice_speak.summary$sex == "male" & voice_speak.summary$agecat == "[5.5,10.5]" ], ")" ),
            paste0(
                voice_speak.summary$mean.F4[ voice_speak.summary$sex == "male" & voice_speak.summary$agecat == "[5.5,10.5]" ],
                " (", voice_speak.summary$sd.F4[ voice_speak.summary$sex == "male" & voice_speak.summary$agecat == "[5.5,10.5]" ], ")" ),
            paste0(
                voice_speak.summary$mean.F5[ voice_speak.summary$sex == "male" & voice_speak.summary$agecat == "[5.5,10.5]" ],
                " (", voice_speak.summary$sd.F5[ voice_speak.summary$sex == "male" & voice_speak.summary$agecat == "[5.5,10.5]" ], ")" ),
            paste0(
                voice_speak.summary$mean.SPL1[ voice_speak.summary$sex == "male" & voice_speak.summary$agecat == "[5.5,10.5]" ],
                " (", voice_speak.summary$sd.SPL1[ voice_speak.summary$sex == "male" & voice_speak.summary$agecat == "[5.5,10.5]" ], ")" ),
            paste0(
                voice_speak.summary$mean.SPL2[ voice_speak.summary$sex == "male" & voice_speak.summary$agecat == "[5.5,10.5]" ],
                " (", voice_speak.summary$sd.SPL2[ voice_speak.summary$sex == "male" & voice_speak.summary$agecat == "[5.5,10.5]" ], ")" ),
            paste0(
                voice_speak.summary$mean.SPL3[ voice_speak.summary$sex == "male" & voice_speak.summary$agecat == "[5.5,10.5]" ],
                " (", voice_speak.summary$sd.SPL3[ voice_speak.summary$sex == "male" & voice_speak.summary$agecat == "[5.5,10.5]" ], ")" ),
            paste0(
                voice_speak.summary$mean.SPL4[ voice_speak.summary$sex == "male" & voice_speak.summary$agecat == "[5.5,10.5]" ],
                " (", voice_speak.summary$sd.SPL4[ voice_speak.summary$sex == "male" & voice_speak.summary$agecat == "[5.5,10.5]" ], ")" ),
            paste0(
                voice_speak.summary$mean.SPL5[ voice_speak.summary$sex == "male" & voice_speak.summary$agecat == "[5.5,10.5]" ],
                " (", voice_speak.summary$sd.SPL5[ voice_speak.summary$sex == "male" & voice_speak.summary$agecat == "[5.5,10.5]" ], ")" ),
            paste0(
                voice_speak.summary$mean.F1[ voice_speak.summary$sex == "female" & voice_speak.summary$agecat == "[5.5,10.5]" ],
                " (", voice_speak.summary$sd.F1[ voice_speak.summary$sex == "female" & voice_speak.summary$agecat == "[5.5,10.5]" ], ")" ),
            paste0(
                voice_speak.summary$mean.F2[ voice_speak.summary$sex == "female" & voice_speak.summary$agecat == "[5.5,10.5]" ],
                " (", voice_speak.summary$sd.F2[ voice_speak.summary$sex == "female" & voice_speak.summary$agecat == "[5.5,10.5]" ], ")" ),
            paste0(
                voice_speak.summary$mean.F3[ voice_speak.summary$sex == "female" & voice_speak.summary$agecat == "[5.5,10.5]" ],
                " (", voice_speak.summary$sd.F3[ voice_speak.summary$sex == "female" & voice_speak.summary$agecat == "[5.5,10.5]" ], ")" ),
            paste0(
                voice_speak.summary$mean.F4[ voice_speak.summary$sex == "female" & voice_speak.summary$agecat == "[5.5,10.5]" ],
                " (", voice_speak.summary$sd.F4[ voice_speak.summary$sex == "female" & voice_speak.summary$agecat == "[5.5,10.5]" ], ")" ),
            paste0(
                voice_speak.summary$mean.F5[ voice_speak.summary$sex == "female" & voice_speak.summary$agecat == "[5.5,10.5]" ],
                " (", voice_speak.summary$sd.F5[ voice_speak.summary$sex == "female" & voice_speak.summary$agecat == "[5.5,10.5]" ], ")" ),
            paste0(
                voice_speak.summary$mean.SPL1[ voice_speak.summary$sex == "female" & voice_speak.summary$agecat == "[5.5,10.5]" ],
                " (", voice_speak.summary$sd.SPL1[ voice_speak.summary$sex == "female" & voice_speak.summary$agecat == "[5.5,10.5]" ], ")" ),
            paste0(
                voice_speak.summary$mean.SPL2[ voice_speak.summary$sex == "female" & voice_speak.summary$agecat == "[5.5,10.5]" ],
                " (", voice_speak.summary$sd.SPL2[ voice_speak.summary$sex == "female" & voice_speak.summary$agecat == "[5.5,10.5]" ], ")" ),
            paste0(
                voice_speak.summary$mean.SPL3[ voice_speak.summary$sex == "female" & voice_speak.summary$agecat == "[5.5,10.5]" ],
                " (", voice_speak.summary$sd.SPL3[ voice_speak.summary$sex == "female" & voice_speak.summary$agecat == "[5.5,10.5]" ], ")" ),
            paste0(
                voice_speak.summary$mean.SPL4[ voice_speak.summary$sex == "female" & voice_speak.summary$agecat == "[5.5,10.5]" ],
                " (", voice_speak.summary$sd.SPL4[ voice_speak.summary$sex == "female" & voice_speak.summary$agecat == "[5.5,10.5]" ], ")" ),
            paste0(
                voice_speak.summary$mean.SPL5[ voice_speak.summary$sex == "female" & voice_speak.summary$agecat == "[5.5,10.5]" ],
                " (", voice_speak.summary$sd.SPL5[ voice_speak.summary$sex == "female" & voice_speak.summary$agecat == "[5.5,10.5]" ], ")" )
            ),
        'AGE_10.5_14.5' = c( 
            paste0(
                voice_speak.summary$mean.F1[ voice_speak.summary$sex == "male" & voice_speak.summary$agecat == "(10.5,14.5]" ],
                " (", voice_speak.summary$sd.F1[ voice_speak.summary$sex == "male" & voice_speak.summary$agecat == "(10.5,14.5]" ], ")" ),
            paste0(
                voice_speak.summary$mean.F2[ voice_speak.summary$sex == "male" & voice_speak.summary$agecat == "(10.5,14.5]" ],
                " (", voice_speak.summary$sd.F2[ voice_speak.summary$sex == "male" & voice_speak.summary$agecat == "(10.5,14.5]" ], ")" ),
            paste0(
                voice_speak.summary$mean.F3[ voice_speak.summary$sex == "male" & voice_speak.summary$agecat == "(10.5,14.5]" ],
                " (", voice_speak.summary$sd.F3[ voice_speak.summary$sex == "male" & voice_speak.summary$agecat == "(10.5,14.5]" ], ")" ),
            paste0(
                voice_speak.summary$mean.F4[ voice_speak.summary$sex == "male" & voice_speak.summary$agecat == "(10.5,14.5]" ],
                " (", voice_speak.summary$sd.F4[ voice_speak.summary$sex == "male" & voice_speak.summary$agecat == "(10.5,14.5]" ], ")" ),
            paste0(
                voice_speak.summary$mean.F5[ voice_speak.summary$sex == "male" & voice_speak.summary$agecat == "(10.5,14.5]" ],
                " (", voice_speak.summary$sd.F5[ voice_speak.summary$sex == "male" & voice_speak.summary$agecat == "(10.5,14.5]" ], ")" ),
            paste0(
                voice_speak.summary$mean.SPL1[ voice_speak.summary$sex == "male" & voice_speak.summary$agecat == "(10.5,14.5]" ],
                " (", voice_speak.summary$sd.SPL1[ voice_speak.summary$sex == "male" & voice_speak.summary$agecat == "(10.5,14.5]" ], ")" ),
            paste0(
                voice_speak.summary$mean.SPL2[ voice_speak.summary$sex == "male" & voice_speak.summary$agecat == "(10.5,14.5]" ],
                " (", voice_speak.summary$sd.SPL2[ voice_speak.summary$sex == "male" & voice_speak.summary$agecat == "(10.5,14.5]" ], ")" ),
            paste0(
                voice_speak.summary$mean.SPL3[ voice_speak.summary$sex == "male" & voice_speak.summary$agecat == "(10.5,14.5]" ],
                " (", voice_speak.summary$sd.SPL3[ voice_speak.summary$sex == "male" & voice_speak.summary$agecat == "(10.5,14.5]" ], ")" ),
            paste0(
                voice_speak.summary$mean.SPL4[ voice_speak.summary$sex == "male" & voice_speak.summary$agecat == "(10.5,14.5]" ],
                " (", voice_speak.summary$sd.SPL4[ voice_speak.summary$sex == "male" & voice_speak.summary$agecat == "(10.5,14.5]" ], ")" ),
            paste0(
                voice_speak.summary$mean.SPL5[ voice_speak.summary$sex == "male" & voice_speak.summary$agecat == "(10.5,14.5]" ],
                " (", voice_speak.summary$sd.SPL5[ voice_speak.summary$sex == "male" & voice_speak.summary$agecat == "(10.5,14.5]" ], ")" ),
            paste0(
                voice_speak.summary$mean.F1[ voice_speak.summary$sex == "female" & voice_speak.summary$agecat == "(10.5,14.5]" ],
                " (", voice_speak.summary$sd.F1[ voice_speak.summary$sex == "female" & voice_speak.summary$agecat == "(10.5,14.5]" ], ")" ),
            paste0(
                voice_speak.summary$mean.F2[ voice_speak.summary$sex == "female" & voice_speak.summary$agecat == "(10.5,14.5]" ],
                " (", voice_speak.summary$sd.F2[ voice_speak.summary$sex == "female" & voice_speak.summary$agecat == "(10.5,14.5]" ], ")" ),
            paste0(
                voice_speak.summary$mean.F3[ voice_speak.summary$sex == "female" & voice_speak.summary$agecat == "(10.5,14.5]" ],
                " (", voice_speak.summary$sd.F3[ voice_speak.summary$sex == "female" & voice_speak.summary$agecat == "(10.5,14.5]" ], ")" ),
            paste0(
                voice_speak.summary$mean.F4[ voice_speak.summary$sex == "female" & voice_speak.summary$agecat == "(10.5,14.5]" ],
                " (", voice_speak.summary$sd.F4[ voice_speak.summary$sex == "female" & voice_speak.summary$agecat == "(10.5,14.5]" ], ")" ),
            paste0(
                voice_speak.summary$mean.F5[ voice_speak.summary$sex == "female" & voice_speak.summary$agecat == "(10.5,14.5]" ],
                " (", voice_speak.summary$sd.F5[ voice_speak.summary$sex == "female" & voice_speak.summary$agecat == "(10.5,14.5]" ], ")" ),
            paste0(
                voice_speak.summary$mean.SPL1[ voice_speak.summary$sex == "female" & voice_speak.summary$agecat == "(10.5,14.5]" ],
                " (", voice_speak.summary$sd.SPL1[ voice_speak.summary$sex == "female" & voice_speak.summary$agecat == "(10.5,14.5]" ], ")" ),
            paste0(
                voice_speak.summary$mean.SPL2[ voice_speak.summary$sex == "female" & voice_speak.summary$agecat == "(10.5,14.5]" ],
                " (", voice_speak.summary$sd.SPL2[ voice_speak.summary$sex == "female" & voice_speak.summary$agecat == "(10.5,14.5]" ], ")" ),
            paste0(
                voice_speak.summary$mean.SPL3[ voice_speak.summary$sex == "female" & voice_speak.summary$agecat == "(10.5,14.5]" ],
                " (", voice_speak.summary$sd.SPL3[ voice_speak.summary$sex == "female" & voice_speak.summary$agecat == "(10.5,14.5]" ], ")" ),
            paste0(
                voice_speak.summary$mean.SPL4[ voice_speak.summary$sex == "female" & voice_speak.summary$agecat == "(10.5,14.5]" ],
                " (", voice_speak.summary$sd.SPL4[ voice_speak.summary$sex == "female" & voice_speak.summary$agecat == "(10.5,14.5]" ], ")" ),
            paste0(
                voice_speak.summary$mean.SPL5[ voice_speak.summary$sex == "female" & voice_speak.summary$agecat == "(10.5,14.5]" ],
                " (", voice_speak.summary$sd.SPL5[ voice_speak.summary$sex == "female" & voice_speak.summary$agecat == "(10.5,14.5]" ], ")" )
            ),
        'AGE_14.5_17.5' = c( 
            paste0(
                voice_speak.summary$mean.F1[ voice_speak.summary$sex == "male" & voice_speak.summary$agecat == "(14.5,17.5]" ],
                " (", voice_speak.summary$sd.F1[ voice_speak.summary$sex == "male" & voice_speak.summary$agecat == "(14.5,17.5]" ], ")" ),
            paste0(
                voice_speak.summary$mean.F2[ voice_speak.summary$sex == "male" & voice_speak.summary$agecat == "(14.5,17.5]" ],
                " (", voice_speak.summary$sd.F2[ voice_speak.summary$sex == "male" & voice_speak.summary$agecat == "(14.5,17.5]" ], ")" ),
            paste0(
                voice_speak.summary$mean.F3[ voice_speak.summary$sex == "male" & voice_speak.summary$agecat == "(14.5,17.5]" ],
                " (", voice_speak.summary$sd.F3[ voice_speak.summary$sex == "male" & voice_speak.summary$agecat == "(14.5,17.5]" ], ")" ),
            paste0(
                voice_speak.summary$mean.F4[ voice_speak.summary$sex == "male" & voice_speak.summary$agecat == "(14.5,17.5]" ],
                " (", voice_speak.summary$sd.F4[ voice_speak.summary$sex == "male" & voice_speak.summary$agecat == "(14.5,17.5]" ], ")" ),
            paste0(
                voice_speak.summary$mean.F5[ voice_speak.summary$sex == "male" & voice_speak.summary$agecat == "(14.5,17.5]" ],
                " (", voice_speak.summary$sd.F5[ voice_speak.summary$sex == "male" & voice_speak.summary$agecat == "(14.5,17.5]" ], ")" ),
            paste0(
                voice_speak.summary$mean.SPL1[ voice_speak.summary$sex == "male" & voice_speak.summary$agecat == "(14.5,17.5]" ],
                " (", voice_speak.summary$sd.SPL1[ voice_speak.summary$sex == "male" & voice_speak.summary$agecat == "(14.5,17.5]" ], ")" ),
            paste0(
                voice_speak.summary$mean.SPL2[ voice_speak.summary$sex == "male" & voice_speak.summary$agecat == "(14.5,17.5]" ],
                " (", voice_speak.summary$sd.SPL2[ voice_speak.summary$sex == "male" & voice_speak.summary$agecat == "(14.5,17.5]" ], ")" ),
            paste0(
                voice_speak.summary$mean.SPL3[ voice_speak.summary$sex == "male" & voice_speak.summary$agecat == "(14.5,17.5]" ],
                " (", voice_speak.summary$sd.SPL3[ voice_speak.summary$sex == "male" & voice_speak.summary$agecat == "(14.5,17.5]" ], ")" ),
            paste0(
                voice_speak.summary$mean.SPL4[ voice_speak.summary$sex == "male" & voice_speak.summary$agecat == "(14.5,17.5]" ],
                " (", voice_speak.summary$sd.SPL4[ voice_speak.summary$sex == "male" & voice_speak.summary$agecat == "(14.5,17.5]" ], ")" ),
            paste0(
                voice_speak.summary$mean.SPL5[ voice_speak.summary$sex == "male" & voice_speak.summary$agecat == "(14.5,17.5]" ],
                " (", voice_speak.summary$sd.SPL5[ voice_speak.summary$sex == "male" & voice_speak.summary$agecat == "(14.5,17.5]" ], ")" ),
            paste0(
                voice_speak.summary$mean.F1[ voice_speak.summary$sex == "female" & voice_speak.summary$agecat == "(14.5,17.5]" ],
                " (", voice_speak.summary$sd.F1[ voice_speak.summary$sex == "female" & voice_speak.summary$agecat == "(14.5,17.5]" ], ")" ),
            paste0(
                voice_speak.summary$mean.F2[ voice_speak.summary$sex == "female" & voice_speak.summary$agecat == "(14.5,17.5]" ],
                " (", voice_speak.summary$sd.F2[ voice_speak.summary$sex == "female" & voice_speak.summary$agecat == "(14.5,17.5]" ], ")" ),
            paste0(
                voice_speak.summary$mean.F3[ voice_speak.summary$sex == "female" & voice_speak.summary$agecat == "(14.5,17.5]" ],
                " (", voice_speak.summary$sd.F3[ voice_speak.summary$sex == "female" & voice_speak.summary$agecat == "(14.5,17.5]" ], ")" ),
            paste0(
                voice_speak.summary$mean.F4[ voice_speak.summary$sex == "female" & voice_speak.summary$agecat == "(14.5,17.5]" ],
                " (", voice_speak.summary$sd.F4[ voice_speak.summary$sex == "female" & voice_speak.summary$agecat == "(14.5,17.5]" ], ")" ),
            paste0(
                voice_speak.summary$mean.F5[ voice_speak.summary$sex == "female" & voice_speak.summary$agecat == "(14.5,17.5]" ],
                " (", voice_speak.summary$sd.F5[ voice_speak.summary$sex == "female" & voice_speak.summary$agecat == "(14.5,17.5]" ], ")" ),
            paste0(
                voice_speak.summary$mean.SPL1[ voice_speak.summary$sex == "female" & voice_speak.summary$agecat == "(14.5,17.5]" ],
                " (", voice_speak.summary$sd.SPL1[ voice_speak.summary$sex == "female" & voice_speak.summary$agecat == "(14.5,17.5]" ], ")" ),
            paste0(
                voice_speak.summary$mean.SPL2[ voice_speak.summary$sex == "female" & voice_speak.summary$agecat == "(14.5,17.5]" ],
                " (", voice_speak.summary$sd.SPL2[ voice_speak.summary$sex == "female" & voice_speak.summary$agecat == "(14.5,17.5]" ], ")" ),
            paste0(
                voice_speak.summary$mean.SPL3[ voice_speak.summary$sex == "female" & voice_speak.summary$agecat == "(14.5,17.5]" ],
                " (", voice_speak.summary$sd.SPL3[ voice_speak.summary$sex == "female" & voice_speak.summary$agecat == "(14.5,17.5]" ], ")" ),
            paste0(
                voice_speak.summary$mean.SPL4[ voice_speak.summary$sex == "female" & voice_speak.summary$agecat == "(14.5,17.5]" ],
                " (", voice_speak.summary$sd.SPL4[ voice_speak.summary$sex == "female" & voice_speak.summary$agecat == "(14.5,17.5]" ], ")" ),
            paste0(
                voice_speak.summary$mean.SPL5[ voice_speak.summary$sex == "female" & voice_speak.summary$agecat == "(14.5,17.5]" ],
                " (", voice_speak.summary$sd.SPL5[ voice_speak.summary$sex == "female" & voice_speak.summary$agecat == "(14.5,17.5]" ], ")" )
            )
        )

table2

table2.add <-
    voice_speak.summary[ , c( "sex", "agecat", "count", "mean.age", "sd.age" ) ]

WriteXLS( table2, ExcelFileName = "table2.xlsx" )
WriteXLS( table2.add, ExcelFileName = "table2.add.xlsx" )
