lhs <- c( "age", "weight", "weight.sds" , "height", 
          "height.sds", "bmi", "bmi.sds",
          "OSTEO", "P1NP", "AP", "BCL", 
          "VDT", "PTH", "VDBP_S_NUM_VALUE", "VD3_2OH",
          "CALCIUM",  "PHOSPHAT", "IGF1", "IGF1.SDS")
rhs <- c( "log10CT")

methods = c( "pearson", "spearman" )
sexes <- c( "female", "male" )

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


