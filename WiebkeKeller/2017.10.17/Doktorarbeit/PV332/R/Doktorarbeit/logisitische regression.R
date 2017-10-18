source( "join.R" )
library( dplyr )
library( ggplot2 )
library( readxl )
library( lubridate )
library( hlpr4life )
load.pkgs( c( "ggplot2", "broom", "reshape2", "lme4", "dplyr" ) )
set.seed( 4 )

(
  sex.lvls <-
    c( "female", "male" ) )

(
  disease.lvls <-
    c( "allergy", "psychiatric", "none", "both" ) )

( 
  prms <-
    data.frame(
      lvls = as.vector( sapply( sex.lvls, function( l ) paste0
                                ( l, ":", hair.lvls ) ) ), #levels
      mue  = m<-c( 30, 35, 32, 39, 41, 38 ), #mean
      var  = v<-c( 7, 5, 7, 11, 13, 15 ), #variance
      slp  = .16 * ( m + v ) ) )

(psy.allerg <- data.frame(lvls = as.vector( sapply(sex.lvls,  
            function( l ) paste0 ( l, ":", disease.lvls))),
            mue = m<-c))