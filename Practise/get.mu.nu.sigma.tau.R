get.msnt <-
	function( data, ages, family = c( "BE", "logNO", "NOF", "SEP1", "SEP2", "SEP3", "SEP4", "BCCGo", "BCPEo", "BCTo" ), sample.density = 1, trans.x = F ) {
		
		# data <- d..[ d..$sex == "male", ]
		# ages <- ages
		# family <- "BCTo"
		# sample.density <- .5
		
		d <-
			na.omit( 
				data[ , c( "val", "age", "weight" ) ] )
		
		d$unique <-
			c( 1 : nrow( d ) )
		
		d <-
			d[ sample( d$unique, size = sample.density * nrow( d ), replace = T, prob = d$weight ), ]
		
		print( "age bins statistic" )
		
		print( st <-
			   	addmargins( table( table( d$age ) ) ) )
		
		tryA <-
			try(
				dlms <-
					lms( y = val, x = age, families = family, data = d, k = 2, trans.x = trans.x ) )
		
		if( !"try-error" %in% class( tryA ) ) {
			
			tryB <-
				try( 
					elms <-
						predictAll(
							dlms,
							newdata = data.frame( age = ages ) ) )
			
			if( !"try-error" %in% class( tryB ) ) {
				
				elms$age <-
					ages
				
				elms$family <-
					rep( dlms$family[ 1 ], length( ages ) )
				
				return( list( LMS = elms, STAT = st ) ) } }
		
		#return( list( LMS = NA, STAT = st ) ) }
		return( NA ) }

# get.msnt(
# 	rename.columns(
# 		d[ d$sex == "female", ],
# 		c( "f0.1" ),
# 		c( "val" ) ),
# 	ages )
sds.BC <-
	function( perc, mu, sigma, nu, tau = 10 ) {
		
		z <-
			qnorm( perc )
		
		f <- 
			ifelse( 
				nu < .000001,
				mu * exp( sigma * z ),
				mu * ( z * nu * sigma + 1 ) ** ( 1 / nu ) ) 
		f }

sds.NORM <-
	function( perc, mu, sigma ) qnorm( perc ) * sigma + mu


get.lss <-
	function( data, ages, family = c( "BE", "logNO", "NOF", "SEP1", "SEP2", "SEP3", "SEP4", "BCCGo", "BCPEo", "BCTo" ), sample.density = 1, trans.x = F ) {
		
		# data <- d..[ d..$sex == "male", ]
		# ages <- ages
		# family <- "BCTo"
		# sample.density <- .5
		
		d <-
			na.omit( 
				data[ , c( "val", "age", "weight" ) ] )
		
		d$unique <-
			c( 1 : nrow( d ) )
		
		d <-
			d[ sample( d$unique, size = sample.density * nrow( d ), replace = T, prob = d$weight ), ]
		
		print( "age bins statistic" )
		
		print( st <-
			   	addmargins( table( table( d$age ) ) ) )
		
		tryA <-
			try(
				dlms <-
					lms( y = val, x = age, families = family, data = d, k = 2, trans.x = trans.x ) )
		
		if( !"try-error" %in% class( tryA ) ) {
			
			tryB <-
				try( 
					elms <-
						predictAll(
							dlms,
							newdata = data.frame( age = ages ) ) )
			
			if( !"try-error" %in% class( tryB ) ) {
				
				elms$age <-
					ages
				
				elms$family <-
					rep( dlms$family[ 1 ], length( ages ) )
				
				return( list( LMS = dlms, PRED = elms, STAT = st ) ) } }
		
		#return( list( LMS = NA, STAT = st ) ) }
		return( NA ) }

