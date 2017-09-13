get.msnt <-
	function( data, ages, family = c( "BCCGo", "BCPEo", "BCTo" ), sample.density = .75 ) {

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
					lms( y = val, x = age, families = family, data = d, k = 2 ) )
		
		if( !"try-error" %in% class( tryA ) && dlms$family != "NO" ) {
			
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
