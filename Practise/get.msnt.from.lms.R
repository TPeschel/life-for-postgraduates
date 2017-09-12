get.msnt <-
	function( data, sex = "male", ages, family = "BCPEo" ) {
		
		d. <-
			data[ data$sex == sex, ]
		
		d.$unique <-
			c( 1 : nrow( d. ) )
		
		d. <-
			d.[ sample( d.$unique, size = .75 * nrow( d. ), replace = F, prob = d.$weight ), ]
		
		print( sapply( d., function( s ) sum( !is.na( s ) ) ) )
		
		try. <-
			try(
				d.lms <-
					lms( y = f0.4, x = age, families = family, data = d., k = 2 ) )
		
		if( d.lms$family != "NO" && !"try-error" %in% class( try. ) ) {
			
			lms. <-
				as.data.frame(
					predictAll(
						d.lms,
						newdata = data.frame( age = ages ) 
					) )
			
			lms.$age <-
				ages
			
			lms.lst[[ length( lms.lst ) + 1 ]] <-
				lms.
			
			print( paste0( "Loop ", loop, " succeded." ) ) } else {
				
				print( paste0( "Loop ", loop, " failed." ) ) } 
		}
get.lms( d, "female", ages )
