####################
rm( list = ls( ) ) #######################################
if( !"devtools" %in% rownames( installed.packages( ) ) ) #
	install.packages( "devtools" )               #########
devtools::install_github( "TPeschel/hlpr4life" ) #
hlpr4life::load.pkgs(  ###########################
	c(                 #               
		"hlpr4life",   #
		"ggplot2",     #
		"ggthemes",    #
		"broom",       #
		"XML",         #
		"gamlss",      #
 		"gamlss.data", #
		"dplyr",       #
		"lubridate",   #
		"reshape2") )  #
########################

books <-
	xmlParse( "books.xml" )

books.root <-
	xmlRoot( books )

xmlSize( books )
xmlSize( books.root )

my.books.1 <-
	xmlToDataFrame( books.root )

my.books.2 <-
	xmlSApply( books.root, function( b ) { xmlAttrs( b ); xmlToDataFrame( b ) } )

xmlAttrs( books.root[[ 1 ]] )

show.xml <-
	function( tag, tab = 0 ) {
		
		s <-
			xmlSize( tag )
		
		if( s == 1 ) {
			
			print( xmlValue( tag ) )
			#			print( paste0( strrep( " ", tab ), xmlName( tag ) ) )
			#			print( paste0( strrep( " ", tab + 2 ), xmlValue( tag ) ) )
			
			return( ) }
		
		for( i in 1 : s ) {
			
			show.xml( tag[[ i ]], tab + 2 ) } }

print.xml <-
	function( tag, tab = 0 ) {
		
		s <-
			xmlSize( tag )
		
		if( s == 1 ) {
			
			return( xmlValue( tag ) ) }
		
		st <-
			"\n"
		
		for( i in 1 : s ) {
			
			st <-
				paste0( st, print.xml( tag[[ i ]], tab + 2 ) ) }
		st
	}

print.xml( tag = books.root )
show.xml( tag = books.root )

parts <-
	xmlParse( "parts.xml" )

show.xml( xmlRoot( parts ) )

food <-
	xmlParse( "food.xml" )

show.xml( xmlRoot( food ) )
cat( print.xml( xmlRoot( food ) ) )
print("nn3")
