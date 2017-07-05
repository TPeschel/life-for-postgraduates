#' key.of.frequency
#'
#' @param frq
#'
#' @return key of frq
#' @export
#'
#' @examples
#' key.of.frequency( 110 )
key.of.frequency <-
    function( frq = 110 ) {
        log2( frq / 440 ) * 12 + 49 }

#' frequency.of.key
#'
#' @param key
#'
#' @return frequency of the key
#' @export
#'
#' @examples
#' frequency.of.key( 25 )
frequency.of.key <-
    function( key = 25 ) {
        440 * 2 ** ( ( key - 49 ) / 12 ) }

#' note.of.key
#'
#' @param key
#'
#' @return note of key
#' @export
#'
#' @examples
#' note.of.key( 25 )
note.of.key <-
    function( key = c( 4, 6, 8, 9, 11, 13, 15 ) ) {
        paste0(
            c(
                "A", "A#",
                "B",
                "C", "C#",
                "D", "D#",
                "E",
                "F", "F#",
                "G", "G#" )[ 1 + ( ( key - 1 ) %% 12 ) ],
            ( key - 1 ) %/% 12 ) }

#' key.of.note
#'
#' @param note
#'
#' @return key of note
#' @export
#'
#' @examples
#' key.of.note( "A2" )
key.of.note <-
    function( note ) {
        if( !require( stringr ) ) {
            install.packages( "stringr" ) }
        base <-
            c(
                "A", "A#",
                "B",
                "C", "C#",
                "D", "D#",
                "E",
                "F", "F#",
                "G", "G#" )
        a <-
            match(
                str_extract( note, "[A-Z]+#*"),
                base )
        b <-
            as.numeric( str_extract( note, "[0-9]$") )
        a + b * 12 }

#' note.of.frequency
#'
#' @param frequency
#'
#' @return note of frequency
#' @export
#'
#' @examples
#' note.of.frequency( 110 )
note.of.frequency <-
    function( frequency = 110 ) {
        note.of.key( key.of.frequency( frequency ) ) }

#' frequency.of.note
#'
#' @param note
#'
#' @return frequency of note
#' @export
#'
#' @examples
#' frequency.of.note( "A2" )
frequency.of.note <-
    function( note ) {
        frequency.of.key( key.of.note( note ) ) }

#' piano
#'
#' @param left.key
#' @param right.key
#'
#' @return piano with keys, their colors, notes and frequencies
#' @export
#'
#' @examples
#' piano( 4, 88 )
piano <-
    function( left.key = 3, right.key = 88 ) {
        k <-
            c( left.key : right.key )
        data.frame(
            key   = k,
            color = c( "ebony", "ivory" )[ match( grepl( "#", note.of.key( k ) ), c( T, F ) ) ],
            note  = factor(
                x      = k,
                levels = k,
                labels = note.of.key( k ) ),
            frequency  = frequency.of.key( k ) ) }

##
# example:
# all keys with their colors, notes, frequencies of a common piano
##
