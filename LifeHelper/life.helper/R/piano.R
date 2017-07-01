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

##
# example:
# all keys with their colors, notes, frequencies of a common piano
##
(
    piano <-
        data.frame(
            key   = c( 1 : 88 ),
            color = c( "ebony", "ivory" )[ match( grepl( "#", note.of.key( c( 1 : 88 ) ) ), c( T, F ) ) ],
            note  = factor(
                x      = c( 1 : 88 ),
                levels = c( 1 : 88 ),
                labels = note.of.key( c( 1 : 88 ) ) ),
            frequency  = round(
                frequency.of.key(
                    c( 1 : 88 ) ),
                2 ) ) )
