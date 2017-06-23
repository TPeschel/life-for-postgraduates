key.of.frequency <-
    function( frq ) log2( frq / 440 ) * 12 + 49

frequency.of.key <-
    function( id ) 440 * 2 ** ( ( id - 49 ) / 12 )

notes.of.nth.octave <-
    function( n ) {
        octave.keys.engl <-
            c( "A0", "A#0", "B0", "C1", "C#1", "D1", "D#1", "E1", "F1", "F#1", "G1", "G#1" )
        num <-
            as.numeric( substr( octave.keys.engl, nchar( octave.keys.engl ), nchar( octave.keys.engl ) ) )
        note  <-
            substr( octave.keys.engl, 1, nchar( octave.keys.engl ) - 1 )
        paste0( note, num + n ) }

piano.key.notes.frequencies <-
    data.frame(
        key  = c( 1 : 88 ),
        note = sapply( sapply( c( 0 : 7 ),  notes.of.nth.octave ), c )[ 1 : 88 ],
        frq  = round( sapply( c( 1 : 88 ), frequency.of.key ), 1 ) )
