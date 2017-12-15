use DBI;

my $dbfile = 'a2voice20150320_DiVAS.sqlite';      # your database file
my $dbh = DBI->connect(          # connect to your database, create if needed
    "dbi:SQLite:dbname=$dbfile", # DSN: dbi, driver, database file
    "",                          # no user
    "",                          # no password
    { RaiseError => 1 },         # complain if something goes wrong
) or die $DBI::errstr;

## $sth = $dbh->table_info();
## my @row = $sth->fetchrow_array();

$readingkey = "a2voice20150320_";
$datadir = "A2-Voice_2015-03-20/Rohdaten/";

open(OUT,">res.txt");

print OUT "patid|cohort|sic|filename|midname|sex|street|bday|zip|location|country|id|commentpat|timestamp|versionnr|" .
    "sessid|phid|commentsess|archivlab|archivdir|sesstype|sesspic|create_at|modified_at|archived_at|locked|diagid|".
    "IsHoldPeriod|mpt|jitter|dsi|" .
    "sequenz|energy|frequ|holdper|proftype|timestampfile\n";


my $stmt = qq(SELECT * from patients, sessions, VoiceRangeAttributes where patients.PatID=sessions.PatID and VoiceRangeAttributes.SessID=sessions.SessId);
my $sth = $dbh->prepare( $stmt );
$sth->execute();


my $zaehler=0;
while(my ($patid,$cohort,$sic,$midname,$sex,$street,$bday,$zip,$location,$country,$id,$commentpat,$timestamp,
	  $sessid,$patid2,$phid,$commentsess,$archivlab,$archivdir,$sesstype,$sesspic,$create_at,$modified_at,$archived_at,$locked,$diagid,
	  $IsHoldPeriod,$mpt,$jitter,$dsi) = $sth->fetchrow_array){
    $filename =  $datadir . $readingkey . sprintf( "%04d",$patid) . "_" . sprintf( "%04d",$sessid) . ".vrp";
    my $datapoint=0;
    my $tmp;
    my $versionnr;
    $sic =~ s/ //g;
    $cohort =~ s/ //g;
 
    # if( $sic =~ /^[^L]|^[0-9]{3}$/i) {
    # 	print "$filename\n";
    # 	print "$sic | $cohort\n";
    # }

    if( $cohort =~ /LI[^f]/i) {
	($sic,$cohort) = ($cohort,$sic)
    };
    if( $sic =~ /^[0-9]{8}$/i) {
	print "$filename\n";
	print "$sic | $cohort\n";
	$sic = "LI" . "$sic";
	print "$sic | $cohort\n"
    }
    next if $sic =~ /LIFEADULT|test|schlomo|Film/;
    if( !($sic =~ /^LI[0-9X]{8}$/i)) {
	print "$filename\n";
	print "$sic | $cohort\n";
	next;
    }

    open(FILE, $filename);## or die $!;
    while(<FILE>){
	chomp;
	chop;
	my @zeile = split /:/, $_ ;
	if(/;/){
	    $versionnr = shift @zeile;
	} else { shift @zeile }
	while($zeile[0] =~ /V/ && scalar @zeile > 0){ 
	    $datapoint++;
	    $weg2 = shift @zeile;
##	    print "weg2: $weg2\n";
	}
	# if($zeile[0] =~ /[^0-9]/ ){
	#     $weg = shift @zeile;
	#     print "weg: $weg\n";
	# }

	if(scalar @zeile != 6){
	    print "zeile unten @zeile\n" unless scalar @zeile == 0;
	    next;
	}
	my ($sequenz,$energy,$frequ,$holdper,$proftype,$timestampfile) = @zeile;
##	print "@zeile\n";

	print OUT "$patid|$cohort|$sic|$filename|$midname|$sex|$street|$bday|$zip|$location|$country|$id|$commentpat|$timestamp|$versionnr|" .
	    "$sessid|$phid|$commentsess|$archivlab|$archivdir|$sesstype|$sesspic|$create_at|$modified_at|$archived_at|$locked|$diagid|$IsHoldPeriod|$mpt|$jitter|$dsi".
	    "|$sequenz|$energy|$frequ|$holdper|$proftype|$timestampfile\n";

    }
    close FILE;
    $zaehler++;
}

print "$zaehler\n";


