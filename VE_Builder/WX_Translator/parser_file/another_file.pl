#use strict;
#use warnings;

# section for functions
sub lookForvarName
{
	my $tempVarName = "p";
	#print "Hi i'm a fcn! with parameter ".$_[0]."\n";
	
	my @currentLine; #this array has the line currently in $eachLine
	push(@currentLine, split(/([",])/, $_[0])); #new form of separating the string, so much easier than what I was going to do.
	shift(@currentLine); #eliminates a whitespace at the begining of the array
	pop(@currentLine); #eliminates a whitespace at the end of the array

	for(my $i=0; $i<@currentLine; $i++)
	{
	
		#print $i."\t".$currentLine[$i]."\n";
		if($currentLine[$i] =~ /\"/ && $i==0)
		{
			$tempVarName=$currentLine[$i+1];
		}
	} #end of for loop going through @currentLine
	
	return $tempVarName;
}

sub lookForvarIndex
{
	#print "The value is: ".$_[0]."\n";
	my @anotherLine;
	push(@anotherLine, split(/([\(\ )])/, $_[0]));
	shift(@anotherLine);
	pop(@anotherLine);
	for(my $v=0; $v<@anotherLine; $v++)
	{
		#print $v."\t".$anotherLine[$v]."\n";
		if($v==5)
		{
			return $anotherLine[$v];
		}
	}
}

my $eachLine; #searches the line to see if it has (3300 (numbers here)
my $tempLine; #stores the previous line, where the variable name is in
my $varName = "nothing"; #variable looked for, should only have one name at a time
my $tmpVarName; #temporary variable, can be repeated with the same name
my @varArrayName; #array where all the variable names will be stored
my @varArrayIndex; #array where all the indexes of the variable names will be stored
my $tmpVarIndex;

my $file="example.txt";
my $file2="example2.txt";
my $file_cas="combustor2D.cas";
my $file_dat="/home/bert/projects/doe/files/temp/combustor2D.dat";
#my $file_dat=$ARGV[0];
open(INFILE, "< $file_dat") or die "Unable to open file";
#my $buf;
binmode(INFILE);

my @Line = <INFILE>; #array where the whole file is stored
close(INFILE);
#print "File has ".@Line." lines! Using an array.\n";

foreach $eachLine (@Line)
{
	if($eachLine =~ /(^\(3300\s\([0-9 ]+\)$)/)
	{
		#print "Found something $eachLine\n";
		#go and search the previous line for the variable name
		$tmpVarName = &lookForvarName($tempLine); #searches the previous line for the name of the variable
		if($varName eq $tmpVarName)
		{
			next;
		}
		else
		{
			push(@varArrayName, $tmpVarName);
			$varName=$tmpVarName;
			push(@varArrayIndex, &lookForvarIndex($eachLine));
			#$tmpVarIndex = &lookForvarIndex($eachLine);
			
		}
	}
	else
	{
		$tempLine = $eachLine;
	}
	#print "the value of varName: $varName\n";
}

#part to output the findings
$outfile="parser_file/varNamesandIndex.txt";
open(OUTFILE, ">$outfile");
#print OUTFILE "Variable Name\t\tIndex Number\n";
for (my $h=0; $h<@varArrayName; $h++)
{
	print OUTFILE $varArrayName[$h]." ".$varArrayIndex[$h]."\n";
}

close(OUTFILE);
