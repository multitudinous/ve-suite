#!/usr/bin/python

file = open ("saylorville.xyz")

outfile = open ( "saylorville.csv", "w" )

outfile.write ("Easting,Northing,Elevation\n" );
miny = 10000000.0
maxy = 0.0

minx = 10000000.0
maxx = 0.0

for line in file:
    parts = line.split ( ' ' )
    outfile.write ( parts[0] )
    outfile.write ( "," )
    outfile.write ( parts[1] )
    outfile.write ( "," )
    outfile.write ( parts[2] )
    #print "%s %s" %( parts[ 0 ], minxx )
    #min = parts[ 0 ]
    if float( parts[ 0 ] ) < float( minx ):
        minx = parts[0]
    elif float( maxx ) < float( parts[0] ):
        maxx = parts[0]
    #print "%s %s" %( float( parts[1] ), float( miny ) )
    if float( parts[1] ) < float( miny ):
        miny = parts[1]
    elif float( maxy ) < float( parts[1] ):
        maxy = parts[1]

file.close();
outfile.close();

print "%s %s %s %s" %( minx, maxx, miny, maxy )
