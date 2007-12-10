/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2007 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
 *   - Reaction Engineering International, www.reaction-eng.com
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 * -----------------------------------------------------------------
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include <cstdio>
#include <string>
#include <iostream>
#include <fstream>
#include <cmath>

#include <vector>

#include <PPConverter.h>

//namespace REI {

PPConverter::PPConverter( std::string pp1_file, std::string pp3_file )
{
    pplot1_file = pp1_file;
    pplot3_file = pp3_file;
}

/*-----------------------------------------------------------------------*/

PPConverter::~PPConverter()
{}

/*-----------------------------------------------------------------------*/

void PPConverter::swap_4_range( char *mem_ptr1, int num )
{
    char *pos;

    pos = mem_ptr1;

    for( int i = 0; i < num; i++ )
    {
        swap_4( pos );
        pos += 4;
    }
}

/*-----------------------------------------------------------------------*/

void PPConverter::swap_4( char* data )
{
    char b;
    b = data[0];
    data[0] = data[3];
    data[3] = b;
    b = data[1];
    data[1] = data[2];
    data[2] = b;
}

/*-----------------------------------------------------------------------*/

int PPConverter::makeVTK( std::string pp_file, std::string pd_file )
{
    char line[500];
    ifstream pplot1, pplot3;

    int total_particle_count;
    int nsl, nps, csl, cps, fcps, fcsl;
    int index, i, j, num_vars;

    // Read PPLOT1 for number of starting locations, sizes and particle counts

    FILE *fptr;
    fptr = fopen( pplot1_file.c_str(), "rt" );

    if( fptr == NULL ) return 0;
    else fclose( fptr );

    fptr = fopen( pplot3_file.c_str(), "rt" );
    if( fptr == NULL ) return 0;
    else fclose( fptr );

    pplot1.open( pplot1_file.c_str() );

    pplot1 >> nsl >> nps;

    std::vector<int> particle_count( nsl*nps );

    total_particle_count = 0;

    for( csl = 0; csl < nsl; csl++ )
    {
        for( cps = 0; cps < nps; cps++ )
        {
            pplot1 >> particle_count[csl*nps+cps];
            total_particle_count += particle_count[csl*nps+cps];
        }
    }

    pplot1.close();

    // Find out how many variables PPLOT3 contains
    char* token;
    pplot3.open( pplot3_file.c_str() );
    pplot3.getline( line, 499 );
    pplot3.getline( line, 499 );
    pplot3.getline( line, 499 );
    pplot3.getline( line, 499 );
    pplot3.close();

    token = strtok( line, " \t" );

    i = 0;
    while( token )
    {
        i++;
        token = strtok( NULL, "/ ,\t\n" );
    }

    num_vars = i - 4;

    // Read data from PPLOT
    pplot3.open( pplot3_file.c_str() );

    pplot3.getline( line, 500 );

    int   cnt, count = 0;
    float lx, ly, lz;

    float *xpts = new float[total_particle_count];
    float *ypts = new float[total_particle_count];
    float *zpts = new float[total_particle_count];

    //float **dpts = new (float*)[num_vars];
    //for(i=0; i<num_vars; i++)
    //  dpts[i] = new float[total_particle_count];
    std::vector<std::vector<float> > dpts( num_vars + 1 );
    for( i = 0; i < num_vars + 1; i++ ) dpts[i].resize( total_particle_count );

    /* Loop through all of the paths */
    for( csl = 0; csl < nsl; csl++ )
    {
        for( cps = 0; cps < nps; cps++ )
        {
            if( particle_count[csl*nps+cps] > 0 )
            {

                lx = ly = lz = -9999.9;

                pplot3 >> index >> fcps >> fcsl;

                /* Make sure loop counters and file position match */
                if( fcps != ( cps + 1 ) || fcsl != ( csl + 1 ) )
                {
                    cerr << "Error in PPConverter\n"
                    << "sizes: " << fcps << " != " << cps + 1 << endl
                    << "locations: " << fcsl << " != " << csl + 1 << endl;
                    exit( 0 );
                }

                for( i = 0, cnt = 0; i < particle_count[csl*nps+cps]; i++ )
                {

                    pplot3 >> index >> xpts[count] >> ypts[count] >> zpts[count];

                    for( j = 0; j < num_vars; j++ )
                        pplot3 >> dpts[j][count];

                    dpts[j][count] = float( csl * nps + cps );

                    if( xpts[count] != lx || ypts[count] != ly || zpts[count] != lz )
                    {
                        lx = xpts[count];
                        ly = ypts[count];
                        lz = zpts[count];
                        cnt++;
                        count++;
                    }
                }
                particle_count[csl*nps+cps] = cnt;
            }
        }
    }

    pplot3.close();

    int floatSize = sizeof( float );
    int intSize = sizeof( int );

    FILE *VTK_FILE;

    if (( VTK_FILE = fopen( pp_file.c_str(), "wb" ) ) == NULL )
    {
        cerr << "Failed to open " << pp_file << endl;
        exit( 0 );
    }

    fprintf( VTK_FILE, "# vtk DataFile Version 4.0\n" );
    fprintf( VTK_FILE, "Data from %s PPLOTs\n", pp_file.c_str() );
    fprintf( VTK_FILE, "BINARY\n" );
    fprintf( VTK_FILE, "DATASET POLYDATA\n" );

    //# Assinine byte swapping for vtk
    swap_4_range(( char* )xpts, count );
    swap_4_range(( char* )ypts, count );
    swap_4_range(( char* )zpts, count );

    fprintf( VTK_FILE, "POINTS %d float\n", count );
    for( i = 0; i < count; i++ )
    {
        fwrite(( char* )&xpts[i], floatSize, 1, VTK_FILE );
        fwrite(( char* )&ypts[i], floatSize, 1, VTK_FILE );
        fwrite(( char* )&zpts[i], floatSize, 1, VTK_FILE );
    }

    int line1 = 2, line2;
    //swap_4((char*)&line1);
    //fprintf(VTK_FILE, "\nLINES %d %d\n", count-1, (count-1)*3);
    //for(i=1; i<count; i++) {
    //  line2 = i-1;
    //  line3 = i;
    //  swap_4((char*)&line2);
    //  swap_4((char*)&line3);

    //  fwrite((char*)&line1, intSize, 1, VTK_FILE);
    //  fwrite((char*)&line2, intSize, 1, VTK_FILE);
    //  fwrite((char*)&line3, intSize, 1, VTK_FILE);
    //}

    cnt = 0;
    for( csl = 0; csl < nsl; csl++ )
        for( cps = 0; cps < nps; cps++ )
            if( particle_count[csl*nps+cps] > 0 ) cnt++;

    fprintf( VTK_FILE, "\nLINES %d %d\n", cnt, cnt + count );// - 1);

    cnt = 0;
    for( csl = 0; csl < nsl; csl++ )
    {
        for( cps = 0; cps < nps; cps++ )
        {
            if( particle_count[csl*nps+cps] > 0 )
            {
                line1 = particle_count[csl*nps+cps];
                swap_4(( char* )&line1 );
                fwrite(( char* )&line1, intSize, 1, VTK_FILE ); // how many points on path
                for( i = cnt; i < cnt + particle_count[csl*nps+cps]; i++ )
                {
                    line2 = i;
                    swap_4(( char* )&line2 );
                    fwrite(( char* )&line2, intSize, 1, VTK_FILE ); // point index
                }
                cnt += particle_count[csl*nps+cps];
            }
        }
    }

    fprintf( VTK_FILE, "\nPOINT_DATA %d", count );

    num_vars++;
    for( i = 0; i < num_vars; i++ )
        for( j = 0; j < total_particle_count; j++ )
            swap_4(( char* )&dpts[i][j] );

    for( i = 0; i < num_vars; i++ )
    {
        fprintf( VTK_FILE, "\nSCALARS scalar%d float 1\n", i );
        fprintf( VTK_FILE, "LOOKUP_TABLE default\n" );
        for( j = 0; j < count; j++ )
            fwrite(( char* )&dpts[i][j], floatSize, 1, VTK_FILE );
    }

    fclose( VTK_FILE );

    /*if((VTK_FILE = fopen(pd_file.c_str(), "wt"))==NULL) {
      cerr << "Failed to open " << pd_file << endl;
      exit(0);
    }

    fprintf(VTK_FILE, "%d\n", nsl);
    fprintf(VTK_FILE, "%d\n", nps);

    cnt = 0;
    fprintf(VTK_FILE, "%d\n", cnt);
    for(csl=0; csl<nsl; csl++)
      for(cps=0; cps<nps; cps++) {
        cnt += particle_count[csl*nps+cps];
        fprintf(VTK_FILE, "%d\n", cnt);
      }

     fclose(VTK_FILE); */

    if( xpts ) delete( xpts );
    if( ypts ) delete( ypts );
    if( zpts ) delete( zpts );

//  for(i=0; i<num_vars; i++)
    //  if(dpts[i]) delete(dpts[i]);
    //if(dpts) delete(dpts);

    return 1;
}

//} // End namespace REI
