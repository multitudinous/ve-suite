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
#include <ves/builder/HDFtoVTK/HDFtoVTK.h>
#include <iostream>
int main( int argc, char** argv )
{
    char* inFile = 0;
    char* outdir = 0;
    char* outprefix = 0;
    if( argc < 3 )
    {
        std::cout << "Usage:" << std::endl;
        std::cout << "HDFtoVTK ./infile.hdf ./outDir outname" << std::endl
        << "./infile.hdf ==> path and filename to read in" << std::endl
        << "./outDir     ==> path to write output" << std::endl
        << "outname      ==> prefix(before .vtk extension) for output files" << std::endl;
        exit( 0 );
    }
    else
    {
        inFile = new char[strlen( argv[1] ) + 1];
        strcpy( inFile, argv[1] );
        outdir = new char[strlen( argv[2] ) + 1];
        strcpy( outdir, argv[2] );
        outprefix = new char[strlen( argv[3] ) + 1];
        strcpy( outprefix, argv[3] );
    }
    cfdHDFToVTK* xlatr = new cfdHDFToVTK( inFile );

    xlatr->setVerboseTranslationFlag( 0 );
    xlatr->setType( cfdHDFToVTK::DCLARKE );
    xlatr->viewGridBeforeWriting( 0 );
    xlatr->setOutputVTKDirectory( outdir );
    xlatr->setVTKOutFileName( outprefix );
    xlatr->translateFileToVTK();

    if( xlatr )
    {
        delete xlatr;
        xlatr = 0;
    }
    if( inFile )
    {
        delete [] inFile;
        inFile = 0;
    }
    if( outdir )
    {
        delete [] outdir;
        outdir = 0;
    }
    if( outprefix )
    {
        delete [] outprefix;
        outprefix = 0;
    }
    return 0;
}
