/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2009 by Iowa State University
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
 *************** <auto-copyright.rb END do not edit this line> ***************/
#include <ves/builder/DataLoader/tecplot/tecplotReader.h>

#include <vtkUnstructuredGrid.h>
#include <vtkXMLUnstructuredGridWriter.h>

#include <boost/lexical_cast.hpp>

using namespace ves::builder::DataLoader;

std::string stripExtension( const std::string& s )
{
    char sep = '.';

    size_t i = s.rfind(sep, s.length());
    if( i != std::string::npos )
    {
        return( s.substr(0, i ) );
    }

    return( s );
} 

std::string extractFileNameFromFullPath( const std::string& s )
{
    char sep = '/';

#ifdef WIN32
    sep = '\\';
#endif

    size_t i = s.rfind(sep, s.length());
    if( i != std::string::npos )
    {
        return( s.substr(i+1, s.length() - i) );
    }

    return( s );
} 

int main( int argc, char** argv )
{
    if( argc < 2 )
    {
        std::cout << "Error: Need at least one argument specifying a tecplot filename!" << std::endl;
        std::cout << "Usage: " << argv[ 0 ] << " tecplot_file1 tecplot_file2 ..." << std::endl;
        std::cout << "Note: If get segmentation fault right away, verify that Tecplot SDK evaluation license" << std::endl;
        std::cout << "      file 'sdkeval.lic' is at location specified by environment variable TECSDKHOME.\n" << std::endl;
        return( 1 );
    }
    
    if( argv[ 1 ] == "--help" )
    {
        //std::string helpAboutString; 
        //Manager& manager = Manager::getInstance(); 
        //helpAboutString  = manager.getHelpAbout(); 
        //std::cout << helpAboutString << std::endl;
        return( 0 );
    }

    for( int i = 1; i < argc; i++ ) // argument array is 0-based, but we won't use the zeroth one (program name)
    {
        std::string inputFileNameAndPath( argv[ i ] );

        //std::cout << "\nAttempting to process file '" << inputFileNameAndPath << "'" << std::endl;
        tecplotReader* reader = new tecplotReader( inputFileNameAndPath );

        int numFiles = reader->GetNumberOfOutputFiles();
        std::cout << "reader->GetNumberOfOutputFiles() = " << numFiles << std::endl;

        for( int i = 0; i < numFiles; i++ )
        {
            vtkUnstructuredGrid * ugrid = reader->GetOutputFile( i );

            std::string outputFileName;
            if( numFiles == 1 )
            {
                // create a *.vtu output filename to be written to current location...
                outputFileName = stripExtension( extractFileNameFromFullPath( inputFileNameAndPath ) ) + ".vtu";
            }
            else
            {
                // Using a zero-based incremental naming scheme, create a *.vtu output filename to be written to current location...
                // Use boost for number-to-string conversion:
                outputFileName = stripExtension( extractFileNameFromFullPath( inputFileNameAndPath ) ) 
                                        + "-" + boost::lexical_cast<std::string>( i ) + ".vtu";
            }
            std::cout << "Writing to file \"" << outputFileName << "\"\n" << std::endl;

            vtkXMLUnstructuredGridWriter *writer = vtkXMLUnstructuredGridWriter::New();
            writer->SetInput( ugrid );
            writer->SetFileName( outputFileName.c_str() );
            writer->SetDataModeToAscii();
            writer->Write();
            writer->Delete();
        }

        delete reader;
    }

    return( 0 );
}

