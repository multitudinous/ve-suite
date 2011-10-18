/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2011 by Iowa State University
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
#include <vtkMultiBlockDataSet.h>
#include <vtkXMLMultiBlockDataWriter.h>

#include <boost/algorithm/string/trim.hpp>
#include <boost/filesystem/operations.hpp>

#include <sstream>
#include <iomanip>

using namespace ves::builder::DataLoader;

int main( int argc, char** argv )
{
    if( argc < 2 )
    {
        std::cout << "Error: Need at least one argument specifying a tecplot filename!" << std::endl;
        std::cout << "For more information enter: " << argv[ 0 ] << " --help" << std::endl;
        return( 1 );
    }
    
    tecplot::sdk::integration::Manager& manager = 
        tecplot::sdk::integration::Manager::instance(); 

    // Examine commandline flags...
    int outputToCurrentDirFlag = 0;
    int asciiOutputFlag = 0;
    int multiBlockFlag = 0;
    for( int i = 1; i < argc; ++i ) // argument array is 0-based, but we won't look at the zeroth one (program name)
    {
        // Look for double dash that indicates a flag
        if( !std::string( "--" ).compare( 0, 2, argv[ i ], 0, 2 ) )
        {
            //std::cout << "found flag" << std::endl;
            // Look for flag that requests help
            if( !std::string( "--help" ).compare( argv[ i ] ) )
            {
                std::string helpAboutString  = manager.getHelpAbout(); 
                std::cout << helpAboutString << std::endl;
                std::cout << "Description: This program converts ascii and binary tecplot files to vtk format" << std::endl;
                std::cout << "Usage: " << argv[ 0 ] << " tecplot_file1 tecplot_file2 ..." << std::endl;
                std::cout << "Optional commandline flags:" << std::endl;
                std::cout << "   --outputToCurrentDir to write converted files to current directory rather than to location specified in filename path" << std::endl;
                std::cout << "   --ascii to write converted files as plain text" << std::endl;
                std::cout << "   --multiblock to write data in multi-block file format" << std::endl;
                std::cout << "Note: If get segmentation fault right away, verify that Tecplot SDK evaluation license" << std::endl;
                std::cout << "      file 'sdkeval.lic' is at location specified by environment variable TECSDKHOME.\n" << std::endl;
                return( 0 );
            }
            // Look for flag that specifies to output to current directory (used mainly for testing)
            else if( !std::string( "--outputToCurrentDir" ).compare( argv[ i ] ) )
            {
                outputToCurrentDirFlag = 1;
            }
            // Look for flag that specifies ascii output (used mainly for testing)
            else if( !std::string( "--ascii" ).compare( argv[ i ] ) )
            {
                asciiOutputFlag = 1;
            }
            // Look for flag that specifies multi-block file format
            else if( !std::string( "--multiblock" ).compare( argv[ i ] ) )
            {
                multiBlockFlag = 1;
            }
            else
            {
                std::cerr << "\nError: Unrecognized flag '" << argv[ i ] << "'" << std::endl;
                std::cerr << "For more information enter: " << argv[ 0 ] << " --help\n" << std::endl;
                return( 1 );
            }
        }
    }

    // Start the manager before we create an instance of the reader
    manager.OneTimeSetup();

    // argument array is 0-based, but we won't look at the zeroth one (program name)
    for( int i = 1; i < argc; ++i )
    {
        // skip over any of the commmandline flags...
        if( !std::string("--help").compare( argv[ i ] ) || 
            !std::string("--outputToCurrentDir").compare( argv[ i ] ) || 
            !std::string("--ascii").compare( argv[ i ] ) ||
            !std::string("--multiblock").compare( argv[ i ] ) )
        {
            continue;
        }

        // Start a new reader object using the current commandline argument as input filename...
        std::string inputFileNameAndPath( argv[ i ] );
        tecplotReader* reader = new tecplotReader( inputFileNameAndPath );
        
        std::string fileExtension(".vtu");
        if( multiBlockFlag )
        {
            reader->SetMultiBlockOn();
            fileExtension = ".vtm";
        }

        // cycle over each timestep and either put all zone data in a single file or in seperate files for multiblock option
        int numTimesteps = reader->GetNumberOfTimesteps();
        for( int j = 0; j < numTimesteps; ++j )
        {
            // Get dataset for a particular timestep...
            vtkDataObject* dataObject = reader->GetOutput( j );

            // create an output filename for static or transient case...
            std::string outputFileName;
            if( numTimesteps == 1 )
            {
                outputFileName = 
                    boost::filesystem::path( inputFileNameAndPath ).replace_extension( fileExtension ).string();
            }
            else
            {
                // Using a zero-based incremental naming scheme, create an output filename...
                std::ostringstream ss;
                ss << std::setw( 3 ) << std::setfill( '0' ) << j;
                
                outputFileName =  
                    boost::filesystem::path( inputFileNameAndPath ).replace_extension("").string() + 
                    "_" + ss.str() + fileExtension;
            }

            // If outputToCurrentDirFlag was set, then write to current location...
            if( outputToCurrentDirFlag )
            {
                outputFileName = boost::filesystem::path( outputFileName ).leaf().string();
            }

            std::cout << "Writing to file \"" << outputFileName << "\"" << std::endl;

            if( multiBlockFlag )
            {
                vtkXMLMultiBlockDataWriter* writer = vtkXMLMultiBlockDataWriter::New();
                writer->SetInput( dataObject );
                writer->SetFileName( outputFileName.c_str() );
                writer->SetDataModeToBinary();
                if( asciiOutputFlag )
                {
                    writer->SetDataModeToAscii();
                }
                writer->SetWriteMetaFile( 1 );  // causes creation of *.vtm meta file
                writer->Write();
                writer->Delete();                    
            }
            else
            {
                vtkXMLUnstructuredGridWriter* writer = vtkXMLUnstructuredGridWriter::New();
                writer->SetInput( dataObject );
                writer->SetFileName( outputFileName.c_str() );
                writer->SetDataModeToBinary();
                if( asciiOutputFlag )
                {
                    writer->SetDataModeToAscii();
                }
                writer->Write();
                writer->Delete();
            }
        }

        delete reader;
        std::cout << std::endl;
    }

    // Now let's shut the manager down
    manager.OneTimeCleanup();
    return( 0 );
}

