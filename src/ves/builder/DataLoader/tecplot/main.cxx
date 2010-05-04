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
#include <vtkMultiBlockDataSet.h>
#include <vtkXMLMultiBlockDataWriter.h>

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
        std::cout << "For more information enter: " << argv[ 0 ] << " --help" << std::endl;
        return( 1 );
    }
    
    tecplot::sdk::integration::Manager& manager = 
        tecplot::sdk::integration::Manager::instance(); 

    if( !std::string("--help").compare( argv[ 1 ] ) )
    {
        std::string helpAboutString  = manager.getHelpAbout(); 
        std::cout << helpAboutString << std::endl;
        std::cout << "Description: This program converts ascii and binary tecplot files to vtk format" << std::endl;
        std::cout << "Usage: " << argv[ 0 ] << " tecplot_file1 tecplot_file2 ..." << std::endl;
        std::cout << "Optional commandline flags:" << std::endl;
        std::cout << "   --outputToCurrentDir to write converted files to current directory rather than to location specified in filename path" << std::endl;
        std::cout << "   --ascii to write converted files as plain text" << std::endl;
        std::cout << "   --multiblock to write transient files in multiblock format" << std::endl;
        std::cout << "Note: If get segmentation fault right away, verify that Tecplot SDK evaluation license" << std::endl;
        std::cout << "      file 'sdkeval.lic' is at location specified by environment variable TECSDKHOME.\n" << std::endl;
        return( 0 );
    }

    // Examine commandline flags...
    int outputToCurrentDir = 0;
    int asciiOutput = 0;
    int multiblock = 0;
    for( int i = 1; i < argc; ++i ) // argument array is 0-based, but we won't look at the zeroth one (program name)
    {
        // Look for flag that specifies to output to current directory (used mainly for testing)
        if( !std::string( "--outputToCurrentDir" ).compare( argv[ i ] ) )
        {
            outputToCurrentDir = 1;
        }
        // Look for flag that specifies ascii output (used mainly for testing)
        else if( !std::string( "--ascii" ).compare( argv[ i ] ) )
        {
            asciiOutput = 1;
        }
        else if( !std::string( "--multiblock" ).compare( argv[ i ] ) )
        {
            multiblock = 1;
        }
    }

    // Start the manager before we create an instance of the reader
    manager.OneTimeSetup();

    // argument array is 0-based, but we won't look at the zeroth one (program name)
    for( int i = 1; i < argc; ++i )
    {
        if( !std::string("--outputToCurrentDir").compare( argv[ i ] ) || 
            !std::string("--ascii").compare( argv[ i ] ) ||
            !std::string("--multiblock").compare( argv[ i ] ) )
        {
            continue;
        }

        std::string inputFileNameAndPath( argv[ i ] );

        //std::cout << "\nAttempting to process file '" 
        //    << inputFileNameAndPath << "'" << std::endl;
        tecplotReader* reader = new tecplotReader( inputFileNameAndPath );
        
        vtkMultiBlockDataSet* mb = NULL;

        int numFiles = reader->GetNumberOfOutputFiles();
        if( numFiles == 1 )
        {
            multiblock = 0; // this is for more than one file
        }

        if( multiblock )
        {
            mb = vtkMultiBlockDataSet::New();
            mb->SetNumberOfBlocks( numFiles );
        }

        for( int j = 0; j < numFiles; ++j )
        {
            vtkUnstructuredGrid* ugrid = reader->GetOutputFile( j );

            std::string outputFileName;
            if( numFiles == 1 )
            {
                // create a *.vtu output filename...
                outputFileName = 
                    stripExtension( inputFileNameAndPath ) + ".vtu";
            }
            else
            {
                // Using a zero-based incremental naming scheme, create a *.vtu output filename...
                // Use boost for number-to-string conversion:
                outputFileName = stripExtension( inputFileNameAndPath ) + 
                    "_" + boost::lexical_cast<std::string>( j ) + ".vtu";
            }

            // If outputToCurrentDir flag was set, then write to current location...
            if( outputToCurrentDir )
            {
                outputFileName = extractFileNameFromFullPath( outputFileName );
            }

            if( multiblock )
            {
                // Add the unstructured grid to the multi-block dataset
                // Will write when all grids are added.
                mb->SetBlock( j, ugrid );
                //ugrid->Delete();  //can't do this here
            }
            else
            {
                std::cout << "Writing to file \"" << outputFileName << "\"" << std::endl;

                vtkXMLUnstructuredGridWriter* writer = vtkXMLUnstructuredGridWriter::New();
                writer->SetInput( ugrid );
                writer->SetFileName( outputFileName.c_str() );
                if( asciiOutput )
                {
                    writer->SetDataModeToAscii();
                }
                writer->Write();
                writer->Delete();

                /*ugrid->Delete();
                ugrid = NULL;*/
            }
        }

        // Now it is time to write the multi-block file...
        if( multiblock )
        {
            // create a *.vtu output filename...
            std::string outputFileName = stripExtension( inputFileNameAndPath ) + ".vtu";
            if( outputToCurrentDir )
            {
                outputFileName = extractFileNameFromFullPath( outputFileName );
            }

            vtkXMLMultiBlockDataWriter* writer = vtkXMLMultiBlockDataWriter::New();
            writer->SetInput( mb );
            writer->SetFileName( outputFileName.c_str() );
            if( asciiOutput )
            {
                writer->SetDataModeToAscii();
            }
            //writer->SetWriteMetaFile( 1 );
            writer->Write();
            writer->Delete();
            mb->Delete();
        }

        delete reader;
        std::cout << std::endl;
    }

    // Now lets shut the manager down
    manager.OneTimeCleanup();
    return( 0 );
}

