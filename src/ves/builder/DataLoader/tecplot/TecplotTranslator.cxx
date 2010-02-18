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
#include <ves/builder/DataLoader/tecplot/TecplotTranslator.h>
#include <ves/builder/DataLoader/tecplot/tecplotReader.h>

#include <vtkUnstructuredGrid.h>
#include <vtkXMLUnstructuredGridWriter.h>

#include <boost/lexical_cast.hpp>

using namespace ves::builder::DataLoader;
using namespace ves::builder::cfdTranslatorToVTK;

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
    sep = '\';
#endif

    size_t i = s.rfind(sep, s.length());
    if( i != std::string::npos )
    {
        return( s.substr(i+1, s.length() - i) );
    }

    return( s );
} 

TecplotTranslator::TecplotTranslator()
{
    SetTranslateCallback( &tecplotToVTK );
    SetPreTranslateCallback( &_cmdParser );
}

TecplotTranslator::~TecplotTranslator()
{}

void TecplotTranslator::TecplotPreTranslateCbk::Preprocess( int argc, char** argv,
                                                            cfdTranslatorToVTK* toVTK )
{
    PreTranslateCallback::Preprocess( argc, argv, toVTK );
}

void TecplotTranslator::TecplotTranslateCbk::Translate( vtkDataObject*& outputDataset,
                                                        cfdTranslatorToVTK* toVTK,
                                                        vtkAlgorithm*& dataReader )
{
    TecplotTranslator* tecplotTransVTK = dynamic_cast<TecplotTranslator*>( toVTK );
    if( ! tecplotTransVTK )
    {
        return;
    }

    //std::cout << "\nAttempting to process file '" << tecplotTransVTK->GetFile( 0 ) << "'" << std::endl;
    tecplotReader* tecplot = new tecplotReader( tecplotTransVTK->GetFile( 0 ) );

    int numFiles = tecplot->GetNumberOfOutputFiles();
    std::cout << "tecplot->GetNumberOfOutputFiles() = " << numFiles << std::endl;

    for( int i = 0; i < numFiles; i++ )
    {
        vtkUnstructuredGrid * ugrid = tecplot->GetOutputFile( i );

        std::string outputFileName;
        if( numFiles == 1 )
        {
            // create a *.vtu output filename to be written to current location...
            outputFileName = stripExtension( extractFileNameFromFullPath( tecplotTransVTK->GetFile( 0 ) ) ) + ".vtu";
        }
        else
        {
            // Using a zero-based incremental naming scheme, create a *.vtu output filename to be written to current location...
            // Use boost for number-to-string conversion:
            outputFileName = stripExtension( extractFileNameFromFullPath( tecplotTransVTK->GetFile( 0 ) ) ) 
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

    delete tecplot;
}

void TecplotTranslator::DisplayHelp( void )
{
    std::cout << "|\tTecplot Translator Usage:" << std::endl
    << "\t -singleFile <rst_filename_to_load> -o <output_dir> "
    << "-outFileName <output_filename> -loader rst -w file" << std::endl;
}

