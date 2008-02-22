/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2008 by Iowa State University
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
#include <ves/builder/DataLoader/DataLoader.h>
#include <ves/builder/DataLoader/FluentTranslator.h>
#include <ves/builder/DataLoader/MFIXTranslator.h>
#include <ves/builder/DataLoader/EnSightTranslator.h>
#include <ves/builder/DataLoader/AVSTranslator.h>
#include <ves/builder/DataLoader/cfdREITranslator.h>
#include <ves/builder/DataLoader/cfdDICOMTranslator.h>
#include <ves/builder/DataLoader/plot3dReader.h>
#include <ves/builder/DataLoader/StarCDTranslator.h>
#include <ves/builder/DataLoader/AnsysTranslator.h>

#include <ves/builder/cfdTranslatorToVTK/cfdTranslatorToVTK.h>

#include <ves/xplorer/util/fileIO.h>
#include <vtkDataObject.h>
#include <iostream>

using namespace ves::builder::DataLoader;
using namespace ves::builder::cfdTranslatorToVTK;
//////////////////////
//Constructor       //
//////////////////////
DataLoader::DataLoader()
{
    inputDataName =  '\0';
    activeLoader = 0;
    // load up the translator map
    // AVS
    translatorMap[ "avs" ] = new AVSTranslator();
    // REI
    translatorMap[ "BANFDB" ] = new cfdREITranslator();
    // DICOM
    translatorMap[ "dcm" ] = new cfdDICOMTranslator();
    // Fluent
    translatorMap[ "cas" ] = new FluentTranslator();
    // MFIX
    translatorMap[ "mfix" ] = new MFIXTranslator();
    // EnSight
    translatorMap[ "ens" ] = new EnSightTranslator();
    // Plot3D
    translatorMap[ "xyz" ] = new plot3dReader();
    // StarCD
    translatorMap[ "star" ] = new StarCDTranslator();
    // ANSYS
    translatorMap[ "rst" ] = new AnsysTranslator();
}
///////////////////////////////////////////////////////////////////////////
DataLoader::~DataLoader()
{
    // Clear the translator map
    //translatorMap
}
///////////////////////////////////////////////////////////////////////////
DataLoader::DataLoader( const DataLoader& input )
{
    ;
}
///////////////////////////////////////////////////////////////////////////
DataLoader& DataLoader::operator=( const DataLoader& input )
{
    if( this != &input )
    {
        ;
    }
    return *this;
}
///////////////////////////////////////////////////////////////////////////
vtkDataObject* DataLoader::GetVTKDataSet( int argc, char** argv )
{
    //Data processing loop
    std::string fileExtension = ves::xplorer::util::fileIO::getExtension( inputDataName );
    if( fileExtension.empty() )
    {
        // an example of this would be rei data
        fileExtension = inputDataName;
    }

    // could extract command line args to get loader to use
    // in addition to the above method
    std::string tempExtension;
    if( translatorMap[ "dcm" ]->_extractOptionFromCmdLine( argc, argv, "-loader", tempExtension ) )
    {
        fileExtension = tempExtension;
    }
    else
    {
        std::map< std::string, ves::builder::cfdTranslatorToVTK::cfdTranslatorToVTK* >::iterator iter;
        for( iter = translatorMap.begin(); iter != translatorMap.end(); ++iter )
        {
            iter->second->DisplayHelp();
        }
        return 0;
    }
    //Check and see if we have a loader
    std::map< std::string, ves::builder::cfdTranslatorToVTK::cfdTranslatorToVTK* >::iterator iter;
    iter = translatorMap.find( fileExtension );
    if( iter == translatorMap.end() )
    {
        std::cout << "|\tLoader " << fileExtension << " not supported." << std::endl;
        return 0;
    }
    // process data with appropriate loader
    activeLoader = translatorMap[ fileExtension ];
    if( argc < 1 )
    {
        activeLoader->SetInputDirectory( inputDataDir );
        activeLoader->AddFoundFile( inputDataName );
    }
    activeLoader->TranslateToVTK( argc, argv );
    vtkDataObject* tempDataset = activeLoader->GetVTKFile( 0 );
    return tempDataset;
}
///////////////////////////////////////////////////////////////////////////
void DataLoader::SetInputData( std::string inputData, std::string inputDir )
{
    inputDataName = inputData;
    inputDataDir = inputDir;
}
///////////////////////////////////////////////////////////////////////////
cfdTranslatorToVTK* DataLoader::GetActiveTranslator( void )
{
    return activeLoader;
}

