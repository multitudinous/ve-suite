/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2006 by Iowa State University
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
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "VE_Builder/Translator/DataLoader/DataLoader.h"
#ifdef FLUENT_MFIX
#include "VE_Builder/Translator/DataLoader/FluentTranslator.h"
#include "VE_Builder/Translator/DataLoader/MFIXTranslator.h"
#endif
#include "VE_Builder/Translator/DataLoader/EnSightTranslator.h"
#include "VE_Builder/Translator/DataLoader/AVSTranslator.h"
#include "VE_Builder/Translator/DataLoader/cfdREITranslator.h"
#include "VE_Builder/Translator/DataLoader/cfdDICOMTranslator.h"
#include "VE_Builder/Translator/DataLoader/plot3dReader.h"
#include "VE_Builder/Translator/DataLoader/StarCDTranslator.h"

#include "VE_Xplorer/Utilities/fileIO.h"

using namespace VE_Builder;
//////////////////////
//Constructor       //
//////////////////////
DataLoader::DataLoader()
{
   inputDataName =  '\0';
   activeLoader = 0;
   // load up the translator map
   // AVS
   translatorMap[ "avs" ] = new VE_Builder::AVSTranslator();
   // REI
   translatorMap[ "BANFDB" ] = new VE_Builder::cfdREITranslator();
   // DICOM
   translatorMap[ "dcm" ] = new VE_Builder::cfdDICOMTranslator();
#ifdef FLUENT_MFIX
   // Fluent
   translatorMap[ "cas" ] = new VE_Builder::FluentTranslator();
   // MFIX
   translatorMap[ "mfix" ] = new VE_Builder::MFIXTranslator();
#endif
   // EnSight
   translatorMap[ "ens" ] = new VE_Builder::EnSightTranslator();
   // Plot3D
   translatorMap[ "xyz" ] = new VE_Builder::plot3dReader();
   // StarCD
   translatorMap[ "star" ] = new VE_Builder::StarCDTranslator();
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
DataLoader& DataLoader::operator=( const DataLoader& input)
{
   if ( this != &input )
   {
      ;
   }
   return *this;
}
///////////////////////////////////////////////////////////////////////////
vtkDataSet* DataLoader::GetVTKDataSet( int argc, char** argv )
{
   //Data processing loop
   std::string fileExtension = VE_Util::fileIO::getExtension( inputDataName );
   if ( fileExtension.empty() )
   {
      // an example of this would be rei data
      fileExtension = inputDataName;
   }

   // could extract command line args to get loader to use
   // in addition to the above method
   std::string tempExtension;
   if ( translatorMap[ "cas" ]->_extractOptionFromCmdLine( argc, argv, "-loader", tempExtension ) )
   {
      fileExtension = tempExtension;
   }

   // process data with appropriate loader
   activeLoader = translatorMap[ fileExtension ];
   if ( argc < 1 )
   {
      activeLoader->SetInputDirectory( inputDataDir );
      activeLoader->AddFoundFile( inputDataName );
   }
   activeLoader->TranslateToVTK( argc, argv );
   vtkDataSet* tempDataset = activeLoader->GetVTKFile( 0 );
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
   
