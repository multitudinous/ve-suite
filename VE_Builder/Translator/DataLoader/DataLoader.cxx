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
 * File:          $RCSfile: DataLoader.cxx,v $
 * Date modified: $Date: 2006-03-18 17:23:46 -0600 (Sat, 18 Mar 2006) $
 * Version:       $Rev: 3936 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "VE_Builder/Translator/DataLoader/DataLoader.h"
#include "VE_Builder/Translator/DataLoader/FluentTranslator.h"
#include "VE_Builder/Translator/AVSTranslator/AVSTranslator.h"
#include "VE_Builder/Translator/REItoVTK/cfdREIToVTK.h"
#include "VE_Builder/Translator/cfdDICOMTranslator/cfdDICOMTranslator.h"
#include "VE_Xplorer/fileIO.h"

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
   translatorMap[ "BANFDB" ] = new VE_Builder::cfdREItoVTK();
   // DICOM
   translatorMap[ "dcm" ] = new VE_Builder::cfdDICOMTranslator();
   // Fluent
   translatorMap[ "cas" ] = new VE_Builder::FluentTranslator();
   // EnSight
   //translatorMap[ "case" ] = new VE_Builder::EnSightTranslator();
   // MFIX
   //translatorMap[ "mfix" ] = new VE_Builder::MFIXTranslator();
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
   activeLoader->SetInputDirectory( inputDataDir );
   activeLoader->AddFoundFile( inputDataName );
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
   
