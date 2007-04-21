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

#include "VE_Xplorer/XplorerHandlers/cfdReadParam.h"
#include "VE_Xplorer/XplorerHandlers/cfdDataSet.h"
#include "VE_Xplorer/Utilities/fileIO.h"
#include "VE_Xplorer/XplorerHandlers/cfdEnum.h"
#include "VE_Xplorer/XplorerHandlers/cfdGlobalBase.h"
#include "VE_Xplorer/XplorerHandlers/cfdCommandArray.h"

#include <iostream>
#include <fstream>
#include <cmath>

#include "VE_Xplorer/XplorerHandlers/cfdDebug.h"
using namespace VE_Util;
using namespace VE_Xplorer;
using namespace VE_SceneGraph;

cfdReadParam::cfdReadParam()
{
   this->changeGeometry = false;
//files.reserve(10);   //added
   this->numGeoms = 0;
   this->bmpFile     = 0;
   this->diameter    = 0.0;
   this->worldTrans[0] = this->worldTrans[1] = this->worldTrans[2] = 0.0f;
   this->worldRot[0] = this->worldRot[1] = this->worldRot[2] = 0.0f;
   this->worldScale[0] = this->worldScale[1] = this->worldScale[2] = 1.0f;
   this->scalarBarPos[0] = this->scalarBarPos[1] = this->scalarBarPos[2] = 0.0;
   this->scalarBarZRot = 0.0;
   this->scalarBarH = this->scalarBarW = 0.0;
   this->frames = 0;
   guiVal = NULL;
   // IHCC Model - should be deleted at a later date
   ihccModel = false;
}

cfdReadParam::~cfdReadParam()
{
   int temp = (int)this->dataSets.size();
   if ( temp > 0 )
   {
      for (int i=0; i < temp; i++)
      {
         vprDEBUG(vesDBG,2) << "\tcfdReadParam:Deleting dataSet " << i 
                                << std::endl << vprDEBUG_FLUSH;
         delete this->dataSets[ i ];
      }
      vprDEBUG(vesDBG,2) << "\tcfdReadParam:clearing dataSets"
                             << std::endl << vprDEBUG_FLUSH;
      this->dataSets.clear();
   }

   if ( guiVal != NULL )
      delete [] guiVal;
}

void cfdReadParam::CreateNewDataSet()
{
   this->dataSets.push_back( new cfdDataSet() );
}

int cfdReadParam::GetNumberOfDataSets()
{
   return (int)this->dataSets.size();
}

cfdDataSet * cfdReadParam::GetDataSet( int i )
{
   if ( 0 <= i && i < (int)this->dataSets.size() )
      return this->dataSets[ i ];
   else
      return NULL;
}

cfdDataSet * cfdReadParam::GetDataSetWithName( const std::string vtkFilename )
{
   int numDataSets = this->GetNumberOfDataSets();
   for ( int i = 0; i < numDataSets; i++ )
   {
      //std::cout << this->GetDataSet( i )->GetFileName() << std::endl;
      if( vtkFilename.compare(this->GetDataSet( i )->GetFileName()) != 0 )//if ( ! strcmp(this->GetDataSet( i )->GetFileName(),vtkFilename.c_str()) )
      {
         return this->GetDataSet( i );
         break;
      }
   }
   return NULL;
}

std::string cfdReadParam::readDirName( std::ifstream &inFile, std::string description )
{
   std::string dirName;// = new char [ 256 ];
   inFile >> dirName;

   char textLine[ 256 ];
   inFile.getline( textLine, 256 );   //skip past remainder of line

   if ( fileIO::DirectoryExists( dirName ) ) 
   {
      vprDEBUG(vesDBG,0) << " " << description << " = \""
                << dirName << "\"" << std::endl << vprDEBUG_FLUSH;
   }
   else
   {
      vprDEBUG(vesDBG,0) << " " << description << " \""
                             << dirName << "\" does not exist"
                             << std::endl << vprDEBUG_FLUSH;
      //delete [] dirName;
      //dirName.empty();// = NULL;
      dirName.erase();
   }
   return dirName;
}

int cfdReadParam::readID( std::ifstream &inFile )
{
   int id;
   inFile >> id;
   char textLine [ 256 ];
   inFile.getline( textLine, 256 );   //skip past remainder of line
   return id;
}

int cfdReadParam::convertDecimalToBinary( double number) 
{
   vprDEBUG(vesDBG,1) << " Number = " << number
                          << std::endl << vprDEBUG_FLUSH;
   testBin.clear();
   double n = number;
   while (n > 1) 
   {
      double rem = fmod( n, static_cast< double >( 2 ) );
      testBin.push_back( rem );
      //std::cout <<  testBin.back() << std::endl;
      n = floor( n/2 );
   }
   // converts decimal to binary but binary number is backwards
   // Number gets converted to forward in function below

   //for ( unsigned int j = 0; j < testBin.size(); j++ )
   //   cout << testBin[ j ];
   return 0;
}

void cfdReadParam::convertBinaryToArray( int gui, int size ) 
{
   while ( size > (int)testBin.size() )
   {
      testBin.push_back( 0 );
   }
   // Must add zeros to binary number because binary number 
   // may be less than the num ber of geometries

   if ( guiVal != NULL )
      delete [] guiVal;
   
   guiVal = new int[ size ];

   for ( int i = 0; i < size; i++ ) 
   {      
      guiVal[ i] = static_cast< int >( testBin[ i ] );
      vprDEBUG(vesDBG,2) << "Binary number : " << guiVal[ i] << std::endl << vprDEBUG_FLUSH;
   }
}

bool cfdReadParam::CheckCommandId( cfdCommandArray* commandArray )
{
   if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == UPDATE_GEOMETRY )
   {
      vprDEBUG(vesDBG,1)
         << commandArray->GetCommandValue( cfdCommandArray::CFD_GEO_STATE ) << std::endl << vprDEBUG_FLUSH;

      long int test = this->convertDecimalToBinary( (int)commandArray->GetCommandValue( cfdCommandArray::CFD_GEO_STATE ) );
      vprDEBUG(vesDBG,1)
         << " test : " << test << std::endl << vprDEBUG_FLUSH;

      this->convertBinaryToArray( test, this->numGeoms );
      this->changeGeometry = true;
      return true;
   }
   return false;
}

void cfdReadParam::UpdateCommand()
{
   std::cerr << "doing nothing in cfdReadParam::UpdateCommand()" << std::endl;
}
