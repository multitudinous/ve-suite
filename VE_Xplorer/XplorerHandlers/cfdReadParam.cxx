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
#include "VE_Xplorer/SceneGraph/cfdFILEInfo.h"
#include "VE_Xplorer/Utilities/fileIO.h"
#include "VE_Xplorer/SceneGraph/cfdDCS.h"
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
   int temp = (int)this->files.size();
   if ( temp > 0 )
   {
      vprDEBUG(vesDBG,2) << " Deleting files"
                             << std::endl << vprDEBUG_FLUSH;
      for (int i=0; i < temp; i++)
      {
         vprDEBUG(vesDBG,2) << "\tcfdReadParam:Deleting file " << i 
                                << std::endl << vprDEBUG_FLUSH;
         delete this->files[ i ];
      }
      vprDEBUG(vesDBG,2) << "\tcfdReadParam:clearing files"
                             << std::endl << vprDEBUG_FLUSH;
      files.clear();
   }

   temp = (int)this->dataSets.size();
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

/*   temp = (int)this->transientInfo.size();
   // If we have any transient data
   if ( temp > 0 )
   {
      for (int i=0; i< temp; i++)
      {
         vprDEBUG(vesDBG,2) << "\tDeleting transientInfo " << i 
                                 << std::endl << vprDEBUG_FLUSH;
                             
         delete this->transientInfo[ i ];
      }
      vprDEBUG(vesDBG,2) << "\tcfdReadParam:clearing transientInfo"
                             << std::endl << vprDEBUG_FLUSH;
      this->transientInfo.clear();
   }
*/
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

/*
void cfdReadParam::data_read ( char * filein_name )
{
   if ( ! fileIO::isFileReadable( filein_name ) ) 
   {
      std::cerr << "\nError: Could not open the input file " 
                << filein_name << " !" << std::endl;
      while( !fileIO::isFileReadable( filein_name ) )
      {
         std::cout << "\nEnter correct filename: " << std::endl; 
         std::cin >> filein_name;        
      }
   }

   std::cout << "in cfdReadParam::data_read with " << filein_name << std::endl;
   std::ifstream inFile( filein_name, std::ios::in ); 

   // Read the information in the file.
   param_read( inFile );
   std::cout <<  "|   Finished reading the parameter file." << std::endl;
}

void cfdReadParam::param_read( std::ifstream &inFile )
{
   int numObjects;
   inFile >> numObjects;
   inFile.getline( textLine, 256 );   //skip past remainder of line

   for(int i=0;i<numObjects;i++)
   {
      int id;
      inFile >> id;
      inFile.getline( textLine, 256 );   //skip past remainder of line
    
      switch(id){
         case 0:
            readWorldDCS( inFile );
            break;
         case 1:
            readScalarBar( inFile );
            break;
         case 2:
            //set1DText( inFile );
            break;
         case 5:
            BMPLoader( inFile );
            break;
         case 6:
            std::cerr << "Type 6 is no longer used: Use type 8" << std::endl;
            exit(1);
            break;
         case 7: 
            std::cerr << "Type 7 is no longer used: Use type 8" << std::endl;
            exit(1);
            break;
         case 8:
            Vtk( inFile );
            break;
         case 9: 
            Stl( inFile );
            break;
         case 10:
            getTransientInfo( inFile );
            break;
         case 11:
            soundData(inFile);
            break;
         case 12:
            IMGReader(inFile);
            break;
         case 13:
            ihccModel = true;
            break;
         case 14:
            quatCamFile( inFile);
            break;
         default:
            std::cerr << "ERROR : param_read Unknown Type: " << id << std::endl;
            exit ( 1 );
      }
   }

   vrxprConfigFiles( inFile );
   this->numGeoms = files.size();
   guiVal = NULL;
}
*/

//read the user specified params for the transient data 
/*void cfdReadParam::getTransientInfo( std::ifstream &inFile )
{
   // how many directories contain transient vtk data?
   int numTransientSets;
   inFile >> numTransientSets;
   inFile.getline( textLine, 256 );   //skip past remainder of line

   int ii = transientInfo.size();
   this->transientInfo.push_back( new cfdTransientInfo() );

   vprDEBUG(vesDBG,0) << " transient DCS parameters:"
                          << std::endl << vprDEBUG_FLUSH;

   float scale[3], trans[3], rotate[3];   // pfDCS stuff
   this->read_pf_DCS_parameters( inFile, scale, trans, rotate);

   cfdDCS* dcs = new cfdDCS;
   dcs->SetScaleArray( scale );
   dcs->SetTranslationArray( trans );
   dcs->SetRotationArray( rotate );
   this->transientInfo[ ii ]->SetDCS( dcs );

   // read the directories...
   char ** transientDataDir = new char * [ numTransientSets ];
   for( int i = 0; i < numTransientSets; i++ )
   {
      transientDataDir[ i ] = readDirName( inFile, "transientDataDir" );
      int id = readID( inFile );
      vprDEBUG(vesDBG,0) << "\tbutton id = " << id
                             << std::endl << vprDEBUG_FLUSH;
      if ( transientDataDir[ i ] )
      {
         cfdTransientSet * cfdtransientset = new cfdTransientSet( 
                                              transientDataDir[ i ], id, dcs );
         // Maybe need to fix but should not used 
         //cfdtransientset->SetParameterFile( this );
         this->transientInfo[ ii ]->LoadTransientSet( cfdtransientset );
      }
      else
      {
         vprDEBUG(vesDBG,0) << " did not find transient data directory "
                                << i << std::endl << vprDEBUG_FLUSH;
      }
   }

   char * geometryDir = readDirName( inFile, "geometryDir" );
   this->transientInfo[ ii ]->SetGeometryDir( geometryDir );

   vprDEBUG(vesDBG,0) << " transient geometry DCS parameters:"
                          << std::endl << vprDEBUG_FLUSH;

   this->read_pf_DCS_parameters( inFile, scale, trans, rotate);

   cfdDCS* geometryDcs = new cfdDCS;
   geometryDcs->SetScaleArray( scale );
   geometryDcs->SetTranslationArray( trans );
   geometryDcs->SetRotationArray( rotate );
   this->transientInfo[ ii ]->SetGeometryDCS( geometryDcs );

   // read geometry transparency flag
   inFile >> this->transientInfo[ ii ]->trans;
   vprDEBUG(vesDBG,0) << " trans flag = " 
                          << this->transientInfo[ ii ]->trans
                          << std::endl << vprDEBUG_FLUSH;

   // read geometry color flag and color if flag = 1
   inFile >> this->transientInfo[ ii ]->color;
   vprDEBUG(vesDBG,0) << " color flag = " 
                          << this->transientInfo[ ii ]->color
                          << std::endl << vprDEBUG_FLUSH;

   this->transientInfo[ ii ]->stlColor[ 0 ] = -1.0;
   this->transientInfo[ ii ]->stlColor[ 1 ] = -1.0;
   this->transientInfo[ ii ]->stlColor[ 2 ] = -1.0;
   if( this->transientInfo[ ii ]->color )
   {
      for(int i=0; i<3; i++)
      {
         inFile >> this->transientInfo[ ii ]->stlColor[ i ];
      }
      vprDEBUG(vesDBG,0) << "   stlColor: " 
                       << this->transientInfo[ ii ]->stlColor[ 0 ] << " : "
                       << this->transientInfo[ ii ]->stlColor[ 1 ] << " : "
                       << this->transientInfo[ ii ]->stlColor[ 2 ]
                       << std::endl << vprDEBUG_FLUSH;
   }
   inFile.getline( textLine, 256 );   //skip past remainder of line
      
   double dur;
   inFile >> dur;
   inFile.getline( textLine, 256 );   //skip past remainder of line
   this->transientInfo[ ii ]->SetDuration( dur );
   vprDEBUG(vesDBG,0) << " duration = " << dur
                          << std::endl << vprDEBUG_FLUSH;
}
*/
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

void cfdReadParam::read_pf_DCS_parameters( std::ifstream &inFile,
                             float* scale, float* trans, float* rot )
{  
   int i;
   char  text[ 256 ];
   
   for(i=0;i<3;i++)
   {
      inFile >> scale[i];
   }
   inFile.getline( text, 256 );   //skip past remainder of line

   vprDEBUG(vesDBG,0) << "\tScale factors:      "
      << "\t" << scale[0] << "\t" << scale[1] << "\t" << scale[2]
      << std::endl << vprDEBUG_FLUSH;

   for(i=0;i<3;i++)
   {
      inFile >> trans[i];
   }
   inFile.getline( text, 256 );   //skip past remainder of line

   vprDEBUG(vesDBG,0) << "\tTranslation factors:"
      << "\t" << trans[0] << "\t" << trans[1] << "\t" << trans[2]
      << std::endl << vprDEBUG_FLUSH;

   for(i=0;i<3;i++)
   {
      inFile >> rot[i];
   }
   inFile.getline( text, 256 );   //skip past remainder of line

   vprDEBUG(vesDBG,0) << "\tRotation factors:   "
      << "\t" << rot[0] << "\t" << rot[1] << "\t" << rot[2]
      << std::endl << vprDEBUG_FLUSH;
}  

void cfdReadParam::SkipModuleBlock( std::ifstream &inFile, int numLines )
{
   char text[ 256 ];
   for ( int i = 0; i < numLines; i++ )
   {
      inFile.getline( text, 256 );   //skip past remainder of line      
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

void cfdReadParam::ContinueRead( std::ifstream &input, unsigned int id )
{
   unsigned int numLines = 0;
   char text[ 256 ];
   // Set how many lines to skip...
   switch(id)
   {
      case 0:
         // World DCS
         numLines = 3;
         break;
      case 1:
         // Scalar Bar
         numLines = 3;
         break;
      case 2:
         //set1DText( inFile );
         std::cerr << "Type 2 is no longer used." << std::endl;
         exit(1);
         break;
      case 5:
         // BMP Loader
         numLines = 3;
         break;
      case 6:
         std::cerr << "Type 6 is no longer used: Use type 8" << std::endl;
         exit(1);
         break;
      case 7: 
         std::cerr << "Type 7 is no longer used: Use type 8" << std::endl;
         exit(1);
         break;
      case 8:
         // VTK File
         numLines = 6;
         break;
      case 9:
         // Geometry File
         numLines = 6;
         break;
      case 10:
         numLines = 11;
         break;
      case 11:
         // Sound loader
         numLines = 11;
         break;
      case 12:
         // IMG Reader
         numLines = 6;
         break;
      case 13:
         // IHCC Hack code
         numLines = 0;
         break;
      case 14:
         // Quat stuff...
         numLines = 1;
         break;
      case 15:
         //texture based model
         //number of lines is == to number of filenames specified for the
         //texture based model so read the next param
         input>>numLines;
         input.getline( text, 256 );
         break;
      default:
         std::cerr << "ERROR : ContinueRead Unknown Type: " << id << std::endl;
         exit ( 1 );
   }

   // Based on the object type skip the respective object 
   
   for ( unsigned int i = 0; i < numLines; i++ )
   {
      input.getline( text, 256 );   //skip past line      
   }
}
