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
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "VE_Xplorer/XplorerHandlers/cfdImage.h"
#include "VE_Xplorer/XplorerHandlers/cfdObjects.h"
#include "VE_Xplorer/XplorerHandlers/cfdDataSet.h"
#include "VE_Xplorer/XplorerHandlers/cfdCommandArray.h"
#include "VE_Xplorer/Utilities/fileIO.h"
#include "VE_Xplorer/XplorerHandlers/cfdReadParam.h"

#include <vtkPlaneSource.h>
#include <vtkBMPReader.h>
#include <vtkActor.h>
#include <vtkPolyDataMapper.h>
#include <vtkTexture.h>
#include <vtkImageReader.h>
#include <vtkImageData.h>
#include <vtkProperty.h>
#include <vtkActor.h>

#include "VE_Xplorer/XplorerHandlers/cfdDebug.h"

#include <fstream>

using namespace VE_Xplorer;
using namespace VE_SceneGraph;
using namespace VE_Util;

cfdImage::cfdImage( std::string param )
{
   this->bmpReader = NULL;
   this->imgReader = NULL;
   this->plane = NULL;
   this->mapper = NULL;
   this->texture = NULL;
   bmpOrientation = -1;

   if ( param.empty() )
      return;
   _param = param;
   _readParam = new cfdReadParam();
   // Fix this if createobjects is true then continue else return
   CreateObjects();

   if ( bmpOrientation == -1 )
      return;

   std::string extension = fileIO::getExtension( bmpFileName );
   vprDEBUG(vesDBG, 1) << "extension = \"" << extension << "\"\n" << vprDEBUG_FLUSH;

   if ( !(extension == "bmp") || !(extension == "BMP") )
   {
      vprDEBUG(vesDBG, 1) << "found bmp\n" << vprDEBUG_FLUSH;
      // Kevin Kenney: The images represent 13.2-in x 13.8-in. plane of the
      // flow field. The lower left coordinates must be computed from
      // information supplied with the image/data. Image in y-z plane with
      // lower left at origin

      this->bmpReader = vtkBMPReader::New();
      this->bmpReader->SetFileName( bmpFileName.c_str() );
      this->bmpReader->Update();

      this->plane = vtkPlaneSource::New();
      double lowerLeftCoords[3];
      lowerLeftCoords[0] = bmpPosition[ 0 ];
      lowerLeftCoords[1] = bmpPosition[ 1 ];
      lowerLeftCoords[2] = bmpPosition[ 2 ];
      this->plane->SetOrigin( lowerLeftCoords );

      double lowerRightCoords[3];
      lowerRightCoords[0] = lowerLeftCoords[0];
      lowerRightCoords[1] = lowerLeftCoords[1] + 13.2/12.0;
      lowerRightCoords[2] = lowerLeftCoords[2];
      this->plane->SetPoint1( lowerRightCoords );

      double upperLeftCoords[3];
      upperLeftCoords[0] = lowerLeftCoords[0];
      upperLeftCoords[1] = lowerLeftCoords[1];
      upperLeftCoords[2] = lowerLeftCoords[2] + 13.8/12.0;
      this->plane->SetPoint2( upperLeftCoords );
      this->plane->SetResolution( 1, 1 );

      if ( bmpOrientation == 1 )
         this->plane->SetNormal( 0, 1, 0 );
      else if ( bmpOrientation == 2 )
         this->plane->SetNormal( 0, 0, 1 );

      this->mapper = vtkPolyDataMapper::New();
      this->mapper->SetInput( this->plane->GetOutput() );

      this->texture = vtkTexture::New();
      this->texture->SetInput( this->bmpReader->GetOutput() );
      this->texture->InterpolateOff();

      vtkActor* temp = vtkActor::New();
      temp->SetMapper( this->mapper );
      temp->GetProperty()->SetSpecularPower( 20.0f );
      temp->SetTexture( this->texture ); 
      geodes.push_back( new VE_SceneGraph::Geode() );
      geodes.back()->TranslateToGeode( temp );
      temp->Delete();

      this->type = bmpOrientation;
      if      (this->type==0)  this->typeLabel = 'X';
      else if (this->type==1)  this->typeLabel = 'Y';
      else if (this->type==2)  this->typeLabel = 'Z';
      else
      {
         std::cerr << "ERROR: in cfdImage, xyz must be 0, 1, or 2" << std::endl;
         exit( 1 );
      }
   }
   else
   {
      vprDEBUG(vesDBG, 0) << "ERROR: invalid extension on file \""
         << bmpFileName << "\"\n" << vprDEBUG_FLUSH;
   }
}

// This should work generally with any vtkImageData

cfdImage::cfdImage ( std::string filename, int resx, int resy, int dim, double *origin, double *spacing )
{
   this->bmpReader = NULL;
   this->imgReader = NULL;
   this->plane = NULL;
   this->mapper = NULL;
   this->texture = NULL;

   std::string extension = fileIO::getExtension( filename );
   vprDEBUG(vesDBG, 1) << "extension = \"" << extension << "\"\n" << vprDEBUG_FLUSH;

   if ( !(extension == "lic") || !(extension == "LIC") )
   {
     // ImageReader

     this->imgReader = vtkImageReader::New();

     if(dim==0)      this->imgReader->SetDataExtent(0, 0,    1, resx, 1, resy);
     else if(dim==1) this->imgReader->SetDataExtent(1, resx, 0, 0,    1, resy);
     else            this->imgReader->SetDataExtent(1, resx, 1, resy, 0, 0);

     this->imgReader->SetDataSpacing(spacing);
     this->imgReader->SetDataOrigin(origin);
     this->imgReader->SetDataScalarTypeToUnsignedChar();
     this->imgReader->SetNumberOfScalarComponents(4);
     this->imgReader->SetFileName(filename.c_str());
     this->imgReader->Update();

     // Plane

     this->plane = vtkPlaneSource::New();
     this->plane->SetOrigin(origin);

     if(dim==0) {
       plane->SetPoint1(origin[0], origin[1]+resx*spacing[1], origin[2]);
       plane->SetPoint2(origin[0], origin[1],                 origin[2]+resy*spacing[2]);
     }
     else if(dim==1) {
       plane->SetPoint1(origin[0]+resx*spacing[0], origin[1], origin[2]);
       plane->SetPoint2(origin[0],                 origin[1], origin[2]+resy*spacing[2]);
     }
     else {
       plane->SetPoint1(origin[0]+resx*spacing[0], origin[1],                 origin[2]);
       plane->SetPoint2(origin[0],                 origin[1]+resy*spacing[1], origin[2]);
     }

     // Mapper

     this->mapper = vtkPolyDataMapper::New();
     this->mapper->SetInput( this->plane->GetOutput() );

     // Texture

     this->texture = vtkTexture::New();
     this->texture->SetInput( this->imgReader->GetOutput() );
     this->texture->InterpolateOn();

     // Actor

      vtkActor* temp = vtkActor::New();
	   temp->SetMapper( this->mapper );
	   temp->GetProperty()->SetSpecularPower( 20.0f );
	   temp->SetTexture( this->texture );        
      temp->GetProperty()->SetAmbient(1.0);
      temp->GetProperty()->SetDiffuse(1.0);
      temp->GetProperty()->SetSpecular(1.0);
      temp->GetProperty()->SetInterpolationToPhong();
      geodes.push_back( new VE_SceneGraph::Geode() );
      geodes.back()->TranslateToGeode( temp );
      temp->Delete();

     // Dim stuff

     this->type = dim;
     if      (this->type==0)  this->typeLabel = 'X';
     else if (this->type==1)  this->typeLabel = 'Y';
     else if (this->type==2)  this->typeLabel = 'Z';
     else
     {
       std::cerr << "ERROR: in cfdImage, xyz must be 0, 1, or 2" << std::endl;
       exit( 1 );
     }
   }
}

cfdImage::~cfdImage()
{
   //vprDEBUG(vesDBG, 1) << "CFDIMAGE DESTRUCTOR\n" << vprDEBUG_FLUSH;
   if (this->bmpReader != NULL ) this->bmpReader->Delete();
   if (this->imgReader != NULL ) this->imgReader->Delete();
   if (this->plane != NULL )     this->plane->Delete();
   if (this->mapper != NULL )    this->mapper->Delete();
   if (this->texture != NULL )   this->texture->Delete();
}

void cfdImage::Update( void )
{
   //this->actor->Print(cout);
   this->updateFlag = true;
}

bool cfdImage::CheckCommandId( cfdCommandArray* commandArray  )
{
   return false;
}

void cfdImage::UpdateCommand()
{
   std::cerr << "doing nothing in cfdImage::UpdateCommand()" << std::endl;
}

void cfdImage::CreateObjects( void )
{
   int numObjects;
   char textLine[ 256 ];
   std::ifstream input;
   input.open( this->_param.c_str() );
   input >> numObjects; 
   input.getline( textLine, 256 );   //skip past remainder of line

   vprDEBUG(vesDBG,1) << " Number of Obejcts in Interactive Geometry : " << numObjects << std::endl  << vprDEBUG_FLUSH;
   for( int i = 0; i < numObjects; i++ )
   {
      int id;
      input >> id;
      vprDEBUG(vesDBG,1) << "Id of object in Interactive Geometry : " << id << std::endl << vprDEBUG_FLUSH;
      input.getline( textLine, 256 );   //skip past remainder of line
      if ( id == 5 )
      {
         input >> this->bmpFileName;
         input.getline( textLine, 256 );   //skip past remainder of line

         if (fileIO::isFileReadable( this->bmpFileName ) ) 
         {
            vprDEBUG(vesDBG,0) << " BMP file = " << this->bmpFileName
                             << std::endl << vprDEBUG_FLUSH;
         }
         else
         {
            std::cerr << "ERROR: unreadable BMP File = " << this->bmpFileName 
                     << ".  You may need to correct your param file." << std::endl;
            exit(1);
         }

         input >> this->bmpPosition[ 0 ] >> this->bmpPosition[ 1 ]
               >> this->bmpPosition[ 2 ];
         input.getline( textLine, 256 );   //skip past remainder of line
         vprDEBUG(vesDBG,0) << " BMP Position = " << this->bmpPosition[ 0 ]
            << "\t" << this->bmpPosition[ 1 ] << "\t" <<  this->bmpPosition[ 2 ]
            << std::endl << vprDEBUG_FLUSH;

         input >> this->bmpOrientation;
         input.getline( textLine, 256 );   //skip past remainder of line
         vprDEBUG(vesDBG,0) << " BMP Orientation = " << this->bmpOrientation
            << std::endl << vprDEBUG_FLUSH;
      }
      else
      {
         // Skip past block
         //_readParam->ContinueRead( input, id );
      }
   }
}
