/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2004 by Iowa State University
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
 * File:          $RCSfile: cfdImage.cxx,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "cfdImage.h"
#include "cfdObjects.h"
#include "cfdDataSet.h"
#include "fileIO.h"

#include <vtkPlaneSource.h>
#include <vtkBMPReader.h>
#include <vtkActor.h>
#include <vtkPolyDataMapper.h>
#include <vtkTexture.h>
#include <vpr/Util/Debug.h>
#include <vtkImageReader.h>
#include <vtkProperty.h>

cfdImage::cfdImage( char * filename, double * position, int xyz )
{
   this->bmpReader = NULL;
   this->imgReader = NULL;
   this->plane = NULL;
   this->mapper = NULL;
   this->texture = NULL;
   this->actor = NULL;

   char * extension = fileIO::getExtension( filename );
   vprDEBUG(vprDBG_ALL, 1) << "extension = \"" << extension << "\"\n" << vprDEBUG_FLUSH;

   if ( !strcmp(extension,"bmp") || !strcmp(extension,"BMP") )
   {
      vprDEBUG(vprDBG_ALL, 1) << "found bmp\n" << vprDEBUG_FLUSH;
      // Kevin Kenney: The images represent 13.2-in x 13.8-in. plane of the
      // flow field. The lower left coordinates must be computed from
      // information supplied with the image/data. Image in y-z plane with
      // lower left at origin

      this->bmpReader = vtkBMPReader::New();
      this->bmpReader->SetFileName( filename );
      this->bmpReader->Update();

      this->plane = vtkPlaneSource::New();
      double lowerLeftCoords[3];
      lowerLeftCoords[0] = position[ 0 ];
      lowerLeftCoords[1] = position[ 1 ];
      lowerLeftCoords[2] = position[ 2 ];
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

      if ( xyz == 1 )
         this->plane->SetNormal( 0, 1, 0 );
      else if ( xyz == 2 )
         this->plane->SetNormal( 0, 0, 1 );

      this->mapper = vtkPolyDataMapper::New();
      this->mapper->SetInput( this->plane->GetOutput() );

      this->texture = vtkTexture::New();
      this->texture->SetInput( this->bmpReader->GetOutput() );
      this->texture->InterpolateOff();

      this->actor = vtkActor::New();
      this->actor->SetMapper( this->mapper );
      this->actor->SetTexture( this->texture );        

      this->type = xyz;
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
      vprDEBUG(vprDBG_ALL, 0) << "ERROR: invalid extension on file \""
         << filename << "\"\n" << vprDEBUG_FLUSH;
   }
}

// This should work generally with any vtkImageData

cfdImage::cfdImage ( char * filename, int resx, int resy, int dim, double *origin, double *spacing )
{
   this->bmpReader = NULL;
   this->imgReader = NULL;
   this->plane = NULL;
   this->mapper = NULL;
   this->texture = NULL;
   this->actor = NULL;

   char * extension = fileIO::getExtension( filename );
   vprDEBUG(vprDBG_ALL, 1) << "extension = \"" << extension << "\"\n" << vprDEBUG_FLUSH;

   if ( !strcmp(extension,"lic") || !strcmp(extension,"LIC") )
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
     this->imgReader->SetFileName(filename);
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

     this->actor = vtkActor::New();
     this->actor->SetMapper( this->mapper );
     this->actor->SetTexture( this->texture );        

     this->actor->GetProperty()->SetAmbient(1.0);
     this->actor->GetProperty()->SetDiffuse(1.0);
     this->actor->GetProperty()->SetSpecular(1.0);
     this->actor->GetProperty()->SetInterpolationToPhong();

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
   vprDEBUG(vprDBG_ALL, 1) << "CFDIMAGE DESTRUCTOR\n" << vprDEBUG_FLUSH;
   if (this->bmpReader != NULL ) this->bmpReader->Delete();
   if (this->imgReader != NULL ) this->imgReader->Delete();
   if (this->plane != NULL )     this->plane->Delete();
   if (this->mapper != NULL )    this->mapper->Delete();
   if (this->texture != NULL )   this->texture->Delete();
   if (this->actor != NULL )     this->actor->Delete();
}

vtkActor * cfdImage::GetActor( )
{
    return this->actor;
}

void cfdImage::Update( void )
{
   //this->actor->Print(cout);
   this->updateFlag = true;
}
