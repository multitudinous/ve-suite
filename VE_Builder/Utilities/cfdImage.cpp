/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2005 by Iowa State University
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
 * File:          $RCSfile: cfdImage.cpp,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "cfdImage.h"
#include <iostream>

//#include <Performer/pr/pfGeoSet.h>
//#include "vtkActorToPF.h"

#include <vtkBMPReader.h>
#include <vtkPlaneSource.h>
#include <vtkPolyDataMapper.h>
#include <vtkTexture.h>
#include <vtkActor.h>

#include "fileIO.h"


cfdImage::cfdImage( char * filename, int xyz )
{
   this->bmpReader = NULL;
   this->plane = NULL;
   this->mapper = NULL;
   this->texture = NULL;
   this->actor = NULL;

   char * extension = fileIO::getExtension( filename );
   //cout << "extension = \"" << extension << "\"" << endl;

   if ( !strcmp(extension,"bmp") || !strcmp(extension,"BMP") )
   {
      //cout << "found bmp" << endl;
      // Kevin Kenney: The images represent 13.2-in x 13.8-in. plane of the flow field
      // the lower left coordinates must be computed from information supplied with the image/data
      // puts image in y-z plane with lower left at origin

      vtkBMPReader *bmpReader = vtkBMPReader::New();
      bmpReader->SetFileName( filename );
      bmpReader->Update();

      vtkPlaneSource * plane = vtkPlaneSource::New();
      double lowerLeftCoords[3];
      lowerLeftCoords[0] = 0.0;
      lowerLeftCoords[1] = 0.0;
      lowerLeftCoords[2] = 0.0;
      plane->SetOrigin( lowerLeftCoords );

      double lowerRightCoords[3];
      lowerRightCoords[0] = lowerLeftCoords[0];
      lowerRightCoords[1] = lowerLeftCoords[1] + 13.2/12.0;
      lowerRightCoords[2] = lowerLeftCoords[2];
      plane->SetPoint1( lowerRightCoords );

      double upperLeftCoords[3];
      upperLeftCoords[0] = lowerLeftCoords[0];
      upperLeftCoords[1] = lowerLeftCoords[1];
      upperLeftCoords[2] = lowerLeftCoords[2] + 13.8/12.0;
      plane->SetPoint2( upperLeftCoords );
      plane->SetResolution( 1, 1 );

      vtkPolyDataMapper *mapper = vtkPolyDataMapper::New();
      mapper->SetInput( plane->GetOutput() );

      vtkTexture *texture = vtkTexture::New();
      texture->SetInput( bmpReader->GetOutput() );
      texture->InterpolateOff();

      actor = vtkActor::New();
      actor->SetMapper(mapper);
      actor->SetTexture(texture);        

      this->type = xyz;
      if      (this->type==0)  this->typeLabel = 'X';
      else if (this->type==1)  this->typeLabel = 'Y';
      else if (this->type==2)  this->typeLabel = 'Z';
      else
      {
         std::cout << "ERROR: in cfdImage, xyz must be 0, 1, or 2" << std::endl;
         exit( 1 );
      }
   }
   else
      std::cout << "ERROR: invalid extension on file \"" << filename << "\"" << std::endl;
}


cfdImage::~cfdImage()
{
   //cout << "CFDIMAGE DESTRUCTOR" << endl;
   if (this->bmpReader) bmpReader->Delete();
   if (this->plane)     plane->Delete();
   if (this->mapper)    mapper->Delete();
   if (this->texture)   texture->Delete();
   if (this->actor)     actor->Delete();
}


vtkActor * cfdImage::GetActor( )
{
    return this->actor;
}

/*
bool cfdImage::Update( pfGeoSet *g[4] )
{
   //this->actor->Print(cout);
   vtkActorToGeoSets( this->actor, g );
   return (true);
}
*/
