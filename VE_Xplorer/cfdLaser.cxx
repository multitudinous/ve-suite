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
 * File:          $RCSfile: cfdLaser.cxx,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

// .NAME cfdMenu - create laser beam.
// .SECTION Description
// A class to build a virtual laser beaming from wand 
// using vtk functions and render using Performer.  
// VTK objects(vtkActor) are translated into Performer objects(pfGeode).

#include "cfdLaser.h"
#include "cfdCommandObjects.h"
#include "cfdCommandArray.h"
#include "cfdDCS.h"
#include "cfdGeode.h"
#include "cfdEnum.h"
#include "cfdGroup.h"

#include <vtkLineSource.h>
#include <vtkPolyDataNormals.h>
#include <vtkTubeFilter.h>
#include <vtkActor.h>
#include <vtkPolyDataMapper.h>
#include <vtkProperty.h>
#include <vtkBox.h>
#include <vpr/Util/Debug.h>

cfdLaser::cfdLaser( cfdGroup* rootNode )
{
   this->menuB = true;
   this->_rootNode = rootNode;
   this->length = 100.0f;

   this->hitXYZ[0] = 0.0f;
   this->hitXYZ[1] = 100.0f;
   this->hitXYZ[2] = 0.0f;

   this->src = vtkLineSource::New();
   this->src->SetPoint1( 0.0, 0.0, 0.0 );
   this->src->SetPoint2( this->hitXYZ[0], this->hitXYZ[1], this->hitXYZ[2] );
   this->src->Update();

   this->filt = vtkTubeFilter::New();
   this->filt->SetInput( this->src->GetOutput() );
   this->filt->SetRadius( 0.01f );
   this->filt->SetNumberOfSides( 6 );

   //this->norm = vtkPolyDataNormals::New();
   //this->norm->SetInput( this->filt->GetOutput() );

   this->mapper = vtkPolyDataMapper::New();
   //this->mapper->SetInput( this->norm->GetOutput() );
   this->mapper->SetInput( this->filt->GetOutput() );

   this->actor = vtkActor::New();
   this->actor->SetMapper( this->mapper );
   this->actor->GetProperty()->SetColor( 1.0f, 0.0f, 0.0f );
   //this->actor->GetMapper()->GetInput()->Update(); //changed to fit vtk4.0

   this->_geode = new cfdGeode();
   this->DCS = new cfdDCS();
   this->_geode->TranslateTocfdGeode( this->actor );

   this->DCS->AddChild( (cfdSceneNode*)this->_geode );
}

bool cfdLaser::CheckCommandId( cfdCommandArray* commandArray  )
{
   if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == BLUE_MENU_TOGGLE )
   {
      if ( this->menuB == true )
      { 
         this->_rootNode->RemoveChild( this->GetcfdDCS() );
         this->menuB = false;
      }
      else
      { 
         // If menu is down, add menu and laser wand, and keep cursor off
         this->_rootNode->AddChild( this->GetcfdDCS() );
         this->menuB = true;
      }

      return true;
   }
   return false;
}

void cfdLaser::UpdateCommand()
{
   cerr << "cfdMenu::UpdateCommand() does nothing" << endl;
}

double * cfdLaser::GetDirection( double vec[3] )
{
   for ( int i=0; i<3; i++ )
   {
      this->dir[i] = vec[i] * this->length;
   }

   return this->dir;
}


int cfdLaser::HitMenu( double bounds[6], double origin[3], double vec[3] )
{
   /* Bounding box intersection modified from Graphics Gems Vol I. The method
   returns a non-zero value if the bounding box is hit. Origin[3] starts the
   ray, dir[3] is the vector components of the ray in the x-y-z directions,
   coord[3] is the location of hit, and t is the parametric coordinate along
   line. (Notes: the intersection ray dir[3] is NOT normalized. Valid
   intersections will only occur between 0<=t<=1.) */
   double t;   // allocate space for parameterized variable
   int flag = vtkBox::IntersectBox( bounds, origin, this->GetDirection( vec ),
                                    this->hitXYZ, t );
   if ( ! flag )
   {
      for ( int i=0; i<3; i++ )
      {
         this->hitXYZ[i] = origin[i] + vec[i]*this->length;
      }
   }

   this->Update( origin, vec );

   return flag;
}

//bool Update( double origin[3], double vec[3], pfGeoSet *g[4], double cur_box[6], int t )
void cfdLaser::Update( double origin[3], double vec[3] )
{
   this->src->SetPoint1( origin[0], origin[1], origin[2] );
   this->src->SetPoint2( this->hitXYZ[0], this->hitXYZ[1], this->hitXYZ[2] );
   this->src->Update();

   //this->actor->GetMapper()->GetInput()->Update(); //changed to fit vtk4.0

   this->_geode->TranslateTocfdGeode( this->actor );
   //return true;
}


cfdDCS * cfdLaser::GetcfdDCS( )
{
   return this->DCS;
}


double * cfdLaser::GetHitXYZ( )
{
   return this->hitXYZ;
}


cfdLaser::~cfdLaser( )
{
   this->src->Delete();
   this->filt->Delete();
   //this->norm->Delete();
   this->mapper->Delete();
   this->actor->Delete();

   delete this->_geode;
   delete this->DCS;
}

