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
 * File:          $RCSfile: cfdArrow.cpp,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include <iostream>
#include <cstdio>
#include <cmath>

#include "cfdArrow.h"
#include <vtkPolyData.h>
#include <vtkPolyDataNormals.h>
#include <vtkPoints.h>
#include <vtkCellArray.h>

cfdArrow::cfdArrow(  )
{    
   // in vtkArrowSource, the shaft base is always at (0,0,0). The arrow tip is always at (1,0,0). 
   // here, the shaft base is always at (-1,0,0). The arrow tip is always at (0,0,0)

   this->PI = 3.14159265;

   //shaft resolution: 1 gives a rectangle, 2 gives crossed rectangles, vtk default is 6
   SetShaftResolution( 6 );
   this->shaftRadius = 0.03;  //vtk default

   //tip resolution: 1 gives a single triangle, 2 gives crossed triangles, vtk default is 6
   SetTipResolution( 6 );
   this->tipRadius = 0.10;    //vtk default
   this->tipLength = 0.35;    //vtk default

   // by default, arrow file will have normals
   this->normalsActivated = 1;
}

cfdArrow::~cfdArrow( )
{
    //cout << "in cfdArrow destructor" << endl;
    //this->Delete( );
}

void cfdArrow::SetShaftResolution( int shaftRes )
{    
   //shaft resolution: 1 gives a rectangle, 2 gives crossed rectangles, vtk default is 6
   this->shaftResolution = shaftRes;

   if ( this->shaftResolution == 1 )
      shaftAngleIncrement = this->PI;
   else
      shaftAngleIncrement = (this->PI/180.0)*360.0/(float)this->shaftResolution;
}

int cfdArrow::GetShaftResolution( )
{    
   return this->shaftResolution;
}

void cfdArrow::SetShaftRadius( float shaftRad )
{    
   //shaft radius: vtk default is 0.03;
   this->shaftRadius = shaftRad;
}

float cfdArrow::GetShaftRadius( )
{    
   return this->shaftRadius;
}

void cfdArrow::SetTipResolution( int tipRes )
{    
   //tip resolution: 1 gives a single triangle, 2 gives crossed triangles, vtk default is 6
   this->tipResolution = tipRes;

   if ( this->tipResolution == 1 )
      this->tipAngleIncrement = this->PI;
   else
      this->tipAngleIncrement = (this->PI/180.0)*360.0/(float)this->tipResolution;
}

int cfdArrow::GetTipResolution( )
{    
   return this->tipResolution;
}

void cfdArrow::SetTipRadius( float tipRad )
{    
   //tip radius: vtk default is 0.10
   this->tipRadius = tipRad;
}

float cfdArrow::GetTipRadius( )
{    
   return this->tipRadius;
}

void cfdArrow::SetTipLength( float tipLen )
{    
   //tip length: vtk default is 0.35
   this->tipLength = tipLen;
}

float cfdArrow::GetTipLength( )
{    
   return this->tipLength;
}

void cfdArrow::TurnNormalsOn( )
{
   this->normalsActivated = 1;
}

void cfdArrow::TurnNormalsOff( )
{
   this->normalsActivated = 0;
}

vtkPolyData * cfdArrow::GetPolyData(  )
{    
   //int numPoints = shaftResolution*4 + tipResolution*3;
   int numCells = shaftResolution + tipResolution;

   float vertex[3];
   int vertexList [4];     // make large enough for rectangles
   vtkPolyData * arrow = vtkPolyData::New();

   int memSize = numCells;
   arrow->Allocate( numCells, memSize );

   // Work on the arrow head
   int i;
   vtkPoints * points = vtkPoints::New();
   for (i=0; i<tipResolution; i++)
   {
      float angle = tipAngleIncrement*float(i);

      //generate and store points for each triangle of the arrow head
      vertex[0] = -tipLength;
      vertex[1] = tipRadius*sin( angle );
      vertex[2] = tipRadius*cos( angle );
      vertexList[0] = points->InsertNextPoint( vertex );

      vertex[0] = -tipLength;
      vertex[1] = tipRadius*sin( angle + tipAngleIncrement );
      vertex[2] = tipRadius*cos( angle + tipAngleIncrement );
      vertexList[1] = points->InsertNextPoint( vertex );

      vertex[0] = 0.0;
      vertex[1] = 0.0;
      vertex[2] = 0.0;
      vertexList[2] = points->InsertNextPoint( vertex );

      arrow->InsertNextCell( VTK_POLYGON, 3, vertexList );

      if ( tipResolution==2 )     // generate crossed polygon and exit...
      {
         vertex[0] = -tipLength;
         vertex[1] = tipRadius;
         vertex[2] = 0.0;
         vertexList[0] = points->InsertNextPoint( vertex );

         vertex[0] = -tipLength;
         vertex[1] = -tipRadius;
         vertex[2] = 0.0;
         vertexList[1] = points->InsertNextPoint( vertex );

         vertex[0] = 0.0;
         vertex[1] = 0.0;
         vertex[2] = 0.0;
         vertexList[2] = points->InsertNextPoint( vertex );

         arrow->InsertNextCell( VTK_POLYGON, 3, vertexList );
         break;
      }
   }

   // Work on the shaft
   for (i=0; i<shaftResolution; i++)
   {
      float angle = shaftAngleIncrement*float(i);

      // generate and store points for each rectangle of the shaft
      vertex[0] = -1.0;
      vertex[1] = shaftRadius*sin( angle );
      vertex[2] = shaftRadius*cos( angle );
      vertexList[0] = points->InsertNextPoint( vertex );

      vertex[0] = -tipLength;
      vertex[1] = shaftRadius*sin( angle );
      vertex[2] = shaftRadius*cos( angle );
      vertexList[1] = points->InsertNextPoint( vertex );

      vertex[0] = -tipLength;
      vertex[1] = shaftRadius*sin( angle + shaftAngleIncrement );
      vertex[2] = shaftRadius*cos( angle + shaftAngleIncrement );
      vertexList[2] = points->InsertNextPoint( vertex );

      vertex[0] = -1.0;
      vertex[1] = shaftRadius*sin( angle + shaftAngleIncrement );
      vertex[2] = shaftRadius*cos( angle + shaftAngleIncrement );
      vertexList[3] = points->InsertNextPoint( vertex );

      arrow->InsertNextCell( VTK_POLYGON, 4, vertexList );

      if ( shaftResolution==2 )   // generate crossed polygon and exit...
      {
         vertex[0] = -1.0;
         vertex[1] = shaftRadius;
         vertex[2] = 0.0;
         vertexList[0] = points->InsertNextPoint( vertex );

         vertex[0] = -tipLength;
         vertex[1] = shaftRadius;
         vertex[2] = 0.0;
         vertexList[1] = points->InsertNextPoint( vertex );

         vertex[0] = -tipLength;
         vertex[1] = -shaftRadius;
         vertex[2] = 0.0;
         vertexList[2] = points->InsertNextPoint( vertex );

         vertex[0] = -1.0;
         vertex[1] = -shaftRadius;
         vertex[2] = 0.0;
         vertexList[3] = points->InsertNextPoint( vertex );

         arrow->InsertNextCell( VTK_POLYGON, 4, vertexList );
         break;
      }
   }

   arrow->SetPoints( points );
   points->Delete();

   if ( this->normalsActivated )
   {
      vtkPolyDataNormals * arrowPolysWithNormals = vtkPolyDataNormals::New();
      arrowPolysWithNormals->SetInput( arrow );
      arrow->Delete();
      //Specify the angle that defines a sharp edge. If the difference in angle across neighboring
      //polygons is greater than this value, the shared edge is considered "sharp".    
      arrowPolysWithNormals->SetFeatureAngle( 60 );
      return arrowPolysWithNormals->GetOutput();
   }

   return arrow;
}

