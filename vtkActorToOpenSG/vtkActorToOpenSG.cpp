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
 * File:          $RCSfile: filename,v $
 * Date modified: $Date: date $
 * Version:       $Rev: 999999 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include <cstdio>
#include <OpenSG/OSGConfig.h>
#include <OpenSG/OSGSimpleGeometry.h>
#include <OpenSG/OSGGeometry.h>
#include <OpenSG/OSGBaseFunctions.h>
#include <OpenSG/OSGGroup.h>
#include <vtkPolyDataMapper.h>
#include <vtkActor.h>
#include <vtkCellData.h>
#include <vtkProperty.h>
#include <vtkPolyData.h>
#include <vtkPointData.h>
#include <vtkCellArray.h>
#include "VTKtoOSG.h"

// Activate the OpenSG namespace
//OSG_USING_NAMESPACE

GeometryPtr convert( vtkActor *actor, int choice ) 
{
   vtkCellArray *primArray;

   if (strcmp(actor->GetMapper()->GetInput()->GetClassName(), "vtkPolyData"))
   {
      printf("Houston, we have a problem...\nUse vtkPolyDataMapper instead\n");
      GeometryPtr geoError = Geometry::create();
      return geoError;
   }

   // get polyData from vtkActor
   vtkPolyData *polyData = (vtkPolyData *) actor->GetMapper()->GetInput();
   int numPolys = polyData->GetNumberOfPolys();
   //int numVerts = polyData->GetNumberOfVerts();

   // this may be a problem //
   /* this is simple code I threw in here to determine if the data was point OR
      polydata.  However, if the actor contains both point AND polydata, this
      will only get polydata.  I'm not entirely sure how to fix this problem
      aside from calling this procedure two times with different primArrays for
      points and polys. (IE passing in the primarray from an external driver
      program). That solution should work since the get calls in the traversal
      are the same regardless of points or polydata. However, I do not know
      the format of wireframe data, so I'm really not sure how that would be
      implemented.*/

   if(numPolys > 0)
      primArray = polyData->GetPolys();
   else
      primArray = polyData->GetVerts();

   printf("CellData converted for use.\n");

   // find the length of the array
   // if this line gives a problem, try setting the calls to
   // GetNumberOf....() to integer variables
   int numIndices = primArray->GetNumberOfConnectivityEntries() -
                    primArray->GetNumberOfCells();
   printf("%d incdices\n" , numIndices);

   // allocate as many verts as there are indices in vtk prim array
   GeoPLengthsPtr OSGlens = GeoPLengthsUI32::create();
   beginEditCP(OSGlens, GeoPLengthsUI32::GeoPropDataFieldMask);
   {
      OSGlens->addValue(numIndices);
   }

   GeoPTypesPtr OSGtype = GeoPTypesUI8::create();
   beginEditCP(OSGtype, GeoPTypesUI8::GeoPropDataFieldMask);
   {
      // selection of the geometry type based on the value of choice
      switch(choice)
      {
         case 0 : OSGtype->addValue(GL_TRIANGLES);
         break;
                  
         case 1 : OSGtype->addValue(GL_POLYGON);
         break;

         case 2 : OSGtype->addValue(GL_QUADS);
         break;

         default : OSGtype->addValue(GL_POINTS);
      }
   }
   endEditCP(OSGtype, GeoPTypesUI8::GeoPropDataFieldMask);

   int isColored = 0;

   // Get color from the actor
   vtkUnsignedCharArray *colorArray =
   actor->GetMapper()->MapScalars(1.0);
   if (colorArray)
   {
      printf("We have colors..\n");
      isColored=1;
   }
   else
      printf("lacking in color...\n");

   GeoPositions3fPtr OSGpos = GeoPositions3f::create();
   GeoColors3fPtr    OSGcols = GeoColors3f::create();

   // copy data from vtk prim array to OSG pointers
   int prim = 0, vertex = 0;
   int i, npts, *pts;

   // traverse the cells and extract the data
   printf("preparing to loop\n");
   for (primArray->InitTraversal(); primArray->GetNextCell(npts, pts); prim++)
   { 
      // go through points in cell (verts)
      for (i=0; i < npts; i++) 
      {
         double *verts = polyData->GetPoint(pts[i]);
         beginEditCP(OSGpos, GeoPositions3f::GeoPropDataFieldMask);
         {
            OSGpos->addValue(Pnt3f(verts[0], verts[1], verts[2]));
         }
         endEditCP(OSGpos, GeoPositions3f::GeoPropDataFieldMask);

         // Add color to the pointer
         if(isColored==1)
         {
            unsigned char *actorColor = colorArray->GetPointer(4*pts[i]);
            beginEditCP(OSGcols ,
            GeoColors3f::GeoPropDataFieldMask);
            {
               OSGcols->addValue(Color3f(actorColor[0], 
                                         actorColor[1], actorColor[2]));
            }

            endEditCP(OSGcols , GeoColors3f::GeoPropDataFieldMask);
         }

         vertex++;
      }
   }

   // create the geometry pointer 
   GeometryPtr geoP = Geometry::create();

   // insert the newly obtained data into it
   beginEditCP(geoP, Geometry::TypesFieldMask | Geometry::LengthsFieldMask |
                     Geometry::PositionsFieldMask | Geometry::MaterialFieldMask);
   {
      geoP->setTypes(OSGtype);
      geoP->setLengths(OSGlens);
      geoP->setPositions(OSGpos);
      geoP->setColors(OSGcols);

      // I have yet to see VTK deal with materials, so this should be fine
      geoP->setMaterial(getDefaultUnlitMaterial());
   }
   endEditCP(geoP, Geometry::TypesFieldMask | Geometry::LengthsFieldMask |
                   Geometry::PositionsFieldMask | Geometry::MaterialFieldMask);

   return geoP;
}

