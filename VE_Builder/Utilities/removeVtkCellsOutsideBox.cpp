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
 * File:          $RCSfile: removeVtkCellsOutsideBox.cpp,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include <iostream>

#include "fileIO.h"
#include "readWriteVtkThings.h"
#include "cleanVtk.h"

#include <vtkGeometryFilter.h>
#include <vtkAppendFilter.h>
#include <vtkFloatArray.h>
#include <vtkDataSet.h>
#include <vtkPointSet.h>
#include <vtkPolyData.h>
#include <vtkUnstructuredGrid.h>
#include <vtkSetGet.h>  // defines data object types
#include <vtkIdList.h>
#include <vtkGenericCell.h>
#include <vtkPointData.h>

int debug = 0;

void removeCellsOutsideBox( vtkPointSet * & pointset, double xmin, double xmax, 
                            double ymin, double ymax, double zmin, double zmax )
{
   int numVertices = pointset->GetNumberOfPoints();
   if ( debug ) cout << "numVertices = " << numVertices << endl;

   int j;

   // if points are in volume, then they are "needed"
   double vertex [ 3 ];
   int * isNeededPoint = new int [ numVertices ];
   for (j=0; j < numVertices; j++)
   {
      // get coordinates of the vertex
      pointset->GetPoints()->GetPoint( j, vertex );
      if ( vertex[0] >= xmin && vertex[0] <= xmax &&
           vertex[1] >= ymin && vertex[1] <= ymax &&
           vertex[2] >= zmin && vertex[2] <= zmax )
      {
         isNeededPoint[ j ] = 1;
      }
      else
      {
         isNeededPoint[ j ] = 0;
      }
   }

   // count the number of "needed points"
   int numNeededVertices = 0;
   for (j=0; j < numVertices; j++)
   {
      if ( debug > 1 && isNeededPoint[j] )
         cout << "\tisNeededPoint[" << j << "] = " << isNeededPoint[j] << endl;
      numNeededVertices += isNeededPoint[j];
   }
   if ( debug ) cout << "numNeededVertices = " << numNeededVertices << endl;
   if ( numNeededVertices  == 0 )
   {
      delete [] isNeededPoint;
      return;
   }
   else if ( numNeededVertices == numVertices ) 
   {
      cout << "Will NOT try to collapse the file" << endl;
      delete [] isNeededPoint;
      return;
   }

   vtkPointSet * smallGrid;
   if ( pointset->GetDataObjectType() == VTK_UNSTRUCTURED_GRID )
      smallGrid = vtkUnstructuredGrid::New();
   else if ( pointset->GetDataObjectType() == VTK_POLY_DATA )
      smallGrid = vtkPolyData::New();
   else
   {
      cout << "removeCellsOutsideBox can not handle this data object type" << endl;
      delete [] isNeededPoint;
      exit( 1 );
   }

   smallGrid->SetPoints( pointset->GetPoints() );

   int numCells = pointset->GetNumberOfCells();
   if (debug) cout << "The original number of cells is " << numCells << endl;

   if ( pointset->GetDataObjectType() == VTK_UNSTRUCTURED_GRID )
      ((vtkUnstructuredGrid*)smallGrid)->Allocate( numCells, numCells );
   else if ( pointset->GetDataObjectType() == VTK_POLY_DATA )
      ((vtkPolyData*)smallGrid)->Allocate( numCells, numCells );

   // Loop over original cells and store revised definitions in smallGrid...
   int npts, ptId, cellId;
   vtkIdList *ptIdList = vtkIdList::New();
   vtkGenericCell *cell = vtkGenericCell::New();

   for( cellId=0; cellId < numCells; cellId++)
   {      
      int useThisCell = 0; // initially mark this cell to NOT be retained 
      pointset->GetCell( cellId, cell );
      if ( debug > 1 ) cout << "\tcellType = " << cell->GetCellType() << endl;

      npts = cell->GetNumberOfPoints();
      ptIdList->Reset();
      for ( int i=0; i < npts; i++)
      {
         ptId = cell->GetPointId( i );
         // mark this cell to be retained if ANY of its points are "needed"
         if ( isNeededPoint[ptId] )
         {
            if ( debug > 1 ) cout << "\t\tNEED ptId= " << ptId << endl;
            useThisCell = 1;
         }
         ptIdList->InsertId( i, ptId );
      }

      if ( debug > 1 )
         cout << "\tcellId = " << cellId
              << ",\tuseThisCell = " << useThisCell << endl;

      if ( useThisCell )

      {
         if ( pointset->GetDataObjectType() == VTK_UNSTRUCTURED_GRID )
            ((vtkUnstructuredGrid*)smallGrid)->InsertNextCell( cell->GetCellType(), ptIdList );
         else if ( pointset->GetDataObjectType() == VTK_POLY_DATA )
            ((vtkPolyData*)smallGrid)->InsertNextCell( cell->GetCellType(), ptIdList );
      }
   }//for all cells
   ptIdList->Delete();
   cell->Delete();

   if ( debug ) cout << "\tsmallGrid->GetNumberOfCells() = "
                     << smallGrid->GetNumberOfCells() << endl;

   if ( pointset->GetPointData()->GetScalars() )
      smallGrid->GetPointData()->SetScalars( 
            pointset->GetPointData()->GetScalars() );

   if ( pointset->GetPointData()->GetVectors() )
      smallGrid->GetPointData()->SetVectors( 
            pointset->GetPointData()->GetVectors() );

   if ( pointset->GetPointData()->GetNormals() )
      smallGrid->GetPointData()->SetNormals( 
            pointset->GetPointData()->GetNormals() );

   int numPdArrays = pointset->GetPointData()->GetNumberOfArrays();
   if ( debug ) cout << "numPdArrays = " << numPdArrays << endl;

   for ( int i=0; i < numPdArrays; i++ )
   {
      if ( 
           ( pointset->GetPointData()->GetScalars() &&
             ! strcmp( pointset->GetPointData()->GetScalars()->GetName(),
                       pointset->GetPointData()->GetArray( i )->GetName() ) )
           ||
           ( pointset->GetPointData()->GetVectors() &&
             ! strcmp( pointset->GetPointData()->GetVectors()->GetName(),
                       pointset->GetPointData()->GetArray( i )->GetName() ) )
           ||
           ( pointset->GetPointData()->GetNormals() &&
             ! strcmp( pointset->GetPointData()->GetNormals()->GetName(),
                       pointset->GetPointData()->GetArray( i )->GetName() ) )
         )
      {
         if ( debug ) cout << "will not add "
              << pointset->GetPointData()->GetArray( i )->GetName()
              << " to point data field" << endl;
      }
      else
      {
         if ( debug ) cout << "will add "
              << pointset->GetPointData()->GetArray( i )->GetName()
              << " to point data field" << endl;
         smallGrid->GetPointData()->AddArray( 
               pointset->GetPointData()->GetArray( i ) );
      }
   }

   if ( pointset->GetDataObjectType() == VTK_UNSTRUCTURED_GRID )
   {
      pointset->Delete();
      pointset = vtkUnstructuredGrid::New();
   }
   else if ( pointset->GetDataObjectType() == VTK_POLY_DATA )
   {
      pointset->Delete();
      pointset = vtkPolyData::New();
   }
   else
   {
      cerr << "removeCellsOutsideBox can not handle this data object type"
           << endl;
      exit( 1 );
   }

   pointset->DeepCopy( smallGrid );
   smallGrid->Delete();
   delete [] isNeededPoint;
}

int main( int argc, char *argv[] )
{
   // Possibly read in an input vtk file name and an output file...
   char *inFileName = NULL;
   char *outFileName = NULL;
   fileIO::processCommandLineArgs( argc, argv, "cut external cells from",
                                   inFileName, outFileName );
   if ( ! inFileName ) return 1;

   // read the data set ("1" means print info to screen)
   vtkDataSet * dataset = readVtkThing( inFileName, 1 );

   int limitOption;
   double x_min, x_max, y_min, y_max, z_min, z_max;
   // if there are lots of items on command line, assume one was executable,
   // two were filenames, and six were extents, and one was limit option
   int arg = 3;
   if ( argc > 3 )
   {
      limitOption = atoi( argv[ arg++ ] );
      x_min = atof( argv[ arg++ ] );
      x_max = atof( argv[ arg++ ] );
      y_min = atof( argv[ arg++ ] );
      y_max = atof( argv[ arg++ ] );
      z_min = atof( argv[ arg++ ] );
      z_max = atof( argv[ arg++ ] );
      cout << "Using commandline-set extents..." << endl;
      cout << "\tlimitOption: " << limitOption << endl;
      cout << "\tx_min: " << x_min << endl;
      cout << "\tx_max: " << x_max << endl;
      cout << "\ty_min: " << y_min << endl;
      cout << "\ty_max: " << y_max << endl;
      cout << "\tz_min: " << z_min << endl;
      cout << "\tz_max: " << z_max << endl;
   }
   else
   {
      cout << "\nYou have a choice on how you specify the limits of the box."
           << "\n\t(0) All cells with any vertices in the box will be retained"
           << "\n\t(1) Only those cells totally inside the box will be retained"
           << endl;
      limitOption = fileIO::getIntegerBetween( 0, 1 );

      cout << "\nNow specify the limits of the box -- ";
      if ( limitOption == 0 )
         cout << "All cells with any vertices in the box will be retained" << endl;
      else if ( limitOption == 1 )
         cout << "Only those cells totally inside the box will be retained" << endl;

      cout << "\tinput x_min: ";
      cin >> x_min;
      cout << "\tinput x_max: ";
      cin >> x_max;
      cout << "\tinput y_min: ";
      cin >> y_min;
      cout << "\tinput y_max: ";
      cin >> y_max;
      cout << "\tinput z_min: ";
      cin >> z_min;
      cout << "\tinput z_max: ";
      cin >> z_max;
   }

   if ( limitOption == 0 )
   {
      vtkPointSet * pointset = vtkPointSet::SafeDownCast( dataset );
      if ( pointset == NULL )
      {
         cerr << "SafeDownCast to a pointset failed";
         exit( 1 );
      }

      removeCellsOutsideBox( pointset, x_min, x_max,
                             y_min, y_max, z_min, z_max );

      if ( debug ) 
         cout << "now to dumpVerticesNotUsedByCells..." << endl;
      dumpVerticesNotUsedByCells( pointset );

      if ( debug )
         writeVtkThing( pointset, outFileName, 0 ); // one is for binary
      else
         writeVtkThing( pointset, outFileName, 1 ); // one is for binary
   }
   else if ( limitOption == 1 )
   {
      //convert vtkDataSet to polydata
      vtkGeometryFilter *gFilter = vtkGeometryFilter::New();
      gFilter->SetInput( dataset );
      // Turn off merging of coincident points. 
      // Note that if merging is on, points with different point attributes
      // (e.g., normals) are merged, which may cause rendering artifacts. 
      gFilter->MergingOff();
      gFilter->SetExtent( x_min, x_max, y_min, y_max, z_min, z_max );
      gFilter->ExtentClippingOn();
      //gFilter->PointClippingOn();
      gFilter->Update();

      vtkPointSet * pointset;
      if ( dataset->GetDataObjectType() == VTK_UNSTRUCTURED_GRID )
      {
         // convert the polydata to an unstructured dataset...
         vtkAppendFilter *aFilter = vtkAppendFilter::New();
         aFilter->SetInput( gFilter->GetOutput() );  
         aFilter->GetOutput()->Update();
         //int numCells = aFilter->GetOutput()->GetNumberOfCells();

         pointset = vtkUnstructuredGrid::New();
         pointset->DeepCopy( aFilter->GetOutput() );
         aFilter->Delete();
      }
      else if ( dataset->GetDataObjectType() == VTK_POLY_DATA )
      {
         pointset = vtkPolyData::New();
         pointset->DeepCopy( gFilter->GetOutput() );
      }
      else
      {
         cerr <<"\nERROR - can only currently delete cells from "
              << "vtkUnstructuredGrids or vtkPolyData" << endl;
         delete [] inFileName;   inFileName = NULL;
         delete [] outFileName;  outFileName = NULL;
         dataset->Delete();
         exit(1);
      }

      cout << "now to dumpVerticesNotUsedByCells..." << endl;
      dumpVerticesNotUsedByCells( pointset );
      if ( debug )
         writeVtkThing( pointset, outFileName, 0 ); // one is for binary
      else
         writeVtkThing( pointset, outFileName, 1 ); // one is for binary

      pointset->Delete();
      gFilter->Delete();
   }

   delete [] inFileName;   inFileName = NULL;
   delete [] outFileName;  outFileName = NULL;
   //cout << "now to delete dataset..." << endl;
   dataset->Delete();
   return 0;
}


