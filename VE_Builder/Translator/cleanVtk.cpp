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
 * File:          $RCSfile: cleanVtk.cpp,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include <iostream>
#include <fstream>

#include "vtkPoints.h"
#include "vtkUnstructuredGrid.h"
#include "vtkFieldData.h"
#include "vtkFloatArray.h"  // this code requires VTK4
#include "vtkGenericCell.h"
#include "vtkCellArray.h"
#include "vtkPointData.h"
#include "vtkPolyData.h"

void dumpVerticesNotUsedByCells( vtkPointSet * grid )
{
   if ( grid == NULL )
   {
       cerr << "ERROR: In dumpVerticesNotUsedByCells, grid = NULL" << endl;
       return;
   }

   // 0=no debug output, 1=some debug output, 2=more debug output
   int debug = 0;

   int j;
   int numVertices = grid->GetNumberOfPoints();
   if ( debug ) cout << "numVertices = " << numVertices << endl;

   // create index of needed points and initialize to "not needed"
   int * isNeededPoint = new int [ numVertices ];
   for (j=0; j<numVertices; j++) 
      isNeededPoint[j] = 0;

   if ( grid->IsA("vtkUnstructuredGrid") )
   {
      if ( debug ) cout << "working on vtkUnstructuredGrid" << endl;

      // if points are referenced by cells, then they are "needed"
      vtkIdType *pts, npts;
      for ( ((vtkUnstructuredGrid *)grid)->GetCells()->InitTraversal(); 
            ((vtkUnstructuredGrid *)grid)->GetCells()->GetNextCell(npts,pts); )
      {
         for (j=0; j<npts; j++) 
            isNeededPoint[ (int)pts[j] ] = 1;
      }
   }
   else if ( grid->IsA("vtkPolyData") )
   {
      if ( debug ) cout << "working on a vtkPolyData" << endl;

      // if points are referenced by polygons, then they are "needed"
      vtkIdType *pts, npts;
      for ( ((vtkPolyData*)grid)->GetPolys()->InitTraversal();
            ((vtkPolyData*)grid)->GetPolys()->GetNextCell(npts,pts); )
      {
         for (j=0; j<npts; j++)
            isNeededPoint[ (int)pts[j] ] = 1;
      }
   }
   else
   {
      cout << "NOTE: currently can only use 'dumpVerticesNotUsedByCells' "
           << "on vtkUnstructuredGrid or vtkPolyData" << endl;
      delete [] isNeededPoint;
      return;
   }

   // count the number of "needed points"
   int numNeededVertices = 0;
   for (j=0; j<numVertices; j++)
   {
      if ( debug > 1 && isNeededPoint[j] ) 
         cout << "\tisNeededPoint[" << j << "] = " << isNeededPoint[j] << endl;
      numNeededVertices += isNeededPoint[j];
   }
   if ( debug ) cout << "numNeededVertices = " << numNeededVertices << endl;

   int * old2NewVertexMapper = new int [ numVertices ];
   for (j=0; j < numVertices; j++) old2NewVertexMapper[j] = -1;

   if ( debug > 1 ) cout << "Vertex relationship..." << endl;
   vtkPoints *smallVertices = vtkPoints::New();
   double vertex [3];
   int k = 0;
   for (j=0; j < numVertices; j++)
   {
      if ( isNeededPoint[j] ) 
      {
         // get coordinates of the vertex and insert vertex in new mesh

         grid->GetPoints()->GetPoint( j, vertex );
         smallVertices->InsertPoint( k, vertex );
         old2NewVertexMapper[j] = k;
         if ( debug > 1 ) cout << "\told\t" << j << "\trevised\t" << k << endl;
         k++;
      }
   }

   // replace the vertex array if vertex array contains some extraneous data
   if ( numNeededVertices < numVertices ) 
   {
      vtkPointSet * smallGrid = NULL;

      if ( grid->IsA("vtkUnstructuredGrid") )
      {
         int numCells = ((vtkUnstructuredGrid *)grid)->GetNumberOfCells();
         if ( debug ) cout << "The number of cells is " << numCells << endl;

         smallGrid = vtkUnstructuredGrid::New();
         ((vtkUnstructuredGrid *)smallGrid)->Allocate( numCells, numCells );

         smallGrid->SetPoints( smallVertices );
         smallVertices->Delete(); 

         // Loop over the original cells and store revised definitions in smallGrid...
         int npts, ptId;
         vtkIdList *ptIdList = vtkIdList::New();
         vtkGenericCell *cell = vtkGenericCell::New();

         for(int cellId=0; cellId < numCells; cellId++)
         {      
            ((vtkUnstructuredGrid *)grid)->GetCell( cellId, cell );
            if ( debug > 1 ) cout << "\tcellType = " << cell->GetCellType() << endl;

            npts = cell->GetNumberOfPoints();
            ptIdList->Reset();
            for ( int i=0; i < npts; i++)
            {
               ptId = cell->GetPointId( i );
               ptIdList->InsertId( i, old2NewVertexMapper[ptId] );
            }
            ((vtkUnstructuredGrid *)smallGrid)->InsertNextCell( cell->GetCellType(), ptIdList );
         }//for all cells
         ptIdList->Delete();
         cell->Delete();
      }
      else if ( grid->IsA("vtkPolyData") )
      {
         int numCells = ((vtkPolyData*)grid)->GetNumberOfCells();
         if ( debug ) cout << "The number of cells is " << numCells << endl;

         smallGrid = vtkPolyData::New();
         ((vtkPolyData*)smallGrid)->Allocate( numCells, numCells );

         smallGrid->SetPoints( smallVertices );
         smallVertices->Delete(); 

         // Loop over the original cells and store revised definitions in smallGrid...
         int npts, ptId;
         vtkIdList *ptIdList = vtkIdList::New();
         vtkGenericCell *cell = vtkGenericCell::New();

         for(int cellId=0; cellId < numCells; cellId++)
         {      
            ((vtkPolyData*)grid)->GetCell( cellId, cell );
            if ( debug > 1 ) cout << "\tcellType = " << cell->GetCellType() << endl;

            npts = cell->GetNumberOfPoints();
            ptIdList->Reset();
            for ( int i=0; i < npts; i++)
            {
               ptId = cell->GetPointId( i );
               ptIdList->InsertId( i, old2NewVertexMapper[ptId] );
            }
            ((vtkPolyData*)smallGrid)->InsertNextCell( cell->GetCellType(), ptIdList );
         }//for all cells
         ptIdList->Delete();
         cell->Delete();
     }

      //cout << "grid->GetPointData()->GetVectors() = " << grid->GetPointData()->GetVectors() << endl;
      // if the data set has VECTORS data, then move over appropriate data...
      if ( grid->GetPointData()->GetVectors() )
      {
         vtkFloatArray *smallVectorSet = vtkFloatArray::New();
         smallVectorSet->SetNumberOfComponents( 3 );
         smallVectorSet->SetName( grid->GetPointData()->GetVectors()->GetName() );

         double vectorTemp[3];
         for (j=0; j < numVertices; j++)
         {   
            if ( isNeededPoint[j] )
            {
               grid->GetPointData()->GetVectors()->GetTuple( j, vectorTemp );
               smallVectorSet->InsertTuple( old2NewVertexMapper[j], vectorTemp );
            }
         }
         smallGrid->GetPointData()->SetVectors( smallVectorSet );
         smallVectorSet->Delete();
      }

      //cout << "grid->GetPointData()->GetScalars() = " << grid->GetPointData()->GetScalars() << endl;
      // if the data set has SCALARS data, then move over appropriate data...
      if ( grid->GetPointData()->GetScalars() )
      {
         vtkFloatArray *smallScalarSet = vtkFloatArray::New();
         smallScalarSet->SetNumberOfComponents( 1 );
         smallScalarSet->SetName( grid->GetPointData()->GetScalars()->GetName() );

         double scalarTemp[1];
         for (j=0; j < numVertices; j++)
         {   
            if ( isNeededPoint[j] )
            {
               grid->GetPointData()->GetScalars()->GetTuple( j, scalarTemp );
               smallScalarSet->InsertTuple( old2NewVertexMapper[j], scalarTemp );
            }
         }
         smallGrid->GetPointData()->SetScalars( smallScalarSet );
         smallScalarSet->Delete();
      }

      //cout << "grid->GetPointData()->GetNormals() = " << grid->GetPointData()->GetNormals() << endl;
      // if the data set has Normals data, then move over appropriate data...
      if ( grid->GetPointData()->GetNormals() )
      {
         vtkFloatArray *smallVectorSet = vtkFloatArray::New();
         smallVectorSet->SetNumberOfComponents( 3 );
         smallVectorSet->SetName( grid->GetPointData()->GetNormals()->GetName() );

         double vectorTemp[3];
         for (j=0; j < numVertices; j++)
         {   
            if ( isNeededPoint[j] )
            {
               grid->GetPointData()->GetNormals()->GetTuple( j, vectorTemp );
               smallVectorSet->InsertTuple( old2NewVertexMapper[j], vectorTemp );
            }
         }
         smallGrid->GetPointData()->SetNormals( smallVectorSet );
         smallVectorSet->Delete();
      }

      // if the data set has FIELD data, then move over appropriate data...
      vtkFieldData * field = NULL;
      vtkFloatArray * tempArray = NULL;
      double * tuple = NULL;
      int numFieldArrays = grid->GetFieldData()->GetNumberOfArrays();
      if ( debug ) cout << "numFieldArrays = " << numFieldArrays << endl;
      if ( numFieldArrays )
      {
         field = vtkFieldData::New();
         field->AllocateArrays( numFieldArrays );
      
         for (int i=0; i<numFieldArrays; i++ )
         {
            int numComponents = grid->GetFieldData()->GetArray(i)->GetNumberOfComponents();
            tempArray = vtkFloatArray::New();
            tempArray->SetNumberOfComponents( numComponents );
            tempArray->SetName( grid->GetFieldData()->GetArray(i)->GetName() );
            tuple = new double [numComponents];
            for (j=0; j< grid->GetFieldData()->GetArray(i)->GetNumberOfTuples(); j++)
            {
               if ( isNeededPoint[j] ) 
               {
                  grid->GetFieldData()->GetArray(i)->GetTuple( j, tuple );
                  tempArray->InsertTuple( old2NewVertexMapper[j], tuple );
               }
            }
            field->AddArray( tempArray );
            tempArray->Delete();
            delete [] tuple;
            tuple = NULL;
         }
         smallGrid->SetFieldData( field ); 
         field->Delete(); 
      }

      // if the data set has FIELD data connected to the point data, then move over appropriate data...
      numFieldArrays = grid->GetPointData()->GetNumberOfArrays();
      if ( debug ) cout << "numFieldArrays connected to the point data = " << numFieldArrays << endl;
      if ( numFieldArrays )
      {
         smallGrid->GetPointData()->AllocateArrays( numFieldArrays ); 
      
         for (int i=0; i<numFieldArrays; i++ )
         {
            int numComponents = grid->GetPointData()->GetArray(i)->GetNumberOfComponents();
            tempArray = vtkFloatArray::New();
            tempArray->SetNumberOfComponents( numComponents );
            tempArray->SetName( grid->GetPointData()->GetArray(i)->GetName() );
            tuple = new double [numComponents];
            for (j=0; j< grid->GetPointData()->GetArray(i)->GetNumberOfTuples(); j++)
            {
               if ( isNeededPoint[j] ) 
               {
                  grid->GetPointData()->GetArray(i)->GetTuple( j, tuple );
                  tempArray->InsertTuple( old2NewVertexMapper[j], tuple );
               }
            }
            smallGrid->GetPointData()->AddArray( tempArray );
            tempArray->Delete();
            delete [] tuple;
            tuple = NULL;
         }
      }

      delete [] old2NewVertexMapper;

      smallGrid->Update();

      if ( grid->IsA("vtkUnstructuredGrid") )
      {
         //grid->Delete();
         //grid->ShallowCopy( (vtkUnstructuredGrid*)smallGrid );
         grid->DeepCopy( (vtkUnstructuredGrid*)smallGrid );
      }
      else if ( grid->IsA("vtkPolyData") )
      {
         //grid->Delete();
         //grid->ShallowCopy( (vtkPolyData*)smallGrid );
         grid->DeepCopy( (vtkPolyData*)smallGrid );
      }

      grid->Squeeze();
      smallGrid->Delete();
   }
   else
   {
      if ( debug ) cout << "Will NOT try to collapse the file" << endl;
      smallVertices->Delete(); 
   }

   delete [] isNeededPoint;
}

void dumpVerticesNotUsedByCells( vtkPointSet * grid, char * vtkFileName )
{
   if ( grid == NULL )
   {
       cerr << "ERROR: In dumpVerticesNotUsedByCells, grid = NULL" << endl;
       return;
   }

   int debug = 0;   // 0=no debug output, 1=some debug output, 2=more debug output
   fstream rpt;
   rpt.open( vtkFileName, ios::out ); 

   cout << "Writing to " << vtkFileName << "..." << endl;

   int j;
   int numVertices = grid->GetNumberOfPoints();
   if ( debug ) cout << "numVertices = " << numVertices << endl;

   // create index of needed points and initialize to "not needed"
   int * isNeededPoint = new int [ numVertices ];
   for (j=0; j<numVertices; j++) 
      isNeededPoint[j] = 0;

   if ( grid->IsA("vtkUnstructuredGrid") )
   {
      if ( debug ) cout << "working on vtkUnstructuredGrid" << endl;

//      vtkUnstructuredGrid * uGrid = vtkUnstructuredGrid::SafeDownCast( grid );
//      if ( uGrid == NULL ) cout << "SafeDownCast to a vtkUnstructuredGrid failed";

      // if points are referenced by cells, then they are "needed"
      vtkIdType *pts, npts;
//      for ( uGrid->GetCells()->InitTraversal(); uGrid->GetCells()->GetNextCell(npts,pts); )
      for ( ((vtkUnstructuredGrid *)grid)->GetCells()->InitTraversal(); 
            ((vtkUnstructuredGrid *)grid)->GetCells()->GetNextCell(npts,pts); )
         for (j=0; j<npts; j++) isNeededPoint[ (int)pts[j] ] = 1;
   }
/*
   else if ( grid->IsA("vtkPolyData") )
   {
      if ( debug ) cout << "working on a vtkPolyData" << endl;

      // if points are referenced by polygons, then they are "needed"
      vtkIdType *pts, npts;
      for ( ((vtkPolyData*)grid)->GetPolys()->InitTraversal();
            ((vtkPolyData*)grid)->GetPolys()->GetNextCell(npts,pts); )
         for (j=0; j<npts; j++) isNeededPoint[ (int)pts[j] ] = 1;
   }
*/
   else
   {
      cout << "NOTE: currently can only use 'dumpVerticesNotUsedByCells' "
           << "on vtkUnstructuredGrids" << endl;
      delete [] isNeededPoint;
      return;
   }

   // count the number of "needed points"
   int numNeededVertices = 0;
   for (j=0; j<numVertices; j++)
   {
      if ( debug > 1 && isNeededPoint[j] ) 
         cout << "\tisNeededPoint[" << j << "] = " << isNeededPoint[j] << endl;
      numNeededVertices += isNeededPoint[j];
   }
   if ( debug ) cout << "numNeededVertices = " << numNeededVertices << endl;

   int * old2NewVertexMapper = new int [ numVertices ];
   for (j=0; j < numVertices; j++) old2NewVertexMapper[j] = -1;

   if ( debug > 1 ) cout << "Vertex relationship..." << endl;
   int k = 0;
   for (j=0; j < numVertices; j++)
   {
      if ( isNeededPoint[j] ) 
      {
         old2NewVertexMapper[j] = k;
         if ( debug > 1 ) cout << "\told\t" << j << "\trevised\t" << k << endl;
         k++;
      }
   }

   rpt << "# vtk DataFile Version 3.0" << endl;
   rpt << "vtk output" << endl;
   rpt << "ASCII" << endl;
   rpt << "DATASET UNSTRUCTURED_GRID" << endl;
   rpt << "POINTS " << numNeededVertices << " float" << endl;
   char str[1024];
   int counter = 0;
   for (j=0; j<numVertices; j++)
   {
      if ( ! isNeededPoint[j] )
         continue;
      
      double vertex [3];
      grid->GetPoints()->GetPoint( j, vertex ); // get vertex coordinates

      for (int k=0; k<3; k++)
      {
         sprintf ( str, "%g ", vertex[k] );
         rpt << str; 
         counter++;
         if ( counter%9 == 0 )
            rpt << "\n";
      }
   }
   rpt << "\n";

   int numCells = ((vtkUnstructuredGrid *)grid)->GetCells()
                                       ->GetNumberOfCells();

   int size = ((vtkUnstructuredGrid *)grid)->GetCells()
                                       ->GetNumberOfConnectivityEntries();

   rpt << "CELLS " << numCells << " " << size << endl;

   vtkIdType *pts = 0;
   vtkIdType npts = 0;
   for (((vtkUnstructuredGrid *)grid)->GetCells()->InitTraversal();
        ((vtkUnstructuredGrid *)grid)->GetCells()->GetNextCell(npts,pts); )
   {
      rpt << (int)npts << " ";
      for (j=0; j<npts; j++)
         rpt << old2NewVertexMapper[ (int)pts[j] ] << " ";
      rpt << "\n";
   }
   rpt << "\n";

   rpt << "CELL_TYPES " << numCells << "\n";
   int * types = new int[numCells];
   for (int cellId=0; cellId < numCells; cellId++)
   {
      types[cellId] = ((vtkUnstructuredGrid *)grid)->GetCellType(cellId);
      rpt << types[cellId] << "\n";
   }
   rpt << "\n";

   rpt << "CELL_DATA " << numCells << endl;
   rpt << "POINT_DATA " << numNeededVertices << endl;

   int numFieldArrays = grid->GetPointData()->GetNumberOfArrays();

   if ( numFieldArrays )
   {
      for (int i=0; i<numFieldArrays; i++ )
      {
         int numComponents = grid->GetPointData()->GetArray(i)->GetNumberOfComponents();
         if ( numComponents > 1 )
         {
            const char * name = grid->GetPointData()->GetArray(i)->GetName();

            // only NORMALS won't be written in field data
            if ( strcmp(name,"NORMALS") )
               continue;
   /* 
            //in vtk, transforming can cause loss of name...
            const char * name = grid->GetPointData()->GetArray(i)->GetName();
            if ( ! strcmp(name,"") )
               rpt << "VECTORS vectors float" << endl;
            else
               rpt << "VECTORS " << name << " float" << endl;
   */
            rpt << "VECTORS " << name << " float" << endl;
            double * tuple = new double [numComponents];
            counter = 0;
            for (j=0; j< grid->GetPointData()->GetArray(i)->GetNumberOfTuples(); j++)
            {
               if ( isNeededPoint[j] ) 
               {
                  grid->GetPointData()->GetArray(i)->GetTuple( j, tuple );
                  for (int k=0; k<numComponents; k++)
                  {
                     rpt << tuple[ k ] << " ";
                     counter++;
                     if ( counter%9 == 0 )
                        rpt << "\n";
                  }
               }
            }
            delete [] tuple;
            rpt << "\n";
            numFieldArrays--;
         }
      }

      rpt << "FIELD FieldData " << numFieldArrays << endl;
      for (int i=0; i<numFieldArrays; i++ )
      {
         counter = 0;
         const char * name = grid->GetPointData()->GetArray(i)->GetName();
         int numComponents = grid->GetPointData()->GetArray(i)->GetNumberOfComponents();

         // NORMALS won't be written in field data
         if ( ! strcmp(name,"NORMALS") )
            continue;

         //in vtk, transforming can cause loss of name...
         if ( ! strcmp(name,"") )
            rpt << "lost_name "  << numComponents << " " << numNeededVertices << " float" << endl;
         else
            rpt << name << " "  << numComponents << " " << numNeededVertices << " float" << endl;

         double * tuple = new double [numComponents];
         for (j=0; j< grid->GetPointData()->GetArray(i)->GetNumberOfTuples(); j++)
         {
            if ( isNeededPoint[j] ) 
            {
               grid->GetPointData()->GetArray(i)->GetTuple( j, tuple );
               for (int k=0; k<numComponents; k++)
               {
                  rpt << tuple[ k ] << " ";
                  counter++;
                  if ( counter%9 == 0 )
                     rpt << "\n";
               }
            }
         }
         delete [] tuple;
         rpt << "\n";
      }
   }
   rpt.close();

   delete [] types;
   delete [] isNeededPoint;
   delete [] old2NewVertexMapper;
   cout << "done" << endl;
   return;
}

