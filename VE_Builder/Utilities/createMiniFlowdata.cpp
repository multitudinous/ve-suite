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
 * File:          $RCSfile: createMiniFlowdata.cpp,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include <iostream>
#include <cstdio>
#include <cstdlib>

#include "fileIO.h"
#include "readWriteVtkThings.h"
#include "cfdAccessoryFunctions.h"

#include <vtkDataSet.h>
#include <vtkPointData.h>
#include <vtkUnstructuredGrid.h>
#include <vtkPoints.h>
#include <vtkCellType.h>
#include <vtkFloatArray.h>

int main( int argc, char *argv[] )
{    
   // Possibly read in an input vtk file name and an output file...
   // there is no default input file name...
	char * inFileName = NULL;

   // there is a default output file name...
	char * outFileName = new char [ strlen("miniFlowdata.vtk")+1 ];
   strcpy( outFileName, "miniFlowdata.vtk" );

   fileIO::processCommandLineArgs( argc, argv,
                                   "create a mini flowdata.vtk file from",
                                   inFileName, outFileName );
   if ( ! inFileName ) return 1;

   vtkDataSet * dataset = readVtkThing( inFileName, 1 ); // "1" means print info to screen
   if ( ! dataset->IsA("vtkUnstructuredGrid") )
   { 
      cerr << "ERROR: This function requires an unstructured grid" << endl;
      delete [] inFileName;   inFileName = NULL;
      delete [] outFileName;  outFileName = NULL;
      return 1;
   }

/*
   // Get the bounds of the data set, where ...
   bounds[ 0 ] = x-min 
   bounds[ 1 ] = x-max
   bounds[ 2 ] = y-min 
   bounds[ 3 ] = y-max 
   bounds[ 4 ] = z-min
   bounds[ 5 ] = z-max
*/
   double bounds[ 6 ];
   dataset->GetBounds( bounds );

   // We are going to create a single hexagonal element,
   // having the same bounding box as the original data set,
   // with the extreme scalar/vector quantities at the top and bottom sides
   vtkPoints *vertices = vtkPoints::New();
   vertices->InsertPoint( 0, bounds[ 0 ], bounds[ 2 ], bounds[ 4 ] );
   vertices->InsertPoint( 1, bounds[ 1 ], bounds[ 2 ], bounds[ 4 ] );
   vertices->InsertPoint( 2, bounds[ 1 ], bounds[ 3 ], bounds[ 4 ] );
   vertices->InsertPoint( 3, bounds[ 0 ], bounds[ 3 ], bounds[ 4 ] );
   vertices->InsertPoint( 4, bounds[ 0 ], bounds[ 2 ], bounds[ 5 ] );
   vertices->InsertPoint( 5, bounds[ 1 ], bounds[ 2 ], bounds[ 5 ] );
   vertices->InsertPoint( 6, bounds[ 1 ], bounds[ 3 ], bounds[ 5 ] );
   vertices->InsertPoint( 7, bounds[ 0 ], bounds[ 3 ], bounds[ 5 ] );

   vtkUnstructuredGrid * uGrid = vtkUnstructuredGrid::New();
   uGrid->SetPoints( vertices );

   int temp [ 8 ] = {0,1,2,3,4,5,6,7};
   uGrid->Allocate( 1, 1 );
   uGrid->InsertNextCell( VTK_HEXAHEDRON, 8, temp );

   int numArrays = dataset->GetPointData()->GetNumberOfArrays();
   //cout << "numArrays = " << numArrays << endl;

   // set up arrays to store data...
   vtkFloatArray ** data = new vtkFloatArray * [ numArrays ];

   double minMax[2];
   for (int i = 0; i < numArrays; i++)
   {
      int numComponents = dataset->GetPointData()->GetArray( i )
                                                 ->GetNumberOfComponents() ;
      data[ i ] = vtkFloatArray::New();
      data[ i ]->SetNumberOfComponents( numComponents );
      data[ i ]->SetName( dataset->GetPointData()->GetArray( i )->GetName() );
      data[ i ]->SetNumberOfTuples( 8 );

      if ( numComponents != 1 && numComponents != 3 )
      {
         cout << "ERROR: Unexpected number of components (" 
              << numComponents << ") in array " << i << endl;
         continue;
      }

      if ( numComponents == 1 )
      {
         dataset->GetPointData()->GetArray( i )->GetRange( minMax );
         cout << "array " << i << ": scalar named \"" 
              << dataset->GetPointData()->GetArray( i )->GetName() 
              << "\", range:\t" 
              << minMax[ 0 ] << "\t" << minMax[ 1 ] << endl;

         for (int j = 0; j < 4; j++)
            data[ i ]->SetTuple1( j, minMax[ 0 ] );
         for (int j = 4; j < 8; j++)
            data[ i ]->SetTuple1( j, minMax[ 1 ] );
      }
      else
      {
         double * vecMagRange = cfdAccessoryFunctions::
                                ComputeVectorMagnitudeRange( 
                                      dataset->GetPointData()->GetArray( i ) );

         cout << "array " << i << ": vector named \"" 
              << dataset->GetPointData()->GetArray( i )->GetName() 
              << "\", vector magnitude range:\t" 
              << vecMagRange[ 0 ] << "\t" << vecMagRange[ 1 ] << endl;

         // set the z-coord to the vector magnitude range...
         for (int j = 0; j < 4; j++)
            data[ i ]->SetTuple3( j, 0.0, 0.0, vecMagRange[ 0 ] );
         for (int j = 4; j < 8; j++)
            data[ i ]->SetTuple3( j, 0.0, 0.0, vecMagRange[ 1 ] );
         delete [] vecMagRange;
      }
      uGrid->GetPointData()->AddArray( data[ i ] );
   }

   writeVtkThing( uGrid, outFileName, 0 ); // one is for binary

   dataset->Delete();
   vertices->Delete();
   uGrid->Delete();
   for (int i = 0; i < numArrays; i++)
   {
      data[ i ]->Delete();
   }
   delete [] inFileName;   inFileName = NULL;
   delete [] outFileName;  outFileName = NULL;

   return 0;
}

