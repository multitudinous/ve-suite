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
 * File:          $RCSfile: scaleVTK.cpp,v $
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

#include <vtkDataSet.h>
#include <vtkPointSet.h>
#include <vtkPolyData.h>
#include <vtkUnstructuredGrid.h>
#include <vtkTransform.h>
#include <vtkTransformFilter.h>
#include <vtkSetGet.h> // defines data object types

int main( int argc, char *argv[] )
{    
   int printInfoToScreen = 0; // "1" means print info to screen

   // If the command line contains an input vtk file name and an output file,
   // set them up.  Otherwise, get them from the user...
	char *inFileName = NULL;
	char *outFileName = NULL;
   fileIO::processCommandLineArgs( argc, argv, 
               "scale geometry AND vector data in", inFileName, outFileName );
   if ( ! inFileName ) return 1;
   int arg = 3;

   vtkDataSet * dataset = readVtkThing( inFileName, printInfoToScreen );

   if ( printInfoToScreen )
   {
      cout << "\nback in main..." << endl; 
      printWhatItIs( dataset );
   }
 
/*
   cout << "\nprinting dataset..." << endl;
   dataset->Print( cout );
*/

   // Determine scale method
   int velSCALE;
   if ( argc > 3 )
   {   
      cout << "Using commandline-set extents..." << endl;
      velSCALE = atoi( argv[ arg++ ] );
      cout << "\tvelSCALE: " << velSCALE << endl;
   }
   else
   {
      cout << "\nSelect the geometry scaling:" << endl;
      cout << "(0) No scaling" << endl;
      cout << "(1) Custom scaling" << endl;
      cout << "(2) meters to feet" << endl;
      cin >> velSCALE;
   }

   // Get factor to scale the geometry
   float geomScale [3] = {1.0f, 1.0f, 1.0f};    // default
   if      ( velSCALE == 0 ) {;}
   else if ( velSCALE == 1 )
   {
      for (int i=0; i<3; i++)
      {
         if ( argc > 3 )
         {   
            geomScale[i] = (float)atof( argv[ arg++ ] );
            cout << "\tgeomScale[" << i << "]: " << geomScale[i] << endl;
         }
         else
         {
            cout << "input the geometry scale factor for axis " << i << ": ";
            cin >> geomScale[i];
         }
      }
   }
   else if ( velSCALE == 2 )
   {
      geomScale[0] = 3.28083989501f;
      geomScale[1] = 3.28083989501f;
      geomScale[2] = 3.28083989501f;
   }
   else
      cout << "Invalid entry: will not scale geometry" << endl;

   // Transform the geometry 
   vtkTransform * transform = NULL;
   vtkTransformFilter * transFilter = NULL;
   //vtkPointSet * pointset = NULL;

   if ( dataset->IsA("vtkPointSet") )
   {
      //pointset = vtkPointSet::SafeDownCast( dataset );
      //if ( pointset == NULL ) cout << "SafeDownCast to a pointset failed";

      transform = vtkTransform::New();
      transform->Scale( geomScale[0], geomScale[1], geomScale[2] );

      //cout << "\nprinting pointset..." << endl;
      //pointset->Print( cout );

      transFilter = vtkTransformFilter::New();
      //transFilter->SetInput( pointset );
      transFilter->SetInput( (vtkPointSet*)dataset );
      transFilter->SetTransform( transform );

      //cout << "\nprinting transFilter->GetOutput()..." << endl;
      //transFilter->GetOutput()->Update();
      //transFilter->GetOutput()->Print( cout );

      if ( dataset->GetDataObjectType() == VTK_UNSTRUCTURED_GRID )
      {
         //cout << "\nprinting transFilter->GetUnstructuredGridOutput()..." << endl;
         //transFilter->GetUnstructuredGridOutput()->Update();
         //transFilter->GetUnstructuredGridOutput()->Print( cout );
         writeVtkThing( transFilter->GetUnstructuredGridOutput(), outFileName, 1 ); // one is for binary
      }
      else if ( dataset->GetDataObjectType() == VTK_STRUCTURED_GRID )
      {
         cout << "doing nothing for VTK_STRUCTURED_GRID" << endl;
      }
      else if ( dataset->GetDataObjectType() == VTK_POLY_DATA )
      {
         //cout << "\nprinting transFilter->GetPolyDataOutput()..." << endl;
         //transFilter->GetPolyDataOutput()->Print( cout );
         writeVtkThing( transFilter->GetPolyDataOutput(), outFileName, 1 ); // one is for binary
      }
      else
      {
         cout <<"\nERROR - Unsupported vtk object type: "
              << dataset->GetDataObjectType() << endl;
         return 1;
      }
      //pointset->Delete();
      transform->Delete();
      transFilter->Delete();
   }
   else
   {
      cout <<"\nERROR - can only scale vtkPointSets" << endl;
      dataset->Delete();
      delete [] inFileName;   inFileName = NULL;
      delete [] outFileName;  outFileName = NULL;
      return 1;
   }

   dataset->Delete();
   delete [] inFileName;   inFileName = NULL;
   delete [] outFileName;  outFileName = NULL;

   return 0;
}

