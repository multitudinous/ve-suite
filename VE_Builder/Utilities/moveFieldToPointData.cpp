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
 * File:          $RCSfile: moveFieldToPointData.cpp,v $
 * Date modified: $Date: 2004/03/23 16:36:52 $
 * Version:       $Revision: 1.5 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include <iostream>
#include <cstdio>
#include <cstdlib>

#include "fileIO.h"
#include "readWriteVtkThings.h"

#include "vtkFieldData.h"
#include "vtkFloatArray.h"
#include "vtkDataSet.h"
#include "vtkPointData.h"


int main( int argc, char *argv[] )
{
   // Possibly read in an input vtk file name and an output file...
	char *inFileName = NULL;
	char *outFileName = NULL;
   fileIO::processCommandLineArgs( argc, argv, "move field to point data arrays in", inFileName, outFileName );
   if ( ! inFileName ) return 1;

   vtkDataSet * dataset = readVtkThing( inFileName, 1 ); // "1" means print info to screen

/*
   cout << "\nback in main..." << endl;
   cout << "dataset->IsA(\"vtkDataSet\") =          " << dataset->IsA("vtkDataSet") << endl;
   cout << "dataset->IsA(\"vtkPointSet\") =         " << dataset->IsA("vtkPointSet") << endl;
   cout << "dataset->IsA(\"vtkUnstructuredGrid\") = " << dataset->IsA("vtkUnstructuredGrid") << endl;
   cout << "dataset->IsA(\"vtkStructuredGrid\") =   " << dataset->IsA("vtkStructuredGrid") << endl;
   cout << "dataset->IsA(\"vtkPolyData\") =         " << dataset->IsA("vtkPolyData") << endl;
   cout << "dataset->GetDataObjectType() =          " << dataset->GetDataObjectType() << endl;
   cout << endl;
*/

   // if the data set has FIELD data, then move over appropriate data to point data arrays...
   char scalarName [100], vectorName [100];

   int numFieldArrays = dataset->GetFieldData()->GetNumberOfArrays();
   //cout << "numFieldArrays = " << numFieldArrays << endl;
   cout << endl;

   // If there are field data, move it (and scalar and vector) to the point data section...
   if ( numFieldArrays )
   {  
      int i=0;
      for ( i=0; i<numFieldArrays; i++ )
      {
         cout << "moving field \"" << dataset->GetFieldData()->GetArray(i)->GetName() << "\" to the point data field" << endl;
         dataset->GetPointData()->AddArray( dataset->GetFieldData()->GetArray(i) );
      }

      //cout << "dataset->GetPointData()->GetScalars() = " << dataset->GetPointData()->GetScalars() << endl;
      if ( dataset->GetPointData()->GetScalars() ) 
      {
         vtkFloatArray * scalars = vtkFloatArray::New();
         scalars->DeepCopy( dataset->GetPointData()->GetScalars() );
         strcpy( scalarName, dataset->GetPointData()->GetScalars()->GetName() );
         cout << "moving scalar \"" << scalarName << "\" to the point data field" << endl;
         dataset->GetPointData()->RemoveArray( scalarName );
         dataset->Update();
         scalars->SetName( scalarName ); // have to do this or the name will be NULL
         dataset->GetPointData()->AddArray( scalars );
         scalars->Delete();
      }

      //cout << "dataset->GetPointData()->GetVectors() = " << dataset->GetPointData()->GetVectors() << endl;
      if ( dataset->GetPointData()->GetVectors() ) 
      {
         vtkFloatArray *  vectors = vtkFloatArray::New();
         vectors->DeepCopy( dataset->GetPointData()->GetVectors() );
         strcpy( vectorName, dataset->GetPointData()->GetVectors()->GetName() );
         cout << "moving vector \"" << vectorName << "\" to the point data field" << endl;
         dataset->GetPointData()->RemoveArray( vectorName );
         vectors->SetName( vectorName ); // have to do this or the name will be NULL
         dataset->GetPointData()->AddArray( vectors );
         vectors->Delete();
      }

      for ( i=0; i<numFieldArrays; i++ )
          dataset->GetFieldData()->RemoveArray( dataset->GetFieldData()->GetArrayName(0) );

      writeVtkThing( dataset, outFileName, 1 );    // 1 is binary
   }
   else
      cout << "\nNOTE: This file did not require reformatting. No changes were made.\n" << endl;

   delete [] inFileName;   inFileName = NULL;
   delete [] outFileName;  outFileName = NULL;
   dataset->Delete();
   return 0;
}


