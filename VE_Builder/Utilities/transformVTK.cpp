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
 * File:          $RCSfile: transformVTK.cpp,v $
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

#include <vtkUnstructuredGrid.h>
#include <vtkTransform.h>
#include <vtkTransformFilter.h>
#include <vtkDataSet.h>
#include <vtkPointSet.h>


int main( int argc, char *argv[] )
{    
   // Possibly read in an input vtk file name and an output file...
	char *inFileName = NULL;
	char *outFileName = NULL;
   fileIO::processCommandLineArgs( argc, argv, "transform file", inFileName, outFileName );
   if ( ! inFileName ) return 1;

   vtkDataSet * dataset = readVtkThing( inFileName, 1 ); // "1" means print info to screen
   if ( ! dataset->IsA("vtkPointSet") )
   { 
      delete [] inFileName;   inFileName = NULL;
      delete [] outFileName;  outFileName = NULL;
      return 1;
   }

   float rotX, rotY, rotZ;
   float transX, transY, transZ;

   int arg = 3;
   if ( argc == 9 )
   {   
      rotX   = (float)atof( argv[ arg++ ] );
      rotY   = (float)atof( argv[ arg++ ] );
      rotZ   = (float)atof( argv[ arg++ ] );
      transX = (float)atof( argv[ arg++ ] );
      transY = (float)atof( argv[ arg++ ] );
      transZ = (float)atof( argv[ arg++ ] );
      cout << "Using commandline-set extents..." << endl;
      cout << "\trotX: " << rotX << endl;
      cout << "\trotY: " << rotY << endl;
      cout << "\trotZ: " << rotZ << endl;
      cout << "\ttransX: " << transX << endl;
      cout << "\ttransY: " << transY << endl;
      cout << "\ttransZ: " << transZ << endl;
   }
   else
    {
       cout << "\nTo rotate X (degrees) : ";
        cin >> rotX;

        cout << "To rotate Y (degrees) : ";
        cin >> rotY;

        cout << "To rotate Z (degrees) : ";
        cin >> rotZ;
 
        cout << "\nTo translate X value : ";
        cin >> transX;
        cout << "To translate Y value : ";
        cin >> transY;
        cout << "To translate Z value : ";
        cin >> transZ;

    }

    // Transform the geometry 
    vtkTransform * aTransform = vtkTransform::New();
    aTransform->RotateX( rotX );
    aTransform->RotateY( rotY );
    aTransform->RotateZ( rotZ );

    // Create a translation matrix and concatenate it with the current transformation
    // according to PreMultiply or PostMultiply semantics
    aTransform->Translate( transX, transY, transZ );

    // Transformation 
    vtkTransformFilter *transFilter = vtkTransformFilter::New();
//        vtkPointSet* ptSet = vtkPointSet::SafeDownCast( dataset );
//        if ( ptSet == NULL ) cout << "SafeDownCast to a vtkPointSet failed";
        transFilter->SetInput( (vtkPointSet *)dataset );
        transFilter->SetTransform( aTransform );
     
    writeVtkThing( transFilter->GetOutput(), outFileName, 1 ); // one is for binary

   transFilter->Delete();
   aTransform->Delete();
   delete [] inFileName;   inFileName = NULL;
   delete [] outFileName;  outFileName = NULL;
   dataset->Delete();
   return 0;
}

