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
 * File:          $RCSfile: appendVTK.cpp,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#include <iostream>
#include <cstdio>
#include <cstdlib>
//#include <fstream.h>

#include "fileIO.h"
#include "readWriteVtkThings.h"

#include <vtkAppendFilter.h>
#include <vtkTransformFilter.h>
#include <vtkUnstructuredGrid.h>
#include <vtkTransform.h>
#include <vtkTransformFilter.h>

int main( int argc, char *argv[] )
{    
    // Read in a file containing an unstructured grid...
	char * inFileName1 = 0;
	char * inFileName2 = 0;
	char * outFileName = 0;
    if (argc > 1)
    {
        inFileName1 = new char [100];
        strcpy( inFileName1, argv[1]);

        if (argc > 2)
        {
            inFileName2 = new char [100];
            strcpy( inFileName2, argv[2]);
        }
        else inFileName2 = fileIO::getReadableFileFromDefault( "the file to be appended", "inFile.vtk" );

        if (argc > 3)
        {
            outFileName = new char [100];
            strcpy( outFileName, argv[3]);
        }
        else outFileName = fileIO::getWritableFile( "outFile.vtk" );

        char response;
        do 
        {
            cout << "\nSo you want to add " << inFileName1 << " to " << inFileName2  << " to get " << outFileName << "? (y/n): ";
            cin >> response;
        } while (response != 'y' && response != 'Y' && response != 'n' && response != 'N');

        //if anything other than y/Y was input then get filenames from user...
        if (response != 'y' && response != 'Y') argc = 1;
    }

    if (argc == 1)  // then get filenames from user...
    {
        inFileName1 = fileIO::getReadableFileFromDefault( "the first file", "inFile.vtk" );
        inFileName2 = fileIO::getReadableFileFromDefault( "the file to be appended", "inFile.vtk" );
        outFileName = fileIO::getWritableFile( "outFile.vtk" );
    }

   vtkDataSet * dataset1 = readVtkThing( inFileName1, 1 ); // "1" means print info to screen
   vtkDataSet * dataset2 = readVtkThing( inFileName2, 1 ); // "1" means print info to screen

    // Transform the geometry 
    int B_trans;
    float rotX = 0.0, rotY = 0.0, rotZ = 0.0;
    float transX = 0.0, transY = 0.0, transZ = 0.0;

    cout << "\nTransform (translate/rotate) the geometry of " << inFileName2 << "? (0) No (1) Yes " << endl;
    cin >> B_trans;

    if ( B_trans )
    {
        cout << "\nTo translate X value : ";
        cin >> transX;
        
        cout << "To translate Y value : ";
        cin >> transY;

        cout << "To translate Z value : ";
        cin >> transZ;

        cout << "\nTo rotate X (degrees) : ";
        cin >> rotX;

        cout << "To rotate Y (degrees) : ";
        cin >> rotY;

        cout << "To rotate Z (degrees) : ";
        cin >> rotZ;
    }

    vtkTransform * t = vtkTransform::New();
       t->Translate( transX, transY, transZ );
       t->RotateX( rotX );
       t->RotateY( rotY );
       t->RotateZ( rotZ );

    vtkTransformFilter * tFilter = vtkTransformFilter::New();
       tFilter->SetInput( (vtkPointSet *)dataset2 );
       tFilter->SetTransform( t );
       tFilter->Update();

   vtkAppendFilter * aFilter = vtkAppendFilter::New();
       aFilter->AddInput( dataset1 );
       aFilter->AddInput( tFilter->GetOutput() );
       aFilter->Update();

  writeVtkThing( aFilter->GetOutput(), outFileName, 1 ); // one is for binary

   tFilter->Delete();
   aFilter->Delete();
   t->Delete();
   delete [] inFileName1;  inFileName1 = NULL;
   delete [] inFileName2;  inFileName2 = NULL;
   delete [] outFileName;  outFileName = NULL;
   dataset1->Delete();
   dataset2->Delete();

   cout << "... done" << endl;
   cout << endl;

   return 0;
}

