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
 * File:          $RCSfile: meshViewer.cpp,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include <iostream>

#include <vtkStructuredGrid.h>
#include <vtkUnstructuredGrid.h>
#include <vtkRectilinearGrid.h>

#include <vtkRenderer.h>
#include <vtkRenderWindow.h>
#include <vtkRenderWindowInteractor.h>
#include <vtkActor.h>
#include <vtkFollower.h>
#include <vtkCubeSource.h>
#include <vtkOutlineCornerFilter.h>
#include <vtkPolyData.h>
#include <vtkPolyDataMapper.h>
#include <vtkDataSetMapper.h>
#include <vtkCamera.h>
#include <vtkPointData.h>
#include <vtkProperty.h>
#include <vtkSTLReader.h>
#include <vtkScalarsToColors.h>
#include <vtkLookupTable.h>

#include "cfdImage.h"  //BMP support
#include "fileIO.h"
#include "viewCells.h"
#include "setScalarAndVector.h"
#include "cfdGrid2Surface.h"

#include "readWriteVtkThings.h"  //debugging

void viewWhatsInFile( char * vtkFilename, const float shrinkFactor );
vtkActor * getActorFromDataSet( vtkDataSet * dataset );
vtkActor * getActorFromFile( char * vtkFilename );

int main( const int argc, char *argv[] )
{  

   int i;
	char * vtkFilename = NULL;
   float shrinkFactor = 1.0;
   if (argc == 1)  // Process a single vtk file that will be obtained from the user...
   {
       char tempText[ 100 ]; 
       strcpy( tempText, "input" );
       vtkFilename = fileIO::getReadableFileFromDefault( tempText, 
                                                         "delaunay3DOut.vtk" );
       shrinkFactor = 0.95;
       cout << "Recommended shrinkFactor = " << shrinkFactor << endl;
       do
       {
           cout << "Input your shrinkFactor (0 < X <= 1, with 1 for no shrinkage)" << endl;
           cin >> shrinkFactor;
       }
       while (shrinkFactor<=0.0 || 1.0<shrinkFactor);

       viewWhatsInFile( vtkFilename, shrinkFactor );
   }
   else    // Process multiple vtk files and view in a simulated C6...
   {
       //Create one-time graphics stuff
       vtkRenderer* ren1 = vtkRenderer::New();
       ren1->TwoSidedLightingOn();
       vtkRenderWindow* renWin = vtkRenderWindow::New();
           renWin->AddRenderer( ren1 );
       vtkRenderWindowInteractor* iren = vtkRenderWindowInteractor::New();
           iren->SetRenderWindow( renWin );

       vtkFilename = new char [100];

       int numActors = argc - 1;
       vtkActor ** actor = new vtkActor* [numActors];
       for ( i=0; i<numActors; i++ )
       {
           strcpy( vtkFilename, argv[i+1]);
           actor[i] = getActorFromFile( vtkFilename );
           if (actor[i] == NULL) continue;

           //Add the actors to the renderer
           ren1->AddActor( actor[i] );
       }

       vtkActor * axesActor = NULL;
       vtkFollower * xActor = NULL;
       vtkFollower * yActor = NULL;
       vtkFollower * zActor = NULL;

       // Create the cube corners and the associated mapper and actor.
       vtkCubeSource * C6 = vtkCubeSource::New();
           C6->SetBounds( -5, 5, -5, 5, 0, 10 );
       vtkOutlineCornerFilter * C6CornerFilter = vtkOutlineCornerFilter::New();
           C6CornerFilter->SetInput( C6->GetOutput() );
       vtkPolyDataMapper * C6Mapper = vtkPolyDataMapper::New();
           C6Mapper->SetInput( C6CornerFilter->GetOutput() );
       vtkActor * C6Actor = vtkActor::New();
           C6Actor->SetMapper( C6Mapper );

       // intermediate cleanup
       C6->Delete();
       C6CornerFilter->Delete();
       C6Mapper->Delete();
       
       // Create the axes symbol actor.
       axesActor = vtkActor::New();
       GetAxesSymbol( axesActor );

       // Create the axes labels
       xActor = vtkFollower::New();
       yActor = vtkFollower::New();
       zActor = vtkFollower::New();
       GetAxesLabels( xActor, yActor, zActor );
       
       // Add the actors to the renderer.
       ren1->AddActor( C6Actor );
       ren1->AddActor( axesActor );
       ren1->AddActor( xActor );
       ren1->AddActor( yActor );
       ren1->AddActor( zActor );

       // Force the axis labels to always face camera
       xActor->SetCamera( ren1->GetActiveCamera() );
       yActor->SetCamera( ren1->GetActiveCamera() );
       zActor->SetCamera( ren1->GetActiveCamera() );

       // Set the position of the camera in world coordinates.
       // The default position is (0,0,1).
       ren1->GetActiveCamera()->SetPosition( 0, -10, 5 );

       //Set the view up direction for the camera. The default is (0,1,0)
       ren1->GetActiveCamera()->SetViewUp( 0, 0, 1 );

       ren1->GetActiveCamera()->Zoom( 0.3 );

       //ren1->SetViewport(0,0,1,1);
       //ren1->SetBackground( 1, 1, 1 );     // white window conflicts with white text and white C6 corners

/*
       vtkCamera *cam1 = ren1->GetActiveCamera();
           //cam1->Zoom( 1.5 );
           cam1->Zoom( );
           cam1->SetParallelProjection(1);    // no perspective
           //cam1->ParallelProjectionOn();    // no perspective
*/

       // Reset the clipping range of the camera
       ren1->ResetCameraClippingRange();

       cout << "\nWith cursor on the graphics window, press 'e' to exit the viewer" << endl;

       // interact with data
       renWin->SetSize( 800, 800 );
       renWin->Render();
       iren->Start();

       // delete all the instances that have been created.
       for (i=0; i<numActors; i++)
       {
           if (actor[i] != NULL) actor[i]->Delete();
       }
       delete [] actor; actor = NULL;

       axesActor->Delete();
       xActor->Delete();
       yActor->Delete();
       zActor->Delete();
       C6Actor->Delete();

       ren1->Delete();
       renWin->Delete();
       iren->Delete();
   }

   delete [] vtkFilename;

	return 0;
}

void viewWhatsInFile( char * vtkFilename, const float shrinkFactor )
{
   //cout << "viewWhatsInFile" << endl;
   //
   // "1" means print info to screen
   vtkDataSet * dataset = readVtkThing( vtkFilename, 1 );

   if ( dataset->GetDataObjectType() == VTK_UNSTRUCTURED_GRID )
   {
      // let the user pick the active scalar
      //activateScalar( unsReader->GetOutput() ); 
      activateScalar( dataset ); 

      //showGraphics: 0=noShowCells, 1=showExteriorCells, 2=showAllCells
      int showGraphics;
      cout << "\nPick an option for displaying cells..." << endl;
      cout << "Answer  (0) noDrawCells   " 
      << "(1) drawExteriorCellsOnly (faster)   (2) drawAllCells" << endl;
      cin >> showGraphics;

      if (showGraphics)
      {
         if (showGraphics == 1) 
         {
            vtkUnstructuredGrid *cTemp = extractExteriorCellsOnly( 
                                               (vtkUnstructuredGrid*)dataset );
            viewCells( cTemp, shrinkFactor );
            cTemp->Delete();
         }
         else 
         {
            // if multiple point data scalar arrays are present,
            // allow user to change active scalar
            do
            {
               viewCells( dataset, shrinkFactor );
               activateScalar( dataset ); 
            }
            while ( 1 );
         }
      }
   }
   else if ( dataset->GetDataObjectType() == VTK_RECTILINEAR_GRID )
   {
      int showGraphics;
      cout << "\nPick an option for displaying cells "
           << "(0=3D mesh, 1=cross-section): " << endl;
      cin >> showGraphics;
      if (showGraphics == 0) viewCells( dataset, shrinkFactor );
      else
      {
         cout << " calling viewXSectionOfRectilinearGrid" << endl;
         viewXSectionOfRectilinearGrid( (vtkRectilinearGrid*)dataset );
      }
   }
   else if ( dataset->GetDataObjectType() == VTK_POLY_DATA )
   {
      cout <<"sjk 1" << endl;
      // let the user pick the active scalar
      activateScalar( dataset ); 
      viewCells( dataset, shrinkFactor );
   }
   else if ( dataset->GetDataObjectType() == VTK_STRUCTURED_GRID )
   {
      cout <<"IsFileStructuredGrid" << endl;
      viewCells( dataset, shrinkFactor );
   }
   else
   {
      cout <<"ERROR - Unsupported vtk file format" << endl;
      exit(1);
   }
   dataset->Delete();

   return;
}

vtkActor * getActorFromDataSet( vtkDataSet * dataset )
{   
   vtkActor *actor  = NULL;
   int numCells = dataset->GetNumberOfCells();
   if ( numCells==0 )
   {
      cout << "\tNothing to plot: The number of cells is " << numCells << endl;
      return actor;
   }
   else
   {
      cout << "\tgetActorFromDataSet: The number of cells is " << numCells << endl;
   }

   vtkDataSetMapper *map = vtkDataSetMapper::New();
   // By default, VTK uses OpenGL display lists which results in another copy
   // of the data being stored in memory. For most large datasets you will be
   // better off saving memory by not using display lists.  You can turn off
   // display lists by turning on ImmediateModeRendering.
   map->ImmediateModeRenderingOn();
   map->SetInput( dataset );

   actor = vtkActor::New();

   cout << "dataset->GetPointData()->GetNumberOfArrays() = "
        << dataset->GetPointData()->GetNumberOfArrays() << endl;

   if ( dataset->GetPointData()->GetScalars() )
   {
      vtkLookupTable *lut = vtkLookupTable::New();
      lut->SetNumberOfColors(256); //default is 256

      cout << "dataset->GetPointData()->GetScalars()->GetName() = "
           << dataset->GetPointData()->GetScalars()->GetName() << endl;
      cout << "dataset->GetPointData()->GetScalars()->GetLookupTable() = "
           << dataset->GetPointData()->GetScalars()->GetLookupTable() << endl;

      double minMax[ 2 ];
      minMax[ 0 ] = 0.0;
      minMax[ 1 ] = 1.0;

      if ( dataset->GetPointData()->GetScalars()->GetLookupTable() )
      {
         cout << "lookup table is added in getActorFromDataSet" << endl;
      }
      else
      {
         lut->SetHueRange(2.0f/3.0f, 0.0f); //a blue-to-red scale
         dataset->GetPointData()->GetScalars()->GetRange( minMax );
         cout << "minMax: " << minMax[0] << " " << minMax[1] << endl;
         lut->SetTableRange( minMax );
         lut->Build();
/*
         // create a white-to-clear lookup table...
         //for ( int i=0; i<256; i++ ) lut->SetTableValue( i, 1.0, 1.0, 1.0, float(255-i)/255.0 );
         for ( int i=0; i<256; i++ ) lut->SetTableValue( i, 1.0, 1.0, 1.0, cos(0.5*3.1415926*float(i)/255.0) );
*/
/*
         //debugging
         dataset->GetPointData()->GetScalars()->SetLookupTable( lut );
         writeVtkThing( dataset, "junk.vtk", 0 );
*/
      }
      map->SetScalarRange( minMax );
      map->SetLookupTable(lut);
      lut->Delete();
   }
   else
   {
      actor->GetProperty()->SetColor( 1, 0, 0 );
   }

   actor->SetMapper( map );
   map->Delete();

   actor->GetProperty()->SetOpacity( 0.5 );

   return actor;
}


vtkActor * getActorFromFile( char * vtkFilename )
{
   //cout << "getActorFromFile" << endl;
   vtkActor *actor = NULL;

   char * extension = fileIO::getExtension( vtkFilename );
   //cout << "vtkFilename = \"" << vtkFilename << "\"" << endl;
   //cout << "extension = \"" << extension << "\"" << endl;

   if ( !strcmp(extension,"bmp") || !strcmp(extension,"BMP") )
   {
      delete [] extension;
      cfdImage *image = new cfdImage( vtkFilename, 0 );
      return image->GetActor();
   }
   else if ( !strcmp(extension,"stl") || !strcmp(extension,"STL") )
   {
      vtkSTLReader * reader = vtkSTLReader::New();
      //reader->DebugOn();
      reader->SetFileName( vtkFilename );
      reader->Update();
      actor = getActorFromDataSet( reader->GetOutput() );
      reader->Delete();
      delete [] extension;
      return actor;
   }
   else
      delete [] extension;

   vtkDataSet * dataset = readVtkThing( vtkFilename, 1 ); // "1" means print info to screen

   if ( dataset->GetDataObjectType() == VTK_UNSTRUCTURED_GRID )
   {
      cout << "IsFileUnstructuredGrid" << endl;

      // let the user pick the active scalar
      activateScalar( dataset ); 

      //showGraphics: 0=noShowCells, 1=showExteriorCells, 2=showAllCells
      int showGraphics;
      cout << "\nPick an option for displaying cells..." << endl;
      //cout << "Answer  (0) noDrawCells   (1) drawExteriorFacesOnly (for faster interaction)   (2) drawAllCells" << endl;
      cout << "Answer  (1) drawExteriorFacesOnly (for faster interaction)   (2) drawAllCells" << endl;
      cin >> showGraphics;

      //if (showGraphics)
      {
         if (showGraphics == 1) 
         {
            vtkPolyData * cTemp = cfdGrid2Surface( dataset, 0 );
            actor = getActorFromDataSet( cTemp );
            cTemp->Delete();
         }
         else 
         {
            // if multiple point data scalar arrays are present,
            // allow user to change active scalar
            //do
            {
               actor = getActorFromDataSet( dataset );
               //activateScalar( dataset ); 
            }
            //while ( 1 );
         }
      }
   }
   else if ( dataset->GetDataObjectType() == VTK_RECTILINEAR_GRID )
   {
      cout <<"IsFileRectilinearGrid" << endl;
      actor = getActorFromDataSet( dataset );
   }
   else if ( dataset->GetDataObjectType() == VTK_POLY_DATA )
   {
      cout <<"IsFilePolyData" << endl;
      actor = getActorFromDataSet( dataset );
   }
   else if ( dataset->GetDataObjectType() == VTK_STRUCTURED_GRID )
   {
      cout <<"IsFileStructuredGrid" << endl;
      actor = getActorFromDataSet( dataset );
   }
   else
   {
      cout <<"ERROR - Unsupported vtk file format" << endl;
   }
   dataset->Delete();
   return actor;
}
