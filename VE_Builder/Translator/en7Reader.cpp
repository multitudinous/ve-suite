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
 * File:          $RCSfile: en7Reader.cpp,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include <iostream>
#include <cstdio>
#include <cmath>

#include <vtkActor.h>
#include <vtkAppendFilter.h>
#include <vtkDataArray.h>
#include <vtkDataArrayCollection.h>
#include <vtkDataSetMapper.h>
#include <vtkEnSight6Reader.h>                  // for gm deice model
//#include "vtkEnSightReader.h"
//#include "vtkEnSightFortranBinaryReader.h"    // for gm engineSpray model
//#include "vtkGenericEnSightReader.h"          // will open any ensight file
#include <vtkFloatArray.h>
#include <vtkGeometryFilter.h>
#include <vtkIdList.h>
#include <vtkLookupTable.h>
#include <vtkPointData.h>
#include <vtkPolyData.h>
#include <vtkRenderWindow.h>
#include <vtkRenderWindowInteractor.h>
#include <vtkRenderer.h>
#include <vtkSTLWriter.h>
#include <vtkTransform.h>
#include <vtkTransformFilter.h>
#include <vtkTriangleFilter.h>
#include <vtkUnstructuredGrid.h>

#include "fileIO.h"
#include "readWriteVtkThings.h"
#include "cleanVtk.h"
#include "cfdGrid2Surface.h"

void writeVtkGeomToStl( vtkDataSet * dataset, char filename [] )
{
   vtkTriangleFilter *tFilter = vtkTriangleFilter::New();
   vtkGeometryFilter *gFilter = NULL;

   // convert dataset to vtkPolyData 
   if ( dataset->IsA("vtkPolyData") )
      tFilter->SetInput( (vtkPolyData*)dataset );
   else 
   {
      cout << "Using vtkGeometryFilter to convert to polydata" << endl;
      gFilter = vtkGeometryFilter::New();
      gFilter->SetInput( dataset );
      tFilter->SetInput( gFilter->GetOutput() );
   }

   cout << "Writing \"" << filename << "\"... ";
   cout.flush();
   vtkSTLWriter *writer = vtkSTLWriter::New();
      writer->SetInput( tFilter->GetOutput() );
      writer->SetFileName( filename );
      writer->SetFileTypeToBinary();
      writer->Write();
      writer->Delete();
   cout << "... done\n" << endl;

   tFilter->Delete();
   if ( gFilter ) gFilter->Delete();
}


vtkUnstructuredGrid * en7Reader( char * caseFileName, vtkTransform * transform, int number, int debug )
{
   vtkUnstructuredGrid * uGrid = NULL;

   cout << "caseFileName = \"" << caseFileName << "\"" << endl;

   char * extension = fileIO::getExtension( caseFileName );
   //cout << "extension = \"" << extension << "\"" << endl;
   if ( strcmp(extension,"case") && strcmp(extension,"CASE") && 
        strcmp(extension,"cas") && strcmp(extension,"encas") )
   {
      cout << "\nERROR: filename extension must be \"case\" or \"CASE\" or \"cas\" or \"encas\"\n" << endl;
      delete [] extension;
      return uGrid;
   }
   delete [] extension;

   int i;

   vtkEnSight6Reader * reader = vtkEnSight6Reader::New();
   //vtkGenericEnSightReader * reader = vtkGenericEnSightReader::New();
   //vtkEnSightFortranBinaryReader * reader = vtkEnSightFortranBinaryReader::New();
      //reader->DebugOn();
      reader->SetCaseFileName( caseFileName );
      reader->Update();

   int numOutputs = reader->GetNumberOfOutputs();
   cout << "numOutputs = " << numOutputs << endl;
   if ( numOutputs == 0 )
   {
      reader->Delete();
      return uGrid;
   }

   // Transformation 
   vtkTransformFilter *transFilter = vtkTransformFilter::New();
       transFilter->SetTransform( transform );

   cout << "reader->GetOutput() = " << reader->GetOutput() << endl;
   if ( reader->GetOutput() ) 
      cout << "reader->GetOutput()->GetNumberOfPoints() = " 
           << reader->GetOutput()->GetNumberOfPoints() << endl;

   cout << "reader->GetNumberOfScalarsPerNode() = " << reader->GetNumberOfScalarsPerNode() << endl;

   //VTK_SCALAR_PER_NODE was defined in vtk4.0, changed to SCALAR_PER_NODE in VTK4.3
   for ( i=0; i<reader->GetNumberOfScalarsPerNode(); i++ )
      cout << "Description " << i << " = \'" 
           << reader->GetDescription( i, vtkEnSightReader::SCALAR_PER_NODE ) << "\'" << endl;

   float minTime = reader->GetMinimumTimeValue();
   float maxTime = reader->GetMaximumTimeValue();
   cout << "minTime = " << minTime << endl;
   cout << "maxTime = " << maxTime << endl;

   // See if we are dealing with transient data
   // depending on the setTimeValue, the data (scalar and vector) will be different
   if ( minTime != maxTime )  // transient
   {
      vtkDataArrayCollection * timeSet = reader->GetTimeSets();
      int timeStep = 0;
      char currentFilename [100];
      double currentTime[1];
      double nextTime[1];

      vtkLookupTable *lut = vtkLookupTable::New();
      lut->SetNumberOfColors(256); //default is 256
      for ( int i=0; i<256; i++ ) 
         lut->SetTableValue( i, 1.0, 1.0, 1.0, cos(0.5*3.1415926*float(i)/255.0) );

      int sampleFrequency = 2; //get every second time step
      do 
      {
         //timeSet->Print(cout);
         //timeSet->GetItem(0)->Print(cout);
         //cout << "timeStep = " << timeStep << endl;
         timeSet->GetItem(0)->GetTuple( timeStep*sampleFrequency, currentTime );
         cout << "\ncurrentTime = " << currentTime[0] << endl;
         reader->SetTimeValue( currentTime[0] );
         reader->Update();

         uGrid = vtkUnstructuredGrid::New();
         //ice-cells was part 1 in Deice_ensight_ice-cells.geo referenced by Deice_transient_shortGeo.encas
         uGrid->DeepCopy( reader->GetOutput( 0 ) );
         //uGrid->DeepCopy( reader->GetOutput( 16 ) );   // 16 refers to part 17: ice cells
         dumpVerticesNotUsedByCells( uGrid );

         vtkPolyData * surface = cfdGrid2Surface( uGrid, 0 );
         surface->GetPointData()->GetScalars()->SetLookupTable( lut );
        
         transFilter->SetInput( surface );
         transFilter->Update();

         sprintf (currentFilename, "%s%03i%s", "deice_", timeStep, ".vtk");
         cout << "currentFilename = \"" << currentFilename << "\"" << endl;

         writeVtkThing( transFilter->GetOutput(), currentFilename, 1 ); //0=ascii
         surface->Delete();
         uGrid->Delete();
         uGrid = NULL;
         timeStep++;

         // check time to see if another loop is needed...
         timeSet->GetItem(0)->GetTuple( timeStep*sampleFrequency, nextTime );
      }
      //while ( 0 ); // to test only one loop iteartion
      while ( (currentTime[0] < nextTime[0]) && (nextTime[0] < maxTime) );
      transFilter->Delete();
      reader->Delete();
      lut->Delete();
      return uGrid;
   }
   else
   {
      //int i;
      uGrid = vtkUnstructuredGrid::New();
         uGrid->DeepCopy( reader->GetOutput( 0 ) );   // 0 refers to part 1: fluid-main
      dumpVerticesNotUsedByCells( uGrid );

      cout << "\tuGrid->GetPointData()->GetNumberOfArrays() = " 
           << uGrid->GetPointData()->GetNumberOfArrays() << endl;
      for ( i=0; i<uGrid->GetPointData()->GetNumberOfArrays(); i++ )
         cout << "\tuGrid->GetPointData()->GetArray(i)->GetName() = "
              << uGrid->GetPointData()->GetArray(i)->GetName() << endl;
      cout << "\tuGrid->GetPointData()->GetArray(x-velocity)->GetNumberOfTuples() = "
           << uGrid->GetPointData()->GetArray("x-velocity")->GetNumberOfTuples() << endl;

      vtkFloatArray* vel_mag = vtkFloatArray::New();
      vel_mag->SetNumberOfTuples( uGrid->GetPointData()->GetArray("x-velocity")->GetNumberOfTuples() );
      vel_mag->SetNumberOfComponents( 1 );
      vel_mag->SetName( "velocity_mag" );

      double tuple[1];
      double velocityMag[1];
      for ( i=0; i<uGrid->GetPointData()->GetArray("x-velocity")->GetNumberOfTuples(); i++ )
      {
         velocityMag[0] = 0.0;
         uGrid->GetPointData()->GetArray("x-velocity")->GetTuple( i, tuple );
         velocityMag[0] += tuple[0]*tuple[0];
         uGrid->GetPointData()->GetArray("y-velocity")->GetTuple( i, tuple );
         velocityMag[0] += tuple[0]*tuple[0];
         uGrid->GetPointData()->GetArray("z-velocity")->GetTuple( i, tuple );
         velocityMag[0] += tuple[0]*tuple[0];
         velocityMag[0] = sqrt( velocityMag[0] );
         vel_mag->SetTuple( i, velocityMag );
      }
      uGrid->GetPointData()->RemoveArray("x-velocity");
      uGrid->GetPointData()->RemoveArray("y-velocity");
      uGrid->GetPointData()->RemoveArray("z-velocity");
      uGrid->GetPointData()->SetScalars( vel_mag );
      vel_mag->Delete();
      uGrid->Update();

      transFilter->SetInput( uGrid );
         transFilter->Update();
      writeVtkThing( transFilter->GetOutput(), "flowdata.vtk", 1 );   //0=ascii
      uGrid->Delete();
      uGrid = NULL;

      vtkUnstructuredGrid ** append = new vtkUnstructuredGrid * [5];
      vtkUnstructuredGrid ** trans_append = new vtkUnstructuredGrid * [5];

      append[0] = (vtkUnstructuredGrid *)reader->GetOutput( 26 );
      append[1] = (vtkUnstructuredGrid *)reader->GetOutput( 29 );
      append[2] = (vtkUnstructuredGrid *)reader->GetOutput( 62 );
      append[3] = (vtkUnstructuredGrid *)reader->GetOutput( 63 );
      append[4] = (vtkUnstructuredGrid *)reader->GetOutput( 64 );

      // now process several geometrical entities into one stl file
      vtkAppendFilter * appendUGrids = vtkAppendFilter::New();

      for ( i=0; i<5; i++ )
      {
         transFilter->SetInput( append[i] );
         transFilter->Update();
         trans_append[i] = vtkUnstructuredGrid::New();
         trans_append[i]->DeepCopy( transFilter->GetOutput() );
         trans_append[i]->Update();
//if ( i == 4) writeVtkThing( trans_append[i], "part64.vtk", 0 );   //0=ascii
         appendUGrids->AddInput( trans_append[i] );
      }
      appendUGrids->Update();

      vtkUnstructuredGrid * junk = vtkUnstructuredGrid::New();
      junk->DeepCopy( appendUGrids->GetOutput() );
      junk->Update();
      dumpVerticesNotUsedByCells( junk );
      junk->Update();

//writeVtkThing( junk, "junk.vtk", 0 );   //0=ascii
      writeVtkGeomToStl( junk, "truck_body.stl" );
      appendUGrids->Delete();

      for ( i=0; i<5; i++ )
         trans_append[i]->Delete();
      delete [] trans_append; trans_append= NULL;
      delete [] append; append = NULL;

cout << "a" << endl;
      transFilter->Delete();
cout << "b" << endl;
      //reader->Delete();  //can't do this: i don't know why
cout << "c" << endl;
      return uGrid;
   }

   vtkRenderer * ren1 = vtkRenderer::New();
   vtkRenderWindow * renWin = vtkRenderWindow::New();
   renWin->AddRenderer( ren1 );
   vtkRenderWindowInteractor* iren = vtkRenderWindowInteractor::New();
   iren->SetRenderWindow( renWin );

   vtkDataSetMapper ** blockMapper = new vtkDataSetMapper * [numOutputs];
   vtkActor ** blockActor = new vtkActor * [numOutputs];

   double range[2];
   double globalRange[2];
   globalRange[0] = 1e12;
   globalRange[1] = -1e12;
   vtkLookupTable * lut = vtkLookupTable::New();

   for ( i=0; i<numOutputs; i++ )
   {
      blockMapper[i] = NULL;
      blockActor[i] = NULL;
      //cout << "reader->GetOutput(" << i << ") = " << reader->GetOutput( i ) << endl;
      if ( reader->GetOutput(i) == NULL ) continue;

      blockMapper[i] = vtkDataSetMapper::New();
      blockMapper[i]->SetInput( reader->GetOutput( i ) );
      blockMapper[i]->SetLookupTable( lut );
      reader->GetOutput( i )->GetScalarRange( range );
      cout << "part " << i << " range:\t" << range[0] << "\t" << range[1] << endl;
      if ( globalRange[0] > range[0] ) globalRange[0] = range[0];
      if ( globalRange[1] < range[1] ) globalRange[1] = range[1];

      blockActor[i] = vtkActor::New();
      blockActor[i]->SetMapper( blockMapper[i] );
      ren1->AddActor( blockActor[i] );
   }
   cout << "globalRange: \t" << globalRange[0] << "\t" << globalRange[1] << endl;

   lut->SetHueRange( 0.667, 0.0 );
   lut->SetTableRange( globalRange );
   for ( i=0; i<numOutputs; i++ )
   {
      if ( reader->GetOutput(i) == NULL ) 
         continue;
      blockMapper[i]->SetScalarRange( globalRange );
   }
  
   cout << "\nWith cursor on graphics window, type \'e\' to exit\n" << endl;

   // interact with data
   renWin->SetSize( 800, 800 );
   renWin->Render();
   iren->Start();

   reader->Delete();
   lut->Delete();
   for ( i=0; i<numOutputs; i++ )
   {
      //cout << "blockMapper[" << i << "] = " << blockMapper[i] << endl;
      if ( blockMapper[i] ) blockMapper[i]->Delete();
      //cout << "blockActor[" << i << "] = " << blockActor[i] << endl;
      if ( blockActor[i] ) blockActor[i]->Delete();
   }
   ren1->Delete();
   renWin->Delete();
   iren->Delete();

   return uGrid;
}

