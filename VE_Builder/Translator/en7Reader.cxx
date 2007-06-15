/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2007 by Iowa State University
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
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include <iostream>
#include <cmath>
#include <sstream>
#include <iomanip>

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

#include "VE_Xplorer/Utilities/fileIO.h"
#include "VE_Xplorer/Utilities/readWriteVtkThings.h"
#include "VE_Xplorer/Utilities/cleanVtk.h"
#include "VE_Xplorer/Utilities/cfdGrid2Surface.h"
using namespace VE_Util;

void writeVtkGeomToStl( vtkDataSet * dataset, char filename [] )
{
   vtkTriangleFilter *tFilter = vtkTriangleFilter::New();
   vtkGeometryFilter *gFilter = NULL;

   // convert dataset to vtkPolyData 
   if ( dataset->IsA("vtkPolyData") )
      tFilter->SetInput( (vtkPolyData*)dataset );
   else 
   {
      std::cout << "Using vtkGeometryFilter to convert to polydata" << std::endl;
      gFilter = vtkGeometryFilter::New();
      gFilter->SetInput( dataset );
      tFilter->SetInput( gFilter->GetOutput() );
   }

   std::cout << "Writing \"" << filename << "\"... ";
   std::cout.flush();
   vtkSTLWriter *writer = vtkSTLWriter::New();
      writer->SetInput( tFilter->GetOutput() );
      writer->SetFileName( filename );
      writer->SetFileTypeToBinary();
      writer->Write();
      writer->Delete();
   std::cout << "... done\n" << std::endl;

   tFilter->Delete();
   if ( gFilter ) gFilter->Delete();
}


vtkUnstructuredGrid * en7Reader( std::string caseFileName, vtkTransform * transform, int number, int debug )
{
   vtkUnstructuredGrid * uGrid = NULL;

   std::cout << "caseFileName = \"" << caseFileName << "\"" << std::endl;

   std::string extension = fileIO::getExtension( caseFileName );
   //std::cout << "extension = \"" << extension << "\"" << std::endl;
   //if ( strcmp(extension,"case") && strcmp(extension,"CASE") && 
   //     strcmp(extension,"cas") && strcmp(extension,"encas") )
   if ( extension.compare("case") && extension.compare("CASE") &&
        extension.compare("cas") && extension.compare("encas") )
   {
      std::cout << "\nERROR: filename extension must be \"case\" or \"CASE\" or \"cas\" or \"encas\"\n" << std::endl;
      //delete [] extension;
      return uGrid;
   }
   //delete [] extension;

   int i;

   vtkEnSight6Reader * reader = vtkEnSight6Reader::New();
   //vtkGenericEnSightReader * reader = vtkGenericEnSightReader::New();
   //vtkEnSightFortranBinaryReader * reader = vtkEnSightFortranBinaryReader::New();
      //reader->DebugOn();
      reader->SetCaseFileName( caseFileName.c_str() );
      reader->Update();

   int numOutputs = reader->GetNumberOfOutputs();
   std::cout << "numOutputs = " << numOutputs << std::endl;
   if ( numOutputs == 0 )
   {
      reader->Delete();
      return uGrid;
   }

   // Transformation 
   vtkTransformFilter *transFilter = vtkTransformFilter::New();
       transFilter->SetTransform( transform );

   std::cout << "reader->GetOutput() = " << reader->GetOutput() << std::endl;
   if ( reader->GetOutput() ) 
      std::cout << "reader->GetOutput()->GetNumberOfPoints() = " 
           << reader->GetOutput()->GetNumberOfPoints() << std::endl;

   std::cout << "reader->GetNumberOfScalarsPerNode() = " << reader->GetNumberOfScalarsPerNode() << std::endl;

   //VTK_SCALAR_PER_NODE was defined in vtk4.0, changed to SCALAR_PER_NODE in VTK4.3
   for ( i=0; i<reader->GetNumberOfScalarsPerNode(); i++ )
      std::cout << "Description " << i << " = \'" 
           << reader->GetDescription( i, vtkEnSightReader::SCALAR_PER_NODE ) << "\'" << std::endl;

   float minTime = reader->GetMinimumTimeValue();
   float maxTime = reader->GetMaximumTimeValue();
   std::cout << "minTime = " << minTime << std::endl;
   std::cout << "maxTime = " << maxTime << std::endl;

   // See if we are dealing with transient data
   // depending on the setTimeValue, the data (scalar and vector) will be different
   if ( minTime != maxTime )  // transient
   {
      vtkDataArrayCollection * timeSet = reader->GetTimeSets();
      int timeStep = 0;
      //char* currentFilename;
      double currentTime[1];
      double nextTime[1];

      vtkLookupTable *lut = vtkLookupTable::New();
      lut->SetNumberOfColors(256); //default is 256
      for ( int i=0; i<256; i++ ) 
         lut->SetTableValue( i, 1.0, 1.0, 1.0, cos(0.5*3.1415926*float(i)/255.0) );

      int sampleFrequency = 2; //get every second time step
      do 
      {
         //timeSet->Print(std::cout);
         //timeSet->GetItem(0)->Print(std::cout);
         //std::cout << "timeStep = " << timeStep << std::endl;
         timeSet->GetItem(0)->GetTuple( timeStep*sampleFrequency, currentTime );
         std::cout << "\ncurrentTime = " << currentTime[0] << std::endl;
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

         std::ostringstream dirStringStream;
         dirStringStream << "deice_" << std::setw(3) << timeStep << ".vtk";
         std::cout << "currentFilename = \"" << dirStringStream.str() << "\"" << std::endl;

         writeVtkThing( transFilter->GetOutput(), (std::string)dirStringStream.str().c_str(), 1 ); //0=ascii
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

      std::cout << "\tuGrid->GetPointData()->GetNumberOfArrays() = " 
           << uGrid->GetPointData()->GetNumberOfArrays() << std::endl;
      for ( i=0; i<uGrid->GetPointData()->GetNumberOfArrays(); i++ )
         std::cout << "\tuGrid->GetPointData()->GetArray(i)->GetName() = "
              << uGrid->GetPointData()->GetArray(i)->GetName() << std::endl;
      std::cout << "\tuGrid->GetPointData()->GetArray(x-velocity)->GetNumberOfTuples() = "
           << uGrid->GetPointData()->GetArray("x-velocity")->GetNumberOfTuples() << std::endl;

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

std::cout << "a" << std::endl;
      transFilter->Delete();
std::cout << "b" << std::endl;
      //reader->Delete();  //can't do this: i don't know why
std::cout << "c" << std::endl;
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
      //std::cout << "reader->GetOutput(" << i << ") = " << reader->GetOutput( i ) << std::endl;
      if ( reader->GetOutput(i) == NULL ) continue;

      blockMapper[i] = vtkDataSetMapper::New();
      blockMapper[i]->SetInput( reader->GetOutput( i ) );
      blockMapper[i]->SetLookupTable( lut );
      reader->GetOutput( i )->GetScalarRange( range );
      std::cout << "part " << i << " range:\t" << range[0] << "\t" << range[1] << std::endl;
      if ( globalRange[0] > range[0] ) globalRange[0] = range[0];
      if ( globalRange[1] < range[1] ) globalRange[1] = range[1];

      blockActor[i] = vtkActor::New();
      blockActor[i]->SetMapper( blockMapper[i] );
      ren1->AddActor( blockActor[i] );
   }
   std::cout << "globalRange: \t" << globalRange[0] << "\t" << globalRange[1] << std::endl;

   lut->SetHueRange( 0.667, 0.0 );
   lut->SetTableRange( globalRange );
   for ( i=0; i<numOutputs; i++ )
   {
      if ( reader->GetOutput(i) == NULL ) 
         continue;
      blockMapper[i]->SetScalarRange( globalRange );
   }
  
   std::cout << "\nWith cursor on graphics window, type \'e\' to exit\n" << std::endl;

   // interact with data
   renWin->SetSize( 800, 800 );
   renWin->Render();
   iren->Start();

   reader->Delete();
   lut->Delete();
   for ( i=0; i<numOutputs; i++ )
   {
      //std::cout << "blockMapper[" << i << "] = " << blockMapper[i] << std::endl;
      if ( blockMapper[i] ) blockMapper[i]->Delete();
      //std::cout << "blockActor[" << i << "] = " << blockActor[i] << std::endl;
      if ( blockActor[i] ) blockActor[i]->Delete();
   }
   ren1->Delete();
   renWin->Delete();
   iren->Delete();

   return uGrid;
}

