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
#include <cstdio>

#include <vtkActor.h>
#include <vtkAppendFilter.h>
#include <vtkDataArray.h>
#include <vtkDataArrayCollection.h>
#include <vtkDataSetMapper.h>
#include <vtkEnSightReader.h>
//#include "vtkEnSight6Reader.h"      // for gm deice model
#include <VE_Builder/Translator/vtkEnSightFortranBinaryReader.h>      // for gm engineSpray model
//#include "vtkGenericEnSightReader.h"      // will open any ensight file
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

#include "VE_Xplorer/fileIO.h"
#include "VE_Xplorer/readWriteVtkThings.h"
#include "VE_Builder/Translator/cleanVtk.h"
#include "VE_Builder/Translator/cfdGrid2Surface.h"
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
   std::cout << "... done\n" <<std::endl;

   tFilter->Delete();
   if ( gFilter ) gFilter->Delete();
}


vtkUnstructuredGrid * en7Reader( std::string caseFileName, vtkTransform * transform, int number, int debug )
{
   vtkUnstructuredGrid * uGrid = NULL;

  std::cout << "caseFileName = \"" << caseFileName << "\"" <<std::endl;

   std::string extension = fileIO::getExtension( caseFileName );
   //cout << "extension = \"" << extension << "\"" <<std::endl;
   if ( strcmp(extension,"case") && strcmp(extension,"CASE") && 
        strcmp(extension,"cas") && strcmp(extension,"encas") )
   {
     std::cout << "\nERROR: filename extension must be \"case\" or \"CASE\" or \"cas\" or \"encas\"\n" <<std::endl;
      delete [] extension;
      return uGrid;
   }
   //delete [] extension;

   int i;

   //vtkEnSight6Reader * reader = vtkEnSight6Reader::New();
   //vtkGenericEnSightReader * reader = vtkGenericEnSightReader::New();
   vtkEnSightFortranBinaryReader * reader = vtkEnSightFortranBinaryReader::New();

//reader->Delete();  //OK
//return uGrid;

      reader->DebugOn();
      reader->SetCaseFileName( caseFileName );
      reader->Update();

//reader->Delete();  //can't do this: i don't know why
//return uGrid;

   int numOutputs = reader->GetNumberOfOutputs();
  std::cout << "numOutputs = " << numOutputs <<std::endl;
   if ( numOutputs == 0 )
   {
      reader->Delete();
      return uGrid;
   }

   // Transformation 
   vtkTransformFilter *transFilter = vtkTransformFilter::New();
       transFilter->SetTransform( transform );

  std::cout << "reader->GetOutput() = " << reader->GetOutput() <<std::endl;
   if ( reader->GetOutput() ) 
     std::cout << "reader->GetOutput()->GetNumberOfPoints() = " 
           << reader->GetOutput()->GetNumberOfPoints() <<std::endl;

  std::cout << "reader->GetNumberOfScalarsPerNode() = " 
        << reader->GetNumberOfScalarsPerNode() <<std::endl;

   //VTK_SCALAR_PER_NODE was defined in vtk4.0, changed to SCALAR_PER_NODE in VTK4.3
   for ( i=0; i<reader->GetNumberOfScalarsPerNode(); i++ )
     std::cout << "Description " << i << " = \'" 
           << reader->GetDescription( i, vtkEnSightReader::SCALAR_PER_NODE ) << "\'" <<std::endl;

  std::cout << "reader->GetNumberOfScalarsPerMeasuredNode() = " 
        << reader->GetNumberOfScalarsPerMeasuredNode() <<std::endl;

   for ( i=0; i<reader->GetNumberOfScalarsPerMeasuredNode(); i++ )
     std::cout << "Description " << i << " = \'" 
           << reader->GetDescription( i, vtkEnSightReader::SCALAR_PER_MEASURED_NODE ) << "\'" <<std::endl;

   float minTime = reader->GetMinimumTimeValue();
   float maxTime = reader->GetMaximumTimeValue();
  std::cout << "minTime = " << minTime <<std::endl;
  std::cout << "maxTime = " << maxTime <<std::endl;

   // See if we are dealing with transient data
   // depending on the setTimeValue, the data (scalar and vector) will be different
   if ( minTime != maxTime )  // transient
   {
      // not yet implemented -- currently I create multiple case files and process each as a steady state case
   }
   else
   { 
     std::cout << "GetNumberOfOutputs = " << reader->GetNumberOfOutputs() <<std::endl;
      vtkPolyData *pData = vtkPolyData::New();
         pData->DeepCopy( reader->GetOutput() );

     std::cout << "\tpData->GetPointData()->GetNumberOfArrays() = " 
           << pData->GetPointData()->GetNumberOfArrays() <<std::endl;
      for ( int i=0; i<pData->GetPointData()->GetNumberOfArrays(); i++ )
        std::cout << "\tpData->GetPointData()->GetArray(i)->GetName() = "
              << pData->GetPointData()->GetArray(i)->GetName() <<std::endl;

      transFilter->SetInput( pData );
         transFilter->Update();

      char outFile [100];
      if ( number == -1 ) strcpy( outFile, "flowdata.vtk" );
      else                sprintf( outFile, "flowdata_%03i.vtk", number );

      writeVtkThing( transFilter->GetOutput(), outFile, 1 );   //0=ascii
      pData->Delete();
      pData = NULL;
/*
      uGrid = vtkUnstructuredGrid::New();
         uGrid->DeepCopy( reader->GetOutput( 0 ) );   // 0 refers to part 1: fluid-main
      dumpVerticesNotUsedByCells( uGrid );

     std::cout << "\tuGrid->GetPointData()->GetNumberOfArrays() = " 
           << uGrid->GetPointData()->GetNumberOfArrays() <<std::endl;
      for ( int i=0; i<uGrid->GetPointData()->GetNumberOfArrays(); i++ )
        std::cout << "\tuGrid->GetPointData()->GetArray(i)->GetName() = "
              << uGrid->GetPointData()->GetArray(i)->GetName() <<std::endl;

      transFilter->SetInput( uGrid );
         transFilter->Update();

      char outFile [100];
      if ( number == -1 ) strcpy( outFile, "flowdata.vtk" );
      else                sprintf( outFile, "flowdata_%03i.vtk", number );

      writeVtkThing( transFilter->GetOutput(), outFile, 1 );   //0=ascii
      uGrid->Delete();
      uGrid = NULL;

      if ( number == -1 ) strcpy( outFile, "surface.stl" );
      else                sprintf( outFile, "surface_%03i.stl", number );

      vtkPolyData * surface = cfdGrid2Surface( transFilter->GetOutput(), 0 );
      writeVtkGeomToStl( surface, outFile );

      vtkUnstructuredGrid ** append = new vtkUnstructuredGrid * [7];
      vtkUnstructuredGrid ** trans_append = new vtkUnstructuredGrid * [7];

      //vtkparts 5-10 are the cylinder valves, vtkpart 12 is the manifold valve
      append[0] = (vtkUnstructuredGrid *)reader->GetOutput(  5 );
      append[1] = (vtkUnstructuredGrid *)reader->GetOutput(  6 );
      append[2] = (vtkUnstructuredGrid *)reader->GetOutput(  7 );
      append[3] = (vtkUnstructuredGrid *)reader->GetOutput(  8 );
      append[4] = (vtkUnstructuredGrid *)reader->GetOutput(  9 );
      append[5] = (vtkUnstructuredGrid *)reader->GetOutput( 10 );
      append[6] = (vtkUnstructuredGrid *)reader->GetOutput( 12 );

      // now process several geometrical entities into one stl file
      vtkAppendFilter * appendUGrids = vtkAppendFilter::New();

      for ( i=0; i<7; i++ )
      {
         transFilter->SetInput( append[i] );
         transFilter->Update();
         trans_append[i] = vtkUnstructuredGrid::New();
         trans_append[i]->DeepCopy( transFilter->GetOutput() );
         trans_append[i]->Update();
         appendUGrids->AddInput( trans_append[i] );
      }
      appendUGrids->Update();

      vtkUnstructuredGrid * junk = vtkUnstructuredGrid::New();
      junk->DeepCopy( appendUGrids->GetOutput() );
      junk->Update();
      dumpVerticesNotUsedByCells( junk );
      junk->Update();

      if ( number == -1 ) strcpy( outFile, "valves.stl" );
      else                sprintf( outFile, "valves_%03i.stl", number );
      writeVtkGeomToStl( junk, outFile );
      appendUGrids->Delete();

      for ( i=0; i<7; i++ )
         trans_append[i]->Delete();
      delete [] trans_append; trans_append= NULL;
      delete [] append; append = NULL;
*/

      transFilter->Delete();
      //reader->Delete();  //can't do this: i don't know why
      return uGrid;
   }

   vtkRenderer * ren1 = vtkRenderer::New();
   vtkRenderWindow * renWin = vtkRenderWindow::New();
   renWin->AddRenderer( ren1 );
   vtkRenderWindowInteractor* iren = vtkRenderWindowInteractor::New();
   iren->SetRenderWindow( renWin );

   vtkDataSetMapper ** blockMapper = new vtkDataSetMapper * [numOutputs];
   vtkActor ** blockActor = new vtkActor * [numOutputs];

   float range[2];
   float globalRange[2];
   globalRange[0] = 1e12;
   globalRange[1] = -1e12;
   vtkLookupTable * lut = vtkLookupTable::New();

   for ( i=0; i<numOutputs; i++ )
   {
      blockMapper[i] = NULL;
      blockActor[i] = NULL;
      //cout << "reader->GetOutput(" << i << ") = " << reader->GetOutput( i ) <<std::endl;
      if ( reader->GetOutput(i) == NULL ) continue;

      blockMapper[i] = vtkDataSetMapper::New();
      blockMapper[i]->SetInput( reader->GetOutput( i ) );
      blockMapper[i]->SetLookupTable( lut );
      reader->GetOutput( i )->GetScalarRange( range );
     std::cout << "part " << i << " range:\t" << range[0] << "\t" << range[1] <<std::endl;
      if ( globalRange[0] > range[0] ) globalRange[0] = range[0];
      if ( globalRange[1] < range[1] ) globalRange[1] = range[1];

      blockActor[i] = vtkActor::New();
      blockActor[i]->SetMapper( blockMapper[i] );
      ren1->AddActor( blockActor[i] );
   }
  std::cout << "globalRange: \t" << globalRange[0] << "\t" << globalRange[1] <<std::endl;

   lut->SetHueRange( 0.667, 0.0 );
   lut->SetTableRange( globalRange );
   for ( i=0; i<numOutputs; i++ )
   {
      if ( reader->GetOutput(i) == NULL ) continue;
      blockMapper[i]->SetScalarRange( globalRange );
   }
  
  std::cout << "\nWith cursor on graphics window, type \'e\' to exit\n" <<std::endl;

   // interact with data
   renWin->SetSize( 800, 800 );
   renWin->Render();
   iren->Start();

   reader->Delete();
   lut->Delete();
   for ( i=0; i<numOutputs; i++ )
   {
      //cout << "blockMapper[" << i << "] = " << blockMapper[i] <<std::endl;
      if ( blockMapper[i] ) blockMapper[i]->Delete();
      //cout << "blockActor[" << i << "] = " << blockActor[i] <<std::endl;
      if ( blockActor[i] ) blockActor[i]->Delete();
   }
   ren1->Delete();
   renWin->Delete();
   iren->Delete();

   return uGrid;
}

