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
 * File:          $RCSfile: readWriteVtkThings.cxx,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include <iostream>
#include <stdlib.h>

#include "readWriteVtkThings.h"

#include <vtkDataSet.h>
#include <vtkDataSetReader.h>

#include <vtkUnstructuredGrid.h>
#include <vtkUnstructuredGridReader.h>
#include <vtkUnstructuredGridWriter.h>

#include <vtkStructuredGrid.h>
#include <vtkStructuredGridReader.h>
#include <vtkStructuredGridWriter.h>

#include <vtkRectilinearGrid.h>
#include <vtkRectilinearGridReader.h>
#include <vtkRectilinearGridWriter.h>

#include <vtkPolyData.h>
#include <vtkPolyDataReader.h>
#include <vtkPolyDataWriter.h>

#include <vtkPointData.h>

void printWhatItIs( vtkDataSet * dataSet )
{
   if ( dataSet == NULL )
   {
      std::cout << "\tdataSet == NULL" << std::endl;
      return;
   }
   if ( dataSet->IsA("vtkDataSet") )          
      std::cout << "\tIsA(\"vtkDataSet\")" << std::endl;
   if ( dataSet->IsA("vtkPointSet") )         
      std::cout << "\tIsA(\"vtkPointSet\")" << std::endl;
   if ( dataSet->IsA("vtkUnstructuredGrid") ) 
      std::cout << "\tIsA(\"vtkUnstructuredGrid\")" << std::endl;
   if ( dataSet->IsA("vtkStructuredGrid") )   
      std::cout << "\tIsA(\"vtkStructuredGrid\")"<< std::endl;
   if ( dataSet->IsA("vtkPolyData") )         
      std::cout << "\tIsA(\"vtkPolyData\")"<< std::endl;
   if ( dataSet->IsA("vtkRectilinearGrid") )  
      std::cout << "\tIsA(\"vtkRectilinearGrid\")" << std::endl;
   //std::cout << "GetDataObjectType() = " << dataSet->GetDataObjectType() << std::endl;
}

void printBounds( double bounds[6] )
{
   std::cout << "geometry bounding box information..." << std::endl;
   std::cout << "\tx-min = \t" << bounds[0]
             << "\tx-max = \t" << bounds[1] << std::endl;
   std::cout << "\ty-min = \t" << bounds[2] 
             << "\ty-max = \t" << bounds[3] << std::endl;
   std::cout << "\tz-min = \t" << bounds[4] 
             << "\tz-max = \t" << bounds[5] << std::endl;
}

vtkDataSet * readVtkThing( char * vtkFilename, int printFlag )
{
   if (printFlag) 
   {
      std::cout << "\nin readVtkThing with \"" << vtkFilename
                << "\"" << std::endl;
   }

/*
   //try to open the file
   vtkDataReader* fileloader = vtkDataReader::New();
   fileloader->SetFileName( vtkFilename );

   if( !fileloader->OpenVTKFile() )
   {
      std::cout << "Error! Could not open vtk file: "
                << vtkFilename << std::endl;
      exit( 1 );
   }
   //close the file
   fileloader->CloseVTKFile();
   fileloader->Delete();
*/

   vtkDataSetReader *genericReader = vtkDataSetReader::New();
   genericReader->SetFileName( vtkFilename );

   if (printFlag) 
   {
      printWhatItIs( genericReader->GetOutput() );
   }

   int DataObjectType = genericReader->GetOutput()->GetDataObjectType();
   genericReader->Delete();

   if (printFlag) 
   {
      std::cout << "\nReading \"" << vtkFilename << "\", " << std::flush;
   }

   if ( DataObjectType == VTK_UNSTRUCTURED_GRID )
   {
      if (printFlag) 
      {
         std::cout << "an unstructuredGrid... " << std::flush;
      }
      vtkUnstructuredGridReader *unsReader = vtkUnstructuredGridReader::New();
      unsReader->SetFileName( vtkFilename );
      unsReader->Update();

      vtkUnstructuredGrid *unsGrid = vtkUnstructuredGrid::New();
      unsGrid->ShallowCopy( unsReader->GetOutput() );

      unsReader->Delete();

      if (printFlag) 
      {
         std::cout << "... done" << std::endl;
         std::cout << " getting bounds...";
         double bounds[ 6 ];
         unsGrid->GetBounds( bounds );
         printBounds( bounds );
      }
/*
      std::cout << "\nprinting unsGrid..." << std::endl;
      unsGrid->Print( cout );
*/
      return unsGrid;
   }
   else if ( DataObjectType == VTK_STRUCTURED_GRID )
   {
      if (printFlag) 
      {
         std::cout << "a structuredGrid... " << std::flush;
      }
      vtkStructuredGridReader * sGridReader = vtkStructuredGridReader::New();
      sGridReader->SetFileName( vtkFilename );
      sGridReader->Update();

/*
      // Because of a vtk BUG involving structured grid deep copy,
      // the follow code is not working...
      vtkStructuredGrid *sGrid = vtkStructuredGrid::New();
      sGrid->DeepCopy( sGridReader->GetOutput() );
      sGridReader->Delete();
*/

      if (printFlag) 
      {
         std::cout << "... done" << std::endl;
         std::cout << " getting bounds...";
         double bounds[ 6 ];
         //sGrid->GetBounds( bounds );
         sGridReader->GetOutput()->GetBounds( bounds );
         printBounds( bounds );
      }

/*
      std::cout << "\nprinting sGrid..." << std::endl;
      sGrid->Print( cout );
*/
      return sGridReader->GetOutput();
   }
   else if ( DataObjectType == VTK_RECTILINEAR_GRID )
   {
      if (printFlag) 
      {
         std::cout << "a rectilinearGrid... " << std::flush;
      }
      vtkRectilinearGridReader *recReader = vtkRectilinearGridReader::New();
      recReader->SetFileName( vtkFilename );
      recReader->Update();

      vtkRectilinearGrid *rectGrid = vtkRectilinearGrid::New();
      rectGrid->ShallowCopy( recReader->GetOutput() );

      recReader->Delete();

      if (printFlag) 
      {
         std::cout << "... done" << std::endl;
      }

      return rectGrid;
   }
   else if ( DataObjectType == VTK_POLY_DATA )
   {
      if (printFlag) 
      {
         std::cout << "a polyData... " << std::flush;
      }
      vtkPolyDataReader *pdReader = vtkPolyDataReader::New();
      pdReader->SetFileName( vtkFilename );
      pdReader->Update();

/*
      std::cout << "\nprinting pdReader->GetOutput()..." << std::endl;
      pdReader->GetOutput()->Print( cout );
*/

      vtkPolyData *polyData = vtkPolyData::New();
      polyData->ShallowCopy( pdReader->GetOutput() );

      pdReader->Delete();

      if (printFlag) 
      {
         std::cout << "... done" << std::endl;
         std::cout << " getting bounds...";
         double bounds[ 6 ];
         polyData->GetBounds( bounds );
         printBounds( bounds );
      }

/*
      if ( polyData->GetPointData()->GetScalars() )
      {
         cout << "polyData->GetPointData()->GetScalars()->GetName() = " 
              << polyData->GetPointData()->GetScalars()->GetName() << endl;          
         cout << "polyData->GetPointData()->GetScalars()->GetLookupTable() = " 
              << polyData->GetPointData()->GetScalars()->GetLookupTable() << endl;          
      }
*/

/*
      std::cout << "\nprinting polyData..." << std::endl;
      polyData->Print( cout );
*/
      return polyData;
   }

   //else
   std::cout <<"\nERROR - Unable to read this unsupported vtk file format"
             << std::endl;
   return NULL;
}

void writeVtkThing( vtkDataSet * vtkThing, char * vtkFilename, int binaryFlag )
{
   //std::cout << "\nIn writeVtkThing... " << std::endl;

   if ( vtkThing == NULL )
   {
      std::cout << "Error: Can't write because vtkThing == NULL" << std::endl;
      return;
   }

   vtkThing->Update();

/*
   printWhatItIs( vtkThing );

   std::cout << "getting bounds..." << std::flush;
   float bounds[6];
   vtkThing->GetBounds( bounds );
   printBounds( bounds );
*/
   std::cout << "Writing \"" << vtkFilename << "\", " << std::flush;

   if ( vtkThing->IsA("vtkUnstructuredGrid") )
   {
      std::cout << "a vtkUnstructuredGrid... " << std::flush;

      vtkUnstructuredGrid * uGrid = vtkUnstructuredGrid::SafeDownCast( vtkThing );
      if ( uGrid == NULL )
      {
         cout << "SafeDownCast to a vtkUnstructuredGrid failed";
      }

      vtkUnstructuredGridWriter *writer = vtkUnstructuredGridWriter::New();
      //writer->SetInput( (vtkUnstructuredGrid *)vtkThing );
      writer->SetInput( uGrid );
      writer->SetFileName( vtkFilename );
      if (binaryFlag)
      {
         writer->SetFileTypeToBinary();
      }
      writer->Write();
      writer->Delete();
   }
   else if ( vtkThing->IsA("vtkStructuredGrid") )
   {
      std::cout << "a vtkStructuredGrid... " << std::flush;

      vtkStructuredGrid * sGrid = vtkStructuredGrid::SafeDownCast( vtkThing );
      if ( sGrid == NULL )
      {
         cout << "SafeDownCast to a vtkStructuredGrid failed";
      }
      // Part of old vtk build may need to fix later
      //sGrid->BlankingOff();

      vtkStructuredGridWriter *writer = vtkStructuredGridWriter::New();
      writer->SetInput( sGrid );
      //writer->SetInput( (vtkStructuredGrid *)vtkThing );    //core dumped
      //writer->SetInput( vtkThing );                         //won't compile
      writer->SetFileName( vtkFilename );
      if (binaryFlag) 
      {
         writer->SetFileTypeToBinary();
      }
      writer->Write();
      writer->Delete();
      //sGrid->Delete();
   }
   else if ( vtkThing->IsA("vtkRectilinearGrid") )
   {
      std::cout << "a vtkRectilinearGrid... " << std::flush;
      vtkRectilinearGridWriter *writer = vtkRectilinearGridWriter::New();
      writer->SetInput( (vtkRectilinearGrid*)vtkThing );
      writer->SetFileName( vtkFilename );
      if (binaryFlag)
      {
         writer->SetFileTypeToBinary();
      }
      writer->Write();
      writer->Delete();
   }
   else if ( vtkThing->IsA("vtkPolyData") )
   {
      std::cout << "a vtkPolyData... " << std::flush;
      vtkPolyDataWriter *writer = vtkPolyDataWriter::New();
      writer->SetInput( (vtkPolyData*)vtkThing );
      writer->SetFileName( vtkFilename );
      if (binaryFlag) 
      {
         writer->SetFileTypeToBinary();
      }
      writer->Write();
      writer->Delete();
   }
   else
   {
      std::cerr <<"\nERROR - Unsupported vtk file format: file not written"
                << std::endl;
      return;
   }

   std::cout << "... done\n" << std::endl;
}

