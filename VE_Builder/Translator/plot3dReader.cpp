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
 * File:          $RCSfile: plot3dReader.cpp,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include <vtkStructuredGridWriter.h>
#include <vtkUnstructuredGridWriter.h>
#include <vtkUnstructuredGrid.h>
#include <vtkStructuredGrid.h>
#include <vtkPLOT3DReader.h>     
#include <vtkAppendFilter.h>     
#include <vtkExtractUnstructuredGrid.h>     
#include <vtkDataSet.h>     
#include <vtkPointData.h>     
#include <vtkPoints.h>      
#include <vtkCellArray.h>
#include <vtkFloatArray.h>
#include <vtkDataSetAttributes.h>
#include <vtkStructuredGridGeometryFilter.h>
#include <vtkAppendPolyData.h>

#include "plot3dReader.h"
//#include <fstream>
#include <string>
#include <sstream>
#include <iostream>
#include "fileIO.h"
#include "cfdGrid2Surface.h"
#include "readWriteVtkThings.h"

plot3dReader::plot3dReader( void ) 
{
   writer      = vtkStructuredGridWriter::New();
   reader      = vtkPLOT3DReader::New();
   //reader->ReleaseDataFlagOn();   
   unswriter   = vtkUnstructuredGridWriter::New();
   unsgrid     = vtkUnstructuredGrid::New();
   //unsgrid->ReleaseDataFlagOn();   
   filter      = vtkAppendFilter::New();
   //filter->ReleaseDataFlagOn();
   numOfSurfaceGrids = 0;
}

plot3dReader::~plot3dReader( void ) 
{
   int i;
   if ( writer != NULL )
      writer->Delete();
   
   if ( reader != NULL )
      reader->Delete();
   
   //if ( unsgrid != NULL )
   //   unsgrid->Delete();
   
   if ( unswriter != NULL )
      unswriter->Delete();
   
   if ( filter != NULL )
      filter->Delete();

	for ( i = 0; i < numGrids; i++ )
	{
      grids[ i ]->Delete();
	}
   
   delete [] grids;
}

plot3dReader::plot3dReader( plot3dReader * ) 
{
}

vtkUnstructuredGrid *plot3dReader::GetUnsGrid( void ) 
{
   //const std::string grid( "grid-C1.iris" );
   //const std::string solution( "q-C1-S1.iris" );//qheat-C1-NS.iris
   //std::string prefix( "test" );
   //std::string suffix( "vtk" );
   //std::string vtkFilename;
   //std::string number("012345");
   //std::ostringstream number;

   //const char *gridName = grid.c_str();
   //const char *solutionName = solution.c_str();
   //const char *Filename; 
   if ( numOfSurfaceGrids > 0 )
      return ( MakeVTKSurfaceFromGeomFiles() );


   if ( reader->GetByteOrder() == 0 )
   {
      reader->SetByteOrderToBigEndian();
   }
   else if ( reader->GetByteOrder() == 1 )
   {
      reader->SetByteOrderToLittleEndian();   
   }

   //reader->DebugOn();
   reader->BreakOnError();
   reader->BinaryFileOn();

   std::cout << "Is this a multi-grid file?" << std::endl;
   std::cout << "Answer (0) No (1) Yes" << std::endl;
   answer = fileIO::getIntegerBetween( 0, 1 );
   if ( answer == 1 )
   {
      reader->MultiGridOn();
   }
   else
   {
      reader->MultiGridOff();
   }

   std::cout << "Does this file use IBlanking?" << std::endl;
   std::cout << "Answer (0) No (1) Yes" << std::endl;
   answer = fileIO::getIntegerBetween( 0, 1 );
   if ( answer == 1 )
   {
      reader->IBlankingOn();
   }
   else
   {
      reader->IBlankingOff();
   }   
   
   reader->HasByteCountOff();
   reader->ForceReadOff();
   reader->DoNotReduceNumberOfOutputsOn();

   std::cout << "Is this a two dimensional grid?" << std::endl;
   std::cout << "Answer (0) No (1) Yes" << std::endl;
   answer = fileIO::getIntegerBetween( 0, 1 );
   if ( answer == 1 )
   {
      reader->TwoDimensionalGeometryOn();
   }
   else
   {
      reader->TwoDimensionalGeometryOff();
   }   
   
	//reader->CanReadBinaryFile( gridName );
	reader->SetXYZFileName( plot3dGeomFileName );
	//reader->CanReadBinaryFile( solutionName );
	reader->SetQFileName( plot3dDataFileName );
	reader->SetScalarFunctionNumber( 100 );
	reader->SetVectorFunctionNumber( 200 );
	reader->AddFunction( 110 );
	reader->AddFunction( 120 );
	reader->AddFunction( 140 );
	reader->AddFunction( 144 );
	reader->AddFunction( 153 );
	reader->AddFunction( 184 );
   //reader->Print( cout );
   reader->Update();
	
	numGrids = reader->GetNumberOfGrids();
	int i;
	grids = new vtkStructuredGrid*[ numGrids ];

   std::cout << "|   Number of grids in " <<  plot3dGeomFileName << " : " << 
                                                numGrids << std::endl;
   //writer->DebugOn();
   writer->BreakOnError();
	for ( i = 0; i < numGrids; i++ )
	{
      grids[ i ] = vtkStructuredGrid::New();
      grids[ i ]->ShallowCopy ( (vtkStructuredGrid * ) reader->GetOutput( i ) );
      //number << i;
      //vtkFilename = prefix + '_' + number.str() + '.' + suffix;
      //Filename = vtkFilename.c_str();
		//writer->SetFileName( Filename );
      //if (!debug)
      //writer->SetFileTypeToBinary();
      //writer->SetInput( grids[ i ] );
      //writer->Print( cout );
      //writer->Write();
	}
   
   //filter->DebugOn();
   filter->BreakOnError();

   for ( i = 0; i < numGrids; i++ )
   {
      filter->AddInput( grids[ i ] );
      filter->Update();
      //filter->Print( cout );
   }

   unsgrid->ShallowCopy( ( vtkUnstructuredGrid * ) filter->GetOutput() );
   this->unsgrid->GetPointData()->SetActiveVectors( "Velocity" );
   //this->unsgrid->Print( cout );
   //vtkFilename = prefix + '.' + suffix;
   //Filename = vtkFilename.c_str();
   //unswriter->DebugOn();
   //unswriter->BreakOnError();
	//unswriter->SetFileName( Filename );
   //if (!debug)
   //unswriter->SetFileTypeToBinary();
   //unswriter->SetInput( ( vtkUnstructuredGrid * ) filter->GetOutput() );
   //unswriter->Print( cout );
   //unswriter->Write();
   
   /*int numPoints = ((vtkPointSet *)filter->GetOutput())->GetNumberOfPoints();
   std::cout << "|   Number of vertices in unstructured grid : " << 
                                                      numPoints << std::endl;
   vtkExtractUnstructuredGrid *extunsgrid = vtkExtractUnstructuredGrid::New();
   //extunsgrid->DebugOn();
   extunsgrid->BreakOnError();
   extunsgrid->PointClippingOn();
   extunsgrid->CellClippingOff();
   extunsgrid->ExtentClippingOff();
   extunsgrid->MergingOn();
   
   extunsgrid->SetInput( ( vtkUnstructuredGrid * ) filter->GetOutput() );
   extunsgrid->SetPointMinimum( 0 );
   extunsgrid->SetPointMaximum( numPoints );
   extunsgrid->Update();
   std::cout << "|   Number of points after merged grid : " <<
               ( (vtkPointSet *)extunsgrid->GetOutput() )->GetNumberOfPoints() << std::endl;
	unswriter->SetFileName( "merged.vtk" );
   unswriter->SetInput( ( vtkUnstructuredGrid * ) extunsgrid->GetOutput() );
   //unswriter->Print( cout );
   //if (!debug)
   unswriter->SetFileTypeToBinary();
   unswriter->Write(); 
   */
   return ( unsgrid );
}

void plot3dReader::GetFileNames( void ) 
{
   numOfSurfaceGrids = 0;
   std::cout << "Do you want to create an unstrcutured grid or a surface" << std::endl;
   std::cout << "Answer (0) grid (1) surface" << std::endl;
   answer = fileIO::getIntegerBetween( 0, 1 );
   if ( answer == 1 )
   {
      do
      {
         plot3dSurfaceFileName[ numOfSurfaceGrids ] = new char[ 100 ];
         do
         {
            std::cout << "Geometry file name (xyz file):\t" << std::endl;
            std::cin >> plot3dSurfaceFileName[ numOfSurfaceGrids ];
            std::cin.ignore();
         }
         while ( ! fileIO::isFileReadable( plot3dSurfaceFileName[ numOfSurfaceGrids ] ) );
         numOfSurfaceGrids+=1;
        
         std::cout << "Do you have another file to enter" << std::endl;
         std::cout << "Answer (0) No (1) Yes" << std::endl;
         answer = fileIO::getIntegerBetween( 0, 1 );
         
         std::cout << answer << std::endl;
         std::cout << "numgrid points   " << numOfSurfaceGrids << std::endl;
      }
      while ( answer == 1 );
   }
   else
   {
      do
      {
         std::cout << "Geometry file name (xyz file):\t" << std::endl;
         std::cin >> plot3dGeomFileName;
         std::cin.ignore();
      }
      while ( ! fileIO::isFileReadable( plot3dGeomFileName ) );

      do
      {
         std::cout << "Data file name (q file):\t" << std::endl;
         std::cin >> plot3dDataFileName;
         std::cin.ignore();
      }
      while ( ! fileIO::isFileReadable( plot3dDataFileName ) );
   }   
}

vtkUnstructuredGrid *plot3dReader::MakeVTKSurfaceFromGeomFiles( void ) 
{
   int i;
   // Render the shell of the dataset
   vtkPLOT3DReader **body3;
   body3 = new vtkPLOT3DReader*[ numOfSurfaceGrids - 1 ];
  vtkStructuredGridGeometryFilter *cFilter = vtkStructuredGridGeometryFilter::New( );
   vtkAppendPolyData *polyfilter = vtkAppendPolyData::New();
   for ( i = 0; i < numOfSurfaceGrids; i++ )
   {
    body3[i] = vtkPLOT3DReader::New();

std::cout << plot3dSurfaceFileName[ i ] << std::endl;
      body3[i]->SetXYZFileName( plot3dSurfaceFileName[ i ] );
	   body3[i]->Update( );

     cFilter->SetInput( body3[i]->GetOutput( ) );
     cFilter->SetExtent( 0, 100, 0, 100, 0, 100 );
     cFilter->Update( );

         
      polyfilter->AddInput( (vtkPolyData *)cfdGrid2Surface( (vtkStructuredGrid * ) cFilter->GetOutput(), 0.6 ) );
      //cFilter->SetInput( body3->GetOutput( ) );
      //cFilter->SetExtent( 0, 100, 0, 100, 0, 100 );
      //cFilter->Update( );
      // filter->AddInput( (vtkStructuredGrid * ) cFilter->GetOutput() );
      polyfilter->Update();
   }
   
   writeVtkThing( (vtkDataSet *)polyfilter->GetOutput(), "testsurf.vtk", 1 );   //1 is for binary
   exit( 1 );
   //unsgrid->ShallowCopy( ( vtkUnstructuredGrid * ) filter->GetOutput() );
   //body3->Delete();
   return ( unsgrid );
}
