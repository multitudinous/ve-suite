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
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include <iostream>

#include "VE_Xplorer/Utilities/fileIO.h"
#include "VE_Xplorer/Utilities/readWriteVtkThings.h"
#include "VE_Xplorer/Utilities/viewCells.h"
#include "VE_Xplorer/Utilities/cfdGrid2Surface.h"
#include "VE_Xplorer/Utilities/setScalarAndVector.h"

#include <vtkDataSet.h>
#include <vtkPointData.h>
#include <vtkContourFilter.h>
#include <vtkPolyData.h>
#include <vtkPolyDataNormals.h>
#include <vtkGeometryFilter.h>
#include <vtkFloatArray.h>
#include <vtkTriangleFilter.h>
#include <vtkSTLWriter.h>
#include <vtkDataObject.h>
#include <vtkMultiGroupDataGeometryFilter.h>
#include <vtkMultiGroupDataSet.h>
#include <vtkCellDataToPointData.h>
#include <vtkCompositeDataPipeline.h>
#include <vtkAlgorithm.h>

#include <vtkAlgorithmOutput.h>
#include <vtkMultiGroupDataSetAlgorithm.h>
#include <vtkAppendPolyData.h>
#include <vtkMultiGroupDataGeometryFilter.h>
#include <vtkMultiBlockDataSetAlgorithm.h>
using namespace VE_Util;

void writeVtkGeomToStl( vtkDataObject * dataset, std::string filename )
{
   vtkTriangleFilter *tFilter = vtkTriangleFilter::New();
   vtkMultiGroupDataGeometryFilter *gFilter = NULL;

   // convert dataset to vtkPolyData 
   if ( dataset->IsA("vtkPolyData") )
      tFilter->SetInput( (vtkPolyData*)dataset );
   else 
   {
      std::cout << "Using vtkGeometryFilter to convert to polydata" << std::endl;
      gFilter = vtkMultiGroupDataGeometryFilter::New();
      gFilter->SetInput( dynamic_cast<vtkMultiGroupDataSet*> (dataset) );
      tFilter->SetInput( gFilter->GetOutput() );
   }

   std::cout << "Writing \"" << filename << "\"... ";
   std::cout.flush();
   vtkSTLWriter *writer = vtkSTLWriter::New();
      writer->SetInput( tFilter->GetOutput() );
      writer->SetFileName( filename.c_str() );
      writer->SetFileTypeToBinary();
      writer->Write();
      writer->Delete();
   std::cout << "... done\n" << std::endl;

   tFilter->Delete();

   if ( gFilter ) 
      gFilter->Delete();
}

int main( int argc, char *argv[] )
{    
   // If the command line contains an input vtk file name and an output file,
   // set them up.  Otherwise, get them from the user...
	std::string inFileName;// = NULL;
   std::string outFileName;// = new char [20];
   outFileName.assign( "surface.vtk" );//strcpy(outFileName, "surface.vtk" );  //default name
   fileIO::processCommandLineArgs( argc, argv, 
                  "make a surface from the data in", inFileName, outFileName );
   if ( ! inFileName.c_str() ) return 1;
   ///This will need to be changed to handle both vtkDataset and vtkMultigroupDataSet
   vtkDataObject* dataset = (readVtkThing( inFileName, 1 ));
   //////////DEBUGGING STUFF//////////
   if ( dataset == NULL )
      std::cout<<"NULL dataset :"<<std::endl;
   //////////DEBUGGING STUFF//////////
   
   std::cout << "\nEnter (0) to wrap the entire solution space in a surface, or"
        << "\n      (1) to extract a particular isosurface: " << std::endl;
   int extractIsosurface = fileIO::getIntegerBetween( 0, 1 );

   vtkPolyData* surface = NULL;

   if( extractIsosurface )
   {
      // set the active scalar...
      std::cout<<"Activating scalar :"<<std::endl;
      activateScalar( dynamic_cast<vtkDataSet*>(dataset) );

      float value = 0.0;
      std::cout << "Enter isosurface value: ";
      std::cin >> value;

      vtkMultiGroupDataGeometryFilter* filter1 = 
               vtkMultiGroupDataGeometryFilter::New( );
      filter1->SetInput( 0, dataset );
      filter1->Update();
      
      vtkContourFilter *contour = vtkContourFilter::New();
         contour->SetInputConnection( 0, filter1->GetOutputPort(0) );
         contour->SetValue( 0, value );
         contour->UseScalarTreeOff();
         contour->Update();
         if ( contour != NULL )
            std::cout<<"Set contour filter :"<<std::endl;

      vtkPolyDataNormals *normals = vtkPolyDataNormals::New();
         normals->SetInputConnection( 0, contour->GetOutputPort(0) );
         normals->Update();
         filter1->Update();

      std::cout<<"Set normals :"<<std::endl;
      
      int numPolys = filter1->GetOutput()->GetNumberOfPolys();
      std::cout << "     The number of polys is "<< numPolys << std::endl;
      if ( numPolys==0 ) return 1;

      float deciVal;
      std::cout << "\nDecimation value (range from 0 [more triangles] to 1 [less triangles]) : ";
      std::cin >> deciVal;

      surface = cfdGrid2Surface( dynamic_cast<vtkDataSet*>(filter1->GetOutputDataObject(0)), deciVal );
      std::cout<<"Num polys in surf :"<<surface->GetNumberOfPolys()<<std::endl;
      if ( surface == NULL )
         std::cout<<"No surface !!!! "<<std::endl;

      //clean up
      contour->Delete();
      normals->Delete();
      filter1->Delete();
   
      std::cout<<" writing :"<<std::endl;
      writeVtkThing( surface, outFileName, 1 );   //1 is for binary
   }
   else // Create a polydata surface file that completely envelopes the solution space
   {
      float deciVal;
      std::cout << "\nDecimation value (range from 0 [more triangles] to 1 [less triangles]) : ";
      std::cin >> deciVal;
      std::cin.ignore();

      surface = cfdGrid2Surface( dynamic_cast<vtkDataSet*>(dataset), deciVal );

      int answer;
      std::string extension = fileIO::getExtension( outFileName );

      if ( !extension.compare("stl") || !extension.compare("STL") )//!strcmp(extension,"stl") || !strcmp(extension,"STL") )
         answer = 1;
      else if ( !extension.compare("vtk") || !extension.compare("VTK") )//( !strcmp(extension,"vtk") || !strcmp(extension,"VTK") )
         answer = 0;
      else
      {
         std::cout << "\nDo you want output as a VTK file or as an STL? "
              << "( 0=VTK, 1=STL )" << std::endl;
         answer = fileIO::getIntegerBetween( 0, 1 );
      }

      //delete [] extension;    extension = NULL;

      if ( answer == 0 )
         writeVtkThing( surface, outFileName, 1 );   //1 is for binary
      else
         writeVtkGeomToStl( surface, outFileName );
   }

   // Display the surface on the screen...
   viewCells( surface );

   //clean up
   surface->Delete();
   dataset->Delete();
   std::cout << "\ndone\n";
   return 0;
}

