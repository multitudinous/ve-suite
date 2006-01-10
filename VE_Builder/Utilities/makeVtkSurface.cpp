/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2006 by Iowa State University
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
 * File:          $RCSfile: makeVtkSurface.cpp,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include <iostream>

#include "VE_Xplorer/fileIO.h"
#include "VE_Xplorer/readWriteVtkThings.h"
#include "VE_Builder/Translator/viewCells.h"
#include "VE_Builder/Translator/cfdGrid2Surface.h"
#include "VE_Builder/Utilities/setScalarAndVector.h"

#include <vtkDataSet.h>
#include <vtkPointData.h>
#include <vtkContourFilter.h>
#include <vtkPolyData.h>
#include <vtkPolyDataNormals.h>
#include <vtkGeometryFilter.h>
#include <vtkFloatArray.h>
#include <vtkTriangleFilter.h>
#include <vtkSTLWriter.h>

using namespace VE_Util;

void writeVtkGeomToStl( vtkDataSet * dataset, std::string filename )
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

   vtkDataSet * dataset = readVtkThing( inFileName, 1 ); // "1" means print info to screen

   std::cout << "\nEnter (0) to wrap the entire solution space in a surface, or"
        << "\n      (1) to extract a particular isosurface: " << std::endl;
   int extractIsosurface = fileIO::getIntegerBetween( 0, 1 );

   vtkPolyData * surface = NULL;

   if (extractIsosurface)
   {
      // set the active scalar...
      activateScalar( dataset );
      double range[2];
      dataset->GetScalarRange( range );
      std::cout << "\nThe scalar range is " << range[0] << " to " << range[1] << std::endl;

      float value = 0.0;
      std::cout << "Enter isosurface value: ";
      std::cin >> value;

      // Create an isosurace with the specified isosurface value...
      vtkContourFilter *contour = vtkContourFilter::New();
         contour->SetInput( dataset );
         contour->SetValue( 0, value );
         contour->UseScalarTreeOff();

      vtkPolyDataNormals *normals = vtkPolyDataNormals::New();
         normals->SetInput( contour->GetOutput() );

      vtkGeometryFilter *filter = vtkGeometryFilter::New();
         filter->SetInput( normals->GetOutput() );
         filter->Update();

      int numPolys = filter->GetOutput()->GetNumberOfPolys();
      std::cout << "     The number of polys is "<< numPolys << std::endl;
      if ( numPolys==0 ) return 1;

      float deciVal;
      std::cout << "\nDecimation value (range from 0 [more triangles] to 1 [less triangles]) : ";
      std::cin >> deciVal;

      surface = cfdGrid2Surface( filter->GetOutput(), deciVal );

      //clean up
      contour->Delete();
      normals->Delete();
      filter->Delete();

      writeVtkThing( surface, outFileName, 1 );   //1 is for binary
   }
   else // Create a polydata surface file that completely envelopes the solution space
   {
      float deciVal;
      std::cout << "\nDecimation value (range from 0 [more triangles] to 1 [less triangles]) : ";
      std::cin >> deciVal;
      std::cin.ignore();
      //std::cout << "Decimation value = " << deciVal << std::endl;

      surface = cfdGrid2Surface( dataset, deciVal );

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
   //delete [] inFileName;    inFileName = NULL;
   //delete [] outFileName;   outFileName = NULL;

   std::cout << "\ndone\n";
   return 0;
}

