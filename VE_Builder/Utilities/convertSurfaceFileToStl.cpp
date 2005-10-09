/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2005 by Iowa State University
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
 * File:          $RCSfile: convertSurfaceFileToStl.cpp,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include <iostream>

#include <vtkDataSet.h>
#include <vtkGeometryFilter.h>
#include <vtkTriangleFilter.h>
#include <vtkSTLWriter.h>

#include "VE_Xplorer/fileIO.h"
#include "VE_Xplorer/readWriteVtkThings.h"
using namespace VE_Util;

int main( int argc, char *argv[] )
{    
   // If the command line contains an input vtk file name and an output file set them up.
   // Otherwise, get them from the user...
   std::string inFileName;// = NULL;
   std::string outFileName;// = new char [20];
   outFileName.assign( "surface.stl" );//strcpy( outFileName, "surface.stl" );  //default name
   fileIO::processCommandLineArgs( argc, argv, "convert geometry to STL format in", inFileName, outFileName );
   if ( ! inFileName.c_str() ) return 1;

   vtkDataSet * dataset = readVtkThing( inFileName, 1 ); // "1" means print info to screen

   // convert to vtkPolyData    
   vtkGeometryFilter *cFilter = vtkGeometryFilter::New();
      cFilter->SetInput( dataset );

   vtkTriangleFilter *tFilter = vtkTriangleFilter::New();
      tFilter->SetInput( cFilter->GetOutput() );

   std::cout << "Writing \"" << outFileName << "\"... ";
   std::cout.flush();
   vtkSTLWriter *writer = vtkSTLWriter::New();
      writer->SetInput( tFilter->GetOutput() );
      writer->SetFileName( outFileName.c_str() );
      writer->SetFileTypeToBinary();
      writer->Write();
      writer->Delete();
   std::cout << "... done\n" << std::endl;

   cFilter->Delete();
   tFilter->Delete();
   dataset->Delete();
   inFileName.erase();//delete [] inFileName;   inFileName = NULL;
   outFileName.erase();//delete [] outFileName;  outFileName = NULL;

   return 0;
}

