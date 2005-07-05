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
 * File:          $RCSfile: arrowCreator.cpp,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include <iostream>

#include "cfdArrow.h"
#include "fileIO.h"

//#include "vtkDataSetReader.h"
//#include "vtkPolyData.h"
//#include "vtkPolyDataReader.h"
#include <vtkPolyDataWriter.h>
using namespace VE_Util;

int main( int argc, char *argv[] )
{    
   // If it is on command line, get output file name...
   char * outFileName = 0;
   if (argc > 1)
   {
      outFileName = new char [100];
      strcpy( outFileName, argv[1]);
   }
   else outFileName = fileIO::getWritableFile( "newArrow.vtk" );

/*
   // for comparison purposes, read and write existing arrow file...
   vtkDataSetReader *reader = vtkDataSetReader::New();
   reader->SetFileName( "/home/users/sjk60/cvs/VRXPR_serial/data/arrow.vtk" );

   vtkPolyDataReader *reader1;
   if (reader->IsFilePolyData())
   {
      reader1 = vtkPolyDataReader::New();
      reader1->SetFileName( "/home/users/sjk60/cvs/VRXPR_serial/data/arrow.vtk" );
      reader1->Update();
   }
   reader->Delete();

   std::cout << "Writing \"currentArrow.vtk\"... ";
   std::cout.flush();
   vtkPolyDataWriter *writer = vtkPolyDataWriter::New();
   writer->SetInput( reader1->GetOutput() );
   writer->SetFileName( "currentArrow.vtk" );
   //writer->SetFileTypeToBinary();
   writer->Write();
   writer->Delete();
   std::cout << "... done" << std::endl;
*/

   std::cout << "\nThis is your opportunity to supply arrow parameter information" << std::endl;
   std::cout << "For each response, you may type '0' to use VE-Suite defaults" << std::endl;

   cfdArrow * arrow = new cfdArrow;

   // in vtkArrowSource, the shaft base is always at (0,0,0). The arrow tip is always at (1,0,0). 
   // here, the shaft base is always at (-1,0,0). The arrow tip is always at (0,0,0)
   int shaftResolution;
   std::cout << "\nInput shaft resolution" << std::endl;
   std::cout << "(1 gives a rectangle, 2 gives crossed rectangles, vtk default is 6)" << std::endl;
   std::cout << "(VE-Suite uses a 3-sided rectangular shaft): ";
   std::cin >> shaftResolution;
   if ( shaftResolution == 0 ) shaftResolution = 3;
   arrow->SetShaftResolution( shaftResolution );

   float shaftRadius;
   std::cout << "\nInput shaft radius" << std::endl;
   std::cout << "(VE-Suite uses vtk default of 0.03): ";
   std::cin >> shaftRadius;
   if ( shaftRadius == 0.0 ) shaftRadius = 0.03;
   arrow->SetShaftRadius( shaftRadius );

   int tipResolution;
   std::cout << "\nInput tip resolution" << std::endl;
   std::cout << "(1 gives a single triangle, 2 gives crossed triangles, vtk default is 6)" << std::endl;
   std::cout << "(VE-Suite uses a 3-sided pyramidal tip): ";
   std::cin >> tipResolution;
   if ( tipResolution == 0 ) tipResolution = 3;
   arrow->SetTipResolution( tipResolution );

   float tipRadius;
   std::cout << "\nInput tip radius" << std::endl;
   std::cout << "(vtk default is 0.10, VE-Suite uses default of 0.15): ";
   std::cin >> tipRadius;
   if ( tipRadius == 0.0 ) tipRadius = 0.15;
   arrow->SetTipRadius( tipRadius );

   float tipLength;
   std::cout << "\nInput tip length" << std::endl;
   std::cout << "(VE-Suite uses vtk default of 0.35): ";
   std::cin >> tipLength;
   if ( tipLength == 0.0 ) tipLength = 0.35;
   arrow->SetTipLength( tipLength );

   std::cout << "\nWriting \"" << outFileName << "\"... ";
   std::cout.flush();
   vtkPolyDataWriter *writer = vtkPolyDataWriter::New();
   writer->SetInput( arrow->GetPolyData() );
   writer->SetFileName( outFileName );
   //writer->SetFileTypeToBinary();
   writer->Write();
   writer->Delete();
   std::cout << "... done\n" << std::endl;

   return 0;
}

