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
 * File:          $RCSfile: mergeVertices.cpp,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include <iostream>

#include "fileIO.h"
#include "readWriteVtkThings.h"

#include <vtkDataSet.h>
#include <vtkUnstructuredGrid.h>
#include <vtkSetGet.h> // defines data object types
#include <vtkExtractUnstructuredGrid.h>

int main( int argc, char *argv[] )
{    
   // If the command line contains an input vtk file name and an output file set them up.
   // Otherwise, get them from the user...
   char *inFileName = NULL;
   char *outFileName = NULL;
   fileIO::processCommandLineArgs( argc, argv, "merge cell vertices in", inFileName, outFileName );
   if ( ! inFileName ) return 1;

   vtkDataSet * dataset = readVtkThing( inFileName, 1 ); // "1" means print info to screen

   //cout << "\nprinting dataset..." << endl;
   //dataset->Print( cout );

   if ( dataset->IsA("vtkUnstructuredGrid") )
   {
      vtkExtractUnstructuredGrid *extunsgrid = vtkExtractUnstructuredGrid::New();
      //extunsgrid->DebugOn();
      extunsgrid->BreakOnError();
      extunsgrid->PointClippingOn();
      extunsgrid->CellClippingOff();
      extunsgrid->ExtentClippingOff();
      extunsgrid->MergingOn();

      int numPoints = dataset->GetNumberOfPoints();
      std::cout << "numPoints = " << numPoints << std::endl;
      extunsgrid->SetInput( ( vtkUnstructuredGrid * ) dataset );
      extunsgrid->SetPointMinimum( 0 );
      extunsgrid->SetPointMaximum( numPoints );
      extunsgrid->Update();

      writeVtkThing( extunsgrid->GetOutput(), outFileName, 1 ); // 1 means print binary
      extunsgrid->Delete();
   }
   else
   {
      std::cout <<"\nERROR - can only merge points for vtkUnstructuredGrids" << std::endl;
      dataset->Delete();
      delete [] inFileName;   inFileName = NULL;
      delete [] outFileName;  outFileName = NULL;
      return 1;
   }

   dataset->Delete();
   delete [] inFileName;   inFileName = NULL;
   delete [] outFileName;  outFileName = NULL;

   return 0;
}

