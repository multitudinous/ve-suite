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
 * File:          $RCSfile: convertVTK2Ascii.cpp,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include <iostream>

#include "fileIO.h"
#include "readWriteVtkThings.h"

#include "vtkDataSet.h"


int main( int argc, char *argv[] )
{    
   int printInfoToScreen = 0; // "1" means print info to screen

   // If the command line contains an input vtk file name and an output file,
   // set them up.  Otherwise, get them from the user...
	char *inFileName = NULL;
	char *outFileName = NULL;
   fileIO::processCommandLineArgs( argc, argv, "convert binary", 
                                   inFileName, outFileName );
   if ( ! inFileName ) return 1;

   vtkDataSet * dataset = readVtkThing( inFileName, printInfoToScreen );

   if ( printInfoToScreen )
   {
      std::cout << "\nback in main..." << std::endl; 
      printWhatItIs( dataset );
   }

   writeVtkThing( dataset, outFileName );

   dataset->Delete();
   delete [] inFileName;   inFileName = NULL;
   delete [] outFileName;  outFileName = NULL;

   return 0;
}

