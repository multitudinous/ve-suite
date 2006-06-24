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
 * File:          $RCSfile: whatIsScalarRange.cpp,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include <iostream>

#include "VE_Xplorer/Utilities/fileIO.h"
#include "VE_Xplorer/Utilities/readWriteVtkThings.h"
#include "VE_Xplorer/Utilities/cfdAccessoryFunctions.h"

#include <vtkDataSet.h>
#include <vtkDataArray.h>
#include <vtkPointData.h>
using namespace VE_Util;

int main( int argc, char *argv[] )
{    
   // If the command line contains an input vtk file name, then use it.
   // Otherwise, get it from the user...
   std::string inFileName;
   if ( argc > 1 )
   {
      //inFileName = new char [ strlen(argv[ 1 ])+1 ];
      inFileName.assign( argv[ 1 ] );//strcpy( inFileName, argv[ 1 ] );
   }
   else  // then get filename from user...
   {
      char tempText[ 100 ]; 
      strcpy( tempText, "the file to compute scalar/vector range" );
      inFileName = fileIO::getReadableFileFromDefault( tempText, "inFile.vtk" );
   }

   // read the data set ("1" means print info to screen)
   vtkDataSet * dataset = readVtkThing( inFileName, 1 );

   int numArrays = dataset->GetPointData()->GetNumberOfArrays();
   //std::cout << "numArrays = " << numArrays << std::endl;
   double minMax[ 2 ];
   for ( int i = 0; i < numArrays; i++ )
   {
      vtkDataArray * array_i = dataset->GetPointData()->GetArray(i);
      int numComponents = array_i->GetNumberOfComponents();
      if ( numComponents != 1 && numComponents != 3 )
      {
         std::cout << "ERROR: Unexpected number of components (" 
              << numComponents << ") in array " << i << std::endl;
         continue;
      }

      if ( numComponents == 3 )
      {
         double * vecMagRange = cfdAccessoryFunctions::
                                ComputeVectorMagnitudeRange( 
                                      dataset->GetPointData()->GetArray( i ) );

         std::cout << "array " << i << ": vector named \"" << array_i->GetName() 
              << "\", vector magnitude range:\t" 
              << vecMagRange[ 0 ] << "\t" << vecMagRange[ 1 ] << std::endl;
         delete [] vecMagRange;

      }
      else // if ( numComponents == 1 ) 
      {
         array_i->GetRange( minMax );
         std::cout << "array " << i << ": scalar named \"" << array_i->GetName()
              << "\", scalar magnitude range:\t"
              << minMax[0] << "\t" << minMax[1] << std::endl;
      }
   }
   std::cout << std::endl;

   dataset->Delete();
   //delete [] inFileName;   inFileName = NULL;

   return 0;
}

