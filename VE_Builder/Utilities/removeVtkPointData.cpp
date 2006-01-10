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
 * File:          $RCSfile: removeVtkPointData.cpp,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include <iostream>
#include <vector> 

#include "VE_Xplorer/fileIO.h"
#include "VE_Xplorer/readWriteVtkThings.h"

#include <vtkDataSet.h>
#include <vtkFloatArray.h>
#include <vtkPointData.h>
using namespace VE_Util;

void removeVtkPointData( vtkDataSet * dataSet )
{
   // if there are data arrays, count the number of arrays
   int numPDArrays = dataSet->GetPointData()->GetNumberOfArrays();
   std::cout << "numPDArrays = " << numPDArrays << std::endl;
   if ( numPDArrays )
   {
      std::vector< std::string > names;//char **names = new char * [numPDArrays];
      for (int i=0; i < numPDArrays; i++)
      {
         int len = strlen( dataSet->GetPointData()->GetArray(i)->GetName() );
         //names[i] = new char [len+1];
         names.push_back(dataSet->GetPointData()->GetArray(i)->GetName());//strcpy( names[i], dataSet->GetPointData()->GetArray(i)->GetName() );
         //std::cout << "names[i] = " <<  names[i] << std::endl;
      }

      for (int i=0; i < numPDArrays; i++)
      {
         char response;
         do 
         {
            std::cout << "Do you want parameter \"" << names[i] 
                 << "\" retained in the flowdata file? [y/n]: ";
            std::cin >> response;
         } while (response != 'y' && response != 'Y' && response != 'n' && response != 'N');

         // go to next scalar if anything other than n/N was input...
         if (response != 'n' && response != 'N') continue;

         dataSet->GetPointData()->RemoveArray( names[i].c_str() );
      }

      for (int i=0; i < numPDArrays; i++)
      {
         //delete [] names[i];
         //names[i] = NULL;
      }
      names.clear();//delete [] names;
      //names = NULL;
   }
   return;
}

int main( int argc, char *argv[] )
{    
   // If the command line contains an input vtk file name and an output file set them up.
   // Otherwise, get them from the user...
	std::string inFileName;// = NULL;
	std::string outFileName;// = NULL;
   fileIO::processCommandLineArgs( argc, argv, "remove point data parameters from", inFileName, outFileName );
   if ( ! inFileName.c_str() ) return 1;

   vtkDataSet * dataset = readVtkThing( inFileName, 1 ); // "1" means print info to screen

   removeVtkPointData( dataset );

   writeVtkThing( dataset, outFileName );

   dataset->Delete();

   return 0;
}

