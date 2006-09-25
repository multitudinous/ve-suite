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
 * Date modified: $Date: 2006-07-08 23:03:51 -0500 (Sat, 08 Jul 2006) $
 * Version:       $Rev: 4909 $
 * Author:        $Author: mccdo $
 * Id:            $Id: whatIsScalarRange.cpp 4909 2006-07-09 04:03:51Z mccdo $
 * -----------------------------------------------------------------
 *
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
#include <vtkCellData.h>
#include <vtkDoubleArray.h>
using namespace VE_Util;

int main( int argc, char *argv[] )
{    
   // If the command line contains an input vtk file name, then use it.
   // Otherwise, get it from the user...
   std::string inFileName;
   if ( argc > 1 )
   {
      inFileName.assign( argv[ 1 ] );
   }
   else  // then get filename from user...
   {
      char tempText[ 100 ]; 
      strcpy( tempText, "the file to add a vector to" );
      inFileName = fileIO::getReadableFileFromDefault( tempText, "inFile.vtk" );
   }

   // read the data set ("1" means print info to screen)
   vtkDataSet* dataset = readVtkThing( inFileName, 1 );

   std::string input;
   std::string tempText;
   std::string response;
   std::vector< std::string > vectorNames;
   do
   {
      tempText.assign( "the scalar to create a vector from" ); 
      tempText = fileIO::getFilenameFromDefault( tempText, "X_VELOCITY" );
      
      do 
      {
         std::cout << "\nSo you want to add " << tempText << " as a component of the vector? (y/n): ";
         std::cin >> response;
         std::cin.ignore();
      } 
      while ( response != "y" && response != "Y" && 
               response != "n" && response != "N" );
      
      if ( response == "y" || response == "Y" )
      {
         if ( dataset->GetCellData() )
         {   
            if ( dataset->GetCellData()->GetArray( tempText.c_str() ) )
            {   
               vectorNames.push_back( tempText );
            }
            else
            {   
               std::cout << " scalar is not in dataset" << std::endl;
            }
         }
         else if ( dataset->GetPointData() )
         {   
            if ( dataset->GetPointData()->GetArray( tempText.c_str() ) )
            {   
               vectorNames.push_back( tempText );
            }
            else
            {   
               std::cout << " scalar is not in dataset" << std::endl;
            }
         }
         else
         {   
            std::cout << "no data in dataset" << std::endl;
         }
      }
   }
   while ( vectorNames.size() < 4 );

   vtkDataArray* uVectorArray = 0;
   vtkDataArray* vVectorArray = 0;
   vtkDataArray* wVectorArray = 0;

   if ( dataset->GetCellData() )
   {
      uVectorArray = dataset->GetCellData()->GetArray( vectorNames.at( 0 ).c_str() );
      vVectorArray = dataset->GetCellData()->GetArray( vectorNames.at( 1 ).c_str() );
      wVectorArray = dataset->GetCellData()->GetArray( vectorNames.at( 2 ).c_str() );
   }
   else if ( dataset->GetPointData() )
   {
      uVectorArray = dataset->GetPointData()->GetArray( vectorNames.at( 0 ).c_str() );
      vVectorArray = dataset->GetPointData()->GetArray( vectorNames.at( 1 ).c_str() );
      wVectorArray = dataset->GetPointData()->GetArray( vectorNames.at( 2 ).c_str() );
   }
   vtkDoubleArray* vectorArray = vtkDoubleArray::New();
   vectorArray->SetName( "Velocity" );
   vectorArray->SetNumberOfComponents( 3 );
   
   int numTuples = uVectorArray->GetNumberOfTuples();
   vectorArray->SetNumberOfTuples( numTuples );

   for ( int i = 0; i < numTuples; ++i )
   {
      vectorArray->SetTuple3( i, uVectorArray->GetTuple1( i ), vVectorArray->GetTuple1( i ), wVectorArray->GetTuple1( i ) );
   }
   
   if ( dataset->GetCellData() )
   {
      dataset->GetCellData()->AddArray( vectorArray );
   }
   else if ( dataset->GetPointData() )
   {
      dataset->GetPointData()->AddArray( vectorArray );
   }
   vectorArray->Delete();
   
   // write vti file
   VE_Util::writeVtkThing( dataset, inFileName, 1 );

   dataset->Delete();

   return 0;
}

