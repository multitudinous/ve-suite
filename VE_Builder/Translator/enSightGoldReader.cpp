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
 * File:          $RCSfile: readWriteVtkThings.h,v $
 * Date modified: $Date: 2004-05-18 15:44:18 -0500 (Tue, 18 May 2004) $
 * Version:       $Rev: 382 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "enSightGoldReader.h"
#include <iostream>
#include <string>
#include <sstream>
#include <vtkGenericEnSightReader.h>          // will open any ensight file
#include <vtkCellDataToPointData.h>
#include <vtkUnstructuredGrid.h>
#include <vtkPointData.h>
#include <vtkFloatArray.h>
#include "converter.h"
enSightGoldReader::enSightGoldReader( void )
{
   reader = NULL;
   cell2point = NULL;
}

enSightGoldReader::~enSightGoldReader( void )
{
   if ( reader != NULL )
      reader->Delete();

   if ( cell2point != NULL )
      cell2point->Delete();
}

vtkUnstructuredGrid* enSightGoldReader::GetUnstructuredGrid( std::string caseFileName, int debug )
{
   reader = vtkGenericEnSightReader::New();
   //reader->DebugOn();
   reader->SetCaseFileName( caseFileName.c_str() );
   reader->Update();
   
   // Convert vtkEnSightGoldReader cell centered data to point data
   cell2point = vtkCellDataToPointData::New();
   cell2point->SetInput( reader->GetOutput() );
   cell2point->PassCellDataOff();
   cell2point->Update();
   
   // Must do this so that we can clean up properly in the destructor
   vtkUnstructuredGrid *finalUGrid = vtkUnstructuredGrid::New();
   finalUGrid->ShallowCopy( cell2point->GetUnstructuredGridOutput() );
   //this portion is to grab scalar data out of the vectors and rewrite it back
   //into the VTK file
   int numArrays = finalUGrid->GetPointData()->GetNumberOfArrays();
   if ( debug )
      std::cout << " Number of arrays is :" << numArrays <<std::endl;
   int numComponents;
   int numOfTuples;
   std::string name;
   std::string scalName;
   double component;
   double velMag;
   for ( int i=0;i<numArrays; i++ ) //loop pver number of arrays
   {
      numComponents = finalUGrid->GetPointData()->GetArray(i)->GetNumberOfComponents();
      if ( numComponents > 1 ) //it is a vector
      {
         vtkFloatArray** scalarsFromVector;
         
         scalarsFromVector = new vtkFloatArray* [ numComponents+1 ];
         name = (std::string) finalUGrid->GetPointData()->GetArray(i)->GetName();
         numOfTuples = finalUGrid->GetPointData()->GetArray(i)->GetNumberOfTuples();
         if ( debug ) std::cout<<" Name :" << name <<std::endl
                               <<" Number of Tuples :"<< numOfTuples <<std::endl;
         for ( int compLoop=0;compLoop<numComponents; compLoop++ )
         {
            scalName = name;
            if ( compLoop == 0 ) scalName.append("_u");
            else if ( compLoop == 1 ) scalName.append("_v");
            else if ( compLoop == 2 ) scalName.append("_w");            
            scalarsFromVector[ compLoop ] = vtkFloatArray::New();
            scalarsFromVector[ compLoop ]->SetNumberOfComponents( 1 );
            scalarsFromVector[ compLoop ]->SetNumberOfTuples( numOfTuples );
            if ( debug ) 
               std::cout << "Scalar name " <<scalName <<std::endl;
            scalarsFromVector[ compLoop ]->SetName( scalName.c_str() );
            scalName.clear();
            for ( int tupLoop=0;tupLoop<numOfTuples; tupLoop++ )
            {
               //get the component data
               component = finalUGrid->GetPointData()->GetArray(i)
                           ->GetComponent( tupLoop, compLoop );
               scalarsFromVector[ compLoop ] ->SetComponent( tupLoop, 0, component ); 
            }
         }
         //now calculate magnitude of the vector
         scalName = name;
         scalarsFromVector[ numComponents ] = vtkFloatArray::New();
         scalName.append("_magnitude");
         scalarsFromVector[ numComponents ]->SetNumberOfComponents( 1 );
         scalarsFromVector[ numComponents ]->SetNumberOfTuples( numOfTuples );
         if ( debug ) 
            std::cout << "Scalar name " <<scalName <<std::endl;
         scalarsFromVector[ numComponents ]->SetName( scalName.c_str() );
         for ( int tupLoop=0;tupLoop<numOfTuples; tupLoop++ )
         {
            velMag = (double)(0);
            for ( int compLoop=0;compLoop<numComponents; compLoop++ )
            {
               component = finalUGrid->GetPointData()->GetArray(i)
                           ->GetComponent( tupLoop, compLoop );
               velMag = velMag + component*component;
            }
            velMag = sqrt( velMag );
            scalarsFromVector[ numComponents ]->SetComponent( tupLoop, 0, velMag );
         }
         letUsersAddParamsToField( numComponents+1, scalarsFromVector, finalUGrid->GetPointData() );
         // deleting the allocated memory
         for ( int c=0;c<numComponents+1;c++ )
         {
            scalarsFromVector[ c ]->Delete();
         }
         delete [] scalarsFromVector;
         scalarsFromVector = NULL;
      }  //end if loop
   }
   return finalUGrid;
}
