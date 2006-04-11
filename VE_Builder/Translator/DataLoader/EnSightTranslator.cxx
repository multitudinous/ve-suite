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
 * File:          $RCSfile: filename,v $
 * Date modified: $Date: date $
 * Version:       $Rev: 999999 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "VE_Builder/Translator/DataLoader/EnSightTranslator.h"

#include "VE_Builder/Translator/converter.h"

#include <vtkDataSet.h>
#include <vtkGenericEnSightReader.h>          // will open any ensight file
#include <vtkUnstructuredGrid.h>
#include <vtkCellDataToPointData.h>
#include <vtkPointData.h>
#include <vtkFloatArray.h>

#include <iostream>

using namespace VE_Builder;
////////////////////////////////////////
//Constructors                        //
////////////////////////////////////////
EnSightTranslator::EnSightTranslator()
{

   SetTranslateCallback( &ensightToVTK );
   SetPreTranslateCallback( &cmdParser );
}
/////////////////////////////////////////
EnSightTranslator::~EnSightTranslator()
{

}
//////////////////////////////////////////////////////////////////////////
void EnSightTranslator::EnSightPreTranslateCbk::Preprocess(int argc,char** argv,
                                               VE_Builder::cfdTranslatorToVTK* toVTK)
{
   PreTranslateCallback::Preprocess( argc, argv, toVTK );
}
////////////////////////////////////////////////////////////////////////////////
void EnSightTranslator::EnSightTranslateCbk::Translate( vtkDataSet*& outputDataset,
		                                     VE_Builder::cfdTranslatorToVTK* toVTK )
{
   VE_Builder::EnSightTranslator* EnSightToVTK =
              dynamic_cast< VE_Builder::EnSightTranslator* >( toVTK );
   if ( EnSightToVTK )
   {
      vtkGenericEnSightReader* reader = vtkGenericEnSightReader::New();
      //reader->DebugOn();
      reader->SetCaseFileName( EnSightToVTK->GetFile(0).c_str() );
      reader->Update();
      int numberOfOutputs = reader->GetNumberOfOutputs();
      if ( !outputDataset )
      {
         outputDataset = vtkUnstructuredGrid::New();
      }
      vtkDataSet* tmpDSet = vtkUnstructuredGrid::New();
      tmpDSet->DeepCopy( reader->GetOutput() );

      //get the info about the data in the data set
      if ( tmpDSet->GetPointData()->GetNumberOfArrays() == 0 )
      {
         //std::cout<<"Warning!!!"<<std::endl;
         //std::cout<<"No point data found!"<<std::endl;
         //std::cout<<"Attempting to convert cell data to point data."<<std::endl;

         vtkCellDataToPointData* dataConvertCellToPoint = vtkCellDataToPointData::New();      
         dataConvertCellToPoint->SetInput(tmpDSet);
         dataConvertCellToPoint->PassCellDataOff();
         dataConvertCellToPoint->Update();
         outputDataset->DeepCopy(dataConvertCellToPoint->GetOutput());
         dataConvertCellToPoint->Delete();
      }
      else
      {
         outputDataset->DeepCopy(tmpDSet);
      }
      outputDataset->Update();
      reader->Delete();
      tmpDSet->Delete();
      AddScalarsFromVectors( outputDataset );
   }
}
////////////////////////////////////////////////////////////////////////////////
void EnSightTranslator::EnSightTranslateCbk::AddScalarsFromVectors( vtkDataSet*& outputDataset )
{
   //this portion is to grab scalar data out of the vectors and rewrite it back
   //into the VTK file
   int numArrays = outputDataset->GetPointData()->GetNumberOfArrays();
   int numComponents;
   int numOfTuples;
   std::string name;
   std::string scalName;
   double component;
   double velMag;
   for ( int i=0;i<numArrays; i++ ) //loop pver number of arrays
   {
      numComponents = outputDataset->GetPointData()->GetArray(i)->GetNumberOfComponents();
      if ( numComponents > 1 ) //it is a vector
      {
         vtkFloatArray** scalarsFromVector;
         
         scalarsFromVector = new vtkFloatArray* [ numComponents+1 ];
         name = (std::string) outputDataset->GetPointData()->GetArray(i)->GetName();
         numOfTuples = outputDataset->GetPointData()->GetArray(i)->GetNumberOfTuples();
         //if ( debug ) std::cout<<" Name :" << name <<std::endl
         //                      <<" Number of Tuples :"<< numOfTuples <<std::endl;
         for ( int compLoop=0;compLoop<numComponents; compLoop++ )
         {
            scalName = name;
            if ( compLoop == 0 ) 
               scalName.append("_u");
            else if ( compLoop == 1 ) 
               scalName.append("_v");
            else if ( compLoop == 2 ) 
               scalName.append("_w");            
            scalarsFromVector[ compLoop ] = vtkFloatArray::New();
            scalarsFromVector[ compLoop ]->SetNumberOfComponents( 1 );
            scalarsFromVector[ compLoop ]->SetNumberOfTuples( numOfTuples );
            //if ( debug ) 
            //   std::cout << "Scalar name " <<scalName <<std::endl;
            scalarsFromVector[ compLoop ]->SetName( scalName.c_str() );
            scalName.clear();
            for ( int tupLoop=0;tupLoop<numOfTuples; tupLoop++ )
            {
               //get the component data
               component = outputDataset->GetPointData()->GetArray(i)
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
         //if ( debug ) 
         //   std::cout << "Scalar name " <<scalName <<std::endl;
         scalarsFromVector[ numComponents ]->SetName( scalName.c_str() );
         for ( int tupLoop=0;tupLoop<numOfTuples; tupLoop++ )
         {
            velMag = (double)(0);
            for ( int compLoop=0;compLoop<numComponents; compLoop++ )
            {
               component = outputDataset->GetPointData()->GetArray(i)
                           ->GetComponent( tupLoop, compLoop );
               velMag = velMag + component*component;
            }
            velMag = sqrt( velMag );
            scalarsFromVector[ numComponents ]->SetComponent( tupLoop, 0, velMag );
         }
         //letUsersAddParamsToField( numComponents+1, scalarsFromVector, outputDataset->GetPointData() );
         for ( int j=0; j<numComponents+1; j++ )
         {
            outputDataset->GetPointData()->AddArray( scalarsFromVector[ j ] );
         }
         // deleting the allocated memory
         for ( int c=0;c<numComponents+1;c++ )
         {
            scalarsFromVector[ c ]->Delete();
         }
         delete [] scalarsFromVector;
         scalarsFromVector = NULL;
      }  //end if loop
   } //end for loop
} 
