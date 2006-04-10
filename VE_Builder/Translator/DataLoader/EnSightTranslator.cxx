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
#include <vtkDataSet.h>
#include <vtkGenericEnSightReader.h>          // will open any ensight file
#include <vtkUnstructuredGrid.h>
#include <vtkCellDataToPointData.h>
#include <vtkPointData.h>

#include <iostream>

using namespace VE_Builder;
////////////////////////////////////////
//Constructors                        //
////////////////////////////////////////
FluentTranslator::FluentTranslator()
{

   SetTranslateCallback( &fluentToVTK );
   SetPreTranslateCallback( &cmdParser );
}
/////////////////////////////////////////
FluentTranslator::~FluentTranslator()
{

}
//////////////////////////////////////////////////////////////////////////
void FluentTranslator::FluentPreTranslateCbk::Preprocess(int argc,char** argv,
                                               VE_Builder::cfdTranslatorToVTK* toVTK)
{
   PreTranslateCallback::Preprocess( argc, argv, toVTK );

   if(toVTK)
   {
      std::string singleFile;
      if ( toVTK->_extractOptionFromCmdLine(argc,argv,std::string("-singleFile"),singleFile) )
      {
         toVTK->AddFoundFile(singleFile);
      }
   }
}
////////////////////////////////////////////////////////////////////////////////
void FluentTranslator::FluentTranslateCbk::Translate( vtkDataSet*& outputDataset,
		                                     VE_Builder::cfdTranslatorToVTK* toVTK )
{
   VE_Builder::FluentTranslator* FluentToVTK =
              dynamic_cast< VE_Builder::FluentTranslator* >( toVTK );
   if ( FluentToVTK )
   {
	   vtkFLUENTReader* reader = vtkFLUENTReader::New();
	   reader->SetFileName( FluentToVTK->GetFile(0).c_str() );
	   reader->Update();

      if ( !outputDataset )
      {
         outputDataset = vtkUnstructuredGrid::New();
      }
      vtkDataSet* tmpDSet = vtkUnstructuredGrid::New();
      tmpDSet->DeepCopy( reader->GetOutput() );

      //get the info about the data in the data set
      if ( tmpDSet->GetPointData()->GetNumberOfArrays() > 0 )
      {
         std::cout<<"Warning!!!"<<std::endl;
         std::cout<<"No point data found!"<<std::endl;
         std::cout<<"Attempting to convert cell data to point data."<<std::endl;

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
   }


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
 
