/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2009 by Iowa State University
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
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.rb END do not edit this line> ***************/
#include <ves/builder/DataLoader/EnSightTranslator.h>

#include <ves/builder/DataLoader/converter.h>

#include <vtkDataSet.h>
#include <vtkGenericEnSightReader.h>          // will open any ensight file
#include <vtkUnstructuredGrid.h>
#include <vtkCellDataToPointData.h>
#include <vtkPointData.h>
#include <vtkFloatArray.h>
#include <vtkAppendFilter.h>
#include <vtkDataArrayCollection.h>
#include <vtkDataArray.h>

#include <ves/xplorer/util/cfdVTKFileHandler.h>
#include <vtkMultiBlockDataSet.h>
#include <vtkDataObject.h>

#include <iostream>
#include <iomanip>
#include <sstream>

using namespace ves::builder::DataLoader;
using namespace ves::builder::cfdTranslatorToVTK;
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
{}
//////////////////////////////////////////////////////////////////////////
void EnSightTranslator::EnSightPreTranslateCbk::Preprocess( int argc, char** argv,
                                                            cfdTranslatorToVTK* toVTK )
{
    PreTranslateCallback::Preprocess( argc, argv, toVTK );
}
////////////////////////////////////////////////////////////////////////////////
void EnSightTranslator::EnSightTranslateCbk::Translate( vtkDataObject*& outputDataset,
                                                       cfdTranslatorToVTK* toVTK,
                                                       vtkAlgorithm*& dataReader )
{
    EnSightTranslator* EnSightToVTK =
        dynamic_cast< EnSightTranslator* >( toVTK );
    if( !EnSightToVTK )
    {
        return;
    }

    vtkGenericEnSightReader* reader = vtkGenericEnSightReader::New();
    //reader->DebugOn();
    reader->SetCaseFileName( EnSightToVTK->GetFile( 0 ).c_str() );
    reader->Update();
    vtkDataArrayCollection* tempArray = reader->GetTimeSets();
    //this must be an int because i goes negative
    if( tempArray->GetNumberOfItems() == 0 )
    {
        //necessary for ensight files that do not have timesteps
        /*int numberOfOutputs = reader->GetNumberOfOutputs();
        vtkAppendFilter* appendFilter = vtkAppendFilter::New();
        for(int i = 0; i < numberOfOutputs; ++i )
        {
           appendFilter->AddInput( reader->GetOutput( i ) );
        }
        appendFilter->Update();

        if(!outputDataset )
        {
           outputDataset = vtkUnstructuredGrid::New();
        }
        vtkDataSet* tmpDSet = vtkUnstructuredGrid::New();
        tmpDSet->DeepCopy( appendFilter->GetOutput() );
        appendFilter->Delete();

        //get the info about the data in the data set
        if(tmpDSet->GetPointData()->GetNumberOfArrays() == 0 )
        {
           vtkCellDataToPointData* dataConvertCellToPoint = vtkCellDataToPointData::New();      
           dataConvertCellToPoint->SetInput(tmpDSet);
           dataConvertCellToPoint->PassCellDataOff();
           dataConvertCellToPoint->Update();
           outputDataset->DeepCopy(dataConvertCellToPoint->GetOutput());
           dataConvertCellToPoint->Delete();
        }
        else*/
        {
            if( !outputDataset )
            {
                outputDataset = vtkMultiBlockDataSet::New();
            }
            outputDataset->ShallowCopy( reader->GetOutput() );
        }
        outputDataset->Update();
        //tmpDSet->Delete();
        //AddScalarsFromVectors( outputDataset );
    }
    else
    {
        //can still work with new datasets where there is only one timestep
        for( int i = tempArray->GetNumberOfItems() - 1; i >= 0; --i )
        {
            std::cout << "Number of Timesteps = " << tempArray->GetItem( i )->GetNumberOfTuples() << std::endl;

            //this must be an int because j goes negative
            // for( int j = tempArray->GetItem( i )->GetNumberOfTuples() - 5; j >= 0; --j )
            //  unsure why the previous line has a - 5

            // This allows the timesteps to go through the loop with positive values.
            for( int j = tempArray->GetItem( i )->GetNumberOfTuples(); j >= 0; --j )
            {
                std::cout << "Translating Timestep = " << tempArray->GetItem( i )->GetTuple1( j ) << std::endl;
                reader->SetTimeValue( tempArray->GetItem( i )->GetTuple1( j ) );
                //reader->Update();
                // used for multiple part ensight files
                /*int numberOfOutputs = reader->GetNumberOfOutputs();
                vtkAppendFilter* appendFilter = vtkAppendFilter::New();
                for(int i = 0; i < numberOfOutputs; ++i )
                {
                   appendFilter->AddInput( reader->GetOutput( i ) );
                }
                appendFilter->Update();*/

                if( !outputDataset )
                {
                    outputDataset = vtkMultiBlockDataSet::New();
                }
                //vtkDataSet* tmpDSet = vtkUnstructuredGrid::New();
                outputDataset->ShallowCopy( reader->GetOutput() );
                //appendFilter->Delete();

                //get the info about the data in the data set
                /*if ( outputDataset->GetPointData()->GetNumberOfArrays() == 0 )
                {
                   vtkCellDataToPointData* dataConvertCellToPoint = vtkCellDataToPointData::New();      
                   dataConvertCellToPoint->SetInput(outputDataset);
                   dataConvertCellToPoint->PassCellDataOff();
                   dataConvertCellToPoint->Update();
                   outputDataset->DeepCopy(dataConvertCellToPoint->GetOutput());
                   dataConvertCellToPoint->Delete();
                }
                else
                {
                   //outputDataset->DeepCopy(tmpDSet);
                }*/
                outputDataset->Update();
                //tmpDSet->Delete();
                //AddScalarsFromVectors( outputDataset );

                if( j > 0 )
                {
                    std::ostringstream strm;
                    strm << EnSightToVTK->GetOutputFileName()
                    << "_"
                    << std::setfill( '0' )
                    << std::setw( 6 )
                    << j << ".vtu";

                    ves::xplorer::util::cfdVTKFileHandler* trans = new ves::xplorer::util::cfdVTKFileHandler();
                    trans->WriteDataSet( outputDataset, strm.str() );
                    delete trans;
                    outputDataset->Delete();
                    outputDataset = 0;
                    EnSightToVTK->SetIsTransient();
                }
            }
        }
    }
    reader->Delete();
}
////////////////////////////////////////////////////////////////////////////////
/*void EnSightTranslator::EnSightTranslateCbk::AddScalarsFromVectors( vtkDataObject*& outputDataset )
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
   for(int i=0;i<numArrays; i++ ) //loop pver number of arrays
   {
      numComponents = outputDataset->GetPointData()->GetArray(i)->GetNumberOfComponents();
      if(numComponents > 1 ) //it is a vector
      {
         vtkFloatArray** scalarsFromVector;

         scalarsFromVector = new vtkFloatArray* [ numComponents+1 ];
         name = (std::string) outputDataset->GetPointData()->GetArray(i)->GetName();
         numOfTuples = outputDataset->GetPointData()->GetArray(i)->GetNumberOfTuples();
         //if ( debug ) std::cout<<" Name :" << name <<std::endl
         //                      <<" Number of Tuples :"<< numOfTuples <<std::endl;
         for(int compLoop=0;compLoop<numComponents; compLoop++ )
         {
            scalName = name;
            if(compLoop == 0 )
               scalName.append("_u");
            else if(compLoop == 1 )
               scalName.append("_v");
            else if(compLoop == 2 )
               scalName.append("_w");
            scalarsFromVector[ compLoop ] = vtkFloatArray::New();
            scalarsFromVector[ compLoop ]->SetNumberOfComponents( 1 );
            scalarsFromVector[ compLoop ]->SetNumberOfTuples( numOfTuples );
            //if ( debug )
            //   std::cout << "Scalar name " <<scalName <<std::endl;
            scalarsFromVector[ compLoop ]->SetName( scalName.c_str() );
            scalName.clear();
            for(int tupLoop=0;tupLoop<numOfTuples; tupLoop++ )
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
         for(int tupLoop=0;tupLoop<numOfTuples; tupLoop++ )
         {
            velMag = (double)(0);
            for(int compLoop=0;compLoop<numComponents; compLoop++ )
            {
               component = outputDataset->GetPointData()->GetArray(i)
                           ->GetComponent( tupLoop, compLoop );
               velMag = velMag + component*component;
            }
            velMag = sqrt( velMag );
            scalarsFromVector[ numComponents ]->SetComponent( tupLoop, 0, velMag );
         }
         //letUsersAddParamsToField( numComponents+1, scalarsFromVector, outputDataset->GetPointData() );
         for(int j=0; j<numComponents+1; j++ )
         {
            outputDataset->GetPointData()->AddArray( scalarsFromVector[ j ] );
         }
         // deleting the allocated memory
         for(int c=0;c<numComponents+1;c++ )
         {
            scalarsFromVector[ c ]->Delete();
         }
         delete [] scalarsFromVector;
         scalarsFromVector = NULL;
      }  //end if loop
   } //end for loop
} */
////////////////////////////////////////////////////////////////////////////////
void EnSightTranslator::DisplayHelp( void )
{
    std::cout << "|\tEnSight Translator Usage:" << std::endl
    << "\t -singleFile <filename_to_load> -o <output_dir> "
    << "-outFileName <output_filename> -loader ens -w file" << std::endl;
}
