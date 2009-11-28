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
#include <ves/xplorer/util/cfdVTKFileHandler.h>

#include <vtkDataSet.h>
#include <vtkGenericEnSightReader.h>          // will open any ensight file
#include <vtkUnstructuredGrid.h>
#include <vtkCellDataToPointData.h>
#include <vtkPointData.h>
#include <vtkFloatArray.h>
#include <vtkAppendFilter.h>
#include <vtkDataArrayCollection.h>
#include <vtkDataArray.h>
#include <vtkCompositeDataIterator.h>
#include <vtkCharArray.h>
#include <vtkDataArraySelection.h>
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

    std::vector< std::string > activeArrays = toVTK->GetActiveArrays();
    
    vtkGenericEnSightReader* reader = vtkGenericEnSightReader::New();
    //reader->DebugOn();
    reader->SetCaseFileName( EnSightToVTK->GetFile( 0 ).c_str() );
    reader->Update();
    
    if( !activeArrays.empty() )
    {
        //Disable user choosen arrays
        vtkDataArraySelection* arraySelector = 
            reader->GetPointDataArraySelection();
        arraySelector->DisableAllArrays();
        for( size_t i = 0; i < activeArrays.size(); ++i )
        {
            std::cout << "Passed arrays are: " 
                << activeArrays[ i ] << std::endl;
            arraySelector->EnableArray( activeArrays[ i ].c_str() );
        }
        
        arraySelector = 
            reader->GetCellDataArraySelection();
        arraySelector->DisableAllArrays();
        for( size_t i = 0; i < activeArrays.size(); ++i )
        {
            std::cout << "Passed arrays are: " 
                << activeArrays[ i ] << std::endl;
            arraySelector->EnableArray( activeArrays[ i ].c_str() );
        }
        //Need to update again before the output of the reader is read
        reader->Update();         
    }
    
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
    /*
    vtkCompositeDataSet* mgd = dynamic_cast<vtkCompositeDataSet*>( outputDataset );
    //unsigned int nGroups = mgd->GetNumberOfGroups();
    unsigned int nDatasetsInGroup = 0;
    vtkCompositeDataIterator* mgdIterator = vtkCompositeDataIterator::New();
    mgdIterator->SetDataSet( mgd );
    ///For traversal of nested multigroupdatasets
    mgdIterator->VisitOnlyLeavesOn();
    mgdIterator->GoToFirstItem();
    
    while( !mgdIterator->IsDoneWithTraversal() )
    {
        vtkDataSet* currentDataset = dynamic_cast<vtkDataSet*>( mgdIterator->GetCurrentDataObject() );
        
        vtkCharArray* tempChar = dynamic_cast< vtkCharArray* >( currentDataset->GetFieldData()->GetArray( "Name" ) );
        std::cout << "test out " << tempChar->WritePointer( 0, 0 ) << std::endl;
        
        mgdIterator->GoToNextItem();
    }
    //if( mgdIterator )
    {
        mgdIterator->Delete();
        mgdIterator = 0;
    }
    */
    reader->Delete();
}
////////////////////////////////////////////////////////////////////////////////
void EnSightTranslator::DisplayHelp( void )
{
    std::cout << "|\tEnSight Translator Usage:" << std::endl
    << "\t -singleFile <filename_to_load> -o <output_dir> "
    << "-outFileName <output_filename> -loader ens -w file" << std::endl;
}
