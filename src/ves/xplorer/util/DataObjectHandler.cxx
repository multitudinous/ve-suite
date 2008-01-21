/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2008 by Iowa State University
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
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include <ves/xplorer/util/DataObjectHandler.h>

#include <vtkDataSet.h>
#include <vtkDataObject.h>
#include <vtkUnstructuredGrid.h>
#include <vtkMultiGroupDataSet.h>
#include <vtkMultiGroupDataIterator.h>
#include <vtkCellDataToPointData.h>
#include <vtkCellData.h>
#include <vtkPointData.h>
#include <iostream>
#include <vtkCompositeDataPipeline.h>
using namespace ves::xplorer::util;

//////////////////////////////////////
DataObjectHandler::DataObjectHandler()
        : m_numberOfPointDataArrays( 0 ),
        m_numberOfCellDataArrays( 0 )

{
    m_datasetOperator = 0;
}
///////////////////////////////////////
DataObjectHandler::~DataObjectHandler()
{}
///////////////////////////////////////////////////////////////////////////////////////////////////////
void DataObjectHandler::OperateOnAllDatasetsInObject( vtkDataObject* dataObject )
{
    /*if(m_datasetOperator)
    {
        std::cout<<"No operator defined for datasets!!!"<<std::endl;
        std::cout<<"DataObjectHandler::operateOnAllDatasetsInObject."<<std::endl;
     return;
    }*/
    unsigned int numDatasets = 0;
    vtkDataSet* currentDataset = 0;
    if( dataObject->IsA( "vtkMultiGroupDataSet" ) )
    {
        try
        {
            vtkMultiGroupDataSet* mgd = dynamic_cast<vtkMultiGroupDataSet*>( dataObject );
            unsigned int nGroups = mgd->GetNumberOfGroups();
            unsigned int nDatasetsInGroup = 0;
            vtkMultiGroupDataIterator* mgdIterator = vtkMultiGroupDataIterator::New();
            mgdIterator->SetDataSet( mgd );
            ///For traversal of nested multigroupdatasets
            mgdIterator->VisitOnlyLeavesOn();
            mgdIterator->GoToFirstItem();

            while( !mgdIterator->IsDoneWithTraversal() )
            {
                currentDataset = dynamic_cast<vtkDataSet*>( mgdIterator->GetCurrentDataObject() );
                _convertCellDataToPointData( currentDataset );
                if( m_datasetOperator )
                {
                    m_datasetOperator->OperateOnDataset( currentDataset );
                }
                mgdIterator->GoToNextItem();
            }
            if( mgdIterator )
            {
                mgdIterator->Delete();
                mgdIterator = 0;
            }
        }
        catch ( ... )
        {
            std::cout << "Invalid Dataset: " << dataObject->GetClassName() << std::endl;
        }
    }
    else //Assume this is a regular vtkdataset
    {
        currentDataset = dynamic_cast<vtkDataSet*>( dataObject );
        _convertCellDataToPointData( currentDataset );
        if( m_datasetOperator )
        {
            m_datasetOperator->OperateOnDataset( currentDataset );
        }
    }

}
////////////////////////////////////////////////////////////////////////
void DataObjectHandler::_convertCellDataToPointData( vtkDataSet* dataSet )
{
    if( dataSet->GetPointData()->GetNumberOfArrays() > m_numberOfPointDataArrays )
    {
        m_numberOfPointDataArrays = dataSet->GetPointData()
                                    ->GetNumberOfArrays();
    }
    if( dataSet->GetCellData()->GetNumberOfArrays() > m_numberOfCellDataArrays )
    {
        m_numberOfCellDataArrays = dataSet->GetCellData()->GetNumberOfArrays();
    }
    if( m_numberOfCellDataArrays > 0 && m_numberOfPointDataArrays  == 0 )
    {
        std::cout << "|\tThe dataset has no point data -- "
        << "will try to convert cell data to point data" << std::endl;

        vtkCellDataToPointData * converter = vtkCellDataToPointData::New();
        converter->SetInput( 0, dataSet );
        converter->PassCellDataOff();
        converter->Update();

        ///Why do we need to do this only for unstructured grids?
        if( dataSet->GetDataObjectType() == VTK_UNSTRUCTURED_GRID )
        {
            dataSet->DeepCopy( converter->GetUnstructuredGridOutput() );
            converter->Delete();
        }
        else
        {
            converter->Delete();
            std::cout << "\nAttempt failed: can not currently handle "
            << "this type of data\n" << std::endl;
            exit( 1 );
        }
        if( dataSet->GetPointData()->GetNumberOfArrays() > m_numberOfPointDataArrays )
        {
            m_numberOfPointDataArrays = dataSet->GetPointData()
                                        ->GetNumberOfArrays();
        }
    }
    return;
}
///////////////////////////////////////////////////////////////////////
unsigned int DataObjectHandler::GetNumberOfDataArrays( bool isPointData )
{
    return ( isPointData ) ? m_numberOfPointDataArrays : m_numberOfCellDataArrays;
}
/////////////////////////////////////////////////////////////////////////////////////////////
void DataObjectHandler::SetDatasetOperatorCallback( DatasetOperatorCallback* dsoCbk )
{
    m_datasetOperator = dsoCbk;
}
