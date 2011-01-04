/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2011 by Iowa State University
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
#include <ves/xplorer/communication/CommunicationHandler.h>
#include <ves/xplorer/event/viz/cfdContours.h>

#include <ves/xplorer/DataSet.h>
#include <ves/xplorer/event/viz/cfdPlanes.h>
#include <ves/xplorer/environment/cfdEnum.h>    // needed for cursorType

#include <ves/open/xml/Command.h>

#include <vtkLookupTable.h>
#include <vtkPolyData.h>
#include <vtkPolyDataMapper.h>
#include <vtkActor.h>
#include <vtkProperty.h>
#include <vtkPointData.h>
#include <vtkAppendPolyData.h>
#include <vtkCellDataToPointData.h>

#include <ves/xplorer/Debug.h>

using namespace ves::xplorer;
using namespace ves::xplorer::scenegraph;

cfdContours::cfdContours( const int xyz )
{
    this->xyz = xyz;
}

cfdContours::~cfdContours()
{}

void cfdContours::Update( void )
{
    vprDEBUG( vesDBG, 1 ) << "cfdContours::Update"
    << std::endl << vprDEBUG_FLUSH;

    cfdPlanes* precomputedPlanes =
        this->GetActiveDataSet()->GetPrecomputedSlices( this->xyz );
    if( !precomputedPlanes )
    {
        vprDEBUG( vesDBG, 0 )
        << "Dataset contains no precomputed contour planes."
        << std::endl << vprDEBUG_FLUSH;
        ves::xplorer::communication::CommunicationHandler::instance()
        ->SendConductorMessage( "Dataset contains no precomputed contour planes.\n" );
        return;
    }

    //make sure that there are planesData and that the cursorType is correct...
    if( this->mapper && this->cursorType == NONE )
    {
        this->GetActiveDataSet()->GetPrecomputedSlices( this->xyz )->SetAllPlanesSelected();
        this->GetActiveDataSet()->GetPrecomputedSlices( this->xyz )->ConcatenateSelectedPlanes();
        //Just need a filter to be able to pass the data into the SetMapper 
        //function. May need to create another function so that this filter
        //is not necessary.
        /*vtkCellDataToPointData* tempPipe = vtkCellDataToPointData::New();
        tempPipe->SetInput( GetActiveDataSet()->GetPrecomputedSlices( this->xyz )->GetPlanesData()->GetOutput() );
        tempPipe->Update();
        SetMapperInput( tempPipe->GetOutputPort() );
        
        tempPipe->Delete();*/
        
        mapper->SetInputConnection( GetActiveDataSet()->GetPrecomputedSlices( this->xyz )->GetPlanesData()->GetOutputPort() );
        //mapper->SetScalarModeToDefault();
        //mapper->SetColorModeToDefault();
        //mapper->SetColorModeToMapScalars();
        //mapper->InterpolateScalarsBeforeMappingOff();
        mapper->SetScalarModeToUsePointFieldData();
        mapper->UseLookupTableScalarRangeOn();
        mapper->SelectColorArray( GetActiveDataSet()->
            GetActiveScalarName().c_str() );
        mapper->SetLookupTable( GetActiveDataSet()->GetLookupTable() );
        mapper->Update();
        
        this->mapper->Update();
        vtkActor* temp = vtkActor::New();
        temp->SetMapper( this->mapper );
        temp->GetProperty()->SetSpecularPower( 20.0f );
        temp->GetProperty()->SetOpacity( contourOpacity );

        try
        {
            osg::ref_ptr< ves::xplorer::scenegraph::Geode > tempGeode = new ves::xplorer::scenegraph::Geode();
            tempGeode->TranslateToGeode( temp );
            geodes.push_back( tempGeode.get() );
            this->updateFlag = true;
        }
        catch ( std::bad_alloc )
        {
            mapper->Delete();
            mapper = vtkPolyDataMapper::New();

            vprDEBUG( vesDBG, 0 ) << "|\tMemory allocation failure : cfdContours"
            << std::endl << vprDEBUG_FLUSH;
        }
        this->GetActiveDataSet()->GetPrecomputedSlices( this->xyz )->GetPlanesData()->Delete();
        temp->Delete();
    }
    else
    {
        vprDEBUG( vesDBG, 0 )
        << "cfdContours: !(mapper && cursorType == NONE)"
        << std::endl << vprDEBUG_FLUSH;

        this->updateFlag = false;
    }
}

