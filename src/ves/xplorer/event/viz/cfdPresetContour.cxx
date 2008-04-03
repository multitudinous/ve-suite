/*************** <auto-copyright.rb BEGIN do not edit this line> **************
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
 *************** <auto-copyright.rb END do not edit this line> ***************/
#include <ves/xplorer/CommandHandler.h>
#include <ves/xplorer/event/viz/cfdPresetContour.h>

#include <ves/xplorer/event/viz/cfdCuttingPlane.h>
#include <ves/xplorer/DataSet.h>
#include <ves/xplorer/event/viz/cfdPlanes.h>
#include <ves/xplorer/util/readWriteVtkThings.h>

#include <ves/open/xml/Command.h>

#include <vtkLookupTable.h>
#include <vtkPlane.h>
#include <vtkDataSet.h>
#include <vtkCutter.h>
#include <vtkPolyDataMapper.h>
#include <vtkPolyData.h>
#include <vtkCellDataToPointData.h>
#include <vtkActor.h>
#include <vtkProperty.h>

#include <ves/xplorer/Debug.h>

using namespace ves::xplorer;
using namespace ves::xplorer::scenegraph;

cfdPresetContour::cfdPresetContour( const int xyz, const int numSteps )
        : cfdContourBase()
{
    this->xyz = xyz;
    this->numSteps = numSteps;
    cuttingPlane = 0;
}

cfdPresetContour::~cfdPresetContour()
{}

void cfdPresetContour::Update( void )
{
    vprDEBUG( vesDBG, 1 ) << "cfdPresetContour::Update, usePreCalcData = "
        << this->usePreCalcData << std::endl << vprDEBUG_FLUSH;

    if( this->usePreCalcData )
    {
        cfdPlanes* precomputedPlanes =
            this->GetActiveDataSet()->GetPrecomputedSlices( this->xyz );
        if( !precomputedPlanes )
        {
            vprDEBUG( vesDBG, 0 )
            << "Dataset contains no precomputed contour planes."
            << std::endl << vprDEBUG_FLUSH;
            ves::xplorer::CommandHandler::instance()
            ->SendConductorMessage( "Dataset contains no precomputed contour planes.\n" );
            return;
        }

        vtkPolyData* preCalcData = precomputedPlanes
                                    ->GetClosestPlane( this->requestedValue );

        if( preCalcData == NULL )
        {
            vprDEBUG( vesDBG, 0 ) << "cfdPresetContour: no precalculated data"
            << std::endl << vprDEBUG_FLUSH;

            this->updateFlag = false;
            return;
        }

        //Just need a filter to be able to pass the data into the SetMapper 
        //function. May need to create another function so that this filter
        //is not necessary.
        vtkCellDataToPointData* tempPipe = vtkCellDataToPointData::New();
        tempPipe->SetInput( preCalcData );
        this->SetMapperInput( tempPipe->GetOutputPort() );
        
        tempPipe->Delete();
    }
    else
    {
        if( cuttingPlane )
        {
            delete this->cuttingPlane;
            this->cuttingPlane = NULL;
        }

        CreatePlane();
    }

    vtkActor* temp = vtkActor::New();
    temp->SetMapper( this->mapper );
    temp->GetProperty()->SetSpecularPower( 20.0f );
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

        vprDEBUG( vesDBG, 0 ) << "|\tMemory allocation failure : cfdPresetContour"
        << std::endl << vprDEBUG_FLUSH;
    }
    temp->Delete();
}
