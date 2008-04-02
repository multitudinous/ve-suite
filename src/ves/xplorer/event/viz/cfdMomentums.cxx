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
#include <ves/xplorer/event/viz/cfdMomentums.h>
#include <ves/xplorer/DataSet.h>
#include <ves/xplorer/environment/cfdEnum.h>    // needed for cursorType
#include <ves/xplorer/event/viz/cfdPlanes.h>

#include <ves/open/xml/Command.h>

#include <vtkLookupTable.h>
#include <vtkPolyData.h>
#include <vtkWarpVector.h>
#include <vtkPolyDataMapper.h>
#include <vtkMultiGroupPolyDataMapper.h>
#include <vtkActor.h>
#include <vtkProperty.h>
#include <vtkPointData.h>
#include <vtkAppendPolyData.h>

#include <ves/xplorer/Debug.h>

using namespace ves::xplorer;
using namespace ves::xplorer::scenegraph;

cfdMomentums::cfdMomentums( const int xyz )
{
    this->xyz = xyz;
    this->warper = vtkWarpVector::New();
}


cfdMomentums::~cfdMomentums()
{
    if( this->warper != NULL )
    {
        this->warper->Delete();
        this->warper = NULL;
    }
}


void cfdMomentums::Update( void )
{
    if( this->GetActiveDataSet()->GetPrecomputedSlices( this->xyz )->GetNumberOfPlanes() == 0 )
    {
        vprDEBUG( vesDBG, 0 )
        << "cfdMomentums: planesData == NULL so returning"
        << std::endl << vprDEBUG_FLUSH;

        return;
    }

    //make sure that there are planesData and that the cursorType is correct...
    if( this->mapper && this->cursorType == NONE )
    {
        this->GetActiveDataSet()->GetPrecomputedSlices( this->xyz )->SetAllPlanesSelected();
        this->GetActiveDataSet()->GetPrecomputedSlices( this->xyz )->ConcatenateSelectedPlanes();

        /*std::string vectorName = this->GetActiveDataSet()->
                                 GetVectorName( this->GetActiveDataSet()->GetActiveVector() );
        this->GetActiveDataSet()->GetPrecomputedSlices( this->xyz )
            ->GetPlanesData()->GetPointData()->SetActiveVectors( vectorName.c_str() );*/

        this->warper->SetInputConnection(  GetActiveDataSet()
                                         ->GetPrecomputedSlices( this->xyz )->GetPlanesData()->GetOutputPort() );
        this->warper->SetScaleFactor( this->warpedContourScale );
        this->warper->Update();//can this go???

        this->SetMapperInput( warper->GetOutputPort() );

        vtkActor* temp = vtkActor::New();
        temp->SetMapper( this->mapper );
        //temp->GetProperty()->SetSpecularPower( 100.0f );
        //temp->GetProperty()->SetSpecular( 100.0f );
        //temp->GetProperty()->SetDiffuse( 100.0f );
        //temp->GetProperty()->SetAmbient( 100.0f );

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

            vprDEBUG( vesDBG, 0 ) << "|\tMemory allocation failure : cfdMomentums "
            << std::endl << vprDEBUG_FLUSH;
        }
        this->GetActiveDataSet()->GetPrecomputedSlices( this->xyz )->GetPlanesData()->Delete();
        temp->Delete();
    }
    else
    {
        vprDEBUG( vesDBG, 0 )
            << "cfdMomentums::Update: !(mapper && cursorType == NONE)"
            << std::endl << vprDEBUG_FLUSH;

        this->updateFlag = false;
    }
}
