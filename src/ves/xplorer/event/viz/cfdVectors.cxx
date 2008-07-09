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
#include <ves/xplorer/event/viz/cfdVectors.h>
#include <ves/xplorer/DataSet.h>
#include <ves/xplorer/event/viz/cfdPlanes.h>
#include <ves/xplorer/environment/cfdEnum.h>

#include <ves/open/xml/Command.h>

#include <vtkLookupTable.h>
#include <vtkPointData.h>
#include <vtkPolyData.h>
#include <vtkAppendPolyData.h>
#include <vtkDataSet.h>
#include <vtkGlyph3D.h>
#include <vtkMaskPoints.h>
#include <vtkActor.h>
#include <vtkPolyDataMapper.h>
#include <vtkProperty.h>
#include <vtkPolyDataWriter.h>
#include <vtkPointData.h>

#include <ves/xplorer/Debug.h>

using namespace ves::xplorer;

cfdVectors::cfdVectors( const int xyz )
{
    this->xyz = xyz;
}

cfdVectors::~cfdVectors()
{
    ;
}

void cfdVectors::Update( void )
{

    cfdPlanes* precomputedPlanes =
        this->GetActiveDataSet()->GetPrecomputedSlices( this->xyz );

    if( !precomputedPlanes )
    {
        vprDEBUG( vesDBG, 0 )
        << "Dataset contains no precomputed vector planes."
        << std::endl << vprDEBUG_FLUSH;
        ves::xplorer::CommandHandler::instance()
        ->SendConductorMessage( "Dataset contains no precomputed vector planes.\n" );
        return;
    }

    if( this->mapper && this->cursorType == CUBE )
    {

        // this->planes = new cfdPlanes();
        // this->planes->SetAllPlanesSelected();
        // this->planes->ConcatenateSelectedPlanes();

        double bd[6];
        this->GetActiveDataSet()->GetBounds( bd );
        vprDEBUG( vesDBG, 0 ) << "d1:" << bd[0] << "d2:" << bd[1] << "d3:" << bd[2]
        << "d4:" << bd[3] << "d5:" << bd[4] << "d6:" << bd[5]
        << std::endl << vprDEBUG_FLUSH;

        if( this->origin[0] > ( float )bd[0] - ( this->box_size[1] - this->box_size[0] ) / 2
                && this->origin[0] < ( float )bd[1] + ( this->box_size[1] - this->box_size[0] ) / 2
                && this->origin[1] > ( float )bd[2] - ( this->box_size[3] - this->box_size[2] ) / 2
                && this->origin[1] < ( float )bd[3] + ( this->box_size[3] - this->box_size[2] ) / 2
                && this->origin[2] > ( float )bd[4] - ( this->box_size[5] - this->box_size[4] ) / 2
                && this->origin[2] < ( float )bd[5] + ( this->box_size[5] - this->box_size[4] ) / 2 )
        {
            /* if(this->box_size[0] != this->box_size[1]
               && this->box_size[2] != this->box_size[3] 
               && this->box_size[4] != this->box_size[5] )
             {
                vprDEBUG(vesDBG, 0)
                    <<"c1:"<<this->box_size[0]<<"c2:"<<this->box_size[1]
                    <<"c3:"<<this->box_size[2]<<"c4:"<<this->box_size[3]
                    <<"c5:"<<this->box_size[4]<<"c6:"<<this->box_size[5]
                    << std::endl << vprDEBUG_FLUSH;

                //--biv do we need this call ??this->filter->SetExtent( this->box_size );
                //--biv do we need this call ??this->filter->ExtentClippingOn();
             }
             else
                //--biv do we need this call ??this->filter->ExtentClippingOff();
            */
            //this->filter->Update();

            this->mapper->SetScalarRange( this->GetActiveDataSet()
                                          ->GetUserRange() );
            this->mapper->SetLookupTable( this->GetActiveDataSet()
                                          ->GetLookupTable() );
            this->mapper->Update();

            this->updateFlag = true;
        }
        else
        {
            vprDEBUG( vesDBG, 0 ) << "cfdVectors Error: cursor not in cube\n"
            << vprDEBUG_FLUSH;
            this->updateFlag = false;
        }
    }
    //make sure that there are planesData and that the cursorType is correct...
    else if( this->mapper && this->cursorType == NONE )
    {
        this->GetActiveDataSet()->GetPrecomputedSlices( this->xyz )->SetAllPlanesSelected();
        this->GetActiveDataSet()->GetPrecomputedSlices( this->xyz )->ConcatenateSelectedPlanes();

        // get every nth point from the dataSet data
        this->ptmask->SetInputConnection( this->GetActiveDataSet()
                                ->GetPrecomputedSlices( this->xyz )->GetPlanesData()->GetOutputPort() );
        this->ptmask->SetOnRatio( this->GetVectorRatioFactor() );
        this->ptmask->Update();

        // Not VTK Functions
        this->SetGlyphWithThreshold();
        this->SetGlyphAttributes();

        mapper->SetInputConnection( glyph->GetOutputPort() );
        mapper->ImmediateModeRenderingOn();
        mapper->SetScalarModeToUsePointFieldData();
        mapper->UseLookupTableScalarRangeOn();
        mapper->SelectColorArray( GetActiveDataSet()->
            GetActiveScalarName().c_str() );
        mapper->SetLookupTable( GetActiveDataSet()->GetLookupTable() );
        mapper->Update();
    }
    else
    {
        this->updateFlag = false;
    }

    vtkActor* temp = vtkActor::New();
    temp->SetMapper( this->mapper );
    temp->GetProperty()->SetSpecularPower( 20.0f );
    //geodes.push_back( new ves::xplorer::scenegraph::Geode() );
    //geodes.back()->TranslateToGeode( temp );
    //temp->Delete();
    //this->updateFlag = true;
    try
    {
        osg::ref_ptr< ves::xplorer::scenegraph::Geode > tempGeode = new ves::xplorer::scenegraph::Geode();
        tempGeode->TranslateToGeode( temp );
        geodes.push_back( tempGeode );
        this->updateFlag = true;
    }
    catch ( std::bad_alloc )
    {
        mapper->Delete();
        mapper = vtkPolyDataMapper::New();
        vprDEBUG( vesDBG, 0 ) << "|\tMemory allocation failure : cfdVectors "
        << std::endl << vprDEBUG_FLUSH;
    }
    this->GetActiveDataSet()->GetPrecomputedSlices( this->xyz )->GetPlanesData()->Delete();
    temp->Delete();
}

