
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
#include <ves/xplorer/event/viz/cfdContourBase.h>
#include <ves/xplorer/DataSet.h>
#include <ves/xplorer/environment/cfdEnum.h>
#include <ves/xplorer/event/viz/cfdCuttingPlane.h>
#include <ves/xplorer/event/viz/cfdPlanes.h>
#include <ves/xplorer/Debug.h>

#include <ves/open/xml/XMLObject.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>

#include <vtkPolyData.h>
#include <vtkContourFilter.h>                // contour lines
#include <vtkBandedPolyDataContourFilter.h>  // banded contours
#include <vtkGeometryFilter.h>
#include <vtkActor.h>
#include <vtkPolyDataMapper.h>
#include <vtkProperty.h>
#include <vtkLookupTable.h>
#include <vtkDecimatePro.h>
#include <vtkTriangleFilter.h>
#include <vtkStripper.h>
#include <vtkPolyDataNormals.h>
#include <vtkCutter.h>
#include <vtkPlane.h>
#include <vtkAlgorithmOutput.h>

#include <vtkCellDataToPointData.h>
#include <vtkPCellDataToPointData.h>

#include <vtkFLUENTReader.h>
#include <vtkMultiBlockDataSet.h>

#include <vtkPassThroughFilter.h>

using namespace ves::xplorer;
using namespace ves::xplorer::scenegraph;

// this class requires that the dataset has a scalar field.
cfdContourBase::cfdContourBase()
        : 
        cfdObjects(),
        mC2p( vtkCellDataToPointData::New() ),
        deci( vtkDecimatePro::New() )
{
    cfilter = vtkContourFilter::New();              // for contourlines
    bfilter = vtkBandedPolyDataContourFilter::New();// for banded contours
    // turn clipping on to avoid unnecessary value generations with
    // vtkBandedPolyDataContourFilter::GenerateValues().
    bfilter->ClippingOn();
    tris = vtkTriangleFilter::New();
    strip = vtkStripper::New();

    mapper = vtkPolyDataMapper::New();
    //mapper->SetColorModeToMapScalars();
    //mapper->ImmediateModeRenderingOn();
    normals = vtkPolyDataNormals::New();

    warpedContourScale = 0.0f;
    contourOpacity = 1.0f;
    contourLOD = 1;
    cuttingPlane = 0;
}
////////////////////////////////////////////////////////////////////////////////
cfdContourBase::~cfdContourBase()
{
    //vprDEBUG(vesDBG,2) << "cfdContourBase destructor"
    //                      << std::endl  << vprDEBUG_FLUSH;

    //this->filter->Delete();
    //this->filter = NULL;

    mC2p->Delete();
    mC2p = 0;

    deci->Delete();
    deci = 0;
    
    if( cfilter )
    {
        this->cfilter->Delete();
        this->cfilter = 0;
    }
    if( bfilter )
    {
        this->bfilter->Delete();
        this->bfilter = 0;
    }

    if( tris )
    {
        this->tris->Delete();
        this->tris = 0;
    }

    if( strip )
    {
        this->strip->Delete();
        this->strip = 0;
    }

    if( mapper )
    {
        this->mapper->Delete();
        this->mapper = 0;
    }

    if( normals )
    {
        normals->Delete();
        normals = 0;
    }

    if( cuttingPlane )
    {
        delete cuttingPlane;
        cuttingPlane = NULL;
    }
}
////////////////////////////////////////////////////////////////////////////////
void cfdContourBase::SetMapperInput( vtkAlgorithmOutput* polydata )
{
    mC2p->SetInputConnection( polydata );
    mC2p->Update();
    
    tris->SetInputConnection( mC2p->GetOutputPort() );
    tris->Update();
    //tris->GetOutput()->ReleaseDataFlagOn();

    // decimate points is used for lod control of contours
    /*this->deci->SetInputConnection( tris->GetOutputPort() );
    this->deci->PreserveTopologyOn();
    this->deci->BoundaryVertexDeletionOff();
    deci->Update();*/
    //deci->GetOutput()->ReleaseDataFlagOn();

    this->strip->SetInputConnection( tris->GetOutputPort() );
    strip->Update();
    //strip->GetOutput()->ReleaseDataFlagOn();

    if( this->fillType == 0 )
    {
        normals->SetInputConnection( strip->GetOutputPort() );
        normals->SetFeatureAngle( 130.0f );
        //normals->GetOutput()->ReleaseDataFlagOn();
        normals->ComputePointNormalsOn();
        //normals->ComputeCellNormalsOn();
        normals->FlipNormalsOn();
        normals->Update();
    }
    else if( this->fillType == 1 ) // banded contours
    {
        // putting the decimation routines as inputs to the bfilter
        // cause the bfilter to crash while being updated
        this->bfilter->SetInputConnection( strip->GetOutputPort() );
        double range[2];
        this->GetActiveDataSet()->GetUserRange( range );
        this->bfilter->GenerateValues( 10, range[0], range[1] );
        this->bfilter->SetScalarModeToValue();
        this->bfilter->GenerateContourEdgesOn();
        bfilter->SetInputArrayToProcess( 0, 0, 0,
              vtkDataObject::FIELD_ASSOCIATION_POINTS, 
              GetActiveDataSet()->GetActiveScalarName().c_str() );
        
        //bfilter->GetOutput()->ReleaseDataFlagOn();
        normals->SetInputConnection( bfilter->GetOutputPort() );
        normals->SetFeatureAngle( 130.0f );
        //normals->GetOutput()->ReleaseDataFlagOn();
        normals->ComputePointNormalsOn();
        //normals->ComputeCellNormalsOn();
        normals->FlipNormalsOn();
    }
    else if( this->fillType == 2 ) // contourlines
    {
        this->cfilter->SetInputConnection( mC2p->GetOutputPort() );
        double range[2];
        this->GetActiveDataSet()->GetUserRange( range );
        this->cfilter->GenerateValues( 10, range[0], range[1] );
        //this->cfilter->UseScalarTreeOn();
        cfilter->SetInputArrayToProcess( 0, 0, 0,
            vtkDataObject::FIELD_ASSOCIATION_POINTS, 
            GetActiveDataSet()->GetActiveScalarName().c_str() );
        //cfilter->GetOutput()->ReleaseDataFlagOn();
        normals->SetInputConnection( cfilter->GetOutputPort() );
        normals->SetFeatureAngle( 130.0f );
        //normals->GetOutput()->ReleaseDataFlagOn();
        normals->ComputePointNormalsOn();
        //normals->ComputeCellNormalsOn();
        normals->FlipNormalsOn();
    }
    vtkAlgorithmOutput* tempPolydata = 0;
    tempPolydata = ApplyGeometryFilterNew( normals->GetOutputPort() );
    
    mapper->SetInputConnection( tempPolydata );
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
}
////////////////////////////////////////////////////////////////////////////////
void cfdContourBase::UpdateCommand()
{
    //Call base method - currently does nothing
    cfdObjects::UpdateCommand();

    //Extract the specific commands from the overall command
    ves::open::xml::DataValuePairPtr activeModelDVP = veCommand->GetDataValuePair( "Sub-Dialog Settings" );
    ves::open::xml::CommandPtr objectCommand = boost::dynamic_pointer_cast<ves::open::xml::Command>(  activeModelDVP->GetDataXMLObject() );

    //Extract the plane position
    activeModelDVP = objectCommand->GetDataValuePair( "Position" );
    double planePosition;
    activeModelDVP->GetData( planePosition );
    SetRequestedValue( planePosition );

    activeModelDVP = objectCommand->GetDataValuePair( "Plane Option" );
    if( activeModelDVP )
    {
        std::string preCalculatedFlag;
        activeModelDVP->GetData( preCalculatedFlag );

        if( preCalculatedFlag == "Use Nearest Precomputed Plane" )
        {
            SetPreCalcFlag( true );
        }
    }
    else
    {
        SetPreCalcFlag( false );
    }

    //Extract the advanced settings from the commands
    activeModelDVP = objectCommand->GetDataValuePair( "Advanced Scalar Settings" );
    objectCommand = boost::dynamic_pointer_cast<ves::open::xml::Command>( activeModelDVP->GetDataXMLObject() );

    // set the opacity
    activeModelDVP = objectCommand->GetDataValuePair( "Contour Opacity" );
    double opacity;
    activeModelDVP->GetData( opacity );
    contourOpacity = opacity * 0.01f;

    // set the warped contour scale
    activeModelDVP = objectCommand->GetDataValuePair( "Warped Contour Scale" );
    double contourScale;
    activeModelDVP->GetData( contourScale );
    double v[2];
    this->GetActiveDataSet()->GetUserRange( v );
    //double scale = contourScale;
    this->warpedContourScale = ( contourScale / 5.0 ) * 2.0f / ( float )( v[1] - v[0] );
    vprDEBUG( vesDBG, 0 ) << "Warped Contour Scale "
        << warpedContourScale << " : " << v[1] << " - " << v[0]
        << std::endl << vprDEBUG_FLUSH;
    
    // Set the lod values
    activeModelDVP = objectCommand->GetDataValuePair( "Contour LOD" );
    double contourLOD;
    activeModelDVP->GetData( contourLOD );
    double lod = contourLOD;
    double realLOD = lod * 0.01f;
    vprDEBUG( vesDBG, 0 ) << "CHANGE_CONTOUR_SETTINGS LOD Settings"
    << contourLOD << " : " << lod << " : " << realLOD
    << std::endl << vprDEBUG_FLUSH;
    this->deci->SetTargetReduction( realLOD );

    activeModelDVP = objectCommand->GetDataValuePair( "Type" );
    std::string contourType;
    activeModelDVP->GetData( contourType );

    if( contourType == "Graduated" )
    {
        SetFillType( 0 );
    }
    else if( contourType == "Banded" )
    {
        SetFillType( 1 );
    }
    else if( contourType == "Lined" )
    {
        SetFillType( 2 );
    }
}
////////////////////////////////////////////////////////////////////////////////
void cfdContourBase::SetFillType( const int type )
{
    if( -1 < type && type < 3 )
        fillType = type;
    else
    {
        vprDEBUG( vesDBG, 0 )
        << "cfdContourBase: requested fillType (" << type
        << ") is not available, using 0 instead"
        << std::endl << vprDEBUG_FLUSH;
        fillType = 0;
    }
}
////////////////////////////////////////////////////////////////////////////////
void cfdContourBase::CreatePlane( void )
{
    if( !cuttingPlane )
    {
        cuttingPlane = new cfdCuttingPlane(
                           GetActiveDataSet()->GetBounds(),
                           xyz, numSteps );
    }

    // insure that we are using correct bounds for the given data set...
    cuttingPlane->SetBounds(
        GetActiveDataSet()->GetBounds() );
    cuttingPlane->Advance( requestedValue );
    
    vtkCutter* tempCutter = vtkCutter::New();
    tempCutter->SetCutFunction( cuttingPlane->GetPlane() );
    tempCutter->SetInput( GetActiveDataSet()->GetDataSet() );
    tempCutter->Update();
    
    SetMapperInput( tempCutter->GetOutputPort(0) );
    
    delete cuttingPlane;
    cuttingPlane = NULL;

    tempCutter->Delete();
}
////////////////////////////////////////////////////////////////////////////////
