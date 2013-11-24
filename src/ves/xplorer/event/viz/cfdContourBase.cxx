
/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2012 by Iowa State University
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

#include <ves/xplorer/environment/cfdEnum.h>
#include <ves/xplorer/event/viz/cfdCuttingPlane.h>
#include <ves/xplorer/event/viz/cfdPlanes.h>

#include <ves/xplorer/Debug.h>
#include <ves/xplorer/Model.h>
#include <ves/xplorer/ModelHandler.h>

#include <propertystore/PropertySet.h>

#include <latticefx/utils/vtk/ExtractGeometryCallback.h>

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
#include <vtkProbeFilter.h>
#include <vtkCompositeDataProbeFilter.h>
#include <vtkPointData.h>
#include <vtkPoints.h>
#include <vtkDoubleArray.h>

#include <vtkCellDataToPointData.h>

//#include <vtkXMLUnstructuredGridWriter.h>
#include <vtkUnstructuredGrid.h>

#include <latticefx/core/vtk/ChannelDatavtkDataObject.h>
#include <latticefx/core/vtk/VTKActorRenderer.h>
#include <latticefx/core/vtk/VTKSurfaceRenderer.h>
#include <latticefx/core/vtk/VTKContourSliceRTP.h>
#include <latticefx/core/vtk/CuttingPlane.h>
#include <latticefx/core/vtk/DataSet.h>

using namespace ves::xplorer;
using namespace ves::xplorer::scenegraph;

// this class requires that the dataset has a scalar field.
cfdContourBase::cfdContourBase()
    :
    cfdObjects(),
    mapper( vtkPolyDataMapper::New() ),
    cfilter( vtkContourFilter::New() ),
    bfilter( vtkBandedPolyDataContourFilter::New() ),
    deci( vtkDecimatePro::New() ),
    tris( vtkTriangleFilter::New() ),
    strip( vtkStripper::New() ),
    normals( vtkPolyDataNormals::New() ),
    mC2p( vtkCellDataToPointData::New() ),
    cuttingPlane( 0 ),
    warpedContourScale( 0.0f ),
    contourOpacity( 1.0f ),
    contourLOD( 1 )
{
    // turn clipping on to avoid unnecessary value generations with
    // vtkBandedPolyDataContourFilter::GenerateValues().
    bfilter->ClippingOn();
}
////////////////////////////////////////////////////////////////////////////////
cfdContourBase::cfdContourBase( cfdContourBase const& src )
    :
    cfdObjects( src ),
    mapper( vtkPolyDataMapper::New() ),
    cfilter( vtkContourFilter::New() ),
    bfilter( vtkBandedPolyDataContourFilter::New() ),
    deci( vtkDecimatePro::New() ),
    tris( vtkTriangleFilter::New() ),
    strip( vtkStripper::New() ),
    normals( vtkPolyDataNormals::New() ),
    mC2p( vtkCellDataToPointData::New() ),
    cuttingPlane( 0 ),
    m_selectDataMapping( src.m_selectDataMapping ),
    fillType( src.fillType ),
    warpedContourScale( src.warpedContourScale ),
    contourOpacity( src.contourOpacity ),
    contourLOD( src.contourLOD ),
    xyz( src.xyz ),
    numSteps( src.numSteps )
{
    // turn clipping on to avoid unnecessary value generations with
    // vtkBandedPolyDataContourFilter::GenerateValues().
    bfilter->ClippingOn();
}
////////////////////////////////////////////////////////////////////////////////
cfdContourBase::~cfdContourBase()
{
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
    //mC2p->SetInputConnection( polydata );
    //mC2p->Update();

    tris->SetInputConnection( polydata );//mC2p->GetOutputPort() );
    //tris->Update();
    //tris->GetOutput()->ReleaseDataFlagOn();

    // decimate points is used for lod control of contours
    /*this->deci->SetInputConnection( tris->GetOutputPort() );
    this->deci->PreserveTopologyOn();
    this->deci->BoundaryVertexDeletionOff();
    deci->Update();*/
    //deci->GetOutput()->ReleaseDataFlagOn();

    this->strip->SetInputConnection( tris->GetOutputPort() );
    //strip->Update();
    //strip->GetOutput()->ReleaseDataFlagOn();

    if( this->fillType == 0 )
    {
        normals->SetInputConnection( strip->GetOutputPort() );
        normals->SetFeatureAngle( 130.0f );
        //normals->GetOutput()->ReleaseDataFlagOn();
        normals->ComputePointNormalsOn();
        //normals->ComputeCellNormalsOn();
        normals->FlipNormalsOn();
        //normals->Update();
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
    //mapper->Update();
}
////////////////////////////////////////////////////////////////////////////////
void cfdContourBase::UpdateCommand()
{
    UpdatePropertySet();
    return;

    //Call base method - currently does nothing
    cfdObjects::UpdateCommand();

    //Extract the specific commands from the overall command
    ves::open::xml::DataValuePairPtr activeModelDVP = veCommand->GetDataValuePair( "Sub-Dialog Settings" );
    ves::open::xml::CommandPtr objectCommand = boost::dynamic_pointer_cast<ves::open::xml::Command>( activeModelDVP->GetDataXMLObject() );

    //Extract the integration direction
    activeModelDVP = objectCommand->GetDataValuePair( "Select Data Mapping" );
    std::string dataMapping;
    activeModelDVP->GetData( dataMapping );

    vprDEBUG( vesDBG, 0 )
            << "|\tSelect scalar or volume flux for contour display"
            << std::endl << vprDEBUG_FLUSH;

    if( !dataMapping.compare( "Map Scalar Data" ) )
    {
        vprDEBUG( vesDBG, 0 ) << "|\t\tVISUALIZE SCALARS"
                              << std::endl << vprDEBUG_FLUSH;

        SelectDataMapping( 0 );
    }
    else if( !dataMapping.compare( "Map Volume Flux Data" ) )
    {
        vprDEBUG( vesDBG, 0 ) << "|\t\tVISUALIZE VOLUME FLUX"
                              << std::endl << vprDEBUG_FLUSH;

        SelectDataMapping( 1 );
    }

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
    activeModelDVP =
        objectCommand->GetDataValuePair( "Advanced Scalar Settings" );
    objectCommand =
        boost::dynamic_pointer_cast<ves::open::xml::Command>(
            activeModelDVP->GetDataXMLObject() );

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
    vprDEBUG( vesDBG, 0 ) << "|\tWarped Contour Scale "
                          << warpedContourScale << " : " << v[1] << " - " << v[0]
                          << std::endl << vprDEBUG_FLUSH;

    // Set the lod values
    activeModelDVP = objectCommand->GetDataValuePair( "Contour LOD" );
    double contourLOD;
    activeModelDVP->GetData( contourLOD );
    double lod = contourLOD;
    double realLOD = lod * 0.01f;
    vprDEBUG( vesDBG, 0 ) << "|\tCHANGE_CONTOUR_SETTINGS LOD Settings"
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

    //Extract the surface flag
    activeModelDVP = objectCommand->GetDataValuePair( "SURF Tools" );
    if( activeModelDVP )
    {
        activeModelDVP->GetData( m_surfDataset );
    }

}
////////////////////////////////////////////////////////////////////////////////
void cfdContourBase::SelectDataMapping( int value )
{
    m_selectDataMapping = value;
}
////////////////////////////////////////////////////////////////////////////////
void cfdContourBase::SetFillType( const int type )
{
    if( -1 < type && type < 3 )
    {
        fillType = type;
    }
    else
    {
        vprDEBUG( vesDBG, 0 )
                << "|\tcfdContourBase: requested fillType (" << type
                << ") is not available, using 0 instead"
                << std::endl << vprDEBUG_FLUSH;
        fillType = 0;
    }
}
////////////////////////////////////////////////////////////////////////////////
void cfdContourBase::CreatePlane()
{
    if( !cuttingPlane )
    {
        cuttingPlane = new cfdCuttingPlane(
            GetActiveDataSet()->GetBounds(),
            xyz, numSteps );
    }

    // insure that we are using correct bounds for the given data set...
    cuttingPlane->SetBounds( GetActiveDataSet()->GetBounds() );
    cuttingPlane->Advance( requestedValue );

    vtkCutter* tempCutter = vtkCutter::New();
    tempCutter->SetCutFunction( cuttingPlane->GetPlane() );
    tempCutter->SetInput( GetActiveDataSet()->GetDataSet() );
    //tempCutter->Update();

    SetMapperInput( tempCutter->GetOutputPort( 0 ) );

    // The code below computes volume flux on the specified contour plane
    // and allows for display of vector direction (in or out) by using only
    // two colors for mapping

    if( m_selectDataMapping == 1 )
    {
        vtkPolyData* pd = dynamic_cast< vtkPolyData* >( mapper->GetInput() );

        vtkPolyDataNormals* normalGen = vtkPolyDataNormals::New();
        normalGen->SetInput( pd );
        normalGen->Update();

        vtkPolyData* normalsOutputPD = ComputeVolumeFlux( normalGen->GetOutput() );

        mapper->SetInput( normalsOutputPD );

        double range[ 2 ];
        normalsOutputPD->GetPointData()->GetScalars( "VolumeFlux" )->GetRange( range );

        vtkLookupTable* lut2;
        lut2 = vtkLookupTable::New();
        lut2->SetNumberOfColors( 2 );            //default is 256
        lut2->SetHueRange( 2.0f / 3.0f, 0.0f );    //a blue-to-red scale
        lut2->SetTableRange( range );
        lut2->Build();

        mapper->SetColorModeToMapScalars();
        mapper->SetScalarRange( range );
        mapper->SetLookupTable( lut2 );
        mapper->SetScalarModeToUsePointFieldData();
        mapper->UseLookupTableScalarRangeOn();
        mapper->SelectColorArray( "VolumeFlux" );
        mapper->Update();

        normalGen->Delete();
        lut2->Delete();
    }

    delete cuttingPlane;
    cuttingPlane = NULL;

    tempCutter->Delete();
}
////////////////////////////////////////////////////////////////////////////////
void cfdContourBase::CreateArbSurface()
{
    //Need to set the active datasetname and get the position of the dataset
    Model* activeModel = ModelHandler::instance()->GetActiveModel();
    // set the dataset as the appropriate dastaset type
    // (and the active dataset as well)
    lfx::core::vtk::DataSetPtr surfDataset =
        activeModel->GetCfdDataSet(
            activeModel->GetIndexOfDataSet( m_surfDataset ) );
    vtkPolyData* pd = surfDataset->GetPolyData();

    if( !pd )
    {
        std::cerr << "ERROR: Activate a polydata file to use this function "
                  << m_surfDataset << std::endl;
        return;
    }

    lfx::vtk_utils::ExtractGeometryCallback* extractGeomCbk =
        new lfx::vtk_utils::ExtractGeometryCallback();
    lfx::vtk_utils::DataObjectHandler handler;
    handler.SetDatasetOperatorCallback( extractGeomCbk );
    extractGeomCbk->SetPolyDataSurface( pd );
    handler.OperateOnAllDatasetsInObject( GetActiveDataSet()->GetDataSet() );

    vtkCompositeDataProbeFilter* surfProbe = vtkCompositeDataProbeFilter::New();
    surfProbe->SetInput( pd );
    //surfProbe->SetSourceConnection( extractGeomCbk->GetDataset() );
    surfProbe->SetSource( extractGeomCbk->GetDataset() );

    if( m_selectDataMapping != 1 )
    {
        normals->SetInputConnection( surfProbe->GetOutputPort() );
        normals->NonManifoldTraversalOn();
        normals->AutoOrientNormalsOn();
        normals->ConsistencyOn();
        normals->SplittingOn();

        mapper->SetColorModeToMapScalars();
        mapper->SetInputConnection( normals->GetOutputPort() );

        mapper->SetScalarModeToUsePointFieldData();
        mapper->UseLookupTableScalarRangeOn();
        mapper->SelectColorArray( GetActiveDataSet()->
                                  GetActiveScalarName().c_str() );
        mapper->SetLookupTable( GetActiveDataSet()->GetLookupTable() );
        mapper->Update();
    }
    else
    {
        // The code below computes volume flux on the specified contour plane
        normals->SetInputConnection( surfProbe->GetOutputPort() );
        normals->Update();

        vtkPolyData* normalsOutputPD =
            ComputeVolumeFlux( normals->GetOutput() );
        if( normalsOutputPD )
        {
            mapper->SetInput( normalsOutputPD );

            double range[ 2 ];
            normalsOutputPD->GetPointData()->
            GetScalars( "VolumeFlux" )->GetRange( range );

            vtkLookupTable* lut1 = vtkLookupTable::New();
            lut1->SetNumberOfColors( 2 );            //default is 256
            lut1->SetHueRange( 2.0f / 3.0f, 0.0f );    //a blue-to-red scale
            lut1->SetTableRange( range );
            lut1->Build();

            mapper->SetColorModeToMapScalars();
            mapper->SetScalarRange( range );
            mapper->SetLookupTable( lut1 );
            mapper->SetScalarModeToUsePointFieldData();
            mapper->UseLookupTableScalarRangeOn();
            mapper->SelectColorArray( "VolumeFlux" );
            mapper->Update();

            lut1->Delete();
        }
    }
    surfProbe->Delete();
    delete extractGeomCbk;
    extractGeomCbk = 0;
}
////////////////////////////////////////////////////////////////////////////////
void cfdContourBase::UpdatePropertySet()
{
    std::string dataMapping = boost::any_cast<std::string >( m_propertySet->GetPropertyValue( "DataMapping" ) );

    vprDEBUG( vesDBG, 0 )
            << "|\tSelect scalar or volume flux for contour display: "
            << dataMapping << std::endl << vprDEBUG_FLUSH;

    if( !dataMapping.compare( "Map Scalar Data" ) )
    {
        vprDEBUG( vesDBG, 0 ) << "|\t\tVisualize Scalars"
                              << std::endl << vprDEBUG_FLUSH;

        SelectDataMapping( 0 );
    }
    else if( !dataMapping.compare( "Map Volume Flux Data" ) )
    {
        vprDEBUG( vesDBG, 0 ) << "|\t\tVisualize Volume Flux"
                              << std::endl << vprDEBUG_FLUSH;

        SelectDataMapping( 1 );
    }

    //Extract the plane position
    double planePosition = boost::any_cast<double>( m_propertySet->GetPropertyValue( "PlaneLocation" ) );
    SetRequestedValue( planePosition );

    // Use Nearest or Cycle Precomputed
    std::string planeOption;
    if( boost::any_cast<bool>( m_propertySet->GetPropertyValue( "Mode_UseNearestPrecomputedPlane" ) ) )
    {
        planeOption = "Use Nearest Precomputed Plane";
    }
    else if( boost::any_cast<bool>( m_propertySet->GetPropertyValue( "Mode_CyclePrecomputedSurfaces" ) ) )
    {
        planeOption = "Cycle Precomputed Surfaces";
    }

    if( planeOption == "Use Nearest Precomputed Plane" )
    {
        SetPreCalcFlag( true );
    }
    else
    {
        SetPreCalcFlag( false );
    }

    // set the opacity
    double opacity = boost::any_cast<double>( m_propertySet->GetPropertyValue( "Advanced_Opacity" ) );
    contourOpacity = opacity * 0.01f;

    // set the warped contour scale
    double contourScale = boost::any_cast<double>( m_propertySet->GetPropertyValue( "Advanced_WarpedContourScale" ) );
    double v[2];
    GetActiveDataSet()->GetUserRange( v );
    //double scale = contourScale;
    warpedContourScale = ( contourScale / 5.0 ) * 2.0f / ( float )( v[1] - v[0] );
    vprDEBUG( vesDBG, 0 ) << "|\tWarped Contour Scale "
                          << warpedContourScale << " : " << v[1] << " - " << v[0]
                          << std::endl << vprDEBUG_FLUSH;

    // Set the lod values
    double lod = boost::any_cast<double>( m_propertySet->GetPropertyValue( "Advanced_ContourLOD" ) );
    double realLOD = lod * 0.01f;
    vprDEBUG( vesDBG, 0 ) << "|\tCHANGE_CONTOUR_SETTINGS LOD Settings: "
                          << lod << " : " << realLOD
                          << std::endl << vprDEBUG_FLUSH;
    this->deci->SetTargetReduction( realLOD );

    std::string contourType =
        boost::any_cast<std::string >( m_propertySet->GetPropertyValue( "Advanced_ContourType" ) );
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

    bool contourGreyscale = boost::any_cast<bool>( m_propertySet->GetPropertyValue( "Advanced_Greyscale" ) );
    GetActiveDataSet()->SetGreyscaleFlag( contourGreyscale );
    vprDEBUG( vesDBG, 0 ) << "|\tContour Greyscale set to : "
                          << contourGreyscale
                          << std::endl << vprDEBUG_FLUSH;

    //Extract the surface flag
    /*activeModelDVP = objectCommand->GetDataValuePair( "SURF Tools" );
     if( activeModelDVP )
     {
     activeModelDVP->GetData( m_surfDataset );
     }*/

    if( m_propertySet->PropertyExists( "UseGPUTools" ) )
    {
        ;//unsigned int checkBox = boost::any_cast<bool>( set.GetPropertyValue( "UseGPUTools" ) );
    }
}
////////////////////////////////////////////////////////////////////////////////
void cfdContourBase::CreateLFXPlane()
{
    m_dsp = lfx::core::DataSetPtr( new lfx::core::DataSet() );
    
    //1st Step
    lfx::core::vtk::ChannelDatavtkDataObjectPtr dobjPtr( new lfx::core::vtk::ChannelDatavtkDataObject( GetActiveDataSet()->GetDataSet(), "vtkDataObject" ) );
    m_dsp->addChannel( dobjPtr );
    
    lfx::core::vtk::VTKContourSliceRTPPtr contourRTP( new lfx::core::vtk::VTKContourSliceRTP() );
    if( xyz == 0 )
    {
        contourRTP->SetPlaneDirection( lfx::core::vtk::CuttingPlane::X_PLANE );
    }
    else if( xyz == 1 )
    {
        contourRTP->SetPlaneDirection( lfx::core::vtk::CuttingPlane::Y_PLANE );
    }
    else if( xyz == 2 )
    {
        contourRTP->SetPlaneDirection( lfx::core::vtk::CuttingPlane::Z_PLANE );
    }
    contourRTP->SetRequestedValue( requestedValue );
    contourRTP->addInput( "vtkDataObject" );
    m_dsp->addOperation( contourRTP );

    //Try the vtkActor renderer
    lfx::core::vtk::VTKSurfaceRendererPtr renderOp( new lfx::core::vtk::VTKSurfaceRenderer() );
    renderOp->SetActiveVector( GetActiveDataSet()->GetActiveVectorName() );
    renderOp->SetActiveScalar( GetActiveDataSet()->GetActiveScalarName() );
    renderOp->addInput( "vtkPolyDataMapper" );
    renderOp->addInput( "vtkDataObject" );
    m_dsp->setRenderer( renderOp );
    m_dsp->setDirty();
    //Now force an update of the lfx pipeline
    bool success = m_dsp->updateAll();
    
    if( !success )
    {
        std::cout << "Some sort of problem with lfx " << std::endl;
    }

	Model* activeModel = ModelHandler::instance()->GetActiveModel();
	activeModel->SetVtkRenderSet( "con", m_dsp );
}
////////////////////////////////////////////////////////////////////////////////
