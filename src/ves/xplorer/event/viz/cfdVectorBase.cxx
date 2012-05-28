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
#include <ves/xplorer/event/viz/cfdVectorBase.h>
#include <ves/xplorer/DataSet.h>
#include <ves/xplorer/environment/cfdEnum.h>
#include <ves/xplorer/Model.h>
#include <ves/xplorer/ModelHandler.h>

#include <ves/xplorer/util/ExtractGeometryCallback.h>

#include <ves/open/xml/XMLObject.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>

#include <ves/xplorer/data/PropertySet.h>

#include <cmath>

#include <ves/xplorer/Debug.h>

#include <vtkPolyData.h>
#include <vtkGlyph3D.h>
#include <vtkMaskPoints.h>
#include <vtkThresholdPoints.h>
#include <vtkActor.h>
#include <vtkPolyDataMapper.h>
#include <vtkProperty.h>
#include <vtkTriangleFilter.h>
#include <vtkStripper.h>
#include <vtkProbeFilter.h>
#include <vtkCompositeDataProbeFilter.h>
#include <vtkPolyDataNormals.h>
#include <vtkLookupTable.h>
#include <vtkPointData.h>

using namespace ves::xplorer;

////////////////////////////////////////////////////////////////////////////////
// this class requires that the dataset has a vector field.
cfdVectorBase::cfdVectorBase()
    :
    cfdObjects(),
    glyph( vtkGlyph3D::New() ),
    mapper( vtkPolyDataMapper::New() ),
    ptmask( vtkMaskPoints::New() ),
    tris( vtkTriangleFilter::New() ),
    strip( vtkStripper::New() ),
    tfilter( vtkThresholdPoints::New() )
{
    this->ptmask->RandomModeOn();

    this->mapper->SetColorModeToMapScalars();
    mapper->ImmediateModeRenderingOn();

    _vectorScale = 1.0;
    _vectorThreshHoldMinPercentage = 0;
    _vectorThreshHoldMaxPercentage = 100;
    _vectorThreshHoldValues[ 0 ] = 0.0;
    _vectorThreshHoldValues[ 1 ] = 100.0;
    _scaleByVector = 0;
    _vectorRatioFactor = 1;
}
////////////////////////////////////////////////////////////////////////////////
cfdVectorBase::cfdVectorBase( cfdVectorBase const& src )
    :
    cfdObjects( src ),
    glyph( vtkGlyph3D::New() ),
    mapper( vtkPolyDataMapper::New() ),
    ptmask( vtkMaskPoints::New() ),
    tris( vtkTriangleFilter::New() ),
    strip( vtkStripper::New() ),
    tfilter( vtkThresholdPoints::New() )
{
    ptmask->RandomModeOn();

    mapper->SetColorModeToMapScalars();
    mapper->ImmediateModeRenderingOn();

    _vectorScale = 1.0;
    _vectorThreshHoldMinPercentage = 0;
    _vectorThreshHoldMaxPercentage = 100;
    _vectorThreshHoldValues[ 0 ] = 0.0;
    _vectorThreshHoldValues[ 1 ] = 100.0;
    _scaleByVector = 0;
    _vectorRatioFactor = 1;
}
////////////////////////////////////////////////////////////////////////////////
cfdVectorBase::~cfdVectorBase()
{
    tfilter->Delete();
    tfilter = 0;

    this->ptmask->Delete();
    this->ptmask = 0;

    this->glyph->Delete();
    this->glyph = 0;

    this->tris->Delete();
    this->tris = 0;

    this->strip->Delete();
    this->strip = 0;


    this->mapper->Delete();
    this->mapper = 0;
}
////////////////////////////////////////////////////////////////////////////////
void cfdVectorBase::UpdateCommand()
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

        m_selectDataMapping = 0;
    }
    else if( !dataMapping.compare( "Map Volume Flux Data" ) )
    {
        vprDEBUG( vesDBG, 0 ) << "|\t\tVISUALIZE VOLUME FLUX"
                              << std::endl << vprDEBUG_FLUSH;

        m_selectDataMapping = 1;
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
    activeModelDVP = objectCommand->GetDataValuePair( "Advanced Vector Settings" );
    objectCommand = boost::dynamic_pointer_cast<ves::open::xml::Command>( activeModelDVP->GetDataXMLObject() );

    activeModelDVP = objectCommand->GetDataValuePair( "Vector Threshold" );
    std::vector< double > threshHoldValues;
    activeModelDVP->GetData( threshHoldValues );
    SetThreshHoldPercentages( static_cast< int >( threshHoldValues.at( 0 ) ),
                              static_cast< int >( threshHoldValues.at( 1 ) ) );
    UpdateThreshHoldValues();

    activeModelDVP = objectCommand->GetDataValuePair( "Vector Scale" );
    double vectorScale;
    activeModelDVP->GetData( vectorScale );
    SetVectorScale( static_cast< float >( vectorScale ) );

    activeModelDVP = objectCommand->GetDataValuePair( "Vector Ratio" );
    double vectorRatio;
    activeModelDVP->GetData( vectorRatio );
    SetVectorRatioFactor( static_cast< int >( vectorRatio ) );

    activeModelDVP = objectCommand->GetDataValuePair( "Scale By Magnitude" );
    unsigned int scaleByMagnitude;
    activeModelDVP->GetData( scaleByMagnitude );
    SetScaleByVectorFlag( static_cast< int >( scaleByMagnitude ) );

    activeModelDVP = objectCommand->GetDataValuePair( "GPU Tools" );
    if( activeModelDVP )
    {
        unsigned int gpuFlag;
        activeModelDVP->GetData( gpuFlag );
        m_gpuTools = gpuFlag;
    }
    else
    {
        m_gpuTools = false;
    }

    //Extract the surface flag
    activeModelDVP = objectCommand->GetDataValuePair( "SURF Tools" );
    if( activeModelDVP )
    {
        activeModelDVP->GetData( m_surfDataset );
    }
}
////////////////////////////////////////////////////////////////////////////////
void cfdVectorBase::SetVectorScale( float x )
{
    _vectorScale = x;
}
////////////////////////////////////////////////////////////////////////////////
float cfdVectorBase::GetVectorScale()
{
    return _vectorScale;
}
////////////////////////////////////////////////////////////////////////////////
void cfdVectorBase::SetGlyphWithThreshold()
{
    vprDEBUG( vesDBG, 1 )
            << "|\tcfdVectorBase::SetGlyphWithThreshold vectorThreshHoldValues : "
            << _vectorThreshHoldValues[ 0 ] << " : "
            << _vectorThreshHoldValues[ 1 ] << std::endl << vprDEBUG_FLUSH;

    double currentScalarRange[ 2 ];
    this->GetActiveDataSet()->GetRange( currentScalarRange );

    if( _vectorThreshHoldValues[ 0 ] > currentScalarRange[ 0 ] &&
            _vectorThreshHoldValues[ 1 ] < currentScalarRange[ 1 ] )
    {
        vprDEBUG( vesDBG, 1 ) << "|\tcfdVectorBase: ThresholdBetween"
                              << std::endl << vprDEBUG_FLUSH;
        tfilter->SetInputConnection( this->ptmask->GetOutputPort() );
        tfilter->ThresholdBetween( _vectorThreshHoldValues[ 0 ],
                                   _vectorThreshHoldValues[ 1 ] );
        tfilter->SetInputArrayToProcess( 0, 0, 0,
                                         vtkDataObject::FIELD_ASSOCIATION_POINTS,
                                         GetActiveDataSet()->GetActiveScalarName().c_str() );

        this->tris->SetInputConnection( this->tfilter->GetOutputPort() );
        this->strip->SetInputConnection( this->tris->GetOutputPort() );
        this->glyph->SetInputConnection( this->strip->GetOutputPort() );
    }
    else if( _vectorThreshHoldValues[ 0 ] > currentScalarRange[ 0 ] )
    {
        vprDEBUG( vesDBG, 1 ) << "|\tcfdVectorBase: ThresholdByUpper"
                              << std::endl << vprDEBUG_FLUSH;
        tfilter->SetInputConnection( this->ptmask->GetOutputPort() );
        tfilter->ThresholdByUpper( _vectorThreshHoldValues[ 0 ] );
        tfilter->SetInputArrayToProcess( 0, 0, 0,
                                         vtkDataObject::FIELD_ASSOCIATION_POINTS,
                                         GetActiveDataSet()->GetActiveScalarName().c_str() );
        this->tris->SetInputConnection( this->tfilter->GetOutputPort() );
        this->strip->SetInputConnection( this->tris->GetOutputPort() );
        this->glyph->SetInputConnection( this->strip->GetOutputPort() );
    }
    else if( _vectorThreshHoldValues[ 1 ] < currentScalarRange[ 1 ] )
    {
        vprDEBUG( vesDBG, 1 ) << "|\tcfdVectorBase: ThresholdByLower"
                              << std::endl << vprDEBUG_FLUSH;
        tfilter->SetInputConnection( this->ptmask->GetOutputPort() );
        tfilter->ThresholdByLower( _vectorThreshHoldValues[ 1 ] );
        tfilter->SetInputArrayToProcess( 0, 0, 0,
                                         vtkDataObject::FIELD_ASSOCIATION_POINTS,
                                         GetActiveDataSet()->GetActiveScalarName().c_str() );
        this->tris->SetInputConnection( this->tfilter->GetOutputPort() );
        this->strip->SetInputConnection( this->tris->GetOutputPort() );
        this->glyph->SetInputConnection( this->strip->GetOutputPort() );
    }
    else
    {
        vprDEBUG( vesDBG, 1 ) << "|\tcfdVectorBase: NO Threshold"
                              << std::endl << vprDEBUG_FLUSH;
        this->glyph->SetInputConnection( this->ptmask->GetOutputPort() );
    }
}
////////////////////////////////////////////////////////////////////////////////
void cfdVectorBase::SetGlyphAttributes()
{
    glyph->SetSource( GetActiveDataSet()->GetArrow() );
    glyph->SetVectorModeToUseVector();
    glyph->SetScaleFactor( GetVectorScaleFactor() );

    if( _scaleByVector == 0 )
    {
        glyph->SetScaleModeToDataScalingOff();
    }
    else
    {
        glyph->SetScaleModeToScaleByVector();
        glyph->SetColorModeToColorByScalar();
        glyph->ClampingOn();
        glyph->SetRange( GetActiveDataSet()->GetVectorMagRange() );
    }
}
////////////////////////////////////////////////////////////////////////////////
float cfdVectorBase::GetVectorScaleFactor()
{
    // this->GetVectorScale() is obtained from gui, 0 < vectorScale < 300
    // we use a function y = exp(x), that has y(0) = 1 and y'(0) = 1

    /*
    float range = 2.5;
    float scaleFactor = exp( this->GetVectorScale() / ( 100.0 / range ) ) *
    this->GetActiveDataSet()->GetMeanCellLength();
    */
    // This scale returns a range of ~ 0.0103 ->  ~ 220 units
    float range = 2.5;
    float scaleFactor = ( exp( this->GetVectorScale() / ( 100.0 / range ) ) ) * 0.01f;

    vprDEBUG( vesDBG, 1 )
            << "|\tcfdVectorBase::GetVectorScaleFactor scaleFactor = "
            << scaleFactor
            << std::endl << vprDEBUG_FLUSH;

    return scaleFactor;
}
////////////////////////////////////////////////////////////////////////////////
/*
int cfdVectorBase::GetVectorRatioFactor()
{
   int ratioFactor = this->ratio;
   if(! ratioFactor )
   {
      // must update to allow numPointsInPlane computation to work
      vprDEBUG(vesDBG, 0) << "updating cutter to compute numPointsInPlane"
                              << std::endl  << vprDEBUG_FLUSH;
      this->cutter->Update();
      int numPointsInPlane = this->cutter->GetOutput()->GetNumberOfPoints();
      ratioFactor = numPointsInPlane/1000;
   }

   if(ratioFactor < 1 )
   {
      ratioFactor = 1;
   }

   vprDEBUG(vesDBG, 1) << "ratioFactor = " << ratioFactor
                           << std::endl << vprDEBUG_FLUSH;

   return ratioFactor;
}
*/
////////////////////////////////////////////////////////////////////////////////
/*
int cfdVectors::GetVectorRatioFactor()
{
   int ratioFactor = this->ratio;
   int numPointsInPlanes = 0;
   int numPlanes = 0;

   if(! ratioFactor )
   {
      numPointsInPlanes = this->GetActiveMeshedVolume()->GetPrecomputedSlices( xyz )
                                  ->GetPlanesData()->GetNumberOfPoints();
      numPlanes = this->GetActiveMeshedVolume()->GetPrecomputedSlices( xyz )
                          ->GetNumberOfPlanes();
      ratioFactor = numPointsInPlanes/(1000*numPlanes);
   }

   if(ratioFactor < 1 )
      ratioFactor = 1;

   vprDEBUG(vesDBG, 1) << "numPlanes = " << numPlanes
      << ", numPointsInPlanes = " << numPointsInPlanes
      << ", ratioFactor = " << ratioFactor
      << std::endl << vprDEBUG_FLUSH;

   return ratioFactor;
}
*/
////////////////////////////////////////////////////////////////////////////////
void cfdVectorBase::UpdateThreshHoldValues()
{
    //double currentScalarRange[ 2 ];
    DataSet* temp = GetActiveDataSet();

    if( temp != NULL )
    {
        //temp->GetRange( currentScalarRange );
        double* currentScalarRange = temp->GetRange();
        vprDEBUG( vesDBG, 1 ) << "|\tcfdVectorBase::UpdateThreshHoldValues currentScalarRange = "
                              << currentScalarRange[ 0 ] << " : " <<  currentScalarRange[ 1 ]
                              << std::endl << vprDEBUG_FLUSH;

        _vectorThreshHoldValues[ 0 ] = currentScalarRange[0] +
                                       ( double )_vectorThreshHoldMinPercentage / 100.0
                                       * ( currentScalarRange[1] - currentScalarRange[0] );

        _vectorThreshHoldValues[ 1 ] = currentScalarRange[0] +
                                       ( double )_vectorThreshHoldMaxPercentage / 100.0
                                       * ( currentScalarRange[1] - currentScalarRange[0] );

        vprDEBUG( vesDBG, 1 ) << "|\tcfdVectorBase::UpdateThreshHoldValues Calculated Threshold Values = "
                              << _vectorThreshHoldValues[ 0 ] << " : "
                              << _vectorThreshHoldValues[ 1 ] << std::endl << vprDEBUG_FLUSH;
    }
}
////////////////////////////////////////////////////////////////////////////////
void cfdVectorBase::SetThreshHoldPercentages( int minThread, int maxThread )
{
    _vectorThreshHoldMinPercentage = minThread;
    _vectorThreshHoldMaxPercentage = maxThread;
}
////////////////////////////////////////////////////////////////////////////////
void cfdVectorBase::SetThreshHoldValues( double* input )
{
    _vectorThreshHoldValues[ 0 ] = input[ 0 ];
    _vectorThreshHoldValues[ 1 ] = input[ 1 ];

    vprDEBUG( vesDBG, 1 ) << "|\tThreshold Values = "
                          << _vectorThreshHoldValues[ 0 ] << " : "
                          << _vectorThreshHoldValues[ 1 ] << std::endl << vprDEBUG_FLUSH;
}
////////////////////////////////////////////////////////////////////////////////
double* cfdVectorBase::GetThreshHoldValues( void )
{
    return ( _vectorThreshHoldValues );
}
////////////////////////////////////////////////////////////////////////////////
void cfdVectorBase::SetVectorRatioFactor( int x )
{
    _vectorRatioFactor = x;
}
////////////////////////////////////////////////////////////////////////////////
int cfdVectorBase::GetVectorRatioFactor()
{
    return _vectorRatioFactor;
}
////////////////////////////////////////////////////////////////////////////////
void cfdVectorBase::SetScaleByVectorFlag( int input )
{
    _scaleByVector = input;
}
////////////////////////////////////////////////////////////////////////////////
int cfdVectorBase::GetScaleByVectorFlag( void )
{
    return _scaleByVector;
}
////////////////////////////////////////////////////////////////////////////////
void cfdVectorBase::CreateArbSurface()
{
    //Need to set the active datasetname and get the position of the dataset
    Model* activeModel = ModelHandler::instance()->GetActiveModel();
    // set the dataset as the appropriate dastaset type
    // (and the active dataset as well)
    DataSet* surfDataset = activeModel->GetCfdDataSet(
                               activeModel->GetIndexOfDataSet( m_surfDataset ) );
    vtkPolyData* pd = surfDataset->GetPolyData();

    if( !pd )
    {
        std::cerr << "ERROR: Activate a polydata file to use this function"
                  << std::endl;
        return;
    }

    ves::xplorer::util::ExtractGeometryCallback* extractGeomCbk =
        new ves::xplorer::util::ExtractGeometryCallback();
    ves::xplorer::util::DataObjectHandler handler;
    handler.SetDatasetOperatorCallback( extractGeomCbk );
    extractGeomCbk->SetPolyDataSurface( pd );
    handler.OperateOnAllDatasetsInObject( GetActiveDataSet()->GetDataSet() );

    vtkCompositeDataProbeFilter* surfProbe = vtkCompositeDataProbeFilter::New();
    surfProbe->SetInput( pd );
    //surfProbe->SetSourceConnection( extractGeomCbk->GetDataset() );
    surfProbe->SetSource( extractGeomCbk->GetDataset() );

    vtkPolyData* surfProbeOutput = surfProbe->GetPolyDataOutput();

    if( !surfProbeOutput )
    {
        return;
    }

    vtkPolyData* normalsOutputPD = 0;
    if( m_selectDataMapping == 1 )
    {
        // The code below computes volume flux on the specified contour plane
        vtkPolyDataNormals* normalGen = vtkPolyDataNormals::New();
        normalGen->SetInputConnection( surfProbe->GetOutputPort() );
        normalGen->Update();

        normalsOutputPD =
            ComputeVolumeFlux( normalGen->GetOutput() );
        ptmask->SetInput( normalsOutputPD );
        ptmask->SetOnRatio( this->GetVectorRatioFactor() );
        //ptmask->Update();

        normalGen->Delete();
    }
    else
    {
        // get every nth point from the dataSet data
        ptmask->SetInputConnection( surfProbe->GetOutputPort() );
        ptmask->SetOnRatio( GetVectorRatioFactor() );
        //ptmask->Update();
    }

    SetGlyphWithThreshold();
    SetGlyphAttributes();

    if( m_selectDataMapping != 1 )
    {
        mapper->SetInputConnection( glyph->GetOutputPort() );
        mapper->SetScalarModeToUsePointFieldData();
        mapper->UseLookupTableScalarRangeOn();
        mapper->SelectColorArray( GetActiveDataSet()->
                                  GetActiveScalarName().c_str() );
        mapper->SetLookupTable( GetActiveDataSet()->GetLookupTable() );
        mapper->Update();
    }
    else
    {
        double range[ 2 ];
        normalsOutputPD->GetPointData()->
        GetScalars( "VolumeFlux" )->GetRange( range );

        vtkLookupTable* lut1 = vtkLookupTable::New();
        lut1->SetNumberOfColors( 2 );            //default is 256
        lut1->SetHueRange( 2.0f / 3.0f, 0.0f );    //a blue-to-red scale
        lut1->SetTableRange( range );
        lut1->Build();

        mapper->SetInputConnection( glyph->GetOutputPort() );
        mapper->SetColorModeToMapScalars();
        mapper->SetScalarRange( range );
        mapper->SetLookupTable( lut1 );
        mapper->SetScalarModeToUsePointFieldData();
        mapper->UseLookupTableScalarRangeOn();
        mapper->SelectColorArray( "VolumeFlux" );
        mapper->Update();

        lut1->Delete();
    }

    surfProbe->Delete();
    delete extractGeomCbk;
    extractGeomCbk = 0;
}
////////////////////////////////////////////////////////////////////////////////
void cfdVectorBase::UpdatePropertySet()
{
    //Extract the integration direction
    std::string dataMapping = boost::any_cast<std::string > ( m_propertySet->GetPropertyAttribute( "DataMapping", "enumCurrentString" ) );

    vprDEBUG( vesDBG, 0 )
            << "|\tSelect scalar or volume flux for contour display: " << dataMapping
            << std::endl << vprDEBUG_FLUSH;

    if( !dataMapping.compare( "Map Scalar Data" ) )
    {
        vprDEBUG( vesDBG, 0 ) << "|\t\tVISUALIZE SCALARS"
                              << std::endl << vprDEBUG_FLUSH;

        m_selectDataMapping = 0;
    }
    else if( !dataMapping.compare( "Map Volume Flux Data" ) )
    {
        vprDEBUG( vesDBG, 0 ) << "|\t\tVISUALIZE VOLUME FLUX"
                              << std::endl << vprDEBUG_FLUSH;

        m_selectDataMapping = 1;
    }

    //Extract the plane position
    SetRequestedValue( boost::any_cast<double>( m_propertySet->GetPropertyValue( "PlaneLocation" ) ) );

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

    //Extract the advanced settings from the commands
    std::vector< double > tempData;
    tempData.push_back( boost::any_cast<double>
                        ( m_propertySet->GetPropertyValue( "Advanced_VectorThreshold_Min" ) ) );
    tempData.push_back( boost::any_cast<double>
                        ( m_propertySet->GetPropertyValue( "Advanced_VectorThreshold_Max" ) ) );
    SetThreshHoldPercentages( static_cast< int >( tempData.at( 0 ) ),
                              static_cast< int >( tempData.at( 1 ) ) );
    UpdateThreshHoldValues();

    SetVectorScale( static_cast< float >( boost::any_cast<double>
                                          ( m_propertySet->GetPropertyValue( "Advanced_VectorScale" ) ) ) );

    SetVectorRatioFactor( static_cast< int >( boost::any_cast<double>
                          ( m_propertySet->GetPropertyValue( "Advanced_VectorRatio" ) ) ) );

    SetScaleByVectorFlag( static_cast< int >( boost::any_cast<bool>( m_propertySet->GetPropertyValue( "Advanced_ScaleByVectorMagnitude" ) ) ) );

    m_gpuTools = boost::any_cast<bool>( m_propertySet->GetPropertyValue( "UseGPUTools" ) );

    bool vectorGreyscale = boost::any_cast<bool>( m_propertySet->GetPropertyValue( "Advanced_Greyscale" ) );
    GetActiveDataSet()->SetGreyscaleFlag( vectorGreyscale );
    vprDEBUG( vesDBG, 0 ) << "|\tVector Greyscale set to : "
                          << vectorGreyscale
                          << std::endl << vprDEBUG_FLUSH;

    //Extract the surface flag
    /*activeModelDVP = objectCommand->GetDataValuePair( "SURF Tools" );
    if( activeModelDVP )
    {
    	activeModelDVP->GetData( m_surfDataset );
    }*/
}
////////////////////////////////////////////////////////////////////////////////
