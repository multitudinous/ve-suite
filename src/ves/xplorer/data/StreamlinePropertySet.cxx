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
#include <ves/xplorer/data/StreamlinePropertySet.h>
#include <ves/xplorer/data/DatasetPropertySet.h>
#include <propertystore/Property.h>
#include <ves/xplorer/data/DatabaseManager.h>
#include <propertystore/MakeLive.h>

#include <latticefx/core/vtk/VTKStreamlineRenderer.h>

#include <switchwire/EventManager.h>
#include <switchwire/OptionalMacros.h>

#include <boost/bind.hpp>
#include <boost/concept_check.hpp>
#include <boost/lexical_cast.hpp>

#include <iostream>

using namespace ves::xplorer::data;
////////////////////////////////////////////////////////////////////////////////
StreamlinePropertySet::StreamlinePropertySet()
{
    SetDataManager( DatabaseManager::instance()->GetDataManager() );

    ///Signal for turning on seed points
    {
        std::string name( "StreamlinePropertySet" );
        name += boost::lexical_cast<std::string>( this );
        name += ".ActivateSeedPoints";

        switchwire::EventManager::instance()->RegisterSignal(
            ( &m_activateSeedPoints ),
            name, switchwire::EventManager::unspecified_SignalType );
    }
    ///Signal to change the bounds of seed points
    {
        std::string name( "StreamlinePropertySet" );
        name += boost::lexical_cast<std::string>( this );
        name += ".UpdateSeedPointBounds";

        switchwire::EventManager::instance()->RegisterSignal(
            ( &m_updateSeedPointBounds ),
            name, switchwire::EventManager::unspecified_SignalType );
    }
    ///Signal to change the active dataset
    {
        std::string name( "StreamlinePropertySet" );
        name += boost::lexical_cast<std::string>( this );
        name += ".ActiveDataSet";

        switchwire::EventManager::instance()->RegisterSignal(
            ( &m_activeDataSet ),
            name, switchwire::EventManager::unspecified_SignalType );
    }

    SetTypeName( "Streamline" );

    RegisterPropertySet( GetTypeName() );

    CreateSkeletonLfxDs();
}
////////////////////////////////////////////////////////////////////////////////
StreamlinePropertySet::StreamlinePropertySet( const StreamlinePropertySet& orig )
    :
    VizBasePropertySet( orig )
{
}
////////////////////////////////////////////////////////////////////////////////
StreamlinePropertySet::~StreamlinePropertySet()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
propertystore::PropertySetPtr StreamlinePropertySet::CreateNew()
{
    return propertystore::PropertySetPtr( new StreamlinePropertySet );
}
////////////////////////////////////////////////////////////////////////////////
void StreamlinePropertySet::CreateSkeletonLfxDs()
{
	CreateSkeleton();

	// TODO: set any other defaults for the renderer here
	lfx::core::vtk::VTKStreamlineRendererPtr renderOp( new lfx::core::vtk::VTKStreamlineRenderer() );
	renderOp->setTransferFunctionDestination( lfx::core::Renderer::TF_RGBA );
	VizBasePropertySet::CreateSkeletonLfxDsRenderer( "stm", renderOp.get() );
}
////////////////////////////////////////////////////////////////////////////////
void StreamlinePropertySet::CreateSkeleton()
{
    AddProperty( "Hide", false, "Toggle Viz Off" );

    AddProperty( "DataSet", std::string(""), "Data Set" );
    PSVectorOfStrings enumValues;

    AddProperty( "DataSet_ScalarData", std::string(""), "Scalar Data" );
    // Dummy value to ensure this gets set up as an enum
    enumValues.push_back( "Select Scalar Data" );
    SetPropertyAttribute( "DataSet_ScalarData", "enumValues", enumValues );
    GetProperty( "DataSet" )->SignalValueChanged.connect( boost::bind( &VizBasePropertySet::UpdateScalarDataOptions, this, _1 ) );

    AddProperty( "DataSet_ScalarRange", boost::any(), "Scalar Range" );
    SetPropertyAttribute( "DataSet_ScalarRange", "isUIGroupOnly", true );
    SetPropertyAttribute( "DataSet_ScalarRange", "setExpanded", true );

    AddProperty( "DataSet_ScalarRange_Min", 0.0, "Min" );
    GetProperty( "DataSet_ScalarRange_Min" )->SetDisabled();

    AddProperty( "DataSet_ScalarRange_Max", 1.0, "Max" );
    GetProperty( "DataSet_ScalarRange_Max" )->SetDisabled();

    GetProperty( "DataSet_ScalarData" )->SignalValueChanged.connect( boost::bind( &VizBasePropertySet::UpdateScalarDataRange, this, _1 ) );
    GetProperty( "DataSet_ScalarRange_Min" )->SignalRequestValidation.connect( boost::bind( &VizBasePropertySet::ValidateScalarMinMax, this, _1, _2 ) );
    GetProperty( "DataSet_ScalarRange_Max" )->SignalRequestValidation.connect( boost::bind( &VizBasePropertySet::ValidateScalarMinMax, this, _1, _2 ) );

    AddProperty( "DataSet_VectorData", std::string(""), "Vector Data" );
    enumValues.clear();
    enumValues.push_back( "Select Vector Data" );
    SetPropertyAttribute( "DataSet_VectorData", "enumValues", enumValues );
    GetProperty( "DataSet" )->SignalValueChanged.connect( boost::bind( &VizBasePropertySet::UpdateVectorDataOptions, this, _1 ) );

    // Now that DataSet subproperties exist, we can initialize the values in
    // the dataset enum. If we had tried to do this beforehand, none of the
    // connections between DataSet and its subproperties would have been in
    // place yet.
    enumValues.clear();
    enumValues = ves::xplorer::data::DatabaseManager::instance()->GetStringVector( "Dataset", "Filename" );
    if( enumValues.empty() )
    {
        enumValues.push_back( "No datasets loaded" );
    }

    SetPropertyAttribute( "DataSet", "enumValues", enumValues );
    // Now that DataSet has choices loaded, force an update on the available
    // scalar and vector data
    propertystore::PropertyPtr nullPtr;
    UpdateScalarDataOptions( nullPtr );
    UpdateVectorDataOptions( nullPtr );

    ///Integration controls
    AddProperty( "IntegrationDirection", std::string("Both"), "Integration Direction" );
    enumValues.clear();
    enumValues.push_back( "Backward" );
    enumValues.push_back( "Forward" );
    enumValues.push_back( "Both" );
    SetPropertyAttribute( "IntegrationDirection", "enumValues", enumValues );

    ///Seed point controls
    AddProperty( "SeedPoints", boost::any(), "Seed Points" );
    SetPropertyAttribute( "SeedPoints", "isUIGroupOnly", true );
    SetPropertyAttribute( "SeedPoints", "setExpanded", true );
    AddProperty( "SeedPoints_DisplaySeedPoints", false, "Display Seed Points" );
    GetProperty( "SeedPoints_DisplaySeedPoints" )->SignalValueChanged.connect( boost::bind( &StreamlinePropertySet::UpdateSeedPointDisplay, this, _1 ) );
    AddProperty( "SeedPoints_NumberOfPointsInX", 5, "Number of Points in X" );
    SetPropertyAttribute( "SeedPoints_NumberOfPointsInX", "minimumValue",   0 );
    AddProperty( "SeedPoints_NumberOfPointsInY", 5, "Number of Points in Y" );
    SetPropertyAttribute( "SeedPoints_NumberOfPointsInY", "minimumValue",   0 );
    AddProperty( "SeedPoints_NumberOfPointsInZ", 1, "Number of Points in Z" );
    SetPropertyAttribute( "SeedPoints_NumberOfPointsInZ", "minimumValue",   0 );

    // Link the three NumberOfPointsIn... properties together and have them
    // fire a signal with signature void( std::vector<int> ) whose tail is
    // named "UpdateSeedPointDimensions"
    std::vector< propertystore::PropertyPtr > numberOfPointsLink;
    numberOfPointsLink.push_back( GetProperty( "SeedPoints_NumberOfPointsInX" ) );
    numberOfPointsLink.push_back( GetProperty( "SeedPoints_NumberOfPointsInY" ) );
    numberOfPointsLink.push_back( GetProperty( "SeedPoints_NumberOfPointsInZ" ) );
    propertystore::MakeLiveBasePtr p( new propertystore::MakeLiveLinked< int >(
                           m_UUIDString,
                           numberOfPointsLink,
                           "UpdateSeedPointDimensions" ) );
    m_liveObjects.push_back( p );

    AddProperty( "SeedPoints_Bounds", boost::any(), "Bounds" );
    SetPropertyAttribute( "SeedPoints_Bounds", "isUIGroupOnly", true );
    SetPropertyAttribute( "SeedPoints_Bounds", "setExpanded", true );
    AddProperty( "SeedPoints_Bounds_XMin",   0.0, "X Minimum" );
    SetPropertyAttribute( "SeedPoints_Bounds_XMin", "minimumValue",   0.0 );
    SetPropertyAttribute( "SeedPoints_Bounds_XMin", "maximumValue", 1.0 );
    AddProperty( "SeedPoints_Bounds_XMax", 1.0, "X Maximum" );
    SetPropertyAttribute( "SeedPoints_Bounds_XMax", "minimumValue",   0.0 );
    SetPropertyAttribute( "SeedPoints_Bounds_XMax", "maximumValue", 1.0 );
    AddProperty( "SeedPoints_Bounds_YMin",   0.0, "Y Minimum" );
    SetPropertyAttribute( "SeedPoints_Bounds_YMin", "minimumValue",   0.0 );
    SetPropertyAttribute( "SeedPoints_Bounds_YMin", "maximumValue", 1.0 );
    AddProperty( "SeedPoints_Bounds_YMax", 1.0, "Y Maximum" );
    SetPropertyAttribute( "SeedPoints_Bounds_YMax", "minimumValue",   0.0 );
    SetPropertyAttribute( "SeedPoints_Bounds_YMax", "maximumValue", 1.0 );
    AddProperty( "SeedPoints_Bounds_ZMin",   0.0, "Z Minimum" );
    SetPropertyAttribute( "SeedPoints_Bounds_ZMin", "minimumValue",   0.0 );
    SetPropertyAttribute( "SeedPoints_Bounds_ZMin", "maximumValue", 1.0 );
    AddProperty( "SeedPoints_Bounds_ZMax", 1.0, "Z Maximum" );
    SetPropertyAttribute( "SeedPoints_Bounds_ZMax", "minimumValue",   0.0 );
    SetPropertyAttribute( "SeedPoints_Bounds_ZMax", "maximumValue", 1.0 );

    ///General streamline properties
    AddProperty( "UseGPUTools", false, "Use GPU Tools" );
    AddProperty( "UseStreamArrows", false, "Use Stream Arrows" );
    AddProperty( "UseStreamRibbons", false, "Use Stream Ribbons" );
    AddProperty( "UseLastSeedPoints", false, "Use Last Seed Points" );

    ///Old values that are no longer used by the vis code
    AddProperty( "CursorDirection", std::string(""), "Cursor Direction" );
    enumValues.clear();
    enumValues.push_back( "x" );
    SetPropertyAttribute( "CursorDirection", "enumValues", enumValues );

    AddProperty( "CursortType", std::string(""), "Cursor Type" );
    enumValues.clear();
    enumValues.push_back( "none" );
    SetPropertyAttribute( "CursortType", "enumValues", enumValues );

    //AddProperty( "StreamlineSize", 0.5, "Streamline Size" );
    //AddProperty( "NumberOfPointsPerPlane", 2.0, "Number of Points Per Plane" );

    ///Advanced settings
    AddProperty( "Advanced", boost::any(), "Advanced" );
    SetPropertyAttribute( "Advanced", "isUIGroupOnly", true );
    AddProperty( "Advanced_PropogationTime", 100.0, "Propogation Time" );
    SetPropertyAttribute( "Advanced_PropogationTime", "minimumValue", 1.0 );
    SetPropertyAttribute( "Advanced_PropogationTime", "maximumValue", 100.0 );
    AddProperty( "Advanced_IntegrationStepSize", 1000.0f, "Integration Step Size" );
    SetPropertyAttribute( "Advanced_IntegrationStepSize", "minimumValue", 1.0 );
    SetPropertyAttribute( "Advanced_IntegrationStepSize", "maximumValue", 5000.0 );
    AddProperty( "Advanced_Diameter", -80.0f, "Diameter" );
    SetPropertyAttribute( "Advanced_Diameter", "minimumValue", -100.0 );
    SetPropertyAttribute( "Advanced_Diameter", "maximumValue", 100.0 );
    AddProperty( "Advanced_SphereArrowParticleSize", 5.0f, "Sphere/Arrow/Particle Size" );
    SetPropertyAttribute( "Advanced_VectorThreshold", "minimumValue", 1.0 );
    SetPropertyAttribute( "Advanced_VectorThreshold", "maximumValue", 50.0 );

    /*
    AddProperty( "Advanced_VectorScale", 200.0, "Vector Scale" );
    SetPropertyAttribute( "Advanced_VectorScale", "minimumValue",   1.0 );
    SetPropertyAttribute( "Advanced_VectorScale", "maximumValue", 400.0 );

    AddProperty( "Advanced_VectorRatio", 1.0, "Vector Ratio" );
    SetPropertyAttribute( "Advanced_VectorRatio", "minimumValue",   1.0 );
    SetPropertyAttribute( "Advanced_VectorRatio", "maximumValue", 200.0 );

    AddProperty( "Advanced_ScaleByVectorMagnitude", false, "Scale By Vector Magnitude" );

    ///Mode controls
    AddProperty( "Mode", std::string(""), "Mode" );
    enumValues.clear();
    enumValues.push_back( "Specify a Single Plane" );
    enumValues.push_back( "Use All Precomputed Surfaces" );
    SetPropertyAttribute( "Mode", "enumValues", enumValues );
    // Connect SignalValueChanged of "Mode" to a function that enables and disables
    // its sub-properties as appropriate
    propertystore::PropertyPtr mode = GetProperty( "Mode" );
    if( mode )
    {
        mode->SignalValueChanged.connect( boost::bind( &StreamlinePropertySet::UpdateModeOptions, this, _1 ) );
    }
    */
}
////////////////////////////////////////////////////////////////////////////////
void StreamlinePropertySet::UpdateSeedPointDisplay( propertystore::PropertyPtr property )
{
    const std::string dataSetName =
        boost::any_cast< std::string >( GetPropertyValue( "DataSet" ) );
    m_activeDataSet.signal( dataSetName );

    bool showDataSet = boost::any_cast< bool >( property->GetValue() );

    //Update the seed point bounds before turning the box off or on
    if( showDataSet )
    {
        std::vector<double> seedPointBounds;
        seedPointBounds.push_back( boost::any_cast<double>( GetPropertyValue( "SeedPoints_Bounds_XMin" ) ) );
        seedPointBounds.push_back( boost::any_cast<double>( GetPropertyValue( "SeedPoints_Bounds_XMax" ) ) );
        seedPointBounds.push_back( boost::any_cast<double>( GetPropertyValue( "SeedPoints_Bounds_YMin" ) ) );
        seedPointBounds.push_back( boost::any_cast<double>( GetPropertyValue( "SeedPoints_Bounds_YMax" ) ) );
        seedPointBounds.push_back( boost::any_cast<double>( GetPropertyValue( "SeedPoints_Bounds_ZMin" ) ) );
        seedPointBounds.push_back( boost::any_cast<double>( GetPropertyValue( "SeedPoints_Bounds_ZMax" ) ) );
        m_updateSeedPointBounds.signal( seedPointBounds );
    }

    m_activateSeedPoints.signal( dataSetName, showDataSet );
}
////////////////////////////////////////////////////////////////////////////////
void StreamlinePropertySet::EnableLiveProperties( bool live )
{
    if( !live )
    {
        m_liveObjects.clear();
        return;
    }
    else if( !m_liveObjects.empty() )
    {
        // Properties are already live
        return;
    }
    else
    {
        propertystore::MakeLiveBasePtr p(
                    new propertystore::MakeLive< bool const& >( m_UUIDString,
                                                         GetProperty("Hide"),
                                                         "HideVizFeature", true ) );
        m_liveObjects.push_back( p );
    }
}
////////////////////////////////////////////////////////////////////////////////
