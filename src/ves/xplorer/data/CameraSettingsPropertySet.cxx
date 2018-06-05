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
#include <ves/xplorer/data/CameraSettingsPropertySet.h>
#include <propertystore/Property.h>
#include <ves/xplorer/data/DatabaseManager.h>
#include <propertystore/MakeLive.h>

#include <switchwire/EventManager.h>
#include <switchwire/OptionalMacros.h>

#include <boost/bind.hpp>
#include <boost/concept_check.hpp>
#include <boost/lexical_cast.hpp>
#include <boost/any.hpp>

#include <iostream>

using namespace ves::xplorer::data;
////////////////////////////////////////////////////////////////////////////////
CameraSettingsPropertySet::CameraSettingsPropertySet()
{
    SetDataManager( DatabaseManager::instance()->GetDataManager() );
    SetTypeName( "CameraSettings" );

    std::string signame( "CameraSettingsPropertySet" );
    signame += boost::lexical_cast<std::string>( this );
    signame += ".CameraProjectionUpdate";

    switchwire::EventManager::instance()->RegisterSignal(
        ( &m_projectionChangedSignal ),
        signame );

    CreateSkeleton();
}
////////////////////////////////////////////////////////////////////////////////
CameraSettingsPropertySet::CameraSettingsPropertySet( const CameraSettingsPropertySet& orig )
    :
    PropertySet( orig )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
CameraSettingsPropertySet::~CameraSettingsPropertySet()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void CameraSettingsPropertySet::CreateSkeleton()
{
    SetPropertyAttribute( "NameTag", "uiLabel", std::string("Camera Name") );

    AddProperty( "ShowCameraGeometry", true, "Show Camera Geometry" );
    AddProperty( "ShowFrustumGeometry", true, "Show Frustum Geometry" );

    AddProperty( "Projection", boost::any(), "Projection Settings" );
    SetPropertyAttribute( "Projection", "isUIGroupOnly", true );
    AddProperty( "Projection_FOVZ", 40.0, "Field of View (z)" );
    SetPropertyAttribute( "Projection_FOVZ", "minimumValue", 0.0 );
    SetPropertyAttribute( "Projection_FOVZ", "maximumValue", 180.0 );
    AddProperty( "Projection_ImageDimensions",
                 std::string( "1280x720 (HD-Ready)" ), "Image Dimensions" );
    std::vector< std::string > enums;
    enums.push_back( "640x480 (VGA)" );
    enums.push_back( "800x600 (SVGA)" );
    enums.push_back( "1024x768 (XGA)" );
    enums.push_back( "1280x720 (HD-Ready)" );
    enums.push_back( "1280x800 (WXGA)" );
    enums.push_back( "1280x1024 (SXGA)" );
    enums.push_back( "1440x900 (WSXGA)" );
    enums.push_back( "1600x900 (HD +)" );
    enums.push_back( "1680x1050 (WSXGA +)" );
    enums.push_back( "1600x1200 (UXGA)" );
    enums.push_back( "1920x1080 (HD-1080)" );
    enums.push_back( "1920x1200 (WUXGA)" );
    SetPropertyAttribute( "Projection_ImageDimensions", "enumValues", enums );
    AddProperty( "Projection_AutoComputeFarPlane", true, "Auto Compute Far Plane" );
    AddProperty( "Projection_NearPlane", 0.1, "Near Plane" );
    SetPropertyAttribute( "Projection_NearPlane", "minimumValue", 0.1 );
    SetPropertyAttribute( "Projection_NearPlane", "maximumValue", 999.9 );
    AddProperty( "Projection_FarPlane", 5.0, "Far Plane" );
    SetPropertyAttribute( "Projection_FarPlane", "minimumValue", 0.2 );
    SetPropertyAttribute( "Projection_FarPlane", "maximumValue", 1000.0 );
    GetProperty( "Projection_NearPlane" )->SignalRequestValidation.connect( boost::bind( &CameraSettingsPropertySet::NearValidator, this, _1, _2 ) );
    GetProperty( "Projection_FarPlane" )->SignalRequestValidation.connect( boost::bind( &CameraSettingsPropertySet::FarValidator, this, _1, _2 ) );


    AddProperty( "DOF", boost::any(), "Depth Of Field" );
    SetPropertyAttribute( "DOF", "isUIGroupOnly", true );
    AddProperty( "DOF_FocalDistance", 1.5, "Focal Distance" );
    SetPropertyAttribute( "DOF_FocalDistance", "minimumValue", 0.0 );
    SetPropertyAttribute( "DOF_FocalDistance", "maximumValue", 100.0 );
    AddProperty( "DOF_FocusRange", 5.0, "Focus Range" );
    SetPropertyAttribute( "DOF_FocusRange", "minimumValue", 0.0 );
    SetPropertyAttribute( "DOF_FocusRange", "maximumValue", 100.0 );
    AddProperty( "DOF_MaxCircleOfConfusion", 6.0, "Max Circle of Confusion" );
    SetPropertyAttribute( "DOF_MaxCircleOfConfusion", "minimumValue", 0.0 );
    SetPropertyAttribute( "DOF_MaxCircleOfConfusion", "maximumValue", 15.0 );
}
////////////////////////////////////////////////////////////////////////////////
void CameraSettingsPropertySet::EnableLiveProperties( bool live )
{
    if( live )
    {
        propertystore::MakeLiveBasePtr p;
        p = propertystore::MakeLiveBasePtr( new propertystore::MakeLive<bool>( m_UUIDString,
                             GetProperty( "ShowCameraGeometry" ),
                             "ShowCameraGeometry" ) );
        m_liveObjects.push_back( p );

        p = propertystore::MakeLiveBasePtr( new propertystore::MakeLive<bool>( m_UUIDString,
                             GetProperty( "ShowFrustumGeometry" ),
                             "ShowCameraFrustumGeometry" ) );
        m_liveObjects.push_back( p );

        //Equivalent of makeLive, but setup to conform to backend needs
        boost::signals2::connection conn;
        conn = GetProperty( "Projection_ImageDimensions" )->SignalValueChanged.connect(
           boost::bind( &CameraSettingsPropertySet::ProjectionChanged, this, _1 ) );
        m_liveConnections.push_back( conn );
        conn = GetProperty( "Projection_AutoComputeFarPlane" )->SignalValueChanged.connect(
           boost::bind( &CameraSettingsPropertySet::ProjectionChanged, this, _1 ) );
        m_liveConnections.push_back( conn );
        conn = GetProperty( "Projection_NearPlane" )->SignalValueChanged.connect(
           boost::bind( &CameraSettingsPropertySet::ProjectionChanged, this, _1 ) );
        m_liveConnections.push_back( conn );
        conn = GetProperty( "Projection_FarPlane" )->SignalValueChanged.connect(
           boost::bind( &CameraSettingsPropertySet::ProjectionChanged, this, _1 ) );
        m_liveConnections.push_back( conn );
    }
    else
    {
        m_liveObjects.clear();
        for( size_t index = 0; index < m_liveConnections.size(); ++index )
        {
            m_liveConnections.at( index ).disconnect();
        }
        m_liveConnections.clear();
    }
}
////////////////////////////////////////////////////////////////////////////////
bool CameraSettingsPropertySet::NearValidator( propertystore::PropertyPtr, boost::any newValue )
{
    double nearVal = boost::any_cast< double >( newValue );
    double farVal = boost::any_cast< double >( GetPropertyValue( "Projection_FarPlane" ) );

    if( !(nearVal < farVal) )
    {
        SetPropertyValue( "Projection_FarPlane", nearVal + 0.1 );
    }

    return true;
}
////////////////////////////////////////////////////////////////////////////////
bool CameraSettingsPropertySet::FarValidator( propertystore::PropertyPtr, boost::any newValue )
{
    double nearVal = boost::any_cast< double >( GetPropertyValue( "Projection_NearPlane" ) );
    double farVal = boost::any_cast< double >( newValue );

    if( !(nearVal < farVal) )
    {
        SetPropertyValue( "Projection_NearPlane", nearVal - 0.1 );
    }

    return true;
}
////////////////////////////////////////////////////////////////////////////////
void CameraSettingsPropertySet::ProjectionChanged( propertystore::PropertyPtr )
{
    SaveNoOverride();
    m_projectionChangedSignal.signal( m_UUIDString );
}
