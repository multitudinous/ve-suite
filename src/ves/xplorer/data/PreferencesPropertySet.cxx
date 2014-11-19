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
#include <ves/xplorer/data/PreferencesPropertySet.h>
#include <propertystore/Property.h>
#include <ves/xplorer/data/DatabaseManager.h>
#include <propertystore/MakeLive.h>

#include <switchwire/EventManager.h>
#include <switchwire/OptionalMacros.h>
#include <ves/xplorer/eventmanager/EventFactory.h>
#include <ves/xplorer/scenegraph/SceneManager.h>

#include <boost/bind.hpp>
#include <boost/concept_check.hpp>
#include <boost/lexical_cast.hpp>

#include <iostream>
#include <limits>

using namespace ves::xplorer::data;
////////////////////////////////////////////////////////////////////////////////
PreferencesPropertySet::PreferencesPropertySet()
{
    SetDataManager( DatabaseManager::instance()->GetDataManager() );

    ///Signal for Near-Far Ratio
    {
        m_nearFarRatioChangedSignal = reinterpret_cast< ves::util::BoolAndDoubleSignal_type* >
                                      ( xplorer::eventmanager::EventFactory::instance()->
                                        GetSignal( "PreferencesPropertySet.NearFarRatio" ) );
    }
    ///Signal for DraggerScaling
    {
        m_draggerScalingChangedSignal = reinterpret_cast< ves::util::BoolAndDoubleSignal_type* >
                                        ( xplorer::eventmanager::EventFactory::instance()->
                                          GetSignal( "PreferencesPropertySet.DraggerScaling" ) );
    }
    ///Signal for Background Color
    {
        m_backgroundColorChangedSignal = reinterpret_cast< ves::util::BoolAndDoubleVectorSignal_type* >
                                         ( xplorer::eventmanager::EventFactory::instance()->
                                           GetSignal( "PreferencesPropertySet.UsePreferredBackgroundColor" ) );
    }
    ///Signal to Update Camera
    {
        m_cameraPositionOrientationChangedSignal = reinterpret_cast< ves::util::TwoDoubleVectorsSignal_type* >
                                                   ( xplorer::eventmanager::EventFactory::instance()->
                                                     GetSignal( "PreferencesPropertySet.CameraPositionOrientationChanged" ) );
    }

    SetTypeName( "XplorerPreferences" );
    CreateSkeleton();
}
////////////////////////////////////////////////////////////////////////////////
PreferencesPropertySet::PreferencesPropertySet( const PreferencesPropertySet& orig )
    :
    PropertySet( orig )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
PreferencesPropertySet::~PreferencesPropertySet()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void PreferencesPropertySet::CreateSkeleton()
{

    /// LOD tools
    {
        AddProperty( "GeometryLODScale", 1.00, "Geometry LOD Scale" );
        SetPropertyAttribute( "GeometryLODScale", "minimumValue", 0.00 );
        SetPropertyAttribute( "GeometryLODScale", "maximumValue", 100.00 );
        GetProperty( "GeometryLODScale" )->SignalValueChanged.connect( boost::bind( &PreferencesPropertySet::SaveChanges, this, _1 ) );
        propertystore::MakeLiveBasePtr p( new propertystore::MakeLive< double >( m_UUIDString,
                           GetProperty( "GeometryLODScale" ),
                           "PreferencesPropertySet.GeometryLODScale",
                           false ) );
        m_liveObjects.push_back( p );
    }

    ///Near far ratio

    AddProperty( "NearFarRatio", false, "Set Near-Far Ratio" );
    SetPropertyAttribute( "NearFarRatio", "isUIGroupOnly", false );
    SetPropertyAttribute( "NearFarRatio", "setExpanded", true );
    GetProperty( "NearFarRatio" )->SignalValueChanged.connect( boost::bind( &PreferencesPropertySet::EnableNearFarRatio, this, _1 ) );
    GetProperty( "NearFarRatio" )->SignalValueChanged.connect( boost::bind( &PreferencesPropertySet::SaveChanges, this, _1 ) );

    AddProperty( "NearFarRatio_Ratio", 0.000005, "Ratio" );
    SetPropertyAttribute( "NearFarRatio_Ratio", "minimumValue", 0.000000 );
    SetPropertyAttribute( "NearFarRatio_Ratio", "maximumValue", 1.000000 );
    GetProperty( "NearFarRatio_Ratio" )->SetDisabled();
    GetProperty( "NearFarRatio_Ratio" )->SignalValueChanged.connect( boost::bind( &PreferencesPropertySet::PropagateNearFarRatioChanged, this ) );
    GetProperty( "NearFarRatio_Ratio" )->SignalValueChanged.connect( boost::bind( &PreferencesPropertySet::SaveChanges, this, _1 ) );
    SetPropertyAttribute( "NearFarRatio_Ratio", "DisplayPrecision", 8 );


    ///Dragger Scaling
    AddProperty( "DraggerScaling", false, "Enable Dragger Scaling" );
    SetPropertyAttribute( "DraggerScaling", "isUIGroupOnly", false );
    SetPropertyAttribute( "DraggerScaling", "setExpanded", true );
    GetProperty( "DraggerScaling" )->SignalValueChanged.connect( boost::bind( &PreferencesPropertySet::EnableDraggerScaling, this, _1 ) );
    GetProperty( "DraggerScaling" )->SignalValueChanged.connect( boost::bind( &PreferencesPropertySet::SaveChanges, this, _1 ) );

    AddProperty( "DraggerScaling_Scale", 6.00,  "Scale Value" );
    SetPropertyAttribute( "DraggerScaling_Scale", "minimumValue", 0.00 );
    SetPropertyAttribute( "DraggerScaling_Scale", "maximumValue", 100.00 );
    GetProperty( "DraggerScaling_Scale" )->SetDisabled();
    GetProperty( "DraggerScaling_Scale" )->SignalValueChanged.connect( boost::bind( &PreferencesPropertySet::PropagateDraggerScalingChanged, this ) );
    GetProperty( "DraggerScaling_Scale" )->SignalValueChanged.connect( boost::bind( &PreferencesPropertySet::SaveChanges, this, _1 ) );


    ///Background color
    AddProperty( "UsePreferredBackgroundColor", false, "Use Preferred Background Color" );
    SetPropertyAttribute( "UsePreferredBackgroundColor", "isUIGroupOnly", false );
    SetPropertyAttribute( "UsePreferredBackgroundColor", "setExpanded", true );
    GetProperty( "UsePreferredBackgroundColor" )->SignalValueChanged.connect( boost::bind( &PreferencesPropertySet::EnableBackgroundColor, this, _1 ) );
    GetProperty( "UsePreferredBackgroundColor" )->SignalValueChanged.connect( boost::bind( &PreferencesPropertySet::SaveChanges, this, _1 ) );

    AddProperty( "UsePreferredBackgroundColor_Red", 0.0, "Red" );
    SetPropertyAttribute( "UsePreferredBackgroundColor_Red", "minimumValue", 0.00 );
    SetPropertyAttribute( "UsePreferredBackgroundColor_Red", "maximumValue", 1.00 );
    GetProperty( "UsePreferredBackgroundColor_Red" )->SetDisabled();
    GetProperty( "UsePreferredBackgroundColor_Red" )->SignalValueChanged.connect( boost::bind( &PreferencesPropertySet::PropagateBackgroundColorChanged, this ) );
    GetProperty( "UsePreferredBackgroundColor_Red" )->SignalValueChanged.connect( boost::bind( &PreferencesPropertySet::SaveChanges, this, _1 ) );

    AddProperty( "UsePreferredBackgroundColor_Green", 0.0, "Green" );
    SetPropertyAttribute( "UsePreferredBackgroundColor_Green", "minimumValue", 0.00 );
    SetPropertyAttribute( "UsePreferredBackgroundColor_Green", "maximumValue", 1.00 );
    GetProperty( "UsePreferredBackgroundColor_Green" )->SetDisabled();
    GetProperty( "UsePreferredBackgroundColor_Green" )->SignalValueChanged.connect( boost::bind( &PreferencesPropertySet::PropagateBackgroundColorChanged, this ) );
    GetProperty( "UsePreferredBackgroundColor_Green" )->SignalValueChanged.connect( boost::bind( &PreferencesPropertySet::SaveChanges, this, _1 ) );

    AddProperty( "UsePreferredBackgroundColor_Blue", 0.0, "Blue" );
    SetPropertyAttribute( "UsePreferredBackgroundColor_Blue", "minimumValue", 0.00 );
    SetPropertyAttribute( "UsePreferredBackgroundColor_Blue", "maximumValue", 1.00 );
    GetProperty( "UsePreferredBackgroundColor_Blue" )->SetDisabled();
    GetProperty( "UsePreferredBackgroundColor_Blue" )->SignalValueChanged.connect( boost::bind( &PreferencesPropertySet::PropagateBackgroundColorChanged, this ) );
    GetProperty( "UsePreferredBackgroundColor_Blue" )->SignalValueChanged.connect( boost::bind( &PreferencesPropertySet::SaveChanges, this, _1 ) );


    {
        AddProperty( "NavigationZEqual0Lock", false, "Navigation z = 0 Lock" );
        GetProperty( "NavigationZEqual0Lock" )->SignalValueChanged.connect( boost::bind( &PreferencesPropertySet::SaveChanges, this, _1 ) );
        propertystore::MakeLiveBasePtr p( new propertystore::MakeLive< bool const& >( m_UUIDString,
                           GetProperty( "NavigationZEqual0Lock" ),
                           "PreferencesPropertySet.NavigationZEqual0Lock",
                           false ) );
        m_liveObjects.push_back( p );
    }

    {
        AddProperty( "NavigationZGreater0Lock", false, "Navigation z > 0 Lock" );
        GetProperty( "NavigationZGreater0Lock" )->SignalValueChanged.connect( boost::bind( &PreferencesPropertySet::SaveChanges, this, _1 ) );
        propertystore::MakeLiveBasePtr p( new propertystore::MakeLive< bool const& >( m_UUIDString,
                           GetProperty( "NavigationZGreater0Lock" ),
                           "PreferencesPropertySet.NavigationZGreater0Lock",
                           false ) );
        m_liveObjects.push_back( p );
    }

    {
        std::vector< std::string > stringVector;
        AddProperty( "NavigationRotationMode", std::string(""), "Nav Mode" );
        stringVector.push_back( "User" );
        stringVector.push_back( "Orbit" );
        SetPropertyAttribute( "NavigationRotationMode", "enumValues", stringVector );

        GetProperty( "NavigationRotationMode" )->SignalValueChanged.connect( boost::bind( &PreferencesPropertySet::SaveChanges, this, _1 ) );
        propertystore::MakeLiveBasePtr p( new propertystore::MakeLive< std::string >( m_UUIDString,
                           GetProperty( "NavigationRotationMode" ),
                           "PreferencesPropertySet.NavigationRotationMode",
                           false ) );
        m_liveObjects.push_back( p );
    }

    {
        AddProperty( "ShutDownXplorerOption", false, "Shut Down Xplorer Option" );
        GetProperty( "ShutDownXplorerOption" )->SignalValueChanged.connect( boost::bind( &PreferencesPropertySet::SaveChanges, this, _1 ) );
        propertystore::MakeLiveBasePtr p( new propertystore::MakeLive< bool >( m_UUIDString,
                           GetProperty( "ShutDownXplorerOption" ),
                           "PreferencesPropertySet.ShutDownXplorerOption",
                           false ) );
        m_liveObjects.push_back( p );
    }

    {
        AddProperty( "PhysicsDebugger", false, "Physics Debugger" );
        GetProperty( "PhysicsDebugger" )->SignalValueChanged.connect( boost::bind( &PreferencesPropertySet::SaveChanges, this, _1 ) );
        propertystore::MakeLiveBasePtr p( new propertystore::MakeLive< const bool& >( m_UUIDString,
                           GetProperty( "PhysicsDebugger" ),
                           "PreferencesPropertySet.PhysicsDebugger",
                           false ) );
        m_liveObjects.push_back( p );
    }

    {
        AddProperty( "CADSelection", false, "CAD Selection" );
        GetProperty( "CADSelection" )->SignalValueChanged.connect( boost::bind( &PreferencesPropertySet::SaveChanges, this, _1 ) );
        propertystore::MakeLiveBasePtr p( new propertystore::MakeLive< bool const& >( m_UUIDString,
                           GetProperty( "CADSelection" ),
                           "PreferencesPropertySet.CADSelection",
                           false ) );
        m_liveObjects.push_back( p );
    }

    {
        AddProperty( "ScriptLogger", false, "Script Logger" );
        GetProperty( "ScriptLogger" )->SignalValueChanged.connect( boost::bind( &PreferencesPropertySet::SaveChanges, this, _1 ) );
        propertystore::MakeLiveBasePtr p( new propertystore::MakeLive< bool >( m_UUIDString,
                           GetProperty( "ScriptLogger" ),
                           "PreferencesPropertySet.ScriptLogger",
                           false ) );
        m_liveObjects.push_back( p );
    }

    {
        AddProperty( "ScreenAlignedNormals", false, "Screen Aligned Normals" );
        GetProperty( "ScreenAlignedNormals" )->SignalValueChanged.connect( boost::bind( &PreferencesPropertySet::SaveChanges, this, _1 ) );
        propertystore::MakeLiveBasePtr p( new propertystore::MakeLive< bool >( m_UUIDString,
                           GetProperty( "ScreenAlignedNormals" ),
                           "PreferencesPropertySet.ScreenAlignedNormals",
                           false ) );
        m_liveObjects.push_back( p );
    }

    {
        AddProperty( "DisplayFrameRate", false, "Display Frame Rate" );
        GetProperty( "DisplayFrameRate" )->SignalValueChanged.connect( boost::bind( &PreferencesPropertySet::SaveChanges, this, _1 ) );
        propertystore::MakeLiveBasePtr p( new propertystore::MakeLive< const bool& >( m_UUIDString,
                           GetProperty( "DisplayFrameRate" ),
                           "PreferencesPropertySet.DisplayFrameRate",
                           false ) );
        m_liveObjects.push_back( p );
    }

    {
        AddProperty( "DisplayGlobalAxis", false, "Display Global Axis" );
        GetProperty( "DisplayGlobalAxis" )->SignalValueChanged.connect( boost::bind( &PreferencesPropertySet::SaveChanges, this, _1 ) );
        propertystore::MakeLiveBasePtr p( new propertystore::MakeLive< bool const& >( m_UUIDString,
                           GetProperty( "DisplayGlobalAxis" ),
                           "PreferencesPropertySet.DisplayGlobalAxis",
                           false ) );
        m_liveObjects.push_back( p );
    }

    {
        AddProperty( "DeviceGloveDisplay", false, "Display Glove Models" );
        GetProperty( "DeviceGloveDisplay" )->SignalValueChanged.connect( boost::bind( &PreferencesPropertySet::SaveChanges, this, _1 ) );
        propertystore::MakeLiveBasePtr p( new propertystore::MakeLive< bool const& >( m_UUIDString,
                           GetProperty( "DeviceGloveDisplay" ),
                           "PreferencesPropertySet.DeviceGloveDisplay",
                           false ) );
        m_liveObjects.push_back( p );
    }

    {
        AddProperty( "AmbientAudioSoundFile", std::string( "null" ), "Ambient Audio File" );
        SetPropertyAttribute( "AmbientAudioSoundFile", "isFilePath", true );
        GetProperty( "AmbientAudioSoundFile" )->
        SignalValueChanged.connect( boost::bind( &PreferencesPropertySet::SaveChanges, this, _1 ) );
        propertystore::MakeLiveBasePtr p( new propertystore::MakeLive< std::string const& >( m_UUIDString,
                           GetProperty( "AmbientAudioSoundFile" ),
                           "PreferencesPropertySet.AmbientAudioSoundFile",
                           false ) );
        m_liveObjects.push_back( p );
    }
    
    {
        AddProperty( "AnalogDeadZone", 0.075,  "Dead Zone Value" );
        SetPropertyAttribute( "AnalogDeadZone", "minimumValue", 0.000 );
        SetPropertyAttribute( "AnalogDeadZone", "maximumValue", 1.000 );
        SetPropertyAttribute( "AnalogDeadZone", "DisplayPrecision", 3 );

        GetProperty( "AnalogDeadZone" )->SignalValueChanged.connect( boost::bind( &PreferencesPropertySet::SaveChanges, this, _1 ) );
        propertystore::MakeLiveBasePtr p( new propertystore::MakeLive< double const& >( m_UUIDString,
                                                                                     GetProperty( "AnalogDeadZone" ),
                                                                                     "PreferencesPropertySet.AnalogAxisDeadZoneChanged",
                                                                                     false ) );
        m_liveObjects.push_back( p );
    }

    // camera properties
    {
        AddProperty( "Camera", boost::any(), "Camera" );
        SetPropertyAttribute( "Camera", "isUIGroupOnly", true );
        SetPropertyAttribute( "Camera", "setExpanded", true );

        {
            AddProperty( "Camera_Position", boost::any(), "Position" );
            SetPropertyAttribute( "Camera_Position", "isUIGroupOnly", true );
            SetPropertyAttribute( "Camera_Position", "setExpanded", true );

            {
                AddProperty( "Camera_Position_X", 0.0, "x" );
                GetProperty( "Camera_Position_X" )->SignalValueChanged.connect( boost::bind( &PreferencesPropertySet::PropagateCameraPositionOrientationChanged, this ) );
                GetProperty( "Camera_Position_X" )->SignalValueChanged.connect( boost::bind( &PreferencesPropertySet::SaveChanges, this, _1 ) );

                AddProperty( "Camera_Position_Y", 0.0, "y" );
                GetProperty( "Camera_Position_Y" )->SignalValueChanged.connect( boost::bind( &PreferencesPropertySet::PropagateCameraPositionOrientationChanged, this ) );
                GetProperty( "Camera_Position_Y" )->SignalValueChanged.connect( boost::bind( &PreferencesPropertySet::SaveChanges, this, _1 ) );

                AddProperty( "Camera_Position_Z", 0.0, "z" );
                GetProperty( "Camera_Position_Z" )->SignalValueChanged.connect( boost::bind( &PreferencesPropertySet::PropagateCameraPositionOrientationChanged, this ) );
                GetProperty( "Camera_Position_Z" )->SignalValueChanged.connect( boost::bind( &PreferencesPropertySet::SaveChanges, this, _1 ) );
            }

            AddProperty( "Camera_ViewDirection", boost::any(), "View Direction" );
            SetPropertyAttribute( "Camera_ViewDirection", "isUIGroupOnly", true );
            SetPropertyAttribute( "Camera_ViewDirection", "setExpanded", true );

            {
                AddProperty( "Camera_ViewDirection_X", 0.0, "x" );
                GetProperty( "Camera_ViewDirection_X" )->SignalValueChanged.connect( boost::bind( &PreferencesPropertySet::PropagateCameraPositionOrientationChanged, this ) );
                GetProperty( "Camera_ViewDirection_X" )->SignalValueChanged.connect( boost::bind( &PreferencesPropertySet::SaveChanges, this, _1 ) );

                AddProperty( "Camera_ViewDirection_Y", 1.0, "y" );
                GetProperty( "Camera_ViewDirection_Y" )->SignalValueChanged.connect( boost::bind( &PreferencesPropertySet::PropagateCameraPositionOrientationChanged, this ) );
                GetProperty( "Camera_ViewDirection_Y" )->SignalValueChanged.connect( boost::bind( &PreferencesPropertySet::SaveChanges, this, _1 ) );

                AddProperty( "Camera_ViewDirection_Z", 0.0, "z" );
                GetProperty( "Camera_ViewDirection_Z" )->SignalValueChanged.connect( boost::bind( &PreferencesPropertySet::PropagateCameraPositionOrientationChanged, this ) );
                GetProperty( "Camera_ViewDirection_Z" )->SignalValueChanged.connect( boost::bind( &PreferencesPropertySet::SaveChanges, this, _1 ) );
            }
        }

        AddProperty( "Camera_MoveScaleFactor", 1.0, "Movement Scale Factor" );
        GetProperty( "Camera_MoveScaleFactor" )->SignalValueChanged.connect( boost::bind( &PreferencesPropertySet::SaveChanges, this, _1 ) );
        SetPropertyAttribute( "Camera_MoveScaleFactor", "minimumValue", 0.01 );
        SetPropertyAttribute( "Camera_MoveScaleFactor", "maximumValue", 100.00 );

        propertystore::MakeLiveBasePtr p( new propertystore::MakeLive< double >( m_UUIDString,
                                                                                 GetProperty( "Camera_MoveScaleFactor" ),
                                                                                 "PreferencesPropertySet.CameraMoveScaleFactorChanged",
                                                                                 false ) );
        m_liveObjects.push_back( p );
    }
}
////////////////////////////////////////////////////////////////////////////////
void PreferencesPropertySet::EnableNearFarRatio( propertystore::PropertyPtr& property )
{
    bool value = boost::any_cast<bool>( property->GetValue() );

    if( value )
    {
        GetProperty( "NearFarRatio_Ratio" )->SetEnabled();
    }
    else
    {
        GetProperty( "NearFarRatio_Ratio" )->SetDisabled();
    }

    PropagateNearFarRatioChanged();
}
////////////////////////////////////////////////////////////////////////////////
void PreferencesPropertySet::EnableBackgroundColor( propertystore::PropertyPtr& property )
{
    bool value = boost::any_cast<bool>( property->GetValue() );

    if( value )
    {
        GetProperty( "UsePreferredBackgroundColor_Red" )->SetEnabled();
        GetProperty( "UsePreferredBackgroundColor_Blue" )->SetEnabled();
        GetProperty( "UsePreferredBackgroundColor_Green" )->SetEnabled();
    }
    else
    {
        GetProperty( "UsePreferredBackgroundColor_Red" )->SetDisabled();
        GetProperty( "UsePreferredBackgroundColor_Blue" )->SetDisabled();
        GetProperty( "UsePreferredBackgroundColor_Green" )->SetDisabled();
    }

    PropagateBackgroundColorChanged();
}
////////////////////////////////////////////////////////////////////////////////
void PreferencesPropertySet::EnableDraggerScaling( propertystore::PropertyPtr& property )
{
    bool value = boost::any_cast<bool>( property->GetValue() );

    if( value )
    {
        GetProperty( "DraggerScaling_Scale" )->SetEnabled();
    }
    else
    {
        GetProperty( "DraggerScaling_Scale" )->SetDisabled();
    }

    PropagateDraggerScalingChanged();
}
////////////////////////////////////////////////////////////////////////////////
void PreferencesPropertySet::PropagateBackgroundColorChanged()
{
    double r = boost::any_cast<double>( GetPropertyValue( "UsePreferredBackgroundColor_Red" ) );
    double g = boost::any_cast<double>( GetPropertyValue( "UsePreferredBackgroundColor_Green" ) );
    double b = boost::any_cast<double>( GetPropertyValue( "UsePreferredBackgroundColor_Blue" ) );
    std::vector< double > colors;
    colors.push_back( r );
    colors.push_back( g );
    colors.push_back( b );

    m_backgroundColorChangedSignal->signal( boost::any_cast<bool>( GetPropertyValue( "UsePreferredBackgroundColor" ) ), colors );
}
////////////////////////////////////////////////////////////////////////////////
void PreferencesPropertySet::PropagateCameraPositionOrientationChanged()
{
    std::vector< double > view, pos;

    view.push_back( boost::any_cast<double>( GetPropertyValue( "Camera_ViewDirection_X" ) ) );
    view.push_back( boost::any_cast<double>( GetPropertyValue( "Camera_ViewDirection_Y" ) ) );
    view.push_back( boost::any_cast<double>( GetPropertyValue( "Camera_ViewDirection_Z" ) ) );

    osg::Vec3d view_vec( view[0], view[1], view[2] );

    // sanity check - make sure we're not forwading bogus view vectors to the scenegraph
    if( view_vec.isNaN() || view_vec.length() <= std::numeric_limits< double >::epsilon() )
    {
        GetProperty( "Camera_ViewDirection_X" )->SetValue( 0.0 );
        GetProperty( "Camera_ViewDirection_Y" )->SetValue( 1.0 );
        GetProperty( "Camera_ViewDirection_Z" )->SetValue( 0.0 );

        view[0] = 0.0;
        view[1] = 1.0;
        view[2] = 0.0;
    }

    pos.push_back( boost::any_cast<double>( GetPropertyValue( "Camera_Position_X" ) ) );
    pos.push_back( boost::any_cast<double>( GetPropertyValue( "Camera_Position_Y" ) ) );
    pos.push_back( boost::any_cast<double>( GetPropertyValue( "Camera_Position_Z" ) ) );

    m_cameraPositionOrientationChangedSignal->signal( view, pos );
}
////////////////////////////////////////////////////////////////////////////////
void PreferencesPropertySet::SaveChanges( propertystore::PropertyPtr& )
{
    Save();
}
////////////////////////////////////////////////////////////////////////////////
void PreferencesPropertySet::GetCurrentCameraState()
{
    osgwMx::MxCore& viewmat = ves::xplorer::scenegraph::SceneManager::instance()->GetMxCoreViewMatrix();
    osg::Vec3d view = viewmat.getDir();
    osg::Vec3d pos = viewmat.getPosition();

    GetProperty( "Camera_ViewDirection_X" )->SetValue( view[0] );
    GetProperty( "Camera_ViewDirection_Y" )->SetValue( view[1] );
    GetProperty( "Camera_ViewDirection_Z" )->SetValue( view[2] );
    GetProperty( "Camera_Position_X" )->SetValue( pos[0] );
    GetProperty( "Camera_Position_Y" )->SetValue( pos[1] );
    GetProperty( "Camera_Position_Z" )->SetValue( pos[2] );
}
////////////////////////////////////////////////////////////////////////////////
void PreferencesPropertySet::PropagateDraggerScalingChanged()
{
    m_draggerScalingChangedSignal->signal( boost::any_cast<bool>( GetPropertyValue( "DraggerScaling" ) ),
                                           boost::any_cast<double>( GetPropertyValue( "DraggerScaling_Scale" ) ) );
}
////////////////////////////////////////////////////////////////////////////////
void PreferencesPropertySet::PropagateNearFarRatioChanged()
{
    m_nearFarRatioChangedSignal->signal( boost::any_cast<bool>( GetPropertyValue( "NearFarRatio" ) ),
                                         boost::any_cast<double>( GetPropertyValue( "NearFarRatio_Ratio" ) ) );
}
////////////////////////////////////////////////////////////////////////////////
