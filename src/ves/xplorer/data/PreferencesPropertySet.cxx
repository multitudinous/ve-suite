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
#include <ves/xplorer/data/PreferencesPropertySet.h>
#include <ves/xplorer/data/DatasetPropertySet.h>
#include <ves/xplorer/data/Property.h>
#include <ves/xplorer/data/DatabaseManager.h>

#include <ves/xplorer/eventmanager/EventManager.h>

#include <boost/bind.hpp>
#include <boost/concept_check.hpp>
#include <boost/lexical_cast.hpp>

#include <iostream>

using namespace ves::xplorer::data;
////////////////////////////////////////////////////////////////////////////////
PreferencesPropertySet::PreferencesPropertySet()
{
    using eventmanager::SignalWrapper;
    ///Signal for GeometryLODScale x
    {
        std::string name("PreferencesPropertySet");
        name += boost::lexical_cast<std::string>( this );
        name += ".GeometryLODScale";
        
        eventmanager::EventManager::instance()->RegisterSignal(
           new SignalWrapper< DoubleValueSignal_type >( &m_lodScaling ),
           name, eventmanager::EventManager::unspecified_SignalType );
    }
    ///Signal for GeometryLODScale - broken
    {
        std::string name("PreferencesPropertySet");
        name += boost::lexical_cast<std::string>( this );
        name += ".NearFarRatio";
        
        eventmanager::EventManager::instance()->RegisterSignal(
            new SignalWrapper< UpdateCheckAndValueSignal_type >( &m_nearFarRatio ),
            name, eventmanager::EventManager::unspecified_SignalType );
    }
    ///Signal for GeometryLODScale x
    {
        std::string name("PreferencesPropertySet");
        name += boost::lexical_cast<std::string>( this );
        name += ".DraggerScaling";
        
        eventmanager::EventManager::instance()->RegisterSignal(
            new SignalWrapper< UpdateCheckAndValueSignal_type >( &m_draggerScaling ),
            name, eventmanager::EventManager::unspecified_SignalType );
    }
    ///Signal for GeometryLODScale x
    {
        std::string name("PreferencesPropertySet");
        name += boost::lexical_cast<std::string>( this );
        name += ".UsePreferredBackgroundColor";
        
        eventmanager::EventManager::instance()->RegisterSignal(
            new SignalWrapper< UpdateCheckAndVectorSignal_type >( &m_backgroundColor ),
            name, eventmanager::EventManager::unspecified_SignalType );
    }
    ///Signal for GeometryLODScale x
    {
        std::string name("PreferencesPropertySet");
        name += boost::lexical_cast<std::string>( this );
        name += ".NavigationZEqual0Lock";
        
        eventmanager::EventManager::instance()->RegisterSignal(
            new SignalWrapper< CheckValueSignal_type >( &m_navZEqual0 ),
            name, eventmanager::EventManager::unspecified_SignalType );
    }
    ///Signal for GeometryLODScale x 
    {
        std::string name("PreferencesPropertySet");
        name += boost::lexical_cast<std::string>( this );
        name += ".NavigationZGreater0Lock";
        
        eventmanager::EventManager::instance()->RegisterSignal(
            new SignalWrapper< CheckValueSignal_type >( &m_navZGreater0 ),
            name, eventmanager::EventManager::unspecified_SignalType );
    }
    ///Signal for GeometryLODScale - not used
    {
        std::string name("PreferencesPropertySet");
        name += boost::lexical_cast<std::string>( this );
        name += ".ShutDownXplorerOption";
        
        eventmanager::EventManager::instance()->RegisterSignal(
            new SignalWrapper< CheckValueSignal_type >( &m_shutdownXplorer ),
            name, eventmanager::EventManager::unspecified_SignalType );
    }
    ///Signal for GeometryLODScale - wait
    {
        std::string name("PreferencesPropertySet");
        name += boost::lexical_cast<std::string>( this );
        name += ".PhysicsDebugger";
        
        eventmanager::EventManager::instance()->RegisterSignal(
            new SignalWrapper< CheckValueSignal_type >( &m_physicsDebugger ),
            name, eventmanager::EventManager::unspecified_SignalType );
    }
    ///Signal for GeometryLODScale x
    {
        std::string name("PreferencesPropertySet");
        name += boost::lexical_cast<std::string>( this );
        name += ".CADSelection";
        
        eventmanager::EventManager::instance()->RegisterSignal(
            new SignalWrapper< CheckValueSignal_type >( &m_cadSelection ),
            name, eventmanager::EventManager::unspecified_SignalType );
    }
    ///Signal for GeometryLODScale - not used
    {
        std::string name("PreferencesPropertySet");
        name += boost::lexical_cast<std::string>( this );
        name += ".ScriptLogger";
        
        eventmanager::EventManager::instance()->RegisterSignal(
            new SignalWrapper< CheckValueSignal_type >( &m_scriptLogger ),
            name, eventmanager::EventManager::unspecified_SignalType );
    }
    ///Signal for GeometryLODScale x
    {
        std::string name("PreferencesPropertySet");
        name += boost::lexical_cast<std::string>( this );
        name += ".ScreenAlignedNormals";
        
        eventmanager::EventManager::instance()->RegisterSignal(
            new SignalWrapper< CheckValueSignal_type >( &m_screenAlignedNormals ),
            name, eventmanager::EventManager::unspecified_SignalType );
    }
    
    mTableName = "XplorerPreferences";
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
    AddProperty( "GeometryLODScale", 1.00, "Geometry LOD Scale" );
    SetPropertyAttribute( "GeometryLODScale", "minimumValue", 0.00 );
    SetPropertyAttribute( "GeometryLODScale", "maximumValue", 100.00 );    
    mPropertyMap["GeometryLODScale"]->SignalValueChanged.connect( boost::bind( &PreferencesPropertySet::UpdateLODScaling, this, _1 ) );

    ///Near far ratio
    AddProperty( "NearFarRatio", false, "Set Near-Far Ratio" );
    SetPropertyAttribute( "NearFarRatio", "isUIGroupOnly", false );
    SetPropertyAttribute( "NearFarRatio", "setExpanded", true );
    mPropertyMap["NearFarRatio"]->SignalValueChanged.connect( boost::bind( &PreferencesPropertySet::EnableNearFarRatio, this, _1 ) );

    AddProperty( "NearFarRatio_Ratio", 0.000005, "Ratio" );
    SetPropertyAttribute( "NearFarRatio_Ratio", "minimumValue", 0.00 );
    SetPropertyAttribute( "NearFarRatio_Ratio", "maximumValue", 1.00 );
    mPropertyMap["NearFarRatio_Ratio"]->SetDisabled();
    mPropertyMap["NearFarRatio_Ratio"]->SignalValueChanged.connect( boost::bind( &PreferencesPropertySet::UpdateNearFarRatio, this, _1 ) );

    ///Dargger Scaling
    AddProperty( "DraggerScaling", false, "Enable Dragger Scaling" );
    SetPropertyAttribute( "DraggerScaling", "isUIGroupOnly", false );
    SetPropertyAttribute( "DraggerScaling", "setExpanded", true );
    mPropertyMap["DraggerScaling"]->SignalValueChanged.connect( boost::bind( &PreferencesPropertySet::EnableDraggerScaling, this, _1 ) );

    AddProperty( "DraggerScaling_Scale", 6.00,  "Scale Value" );
    SetPropertyAttribute( "DraggerScaling_Scale", "minimumValue", 0.00 );
    SetPropertyAttribute( "DraggerScaling_Scale", "maximumValue", 100.00 );
    mPropertyMap["DraggerScaling_Scale"]->SetDisabled();
    mPropertyMap["DraggerScaling_Scale"]->SignalValueChanged.connect( boost::bind( &PreferencesPropertySet::UpdateDraggerScaling, this, _1 ) );

    ///Background color
    AddProperty( "UsePreferredBackgroundColor", false, "Use Preferred Background Color" );
    SetPropertyAttribute( "UsePreferredBackgroundColor", "isUIGroupOnly", false );
    SetPropertyAttribute( "UsePreferredBackgroundColor", "setExpanded", true );
    mPropertyMap["UsePreferredBackgroundColor"]->SignalValueChanged.connect( boost::bind( &PreferencesPropertySet::EnableBackgroundColor, this, _1 ) );

    AddProperty( "UsePreferredBackgroundColor_Red", 0.0, "Red" );
    SetPropertyAttribute( "UsePreferredBackgroundColor_Red", "minimumValue", 0.00 );
    SetPropertyAttribute( "UsePreferredBackgroundColor_Red", "maximumValue", 1.00 );
    mPropertyMap["UsePreferredBackgroundColor_Red"]->SetDisabled();
    mPropertyMap["UsePreferredBackgroundColor_Red"]->SignalValueChanged.connect( boost::bind( &PreferencesPropertySet::UpdateBackgroundColor, this, _1 ) );

    AddProperty( "UsePreferredBackgroundColor_Green", 0.0, "Green" );
    SetPropertyAttribute( "UsePreferredBackgroundColor_Green", "minimumValue", 0.00 );
    SetPropertyAttribute( "UsePreferredBackgroundColor_Green", "maximumValue", 1.00 );
    mPropertyMap["UsePreferredBackgroundColor_Green"]->SetDisabled();
    mPropertyMap["UsePreferredBackgroundColor_Green"]->SignalValueChanged.connect( boost::bind( &PreferencesPropertySet::UpdateBackgroundColor, this, _1 ) );

    AddProperty( "UsePreferredBackgroundColor_Blue", 0.0, "Blue" );
    SetPropertyAttribute( "UsePreferredBackgroundColor_Blue", "minimumValue", 0.00 );
    SetPropertyAttribute( "UsePreferredBackgroundColor_Blue", "maximumValue", 1.00 );
    mPropertyMap["UsePreferredBackgroundColor_Blue"]->SetDisabled();
    mPropertyMap["UsePreferredBackgroundColor_Blue"]->SignalValueChanged.connect( boost::bind( &PreferencesPropertySet::UpdateBackgroundColor, this, _1 ) );

    AddProperty( "NavigationZEqual0Lock", false, "Navigation z = 0 Lock" );
    mPropertyMap["NavigationZEqual0Lock"]->SignalValueChanged.connect( boost::bind( &PreferencesPropertySet::UpdateNavEqualZero, this, _1 ) );

    AddProperty( "NavigationZGreater0Lock", false, "Navigation z > 0 Lock" );
    mPropertyMap["NavigationZGreater0Lock"]->SignalValueChanged.connect( boost::bind( &PreferencesPropertySet::UpdateNavGreaterZero, this, _1 ) );

    AddProperty( "ShutDownXplorerOption", false, "Shut Down Xplorer Option" );
    mPropertyMap["ShutDownXplorerOption"]->SignalValueChanged.connect( boost::bind( &PreferencesPropertySet::UpdateShutdownXplorer, this, _1 ) );

    AddProperty( "PhysicsDebugger", false, "Physics Debugger" );
    mPropertyMap["PhysicsDebugger"]->SignalValueChanged.connect( boost::bind( &PreferencesPropertySet::UpdatePhysicsDebugger, this, _1 ) );

    AddProperty( "CADSelection", false, "CAD Selection" );
    mPropertyMap["CADSelection"]->SignalValueChanged.connect( boost::bind( &PreferencesPropertySet::UpdateCADSelection, this, _1 ) );

    AddProperty( "ScriptLogger", false, "Script Logger" );
    mPropertyMap["ScriptLogger"]->SignalValueChanged.connect( boost::bind( &PreferencesPropertySet::UpdateScriptLogger, this, _1 ) );

    AddProperty( "ScreenAlignedNormals", false, "Screen Aligned Normals" );
    mPropertyMap["ScreenAlignedNormals"]->SignalValueChanged.connect( boost::bind( &PreferencesPropertySet::UpdateScreenAlignedNormals, this, _1 ) );
}
////////////////////////////////////////////////////////////////////////////////
void PreferencesPropertySet::EnableNearFarRatio( PropertyPtr property )
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
    
    UpdateNearFarRatio( GetProperty( "NearFarRatio_Ratio" ) );
}
////////////////////////////////////////////////////////////////////////////////
void PreferencesPropertySet::EnableBackgroundColor( PropertyPtr property )
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
    
    UpdateBackgroundColor( property );
}
////////////////////////////////////////////////////////////////////////////////
void PreferencesPropertySet::EnableDraggerScaling( PropertyPtr property )
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
    
    UpdateDraggerScaling( GetProperty( "DraggerScaling_Scale" ) );
}
////////////////////////////////////////////////////////////////////////////////
void PreferencesPropertySet::UpdateBackgroundColor( PropertyPtr property )
{
    double r = boost::any_cast<double>( GetPropertyValue( "UsePreferredBackgroundColor_Red" ) );
    double g = boost::any_cast<double>( GetPropertyValue( "UsePreferredBackgroundColor_Green" ) );
    double b = boost::any_cast<double>( GetPropertyValue( "UsePreferredBackgroundColor_Blue" ) );
    std::vector< double > colors;
    colors.push_back( r );
    colors.push_back( g );
    colors.push_back( b );
    
    m_backgroundColor( boost::any_cast<bool>( GetPropertyValue( "UsePreferredBackgroundColor" ) ), colors );
}
////////////////////////////////////////////////////////////////////////////////
void PreferencesPropertySet::UpdateNavEqualZero( PropertyPtr property )
{
    m_navZEqual0( boost::any_cast<bool>( property->GetValue() ) );
}
////////////////////////////////////////////////////////////////////////////////
void PreferencesPropertySet::UpdateNavGreaterZero( PropertyPtr property )
{
    m_navZGreater0( boost::any_cast<bool>( property->GetValue() ) );
}
////////////////////////////////////////////////////////////////////////////////
void PreferencesPropertySet::UpdateShutdownXplorer( PropertyPtr property )
{
    m_shutdownXplorer( boost::any_cast<bool>( property->GetValue() ) );
}
////////////////////////////////////////////////////////////////////////////////
void PreferencesPropertySet::UpdatePhysicsDebugger( PropertyPtr property )
{
    m_physicsDebugger( boost::any_cast<bool>( property->GetValue() ) );
}
////////////////////////////////////////////////////////////////////////////////
void PreferencesPropertySet::UpdateCADSelection( PropertyPtr property )
{
    m_cadSelection( boost::any_cast<bool>( property->GetValue() ) );
}
////////////////////////////////////////////////////////////////////////////////
void PreferencesPropertySet::UpdateScriptLogger( PropertyPtr property )
{
    m_scriptLogger( boost::any_cast<bool>( property->GetValue() ) );
}
////////////////////////////////////////////////////////////////////////////////
void PreferencesPropertySet::UpdateScreenAlignedNormals( PropertyPtr property )
{
    m_screenAlignedNormals( boost::any_cast<bool>( property->GetValue() ) );
}
////////////////////////////////////////////////////////////////////////////////
void PreferencesPropertySet::UpdateDraggerScaling( PropertyPtr property )
{
    m_draggerScaling( boost::any_cast<bool>( GetPropertyValue( "DraggerScaling" ) ), boost::any_cast<double>( property->GetValue() ) );
}
////////////////////////////////////////////////////////////////////////////////
void PreferencesPropertySet::UpdateNearFarRatio( PropertyPtr property )
{
    m_nearFarRatio( boost::any_cast<bool>( GetPropertyValue( "NearFarRatio" ) ), boost::any_cast<double>( property->GetValue() ) );
}
////////////////////////////////////////////////////////////////////////////////
void PreferencesPropertySet::UpdateLODScaling( PropertyPtr property )
{
    m_lodScaling( boost::any_cast<double>( property->GetValue() ) );
}
////////////////////////////////////////////////////////////////////////////////

