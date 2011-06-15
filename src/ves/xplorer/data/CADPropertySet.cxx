#/*************** <auto-copyright.rb BEGIN do not edit this line> **************
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
#include <ves/xplorer/data/CADPropertySet.h>
#include <ves/xplorer/data/Property.h>
#include <ves/xplorer/data/MakeLive.h>
#include <ves/xplorer/data/DatabaseManager.h>

#include <ves/xplorer/eventmanager/EventManager.h>
#include <ves/xplorer/eventmanager/SignalWrapper.h>

#include <boost/bind.hpp>
#include <boost/concept_check.hpp>

#include <iostream>

namespace ves
{
namespace xplorer
{
namespace data
{


////////////////////////////////////////////////////////////////////////////////
CADPropertySet::CADPropertySet()
{ 
    mTableName = "CADPropertySet";

    std::string prependTag( mTableName );
    prependTag.append(" ");
    std::string tag = boost::any_cast<std::string>(GetPropertyValue("NameTag"));
    SetPropertyValue( "NameTag", tag.insert( 0, prependTag ) );

    ///Tie up the animation controls
    {
        std::string name("CADPropertySet");
        name += boost::lexical_cast<std::string>( this );
        name += ".CADAnimation";
        
        eventmanager::EventManager::instance()->RegisterSignal(
            new eventmanager::SignalWrapper< AddAnimationDataSignal_type >( &m_animateCAD ),
            name, eventmanager::EventManager::unspecified_SignalType );
    }
    
    CreateSkeleton();
}
////////////////////////////////////////////////////////////////////////////////
CADPropertySet::CADPropertySet( const CADPropertySet& orig )
    :
    PropertySet( orig )
{
}
////////////////////////////////////////////////////////////////////////////////
CADPropertySet::~CADPropertySet()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void CADPropertySet::CreateSkeleton()
{
    AddProperty( "Visible", true, "Visible" );


    AddProperty( "Transform", boost::any(), "Transform" );
    SetPropertyAttribute( "Transform", "isUIGroupOnly", true );
    SetPropertyAttribute( "Transform", "setExpanded", false );

    AddProperty( "Transform_Translation", boost::any(), "Translation" );
    SetPropertyAttribute( "Transform_Translation", "isUIGroupOnly", true );

    AddProperty( "Transform_Translation_X", 0.0, "x" );
    AddProperty( "Transform_Translation_Y", 0.0, "y" );
    AddProperty( "Transform_Translation_Z", 0.0, "z" );
    SetPropertyAttribute( "Transform_Translation_X", "DisplayPrecision", 4 );
    SetPropertyAttribute( "Transform_Translation_Y", "DisplayPrecision", 4 );
    SetPropertyAttribute( "Transform_Translation_Z", "DisplayPrecision", 4 );
    
    AddProperty( "Transform_Rotation", boost::any(), "Rotation" );
    SetPropertyAttribute( "Transform_Rotation", "isUIGroupOnly", true );

    AddProperty( "Transform_Rotation_X", 0.0, "x" );
    AddProperty( "Transform_Rotation_Y", 0.0, "y" );
    AddProperty( "Transform_Rotation_Z", 0.0, "z" );

    AddProperty( "Transform_Scale", boost::any(), "Scale" );
    SetPropertyAttribute( "Transform_Scale", "isUIGroupOnly", true );

    AddProperty( "Transform_Scale_X", 0.0, "x" );
    AddProperty( "Transform_Scale_Y", 0.0, "y" );
    AddProperty( "Transform_Scale_Z", 0.0, "z" );
    SetPropertyAttribute( "Transform_Scale_X", "DisplayPrecision", 6 );
    SetPropertyAttribute( "Transform_Scale_Y", "DisplayPrecision", 6 );
    SetPropertyAttribute( "Transform_Scale_Z", "DisplayPrecision", 6 );



    AddProperty( "Transform_Scale_Uniform", false, "Uniform Scaling" );

    AddProperty( "Physics", false, "Physics Enabled" );

    AddProperty( "Physics_Mass", 1.00, "Mass" );
    AddProperty( "Physics_Friction", 0.00, "Coeff. of Friction" );
    AddProperty( "Physics_Restitution", 0.00, "Coeff. of Restitution" );

    PSVectorOfStrings enumValues;

    AddProperty( "Physics_MotionType", 0, "Motion Type" );
    enumValues.push_back( "None" );
    enumValues.push_back( "Static" );
    enumValues.push_back( "Dynamic" );
    SetPropertyAttribute( "Physics_MotionType", "enumValues", enumValues );

    AddProperty( "Physics_LODType", 0, "LOD Type" );
    enumValues.clear();
    enumValues.push_back( "None" );
    enumValues.push_back( "Overall" );
    enumValues.push_back( "Compound" );
    SetPropertyAttribute( "Physics_LODType", "enumValues", enumValues );

    AddProperty( "Physics_MeshType", 0, "Mesh Type" );
    enumValues.clear();
    enumValues.push_back( "None" );
    enumValues.push_back( "Box" );
    enumValues.push_back( "Sphere" );
    enumValues.push_back( "Cylinder" );
    enumValues.push_back( "Mesh" );
    SetPropertyAttribute( "Physics_MeshType", "enumValues", enumValues );

    AddProperty( "Physics_MeshDecimation", 0, "Mesh Decimation" );
    enumValues.clear();
    enumValues.push_back( "Exact" );
    enumValues.push_back( "Low" );
    enumValues.push_back( "Medium" );
    enumValues.push_back( "High" );
    SetPropertyAttribute( "Physics_MeshDecimation", "enumValues", enumValues );

//    std::vector< PropertyPtr > physicsLink;
//    physicsLink.push_back( GetProperty("Physics_MotionType") );
//    physicsLink.push_back( GetProperty("Physics_LODType") );
//    physicsLink.push_back( GetProperty("Physics_MeshType") );
//    physicsLink.push_back( GetProperty("Physics_MeshDecimation") );
//    p = MakeLiveBasePtr(new MakeLiveLinked< std::string >(
//            mUUIDString,
//            physicsLink,
//            "SetCADPhysicsMesh"));
//    mLiveObjects.push_back(p);

    AddProperty( "Culling", 0, "Occlusion Culling" );
    enumValues.clear();
    enumValues.push_back( "Off" );
    enumValues.push_back( "Low" );
    enumValues.push_back( "Medium" );
    enumValues.push_back( "High" );
    SetPropertyAttribute( "Culling", "enumValues", enumValues );

    AddProperty( "Opacity", 1.0, "Opacity" );
    SetPropertyAttribute( "Opacity", "minimumValue", 0.0 );
    SetPropertyAttribute( "Opacity", "maximumValue", 1.0 );


    AddProperty( "TransparencyFlag", false, "Make translucent when viz is active" );

    AddProperty( "GPS", false, "Global Positioning" );
    AddProperty( "GPS_Longitude", 0.0 );
    AddProperty( "GPS_Latitude", 0.0 );

    std::string emptyString;
    AddProperty( "NodePath", emptyString, "Not visible in UI"  );
    SetPropertyAttribute( "NodePath", "userVisible", false );
    
    AddProperty( "DynamicAnalysisData", std::string("null"), "Multi-body Dynamics Data" );
    SetPropertyAttribute( "DynamicAnalysisData", "isFilePath", true );
    mPropertyMap["DynamicAnalysisData"]->
        SignalValueChanged.connect( boost::bind( &CADPropertySet::AddDynamicAnalysisData, this, _1 ) );
}
////////////////////////////////////////////////////////////////////////////////
void CADPropertySet::AddDynamicAnalysisData( PropertyPtr property )
{
    std::string fileName = boost::any_cast<std::string>( GetPropertyValue( "DynamicAnalysisData" ) );
    std::string nodeType = "Part";
    std::string modeID = GetUUIDAsString();
    
    m_animateCAD( nodeType, fileName, modeID );
    
    // All properties here are live; save to db whenever they change.
    WriteToDatabase();
}
////////////////////////////////////////////////////////////////////////////////
void CADPropertySet::EnableLiveProperties( bool live )
{
    if( !live )
    {
        // Clearing list will allow live objs to go out of scope and autodelete
        mLiveObjects.clear();
        return;
    }
    else if( !mLiveObjects.empty() )
    {
        // Properties are already live
        return;
    }
    else
    {
        MakeLiveBasePtr p;
        p = MakeLiveBasePtr(new MakeLive<bool>( mUUIDString,
                                                     GetProperty("Visible"),
                                                     "ToggleCADNode" ));
        mLiveObjects.push_back(p);

        // Link up all the transform properties so that a single signal named
        // "TransformCADNode" is fired whenever any of the values changes.
        std::vector< PropertyPtr > transformLink;
        transformLink.push_back( GetProperty( "Transform_Translation_X" ) );
        transformLink.push_back( GetProperty( "Transform_Translation_Y" ) );
        transformLink.push_back( GetProperty( "Transform_Translation_Z" ) );
        transformLink.push_back( GetProperty( "Transform_Rotation_X" ) );
        transformLink.push_back( GetProperty( "Transform_Rotation_Y" ) );
        transformLink.push_back( GetProperty( "Transform_Rotation_Z" ) );
        transformLink.push_back( GetProperty( "Transform_Scale_X" ) );
        transformLink.push_back( GetProperty( "Transform_Scale_Y" ) );
        transformLink.push_back( GetProperty( "Transform_Scale_Z" ) );
        p = MakeLiveBasePtr(new MakeLiveLinked< double >(
                mUUIDString,
                transformLink,
                "TransformCADNode"));
        mLiveObjects.push_back(p);

        p = MakeLiveBasePtr(new MakeLive<double>( mUUIDString,
                                                     GetProperty("Opacity"),
                                                     "SetOpacityOnCADNode" ));
        mLiveObjects.push_back(p);
    }
}

////////////////////////////////////////////////////////////////////////////////
} // namespace data
} // namespace xplorer
} // namespace ves
