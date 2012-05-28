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
#include <ves/xplorer/data/CADSubNodePropertySet.h>
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
CADSubNodePropertySet::CADSubNodePropertySet()
    :
    CADPropertySet()
{
    mTableName = "CADSubNodePropertySet";

    std::string prependTag( mTableName );
    prependTag.append( " " );
    std::string tag = boost::any_cast<std::string>( GetPropertyValue( "NameTag" ) );
    SetPropertyValue( "NameTag", tag.insert( 0, prependTag ) );

    DisableSkeleton();
}
////////////////////////////////////////////////////////////////////////////////
CADSubNodePropertySet::CADSubNodePropertySet( const CADSubNodePropertySet& orig )
    :
    CADPropertySet( orig )
{
}
////////////////////////////////////////////////////////////////////////////////
CADSubNodePropertySet::~CADSubNodePropertySet()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void CADSubNodePropertySet::DisableSkeleton()
{
    //AddProperty( "Visible", true, "Visible" );

    SetPropertyAttribute( "SurfaceData", "userVisible", false );

    SetPropertyAttribute( "Transform", "userVisible", false );
    SetPropertyAttribute( "Transform_Translation", "userVisible", false );
    SetPropertyAttribute( "Transform_Translation_X", "userVisible", false );
    SetPropertyAttribute( "Transform_Translation_Y", "userVisible", false );
    SetPropertyAttribute( "Transform_Translation_Z", "userVisible", false );
    SetPropertyAttribute( "Transform_Rotation", "userVisible", false );
    SetPropertyAttribute( "Transform_Rotation_X", "userVisible", false );
    SetPropertyAttribute( "Transform_Rotation_Y", "userVisible", false );
    SetPropertyAttribute( "Transform_Rotation_Z", "userVisible", false );
    SetPropertyAttribute( "Transform_Scale", "userVisible", false );
    SetPropertyAttribute( "Transform_Scale_X", "userVisible", false );
    SetPropertyAttribute( "Transform_Scale_Y", "userVisible", false );
    SetPropertyAttribute( "Transform_Scale_Z", "userVisible", false );
    SetPropertyAttribute( "Transform_Scale_Uniform", "userVisible", false );

    /*AddProperty( "Transform", boost::any(), "Transform" );
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
    GetProperty("Transform_Scale_X")->SignalValueChanged.connect( boost::bind( &CADSubNodePropertySet::Scale, this, _1 ) );
    GetProperty("Transform_Scale_Y")->SignalValueChanged.connect( boost::bind( &CADSubNodePropertySet::Scale, this, _1 ) );
    GetProperty("Transform_Scale_Z")->SignalValueChanged.connect( boost::bind( &CADSubNodePropertySet::Scale, this, _1 ) );

    AddProperty( "Transform_Scale_Uniform", false, "Uniform Scaling" );*/

    SetPropertyAttribute( "Physics", "userVisible", false );
    SetPropertyAttribute( "Physics_Mass", "userVisible", false );
    SetPropertyAttribute( "Physics_Friction", "userVisible", false );
    SetPropertyAttribute( "Physics_Restitution", "userVisible", false );
    SetPropertyAttribute( "Physics_MotionType", "userVisible", false );
    SetPropertyAttribute( "Physics_LODType", "userVisible", false );
    SetPropertyAttribute( "Physics_MeshType", "userVisible", false );
    SetPropertyAttribute( "Physics_MeshDecimation", "userVisible", false );

    /*AddProperty( "Physics", false, "Physics Enabled" );

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
    //    mLiveObjects.push_back(p);*/

    SetPropertyAttribute( "Culling", "userVisible", false );

    /*AddProperty( "Culling", 0, "Occlusion Culling" );
    enumValues.clear();
    enumValues.push_back( "Off" );
    enumValues.push_back( "Low" );
    enumValues.push_back( "Medium" );
    enumValues.push_back( "High" );
    SetPropertyAttribute( "Culling", "enumValues", enumValues );*/

    SetPropertyAttribute( "Opacity", "userVisible", false );

    /*AddProperty( "Opacity", 1.0, "Opacity" );
    SetPropertyAttribute( "Opacity", "minimumValue", 0.0 );
    SetPropertyAttribute( "Opacity", "maximumValue", 1.0 );*/


    SetPropertyAttribute( "TransparencyFlag", "userVisible", false );

    //AddProperty( "TransparencyFlag", false, "Make translucent when viz is active" );

    SetPropertyAttribute( "GPS", "userVisible", false );
    SetPropertyAttribute( "GPS_Longitude", "userVisible", false );
    SetPropertyAttribute( "GPS_Latitude", "userVisible", false );
    /*AddProperty( "GPS", false, "Global Positioning" );
    AddProperty( "GPS_Longitude", 0.0 );
    AddProperty( "GPS_Latitude", 0.0 );*/

    /*std::string emptyString;
    AddProperty( "NodePath", emptyString, "Not visible in UI"  );
    SetPropertyAttribute( "NodePath", "userVisible", false );*/

    /*AddProperty( "DynamicAnalysisData", std::string("null"), "Multi-body Dynamics Data" );
    SetPropertyAttribute( "DynamicAnalysisData", "isFilePath", true );
    mPropertyMap["DynamicAnalysisData"]->
        SignalValueChanged.connect( boost::bind( &CADSubNodePropertySet::AddDynamicAnalysisData, this, _1 ) );*/

    /*AddProperty( "Filename", emptyString, "Filename: not visible in UI" );
    SetPropertyAttribute( "Filename", "userVisible", false );*/
}
////////////////////////////////////////////////////////////////////////////////
PropertySetPtr CADSubNodePropertySet::CreateNew()
{
    return PropertySetPtr( new CADSubNodePropertySet() );
}
////////////////////////////////////////////////////////////////////////////////
} // namespace data
} // namespace xplorer
} // namespace ves
