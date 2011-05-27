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
#include <ves/xplorer/data/DatasetPropertySet.h>
#include <ves/xplorer/data/Property.h>

using namespace ves::xplorer::data;

////////////////////////////////////////////////////////////////////////////////
DatasetPropertySet::DatasetPropertySet()
{
    mTableName = "Dataset";

    std::string prependTag( mTableName );
    prependTag.append(" ");
    std::string tag = boost::any_cast<std::string>(GetPropertyValue("NameTag"));
    SetPropertyValue( "NameTag", tag.insert( 0, prependTag ) );

    CreateSkeleton();
}
////////////////////////////////////////////////////////////////////////////////
DatasetPropertySet::DatasetPropertySet( const DatasetPropertySet& orig )
    :
    PropertySet( orig )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
DatasetPropertySet::~DatasetPropertySet()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void DatasetPropertySet::CreateSkeleton()
{
    AddProperty( "SurfaceWrap", false, "Surface Wrap" );

    AddProperty( "BoundingBox", false, "Bounding Box" );

    AddProperty( "ScalarBar", false, "Scalar Bar" );

    AddProperty( "Axes", false, "Axes" );
    SetPropertyAttribute( "Axes", "setExpanded", false );

    AddProperty( "Axes_XLabel", std::string( "X Axis" ), "X axis label" );

    AddProperty( "Axes_YLabel", std::string( "Y Axis" ), "Y axis label" );

    AddProperty( "Axes_ZLabel", std::string( "Z Axis" ), "Z axis label" );


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



    //***** The following properties should all eventually make use of
    // of userVisibile = false to hide them from the user. They are intended
    // to provide persistence and access mechanisms to dataset info without
    // having to go through xplorer directly.

    AddProperty( "Filename", std::string(""), "File name");
    SetPropertyAttribute( "Filename", "userVisible", false );

    std::vector< std::string > stringVector;
    AddProperty( "ScalarNames", stringVector, "Scalar Names" );
    SetPropertyAttribute( "ScalarNames", "userVisible", false );

    AddProperty( "VectorNames", stringVector, "Vector Names" );
    SetPropertyAttribute( "VectorNames", "userVisible", false );

    std::vector< double > doubleVector;
    AddProperty( "ScalarMins", doubleVector, "Scalar Mins" );
    SetPropertyAttribute( "ScalarMins", "userVisible", false );
    AddProperty( "ScalarMaxes", doubleVector, "Scalar Maxes" );
    SetPropertyAttribute( "ScalarMaxes", "userVisible", false );

    AddProperty( "StepLength", 0.0f, "Integration Step Length");
    SetPropertyAttribute( "StepLength", "userVisible", false );

    AddProperty( "TimeStep", 0.0f, "Integration Time Step");
    SetPropertyAttribute( "TimeStep", "userVisible", false );

    AddProperty( "MaxTime", 0.0f, "Max Integration Time" );
    SetPropertyAttribute( "MaxTime", "userVisible", false );

    AddProperty( "Type", 0, "Type" );
    SetPropertyAttribute( "Type", "userVisible", false );

    AddProperty( "PrecomputedDataSliceDir", std::string(""), "Precomputed Data Slice Dir" );
    SetPropertyAttribute( "PrecomputedDataSliceDir", "userVisible", false );

    AddProperty( "PrecomputedSurfaceDir", std::string(""), "Precomputed Surface Dir" );
    SetPropertyAttribute( "PrecomputedSurfaceDir", "userVisible", false );
}
////////////////////////////////////////////////////////////////////////////////
void DatasetPropertySet::EnableLiveProperties( bool live )
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

        p = MakeLiveBasePtr( new MakeLive<const bool>( mUUIDString,
                                                     GetProperty( "SurfaceWrap" ),
                                                     "SetDatasetSurfaceWrap",
                                                     true ));
        mLiveObjects.push_back( p );

        p = MakeLiveBasePtr( new MakeLive<const bool>( mUUIDString,
                                                     GetProperty( "BoundingBox" ),
                                                     "ShowDatasetBBox",
                                                     true ));
        mLiveObjects.push_back( p );

        p = MakeLiveBasePtr( new MakeLive<const bool>( mUUIDString,
                                                       GetProperty( "ScalarBar" ),
                                                       "ShowDatasetScalarBar",
                                                       true ));
        mLiveObjects.push_back( p );

        p = MakeLiveBasePtr( new MakeLive<const bool>( mUUIDString,
                                                       GetProperty( "Axes" ),
                                                       "ShowDatasetAxes",
                                                       true ));
        mLiveObjects.push_back( p );

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
                "TransformDataNode"));
        mLiveObjects.push_back(p);

    }
}
