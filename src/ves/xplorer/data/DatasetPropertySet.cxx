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
#include <ves/xplorer/data/DatasetPropertySet.h>
#include <propertystore/Property.h>
#include <propertystore/MakeLive.h>

#include <ves/xplorer/eventmanager/EventFactory.h>
#include <ves/xplorer/data/DatabaseManager.h>

#include <ves/util/SimpleDataTypeSignalSignatures.h>

#include <boost/filesystem/path.hpp>
#include <boost/filesystem/operations.hpp>
#include <boost/filesystem/convenience.hpp>

using namespace ves::xplorer::data;

////////////////////////////////////////////////////////////////////////////////
DatasetPropertySet::DatasetPropertySet()
{
    SetDataManager( DatabaseManager::instance()->GetDataManager() );
    SetTypeName( "Dataset" );

    std::string prependTag( GetTypeName() );
    prependTag.append( " " );
    std::string tag = boost::any_cast<std::string>( GetPropertyValue( "NameTag" ) );
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
    AddProperty( "Visible", true, "Visible" );

    AddProperty( "SurfaceWrap", false, "Surface Wrap" );

    AddProperty( "BoundingBox", false, "Bounding Box" );

    AddProperty( "ScalarBar", false, "Scalar Bar" );

    AddProperty( "TimeDataset", false, "Time Based" );

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

    AddProperty( "Transform_Scale_Uniform", true, "Uniform Scaling" );

    AddProperty( "Transform_Scale_X", 1.0, "x" );
    AddProperty( "Transform_Scale_Y", 1.0, "y" );
    AddProperty( "Transform_Scale_Z", 1.0, "z" );
    SetPropertyAttribute( "Transform_Scale_X", "DisplayPrecision", 6 );
    SetPropertyAttribute( "Transform_Scale_Y", "DisplayPrecision", 6 );
    SetPropertyAttribute( "Transform_Scale_Z", "DisplayPrecision", 6 );
    GetProperty( "Transform_Scale_X" )->
    SignalValueChanged.connect( boost::bind( &DatasetPropertySet::Scale, this, _1 ) );
    GetProperty( "Transform_Scale_Y" )->
    SignalValueChanged.connect( boost::bind( &DatasetPropertySet::Scale, this, _1 ) );
    GetProperty( "Transform_Scale_Z" )->
    SignalValueChanged.connect( boost::bind( &DatasetPropertySet::Scale, this, _1 ) );

    //***** The following properties should all eventually make use of
    // of userVisibile = false to hide them from the user. They are intended
    // to provide persistence and access mechanisms to dataset info without
    // having to go through xplorer directly.

    AddProperty( "Filename", std::string( "" ), "File name" );
    SetPropertyAttribute( "Filename", "userVisible", false );

    //AddProperty( "LongFilename", std::string(""), "Long File name");
    //SetPropertyAttribute( "LongFilename", "userVisible", false );

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

    AddProperty( "StepLength", 0.0f, "Integration Step Length" );
    SetPropertyAttribute( "StepLength", "userVisible", false );

    AddProperty( "TimeStep", 0.0f, "Integration Time Step" );
    SetPropertyAttribute( "TimeStep", "userVisible", false );

    AddProperty( "MaxTime", 0.0f, "Max Integration Time" );
    SetPropertyAttribute( "MaxTime", "userVisible", false );

    AddProperty( "Type", 0, "Type" );
    SetPropertyAttribute( "Type", "userVisible", false );

    AddProperty( "PrecomputedDataSliceDir", std::string( "" ), "Precomputed Data Slice Dir" );
    SetPropertyAttribute( "PrecomputedDataSliceDir", "userVisible", false );

    AddProperty( "PrecomputedSurfaceDir", std::string( "" ), "Precomputed Surface Dir" );
    SetPropertyAttribute( "PrecomputedSurfaceDir", "userVisible", false );

    AddProperty( "TBETScalarChooser", std::string( "null" ), "TBET Scalar Chooser" );
    SetPropertyAttribute( "TBETScalarChooser", "isFilePath", true );
    GetProperty( "TBETScalarChooser" )->SignalValueChanged.connect( boost::bind( &DatasetPropertySet::LoadVTIScalars, this, _1 ) );

    AddProperty( "TBETScalarNames", stringVector, "TBET Scalar Names" );
    SetPropertyAttribute( "TBETScalarNames", "userVisible", false );
}
////////////////////////////////////////////////////////////////////////////////
void DatasetPropertySet::EnableLiveProperties( bool live )
{
    m_isLive = live;

    if( !live )
    {
        // Clearing list will allow live objs to go out of scope and autodelete
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
        propertystore::MakeLiveBasePtr p;

        p = propertystore::MakeLiveBasePtr( new propertystore::MakeLive<bool const&>( m_UUIDString,
                             GetProperty( "SurfaceWrap" ),
                             "SetDatasetSurfaceWrap" ) );
        m_liveObjects.push_back( p );

        p = propertystore::MakeLiveBasePtr( new propertystore::MakeLive<bool const&>( m_UUIDString,
                             GetProperty( "BoundingBox" ),
                             "ShowDatasetBBox" ) );
        m_liveObjects.push_back( p );

        p = propertystore::MakeLiveBasePtr( new propertystore::MakeLive<bool const&>( m_UUIDString,
                             GetProperty( "ScalarBar" ),
                             "ShowDatasetScalarBar" ) );
        m_liveObjects.push_back( p );

        p = propertystore::MakeLiveBasePtr( new propertystore::MakeLive<bool const&>( m_UUIDString,
                             GetProperty( "Axes" ),
                             "ShowDatasetAxes" ) );
        m_liveObjects.push_back( p );

        // Link up all the transform properties so that a single signal named
        // "TransformCADNode" is fired whenever any of the values changes.
        std::vector< propertystore::PropertyPtr > transformLink;
        transformLink.push_back( GetProperty( "Transform_Translation_X" ) );
        transformLink.push_back( GetProperty( "Transform_Translation_Y" ) );
        transformLink.push_back( GetProperty( "Transform_Translation_Z" ) );
        transformLink.push_back( GetProperty( "Transform_Rotation_Z" ) );
        transformLink.push_back( GetProperty( "Transform_Rotation_X" ) );
        transformLink.push_back( GetProperty( "Transform_Rotation_Y" ) );
        transformLink.push_back( GetProperty( "Transform_Scale_X" ) );
        transformLink.push_back( GetProperty( "Transform_Scale_Y" ) );
        transformLink.push_back( GetProperty( "Transform_Scale_Z" ) );
        p = propertystore::MakeLiveBasePtr( new propertystore::MakeLiveLinked< double >(
                                 m_UUIDString,
                                 transformLink,
                                 "TransformDataNode" ) );
        m_liveObjects.push_back( p );

        p = propertystore::MakeLiveBasePtr( new propertystore::MakeLive<bool const&>( m_UUIDString,
                             GetProperty( "Visible" ),
                             "ToggleDataNode" ) );
        m_liveObjects.push_back( p );

        p = propertystore::MakeLiveBasePtr( new propertystore::MakeLive<bool const&>( m_UUIDString,
                            GetProperty( "TimeDataset" ),
                            "TimeDatasetChanged" ) );
        m_liveObjects.push_back( p );
    }
}
////////////////////////////////////////////////////////////////////////////////
void DatasetPropertySet::LoadVTIScalars( propertystore::PropertyPtr& )
{
    if( !m_isLive )
    {
        return;
    }

    std::string const filename =
        boost::any_cast<std::string>( GetPropertyValue( "TBETScalarChooser" ) );

    std::string extension( boost::filesystem::extension( filename ) );
    //if the file extension is vti then we will add it as a vti scalar for this
    //dataset
    if( extension != ".vti" )
    {
        return;
    }


    //Now extract the directory from the filename string handed back from the
    //file selection dialog
    boost::filesystem::path tmp( filename );
    std::string const directory = tmp.remove_filename().string();

    PSVectorOfStrings enumValues =
        boost::any_cast< std::vector<std::string> >(
            GetPropertyValue( "TBETScalarNames" ) );

    std::string scalarName = tmp.filename().string();
    enumValues.push_back( scalarName );
    SetPropertyValue( "TBETScalarNames", enumValues );

    std::cout << "DatasetPropertySet::LoadVTIScalars: "  << directory << " " << filename << " " << scalarName << std::endl;

    ves::util::TwoStringSignal_type* addTexture =
        reinterpret_cast< ves::util::TwoStringSignal_type* >
        ( xplorer::eventmanager::EventFactory::instance()->GetSignal( "DatasetPropertySet.TBETAddScalarSignal" ) );

    std::string const nodeID = GetUUIDAsString();
    addTexture->signal( nodeID, directory );

    Save();
}
////////////////////////////////////////////////////////////////////////////////
void DatasetPropertySet::Scale( propertystore::PropertyPtr& property )
{
    bool uniform = boost::any_cast<bool>( GetPropertyValue( "Transform_Scale_Uniform" ) );
    if( uniform )
    {
        double scale =  property->extract<double>();
        std::string name = property->GetAttribute( "nameInSet" )->extract<std::string>();
        if( name == "Transform_Scale_X" )
        {
            SetPropertyValue( "Transform_Scale_Y", scale );
            SetPropertyValue( "Transform_Scale_Z", scale );
        }
        else if( name == "Transform_Scale_Y" )
        {
            SetPropertyValue( "Transform_Scale_X", scale );
            SetPropertyValue( "Transform_Scale_Z", scale );
        }
        else if( name == "Transform_Scale_Z" )
        {
            SetPropertyValue( "Transform_Scale_X", scale );
            SetPropertyValue( "Transform_Scale_Y", scale );
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
propertystore::PropertySetPtr DatasetPropertySet::CreateNew()
{
    return propertystore::PropertySetPtr( new DatasetPropertySet() );
}

////////////////////////////////////////////////////////////////////////////////
