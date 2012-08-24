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
#include <ves/xplorer/data/VectorPlanePropertySet.h>
#include <ves/xplorer/data/DatasetPropertySet.h>
#include <propertystore/Property.h>
#include <propertystore/MakeLive.h>

#include <boost/bind.hpp>
#include <boost/concept_check.hpp>
#include <boost/lexical_cast.hpp>

#include <iostream>
#include <ves/xplorer/data/DatabaseManager.h>

using namespace ves::xplorer::data;
////////////////////////////////////////////////////////////////////////////////
VectorPlanePropertySet::VectorPlanePropertySet()
    :
    VizBasePropertySet()
{
    SetDataManager( DatabaseManager::instance()->GetDataManager() );
    SetTypeName( "VectorPlane" );
    RegisterPropertySet( GetTypeName() );

    CreateSkeleton();
}
////////////////////////////////////////////////////////////////////////////////
VectorPlanePropertySet::VectorPlanePropertySet( const VectorPlanePropertySet& orig )
    :
    VizBasePropertySet( orig )
{
}
////////////////////////////////////////////////////////////////////////////////
VectorPlanePropertySet::~VectorPlanePropertySet()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
propertystore::PropertySetPtr VectorPlanePropertySet::CreateNew()
{
    return propertystore::PropertySetPtr( new VectorPlanePropertySet );
}
////////////////////////////////////////////////////////////////////////////////
void VectorPlanePropertySet::CreateSkeleton()
{
    {
        AddProperty( "Hide", false, "Toggle Viz Off" );
        const std::string slotName =
            boost::lexical_cast<std::string>( this ) + ".HideVizFeature";
        std::vector< propertystore::PropertyPtr > dataLink;
        dataLink.push_back( GetProperty( "Hide" ) );
        propertystore::MakeLiveBasePtr p(
            new propertystore::MakeLiveLinked< bool >( m_UUIDString, dataLink,
                                        slotName ) );
        m_liveObjects.push_back( p );
    }

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

    AddProperty( "Direction", std::string(""), "Direction" );
    enumValues.clear();
    enumValues.push_back( "x" );
    enumValues.push_back( "y" );
    enumValues.push_back( "z" );
    enumValues.push_back( "By Wand" );
    enumValues.push_back( "All" );
    enumValues.push_back( "By Surface" );
    SetPropertyAttribute( "Direction", "enumValues", enumValues );

    AddProperty( "DataMapping", std::string(""), "Data Mapping" );
    enumValues.clear();
    enumValues.push_back( "Map Scalar Data" );
    enumValues.push_back( "Map Volume Flux Data" );
    SetPropertyAttribute( "DataMapping", "enumValues", enumValues );

    AddProperty( "Mode", std::string(""), "Mode" );
    enumValues.clear();
    enumValues.push_back( "Specify a Single Plane" );
    enumValues.push_back( "Use All Precomputed Surfaces" );
    SetPropertyAttribute( "Mode", "enumValues", enumValues );

    AddProperty( "UseGPUTools", false, "Use GPU Tools" );

    // Connect SignalValueChanged of "Mode" to a function that enables and disables
    // its sub-properties as appropriate
    propertystore::PropertyPtr mode = GetProperty( "Mode" );
    if( mode )
    {
        mode->SignalValueChanged.connect( boost::bind( &VizBasePropertySet::UpdateModeOptions, this, _1 ) );
    }

    AddProperty( "Mode_UseNearestPrecomputedPlane", false, "Use Nearest Precomputed Plane" );

    AddProperty( "Mode_CyclePrecomputedSurfaces", false, "Cycle Precomputed Surfaces" );
    // We disable this one by default since the selected Mode,
    // "Specify a Single Plane", does not support this option.
    GetProperty( "Mode_CyclePrecomputedSurfaces" )->SetDisabled();


    AddProperty( "PlaneLocation", 0.00, "Plane Location" );
    SetPropertyAttribute( "PlaneLocation", "minimumValue", 0.00 );
    SetPropertyAttribute( "PlaneLocation", "maximumValue", 100.00 );

    AddProperty( "Advanced", boost::any(), "Advanced" );
    SetPropertyAttribute( "Advanced", "isUIGroupOnly", true );

    AddProperty( "Advanced_VectorThreshold", boost::any(), "Vector Threshold" );
    SetPropertyAttribute( "Advanced_VectorThreshold", "isUIGroupOnly", true );
    SetPropertyAttribute( "Advanced_VectorThreshold", "setExpanded", true );
    AddProperty( "Advanced_VectorThreshold_Min",   1.0f, "Vector Threshold Min" );
    AddProperty( "Advanced_VectorThreshold_Max", 100.0f, "Vector Threshold Max" );
    //SetPropertyAttribute( "Advanced_VectorThreshold", "minimumValue", 0.0 );
    //SetPropertyAttribute( "Advanced_VectorThreshold", "maximumValue", 1.0 );

    AddProperty( "Advanced_VectorScale", 200.0, "Vector Scale" );
    SetPropertyAttribute( "Advanced_VectorScale", "minimumValue",   1.0 );
    SetPropertyAttribute( "Advanced_VectorScale", "maximumValue", 400.0 );

    AddProperty( "Advanced_VectorRatio", 1.0, "Vector Ratio" );
    SetPropertyAttribute( "Advanced_VectorRatio", "minimumValue",   1.0 );
    SetPropertyAttribute( "Advanced_VectorRatio", "maximumValue", 200.0 );

    AddProperty( "Advanced_ScaleByVectorMagnitude", false, "Scale By Vector Magnitude" );

    {
        AddProperty( "Advanced_Greyscale", false, "Greyscale" );
        /*std::vector< propertystore::PropertyPtr > greyscale;
        greyscale.push_back( GetProperty( "Advanced_Greyscale" ) );
        const std::string slotName =
            boost::lexical_cast<std::string>( this ) +".SetVectorPlaneGreyscale";
        propertystore::MakeLiveBasePtr p( new propertystore::MakeLiveLinked< bool >(
                m_UUIDString,
                greyscale,
                slotName ) );
        m_liveObjects.push_back( p );*/
    }
}
////////////////////////////////////////////////////////////////////////////////
