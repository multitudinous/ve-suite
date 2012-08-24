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
#include <ves/xplorer/data/IsosurfacePropertySet.h>
#include <ves/xplorer/data/DatasetPropertySet.h>
#include <propertystore/Property.h>
#include <propertystore/MakeLive.h>

#include <boost/bind.hpp>
#include <boost/concept_check.hpp>

#include <iostream>
#include <ves/xplorer/data/DatabaseManager.h>

using namespace ves::xplorer::data;
////////////////////////////////////////////////////////////////////////////////
IsosurfacePropertySet::IsosurfacePropertySet()
{
    SetDataManager( DatabaseManager::instance()->GetDataManager() );
    SetTypeName( "Isosurface" );
    RegisterPropertySet( GetTypeName() );

    CreateSkeleton();
}
////////////////////////////////////////////////////////////////////////////////
IsosurfacePropertySet::IsosurfacePropertySet( const IsosurfacePropertySet& orig )
    :
    VizBasePropertySet( orig )
{
}
////////////////////////////////////////////////////////////////////////////////
IsosurfacePropertySet::~IsosurfacePropertySet()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
propertystore::PropertySetPtr IsosurfacePropertySet::CreateNew()
{
    return propertystore::PropertySetPtr( new IsosurfacePropertySet() );
}
////////////////////////////////////////////////////////////////////////////////
void IsosurfacePropertySet::CreateSkeleton()
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

    AddProperty( "ColorByScalar", std::string(""), "Color By Scalar" );
    enumValues.clear();
    enumValues.push_back( "Select Scalar Data" );
    SetPropertyAttribute( "ColorByScalar", "enumValues", enumValues );

    AddProperty( "ColorByScalar_ScalarRange", boost::any(), "Scalar Range" );
    SetPropertyAttribute( "ColorByScalar_ScalarRange", "isUIGroupOnly", true );
    SetPropertyAttribute( "ColorByScalar_ScalarRange", "setExpanded", true );

    AddProperty( "ColorByScalar_ScalarRange_Min", 0.0, "Min" );
    GetProperty( "ColorByScalar_ScalarRange_Min" )->SetDisabled();

    AddProperty( "ColorByScalar_ScalarRange_Max", 1.0, "Max" );
    GetProperty( "ColorByScalar_ScalarRange_Max" )->SetDisabled();

    GetProperty( "ColorByScalar" )->SignalValueChanged.connect( boost::bind( &IsosurfacePropertySet::UpdateColorByScalarDataRange, this, _1 ) );
    GetProperty( "ColorByScalar_ScalarRange_Min" )->SignalRequestValidation.connect( boost::bind( &IsosurfacePropertySet::ValidateColorByScalarMinMax, this, _1, _2 ) );
    GetProperty( "ColorByScalar_ScalarRange_Max" )->SignalRequestValidation.connect( boost::bind( &IsosurfacePropertySet::ValidateColorByScalarMinMax, this, _1, _2 ) );

    AddProperty( "IsosurfaceValue", 50.0, "Isosurface Value" );
    SetPropertyAttribute( "IsosurfaceValue", "minimumValue",   0.0 );
    SetPropertyAttribute( "IsosurfaceValue", "maximumValue", 100.0 );

    AddProperty( "UseGPUTools", false, "Use GPU Tools" );

    AddProperty( "Advanced", boost::any(), "Advanced" );
    SetPropertyAttribute( "Advanced", "isUIGroupOnly", true );

    {
        AddProperty( "Advanced_Greyscale", false, "Greyscale" );
        /*std::vector< propertystore::PropertyPtr > greyscale;
        greyscale.push_back( GetProperty( "Advanced_Greyscale" ) );
        const std::string slotName =
            boost::lexical_cast<std::string>( this ) +".SetIsosurfaceGreyscale";
        propertystore::MakeLiveBasePtr p( new propertystore::MakeLiveLinked< bool >(
                m_UUIDString,
                greyscale,
                slotName ) );
        m_liveObjects.push_back( p );*/
    }

    // Now that DataSet has choices loaded, force an update on the available
    // scalar and vector data
    propertystore::PropertyPtr nullPtr;
    UpdateScalarDataOptions( nullPtr );
    //UpdateVectorDataOptions( nullPtr );
}
////////////////////////////////////////////////////////////////////////////////
void IsosurfacePropertySet::UpdateColorByScalarDataRange( propertystore::PropertyPtr property )
{
    boost::ignore_unused_variable_warning( property );

    GetProperty( "ColorByScalar_ScalarRange_Min" )->SetEnabled();
    GetProperty( "ColorByScalar_ScalarRange_Max" )->SetEnabled();

    // Load the current Dataset and get the list of min and max values for its scalars
    std::string selectedDataset = boost::any_cast<std::string > ( GetPropertyValue( "DataSet" ) );
    DatasetPropertySet dataset;
    dataset.LoadByKey( "Filename", selectedDataset );
    std::vector<double> mins = boost::any_cast< std::vector<double> >( dataset.GetPropertyValue( "ScalarMins" ) );
    std::vector<double> maxes = boost::any_cast< std::vector<double> >( dataset.GetPropertyValue( "ScalarMaxes" ) );

    // DataSet_ScalarData is an exact copy of the ScalarNames property of the Dataset,
    // so its number in the enum will be the same as the index into the min and max
    // lists
    int index = GetPropertyAttributeValue< int >( "ColorByScalar", "enumCurrentIndex" );

    if( ( !mins.empty() ) && ( !maxes.empty() ) )
    {
        double min = mins.at( index );
        double max = maxes.at( index );

        // Update the upper and lower bounds of Min and Max first so that
        // boundary values will be allowed!
        SetPropertyAttribute( "ColorByScalar_ScalarRange_Min", "minimumValue", min );
        SetPropertyAttribute( "ColorByScalar_ScalarRange_Min", "maximumValue", max );
        SetPropertyAttribute( "ColorByScalar_ScalarRange_Max", "minimumValue", min );
        SetPropertyAttribute( "ColorByScalar_ScalarRange_Max", "maximumValue", max );

        // Set min and max to the lower and upper boundary values, respectively
        SetPropertyValue( "ColorByScalar_ScalarRange_Min", min );
        SetPropertyValue( "ColorByScalar_ScalarRange_Max", max );
    }
}
////////////////////////////////////////////////////////////////////////////////
bool IsosurfacePropertySet::ValidateColorByScalarMinMax( propertystore::PropertyPtr property, boost::any value )
{
    propertystore::PropertyPtr min = GetProperty( "ColorByScalar_ScalarRange_Min" );
    propertystore::PropertyPtr max = GetProperty( "ColorByScalar_ScalarRange_Max" );

    double castMin, castMax;

    if( property == min )
    {
        castMin = boost::any_cast<double>( value );
        castMax = boost::any_cast<double>( max->GetValue() );
    }
    else
    {
        castMin = boost::any_cast<double>( min->GetValue() );
        castMax = boost::any_cast<double>( value );
    }

    if( castMin < castMax )
    {
        return true;
    }
    else
    {
        return false;
    }
}
////////////////////////////////////////////////////////////////////////////////
