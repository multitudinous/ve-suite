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
#include <ves/xplorer/data/VizBasePropertySet.h>
#include <ves/xplorer/data/DatasetPropertySet.h>
#include <ves/xplorer/data/Property.h>

#include <ves/util/Exception.h>

#include <boost/bind.hpp>
#include <boost/concept_check.hpp>

#include <iostream>

#include <ves/xplorer/data/DatabaseManager.h>
#include <ves/xplorer/eventmanager/EventFactory.h>

using namespace ves::xplorer::data;
////////////////////////////////////////////////////////////////////////////////
VizBasePropertySet::VizBasePropertySet()
{
    /*
    ves::xplorer::eventmanager::EventManager* evm =
        ves::xplorer::eventmanager::EventManager::instance();
    using ves::xplorer::eventmanager::SignalWrapper;
    
    ///Setup the delete viz feature
    {
        std::string signalName = "VizBasePropertySet" +
            boost::lexical_cast<std::string>( this ) + ".DeleteVizFeature";
        evm->RegisterSignal(
            new SignalWrapper< ves::util::StringSignal_type >( &m_deleteVizSignal ),
            signalName, ves::xplorer::eventmanager::EventManager::unspecified_SignalType );
    }
    /Setup the add viz feature
    {
        std::string signalName = "VizBasePropertySet" +
            boost::lexical_cast<std::string>( this ) + ".AddVizFeature";
        evm->RegisterSignal(
            new SignalWrapper< ves::util::TwoStringSignal_type >( &m_addVizSignal ),
            signalName, ves::xplorer::eventmanager::EventManager::unspecified_SignalType );
    }*/

    // Grab pointers to addVizFeature and DeleteVizFeature signals from EventFactory.
    m_addVizSignal =
        reinterpret_cast< eventmanager::SignalWrapper< ves::util::TwoStringSignal_type >* >
        ( eventmanager::EventFactory::instance()->GetSignal( "VizBasePropertySet.AddVizFeature" ) )
        ->mSignal;

    m_deleteVizSignal =
        reinterpret_cast< eventmanager::SignalWrapper< ves::util::StringSignal_type >* >
        ( eventmanager::EventFactory::instance()->GetSignal( "VizBasePropertySet.DeleteVizFeature" ) )
        ->mSignal;
}
////////////////////////////////////////////////////////////////////////////////
VizBasePropertySet::VizBasePropertySet( const VizBasePropertySet& orig )
    :
    PropertySet( orig )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
VizBasePropertySet::~VizBasePropertySet()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void VizBasePropertySet::RegisterPropertySet( std::string const& tableName )
{
    std::string prependTag( tableName );
    prependTag.append(" ");
    std::string tag = boost::any_cast<std::string>(GetPropertyValue("NameTag"));
    SetPropertyValue( "NameTag", tag.insert( 0, prependTag ) );
}
////////////////////////////////////////////////////////////////////////////////
bool VizBasePropertySet::DeleteFromDatabase( Poco::Data::Session* const session, 
    std::string const& TableName )
{
    //m_deleteVizSignal( GetUUIDAsString() );
    m_deleteVizSignal->operator()( GetUUIDAsString() );
    ///Send the signal to xplorer to tell it to remove the viz feature
    return PropertySet::DeleteFromDatabase( session, TableName );
}
////////////////////////////////////////////////////////////////////////////////
bool VizBasePropertySet::WriteToDatabase( Poco::Data::Session* const session, 
    std::string const& TableName, Poco::Data::Statement& statement )
{
    bool temp = PropertySet::WriteToDatabase( session, TableName, statement );

//    m_addVizSignal( GetUUIDAsString(), TableName );
    m_addVizSignal->operator()( GetUUIDAsString(), TableName );

    ///Send the signal to xplorer to tell it to add the viz feature
    return temp;
}
////////////////////////////////////////////////////////////////////////////////
/*void VizBasePropertySet::CreateSkeleton()
{
    AddProperty( "DataSet", 0, "Data Set" );
    PSVectorOfStrings enumValues;

    AddProperty( "DataSet_ScalarData", 0, "Scalar Data" );
    // Dummy value to ensure this gets set up as an enum
    enumValues.push_back( "Select Scalar Data" );
    SetPropertyAttribute( "DataSet_ScalarData", "enumValues", enumValues );
    mPropertyMap["DataSet"]->SignalValueChanged.connect( boost::bind( &VizBasePropertySet::UpdateScalarDataOptions, this, _1 ) );

    AddProperty( "DataSet_ScalarRange", boost::any(), "Scalar Range" );
    SetPropertyAttribute( "DataSet_ScalarRange", "isUIGroupOnly", true );
    SetPropertyAttribute( "DataSet_ScalarRange", "setExpanded", true );

    AddProperty( "DataSet_ScalarRange_Min", 0.0, "Min" );
    mPropertyMap["DataSet_ScalarRange_Min"]->SetDisabled();

    AddProperty( "DataSet_ScalarRange_Max", 1.0, "Max" );
    mPropertyMap["DataSet_ScalarRange_Max"]->SetDisabled();

    mPropertyMap["DataSet_ScalarData"]->SignalValueChanged.connect( boost::bind( &VizBasePropertySet::UpdateScalarDataRange, this, _1 ) );
    mPropertyMap["DataSet_ScalarRange_Min"]->SignalRequestValidation.connect( boost::bind( &VizBasePropertySet::ValidateScalarMinMax, this, _1, _2 ) );
    mPropertyMap["DataSet_ScalarRange_Max"]->SignalRequestValidation.connect( boost::bind( &VizBasePropertySet::ValidateScalarMinMax, this, _1, _2 ) );

    AddProperty( "DataSet_VectorData", 0, "Vector Data" );
    enumValues.clear();
    enumValues.push_back( "Select Vector Data" );
    SetPropertyAttribute( "DataSet_VectorData", "enumValues", enumValues );
    mPropertyMap["DataSet"]->SignalValueChanged.connect( boost::bind( &VizBasePropertySet::UpdateVectorDataOptions, this, _1 ) );

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
    PropertyPtr nullPtr;
    UpdateScalarDataOptions( nullPtr );
    UpdateVectorDataOptions( nullPtr );

    AddProperty( "Direction", 0, "Direction" );
    enumValues.clear();
    enumValues.push_back( "x" );
    enumValues.push_back( "y" );
    enumValues.push_back( "z" );
    enumValues.push_back( "By Wand" );
    enumValues.push_back( "All" );
    enumValues.push_back( "By Surface" );
    SetPropertyAttribute( "Direction", "enumValues", enumValues );

    AddProperty( "DataMapping", 0, "Data Mapping");
    enumValues.clear();
    enumValues.push_back( "Map Scalar Data" );
    enumValues.push_back( "Map Volume Flux Data" );
    SetPropertyAttribute( "DataMapping", "enumValues", enumValues );

    AddProperty( "Mode", 0, "Mode" );
    enumValues.clear();
    enumValues.push_back( "Specify a Single Plane" );
    enumValues.push_back( "Use All Precomputed Surfaces" );
    SetPropertyAttribute( "Mode", "enumValues", enumValues );

    AddProperty( "UseGPUTools", false, "Use GPU Tools" );

    // Connect SignalValueChanged of "Mode" to a function that enables and disables
    // its sub-properties as appropriate
    PropertyPtr mode = mPropertyMap["Mode"];
    if( mode )
    {
        mode->SignalValueChanged.connect( boost::bind( &VizBasePropertySet::UpdateModeOptions, this, _1 ) );
    }

    AddProperty( "Mode_UseNearestPrecomputedPlane", false, "Use Nearest Precomputed Plane" );

    AddProperty( "Mode_CyclePrecomputedSurfaces", false, "Cycle Precomputed Surfaces" );
    // We disable this one by default since the selected Mode,
    // "Specify a Single Plane", does not support this option.
    mPropertyMap["Mode_CyclePrecomputedSurfaces"]->SetDisabled();


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
}*/
////////////////////////////////////////////////////////////////////////////////
void VizBasePropertySet::UpdateScalarDataOptions( PropertyPtr property )
{
    boost::ignore_unused_variable_warning( property );
    VES_BEGIN_TRY
    PSVectorOfStrings enumValues;
    const std::string selectedDataset = boost::any_cast< std::string >( GetPropertyAttribute( "DataSet", "enumCurrentString" ) );
    DatasetPropertySet dataset;
    dataset.LoadByKey( "Filename", selectedDataset );
    enumValues = boost::any_cast< std::vector<std::string> >( dataset.GetPropertyValue( "ScalarNames" ) );
    if( enumValues.empty() )
    {
        enumValues.push_back( "No scalars available" );
    }
    SetPropertyAttribute( "DataSet_ScalarData", "enumValues", enumValues );

    if( PropertyExists( "ColorByScalar" ) )
    {
        SetPropertyAttribute( "ColorByScalar", "enumValues", enumValues );
    }

    PropertyPtr nullPtr;
    UpdateScalarDataRange( nullPtr );
    UpdateVectorDataOptions( nullPtr );
    VES_END_TRY( "An error occured with setting the selected scalar" )
}
////////////////////////////////////////////////////////////////////////////////
void VizBasePropertySet::UpdateScalarDataRange( PropertyPtr property )
{
    boost::ignore_unused_variable_warning( property );
    
    mPropertyMap["DataSet_ScalarRange_Min"]->SetEnabled();
    mPropertyMap["DataSet_ScalarRange_Max"]->SetEnabled();

    // Load the current Dataset and get the list of min and max values for its scalars
    std::string selectedDataset = boost::any_cast<std::string > ( GetPropertyAttribute( "DataSet", "enumCurrentString" ) );
    DatasetPropertySet dataset;
    dataset.LoadByKey( "Filename", selectedDataset );
    std::vector<double> mins = boost::any_cast< std::vector<double> >( dataset.GetPropertyValue( "ScalarMins" ) );
    std::vector<double> maxes = boost::any_cast< std::vector<double> >( dataset.GetPropertyValue( "ScalarMaxes" ) );

    // DataSet_ScalarData is an exact copy of the ScalarNames property of the Dataset,
    // so its number in the enum will be the same as the index into the min and max
    // lists
    int index = boost::any_cast<int>( GetPropertyValue( "DataSet_ScalarData" ) );

    if( ( !mins.empty() ) && ( !maxes.empty() ) )
    {
        double min = mins.at( index );
        double max = maxes.at( index );

        // Update the upper and lower bounds of Min and Max first so that
        // boundary values will be allowed!
        SetPropertyAttribute( "DataSet_ScalarRange_Min", "minimumValue", min );
        SetPropertyAttribute( "DataSet_ScalarRange_Min", "maximumValue", max );
        SetPropertyAttribute( "DataSet_ScalarRange_Max", "minimumValue", min );
        SetPropertyAttribute( "DataSet_ScalarRange_Max", "maximumValue", max );

        // Set min and max to the lower and upper boundary values, respectively
        bool success = SetPropertyValue( "DataSet_ScalarRange_Min", min );
        success = SetPropertyValue( "DataSet_ScalarRange_Max", max );
    }
}
////////////////////////////////////////////////////////////////////////////////
void VizBasePropertySet::UpdateVectorDataOptions( PropertyPtr property )
{
    boost::ignore_unused_variable_warning( property );

    PSVectorOfStrings enumValues;
    std::string selectedDataset = boost::any_cast<std::string > ( GetPropertyAttribute( "DataSet", "enumCurrentString" ) );
    DatasetPropertySet dataset;
    dataset.LoadByKey( "Filename", selectedDataset );
    enumValues = boost::any_cast< std::vector<std::string> >( dataset.GetPropertyValue( "VectorNames" ) );
    if( enumValues.empty() )
    {
        enumValues.push_back( "No vectors available" );
    }
    SetPropertyAttribute( "DataSet_VectorData", "enumValues", enumValues );
}
////////////////////////////////////////////////////////////////////////////////
void VizBasePropertySet::UpdateModeOptions( PropertyPtr property )
{
    // Make sure the main value is an int as it should be
    if( property->IsInt() )
    {
        int value = boost::any_cast<int>( property->GetValue() );
        if( value == 0 ) // "Specify a Single Plane"
        {
            mPropertyMap["Mode_UseNearestPrecomputedPlane"]->SetEnabled();
            mPropertyMap["Mode_CyclePrecomputedSurfaces"]->SetDisabled();
        }
        else
        {
            mPropertyMap["Mode_UseNearestPrecomputedPlane"]->SetDisabled();
            mPropertyMap["Mode_CyclePrecomputedSurfaces"]->SetEnabled();
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void VizBasePropertySet::EnableLineWidth( PropertyPtr property )
{
    // Make sure the main value is an int as it should be
    if( property->IsInt() )
    {
        int value = boost::any_cast<int>( property->GetValue() );
        if( value == 2 ) // "lined"
        {
            mPropertyMap["Advanced_LinedContourWidth"]->SetEnabled();
        }
        else
        {
            mPropertyMap["Advanced_LinedContourWidth"]->SetDisabled();
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
bool VizBasePropertySet::ValidateScalarMinMax( PropertyPtr property, boost::any value )
{
    PropertyPtr min = mPropertyMap["DataSet_ScalarRange_Min"];
    PropertyPtr max = mPropertyMap["DataSet_ScalarRange_Max"];

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
void VizBasePropertySet::UpdateDirectionSelection( PropertyPtr property )
{
    boost::ignore_unused_variable_warning( property );

    const std::string value = 
        boost::any_cast<std::string >( GetPropertyAttribute( "Direction", "enumCurrentString" ) );

    if( value == "By Surface" )
    {
        mPropertyMap[ "Direction_Surface" ]->SetEnabled();
    }
    else
    {
        mPropertyMap[ "Direction_Surface" ]->SetDisabled();
    }
}
////////////////////////////////////////////////////////////////////////////////