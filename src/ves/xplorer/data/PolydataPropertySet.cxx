/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2010 by Iowa State University
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
#include <ves/xplorer/data/PolydataPropertySet.h>
#include <ves/xplorer/data/DatasetPropertySet.h>
#include <ves/xplorer/data/Property.h>

#include <boost/bind.hpp>
#include <boost/concept_check.hpp>

#include <iostream>
#include <ves/xplorer/data/DatabaseManager.h>

using namespace ves::xplorer::data;
////////////////////////////////////////////////////////////////////////////////
PolydataPropertySet::PolydataPropertySet()
{
    mTableName = "Polydata";
    CreateSkeleton();
}
////////////////////////////////////////////////////////////////////////////////
PolydataPropertySet::PolydataPropertySet( const PolydataPropertySet& orig )
    :
    PropertySet( orig )
{
}
////////////////////////////////////////////////////////////////////////////////
PolydataPropertySet::~PolydataPropertySet()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void PolydataPropertySet::CreateSkeleton()
{
    AddProperty( "DataSet", 0, "Data Set" );
    PSVectorOfStrings enumValues;

    AddProperty( "DataSet_ScalarData", 0, "Scalar Data" );
    // Dummy value to ensure this gets set up as an enum
    enumValues.push_back( "Select Scalar Data" );
    SetPropertyAttribute( "DataSet_ScalarData", "enumValues", enumValues );
    mPropertyMap["DataSet"]->SignalValueChanged.connect( boost::bind( &PolydataPropertySet::UpdateScalarDataOptions, this, _1 ) );

    AddProperty( "DataSet_ScalarRange", boost::any(), "Scalar Range" );
    SetPropertyAttribute( "DataSet_ScalarRange", "isUIGroupOnly", true );
    SetPropertyAttribute( "DataSet_ScalarRange", "setExpanded", true );

    AddProperty( "DataSet_ScalarRange_Min", 0.0, "Min" );
    mPropertyMap["DataSet_ScalarRange_Min"]->SetDisabled();

    AddProperty( "DataSet_ScalarRange_Max", 1.0, "Max" );
    mPropertyMap["DataSet_ScalarRange_Max"]->SetDisabled();

    mPropertyMap["DataSet_ScalarData"]->SignalValueChanged.connect( boost::bind( &PolydataPropertySet::UpdateScalarDataRange, this, _1 ) );
    mPropertyMap["DataSet_ScalarRange_Min"]->SignalRequestValidation.connect( boost::bind( &PolydataPropertySet::ValidateScalarMinMax, this, _1, _2 ) );
    mPropertyMap["DataSet_ScalarRange_Max"]->SignalRequestValidation.connect( boost::bind( &PolydataPropertySet::ValidateScalarMinMax, this, _1, _2 ) );

    AddProperty( "DataSet_VectorData", 0, "Vector Data" );
    enumValues.clear();
    enumValues.push_back( "Select Vector Data" );
    SetPropertyAttribute( "DataSet_VectorData", "enumValues", enumValues );
    mPropertyMap["DataSet"]->SignalValueChanged.connect( boost::bind( &PolydataPropertySet::UpdateVectorDataOptions, this, _1 ) );
    
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
    //UpdateVectorDataOptions( nullPtr );

    AddProperty( "ColorByScalar", 0, "Color By Scalar" );
    enumValues.clear();
    enumValues.push_back( "Select Scalar Data" );
    SetPropertyAttribute( "ColorByScalar", "enumValues", enumValues );

    AddProperty( "ColorByScalar_ScalarRange", boost::any(), "Scalar Range" );
    SetPropertyAttribute( "ColorByScalar_ScalarRange", "isUIGroupOnly", true );
    SetPropertyAttribute( "ColorByScalar_ScalarRange", "setExpanded", true );
    
    AddProperty( "ColorByScalar_ScalarRange_Min", 0.0, "Min" );
    mPropertyMap["ColorByScalar_ScalarRange_Min"]->SetDisabled();
    
    AddProperty( "ColorByScalar_ScalarRange_Max", 1.0, "Max" );
    mPropertyMap["ColorByScalar_ScalarRange_Max"]->SetDisabled();
    
    mPropertyMap["ColorByScalar"]->SignalValueChanged.connect( boost::bind( &PolydataPropertySet::UpdateColorByScalarDataRange, this, _1 ) );
    mPropertyMap["ColorByScalar_ScalarRange_Min"]->SignalRequestValidation.connect( boost::bind( &PolydataPropertySet::ValidateColorByScalarMinMax, this, _1, _2 ) );
    mPropertyMap["ColorByScalar_ScalarRange_Max"]->SignalRequestValidation.connect( boost::bind( &PolydataPropertySet::ValidateColorByScalarMinMax, this, _1, _2 ) );

    AddProperty( "WarpedScaleFactor", 1.0, "Warped Scale Factor");
    SetPropertyAttribute( "WarpedScaleFactor", "minimumValue",  0.01 );
    SetPropertyAttribute( "WarpedScaleFactor", "maximumValue", 100.0 );

    AddProperty( "Opacity", 0.0, "Opacity");
    SetPropertyAttribute( "Opacity", "minimumValue",   0.0 );
    SetPropertyAttribute( "Opacity", "maximumValue", 100.0 );
    
    AddProperty( "UseGPUTools", false, "Use GPU Tools" );
    AddProperty( "UseWarpedSurface", false, "Use Warped Surface" );
    AddProperty( "ParticleData", false, "Particles" );
    AddProperty( "TwoSidedLighting", false, "Two Sided Lighting" );
}
////////////////////////////////////////////////////////////////////////////////
void PolydataPropertySet::UpdateScalarDataOptions( PropertyPtr property )
{
    boost::ignore_unused_variable_warning( property );

    PSVectorOfStrings enumValues;
    std::string selectedDataset = boost::any_cast<std::string > ( GetPropertyAttribute( "DataSet", "enumCurrentString" ) );
    std::cout << " UpdateScalarDataOptions " << selectedDataset << std::endl;
    DatasetPropertySet dataset;
    dataset.LoadByKey( "Filename", selectedDataset );
    enumValues = boost::any_cast< std::vector<std::string> >( dataset.GetPropertyValue( "ScalarNames" ) );
    if( enumValues.empty() )
    {
        enumValues.push_back( "No scalars loaded" );
    }
    std::cout << enumValues.size() << std::endl;
    SetPropertyAttribute( "DataSet_ScalarData", "enumValues", enumValues );
    
    SetPropertyAttribute( "ColorByScalar", "enumValues", enumValues );

    PropertyPtr nullPtr;
    UpdateScalarDataRange( nullPtr );
    UpdateVectorDataOptions( nullPtr );
}
////////////////////////////////////////////////////////////////////////////////
void PolydataPropertySet::UpdateScalarDataRange( PropertyPtr property )
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
        SetPropertyValue( "DataSet_ScalarRange_Min", min );
        SetPropertyValue( "DataSet_ScalarRange_Max", max );
    }
}
////////////////////////////////////////////////////////////////////////////////
void PolydataPropertySet::UpdateColorByScalarDataRange( PropertyPtr property )
{
    boost::ignore_unused_variable_warning( property );
    
    mPropertyMap["ColorByScalar_ScalarRange_Min"]->SetEnabled();
    mPropertyMap["ColorByScalar_ScalarRange_Max"]->SetEnabled();
    
    // Load the current Dataset and get the list of min and max values for its scalars
    std::string selectedDataset = boost::any_cast<std::string > ( GetPropertyAttribute( "DataSet", "enumCurrentString" ) );
    DatasetPropertySet dataset;
    dataset.LoadByKey( "Filename", selectedDataset );
    std::vector<double> mins = boost::any_cast< std::vector<double> >( dataset.GetPropertyValue( "ScalarMins" ) );
    std::vector<double> maxes = boost::any_cast< std::vector<double> >( dataset.GetPropertyValue( "ScalarMaxes" ) );
    
    // DataSet_ScalarData is an exact copy of the ScalarNames property of the Dataset,
    // so its number in the enum will be the same as the index into the min and max
    // lists
    int index = boost::any_cast<int>( GetPropertyValue( "ColorByScalar" ) );
    
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
void PolydataPropertySet::UpdateVectorDataOptions( PropertyPtr property )
{
    boost::ignore_unused_variable_warning( property );

    PSVectorOfStrings enumValues;
    std::string selectedDataset = boost::any_cast<std::string > ( GetPropertyAttribute( "DataSet", "enumCurrentString" ) );
    std::cout << " UpdateVectorDataOptions " << selectedDataset << std::endl;
    DatasetPropertySet dataset;
    dataset.LoadByKey( "Filename", selectedDataset );
    enumValues = boost::any_cast< std::vector<std::string> >( dataset.GetPropertyValue( "VectorNames" ) );
    if( enumValues.empty() )
    {
        enumValues.push_back( "No vectors loaded" );
    }
    std::cout << enumValues.size() << std::endl;
    SetPropertyAttribute( "DataSet_VectorData", "enumValues", enumValues );
}
////////////////////////////////////////////////////////////////////////////////
void PolydataPropertySet::UpdateModeOptions( PropertyPtr property )
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
bool PolydataPropertySet::ValidateScalarMinMax( PropertyPtr property, boost::any value )
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
bool PolydataPropertySet::ValidateColorByScalarMinMax( PropertyPtr property, boost::any value )
{
    PropertyPtr min = mPropertyMap["ColorByScalar_ScalarRange_Min"];
    PropertyPtr max = mPropertyMap["ColorByScalar_ScalarRange_Max"];
    
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
