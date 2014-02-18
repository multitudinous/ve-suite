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
#include <ves/xplorer/data/VizBasePropertySet.h>
#include <ves/xplorer/data/DatasetPropertySet.h>
#include <propertystore/Property.h>

#include <ves/xplorer/Debug.h>
#include <ves/util/Exception.h>

#include <boost/bind.hpp>
#include <boost/concept_check.hpp>
#include <boost/lexical_cast.hpp>

#include <iostream>

#include <ves/xplorer/data/DatabaseManager.h>
#include <ves/xplorer/eventmanager/EventFactory.h>

#include <latticefx/core/DataSet.h>
#include <latticefx/core/Renderer.h>


using namespace ves::xplorer::data;
////////////////////////////////////////////////////////////////////////////////
VizBasePropertySet::VizBasePropertySet()
{
    // Grab pointers to addVizFeature and DeleteVizFeature signals from EventFactory.
    m_addVizSignal =
        reinterpret_cast< ves::util::TwoStringSignal_type* >
        ( xplorer::eventmanager::EventFactory::instance()->GetSignal( "VizBasePropertySet.AddVizFeature" ) );

    m_deleteVizSignal =
        reinterpret_cast< ves::util::StringSignal_type* >
        ( xplorer::eventmanager::EventFactory::instance()->GetSignal( "VizBasePropertySet.DeleteVizFeature" ) );

	///Signal to update an Lfx Vtk scalar range
    {
        std::string name( "VizBasePropertySet" );
        name += boost::lexical_cast<std::string>( this );
        name += ".TBETUpdateLfxVtkScalarRange";
        switchwire::EventManager::instance()->RegisterSignal( ( &m_updateLfxVtkScalarRange ), name, switchwire::EventManager::unspecified_SignalType );
    }
	
	///Signal to update an Lfx Vtk scalar
    {
        std::string name( "VizBasePropertySet" );
        name += boost::lexical_cast<std::string>( this );
        name += ".TBETUpdateLfxVtkScalar";
        switchwire::EventManager::instance()->RegisterSignal( ( &m_updateLfxVtkScalar ), name, switchwire::EventManager::unspecified_SignalType );
    }

	///Signal to update an Lfx Vtk vector
    {
        std::string name( "VizBasePropertySet" );
        name += boost::lexical_cast<std::string>( this );
        name += ".TBETUpdateLfxVtkVector";
        switchwire::EventManager::instance()->RegisterSignal( ( &m_updateLfxVtkVector ), name, switchwire::EventManager::unspecified_SignalType );
    }

	///Signal to update an Lfx Vtk scalar
    {
        std::string name( "VizBasePropertySet" );
        name += boost::lexical_cast<std::string>( this );
        name += ".TBETUpdateLfxVtkColorByScalar";
        switchwire::EventManager::instance()->RegisterSignal( ( &m_updateLfxVtkColorByScalar ), name, switchwire::EventManager::unspecified_SignalType );
    }
	
	///Signal to update an Lfx channel
    {
        std::string name( "VizBasePropertySet" );
        name += boost::lexical_cast<std::string>( this );
        name += ".TBETUpdateLfxChannel";
        switchwire::EventManager::instance()->RegisterSignal( ( &m_updateLfxChan ), name, switchwire::EventManager::unspecified_SignalType );
    }

	///Signal to update an Lfx render property
    {
        std::string name( "VizBasePropertySet" );
        name += boost::lexical_cast<std::string>( this );
        name += ".TBETUpdateLfxRenderProp";
        switchwire::EventManager::instance()->RegisterSignal( ( &m_updateLfxRenderProp ), name, switchwire::EventManager::unspecified_SignalType );
    }
	
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
    prependTag.append( " " );
    std::string tag = boost::any_cast<std::string>( GetPropertyValue( "NameTag" ) );
    SetPropertyValue( "NameTag", tag.insert( 0, prependTag ) );
}
////////////////////////////////////////////////////////////////////////////////
bool VizBasePropertySet::Remove(  )
{
    m_deleteVizSignal->signal( GetUUIDAsString() );
    ///Send the signal to xplorer to tell it to remove the viz feature
    return PropertySet::Remove(  );
}
////////////////////////////////////////////////////////////////////////////////
bool VizBasePropertySet::Save(  )
{
    bool temp = propertystore::PropertySet::Save( );

    ///Send the signal to xplorer to tell it to add the viz feature
    m_addVizSignal->signal( GetUUIDAsString(), GetTypeName() );

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

    AddProperty( "DataSet_VectorData", 0, "Vector Data" );
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
}*/
////////////////////////////////////////////////////////////////////////////////
void VizBasePropertySet::UpdateScalarDataOptions( propertystore::PropertyPtr property )
{
    boost::ignore_unused_variable_warning( property );
    VES_BEGIN_TRY
    PSVectorOfStrings enumValues;
    const std::string selectedDataset = GetDatumValue< std::string >( "DataSet" );
    DatasetPropertySet dataset;
    dataset.LoadByKey( "Filename", selectedDataset );
    enumValues = dataset.GetDatumValue< std::vector< std::string > >( "ScalarNames" );
    if( enumValues.empty() )
    {
        enumValues.push_back( "No scalars available" );
    }
    SetPropertyAttribute( "DataSet_ScalarData", "enumValues", enumValues );

    if( PropertyExists( "ColorByScalar" ) )
    {
        SetPropertyAttribute( "ColorByScalar", "enumValues", enumValues );
    }

    propertystore::PropertyPtr nullPtr;
    UpdateScalarDataRange( nullPtr );
    UpdateVectorDataOptions( nullPtr );
    VES_END_TRY( "An error occured with setting the selected scalar" )
}
////////////////////////////////////////////////////////////////////////////////
void VizBasePropertySet::UpdateScalarDataRange( propertystore::PropertyPtr property )
{
    boost::ignore_unused_variable_warning( property );

    GetProperty( "DataSet_ScalarRange_Min" )->SetEnabled();
    GetProperty( "DataSet_ScalarRange_Max" )->SetEnabled();

    // Load the current Dataset and get the list of min and max values for its scalars
    std::string selectedDataset = GetDatumValue< std::string >( "DataSet" );
    DatasetPropertySet dataset;
    dataset.LoadByKey( "Filename", selectedDataset );
    std::vector<double> mins = dataset.GetDatumValue< std::vector<double> >( "ScalarMins" );
    std::vector<double> maxes = dataset.GetDatumValue< std::vector<double> >( "ScalarMaxes" );

    // DataSet_ScalarData is an exact copy of the ScalarNames property of the Dataset,
    // so its number in the enum will be the same as the index into the min and max
    // lists
    int index = GetPropertyAttributeValue<int>( "DataSet_ScalarData", "enumCurrentIndex" );

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


	UpdateScalarData( property );
}

////////////////////////////////////////////////////////////////////////////////
void VizBasePropertySet::UpdateScalarDataRangeMin( propertystore::PropertyPtr property )
{
    boost::ignore_unused_variable_warning( property );

	double min = GetDatumValue< double >( "DataSet_ScalarRange_Min" );
	double max= GetDatumValue< double >( "DataSet_ScalarRange_Max" );
	m_updateLfxVtkScalarRange.signal( _renderSetType, min, max );
}

////////////////////////////////////////////////////////////////////////////////
void VizBasePropertySet::UpdateScalarDataRangeMax( propertystore::PropertyPtr property )
{
    boost::ignore_unused_variable_warning( property );

	double min = GetDatumValue< double >( "DataSet_ScalarRange_Min" );
	double max= GetDatumValue< double >( "DataSet_ScalarRange_Max" );
	m_updateLfxVtkScalarRange.signal( _renderSetType, min, max );
}

////////////////////////////////////////////////////////////////////////////////
void VizBasePropertySet::UpdateVectorDataOptions( propertystore::PropertyPtr property )
{
    boost::ignore_unused_variable_warning( property );

    PSVectorOfStrings enumValues;
    std::string selectedDataset = boost::any_cast<std::string > ( GetPropertyValue( "DataSet" ) );
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
void VizBasePropertySet::UpdateScalarData( propertystore::PropertyPtr property )
{
    std::string selectedScalar = GetDatumValue< std::string >( "DataSet_ScalarData" );
	m_updateLfxVtkScalar.signal( _renderSetType, selectedScalar );
}
////////////////////////////////////////////////////////////////////////////////
void VizBasePropertySet::UpdateVectorData( propertystore::PropertyPtr property )
{
    std::string selectedVector = GetDatumValue< std::string >( "DataSet_VectorData" );
	m_updateLfxVtkVector.signal( _renderSetType, selectedVector );
}
////////////////////////////////////////////////////////////////////////////////
void VizBasePropertySet::UpdateColorByScalarData( propertystore::PropertyPtr property )
{
    std::string selectedScalar = GetDatumValue< std::string >( "ColorByScalar" );
	m_updateLfxVtkColorByScalar.signal( _renderSetType, selectedScalar );
}
////////////////////////////////////////////////////////////////////////////////
void VizBasePropertySet::UpdateModeOptions( propertystore::PropertyPtr property )
{
    // Make sure the main value is a string as it should be
    if( property->IsString() )
    {
        std::string value = boost::any_cast<std::string>( property->GetValue() );
        if( value == "Specify a Single Plane" )
        {
            GetProperty( "Mode_UseNearestPrecomputedPlane" )->SetEnabled();
            GetProperty( "Mode_CyclePrecomputedSurfaces" )->SetDisabled();
        }
        else
        {
            GetProperty( "Mode_UseNearestPrecomputedPlane" )->SetDisabled();
            GetProperty( "Mode_CyclePrecomputedSurfaces" )->SetEnabled();
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void VizBasePropertySet::EnableLineWidth( propertystore::PropertyPtr property )
{
    // Make sure the main value is a string as it should be
    if( property->IsString() )
    {
        std::string value = boost::any_cast<std::string>( property->GetValue() );
        if( value == "Lined" )
        {
            GetProperty( "Advanced_LinedContourWidth" )->SetEnabled();
        }
        else
        {
            GetProperty( "Advanced_LinedContourWidth" )->SetDisabled();
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
bool VizBasePropertySet::ValidateScalarMinMax( propertystore::PropertyPtr property, boost::any value )
{
    propertystore::PropertyPtr min = GetProperty( "DataSet_ScalarRange_Min" );
    propertystore::PropertyPtr max = GetProperty( "DataSet_ScalarRange_Max" );

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
		m_updateLfxVtkScalarRange.signal( _renderSetType, castMin, castMax );
        return true;
    }
    else
    {
        return false;
    }
}
////////////////////////////////////////////////////////////////////////////////
void VizBasePropertySet::UpdateDirectionSelection( propertystore::PropertyPtr property )
{
    boost::ignore_unused_variable_warning( property );

    const std::string value =
        boost::any_cast<std::string >( GetPropertyValue( "Direction" ) );

    if( value == "By Surface" )
    {
        GetProperty( "Direction_Surface" )->SetEnabled();
    }
    else
    {
        GetProperty( "Direction_Surface" )->SetDisabled();
    }
}
////////////////////////////////////////////////////////////////////////////////
void VizBasePropertySet::EnableLiveProperties( bool live )
{
    boost::ignore_unused_variable_warning( live );
    // Do nothing
    // Doing nothing here prevents live autosave from
    // turning on for all derived classes that do not override this method again
    // and explicitly call PropertySet::EnableLiveProperties.
}
////////////////////////////////////////////////////////////////////////////////
void VizBasePropertySet::AddPropRtpMask()
{
	/*
	AddProperty( "RTP_Point_Mask", 1.0, "Point Mask" );
    SetPropertyAttribute( "RTP_Point_Mask", "minimumValue",   1.0 );
    SetPropertyAttribute( "RTP_Point_Mask", "maximumValue", 300.0 );
	SetPropertyAttribute( "RTP_Point_Mask", "proptype", lfx::core::Renderer::PT_RTP_PTMASK );
	GetProperty( "RTP_Point_Mask" )->SignalValueChanged.connect( boost::bind( &VizBasePropertySet::UpdateVectorData, this, _1 ) );
	*/

	AddPropFloat( "RTP_Point_Mask", "Point Mask", 1.0f, lfx::core::Renderer::PT_RTP_PTMASK );
}

////////////////////////////////////////////////////////////////////////////////
void VizBasePropertySet::AddPropRtpRoi()
{
	/*
	AddProperty( "RTP_Point_Mask", 1.0, "Point Mask" );
    SetPropertyAttribute( "RTP_Point_Mask", "minimumValue",   1.0 );
    SetPropertyAttribute( "RTP_Point_Mask", "maximumValue", 300.0 );
	SetPropertyAttribute( "RTP_Point_Mask", "proptype", lfx::core::Renderer::PT_RTP_PTMASK );
	GetProperty( "RTP_Point_Mask" )->SignalValueChanged.connect( boost::bind( &VizBasePropertySet::UpdateVectorData, this, _1 ) );
	*/

	return;

	float min = boost::numeric::bounds<float>::lowest();
	float max = boost::numeric::bounds<float>::highest();
	AddPropFloat( "RTP_ROI_X_MIN", "ROIBOX X Min", min, lfx::core::Renderer::PT_RTP_ROIBOX_X_MIN );
	AddPropFloat( "RTP_ROI_X_MAX", "ROIBOX X Max", max, lfx::core::Renderer::PT_RTP_ROIBOX_X_MAX );
	AddPropFloat( "RTP_ROI_Y_MIN", "ROIBOX Y Min", min, lfx::core::Renderer::PT_RTP_ROIBOX_Y_MIN );
	AddPropFloat( "RTP_ROI_Y_MAX", "ROIBOX Y Max", max, lfx::core::Renderer::PT_RTP_ROIBOX_Y_MAX );
	AddPropFloat( "RTP_ROI_Z_MIN", "ROIBOX Z Min", min, lfx::core::Renderer::PT_RTP_ROIBOX_Z_MIN );
	AddPropFloat( "RTP_ROI_Z_MAX", "ROIBOX Z Max", max, lfx::core::Renderer::PT_RTP_ROIBOX_Z_MAX );
}

////////////////////////////////////////////////////////////////////////////////
void VizBasePropertySet::CreateSkeletonLfxDsRenderer( const std::string &renderSetType, lfx::core::Renderer *prender )
{
	std::string currentDataSet = "";

	_renderSetType = renderSetType;
	_lfxValueProps.clear();

	if( !prender ) return;

	// use renderer's interface to get and set properties, don't deal with uniforms directly

	std::vector< std::string > enumNames;
	lfx::core::Renderer::PropType propType;
	std::string sval;

	propType = lfx::core::Renderer::PT_TF_INR;
	const osg::Vec2f tfRange = prender->getTransferFunctionInputRange();
	AddPropFloat( "TF_INR_MIN", "TransferFunction Min", tfRange[0], propType );
	AddPropFloat( "TF_INR_MAX", "TransferFunction Max", tfRange[1], propType );

	propType = lfx::core::Renderer::PT_TF_DST;
	enumNames.clear();
	prender->getEnumListTrans( &enumNames );
	sval = prender->getEnumName( prender->getTransferFunctionDestination() );
	AddPropEnum( "TF_DST", "TransferFunction Destination", sval, propType, enumNames );

	if( renderSetType == "vol" )
	{
		propType = lfx::core::Renderer::PT_HM_SRC;
		enumNames.clear();
		prender->getEnumListMaskInput( &enumNames );
		sval = prender->getEnumName( prender->getHardwareMaskInputSource() );
		AddPropEnum( "HM_SRC", "HardwareMask Input Source",  sval, propType, enumNames );

		propType = lfx::core::Renderer::PT_HM_REF;
		AddPropFloat( "HM_REF", "HardwareMask Reference", prender->getHardwareMaskReference(), propType );

		propType = lfx::core::Renderer::PT_HM_EPS;
		AddPropFloat( "HM_EPS", "HardwareMask Epsilon", prender->getHardwareMaskEpsilon(), propType );
	
		propType = lfx::core::Renderer::PT_HM_OPE;
		enumNames.clear();
		prender->getEnumListHardwareMaskOperator( &enumNames );
		sval = prender->getEnumName( (lfx::core::Renderer::HardwareMaskOperator)prender->getHardwareMaskOperator() );
		AddPropEnum( "HM_OPE", "HardwareMask Operator",  sval, propType, enumNames );
	}
	else
	{
		AddPropRtpMask();
		AddPropRtpRoi();
	}

}
////////////////////////////////////////////////////////////////////////////////
void VizBasePropertySet::CreateSkeletonLfxDsVolume( lfx::core::Renderer *prender )
{
	std::string currentDataSet = "";

    AddProperty( "Hide", false, "Toggle Viz Off" );

	// add dataset property
	AddProperty( "DataSet", std::string(""), "Data Set" );
    PSVectorOfStrings enumValues;

	enumValues = ves::xplorer::data::DatabaseManager::instance()->GetStringVector( "Dataset", "Filename" );
    if( enumValues.empty() )
    {
        enumValues.push_back( "No datasets loaded" );
	}
	else
	{
		currentDataSet = enumValues[0];
	}
	SetPropertyAttribute( "DataSet", "enumValues", enumValues );
	GetProperty( "DataSet" )->SignalValueChanged.connect( boost::bind( &VizBasePropertySet::UpdateLfxDataSet, this, _1 ) );


	// add a channel property
	AddProperty( "Channel", std::string(""), "Channel" );
	InitLfxChannelOptions();
	GetProperty( "Channel" )->SignalValueChanged.connect( boost::bind( &VizBasePropertySet::UpdateLfxChannel, this, _1 ) );

	CreateSkeletonLfxDsRenderer( "vol", prender );

	/*
	// use renderer's interface to get and set properties, don't deal with uniforms directly

	std::vector< std::string > enumNames;
	lfx::core::Renderer::PropType propType;
	std::string sval;

	propType = lfx::core::Renderer::PT_TF_INR;
	const osg::Vec2f tfRange = prender->getTransferFunctionInputRange();
	AddPropFloat( "TF_INR_MIN", "TransferFunction Min", tfRange[0], propType );
	AddPropFloat( "TF_INR_MAX", "TransferFunction Max", tfRange[1], propType );

	propType = lfx::core::Renderer::PT_TF_DST;
	enumNames.clear();
	prender->getEnumListTrans( &enumNames );
	sval = prender->getEnumName( prender->getTransferFunctionDestination() );
	AddPropEnum( "TF_DST", "TransferFunction Destination", sval, propType, enumNames );

	propType = lfx::core::Renderer::PT_HM_SRC;
	enumNames.clear();
	prender->getEnumListMaskInput( &enumNames );
	sval = prender->getEnumName( prender->getHardwareMaskInputSource() );
	AddPropEnum( "HM_SRC", "HardwareMask Input Source",  sval, propType, enumNames );

	propType = lfx::core::Renderer::PT_HM_REF;
	AddPropFloat( "HM_REF", "HardwareMask Reference", prender->getHardwareMaskReference(), propType );

	propType = lfx::core::Renderer::PT_HM_EPS;
	AddPropFloat( "HM_EPS", "HardwareMask Epsilon", prender->getHardwareMaskEpsilon(), propType );
	
	propType = lfx::core::Renderer::PT_HM_OPE;
	enumNames.clear();
	prender->getEnumListHardwareMaskOperator( &enumNames );
	sval = prender->getEnumName( (lfx::core::Renderer::HardwareMaskOperator)prender->getHardwareMaskOperator() );
	AddPropEnum( "HM_OPE", "HardwareMask Operator",  sval, propType, enumNames );
	*/

	/*
	///How to control vis animations????
    AddProperty( "AnimationControls", boost::any(), "Animation Controls" );
    SetPropertyAttribute( "AnimationControls", "isUIGroupOnly", true );
    SetPropertyAttribute( "AnimationControls", "setExpanded", true );
    AddProperty( "AnimationControls_Controls", std::string(""), "Controls" );
    enumValues.clear();
    enumValues.push_back( "Play" );
    enumValues.push_back( "Stop" );
    enumValues.push_back( "Step Forward" );
    enumValues.push_back( "Step Back" );
    SetPropertyAttribute( "AnimationControls_Controls", "enumValues", enumValues );

    AddProperty( "AnimationControls_Duration", 0.0, "Duration" );
    SetPropertyAttribute( "AnimationControls_Duration", "minimumValue", 0.0 );
    SetPropertyAttribute( "AnimationControls_Duration", "maximumValue", 100.0 );
	*/
}
////////////////////////////////////////////////////////////////////////////////
void VizBasePropertySet::AddPropEnum( const std::string &propName, const std::string &propDisp, const std::string &defValue, int propType, const std::vector<std::string> &enumValues )
{
	AddProperty( propName, defValue, propDisp );
	SetPropertyAttribute( propName, "enumValues", enumValues );
	GetProperty( propName )->SetAttribute( "proptype", propType );
	GetProperty( propName )->SignalValueChanged.connect( boost::bind( &VizBasePropertySet::UpdateLfxValue, this, _1 ) );

	_lfxValueProps.push_back( propName );
}
////////////////////////////////////////////////////////////////////////////////
void VizBasePropertySet::AddPropFloat( const std::string &propName, const std::string &propDisp, float value, int propType )
{
	AddProperty( propName, (double)value, propDisp );
	GetProperty( propName )->SetAttribute( "proptype", propType );
	GetProperty( propName )->SignalValueChanged.connect( boost::bind( &VizBasePropertySet::UpdateLfxValue, this, _1 ) );

	_lfxValueProps.push_back( propName );
}
////////////////////////////////////////////////////////////////////////////////
void VizBasePropertySet::InitLfxChannelOptions()
{
    try
	{
		PSVectorOfStrings enumValues;
		const std::string selectedDataset = GetDatumValue< std::string >( "DataSet" );
		DatasetPropertySet dataset;
		dataset.LoadByKey( "Filename", selectedDataset );
		enumValues = dataset.GetDatumValue< std::vector< std::string > >( "Channels" );
		if( enumValues.empty() )
		{
			enumValues.push_back( "No channels available" );
		}
		SetPropertyAttribute( "Channel", "enumValues", enumValues );
	}
	catch( ... )
	{
		PSVectorOfStrings enumValues;
		enumValues.push_back( "No channels available" );
		SetPropertyAttribute( "Channel", "enumValues", enumValues );
	}
    //VES_END_TRY( "An error occured with setting the selected scalar" )
}
////////////////////////////////////////////////////////////////////////////////
void VizBasePropertySet::UpdateLfxDataSet( propertystore::PropertyPtr property )
{
}

////////////////////////////////////////////////////////////////////////////////
void VizBasePropertySet::UpdateLfxChannel( propertystore::PropertyPtr property )
{
	propertystore::PropertyPtr propds = GetProperty( "DataSet" );
	if( !propds.get() )
	{
		vprDEBUG( vesDBG, 0 ) << "|\tUpdateLfxChannel - no DataSet property was found!" << std::endl << vprDEBUG_FLUSH;
		return;
	}

	std::string dataSetName = boost::any_cast<std::string>( propds->GetValue() );
	std::string channel = boost::any_cast<std::string>( property->GetValue() );
	m_updateLfxChan.signal( dataSetName, channel );
}

////////////////////////////////////////////////////////////////////////////////
void VizBasePropertySet::UpdateLfxValue( propertystore::PropertyPtr prop )
{
	UpdateLfxValue( prop, false );
}

void VizBasePropertySet::UpdateLfxValue( propertystore::PropertyPtr prop, bool init )
{
	propertystore::PropertyPtr propds = GetProperty( "DataSet" );
	if( !propds.get() )
	{
		vprDEBUG( vesDBG, 0 ) << "|\tUpdateLfxValue- no DataSet property was found!" << std::endl << vprDEBUG_FLUSH;
		return;
	}

	std::string dataSetName = boost::any_cast<std::string>( propds->GetValue() );

	crunchstore::DatumPtr proptype = prop->GetAttribute( "proptype" );
	if( !proptype.get() )
	{
		vprDEBUG( vesDBG, 0 ) << "|\tUpdateLfxValue - propname not found. unknown property!" << std::endl << vprDEBUG_FLUSH;
		return;
	}

	int propType = boost::any_cast<int>( proptype->GetValue() );
	boost::any v1, v2;

	if( propType == lfx::core::Renderer::PT_TF_INR )
	{
		v1 = GetProperty( "TF_INR_MIN" )->GetValue();
		v2 = GetProperty( "TF_INR_MAX" )->GetValue();
	}
	else
	{
		v1 = prop->GetValue();
	}

	
	m_updateLfxRenderProp.signal( _renderSetType, dataSetName, propType, v1, v2 );
}

////////////////////////////////////////////////////////////////////////////////
void VizBasePropertySet::UpdateRendererLfxValues()
{
	for( size_t i=0; i<_lfxValueProps.size(); i++ )
	{
		propertystore::PropertyPtr prop = GetProperty( _lfxValueProps[i] );
		if( !prop.get() )
		{
			vprDEBUG( vesDBG, 0 ) << "|\tUpdateRendereLfxValues- failed to find the property: " << _lfxValueProps[i] << std::endl << vprDEBUG_FLUSH;
			continue;
		}

		UpdateLfxValue( prop, true );
	}
}

////////////////////////////////////////////////////////////////////////////////
void VizBasePropertySet::GetRendererLfxValues( std::vector<SLfxPropValues> *v )
{
	for( size_t i=0; i<_lfxValueProps.size(); i++ )
	{
		propertystore::PropertyPtr prop = GetProperty( _lfxValueProps[i] );
		if( !prop.get() )
		{
			vprDEBUG( vesDBG, 0 ) << "|\tUpdateRendereLfxValues- failed to find the property: " << _lfxValueProps[i] << std::endl << vprDEBUG_FLUSH;
			continue;
		}

		SLfxPropValues pv;
		if( GetLfxValues( prop, &pv ) )
		{
			v->push_back( pv );
		}
	}
}

////////////////////////////////////////////////////////////////////////////////
bool VizBasePropertySet::GetLfxValues( propertystore::PropertyPtr prop, SLfxPropValues *pv )
{
	propertystore::PropertyPtr propds = GetProperty( "DataSet" );
	if( !propds.get() )
	{
		vprDEBUG( vesDBG, 0 ) << "|\tUpdateLfxValue- no DataSet property was found!" << std::endl << vprDEBUG_FLUSH;
		return false;
	}

	pv->dataSetName = boost::any_cast<std::string>( propds->GetValue() );

	crunchstore::DatumPtr proptype = prop->GetAttribute( "proptype" );
	if( !proptype.get() )
	{
		vprDEBUG( vesDBG, 0 ) << "|\tUpdateLfxValue - propname not found. unknown property!" << std::endl << vprDEBUG_FLUSH;
		return false;
	}

	pv->propType = boost::any_cast<int>( proptype->GetValue() );
	boost::any v1, v2;

	if( pv->propType == lfx::core::Renderer::PT_TF_INR )
	{
		pv->v1 = GetProperty( "TF_INR_MIN" )->GetValue();
		pv->v2 = GetProperty( "TF_INR_MAX" )->GetValue();
	}
	else
	{
		pv->v1 = prop->GetValue();
	}

	return true;
}

////////////////////////////////////////////////////////////////////////////////