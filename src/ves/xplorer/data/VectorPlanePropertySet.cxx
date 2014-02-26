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

#include <latticefx/core/vtk/VTKVectorRenderer.h>

using namespace ves::xplorer::data;
////////////////////////////////////////////////////////////////////////////////
VectorPlanePropertySet::VectorPlanePropertySet()
    :
    VizBasePropertySet()
{
    SetDataManager( DatabaseManager::instance()->GetDataManager() );
    SetTypeName( "VectorPlane" );
    RegisterPropertySet( GetTypeName() );

	///Signal to update an Lfx Vtk VectorData
    {
        std::string name( "VectorPlanePropertySet" );
        name += boost::lexical_cast<std::string>( this );
        name += ".TBETUpdateLfxVtkVectorData";
		switchwire::EventManager::instance()->RegisterSignal( ( &m_updateLfxVtkVectorData ), name, switchwire::EventManager::unspecified_SignalType );
    }

    CreateSkeletonLfxDs();
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
void VectorPlanePropertySet::CreateSkeletonLfxDs()
{
	CreateSkeleton();

	// TODO: set any other defaults for the renderer here
	lfx::core::vtk::VTKVectorRendererPtr renderOp( new lfx::core::vtk::VTKVectorRenderer() );
	renderOp->setTransferFunctionDestination( lfx::core::Renderer::TF_RGBA );
	VizBasePropertySet::CreateSkeletonLfxDsRenderer( "vec", renderOp.get() );
}
////////////////////////////////////////////////////////////////////////////////
void VectorPlanePropertySet::CreateSkeleton()
{
    AddProperty( "Hide", false, "Toggle Viz Off" );

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
	GetProperty( "DataSet_VectorData" )->SignalValueChanged.connect( boost::bind( &VizBasePropertySet::UpdateVectorData, this, _1 ) );

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
    // enumValues.push_back( "By Wand" ); // TODO: not sure what By Wand is
    enumValues.push_back( "All" );
    enumValues.push_back( "By Surface" ); // TODO: need to support vectors by Surface with LFX, currently not implemented.
    SetPropertyAttribute( "Direction", "enumValues", enumValues );
	GetProperty( "Direction" )->SignalValueChanged.connect( boost::bind( &VectorPlanePropertySet::UpdateDirection, this, _1 ) );

	// TODO: This is for an ARBSurface
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


    // AddProperty( "UseGPUTools", false, "Use GPU Tools" );

    // Connect SignalValueChanged of "Mode" to a function that enables and disables
    // its sub-properties as appropriate
    propertystore::PropertyPtr mode = GetProperty( "Mode" );
    if( mode )
    {
        mode->SignalValueChanged.connect( boost::bind( &VizBasePropertySet::UpdateModeOptions, this, _1 ) );
    }

	// TODO: this sets a flag in cfdObjects.. usePreCalcData to true, but not finding where that is used anywhere
    AddProperty( "Mode_UseNearestPrecomputedPlane", false, "Use Nearest Precomputed Plane" );
    AddProperty( "Mode_CyclePrecomputedSurfaces", false, "Cycle Precomputed Surfaces" );
    // We disable this one by default since the selected Mode,
    // "Specify a Single Plane", does not support this option.
    GetProperty( "Mode_CyclePrecomputedSurfaces" )->SetDisabled();


    AddProperty( "PlaneLocation", 50.00, "Plane Location" );
    SetPropertyAttribute( "PlaneLocation", "minimumValue", 0.00 );
    SetPropertyAttribute( "PlaneLocation", "maximumValue", 100.00 );
	GetProperty( "PlaneLocation" )->SignalValueChanged.connect( boost::bind( &VectorPlanePropertySet::UpdatePlaneLocation, this, _1 ) );

    AddProperty( "Advanced", boost::any(), "Advanced" );
    SetPropertyAttribute( "Advanced", "isUIGroupOnly", true );

    AddProperty( "Advanced_VectorThreshold", boost::any(), "Vector Threshold" );
    SetPropertyAttribute( "Advanced_VectorThreshold", "isUIGroupOnly", true );
    SetPropertyAttribute( "Advanced_VectorThreshold", "setExpanded", true );
    AddProperty( "Advanced_VectorThreshold_Min",   1.0, "Vector Threshold Min" );
    AddProperty( "Advanced_VectorThreshold_Max", 100.0, "Vector Threshold Max" );
	GetProperty( "Advanced_VectorThreshold_Min" )->SignalValueChanged.connect( boost::bind( &VectorPlanePropertySet::UpdateThreshHold, this, _1 ) );
	GetProperty( "Advanced_VectorThreshold_Max" )->SignalValueChanged.connect( boost::bind( &VectorPlanePropertySet::UpdateThreshHold, this, _1 ) );
    
	// TODO: This is currently not supported in LFX
    AddProperty( "Advanced_VectorScale", 200.0, "Vector Scale" );
    SetPropertyAttribute( "Advanced_VectorScale", "minimumValue",   1.0 );
    SetPropertyAttribute( "Advanced_VectorScale", "maximumValue", 400.0 );

    AddProperty( "Advanced_VectorRatio", 1.0, "Vector Ratio" );
    SetPropertyAttribute( "Advanced_VectorRatio", "minimumValue",   1.0 );
    SetPropertyAttribute( "Advanced_VectorRatio", "maximumValue", 200.0 );
	GetProperty( "Advanced_VectorRatio" )->SignalValueChanged.connect( boost::bind( &VectorPlanePropertySet::UpdateVectorRatio, this, _1 ) );

	// TODO: this is a vtkGlpyh setting and currently not supported in LFX
    AddProperty( "Advanced_ScaleByVectorMagnitude", false, "Scale By Vector Magnitude" );

	// TODO: this gets set on the dataset so it should keep doing whatever it was doing before, not sure what happens if its changed on actively rendering vectors though.
    AddProperty( "Advanced_Greyscale", false, "Greyscale" );
}
////////////////////////////////////////////////////////////////////////////////
void VectorPlanePropertySet::EnableLiveProperties( bool live )
{
    if( !live )
    {
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
        propertystore::MakeLiveBasePtr p(
                    new propertystore::MakeLive< bool const& >( m_UUIDString,
                                                         GetProperty("Hide"),
                                                         "HideVizFeature", true ) );
        m_liveObjects.push_back( p );
    }
}
////////////////////////////////////////////////////////////////////////////////
void VectorPlanePropertySet::UpdateThreshHold( propertystore::PropertyPtr property )
{
    boost::ignore_unused_variable_warning( property );
	UpdateVector();
}
////////////////////////////////////////////////////////////////////////////////
void VectorPlanePropertySet::UpdateDirection( propertystore::PropertyPtr property )
{
    boost::ignore_unused_variable_warning( property );
	UpdateVector();
}
////////////////////////////////////////////////////////////////////////////////
void VectorPlanePropertySet::UpdatePlaneLocation( propertystore::PropertyPtr property )
{
	boost::ignore_unused_variable_warning( property );
	UpdateVector();
}
////////////////////////////////////////////////////////////////////////////////
void VectorPlanePropertySet::UpdateVectorRatio( propertystore::PropertyPtr property )
{
	boost::ignore_unused_variable_warning( property );
	UpdateVector();
}
////////////////////////////////////////////////////////////////////////////////
void VectorPlanePropertySet::UpdateVector()
{
	std::string strdir = GetDatumValue< std::string >( "Direction" );

	double threshHoldMin = GetDatumValue< double >( "Advanced_VectorThreshold_Min" );
	double threshHoldMax = GetDatumValue< double >( "Advanced_VectorThreshold_Max" );
	double requestedValue = GetDatumValue< double >( "PlaneLocation" );
	double vectorRatio = GetDatumValue< double >( "Advanced_VectorRatio" );

	int planeDir = 0;
	if( !strdir.compare( "x" ) )
	{
		planeDir = 0;
	}
	else if( !strdir.compare( "y" ) )
	{
		planeDir = 1;
	}
	else if( !strdir.compare( "z" ) )
	{
		planeDir = 2;
	}
	else if( !strdir.compare( "All" ) )
	{
		planeDir = 3;
	}
	else if( !strdir.compare( "By Surface" ) )
	{
		planeDir = 4;
	}

	m_updateLfxVtkVectorData.signal( threshHoldMin, threshHoldMax, requestedValue, vectorRatio, planeDir);
}
////////////////////////////////////////////////////////////////////////////////