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
#include <ves/xplorer/data/PolydataPropertySet.h>
#include <ves/xplorer/data/DatasetPropertySet.h>
#include <propertystore/Property.h>
#include <propertystore/MakeLive.h>

#include <boost/bind.hpp>
#include <boost/concept_check.hpp>

#include <iostream>
#include <ves/xplorer/data/DatabaseManager.h>

#include <latticefx/core/vtk/VTKVectorRenderer.h>

using namespace ves::xplorer::data;
////////////////////////////////////////////////////////////////////////////////
PolydataPropertySet::PolydataPropertySet()
{
    SetDataManager( DatabaseManager::instance()->GetDataManager() );
    SetTypeName( "Polydata" );

    RegisterPropertySet( GetTypeName() );

	///Signal to update an Lfx Vtk VectorData
    {
        std::string name( "PolydataPropertySet" );
        name += boost::lexical_cast<std::string>( this );
        name += ".TBETUpdateLfxVtkPolyData";
		switchwire::EventManager::instance()->RegisterSignal( ( &m_updateLfxVtkPolyData ), name, switchwire::EventManager::unspecified_SignalType );
    }

	

    CreateSkeletonLfxDs();
}
////////////////////////////////////////////////////////////////////////////////
PolydataPropertySet::PolydataPropertySet( const PolydataPropertySet& orig )
    :
    VizBasePropertySet( orig )
{
}
////////////////////////////////////////////////////////////////////////////////
PolydataPropertySet::~PolydataPropertySet()
{  
    ;
}
////////////////////////////////////////////////////////////////////////////////
propertystore::PropertySetPtr PolydataPropertySet::CreateNew()
{
    return propertystore::PropertySetPtr( new PolydataPropertySet );
}
////////////////////////////////////////////////////////////////////////////////
void PolydataPropertySet::CreateSkeletonLfxDs()
{
	CreateSkeleton();

	// TODO: see cfdPolyData::getPolyDataType()
	// get the type for the dataset then either init with a vector or surface renderer
	//
	lfx::core::vtk::VTKVectorRendererPtr renderOp( new lfx::core::vtk::VTKVectorRenderer() );
	renderOp->setTransferFunctionDestination( lfx::core::Renderer::TF_RGBA );
	VizBasePropertySet::CreateSkeletonLfxDsRenderer( "pol", renderOp.get() );
}
////////////////////////////////////////////////////////////////////////////////
void PolydataPropertySet::CreateSkeleton()
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

    GetProperty( "ColorByScalar" )->SignalValueChanged.connect( boost::bind( &PolydataPropertySet::UpdateColorByScalarDataRange, this, _1 ) );
    GetProperty( "ColorByScalar_ScalarRange_Min" )->SignalRequestValidation.connect( boost::bind( &PolydataPropertySet::ValidateColorByScalarMinMax, this, _1, _2 ) );
    GetProperty( "ColorByScalar_ScalarRange_Max" )->SignalRequestValidation.connect( boost::bind( &PolydataPropertySet::ValidateColorByScalarMinMax, this, _1, _2 ) );

	// Now that DataSet has choices loaded and ColorByScalar has been added, force an update on the available
    // scalar and vector data
    propertystore::PropertyPtr nullPtr;
    UpdateScalarDataOptions( nullPtr );
    //UpdateVectorDataOptions( nullPtr );

	// only applicable for surface polydata
	AddProperty( "UseWarpedSurface", false, "Use Warped Surface" );
	GetProperty( "UseWarpedSurface" )->SignalValueChanged.connect( boost::bind( &PolydataPropertySet::UpdateWarping, this, _1 ) );

    AddProperty( "WarpedScaleFactor", 1.0, "Warped Scale Factor" );
    SetPropertyAttribute( "WarpedScaleFactor", "minimumValue",  0.01 );
    SetPropertyAttribute( "WarpedScaleFactor", "maximumValue", 100.0 );
	GetProperty( "WarpedScaleFactor" )->SignalValueChanged.connect( boost::bind( &PolydataPropertySet::UpdateWarping, this, _1 ) );

	//
	// TODO: Particle Data really needs it own PropertySet and space in the GUI so lets not worry about updating it for now.
	//       - When checked this currently uses ParticleAnimation.cxx to create the rendering pipeline,
	//		 - After getting its own place in the gui, this can then be restore to set the particle flag, so cfdPolyData can decide to render points or spheres on
	//		    on vtk POLYDATA vertex data.
	//

	AddProperty( "Particles", std::string(""), "Particle Animation" );
	AddProperty( "Particles_ParticlesOn", false, "Particles" );
	GetProperty( "Particles_ParticlesOn" )->SignalValueChanged.connect( boost::bind( &PolydataPropertySet::UpdateParticleData, this, _1 ) ); 
	
	AddProperty( "Particles_DiameterData", std::string(""), "Diameter Data" );
    enumValues.clear();
    enumValues.push_back( "Select Diameter Data" );
    SetPropertyAttribute( "Particles_DiameterData", "enumValues", enumValues );

	AddProperty( "Particles_VmagData", std::string(""), "Vmag Data" );
    enumValues.clear();
    enumValues.push_back( "Select Vmag Data" );
    SetPropertyAttribute( "Particles_VmagData", "enumValues", enumValues );

	AddProperty( "Particles_ConversionFactor", 0.00328084, "Conversion Factor" );
	//SetPropertyAttribute( "Particles_ConversionFactor", "minimumValue",  0.000001 );
    //SetPropertyAttribute( "Particles_ConversionFactor", "maximumValue", 100.0 );
	SetPropertyAttribute( "Particles_ConversionFactor", "DisplayPrecision", 8 );
	
	GetProperty( "DataSet" )->SignalValueChanged.connect( boost::bind( &PolydataPropertySet::UpdateTransientDataOptions, this, _1 ) );

	// update to disable or enable diam and vmag
	UpdateParticleData( propertystore::PropertyPtr() );
	UpdateTransientDataOptions( propertystore::PropertyPtr() );
	/*
    AddProperty( "Opacity", 0.0, "Opacity" );
    SetPropertyAttribute( "Opacity", "minimumValue",   0.0 );
    SetPropertyAttribute( "Opacity", "maximumValue", 100.0 );
	*/

    //AddProperty( "UseGPUTools", false, "Use GPU Tools" );
    
 
    //AddProperty( "TwoSidedLighting", false, "Two Sided Lighting" );

}
////////////////////////////////////////////////////////////////////////////////
void PolydataPropertySet::UpdateColorByScalarDataRange( propertystore::PropertyPtr property )
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

	UpdateColorByScalarData( property );
}
////////////////////////////////////////////////////////////////////////////////
bool PolydataPropertySet::ValidateColorByScalarMinMax( propertystore::PropertyPtr property, boost::any value )
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
void PolydataPropertySet::EnableLiveProperties( bool live )
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
void PolydataPropertySet::UpdateWarping( propertystore::PropertyPtr property )
{
	bool warpSurface = GetDatumValue< bool >( "UseWarpedSurface" );
	double warpScaleFactor = GetDatumValue< double >( "WarpedScaleFactor" );
	bool particlesOn = GetDatumValue< bool >( "Particles_ParticlesOn" ); // TODO: remove - this is not needed here

	m_updateLfxVtkPolyData.signal( warpSurface, warpScaleFactor, particlesOn );
}
////////////////////////////////////////////////////////////////////////////////
void PolydataPropertySet::UpdateParticleData( propertystore::PropertyPtr property )
{
	bool particlesOn = GetDatumValue< bool >( "Particles_ParticlesOn" );

	SetPropertyEnabled( "Particles_DiameterData", particlesOn );
	SetPropertyEnabled( "Particles_VmagData", particlesOn );
	SetPropertyEnabled( "Particles_ConversionFactor", particlesOn );
}
////////////////////////////////////////////////////////////////////////////////
// TODO: THIS IS FOR PARTICLE DATA ONLY.. PARTICLE DATA NEEDS ITS OWN PROPERTYSET and GUI PAGE
void PolydataPropertySet::UpdateTransientDataOptions( propertystore::PropertyPtr property )
{
    boost::ignore_unused_variable_warning( property );

    PSVectorOfStrings enumValues;
    std::string selectedDataset = boost::any_cast<std::string > ( GetPropertyValue( "DataSet" ) );
    DatasetPropertySet dataset;
    dataset.LoadByKey( "Filename", selectedDataset );
    enumValues = boost::any_cast< std::vector<std::string> >( dataset.GetPropertyValue( "TransientScalarNames" ) );
    if( enumValues.empty() )
    {
        enumValues.push_back( "No data available" );
    }

	// make sure properties are enabled to get the name change updates
	SetPropertyEnabled( "Particles_DiameterData", true );
	SetPropertyEnabled( "Particles_VmagData", true );

	std::string curDiam = boost::any_cast< std::string >( GetPropertyValue( "Particles_DiameterData" ) );
	std::string curVmag = boost::any_cast< std::string >( GetPropertyValue( "Particles_VmagData" ) );

	SetPropertyAttribute( "Particles_DiameterData", "enumValues", enumValues );
    SetPropertyAttribute( "Particles_VmagData", "enumValues", enumValues );

	bool setDiam = false, setVmag = false;
	for( size_t i=0; i<enumValues.size(); i++ )
	{
		std::string name = enumValues.at(i);
		if( !setDiam && ( curDiam == name || name.at(0) == 'd' || name.at(0) == 'D' ) )
		{
			SetPropertyValue( "Particles_DiameterData", name );
			setDiam  = true;
		}
		else if( !setVmag || curVmag == name )
		{
			SetPropertyValue( "Particles_VmagData", name );
			setVmag  = true;
		}
	}

	std::string name = enumValues.at(0);
	if( !setDiam )
	{
		SetPropertyValue( "Particles_DiameterData", name );
	}
	if( !setVmag )
	{
		SetPropertyValue( "Particles_VmagData", name );
	}


	// restore enabled state
	UpdateParticleData( propertystore::PropertyPtr() );
}
////////////////////////////////////////////////////////////////////////////////
