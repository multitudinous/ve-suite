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
#include <ves/xplorer/event/viz/VolumeVisFeatureMaker.h>

#include <ves/xplorer/event/volume/VolumeVisSlots.h>

#include <propertystore/PropertySet.h>
#include <ves/xplorer/data/VolumeVisPropertySet.h>
#include <ves/xplorer/Debug.h>

#include <ves/xplorer/SteadyStateVizHandler.h>

#include <boost/any.hpp>

using namespace ves::conductor;
using namespace ves;
////////////////////////////////////////////////////////////////////////////////
VolumeVisFeatureMaker::VolumeVisFeatureMaker()
    :
    VisFeatureMakerBase()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
VolumeVisFeatureMaker::VolumeVisFeatureMaker( const VolumeVisFeatureMaker& orig )
    :
    VisFeatureMakerBase( orig )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
VolumeVisFeatureMaker::~VolumeVisFeatureMaker() 
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void VolumeVisFeatureMaker::Update( const::std::string& recordUUID )
{
    // For now we won't worry about how to discover an existing plane that needs
    // to be deleted, moved, etc. We will just create a new one
    propertystore::PropertySetPtr ptr = propertystore::PropertySetPtr( new xplorer::data::VolumeVisPropertySet() );
    ptr->SetUUID( recordUUID );
    ptr->Load();
    if( AddPlaneLfxDs( ptr ) ) return;
	AddPlaneLfxVtk( ptr );
    //Execute( ptr );
}

////////////////////////////////////////////////////////////////////////////////
bool VolumeVisFeatureMaker::AddPlaneLfxDs( propertystore::PropertySetPtr& set )
{

    std::string const currentScalar = boost::any_cast<std::string >
                                      ( set->GetPropertyValue( "DataSet_ScalarData" ) );

    std::string const currentDataset = boost::any_cast<std::string >
                                       ( set->GetPropertyValue( "DataSet" ) );

    double minimumValue =
        boost::any_cast<double>( set->GetPropertyValue( "DataSet_ScalarRange_Min" ) );

    double maximumValue =
        boost::any_cast<double>( set->GetPropertyValue( "DataSet_ScalarRange_Max" ) );
	
	using namespace ves::xplorer::event::volume;
	lfx::core::DataSetPtr ds = activateLfxDataSet( currentDataset );
    if( !ds ) return false; // we don't have an lfx::core::DataSet

	lfx::core::RendererPtr rp = ds->getRenderer();
	if( !rp ) return true;

	std::stringstream ss;
	rp->dumpUniformInfo( ss, true );
	std::string str = ss.str();
	vprDEBUG( vesDBG, 0 )
		<< "|\tLfxDataSet Uniform Info " << std::endl << str;

	std::string uuid = set->GetUUIDAsString();
	ves::xplorer::SteadyStateVizHandler::instance()->SetLfxDataObjReady( true, uuid );

	return true;

	//activeObject->SetCursorType( NONE );
    //activeObject->SetUpdateFlag( false );
    //call back over to ssvishandler to set the flags
    //SteadyStateVizHandler::instance()->SetActiveVisObject( activeObject );
    //SteadyStateVizHandler::instance()->SetComputeActorsAndGeodes( true );
	

	// VisFeatureMakerBase::Execute( propertystore::PropertySetPtr set )
	 // SteadyStateVizHandler::instance()->SetActiveVisObject( activeObject );

	/*
	PUBLIC UNIFORMS

	Available uniforms:
tf1d	SAMPLER_1D	1D transfer function sampler unit.
	Default: 0
tfRange	FLOAT_VEC2	Transfer function input range (x=min, y=max).
	Default: 0 1
tfDest	FLOAT_VEC4	Transfer function destination as rgba mask.
	Default: 0 0 0 1
hmEpsilon	FLOAT	Hardware mask comparison epsilon.
	Default: 0
volumeNumPlanes	FLOAT	Number of planes to render the volume.
	Default: 0
volumeMaxSamples	FLOAT	Max ray Samples for ray traced rendering.
	Default: 0
volumeTransparency	FLOAT	Alpha coefficient, default: 1.0.
	Default: 0
volumeTransparencyEnable	BOOL	Blending enable, default: true.
	Default: 0
volumeClipPlaneEnable0	INT	Clip plane 0: 1=enabled, 0=disabled.
	Default: 0
volumeClipPlaneEnable1	INT	Clip plane 1: 1=enabled, 0=disabled.
	Default: 0
volumeClipPlaneEnable2	INT	Clip plane 2: 1=enabled, 0=disabled.
	Default: 0
volumeClipPlaneEnable3	INT	Clip plane 3: 1=enabled, 0=disabled.
	Default: 0
volumeClipPlaneEnable4	INT	Clip plane 4: 1=enabled, 0=disabled.
	Default: 0
volumeClipPlaneEnable5	INT	Clip plane 5: 1=enabled, 0=disabled.
	Default: 0


	*/


	/*
	// replace these...  pass data down to lattice fx...
	// 
    using namespace ves::xplorer::event::volume;
    //1. ActivateTextureVisualization - TB_ACTIVATE
    ActivateTBDataset( currentDataset );
    //2. _updateActiveScalar - TB_ACTIVE_SOLUTION
    UpdateTBSolution( currentScalar, "Scalar", minimumValue, maximumValue );
    //3. TB_SCALAR_RANGE
    UpdateScalarRange( minimumValue, maximumValue );
	*/
}

////////////////////////////////////////////////////////////////////////////////
void VolumeVisFeatureMaker::AddPlaneLfxVtk( propertystore::PropertySetPtr& set )
{

    std::string const currentScalar = boost::any_cast<std::string >
                                      ( set->GetPropertyValue( "DataSet_ScalarData" ) );

    std::string const currentDataset = boost::any_cast<std::string >
                                       ( set->GetPropertyValue( "DataSet" ) );

    double minimumValue =
        boost::any_cast<double>( set->GetPropertyValue( "DataSet_ScalarRange_Min" ) );

    double maximumValue =
        boost::any_cast<double>( set->GetPropertyValue( "DataSet_ScalarRange_Max" ) );

	// replace these...  pass data down to lattice fx...
	// 


    using namespace ves::xplorer::event::volume;
    //1. ActivateTextureVisualization - TB_ACTIVATE
    ActivateTBDataset( currentDataset );
    //2. _updateActiveScalar - TB_ACTIVE_SOLUTION
    UpdateTBSolution( currentScalar, "Scalar", minimumValue, maximumValue );
    //3. TB_SCALAR_RANGE
    UpdateScalarRange( minimumValue, maximumValue );
}


////////////////////////////////////////////////////////////////////////////////
