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
#include <ves/xplorer/event/viz/VolumeVisFeatureMaker.h>

#include <ves/xplorer/event/volume/VolumeVisSlots.h>

#include <ves/xplorer/data/PropertySet.h>
#include <ves/xplorer/data/VolumeVisPropertySet.h>

#include <boost/any.hpp>

using namespace ves::conductor;
using namespace ves;
////////////////////////////////////////////////////////////////////////////////
VolumeVisFeatureMaker::VolumeVisFeatureMaker()
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
    xplorer::data::PropertySetPtr ptr = xplorer::data::PropertySetPtr( new xplorer::data::VolumeVisPropertySet() );
    ptr->SetUUID( recordUUID );
    ptr->LoadFromDatabase();
    AddPlane( ptr );
    //Execute( ptr );
}
////////////////////////////////////////////////////////////////////////////////
void VolumeVisFeatureMaker::UpdateContourInformation( xplorer::data::PropertySet& )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void VolumeVisFeatureMaker::AddPlane( xplorer::data::PropertySetPtr& set )
{

    std::string const currentScalar = boost::any_cast<std::string >
        ( set->GetPropertyAttribute( "DataSet_ScalarData", "enumCurrentString" ) );
                                
    std::string const currentDataset = boost::any_cast<std::string >
        ( set->GetPropertyAttribute( "DataSet", "enumCurrentString" ) );
    
    double minimumValue = 
        boost::any_cast<double>( set->GetPropertyValue( "DataSet_ScalarRange_Min" ) );
    
    double maximumValue = 
        boost::any_cast<double>( set->GetPropertyValue( "DataSet_ScalarRange_Max" ) );
    
    using namespace ves::xplorer::event::volume;
    //1. ActivateTextureVisualization - TB_ACTIVATE
    ActivateTBDataset( currentDataset );
    //2. _updateActiveScalar - TB_ACTIVE_SOLUTION
    UpdateTBSolution( currentScalar, "Scalar", minimumValue, maximumValue );
    //3. TB_SCALAR_RANGE
    UpdateScalarRange( minimumValue, maximumValue );
}
////////////////////////////////////////////////////////////////////////////////
void VolumeVisFeatureMaker::UpdateAdvancedSettings( xplorer::data::PropertySet& )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
