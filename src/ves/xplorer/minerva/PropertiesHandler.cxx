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

///////////////////////////////////////////////////////////////////////////////
//
//  Event handler for set geographic properties command.
//
///////////////////////////////////////////////////////////////////////////////

#include <ves/xplorer/minerva/PropertiesHandler.h>
#include <ves/xplorer/minerva/MinervaManager.h>
#include <ves/xplorer/minerva/ModelWrapper.h>

#include <ves/xplorer/Model.h>
#include <ves/xplorer/ModelHandler.h>
#include <ves/xplorer/ModelCADHandler.h>

#include <ves/xplorer/scenegraph/CADEntity.h>

#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>

#include <ves/util/commands/Minerva.h>

#include <boost/assert.hpp>

using namespace ves::xplorer::minerva;


///////////////////////////////////////////////////////////////////////////////
//
//  Constructor.
//
///////////////////////////////////////////////////////////////////////////////

PropertiesHandler::PropertiesHandler() : BaseClass()
{
}


///////////////////////////////////////////////////////////////////////////////
//
//  Destructor.
//
///////////////////////////////////////////////////////////////////////////////

PropertiesHandler::~PropertiesHandler()
{
}


///////////////////////////////////////////////////////////////////////////////
//
//  
//
///////////////////////////////////////////////////////////////////////////////

void PropertiesHandler::Execute ( CommandPtr command, MinervaManager& manager )
{
  ves::open::xml::DataValuePairPtr nodeIDData ( command->GetDataValuePair( "Node ID" ) );
  ves::open::xml::DataValuePairPtr nodeTypeData ( command->GetDataValuePair( "Node Type" ) );

  ves::open::xml::DataValuePairPtr longitudeData ( command->GetDataValuePair ( ves::util::names::LONGITUDE_VALUE ) );
  ves::open::xml::DataValuePairPtr latitudeData ( command->GetDataValuePair ( ves::util::names::LATITUDE_VALUE ) );

  // Note: node id is a guid.
  std::string nodeId;
  nodeIDData->GetData ( nodeId );

  double longitude ( 0.0 );
  longitudeData->GetData ( longitude );

  double latitude ( 0.0 );
  latitudeData->GetData ( latitude );

  ModelWrapper::RefPtr modelWrapper ( this->GetOrCreateModel ( nodeId, manager ) );

  // Model wrapper may be null if no CAD exists with node id.
  if ( modelWrapper.valid() )
  {
    modelWrapper->location ( osg::Vec3d ( longitude, latitude, 0.0 ) );
    manager.UpdateModel ( modelWrapper.get() );
  }
}
