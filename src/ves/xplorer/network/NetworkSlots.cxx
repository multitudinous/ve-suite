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
#include <ves/xplorer/network/NetworkSlots.h>

#include <ves/xplorer/scenegraph/SceneManager.h>

#include <ves/xplorer/network/GraphicalPluginManager.h>

#include <ves/xplorer/data/DatabaseManager.h>

#include <ves/xplorer/DeviceHandler.h>
#include <ves/xplorer/ModelHandler.h>
namespace ves
{
namespace xplorer
{
namespace network
{
////////////////////////////////////////////////////////////////////////////////
void UpdateNetwork()
{
    // CORBA no longer runs in typical desktop mode, so we have to test
    /*if( ves::xplorer::network::GraphicalPluginManager::instance()->GetCORBAInterface() )
    {
        ves::xplorer::network::GraphicalPluginManager::instance()->GetCORBAInterface()->GetNetworkFromCE();
    }*/
    ves::xplorer::network::GraphicalPluginManager::instance()->LoadDataFromCE();
    
    ves::xplorer::DeviceHandler::instance()->SetActiveDCS(
        ves::xplorer::scenegraph::SceneManager::instance()->GetActiveNavSwitchNode() );
}
////////////////////////////////////////////////////////////////////////////////
void NewFileLoading( std::string const& )
{
    //Set active model to null so that if the previous active model is deleted
    //that we don't get errors in our code other places.
    std::string nullString;
    ves::xplorer::ModelHandler::instance()->SetActiveModel( nullString );
    
    ves::xplorer::data::DatabaseManager::instance()->ResetAll();
}
////////////////////////////////////////////////////////////////////////////////
}
}
}

