/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2008 by Iowa State University
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
// --- VE-Suite Includes --- //
#include <ves/xplorer/event/cad/TogglePluginsEventHandler.h>

#include <ves/xplorer/scenegraph/SceneManager.h>
#include <ves/xplorer/scenegraph/util/TogglePluginVisitor.h>

#include <ves/open/xml/XMLObject.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>

#include <ves/xplorer/Debug.h>

// --- OSG Includes --- //
#include <osg/MatrixTransform>
#include <osg/Group>

// --- C/C++ Libraries --- //
#include <string>

using namespace ves::xplorer::event;
using namespace ves::xplorer::event::cad;
using namespace ves::xplorer;
using namespace ves::xplorer::scenegraph;
using namespace ves::open::xml;

////////////////////////////////////////////////////////////////////////////////
TogglePluginsEventHandler::TogglePluginsEventHandler()
        :
        ves::xplorer::event::EventHandler()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
TogglePluginsEventHandler::~TogglePluginsEventHandler()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
TogglePluginsEventHandler::TogglePluginsEventHandler( const TogglePluginsEventHandler& rhs )
        :
        ves::xplorer::event::EventHandler( rhs )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
TogglePluginsEventHandler& TogglePluginsEventHandler::operator=( const TogglePluginsEventHandler& rhs )
{
    if( this != &rhs )
    {
        ves::xplorer::event::EventHandler::operator=( rhs );
    }

    return *this;
}
////////////////////////////////////////////////////////////////////////////////
void TogglePluginsEventHandler::SetGlobalBaseObject( ves::xplorer::GlobalBase* model )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void TogglePluginsEventHandler::Execute( const ves::open::xml::XMLObjectPtr& xmlObject )
{
    CommandPtr command( 
        boost::dynamic_pointer_cast< ves::open::xml::Command >( xmlObject ) );
    DataValuePairPtr activeModelDVP =
        command->GetDataValuePair( "VE_XPLORER_PLUGIN_ID" );

    if( !activeModelDVP )
    {
        return;
    }
    std::string viewData;
    activeModelDVP->GetData( viewData );
    ves::xplorer::scenegraph::util::TogglePluginVisitor( 
        SceneManager::instance()->GetWorldDCS(), true, viewData );
}
////////////////////////////////////////////////////////////////////////////////
