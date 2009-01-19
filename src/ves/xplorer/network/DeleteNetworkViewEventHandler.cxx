/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2009 by Iowa State University
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
#include <ves/xplorer/network/DeleteNetworkViewEventHandler.h>
#include <ves/xplorer/Model.h>
#include <ves/xplorer/ModelHandler.h>
#include <ves/xplorer/network/cfdExecutive.h>
#include <ves/xplorer/plugin/PluginBase.h>


#include <ves/open/xml/XMLObject.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/FloatArray.h>
#include <ves/open/xml/Transform.h>
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/ParameterBlock.h>
#include <ves/open/xml/model/Model.h>

#include <ves/xplorer/Debug.h>

#include <iostream>

using namespace ves::xplorer::event;
using namespace ves::xplorer;
using namespace ves::open::xml;
using namespace ves::xplorer::plugin;
using namespace ves::xplorer::network;

////////////////////////////////////////////////////////////////////////////
//Constructor                                                             //
////////////////////////////////////////////////////////////////////////////
DeleteNetworkViewEventHandler::DeleteNetworkViewEventHandler()
        : ves::xplorer::event::EventHandler()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
DeleteNetworkViewEventHandler::DeleteNetworkViewEventHandler( const DeleteNetworkViewEventHandler& rhs )
        : ves::xplorer::event::EventHandler( rhs )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
///Destructor                                      //
////////////////////////////////////////////////////////////////////////////////
DeleteNetworkViewEventHandler::~DeleteNetworkViewEventHandler()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
///Equal operator
////////////////////////////////////////////////////////////////////////////////
DeleteNetworkViewEventHandler& DeleteNetworkViewEventHandler::operator=( const DeleteNetworkViewEventHandler& rhs )
{
    if( this != &rhs )
    {
        DeleteNetworkViewEventHandler::operator=( rhs );
    }
    return *this;
}
////////////////////////////////////////////////////////////////////////////////
void DeleteNetworkViewEventHandler::SetGlobalBaseObject( ves::xplorer::GlobalBase* model )
{
    ;
}
//////////////////////////////////////////////////////////////////////////
void DeleteNetworkViewEventHandler::Execute( const ves::open::xml::XMLObjectPtr& xmlObject )
{
    cfdExecutive::instance()->DeleteNetworkSystemView();
}
