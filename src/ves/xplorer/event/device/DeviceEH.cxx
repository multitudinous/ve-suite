/*************** <auto-copyright.pl BEGIN do not edit this line> *************
 *
 * VE-Suite is (C) Copyright 1998-2007 by Iowa State University
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
 *************** <auto-copyright.pl END do not edit this line> **************/
#include <string>

#include <ves/xplorer/event/device/DeviceEH.h>

#include <ves/xplorer/cfdGlobalBase.h>
#include <ves/xplorer/DeviceHandler.h>


#include <ves/open/xml/XMLObject.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>

#include <boost/filesystem/operations.hpp>
#include <boost/filesystem/path.hpp>

using namespace VE_EVENTS;
using namespace ves::open::xml;

////////////////////////////////////////////////////////////////////////////////
DeviceEventHandler::DeviceEventHandler()
:
VE_EVENTS::EventHandler()
{
   ;
}
////////////////////////////////////////////////////////////////////////////////
DeviceEventHandler::DeviceEventHandler( const DeviceEventHandler& rhs )
:
VE_EVENTS::EventHandler()
{
   ;
}
////////////////////////////////////////////////////////////////////////////////
DeviceEventHandler::~DeviceEventHandler()
{
   ;
}
////////////////////////////////////////////////////////////////////////////////
void DeviceEventHandler::SetGlobalBaseObject( VE_Xplorer::cfdGlobalBase* modelHandler )
{
   ;
}
////////////////////////////////////////////////////////////////////////////////
void DeviceEventHandler::Execute( XMLObject* veXMLObject )
{
   Command* command = dynamic_cast< Command* >( veXMLObject );
   
   std::string device;
   command->GetDataValuePair( "Device" )->GetData( device );

   VE_Xplorer::DeviceHandler::instance()->SetActiveDevice( device );
}
////////////////////////////////////////////////////////////////////////////////
DeviceEventHandler& DeviceEventHandler::operator=( const DeviceEventHandler& rhs )
{
   if( this != &rhs )
   {
      VE_EVENTS::EventHandler::operator=( rhs );
   }

   return *this;
}
////////////////////////////////////////////////////////////////////////////////
