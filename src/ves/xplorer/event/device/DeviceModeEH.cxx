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
#include <VE_Xplorer/XplorerHandlers/DeviceModeEH.h>

#include <VE_Xplorer/XplorerHandlers/cfdGlobalBase.h>
#include <VE_Xplorer/XplorerHandlers/DeviceHandler.h>


#include <VE_Open/XML/XMLObject.h>
#include <VE_Open/XML/Command.h>
#include <VE_Open/XML/DataValuePair.h>

#include <boost/filesystem/operations.hpp> //includes boost/filesystem/path.hpp
#include <boost/filesystem/path.hpp>

#include <string>

#ifdef WIN32
#include <direct.h>
#else
#include <unistd.h>
#endif

using namespace VE_EVENTS;

////////////////////////////////////////////////////////////////////////////////
DeviceModeEventHandler::DeviceModeEventHandler()
:
VE_EVENTS::EventHandler()
{
   ;
}
////////////////////////////////////////////////////////////////////////////////
DeviceModeEventHandler::DeviceModeEventHandler( const DeviceModeEventHandler& rhs )
:
VE_EVENTS::EventHandler()
{
   ;
}
////////////////////////////////////////////////////////////////////////////////
DeviceModeEventHandler::~DeviceModeEventHandler()
{
   ;
}
////////////////////////////////////////////////////////////////////////////////
void DeviceModeEventHandler::SetGlobalBaseObject( VE_Xplorer::cfdGlobalBase* modelHandler )
{
   ;
}
////////////////////////////////////////////////////////////////////////////////
void DeviceModeEventHandler::Execute( VE_XML::XMLObject* veXMLObject )
{
   VE_XML::Command* command = dynamic_cast< VE_XML::Command* >( veXMLObject );
   
   std::string mode;
   command->GetDataValuePair( "Mode" )->GetData( mode );

   VE_Xplorer::DeviceHandler::instance()->SetDeviceMode( mode );
}
////////////////////////////////////////////////////////////////////////////////
DeviceModeEventHandler& DeviceModeEventHandler::operator=( const DeviceModeEventHandler& rhs )
{
   if( this != &rhs )
   {
      VE_EVENTS::EventHandler::operator=( rhs );
   }

   return *this;
}
////////////////////////////////////////////////////////////////////////////////
