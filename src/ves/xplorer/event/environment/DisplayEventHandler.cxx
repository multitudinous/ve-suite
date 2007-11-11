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

#include <ves/xplorer/event/environment/DisplayEventHandler.h>

#include <ves/xplorer/GlobalBase.h>
#include <ves/xplorer/cfdEnvironmentHandler.h>
#include <ves/xplorer/environment/DisplayInformation.h>

#include <ves/open/xml/XMLObject.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>

#include <boost/filesystem/operations.hpp>   //includes boost/filesystem/path.hpp
#include <boost/filesystem/path.hpp>

#ifdef WIN32
   #include <direct.h>
#else
   #include <unistd.h>
#endif

using namespace ves::xplorer::event;
using namespace ves::open::xml;

////////////////////////////////////////////////////////////////////////////////
DisplayEventHandler::DisplayEventHandler()
:ves::xplorer::event::EventHandler()
{
   ;
}
////////////////////////////////////////////////////////////////////////////////
DisplayEventHandler::DisplayEventHandler(const DisplayEventHandler& rhs)
:ves::xplorer::event::EventHandler()
{
   ;
}
////////////////////////////////////////////////////////////////////////////////
DisplayEventHandler::~DisplayEventHandler()
{
   ;
}
////////////////////////////////////////////////////////////////////////////////
void DisplayEventHandler::SetGlobalBaseObject(ves::xplorer::GlobalBase* modelHandler)
{
   ;
}
////////////////////////////////////////////////////////////////////////////////
void DisplayEventHandler::Execute(XMLObject* veXMLObject)
{
   Command* command=dynamic_cast<Command*>(veXMLObject);
   DataValuePairWeakPtr DVP;
   unsigned int value;

   if(command->GetDataValuePair("FrameRateID"))
	{
      DVP=command->GetDataValuePair("FrameRateID");
      DVP->GetData(value);
   
      ves::xplorer::cfdEnvironmentHandler::instance()->GetDisplayInformation()->SetFrameRateFlag( value );
   }

   else if(command->GetDataValuePair("CoordSysID"))
	{
      DVP=command->GetDataValuePair("CoordSysID");
      DVP->GetData(value);
   
      ves::xplorer::cfdEnvironmentHandler::instance()->GetDisplayInformation()->SetCoordSysFlag( value );
   }
}
////////////////////////////////////////////////////////////////////////////////
DisplayEventHandler& DisplayEventHandler::operator=(const DisplayEventHandler& rhs)
{
   if(this!=&rhs)
	{
      ves::xplorer::event::EventHandler::operator=(rhs);
   }

   return *this;
}
////////////////////////////////////////////////////////////////////////////////
