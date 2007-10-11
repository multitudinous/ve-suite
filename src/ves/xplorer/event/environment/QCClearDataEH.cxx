/*************** <auto-copyright.pl BEGIN do not edit this line> **************
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
 *************** <auto-copyright.pl END do not edit this line> ***************/

#include <ves/xplorer/event/QCClearDataEH.h>
#include <ves/xplorer/event/cfdModel.h>
#include <ves/xplorer/event/cfdQuatCamHandler.h>

#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>

using namespace VE_EVENTS;
using namespace VE_Xplorer;

////////////////////////////////////////////////////////////////////
QuatCamClearDataEventHandler::QuatCamClearDataEventHandler()
{
}
///////////////////////////////////////////////////////////////////
QuatCamClearDataEventHandler
::QuatCamClearDataEventHandler(const QuatCamClearDataEventHandler& ceh)
{
}
/////////////////////////////////////////////////////////////////////
QuatCamClearDataEventHandler::~QuatCamClearDataEventHandler()
{
}
///////////////////////////////////////////////////////////////////////////////////////
QuatCamClearDataEventHandler& 
QuatCamClearDataEventHandler::operator=(const QuatCamClearDataEventHandler& rhs)
{
   if(&rhs != this)
   {
      VE_EVENTS::EventHandler::operator=(rhs);
   }
   return *this;
}
///////////////////////////////////////////////////////////////
void QuatCamClearDataEventHandler::SetGlobalBaseObject(VE_Xplorer::cfdGlobalBase* baseObject)
{
}
/////////////////////////////////////////////////////////////////////////////////////   
void QuatCamClearDataEventHandler::Execute(VE_XML::XMLObject* veXMLObject)
{
   try
   {
      VE_XML::Command* command = dynamic_cast< VE_XML::Command* >( veXMLObject );
      //This isn't used but I think we need to pass in something to the command
      VE_XML::DataValuePairWeakPtr velFile = command->GetDataValuePair("Clear Quat Data");      
      std::string clearCmdDummy;
      velFile->GetData(clearCmdDummy);
      VE_Xplorer::cfdQuatCamHandler::instance()->ClearQuaternionData();
   }
   catch(...)
   {
      std::cout<<"Error!!"<<std::endl;
      std::cout<<"QuatCamClearDataEventHandler::_operateOnNode()"<<std::endl;
   }
}
