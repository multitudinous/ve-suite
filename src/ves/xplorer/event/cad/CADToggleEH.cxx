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
#include <ves/xplorer/event/CADToggleEH.h>
#include <ves/xplorer/event/cfdModel.h>
#include <ves/xplorer/event/ModelCADHandler.h>

#include <ves/xplorer/scenegraph/CADEntity.h>
#include <ves/xplorer/scenegraph/Clone.h>

#include <ves/open/xml/XMLObject.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/CAD/CADNode.h>

#include <iostream>

using namespace VE_EVENTS;
using namespace VE_CAD;
////////////////////////////////////////////////////////////////////////////
//Constructor                                                             //
////////////////////////////////////////////////////////////////////////////
CADToggleEventHandler::CADToggleEventHandler()
:VE_EVENTS::CADEventHandler()
{
}
////////////////////////////////////////////////////////////////////////////////
CADToggleEventHandler::CADToggleEventHandler(const CADToggleEventHandler& rhs)
:VE_EVENTS::CADEventHandler(rhs)
{
   
}
////////////////////////////////////////////////////////////////////////////////
///Destructor                                      //
////////////////////////////////////////////////////////////////////////////////
CADToggleEventHandler::~CADToggleEventHandler()
{
}
///Equal operator
////////////////////////////////////////////////////////////////////////////////
CADToggleEventHandler& CADToggleEventHandler::operator=(const CADToggleEventHandler& rhs)
{
   if(this != &rhs)
   {
      VE_EVENTS::CADEventHandler::operator=(rhs);
   }
   return *this;
}
//////////////////////////////////////////////////////////////////////////
void CADToggleEventHandler::_operateOnNode(VE_XML::XMLObject* xmlObject)
{
   try
   {
      VE_XML::Command* command = dynamic_cast<VE_XML::Command*>(xmlObject);
      VE_XML::DataValuePairWeakPtr toggleValue = command->GetDataValuePair("Toggle Value");
      VE_XML::DataValuePairWeakPtr nodeID = command->GetDataValuePair("Node ID");
      VE_XML::DataValuePairWeakPtr nodeType = command->GetDataValuePair("Node Type");

      //This assumes the part/assembly is there already
      if(nodeType->GetDataString() == std::string("Assembly"))
      {
         m_cadHandler->GetAssembly(nodeID->GetDataString())->ToggleDisplay(toggleValue->GetDataString());
      }
      else if(nodeType->GetDataString() == std::string("Part"))
      {
         std::cout<<"---Toggle part---"<<std::endl;
         std::cout<<"---"<<toggleValue->GetDataString()<<"---"<<std::endl;
         m_cadHandler->GetPart(nodeID->GetDataString())->GetDCS()->ToggleDisplay(toggleValue->GetDataString());
         std::cout<<"---Toggled part---"<<std::endl;
      }
      else if(nodeType->GetDataString() == std::string("Clone"))
      {
         m_cadHandler->GetClone(nodeID->GetDataString())->GetClonedGraph()->ToggleDisplay(toggleValue->GetDataString());
      }
   }
   catch(...)
   {
      std::cout<<"Error!!"<<std::endl;
      std::cout<<"---Invalid node specified to toggle!---"<<std::endl;
   }
}
