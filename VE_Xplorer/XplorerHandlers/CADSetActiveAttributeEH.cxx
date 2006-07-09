/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2006 by Iowa State University
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
#include "VE_Xplorer/XplorerHandlers/CADSetActiveAttributeEH.h"
#include "VE_Xplorer/XplorerHandlers/cfdModel.h"
#include "VE_Xplorer/SceneGraph/Utilities/Attribute.h"

#include "VE_Open/XML/XMLObject.h"
#include "VE_Open/XML/Command.h"
#include "VE_Open/XML/DataValuePair.h"
#include "VE_Open/XML/CAD/CADNode.h"
#include "VE_Open/XML/CAD/CADAttribute.h"
#include <iostream>

using namespace VE_SceneGraph::Utilities;
using namespace VE_EVENTS;
using namespace VE_CAD;
////////////////////////////////////////////////////////////////////////////
//Constructor                                                             //
////////////////////////////////////////////////////////////////////////////
CADSetActiveAttributeEventHandler::CADSetActiveAttributeEventHandler()
:VE_EVENTS::CADEventHandler()
{
}
///////////////////////////////////////////////////////////////////////////////////////
CADSetActiveAttributeEventHandler::CADSetActiveAttributeEventHandler(const CADSetActiveAttributeEventHandler& rhs)
:VE_EVENTS::CADEventHandler(rhs)
{
   
}
/////////////////////////////////////////////////////
///Destructor                                      //
/////////////////////////////////////////////////////
CADSetActiveAttributeEventHandler::~CADSetActiveAttributeEventHandler()
{
}
///Equal operator
//////////////////////////////////////////////////////////////////////////////////////////////////
CADSetActiveAttributeEventHandler& CADSetActiveAttributeEventHandler::operator=(const CADSetActiveAttributeEventHandler& rhs)
{
   if(this != &rhs)
   {
      VE_EVENTS::CADEventHandler::operator=(rhs);
   }
   return *this;
}
//////////////////////////////////////////////////////////////////////////
void CADSetActiveAttributeEventHandler::_operateOnNode(VE_XML::XMLObject* xmlObject)
{
   try
   {
      VE_XML::Command* command = dynamic_cast<VE_XML::Command*>(xmlObject);
      VE_XML::DataValuePair* nodeID = command->GetDataValuePair("Node ID");
      VE_XML::DataValuePair* nodeType = command->GetDataValuePair("Node Type");
      VE_XML::DataValuePair* activeAttribute = command->GetDataValuePair("Active Attribute");

      std::cout<<"--Setting active attribute--"<<std::endl;
      //VE_Xplorer::cfdModel* activeModel = dynamic_cast<VE_Xplorer::cfdModel*>(_baseObject);
      _activeModel->SetActiveAttributeOnNode(nodeID->GetUIntData(),nodeType->GetDataString(),activeAttribute->GetDataString());
      std::cout<<"--done--"<<std::endl;
   }
   catch(...)
   {
      std::cout<<"Couldn't add attribute to node!!!"<<std::endl;
      std::cout<<"CADAddAttributeEventHandler."<<std::endl;
   }
}
