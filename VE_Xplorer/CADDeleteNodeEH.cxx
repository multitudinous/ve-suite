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
 * File:          $RCSfile: SceneGraphBuilder.cxx,v $
 * Date modified: $Date: 2006-01-10 13:45:28 -0600 (Tue, 10 Jan 2006) $
 * Version:       $Rev: 3477 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "VE_Xplorer/CADDeleteNodeEH.h"
#include "VE_Xplorer/cfdModel.h"
#include "VE_Xplorer/cfdFILE.h"
#include "VE_SceneGraph/cfdDCS.h"
#include "VE_SceneGraph/cfdClone.h"

#include "VE_Open/XML/XMLObject.h"
#include "VE_Open/XML/Command.h"
#include "VE_Open/XML/FloatArray.h"
#include "VE_Open/XML/Transform.h"
#include "VE_Open/XML/DataValuePair.h"
#include "VE_Open/XML/CAD/CADNode.h"
#include <iostream>
using namespace VE_EVENTS;
using namespace VE_CAD;
////////////////////////////////////////////////////////////////////////////
//Constructor                                                             //
////////////////////////////////////////////////////////////////////////////
CADDeleteNodeEventHandler::CADDeleteNodeEventHandler()
:VE_EVENTS::CADEventHandler()
{
}
///////////////////////////////////////////////////////////////////////////////////////
CADDeleteNodeEventHandler::CADDeleteNodeEventHandler(const CADDeleteNodeEventHandler& rhs)
:VE_EVENTS::CADEventHandler(rhs)
{
   
}
/////////////////////////////////////////////////////
///Destructor                                      //
/////////////////////////////////////////////////////
CADDeleteNodeEventHandler::~CADDeleteNodeEventHandler()
{
}
///Equal operator
//////////////////////////////////////////////////////////////////////////////////////////////////
CADDeleteNodeEventHandler& CADDeleteNodeEventHandler::operator=(const CADDeleteNodeEventHandler& rhs)
{
   if(this != &rhs)
   {
      VE_EVENTS::CADEventHandler::operator=(rhs);
   }
   return *this;
}
//////////////////////////////////////////////////////////////////////////
void CADDeleteNodeEventHandler::_operateOnNode(VE_XML::XMLObject* xmlObject)
{
   try
   {
      VE_XML::Command* command = dynamic_cast<VE_XML::Command*>(xmlObject);
      VE_XML::DataValuePair* parentID = command->GetDataValuePair("Parent ID");
      VE_XML::DataValuePair* nodeID = command->GetDataValuePair("Node ID");
      VE_XML::DataValuePair* nodeType = command->GetDataValuePair("Node Type");

      //VE_Xplorer::cfdModel* activeModel = dynamic_cast<VE_Xplorer::cfdModel*>(_baseObject);
      std::cout<<"---Deleting node---"<<std::endl;
      VE_SceneGraph::cfdDCS* parentAssembly = 0;
      parentAssembly = _activeModel->GetAssembly(parentID->GetUIntData());

      //This assumes the part/assembly isn't there already
      if(nodeType->GetDataString() == std::string("Assembly"))
      {
         parentAssembly->RemoveChild(_activeModel->GetAssembly(nodeID->GetUIntData()));
      }
      else if(nodeType->GetDataString() == std::string("Part"))
      {
         parentAssembly->RemoveChild(_activeModel->GetPart(nodeID->GetUIntData())->GetDCS());
      }
      else if(nodeType->GetDataString() == std::string("Clone"))
      {
         parentAssembly->RemoveChild(_activeModel->GetClone(nodeID->GetUIntData())->GetClonedGraph());
      }
   }
   catch(...)
   {
      std::cout<<"Error!!"<<std::endl;
      std::cout<<"---Invalid node specified to remove!---"<<std::endl;
   }
}
