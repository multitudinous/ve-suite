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
#include "VE_Xplorer/XplorerHandlers/CADAddNodeEH.h"
#include "VE_Xplorer/XplorerHandlers/cfdModel.h"

#include "VE_Open/XML/XMLObject.h"
#include "VE_Open/XML/Command.h"
#include "VE_Open/XML/FloatArray.h"
#include "VE_Open/XML/Transform.h"
#include "VE_Open/XML/DataValuePair.h"
#include "VE_Open/XML/CAD/CADNode.h"
#include "VE_Open/XML/CAD/CADPart.h"
#include "VE_Open/XML/CAD/CADAssembly.h"
#include "VE_Open/XML/CAD/CADClone.h"
#include <iostream>
using namespace VE_EVENTS;
using namespace VE_XML::VE_CAD;
////////////////////////////////////////////////////////////////////////////
//Constructor                                                             //
////////////////////////////////////////////////////////////////////////////
CADAddNodeEventHandler::CADAddNodeEventHandler()
:VE_EVENTS::CADEventHandler()
{
}
///////////////////////////////////////////////////////////////////////////////////////
CADAddNodeEventHandler::CADAddNodeEventHandler(const CADAddNodeEventHandler& rhs)
:VE_EVENTS::CADEventHandler(rhs)
{
   
}
/////////////////////////////////////////////////////
///Destructor                                      //
/////////////////////////////////////////////////////
CADAddNodeEventHandler::~CADAddNodeEventHandler()
{
}
///Equal operator
//////////////////////////////////////////////////////////////////////////////////////////////////
CADAddNodeEventHandler& CADAddNodeEventHandler::operator=(const CADAddNodeEventHandler& rhs)
{
   if(this != &rhs)
   {
      VE_EVENTS::CADEventHandler::operator=(rhs);
   }
   return *this;
}
//////////////////////////////////////////////////////////////////////////
void CADAddNodeEventHandler::_operateOnNode(VE_XML::XMLObject* xmlObject)
{
   try
   {
      VE_XML::Command* command = dynamic_cast<VE_XML::Command*>(xmlObject);
      VE_XML::DataValuePair* cadNode = command->GetDataValuePair("New Node");
      std::string nodeType = dynamic_cast<VE_XML::VE_CAD::CADNode*>(cadNode->GetDataXMLObject())->GetNodeType();

      VE_XML::VE_CAD::CADNode* node = 0;
      VE_XML::VE_CAD::CADAssembly* assembly = 0; 
      VE_XML::VE_CAD::CADPart* part = 0;
      VE_XML::VE_CAD::CADClone* clone = 0;

		VE_SceneGraph::DCS* parentAssembly = 0;

      if(nodeType == "Assembly")
      {
         
         assembly = dynamic_cast<VE_XML::VE_CAD::CADAssembly*>(cadNode->GetDataXMLObject());
         node = dynamic_cast<CADNode*>(assembly);
         if(_activeModel->AssemblyExists(node->GetID()))
         {
            throw("Assembly already exists");
         }
      }
      else if(nodeType == "Part")
      {
         part = dynamic_cast<VE_XML::VE_CAD::CADPart*>(cadNode->GetDataXMLObject());
         node = dynamic_cast<CADNode*>(part);
         if(_activeModel->PartExists(node->GetID()))
         {
            throw("Part already exists");
         }
      }

      ///This is the root
      if(node->GetParent().empty())
      {
         ///add the root to the VEBaseClass DCS
         node->SetParent("rootNode");
         _activeModel->SetRootCADNodeID(node->GetID());
      }
      else
      {
         //need to check if parent is on the graph already
         if(!_activeModel->AssemblyExists(node->GetParent()))
         {
            _activeModel->CreateAssembly(node->GetParent());
            //We have to initialize some properties on the root node since
            //we are not creating it from xml data.
            //From intial creation, the top level node is called Model_Geometry in the GUI.
            //After that, CADSetNameEH handles the name appropriately.
            _activeModel->GetAssembly(node->GetParent())->setName("Model_Geometry");

            //update the top level node descriptors
            SetNodeDescriptors(node->GetParent(),"Assembly","VE_XML_ID",node->GetParent());
            //Add the top level CAD to the VEBaseClass
            _activeModel->GetDCS()->addChild(_activeModel->GetAssembly(node->GetParent()));
         }
      }
      
      if(nodeType == "Assembly")
         _addNodeToNode(node->GetParent(),assembly);
      else if(nodeType == "Part")
         _addNodeToNode(node->GetParent(),part);
   }
   catch(char* str)
   {
      std::cout<<str<<std::endl;
   }
   catch(...)
   {
   }
   
}
