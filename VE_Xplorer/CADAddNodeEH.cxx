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
#include "VE_Xplorer/CADAddNodeEH.h"
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
      VE_XML::DataValuePair* parentID = command->GetDataValuePair("Parent ID");
      VE_XML::DataValuePair* nodeID = command->GetDataValuePair("Node ID");
      VE_XML::DataValuePair* nodeType = command->GetDataValuePair("Node Type");
      VE_XML::DataValuePair* transform = command->GetDataValuePair("Transform");
      VE_XML::DataValuePair* nodeName = command->GetDataValuePair("Node Name");
      VE_XML::Transform* rawTransform = dynamic_cast<VE_XML::Transform*>(transform->GetDataXMLObject());

      VE_Xplorer::cfdModel* activeModel = dynamic_cast<VE_Xplorer::cfdModel*>(_baseObject);
      std::cout<<"---Adding node---"<<std::endl;
      VE_SceneGraph::cfdDCS* parentAssembly = 0;
      parentAssembly = activeModel->GetAssembly(parentID->GetUIntData());
      if(!parentAssembly)
      {
         //create the root
         activeModel->CreateAssembly(parentID->GetUIntData());
         parentAssembly = activeModel->GetAssembly(parentID->GetUIntData());
         activeModel->GetCfdDCS()->AddChild(parentAssembly);
         std::cout<<"Root ID: "<<parentID->GetUIntData()<<std::endl;
      }

      //This assumes the part/assembly isn't there already
      if(nodeType->GetDataString() == std::string("Assembly"))
      {
         activeModel->CreateAssembly(nodeID->GetUIntData());

         parentAssembly->AddChild(activeModel->GetAssembly(nodeID->GetUIntData()));

         activeModel->GetAssembly(nodeID->GetUIntData())->SetTranslationArray(rawTransform->GetTranslationArray()->GetArray() );
         activeModel->GetAssembly(nodeID->GetUIntData())->SetRotationArray(rawTransform->GetRotationArray()->GetArray() );
         activeModel->GetAssembly(nodeID->GetUIntData())->SetScaleArray(rawTransform->GetScaleArray()->GetArray() );
         activeModel->GetAssembly(nodeID->GetUIntData())->SetName(nodeName->GetDataString());
         std::cout<<"Parent ID: "<<parentID->GetUIntData()<<std::endl;
         std::cout<<"Assembly ID: "<<nodeID->GetUIntData()<<std::endl;
      }
      else if(nodeType->GetDataString() == std::string("Part"))
      {
         VE_XML::DataValuePair* partFileName = command->GetDataValuePair("CAD Filename");
         activeModel->CreatePart(partFileName->GetDataString(),nodeID->GetUIntData(),parentID->GetUIntData());
	      
         VE_Xplorer::cfdFILE* partNode = activeModel->GetPart(nodeID->GetUIntData());
         partNode->GetNode()->SetName(nodeName->GetDataString());
	      VE_SceneGraph::cfdDCS* partDCS = partNode->GetDCS();

         partDCS->SetTranslationArray(rawTransform->GetTranslationArray()->GetArray() );
         partDCS->SetRotationArray(rawTransform->GetRotationArray()->GetArray() );
         partDCS->SetScaleArray(rawTransform->GetScaleArray()->GetArray() );
         std::cout<<"Parent ID: "<<parentID->GetUIntData()<<std::endl;
         std::cout<<"Part ID: "<<nodeID->GetUIntData()<<std::endl;
      
      }
      else if(nodeType->GetDataString() == std::string("Clone"))
      {
         
         VE_XML::DataValuePair* originalNodeType = command->GetDataValuePair("Original Type");
         VE_XML::DataValuePair* originalNodeID = command->GetDataValuePair("Original ID");

         activeModel->CreateClone(nodeID->GetUIntData(),originalNodeID->GetUIntData(),originalNodeType->GetDataString());

         VE_SceneGraph::cfdClone* newClone = activeModel->GetClone(nodeID->GetUIntData());
         if(newClone)
         {
            newClone->GetClonedGraph()->SetTranslationArray(rawTransform->GetTranslationArray()->GetArray() );
            newClone->GetClonedGraph()->SetRotationArray(rawTransform->GetRotationArray()->GetArray() );
            newClone->GetClonedGraph()->SetScaleArray(rawTransform->GetScaleArray()->GetArray() );
            parentAssembly->AddChild(newClone->GetClonedGraph());
         }
         std::cout<<"Parent ID: "<<parentID->GetUIntData()<<std::endl;
         std::cout<<"Clone ID: "<<nodeID->GetUIntData()<<std::endl;
      
      }
   }
   catch(...)
   {
   }
}
