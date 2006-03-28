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
#include "VE_Xplorer/CADEventHandler.h"
#include "VE_Xplorer/cfdModel.h"
#include "VE_Xplorer/cfdFILE.h"
#include "VE_Xplorer/cfdGlobalBase.h"
#include "VE_Xplorer/cfdModel.h"

#include "VE_SceneGraph/cfdDCS.h"
#include "VE_SceneGraph/cfdClone.h"

#include "VE_Open/XML/CAD/CADNode.h"
#include "VE_Open/XML/CAD/CADAssembly.h"
#include "VE_Open/XML/CAD/CADClone.h"
#include "VE_Open/XML/CAD/CADAttribute.h"
#include "VE_Open/XML/CAD/CADPart.h"
#include "VE_Open/XML/XMLObject.h"
#include "VE_Open/XML/Transform.h"
#include "VE_Open/XML/FloatArray.h"
#include <iostream>

using namespace VE_EVENTS;
using namespace VE_CAD;
using namespace VE_SceneGraph;
//////////////////////////////////////////////////////////
///Constructor                                          //
//////////////////////////////////////////////////////////
CADEventHandler::CADEventHandler()
:VE_EVENTS::EventHandler()
{
   _cadNode = 0;
   _activeModel = 0;
}
////////////////////////////////////////////////////////////
CADEventHandler::CADEventHandler(const CADEventHandler& rhs)
:VE_EVENTS::EventHandler()
{
   _cadNode = rhs._cadNode;
   _activeModel = rhs._activeModel;
}
////////////////////////////////////
///Destructor                     //
////////////////////////////////////
CADEventHandler::~CADEventHandler()
{
}
///////////////////////////////////////////////////////////////////////////
void CADEventHandler::SetGlobalBaseObject(VE_Xplorer::cfdGlobalBase* model)
{
   try
   {
      _activeModel = dynamic_cast<VE_Xplorer::cfdModel*>(model);
   }
   catch(...)
   {
      _activeModel = 0;
      std::cout<<"Invalid object passed to CADEventHandler!"<<std::endl;
   }
}
///////////////////////////////////////////////////////
///Exectute the event                                //
///////////////////////////////////////////////////////
void CADEventHandler::Execute(VE_XML::XMLObject* veXMLObject)
{
   if(_activeModel)
   {
      //this is overridden in derived classes
      _operateOnNode(veXMLObject);
   }
}
///////////////////////////////////////////////////////////////////////
CADEventHandler& CADEventHandler::operator=(const CADEventHandler& rhs)
{
   if(this != &rhs)
   {
      _cadNode = rhs._cadNode;
      _activeModel = rhs._activeModel;
   }
   return *this;
}
///////////////////////////////////////////////////////////////
void CADEventHandler::_setAttributesOnNode(CADNode* activeNode)
{
   //std::cout<<"Setting Attributes!!"<<std::endl;
   //set attributes
   size_t nAttributes = 0;
   nAttributes = activeNode->GetAttributeList().size();
   for(size_t i = 0;  i < nAttributes; i++)
   {
      CADAttribute currentAttribute = activeNode->GetAttribute(i);
    //  std::cout<<"Adding attribute: "<<currentAttribute.GetAttributeName()<<std::endl;
      _activeModel->AddAttributeToNode(activeNode->GetID(),&currentAttribute);
   }
   if(nAttributes)
   {
      _activeModel->SetActiveAttributeOnNode(activeNode->GetID(),
                                             activeNode->GetNodeType(), 
                                             activeNode->GetActiveAttribute().GetAttributeName());
   }
}
///////////////////////////////////////////////////////
void CADEventHandler::_setTransformOnNode(CADNode* activeNode)
{
   //set the transform
   cfdDCS* transform = 0;
   unsigned int nodeID = activeNode->GetID();
   if(activeNode->GetNodeType() == "Assembly")
   {
      //std::cout<<"Setting transform on Assembly: "<<nodeID<<std::endl;
      transform = _activeModel->GetAssembly(nodeID);
   }
   else if(activeNode->GetNodeType() == "Part")
   {
      //std::cout<<"Setting transform on Part: "<<nodeID<<std::endl;
      transform = _activeModel->GetPart(nodeID)->GetDCS();
   }
   else if(activeNode->GetNodeType() == "Clone")
   {
      //std::cout<<"Setting transform on Clone: "<<nodeID<<std::endl;
      transform = _activeModel->GetClone(nodeID)->GetClonedGraph();
   }
   if(transform)
   {
      transform->SetTranslationArray(activeNode->GetTransform()->GetTranslationArray()->GetArray() );
      transform->SetRotationArray(activeNode->GetTransform()->GetRotationArray()->GetArray() );
      transform->SetScaleArray(activeNode->GetTransform()->GetScaleArray()->GetArray() );
   }
   else
   {
      std::cout<<"No transform found!!"<<std::endl;
   }
}
/////////////////////////////////////////////////////////////////////////
void CADEventHandler::_addNodeToNode(unsigned int parentID, CADNode* activeNode)
{
   cfdDCS* parentAssembly = 0;
   parentAssembly = _activeModel->GetAssembly(parentID);

   //std::cout<<"---Adding node---"<<std::endl;
   if(parentAssembly)
   {
      if(activeNode->GetNodeType() == "Assembly")
      {
         CADAssembly* newAssembly = dynamic_cast<CADAssembly*>(activeNode);
         //std::cout<<"---Assembly---"<<std::endl;
         //std::cout<<"   ---"<<newAssembly->GetID()<<"---"<<std::endl;
         //std::cout<<"   ---"<<newAssembly->GetNodeName()<<"---"<<std::endl;
         //std::cout<<"   --- ("<<newAssembly->GetNumberOfChildren()<<") child nodes---"<<std::endl;

         _activeModel->CreateAssembly(newAssembly->GetID());
         _activeModel->GetAssembly(newAssembly->GetID())->SetName(newAssembly->GetNodeName());

         //std::cout<<"   ---Setting node properties---"<<std::endl;

         _setTransformOnNode(newAssembly);
         //std::cout<<"      ---Set transform---"<<std::endl;

         _setAttributesOnNode(newAssembly);
         //std::cout<<"      ---Set Attributes---"<<std::endl;

         parentAssembly->AddChild(_activeModel->GetAssembly(newAssembly->GetID()));
         unsigned int nChildren = newAssembly->GetNumberOfChildren();
         for(unsigned int i = 0; i < nChildren; i++)
         {
            //std::cout<<"      Adding child: "<<newAssembly->GetChild(i)->GetNodeName()<<std::endl;
            _addNodeToNode(newAssembly->GetID(), newAssembly->GetChild(i));
         }
      }
      else if(activeNode->GetNodeType() == "Part")
      {
         CADPart* newPart = dynamic_cast<CADPart*>(activeNode);
         //std::cout<<"      ---Part---"<<std::endl;
         //std::cout<<"      ---"<<newPart->GetID()<<"---"<<std::endl;
          _activeModel->CreatePart(newPart->GetCADFileName(),newPart->GetID(),parentID);

         VE_Xplorer::cfdFILE* partNode = _activeModel->GetPart(newPart->GetID());
         partNode->GetNode()->SetName(newPart->GetNodeName());

         //std::cout<<"   ---Setting node properties---"<<std::endl;
         _setTransformOnNode(newPart);
         //std::cout<<"      ---Set transform---"<<std::endl;
         _setAttributesOnNode(newPart);
         //std::cout<<"      ---Set Attributes---"<<std::endl;
      }
      else if(activeNode->GetNodeType() == "Clone")
      {
         CADClone* clone = dynamic_cast<CADClone*>(activeNode);
         _activeModel->CreateClone(clone->GetID(),clone->GetOriginalNode()->GetID(),clone->GetOriginalNode()->GetNodeType());         
         //std::cout<<"      ---Clone---"<<std::endl;
         //std::cout<<"      ---"<<clone->GetID()<<"---"<<std::endl;

         //std::cout<<"   ---Setting node properties---"<<std::endl;
         _setTransformOnNode(clone);
         //std::cout<<"      ---Set transform---"<<std::endl;
         _setAttributesOnNode(clone);
         //std::cout<<"      ---Set Attributes---"<<std::endl;
         parentAssembly->AddChild(_activeModel->GetClone(clone->GetID())->GetClonedGraph());
      }
   }
   else
   {
      std::cout<<"No parent found!"<<std::endl;
   }
   //std::cout<<"---Done---"<<std::endl;
}   
