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
////////////////////////////////////////////////////////
void CADEventHandler::_setAttributesOnNode(CADNode* node)
{
   //set attributes
   std::vector<CADAttribute>::iterator currentAttribute;
   for(currentAttribute = node->GetAttributeList().begin();
       currentAttribute != node->GetAttributeList().end();
       currentAttribute++)
   {
      _activeModel->AddAttributeToNode(node->GetID(),&(*currentAttribute));
   }
}
///////////////////////////////////////////////////////
void CADEventHandler::_setTransformOnNode(CADNode* node)
{
   //set the transform
   cfdDCS* transform = 0;
   unsigned int nodeID = node->GetID();
   if(node->GetNodeType() == "Assembly")
   {
      transform = _activeModel->GetAssembly(nodeID);
   }
   else if(node->GetNodeType() == "Part")
   {
      transform = _activeModel->GetPart(nodeID)->GetDCS();
   }
   else if(node->GetNodeType() == "Clone")
   {
      transform = _activeModel->GetClone(nodeID)->GetClonedGraph();
   }
   if(transform)
   {
      transform->SetTranslationArray(node->GetTransform()->GetTranslationArray()->GetArray() );
      transform->SetRotationArray(node->GetTransform()->GetRotationArray()->GetArray() );
      transform->SetScaleArray(node->GetTransform()->GetScaleArray()->GetArray() );
   }
}
/////////////////////////////////////////////////////////////////////////
void CADEventHandler::_addNodeToNode(unsigned int parentID, CADNode* node)
{
   unsigned int nodeID = node->GetID();
   cfdDCS* parentAssembly = 0;
   parentAssembly = _activeModel->GetAssembly(parentID);

   if(parentAssembly)
   {
      if(node->GetNodeType() == "Assembly")
      {
         _activeModel->CreateAssembly(nodeID);
         _activeModel->GetAssembly(nodeID)->SetName(node->GetNodeName());

         parentAssembly->AddChild(_activeModel->GetAssembly(nodeID));
         unsigned int nChildren = dynamic_cast<CADAssembly*>(node)->GetNumberOfChildren();
         for(unsigned int i = 0; i < nChildren; i++)
         {
            _addNodeToNode(nodeID,dynamic_cast<CADAssembly*>(node)->GetChild(i));
         }
      }
      else if(node->GetNodeType() == "Part")
      {
          _activeModel->CreatePart(dynamic_cast<CADPart*>(node)->GetCADFileName(),nodeID,parentID);

          VE_Xplorer::cfdFILE* partNode = _activeModel->GetPart(nodeID);
          partNode->GetNode()->SetName(node->GetNodeName());
      }
      else if(node->GetNodeType() == "Clone")
      {
         CADClone* clone = dynamic_cast<CADClone*>(node);
         _activeModel->CreateClone(nodeID,clone->GetOriginalNode()->GetID(),clone->GetOriginalNode()->GetNodeType());         
      }
      _setAttributesOnNode(node);
      _setTransformOnNode(node);
   }
}   
