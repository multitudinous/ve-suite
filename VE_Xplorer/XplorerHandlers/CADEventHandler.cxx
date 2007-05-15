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
#include "VE_Xplorer/XplorerHandlers/CADEventHandler.h"
#include "VE_Xplorer/XplorerHandlers/cfdModel.h"
#include "VE_Xplorer/XplorerHandlers/cfdGlobalBase.h"
#include "VE_Xplorer/XplorerHandlers/cfdModel.h"
#include "VE_Xplorer/XplorerHandlers/cfdModelHandler.h"

#include "VE_Xplorer/SceneGraph/CADEntity.h"
#include "VE_Xplorer/SceneGraph/CADEntityHelper.h"
#include "VE_Xplorer/SceneGraph/Clone.h"
#include "VE_Xplorer/SceneGraph/UpdateIDOnChildrenVisitor.h"

#include "VE_Xplorer/SceneGraph/Utilities/MaterialInitializer.h"

#include "VE_Open/XML/CAD/CADNode.h"
#include "VE_Open/XML/CAD/CADAssembly.h"
#include "VE_Open/XML/CAD/CADClone.h"
#include "VE_Open/XML/CAD/CADAttribute.h"
#include "VE_Open/XML/CAD/CADPart.h"
#include "VE_Open/XML/XMLObject.h"
#include "VE_Open/XML/Transform.h"
#include "VE_Open/XML/FloatArray.h"

#include "VE_Xplorer/XplorerHandlers/cfdDebug.h"

#include <boost/filesystem/path.hpp>
#include <iostream>

#ifdef _OSG
#include <osg/Node>
#endif
using namespace VE_EVENTS;
using namespace VE_XML::VE_CAD;
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
      if(model)
      {
         _activeModel = dynamic_cast<VE_Xplorer::cfdModel*>(model);
      }
      else
      {
         _activeModel = VE_Xplorer::cfdModelHandler::instance()->GetActiveModel();
      }
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
   VE_SceneGraph::DCS* transform = 0;
   std::string nodeID = activeNode->GetID();
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
      if ( _activeModel->GetClone(nodeID) )
      {
         transform = _activeModel->GetClone(nodeID)->GetClonedGraph();
      }
   }
   if( transform )
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
///////////////////////////////////////////////////////////////////////
void CADEventHandler::SetNodeDescriptors(std::string nodeID,
                                         std::string nodeType,
                                         std::string descriptorName,
                                         std::string descriptorValue)
{
   //set the uuid on the osg node so that we can get back to vexml
   osg::Node::DescriptionList descriptorsList;
   descriptorsList.push_back( descriptorName );
   descriptorsList.push_back( descriptorValue );
   
   if(nodeType == "Assembly")
   {
      VE_SceneGraph::DCS* assemblyNode = _activeModel->GetAssembly(nodeID);
      assemblyNode->setDescriptions( descriptorsList );
   }
   else if(nodeType == "Part")
   {
      VE_SceneGraph::CADEntity* partNode = _activeModel->GetPart(nodeID);
      partNode->GetDCS()->setDescriptions( descriptorsList );
   }
   else if(nodeType == "Clone")
   {
      VE_SceneGraph::Clone* cloneNode = _activeModel->GetClone(nodeID);
      VE_SceneGraph::UpdateIDOnChildrenVisitor idUpdate( cloneNode->GetClonedGraph(), descriptorValue );
      cloneNode->GetClonedGraph()->setDescriptions(descriptorsList);
   }
}
/////////////////////////////////////////////////////////////////////////
void CADEventHandler::_addNodeToNode(std::string parentID, CADNode* activeNode)
{
   VE_SceneGraph::DCS* parentAssembly = 0;
   parentAssembly = _activeModel->GetAssembly(parentID);

   vprDEBUG( vesDBG, 1 ) << "|---Adding node to parent---" << parentID 
                           << std::endl << vprDEBUG_FLUSH;
   if( parentAssembly )
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
			_activeModel->GetAssembly(newAssembly->GetID())->ToggleDisplay(newAssembly->GetVisibility());
         SetNodeDescriptors(newAssembly->GetID(),"Assembly","VE_XML_ID",newAssembly->GetID());
         //Set a default material on nodes that have no initial material
         VE_SceneGraph::Utilities::MaterialInitializer material_initializer( _activeModel->GetAssembly( newAssembly->GetID() ) );
      }
      else if(activeNode->GetNodeType() == "Part")
      {
         CADPart* newPart = dynamic_cast<CADPart*>(activeNode);
         vprDEBUG( vesDBG, 1 ) <<"|\t---Part---"
                                 << std::endl << vprDEBUG_FLUSH;
         vprDEBUG( vesDBG, 1 ) <<"|\t---"<<newPart->GetID()
                                 <<"---"<< std::endl << vprDEBUG_FLUSH;
         std::string tempFilename = newPart->GetCADFileName();
         boost::filesystem::path correctedPath( newPart->GetCADFileName(), boost::filesystem::no_check );
         vprDEBUG( vesDBG, 1 ) <<"|\t---" << tempFilename << "---" 
                                 << correctedPath.native_file_string() 
                                 << std::endl << vprDEBUG_FLUSH;
          _activeModel->CreatePart( correctedPath.native_file_string(),
                                    newPart->GetID(),
                                    parentID
                                  );

         VE_SceneGraph::CADEntity* partNode = _activeModel->GetPart(newPart->GetID());
         if ( partNode->GetNode()->GetNode() )
         {
            partNode->GetNode()->SetName(newPart->GetNodeName());
            partNode->GetDCS()->setName(newPart->GetNodeName());
            
            //set the visibility
            partNode->GetDCS()->ToggleDisplay(newPart->GetVisibility());

            vprDEBUG( vesDBG, 1 ) <<"|\t---Setting node properties---"<< std::endl << vprDEBUG_FLUSH;
            _setTransformOnNode(newPart);
            vprDEBUG( vesDBG, 1 ) <<"|\t---Set transform---"<< std::endl << vprDEBUG_FLUSH;
            _setAttributesOnNode(newPart);
            vprDEBUG( vesDBG, 1 ) <<"|\t---Set Attributes---"<< std::endl << vprDEBUG_FLUSH;

            //set the uuid on the osg node so that we can get back to vexml
            SetNodeDescriptors(newPart->GetID(),"Part","VE_XML_ID",newPart->GetID());
            //Set a default material on nodes that have no initial material
            VE_SceneGraph::Utilities::MaterialInitializer material_initializer( partNode->GetDCS() );
         }
         else
         {
            std::cerr << "|\t---ERROR: (CADEventHandler::_addNodeToNode) Unable to load file name: " 
                        << correctedPath.native_file_string() << std::endl;
         }
      }
   }
   else
   {
      std::cout<<"|---No parent found---id "<< parentID << std::endl;
   }
}
