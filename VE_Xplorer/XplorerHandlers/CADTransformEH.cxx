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
#include "VE_Xplorer/XplorerHandlers/CADTransformEH.h"
#include "VE_Xplorer/XplorerHandlers/cfdModel.h"
#include "VE_Xplorer/XplorerHandlers/ModelCADHandler.h"

#include "VE_Xplorer/SceneGraph/CADEntity.h"
#include "VE_Xplorer/SceneGraph/Clone.h"

#include "VE_Open/XML/XMLObject.h"
#include "VE_Open/XML/Command.h"
#include "VE_Open/XML/DataValuePair.h"

#include "VE_Open/XML/Transform.h"
#include "VE_Open/XML/FloatArray.h"
#include "VE_Open/XML/CAD/CADNode.h"
#include <iostream>
using namespace VE_EVENTS;
using namespace VE_CAD;
////////////////////////////////////////////////////////////////////////////
//Constructor                                                             //
////////////////////////////////////////////////////////////////////////////
CADTransformEventHandler::CADTransformEventHandler()
:VE_EVENTS::CADEventHandler()
{
}
///////////////////////////////////////////////////////////////////////////////////////
CADTransformEventHandler::CADTransformEventHandler(const CADTransformEventHandler& rhs)
:VE_EVENTS::CADEventHandler(rhs)
{
   
}
/////////////////////////////////////////////////////
///Destructor                                      //
/////////////////////////////////////////////////////
CADTransformEventHandler::~CADTransformEventHandler()
{
}
///Equal operator
//////////////////////////////////////////////////////////////////////////////////////////////////
CADTransformEventHandler& CADTransformEventHandler::operator=(const CADTransformEventHandler& rhs)
{
   if(this != &rhs)
   {
      VE_EVENTS::CADEventHandler::operator=(rhs);
   }
   return *this;
}
///////////////////////////////////////////////////////////////////////
void CADTransformEventHandler::_operateOnNode(VE_XML::XMLObject* xmlObject)
{
   try{

      VE_XML::Command* command = dynamic_cast<VE_XML::Command*>(xmlObject);

      VE_XML::DataValuePair* nodeID = command->GetDataValuePair("Node ID");
      VE_XML::DataValuePair* nodeType = command->GetDataValuePair("Node Type");
      VE_XML::DataValuePair* newTransform = command->GetDataValuePair("Transform");
      
		VE_SceneGraph::DCS* transform = 0;
      if(nodeType->GetDataString() == std::string("Part"))
      {
         if(m_cadHandler->PartExists(nodeID->GetDataString()))
         {
            transform = m_cadHandler->GetPart(nodeID->GetDataString())->GetDCS();
         }
      }
      else if(nodeType->GetDataString() == std::string("Assembly"))
      {
         if(m_cadHandler->AssemblyExists(nodeID->GetDataString()))
         {
            transform = m_cadHandler->GetAssembly(nodeID->GetDataString());
         }
      }
      else if(nodeType->GetDataString() == std::string("Clone"))
      {
         if(m_cadHandler->CloneExists(nodeID->GetDataString()))
         {
            transform = m_cadHandler->GetClone(nodeID->GetDataString())->GetClonedGraph();
         }
      }
      if( transform )
      {
         VE_XML::Transform* rawTransform = dynamic_cast<VE_XML::Transform*>(newTransform->GetDataXMLObject());
         transform->SetTranslationArray( rawTransform->GetTranslationArray()->GetArray() );
         transform->SetRotationArray( rawTransform->GetRotationArray()->GetArray() );
         transform->SetScaleArray( rawTransform->GetScaleArray()->GetArray() );
      }
   }
   catch(...)
   {
      std::cout<<"Error!!!Invalid command passed to CADTransformEH!!"<<std::endl;
   }
}
