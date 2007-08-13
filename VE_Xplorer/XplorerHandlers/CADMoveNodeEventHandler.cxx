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
#include "VE_Xplorer/XplorerHandlers/CADMoveNodeEventHandler.h"
#include "VE_Xplorer/XplorerHandlers/cfdModel.h"
#include "VE_Xplorer/XplorerHandlers/ModelCADHandler.h"

#include "VE_Xplorer/SceneGraph/CADEntity.h"

#include "VE_Open/XML/XMLObject.h"
#include "VE_Open/XML/Command.h"
#include "VE_Open/XML/FloatArray.h"
#include "VE_Open/XML/Transform.h"
#include "VE_Open/XML/DataValuePair.h"
#include "VE_Open/XML/CAD/CADNode.h"
#include "VE_Open/XML/CAD/CADPart.h"
#include "VE_Open/XML/CAD/CADAssembly.h"

#include <iostream>
using namespace VE_EVENTS;
using namespace VE_XML::VE_CAD;
////////////////////////////////////////////////////////////////////////////
//Constructor                                                             //
////////////////////////////////////////////////////////////////////////////
CADMoveNodeEventHandler::CADMoveNodeEventHandler()
:VE_EVENTS::CADEventHandler()
{
}
///////////////////////////////////////////////////////////////////////////////////////
CADMoveNodeEventHandler::CADMoveNodeEventHandler(const CADMoveNodeEventHandler& rhs)
:VE_EVENTS::CADEventHandler(rhs)
{
   
}
/////////////////////////////////////////////////////
///Destructor                                      //
/////////////////////////////////////////////////////
CADMoveNodeEventHandler::~CADMoveNodeEventHandler()
{
}
///Equal operator
//////////////////////////////////////////////////////////////////////////////////////////////////
CADMoveNodeEventHandler& CADMoveNodeEventHandler::operator=(const CADMoveNodeEventHandler& rhs)
{
   if(this != &rhs)
   {
      VE_EVENTS::CADEventHandler::operator=(rhs);
   }
   return *this;
}
//////////////////////////////////////////////////////////////////////////
void CADMoveNodeEventHandler::_operateOnNode(VE_XML::XMLObject* xmlObject)
{
   try
   {
      VE_XML::Command* command = dynamic_cast<VE_XML::Command*>(xmlObject);
      VE_XML::DataValuePairWeakPtr movingNodeType = 
          command->GetDataValuePair("Move Node Type");
   
      std::string nodeType;
      movingNodeType->GetData( nodeType );

      VE_XML::DataValuePairWeakPtr movingNode = 
          command->GetDataValuePair("Move Node ID");
      std::string movingNodeID;
      movingNode->GetData( movingNodeID );

      VE_XML::DataValuePairWeakPtr oldParent = 
          command->GetDataValuePair("Old Parent ID");
      std::string oldParentID;
      oldParent->GetData( oldParentID );

      VE_XML::DataValuePairWeakPtr newParent = 
          command->GetDataValuePair("New Parent ID");
      std::string newParentID;
      newParent->GetData( newParentID );
      
      osg::ref_ptr<VE_SceneGraph::DCS> oldParentCAD; 
      osg::ref_ptr<VE_SceneGraph::DCS> newParentCAD; 
      
      if(m_cadHandler->GetAssembly(oldParentID))
      {
         oldParentCAD = m_cadHandler->GetAssembly(oldParentID);
      }

      if(m_cadHandler->GetAssembly(newParentID))
      {
         newParentCAD = m_cadHandler->GetAssembly(newParentID);
      }
      if( oldParentCAD.valid() && newParentCAD.valid())
      {
          if( nodeType == "Assembly" )
          {
              if( oldParentCAD->removeChild(m_cadHandler->GetAssembly(movingNodeID) ) )
              {
                 newParentCAD->AddChild(m_cadHandler->GetAssembly(movingNodeID) );
              }
          }
          else if( nodeType == "Part" )
          {
              if( oldParentCAD->removeChild(m_cadHandler->GetPart(movingNodeID)->GetDCS() ) )
              {
                  newParentCAD->AddChild(m_cadHandler->GetPart(movingNodeID)->GetDCS() );
              }
          }
      }

   }
   catch(char* str)
   {
      std::cout<<str<<std::endl;
   }
   catch(...)
   {
   }
   
}
