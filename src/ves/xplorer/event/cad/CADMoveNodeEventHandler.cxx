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
#include <ves/xplorer/event/cad/CADMoveNodeEventHandler.h>
#include <ves/xplorer/cfdModel.h>
#include <ves/xplorer/ModelCADHandler.h>

#include <ves/xplorer/scenegraph/CADEntity.h>

#include <ves/open/xml/XMLObject.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/FloatArray.h>
#include <ves/open/xml/Transform.h>
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/cad/CADNode.h>
#include <ves/open/xml/cad/CADPart.h>
#include <ves/open/xml/cad/CADAssembly.h>

#include <iostream>
using namespace VE_EVENTS;
using namespace ves::open::xml::cad;
using namespace ves::open::xml;
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
void CADMoveNodeEventHandler::_operateOnNode(XMLObject* xmlObject)
{
   try
   {
      Command* command = dynamic_cast<Command*>(xmlObject);
      DataValuePairWeakPtr movingNodeType = 
          command->GetDataValuePair("Move Node Type");
   
      std::string nodeType;
      movingNodeType->GetData( nodeType );

      DataValuePairWeakPtr movingNode = 
          command->GetDataValuePair("Move Node ID");
      std::string movingNodeID;
      movingNode->GetData( movingNodeID );

      DataValuePairWeakPtr oldParent = 
          command->GetDataValuePair("Old Parent ID");
      std::string oldParentID;
      oldParent->GetData( oldParentID );

      DataValuePairWeakPtr newParent = 
          command->GetDataValuePair("New Parent ID");
      std::string newParentID;
      newParent->GetData( newParentID );
      
      osg::ref_ptr<ves::xplorer::scenegraph::DCS> oldParentCAD; 
      osg::ref_ptr<ves::xplorer::scenegraph::DCS> newParentCAD; 
      
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
