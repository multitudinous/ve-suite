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
#include <ves/xplorer/event/CADSetNameEH.h>
#include <ves/xplorer/event/viz/cfdModel.h>

#include <ves/xplorer/scenegraph/CADEntity.h>
#include <ves/xplorer/scenegraph/CADEntityHelper.h>
#include <ves/xplorer/scenegraph/Clone.h>
#include <ves/xplorer/event/ModelCADHandler.h>

#include <ves/open/xml/XMLObject.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/CAD/CADNode.h>
#include <iostream>
using namespace VE_EVENTS;
using namespace VE_CAD;
////////////////////////////////////////////////////////////////////////////
//Constructor                                                             //
////////////////////////////////////////////////////////////////////////////
CADSetNameEventHandler::CADSetNameEventHandler()
:VE_EVENTS::CADEventHandler()
{
}
///////////////////////////////////////////////////////////////////////////////////////
CADSetNameEventHandler::CADSetNameEventHandler(const CADSetNameEventHandler& rhs)
:VE_EVENTS::CADEventHandler(rhs)
{
   
}
/////////////////////////////////////////////////////
///Destructor                                      //
/////////////////////////////////////////////////////
CADSetNameEventHandler::~CADSetNameEventHandler()
{
}
///Equal operator
//////////////////////////////////////////////////////////////////////////////////////////////////
CADSetNameEventHandler& CADSetNameEventHandler::operator=(const CADSetNameEventHandler& rhs)
{
   if(this != &rhs)
   {
      VE_EVENTS::CADEventHandler::operator=(rhs);
   }
   return *this;
}
//////////////////////////////////////////////////////////////////////////
void CADSetNameEventHandler::_operateOnNode(VE_XML::XMLObject* xmlObject)
{
   try
   {
      VE_XML::Command* command = dynamic_cast<VE_XML::Command*>(xmlObject);
      VE_XML::DataValuePairWeakPtr newName = command->GetDataValuePair("Node Name");
      VE_XML::DataValuePairWeakPtr nodeID = command->GetDataValuePair("Node ID");
      VE_XML::DataValuePairWeakPtr nodeType = command->GetDataValuePair("Node Type");

      std::string errorString;
          
      //This assumes the part/assembly is there already
      if(nodeType->GetDataString() == std::string("Assembly"))
      {
         if(m_cadHandler->AssemblyExists(nodeID->GetDataString()))
         {
            std::cout<<"Setting name: "<<newName->GetDataString()<<std::endl;
            m_cadHandler->GetAssembly(nodeID->GetDataString())->SetName(newName->GetDataString());
         }
         else
         {
            errorString = std::string("Assembly: ") + newName->GetDataString()+ std::string(" not added to the graph yet!");;
            throw(errorString);
         }
      }
      else if(nodeType->GetDataString() == std::string("Part"))
      {
         if(m_cadHandler->PartExists(nodeID->GetDataString()))
         {
            m_cadHandler->GetPart(nodeID->GetDataString())->GetNode()->SetName(newName->GetDataString());
         }
         else
         {
            errorString = std::string("Part: ") + newName->GetDataString()+ std::string(" not added to the graph yet!");;
            throw(errorString);
         }
      }
      else if(nodeType->GetDataString() == std::string("Clone"))
      {
         if(m_cadHandler->CloneExists(nodeID->GetDataString()))
         {
            m_cadHandler->GetClone(nodeID->GetDataString())->GetClonedGraph()->SetName(newName->GetDataString());
         }
         else
         {
            errorString = std::string("Clone: ") + newName->GetDataString()+ std::string(" not added to the graph yet!");;
            throw(errorString);
         }
      }
   }
   catch(std::string str)
   {
      std::cout<<str<<std::endl;
   }
   catch(...)
   {
      std::cout<<"Error!!"<<std::endl;
      std::cout<<"---Invalid node specified to rename!---"<<std::endl;
   }
}
