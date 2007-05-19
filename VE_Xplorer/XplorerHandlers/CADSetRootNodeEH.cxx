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
 * Date modified: $Date: 2007-03-27 17:53:45 -0500 (Tue, 27 Mar 2007) $
 * Version:       $Rev: 7229 $
 * Author:        $Author: biv $
 * Id:            $Id: CADSetRootNodeEH.cxx 7229 2007-03-27 22:53:45Z biv $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "VE_Xplorer/XplorerHandlers/CADSetRootNodeEH.h"
#include "VE_Xplorer/XplorerHandlers/cfdModel.h"
#include "VE_Xplorer/XplorerHandlers/ModelCADHandler.h"

#include "VE_Open/XML/XMLObject.h"
#include "VE_Open/XML/Command.h"
#include "VE_Open/XML/DataValuePair.h"
#include <iostream>
using namespace VE_EVENTS;
////////////////////////////////////////////////////////////////////////////
//Constructor                                                             //
////////////////////////////////////////////////////////////////////////////
CADSetRootNodeEventHandler::CADSetRootNodeEventHandler()
:VE_EVENTS::CADEventHandler()
{
}
///////////////////////////////////////////////////////////////////////////////////////
CADSetRootNodeEventHandler::CADSetRootNodeEventHandler(const CADSetRootNodeEventHandler& rhs)
:VE_EVENTS::CADEventHandler(rhs)
{
   
}
/////////////////////////////////////////////////////
///Destructor                                      //
/////////////////////////////////////////////////////
CADSetRootNodeEventHandler::~CADSetRootNodeEventHandler()
{
}
///Equal operator
//////////////////////////////////////////////////////////////////////////////////////////////////
CADSetRootNodeEventHandler& CADSetRootNodeEventHandler::operator=(const CADSetRootNodeEventHandler& rhs)
{
   if(this != &rhs)
   {
      VE_EVENTS::CADEventHandler::operator=(rhs);
   }
   return *this;
}
//////////////////////////////////////////////////////////////////////////
void CADSetRootNodeEventHandler::_operateOnNode(VE_XML::XMLObject* xmlObject)
{
   try
   {
      VE_XML::Command* command = dynamic_cast<VE_XML::Command*>(xmlObject);
      VE_XML::DataValuePair* newRootNode = command->GetDataValuePair("Root Node ID");

      std::string rootNodeID;
      newRootNode->GetData(rootNodeID);       
      m_cadHandler->SetRootCADNodeID(rootNodeID);
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
