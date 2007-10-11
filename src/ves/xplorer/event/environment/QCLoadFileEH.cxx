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

#include <VE_Xplorer/XplorerHandlers/QCLoadFileEH.h>
#include <VE_Xplorer/XplorerHandlers/cfdModel.h>
#include <VE_Xplorer/XplorerHandlers/cfdQuatCamHandler.h>

#include <VE_Open/XML/Command.h>
#include <VE_Open/XML/DataValuePair.h>

using namespace VE_EVENTS;
using namespace VE_Xplorer;

////////////////////////////////////////////////////////////////////
QuatCamLoadFileEventHandler::QuatCamLoadFileEventHandler()
{
}
///////////////////////////////////////////////////////////////////
QuatCamLoadFileEventHandler
::QuatCamLoadFileEventHandler(const QuatCamLoadFileEventHandler& ceh)
{
}
/////////////////////////////////////////////////////////////////////
QuatCamLoadFileEventHandler::~QuatCamLoadFileEventHandler()
{
}
///////////////////////////////////////////////////////////////////////////////////////
QuatCamLoadFileEventHandler& 
QuatCamLoadFileEventHandler::operator=(const QuatCamLoadFileEventHandler& rhs)
{
   if(&rhs != this)
   {
      VE_EVENTS::EventHandler::operator=(rhs);
   }
   return *this;
}
///////////////////////////////////////////////////////////////
void QuatCamLoadFileEventHandler::SetGlobalBaseObject(VE_Xplorer::cfdGlobalBase* baseObject)
{
}
/////////////////////////////////////////////////////////////////////////////////////   
void QuatCamLoadFileEventHandler::Execute(VE_XML::XMLObject* veXMLObject)
{
   std::string fileName;
   try
   {
      VE_XML::Command* command = dynamic_cast< VE_XML::Command* >( veXMLObject );
      VE_XML::DataValuePairWeakPtr velFile = command->GetDataValuePair("View Locations file");      
      velFile->GetData(fileName);
      VE_Xplorer::cfdQuatCamHandler::instance()->LoadFromFile(fileName);
   }
   catch(...)
   {
      std::cout<<"Error!!"<<std::endl;
	  std::cout<<"QuatCamLoadFileEventHandler"<<std::endl;
	  std::cout<<"Couldn't load viewpoints file: "<<fileName<<std::endl;
   }
}
