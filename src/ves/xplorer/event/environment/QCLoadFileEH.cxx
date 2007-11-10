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

#include <ves/xplorer/event/environment/QCLoadFileEH.h>
#include <ves/xplorer/cfdModel.h>
#include <ves/xplorer/environment/cfdQuatCamHandler.h>

#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>

using namespace ves::xplorer::event;
using namespace ves::xplorer;
using namespace ves::open::xml;

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
      ves::xplorer::event::EventHandler::operator=(rhs);
   }
   return *this;
}
///////////////////////////////////////////////////////////////
void QuatCamLoadFileEventHandler::SetGlobalBaseObject(ves::xplorer::cfdGlobalBase* baseObject)
{
}
/////////////////////////////////////////////////////////////////////////////////////   
void QuatCamLoadFileEventHandler::Execute(XMLObject* veXMLObject)
{
   std::string fileName;
   try
   {
      Command* command = dynamic_cast< Command* >( veXMLObject );
      DataValuePairWeakPtr velFile = command->GetDataValuePair("View Locations file");      
      velFile->GetData(fileName);
      ves::xplorer::cfdQuatCamHandler::instance()->LoadFromFile(fileName);
   }
   catch(...)
   {
      std::cout<<"Error!!"<<std::endl;
	  std::cout<<"QuatCamLoadFileEventHandler"<<std::endl;
	  std::cout<<"Couldn't load viewpoints file: "<<fileName<<std::endl;
   }
}
