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

#include <ves/xplorer/event/environment/StoredSceneEH.h>
#include <ves/xplorer/Model.h>
#include <ves/xplorer/cfdEnvironmentHandler.h>
#include <ves/xplorer/environment/cfdTeacher.h>

#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>

using namespace ves::xplorer::event;
using namespace ves::xplorer;
using namespace ves::open::xml;

////////////////////////////////////////////////////////////////////
StoredSceneEventHandler::StoredSceneEventHandler()
{
}
///////////////////////////////////////////////////////////////////
StoredSceneEventHandler
::StoredSceneEventHandler(const StoredSceneEventHandler& ceh)
{
}
/////////////////////////////////////////////////////////////////////
StoredSceneEventHandler::~StoredSceneEventHandler()
{
}
///////////////////////////////////////////////////////////////////////////////////////
StoredSceneEventHandler& 
StoredSceneEventHandler::operator=(const StoredSceneEventHandler& rhs)
{
   if(&rhs != this)
   {
      ves::xplorer::event::EventHandler::operator=(rhs);
   }
   return *this;
}
///////////////////////////////////////////////////////////////
void StoredSceneEventHandler::SetGlobalBaseObject(ves::xplorer::GlobalBase* baseObject)
{
}
//////////////////////////////////////////////////////////////////////////   
void StoredSceneEventHandler::Execute(XMLObject* veXMLObject)
{
   try
   {
      Command* command = dynamic_cast< Command* >( veXMLObject );
      unsigned int whichChild =0;

      if(command->GetDataValuePair("LOAD_PFB_FILE"))
      {
         command->GetDataValuePair("LOAD_PFB_FILE")->GetData(whichChild);
	      ves::xplorer::cfdEnvironmentHandler::instance()->GetTeacher()->LoadScene(whichChild);
         
      }
      else if(command->GetDataValuePair("CLEAR_PFB_FILE"))
      {
	      ves::xplorer::cfdEnvironmentHandler::instance()->GetTeacher()->ClearStoredScenes();
      }
      else if(command->GetDataValuePair("RECORD_SCENE"))
      {
	      ves::xplorer::cfdEnvironmentHandler::instance()->GetTeacher()->RecordScene();
      }
   }
   catch(...)
   {
      std::cout<<"Error!!"<<std::endl;
      std::cout<<"StoredSceneEventHandler::_operateOnNode()"<<std::endl;
   }
}
