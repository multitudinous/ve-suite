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

#include <ves/xplorer/event/environment/SoundActivateEH.h>
#include <ves/xplorer/cfdModel.h>
#include <ves/xplorer/ModelHandler.h>

#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>

using namespace ves::xplorer::event;
using namespace ves::xplorer;
using namespace ves::open::xml;

////////////////////////////////////////////////////////////////////
SoundActivateEventHandler::SoundActivateEventHandler()
{
   _activeModel = 0;
}
///////////////////////////////////////////////////////////////////
SoundActivateEventHandler
::SoundActivateEventHandler(const SoundActivateEventHandler& ceh)
{
   _activeModel = ceh._activeModel;
}
/////////////////////////////////////////////////////////////////////
SoundActivateEventHandler::~SoundActivateEventHandler()
{
}
///////////////////////////////////////////////////////////////////////////////////////
SoundActivateEventHandler& 
SoundActivateEventHandler::operator=(const SoundActivateEventHandler& rhs)
{
   if(&rhs != this)
   {
      _activeModel = rhs._activeModel;
   }
   return *this;
}
//////////////////////////////////////////////////////////////////////////////////////////////
void SoundActivateEventHandler::SetGlobalBaseObject(ves::xplorer::GlobalBase* baseObject)
{
   ///is this overkill????
   try
   {
      if(baseObject)
      {
         _activeModel = dynamic_cast<ves::xplorer::cfdModel*>(baseObject);
      }
      else
      {
         _activeModel = ves::xplorer::cfdModelHandler::instance()->GetActiveModel();
      }
   }
   catch(...)
   {
      _activeModel = 0;
      std::cout<<"Invalid object passed to TextureBasedEventHandler!"<<std::endl;
   }
}
/////////////////////////////////////////////////////////////////////////////////////   
void SoundActivateEventHandler::Execute(XMLObject* veXMLObject)
{
   try
   {
      if(_activeModel)
      { 
         Command* command = dynamic_cast< Command* >( veXMLObject );
         DataValuePairWeakPtr soundStatus = command->GetDataValuePair( "Status" );
         unsigned int onOff = 0;
         soundStatus->GetData(onOff);

         std::string name;
         DataValuePairWeakPtr soundName = command->GetDataValuePair( "Sound Name" );
         soundName->GetData(name);
         (onOff==1)?_activeModel->ActivateSound(name):_activeModel->DeactivateSound(name);
      } 
   }
   catch(...)
   {
      std::cout<<"Invalid Model!!"<<std::endl;
      std::cout<<"SoundActivateEventHandler::_operateOnNode()"<<std::endl;
   }

}
