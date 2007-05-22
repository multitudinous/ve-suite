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
 * Date modified: $Date: 2007-02-22 18:56:39 -0600 (Thu, 22 Feb 2007) $
 * Version:       $Rev: 6981 $
 * Author:        $Author: biv $
 * Id:            $Id: TBActivateEH.cxx 6981 2007-02-23 00:56:39Z biv $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#include "VE_Xplorer/XplorerHandlers/SoundActivateEH.h"
#include "VE_Xplorer/XplorerHandlers/cfdModel.h"
#include "VE_Xplorer/XplorerHandlers/cfdModelHandler.h"

#include "VE_Open/XML/Command.h"
#include "VE_Open/XML/DataValuePair.h"

using namespace VE_EVENTS;
using namespace VE_Xplorer;
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
void SoundActivateEventHandler::SetGlobalBaseObject(VE_Xplorer::cfdGlobalBase* baseObject)
{
   ///is this overkill????
   try
   {
      if(baseObject)
      {
         _activeModel = dynamic_cast<VE_Xplorer::cfdModel*>(baseObject);
      }
      else
      {
         _activeModel = VE_Xplorer::cfdModelHandler::instance()->GetActiveModel();
      }
   }
   catch(...)
   {
      _activeModel = 0;
      std::cout<<"Invalid object passed to TextureBasedEventHandler!"<<std::endl;
   }
}
/////////////////////////////////////////////////////////////////////////////////////   
void SoundActivateEventHandler::Execute(VE_XML::XMLObject* veXMLObject)
{
   try
   {
      if(_activeModel)
      { 
         VE_XML::Command* command = dynamic_cast< VE_XML::Command* >( veXMLObject );
         VE_XML::DataValuePair* soundStatus = command->GetDataValuePair( "Status" );
         unsigned int onOff = 0;
         soundStatus->GetData(onOff);

         std::string name;
         VE_XML::DataValuePair* soundName = command->GetDataValuePair( "Sound Name" );
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
