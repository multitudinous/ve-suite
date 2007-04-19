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

#include "VE_Xplorer/XplorerHandlers/SoundAddNewEH.h"
#include "VE_Xplorer/XplorerHandlers/cfdModel.h"
#include "VE_Xplorer/XplorerHandlers/cfdModelHandler.h"

#include "VE_Open/XML/Command.h"
#include "VE_Open/XML/DataValuePair.h"

using namespace VE_EVENTS;
using namespace VE_Xplorer;
#ifdef VE_PATENTED
////////////////////////////////////////////////////////////////////
SoundAddNewEventHandler::SoundAddNewEventHandler()
{
   _activeModel = 0;
}
///////////////////////////////////////////////////////////////////
SoundAddNewEventHandler
::SoundAddNewEventHandler(const SoundAddNewEventHandler& ceh)
{
   _activeModel = ceh._activeModel;
}
/////////////////////////////////////////////////////////////////////
SoundAddNewEventHandler::~SoundAddNewEventHandler()
{
}
///////////////////////////////////////////////////////////////////////////////////////
SoundAddNewEventHandler& 
SoundAddNewEventHandler::operator=(const SoundAddNewEventHandler& rhs)
{
   if(&rhs != this)
   {
      _activeModel = rhs._activeModel;
   }
   return *this;
}
//////////////////////////////////////////////////////////////////////////////////////////////
void SoundAddNewEventHandler::SetGlobalBaseObject(VE_Xplorer::cfdGlobalBase* baseObject)
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
void SoundAddNewEventHandler::Execute(VE_XML::XMLObject* veXMLObject)
{
   try
   {
      if(_activeModel)
      { 
         VE_XML::Command* command = dynamic_cast< VE_XML::Command* >( veXMLObject );
         VE_XML::DataValuePair* soundName = command->GetDataValuePair( "Sound Name" );
         std::string guiName;
         soundName->GetData(guiName);

         std::string fileName;
         VE_XML::DataValuePair* soundFile = command->GetDataValuePair( "Sound Filename" );
         soundFile->GetData(fileName);
         _activeModel->AddNewSound(guiName,fileName);
      } 
   }
   catch(...)
   {
      std::cout<<"Invalid Model!!"<<std::endl;
      std::cout<<"SoundAddNewEventHandler::_operateOnNode()"<<std::endl;
   }

}
#endif
