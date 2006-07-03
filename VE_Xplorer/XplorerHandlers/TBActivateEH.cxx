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
 * Date modified: $Date: 2006-06-24 17:15:54 -0500 (Sat, 24 Jun 2006) $
 * Version:       $Rev: 4730 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#include "VE_Xplorer/XplorerHandlers/TBActivateEH.h"
#include "VE_Xplorer/XplorerHandlers/cfdModel.h"
#include "VE_Xplorer/XplorerHandlers/cfdDataSet.h"
#include "VE_Xplorer/XplorerHandlers/cfdTextureBasedVizHandler.h"

#include "VE_Xplorer/SceneGraph/cfdSwitch.h"
using namespace VE_EVENTS;
using namespace VE_Xplorer;
#ifdef VE_PATENTED
////////////////////////////////////////////////////////////////////
TextureBasedActivateEventHandler::TextureBasedActivateEventHandler()
{
}
///////////////////////////////////////////////////////////////////
TextureBasedActivateEventHandler
::TextureBasedActivateEventHandler(const TextureBasedActivateEventHandler& ceh)
{
}
/////////////////////////////////////////////////////////////////////
TextureBasedActivateEventHandler::~TextureBasedActivateEventHandler()
{
}
///////////////////////////////////////////////////////////////////////////////////////
TextureBasedActivateEventHandler& 
TextureBasedActivateEventHandler::operator=(const TextureBasedActivateEventHandler& rhs)
{
   if(&rhs != this)
   {
      TextureBasedEventHandler::operator=(rhs);
   }
   return *this;
}
/////////////////////////////////////////////////////////////////////////////////////   
void TextureBasedActivateEventHandler::_operateOnNode(VE_XML::XMLObject* veXMLObject)
{
   try
   {
      if(_activeModel)
      {
         ///what happens if texture is somehow added first? Is that possible?
         _activeModel->GetActiveDataSet()->GetSwitchNode()->SetVal(1);
         _activeTDSet = _activeModel->GetTextureDataSet( 0 );
         VE_TextureBased::cfdTextureBasedVizHandler::instance()->SetActiveTextureDataSet( _activeTDSet );
      } 
   }
   catch(...)
   {
      std::cout<<"Invalid Model!!"<<std::endl;
      std::cout<<"TextureBasedActivateEventHandler::_operateOnNode()"<<std::endl;
   }

}
#endif
