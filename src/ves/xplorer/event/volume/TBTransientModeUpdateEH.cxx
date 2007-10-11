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

#include <VE_Xplorer/XplorerHandlers/TBTransientModeUpdateEH.h>
#include <VE_Xplorer/XplorerHandlers/cfdModel.h>
#include <VE_Xplorer/XplorerHandlers/cfdDataSet.h>
#include <VE_Xplorer/XplorerHandlers/cfdTextureBasedVizHandler.h>

#include <VE_Xplorer/TextureBased/cfdTextureDataSet.h>

#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>

using namespace VE_EVENTS;
using namespace VE_Xplorer;
////////////////////////////////////////////////////////////////////
TextureBasedTransientModeUpdateEventHandler::TextureBasedTransientModeUpdateEventHandler()
{
}
///////////////////////////////////////////////////////////////////
TextureBasedTransientModeUpdateEventHandler
::TextureBasedTransientModeUpdateEventHandler(const TextureBasedTransientModeUpdateEventHandler& ceh)
{
}
/////////////////////////////////////////////////////////////////////
TextureBasedTransientModeUpdateEventHandler::~TextureBasedTransientModeUpdateEventHandler()
{
}
///////////////////////////////////////////////////////////////////////////////////////
TextureBasedTransientModeUpdateEventHandler& 
TextureBasedTransientModeUpdateEventHandler::operator=(const TextureBasedTransientModeUpdateEventHandler& rhs)
{
   if(&rhs != this)
   {
      TextureBasedEventHandler::operator=(rhs);
   }
   return *this;
}
/////////////////////////////////////////////////////////////////////////////////////   
void TextureBasedTransientModeUpdateEventHandler::_operateOnNode(VE_XML::XMLObject* veXMLObject)
{
   try
   {
      VE_XML::Command* command = dynamic_cast< VE_XML::Command* >( veXMLObject );
      VE_XML::DataValuePairWeakPtr playMode = command->GetDataValuePair("Mode");      
      std::string mode;
      playMode->GetData(mode);

      if(mode == "Step")
      {
         VE_XML::DataValuePairWeakPtr playDirection = command->GetDataValuePair("Direction");      
         std::string direction;
         playDirection->GetData(direction);
         VE_TextureBased::cfdTextureBasedVizHandler::instance()->StepTransientVisualization(direction);
      }
      else if(mode == "Play")
      {
         VE_TextureBased::cfdTextureBasedVizHandler::instance()->PlayTransientVisualization();
      }
      else if(mode == "Stop")
      {
         VE_TextureBased::cfdTextureBasedVizHandler::instance()->StopTransientVisualization();
      }
   }
   catch(...)
   {
      std::cout<<"Invalid TextureDataSet!!"<<std::endl;
      std::cout<<"TextureBasedTransientModeUpdateEventHandler::_operateOnNode()"<<std::endl;
   }
}
