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

#include <VE_Xplorer/XplorerHandlers/TBPreIntegrateEH.h>
#include <VE_Xplorer/XplorerHandlers/cfdModel.h>
#include <VE_Xplorer/XplorerHandlers/cfdDataSet.h>
#include <VE_Xplorer/XplorerHandlers/cfdTextureBasedVizHandler.h>

#include <VE_Xplorer/TextureBased/cfdTextureDataSet.h>

#include <VE_Open/XML/Command.h>
#include <VE_Open/XML/DataValuePair.h>

using namespace VE_EVENTS;
using namespace VE_Xplorer;
////////////////////////////////////////////////////////////////////
TextureBasedPreIntegrateEnableEventHandler::TextureBasedPreIntegrateEnableEventHandler()
{
}
///////////////////////////////////////////////////////////////////
TextureBasedPreIntegrateEnableEventHandler
::TextureBasedPreIntegrateEnableEventHandler(const TextureBasedPreIntegrateEnableEventHandler& ceh)
{
}
/////////////////////////////////////////////////////////////////////
TextureBasedPreIntegrateEnableEventHandler::~TextureBasedPreIntegrateEnableEventHandler()
{
}
///////////////////////////////////////////////////////////////////////////////////////
TextureBasedPreIntegrateEnableEventHandler& 
TextureBasedPreIntegrateEnableEventHandler::operator=(const TextureBasedPreIntegrateEnableEventHandler& rhs)
{
   if(&rhs != this)
   {
      TextureBasedEventHandler::operator=(rhs);
   }
   return *this;
}
/////////////////////////////////////////////////////////////////////////////////////   
void TextureBasedPreIntegrateEnableEventHandler::_operateOnNode(VE_XML::XMLObject* veXMLObject)
{
   try
   {
      VE_XML::Command* command = dynamic_cast< VE_XML::Command* >( veXMLObject );
      VE_XML::DataValuePairWeakPtr enable = command->GetDataValuePair("Recalculate Pre-Integration");      
      unsigned int onOff;
      enable->GetData(onOff);
      VE_TextureBased::cfdTextureBasedVizHandler::instance()->UpdatePreIntegrationTable((onOff==1)?true:false);
   }
   catch(...)
   {
      std::cout<<"Invalid TextureDataSet!!"<<std::endl;
      std::cout<<"TextureBasedPreIntegrateEnableEventHandler::_operateOnNode()"<<std::endl;
   }
}
