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
 * Date modified: $Date: 2007-03-18 11:01:44 -0500 (Sun, 18 Mar 2007) $
 * Version:       $Rev: 7159 $
 * Author:        $Author: mccdo $
 * Id:            $Id: TBSliceNumberUpdateEH.cxx 7159 2007-03-18 16:01:44Z mccdo $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#include "VE_Xplorer/XplorerHandlers/TBSliceNumberUpdateEH.h"
#include "VE_Xplorer/XplorerHandlers/cfdModel.h"
#include "VE_Xplorer/XplorerHandlers/cfdDataSet.h"
#include "VE_Xplorer/XplorerHandlers/cfdTextureBasedVizHandler.h"

#include "VE_Xplorer/TextureBased/cfdTextureDataSet.h"

#include "VE_Open/XML/Command.h"
#include "VE_Open/XML/DataValuePair.h"

using namespace VE_EVENTS;
using namespace VE_Xplorer;
#ifdef VE_PATENTED
////////////////////////////////////////////////////////////////////
TextureBasedSliceNumberUpdateEventHandler::TextureBasedSliceNumberUpdateEventHandler()
{
}
///////////////////////////////////////////////////////////////////
TextureBasedSliceNumberUpdateEventHandler
::TextureBasedSliceNumberUpdateEventHandler(const TextureBasedSliceNumberUpdateEventHandler& ceh)
{
}
/////////////////////////////////////////////////////////////////////
TextureBasedSliceNumberUpdateEventHandler::~TextureBasedSliceNumberUpdateEventHandler()
{
}
///////////////////////////////////////////////////////////////////////////////////////
TextureBasedSliceNumberUpdateEventHandler& 
TextureBasedSliceNumberUpdateEventHandler::operator=(const TextureBasedSliceNumberUpdateEventHandler& rhs)
{
   if(&rhs != this)
   {
      TextureBasedEventHandler::operator=(rhs);
   }
   return *this;
}
/////////////////////////////////////////////////////////////////////////////////////   
void TextureBasedSliceNumberUpdateEventHandler::_operateOnNode(VE_XML::XMLObject* veXMLObject)
{
   try
   {
      VE_XML::Command* command = dynamic_cast< VE_XML::Command* >( veXMLObject );
      VE_XML::DataValuePair* isovalue = command->GetDataValuePair("Number of Slice Planes");      
      unsigned int value;
      isovalue->GetData(value);
      VE_TextureBased::cfdTextureBasedVizHandler::instance()->UpdateNumberOfSlicePlanes(value);
   }
   catch(...)
   {
      std::cout<<"Invalid TextureDataSet!!"<<std::endl;
      std::cout<<"TextureBasedSliceNumberUpdateEventHandler::_operateOnNode()"<<std::endl;
   }
}
#endif
