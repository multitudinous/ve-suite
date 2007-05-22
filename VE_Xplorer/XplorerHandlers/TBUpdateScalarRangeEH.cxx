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
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#include "VE_Xplorer/XplorerHandlers/TBUpdateScalarRangeEH.h"
#include "VE_Xplorer/XplorerHandlers/cfdModel.h"
#include "VE_Xplorer/XplorerHandlers/cfdDataSet.h"
#include "VE_Xplorer/XplorerHandlers/cfdTextureBasedVizHandler.h"

#include "VE_Xplorer/TextureBased/cfdTextureDataSet.h"

#include "VE_Open/XML/Command.h"
#include "VE_Open/XML/DataValuePair.h"

using namespace VE_EVENTS;
using namespace VE_Xplorer;
////////////////////////////////////////////////////////////////////
TextureBasedUpdateScalarRangeEventHandler::TextureBasedUpdateScalarRangeEventHandler()
{
}
///////////////////////////////////////////////////////////////////
TextureBasedUpdateScalarRangeEventHandler
::TextureBasedUpdateScalarRangeEventHandler(const TextureBasedUpdateScalarRangeEventHandler& ceh)
{
}
/////////////////////////////////////////////////////////////////////
TextureBasedUpdateScalarRangeEventHandler::~TextureBasedUpdateScalarRangeEventHandler()
{
}
///////////////////////////////////////////////////////////////////////////////////////
TextureBasedUpdateScalarRangeEventHandler& 
TextureBasedUpdateScalarRangeEventHandler::operator=(const TextureBasedUpdateScalarRangeEventHandler& rhs)
{
   if(&rhs != this)
   {
      TextureBasedEventHandler::operator=(rhs);
   }
   return *this;
}
/////////////////////////////////////////////////////////////////////////////////////   
void TextureBasedUpdateScalarRangeEventHandler::_operateOnNode(VE_XML::XMLObject* veXMLObject)
{
   try
   {
      VE_XML::Command* command = dynamic_cast< VE_XML::Command* >( veXMLObject );
      VE_TextureBased::cfdTextureBasedVizHandler::instance()->UpdateActiveTextureManager();
      
      double scalarRange[2] = {0.f,100.f};

      VE_XML::DataValuePair* minScalarRange = command->GetDataValuePair( "Mininum Scalar Range" );
      minScalarRange->GetData( scalarRange[0] );
            
      VE_XML::DataValuePair* maxScalarRange = command->GetDataValuePair( "Maximum Scalar Range" );
      maxScalarRange->GetData( scalarRange[1] );
      
      //this is overkill
      float floatRange[2];
      floatRange[0] = scalarRange[0];
      floatRange[1] = scalarRange[1];
      VE_TextureBased::cfdTextureBasedVizHandler::instance()->UpdateScalarRange(floatRange);
      
   }
   catch(...)
   {
      std::cout<<"Invalid TextureDataSet!!"<<std::endl;
      std::cout<<"TextureBasedUpdateScalarRangeEventHandler::_operateOnNode()"<<std::endl;
   }
}
