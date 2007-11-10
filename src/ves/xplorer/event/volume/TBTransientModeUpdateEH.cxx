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

#include <ves/xplorer/event/volume/TBTransientModeUpdateEH.h>
#include <ves/xplorer/cfdModel.h>
#include <ves/xplorer/cfdDataSet.h>
#include <ves/xplorer/cfdTextureBasedVizHandler.h>

#include <ves/xplorer/volume/cfdTextureDataSet.h>

#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>

using namespace ves::xplorer::event;
using namespace ves::xplorer;
using namespace ves::open::xml;

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
void TextureBasedTransientModeUpdateEventHandler::_operateOnNode(XMLObject* veXMLObject)
{
   try
   {
      Command* command = dynamic_cast< Command* >( veXMLObject );
      DataValuePairWeakPtr playMode = command->GetDataValuePair("Mode");      
      std::string mode;
      playMode->GetData(mode);

      if(mode == "Step")
      {
         DataValuePairWeakPtr playDirection = command->GetDataValuePair("Direction");      
         std::string direction;
         playDirection->GetData(direction);
         ves::xplorer::volume::cfdTextureBasedVizHandler::instance()->StepTransientVisualization(direction);
      }
      else if(mode == "Play")
      {
         ves::xplorer::volume::cfdTextureBasedVizHandler::instance()->PlayTransientVisualization();
      }
      else if(mode == "Stop")
      {
         ves::xplorer::volume::cfdTextureBasedVizHandler::instance()->StopTransientVisualization();
      }
   }
   catch(...)
   {
      std::cout<<"Invalid TextureDataSet!!"<<std::endl;
      std::cout<<"TextureBasedTransientModeUpdateEventHandler::_operateOnNode()"<<std::endl;
   }
}
