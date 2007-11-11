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

#include <ves/xplorer/event/volume/TBIsosurfaceUpdateEH.h>
#include <ves/xplorer/Model.h>
#include <ves/xplorer/DataSet.h>
#include <ves/xplorer/TextureBasedVizHandler.h>

#include <ves/xplorer/volume/cfdTextureDataSet.h>

#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>

using namespace ves::xplorer::event;
using namespace ves::xplorer;
using namespace ves::open::xml;

////////////////////////////////////////////////////////////////////
TextureBasedIsosurfaceUpdateEventHandler::TextureBasedIsosurfaceUpdateEventHandler()
{
}
///////////////////////////////////////////////////////////////////
TextureBasedIsosurfaceUpdateEventHandler
::TextureBasedIsosurfaceUpdateEventHandler(const TextureBasedIsosurfaceUpdateEventHandler& ceh)
{
}
/////////////////////////////////////////////////////////////////////
TextureBasedIsosurfaceUpdateEventHandler::~TextureBasedIsosurfaceUpdateEventHandler()
{
}
///////////////////////////////////////////////////////////////////////////////////////
TextureBasedIsosurfaceUpdateEventHandler& 
TextureBasedIsosurfaceUpdateEventHandler::operator=(const TextureBasedIsosurfaceUpdateEventHandler& rhs)
{
   if(&rhs != this)
   {
      TextureBasedEventHandler::operator=(rhs);
   }
   return *this;
}
/////////////////////////////////////////////////////////////////////////////////////   
void TextureBasedIsosurfaceUpdateEventHandler::_operateOnNode(XMLObject* veXMLObject)
{
   try
   {
      Command* command = dynamic_cast< Command* >( veXMLObject );
      DataValuePairWeakPtr isovalue = command->GetDataValuePair("Iso-Surface Value");      
      double value;
      isovalue->GetData(value);
      ves::xplorer::TextureBasedVizHandler::instance()->UpdateIsosurface(value);
   }
   catch(...)
   {
      std::cout<<"Invalid TextureDataSet!!"<<std::endl;
      std::cout<<"TextureBasedIsosurfaceUpdateEventHandler::_operateOnNode()"<<std::endl;
   }
}
