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
#include <ves/xplorer/event/cad/AttributeEventHandler.h>
#include <ves/xplorer/cfdModel.h>
#include <ves/xplorer/GlobalBase.h>
#include <ves/xplorer/ModelHandler.h>

#include <ves/open/xml/XMLObject.h>
#include <iostream>

using namespace ves::xplorer::event;
using namespace ves::open::xml::cad;
using namespace ves::xplorer::scenegraph;
//////////////////////////////////////////////////////////
///Constructor                                          //
//////////////////////////////////////////////////////////
AttributeEventHandler::AttributeEventHandler()
:ves::xplorer::event::EventHandler()
{
   _activeModel = 0;
}
////////////////////////////////////////////////////////////
AttributeEventHandler::AttributeEventHandler(const AttributeEventHandler& rhs)
:ves::xplorer::event::EventHandler()
{
   _activeModel = rhs._activeModel;
}
////////////////////////////////////
///Destructor                     //
////////////////////////////////////
AttributeEventHandler::~AttributeEventHandler()
{
}
///////////////////////////////////////////////////////////////////////////
void AttributeEventHandler::SetGlobalBaseObject(ves::xplorer::GlobalBase* model)
{
   try
   {
      if(model)
      {
         _activeModel = dynamic_cast<ves::xplorer::Model*>(model);
      }
      else
      {
         _activeModel = ves::xplorer::cfdModelHandler::instance()->GetActiveModel();
      }
   }
   catch(...)
   {
      _activeModel = 0;
      std::cout<<"Invalid object passed to CADEventHandler!"<<std::endl;
   }
}
///////////////////////////////////////////////////////
///Exectute the event                                //
///////////////////////////////////////////////////////
void AttributeEventHandler::Execute(ves::open::xml::XMLObject* veXMLObject)
{
   if(_activeModel)
   {
      //this is overridden in derived classes
      _operateOnNode(veXMLObject);
   }
}
///////////////////////////////////////////////////////////////////////
AttributeEventHandler& AttributeEventHandler::operator=(const AttributeEventHandler& rhs)
{
   if(this != &rhs)
   {
      _activeModel = rhs._activeModel;
   }
   return *this;
}
