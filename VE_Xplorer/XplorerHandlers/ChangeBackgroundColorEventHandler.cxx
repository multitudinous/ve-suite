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
 * Date modified: $Date: 2006-07-08 22:57:46 -0500 (Sat, 08 Jul 2006) $
 * Version:       $Rev: 4907 $
 * Author:        $Author: mccdo $
 * Id:            $Id: ActiveModelEventHandler.cxx 4907 2006-07-09 03:57:46Z mccdo $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "VE_Xplorer/XplorerHandlers/ChangeBackgroundColorEventHandler.h"
#include "VE_Xplorer/XplorerHandlers/cfdGlobalBase.h"
#include "VE_Xplorer/XplorerHandlers/cfdEnvironmentHandler.h"
#include "VE_Open/XML/XMLObject.h"
#include "VE_Open/XML/Command.h"
#include "VE_Open/XML/DataValuePair.h"

#include <string>

#include <boost/filesystem/operations.hpp> // includes boost/filesystem/path.hpp
#include <boost/filesystem/path.hpp>

#ifdef WIN32
#include <direct.h>
#else
#include <unistd.h>
#endif

using namespace VE_EVENTS;
//////////////////////////////////////////////////////////
///Constructor                                          //
//////////////////////////////////////////////////////////
ChangeBackgroundColorEventHandler::ChangeBackgroundColorEventHandler()
:VE_EVENTS::EventHandler()
{
}
////////////////////////////////////////////////////////////
ChangeBackgroundColorEventHandler::ChangeBackgroundColorEventHandler(const ChangeBackgroundColorEventHandler& rhs)
:VE_EVENTS::EventHandler()
{
}
////////////////////////////////////
///Destructor                     //
////////////////////////////////////
ChangeBackgroundColorEventHandler::~ChangeBackgroundColorEventHandler()
{
}
///////////////////////////////////////////////////////////////////////////
void ChangeBackgroundColorEventHandler::SetGlobalBaseObject( VE_Xplorer::cfdGlobalBase* modelHandler )
{
}
///////////////////////////////////////////////////////
///Exectute the event                                //
///////////////////////////////////////////////////////
void ChangeBackgroundColorEventHandler::Execute(VE_XML::XMLObject* veXMLObject)
{
   VE_XML::Command* command = dynamic_cast< VE_XML::Command* >( veXMLObject );
   VE_XML::DataValuePair* activeModelDVP = command->GetDataValuePair( "Background Color" );
   std::vector<double> color;
   activeModelDVP->GetData( color );
   if(!color.empty())
   {
      VE_Xplorer::cfdEnvironmentHandler::instance()->SetBackgroundColor(color);
   }
}
///////////////////////////////////////////////////////////////////////
ChangeBackgroundColorEventHandler& ChangeBackgroundColorEventHandler::operator=(const ChangeBackgroundColorEventHandler& rhs)
{
   if(this != &rhs)
   {
   }
   return *this;
}
