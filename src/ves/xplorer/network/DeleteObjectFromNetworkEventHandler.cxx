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
#include <ves/xplorer/network/DeleteObjectFromNetworkEventHandler.h>
#include <ves/xplorer/cfdModel.h>
#include <ves/xplorer/ModelHandler.h>
#include <ves/xplorer/network/cfdExecutive.h>
#include <ves/xplorer/plugincfdVEBaseClass.h>


#include <ves/open/xml/XMLObject.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/FloatArray.h>
#include <ves/open/xml/Transform.h>
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/ParameterBlock.h>
#include <ves/open/xml/Model/Model.h>

#include <ves/xplorer/cfdDebug.h>

#include <iostream>

using namespace VE_EVENTS;
using namespace VE_Xplorer;

////////////////////////////////////////////////////////////////////////////
//Constructor                                                             //
////////////////////////////////////////////////////////////////////////////
DeleteObjectFromNetworkEventHandler::DeleteObjectFromNetworkEventHandler()
:VE_EVENTS::EventHandler()
{
   ;
}
////////////////////////////////////////////////////////////////////////////////
DeleteObjectFromNetworkEventHandler::DeleteObjectFromNetworkEventHandler(const DeleteObjectFromNetworkEventHandler& rhs)
:VE_EVENTS::EventHandler(rhs)
{
   ;
}
////////////////////////////////////////////////////////////////////////////////
///Destructor                                      //
////////////////////////////////////////////////////////////////////////////////
DeleteObjectFromNetworkEventHandler::~DeleteObjectFromNetworkEventHandler()
{
   ;
}
////////////////////////////////////////////////////////////////////////////////
///Equal operator
////////////////////////////////////////////////////////////////////////////////
DeleteObjectFromNetworkEventHandler& DeleteObjectFromNetworkEventHandler::operator=(const DeleteObjectFromNetworkEventHandler& rhs)
{
   if(this != &rhs)
   {
      VE_EVENTS::DeleteObjectFromNetworkEventHandler::operator=(rhs);
   }
   return *this;
}
////////////////////////////////////////////////////////////////////////////////
void DeleteObjectFromNetworkEventHandler::SetGlobalBaseObject(VE_Xplorer::cfdGlobalBase* model)
{
   ;
}
//////////////////////////////////////////////////////////////////////////
void DeleteObjectFromNetworkEventHandler::Execute( VE_XML::XMLObject* xmlObject )
{
   // Get the active object
   VE_XML::Command* command = dynamic_cast< VE_XML::Command* >( xmlObject );
   VE_XML::DataValuePairWeakPtr activeModelDVP = command->GetDataValuePair( "Object ID" );   
   unsigned int id = 0;
   activeModelDVP->GetData( id );

   _plugins = cfdExecutive::instance()->GetTheCurrentPlugins();

   // Remove any plugins that aren't present in the current network
   std::map< int, cfdVEBaseClass* >::iterator foundPlugin;
   foundPlugin = _plugins->find( id );

   if ( foundPlugin != _plugins->end() )
   {
      vprDEBUG(vesDBG,1) << "|\t\tPlugin [ " << foundPlugin->first 
                              << " ]-> " << foundPlugin->second 
                              << " is being deleted."
                              << std::endl << vprDEBUG_FLUSH;
      // if a module is on the plugins map then remove it
      foundPlugin->second->RemoveSelfFromSG();
      cfdModelHandler::instance()->RemoveModel( foundPlugin->second->GetCFDModel() );
      // Must delete current instance of vebaseclass object
      delete foundPlugin->second;
      _plugins->erase( foundPlugin );
   }
   else
   {
      vprDEBUG(vesDBG,1) << "|\t\tPlugin [ " << id 
                              << " ] not present."
                              << std::endl << vprDEBUG_FLUSH;
   }
   //Set active model to null so that if the previous active model is deleted
   //that we don't get errors in our code other places.
   cfdModelHandler::instance()->SetActiveModel( 0 );
   vprDEBUG(vesDBG,1) << "|\t\tPlugin is deleted if present."
                           << std::endl << vprDEBUG_FLUSH;
}
