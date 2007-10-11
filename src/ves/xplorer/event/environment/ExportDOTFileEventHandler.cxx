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
 * Date modified: $Date: 2007-06-15 11:02:33 -0500 (Fri, 15 Jun 2007) $
 * Version:       $Rev: 8205 $
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#include <VE_Xplorer/XplorerHandlers/ExportDOTFileEventHandler.h>
#include <VE_Xplorer/SceneGraph/SceneManager.h>
#include <VE_Xplorer/SceneGraph/CreateGraphDOTVisitor.h>

#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>

using namespace VE_EVENTS;
using namespace VE_Xplorer;

////////////////////////////////////////////////////////////////////
ExportDOTFileEventHandler::ExportDOTFileEventHandler()
{
}
///////////////////////////////////////////////////////////////////
ExportDOTFileEventHandler
::ExportDOTFileEventHandler(const ExportDOTFileEventHandler& ceh)
{
}
/////////////////////////////////////////////////////////////////////
ExportDOTFileEventHandler::~ExportDOTFileEventHandler()
{
    ;
}
///////////////////////////////////////////////////////////////////////////////////////
ExportDOTFileEventHandler& 
ExportDOTFileEventHandler::operator=(const ExportDOTFileEventHandler& rhs)
{
   if(&rhs != this)
   {
      VE_EVENTS::EventHandler::operator=(rhs);
   }
   return *this;
}
///////////////////////////////////////////////////////////////
void ExportDOTFileEventHandler::SetGlobalBaseObject(VE_Xplorer::cfdGlobalBase* baseObject)
{
    ;
}
//////////////////////////////////////////////////////////////////////////   
void ExportDOTFileEventHandler::Execute(VE_XML::XMLObject* veXMLObject)
{
    try
    {
        VE_XML::Command* command = 
            dynamic_cast< VE_XML::Command* >( veXMLObject );
        std::string filename;
        command->GetDataValuePair("Filename")->GetData( filename );
        // store the active geometry and viz objects as a pfb
        // (but not the sun, menu, laser, or text)
        VE_SceneGraph::CreateGraphDOTVisitor dotCreator( 
            VE_SceneGraph::SceneManager::instance()->GetRootNode(), filename );
    }
    catch( ... )
    {
        std::cout<<"Error!!"<<std::endl;
        std::cout<<"StoredSceneEventHandler::_operateOnNode()"<<std::endl;
    }
}
