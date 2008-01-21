/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2008 by Iowa State University
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

#include <ves/xplorer/event/environment/ExportDOTFileEventHandler.h>
#include <ves/xplorer/scenegraph/SceneManager.h>
#include <ves/xplorer/scenegraph/CreateGraphDOTVisitor.h>

#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>

using namespace ves::xplorer::event;
using namespace ves::xplorer;
using namespace ves::open::xml;

////////////////////////////////////////////////////////////////////
ExportDOTFileEventHandler::ExportDOTFileEventHandler()
{}
///////////////////////////////////////////////////////////////////
ExportDOTFileEventHandler
::ExportDOTFileEventHandler( const ExportDOTFileEventHandler& ceh )
{}
/////////////////////////////////////////////////////////////////////
ExportDOTFileEventHandler::~ExportDOTFileEventHandler()
{
    ;
}
///////////////////////////////////////////////////////////////////////////////////////
ExportDOTFileEventHandler&
ExportDOTFileEventHandler::operator=( const ExportDOTFileEventHandler& rhs )
{
    if( &rhs != this )
    {
        ves::xplorer::event::EventHandler::operator=( rhs );
    }
    return *this;
}
///////////////////////////////////////////////////////////////
void ExportDOTFileEventHandler::SetGlobalBaseObject( ves::xplorer::GlobalBase* baseObject )
{
    ;
}
//////////////////////////////////////////////////////////////////////////
void ExportDOTFileEventHandler::Execute( XMLObject* veXMLObject )
{
    try
    {
        Command* command =
            dynamic_cast< Command* >( veXMLObject );
        std::string filename;
        command->GetDataValuePair( "Filename" )->GetData( filename );
        // store the active geometry and viz objects as a pfb
        // (but not the sun, menu, laser, or text)
        ves::xplorer::scenegraph::CreateGraphDOTVisitor dotCreator(
            ves::xplorer::scenegraph::SceneManager::instance()->GetRootNode(), filename );
    }
    catch ( ... )
    {
        std::cout << "Error!!" << std::endl;
        std::cout << "StoredSceneEventHandler::_operateOnNode()" << std::endl;
    }
}
