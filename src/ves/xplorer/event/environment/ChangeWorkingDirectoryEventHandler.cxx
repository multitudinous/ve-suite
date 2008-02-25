/*************** <auto-copyright.rb BEGIN do not edit this line> **************
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
 *************** <auto-copyright.rb END do not edit this line> ***************/
#include <ves/xplorer/event/environment/ChangeWorkingDirectoryEventHandler.h>
#include <ves/xplorer/GlobalBase.h>
#include <ves/xplorer/environment/cfdTeacher.h>
#include <ves/xplorer/EnvironmentHandler.h>
#include <ves/xplorer/scenegraph/SceneManager.h>

#include <ves/open/xml/XMLObject.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>

#include <string>

#include <boost/filesystem/operations.hpp> // includes boost/filesystem/path.hpp
#include <boost/filesystem/path.hpp>

#ifdef WIN32
#include <direct.h>
#else
#include <unistd.h>
#endif

using namespace ves::xplorer::event;
using namespace ves::open::xml;
//////////////////////////////////////////////////////////
///Constructor                                          //
//////////////////////////////////////////////////////////
ChangeWorkingDirectoryEventHandler::ChangeWorkingDirectoryEventHandler()
        : ves::xplorer::event::EventHandler()
{}
////////////////////////////////////////////////////////////
ChangeWorkingDirectoryEventHandler::ChangeWorkingDirectoryEventHandler( const ChangeWorkingDirectoryEventHandler& rhs )
        : ves::xplorer::event::EventHandler()
{}
////////////////////////////////////
///Destructor                     //
////////////////////////////////////
ChangeWorkingDirectoryEventHandler::~ChangeWorkingDirectoryEventHandler()
{}
///////////////////////////////////////////////////////////////////////////
void ChangeWorkingDirectoryEventHandler::SetGlobalBaseObject( ves::xplorer::GlobalBase* modelHandler )
{}
///////////////////////////////////////////////////////
///Exectute the event                                //
///////////////////////////////////////////////////////
void ChangeWorkingDirectoryEventHandler::Execute( const ves::open::xml::XMLObjectPtr& veXMLObject )
{
    CommandPtr command = veXMLObject;
    DataValuePairWeakPtr activeModelDVP = command->GetDataValuePair( "WORKING_DIRECTORY" );
    std::string newWorkingDir;
    activeModelDVP->GetData( newWorkingDir );
    std::cout << newWorkingDir << std::endl;

    if( newWorkingDir.empty() )
    {
        newWorkingDir.assign( "./" );
    }
    boost::filesystem::path dir_path( newWorkingDir, boost::filesystem::native );
    try
    {
        if( !boost::filesystem::is_directory( dir_path ) )
        {
            return;
        }
    }
    catch( ... )
    {
        return;
    }
#ifdef WIN32
    _chdir( newWorkingDir.c_str() );
#else
    chdir( newWorkingDir.c_str() );
#endif
    //A new working directory also means that 
    //the STORED scenes are no longer valid
    ves::xplorer::EnvironmentHandler::instance()->GetTeacher()->Reset();
    //Since Xplorer does not really have a "new" eh clear the osgOQ stuff here
    //ves::xplorer::scenegraph::SceneManager::instance()->ResetOcclusionQueryContext();
}
///////////////////////////////////////////////////////////////////////
ChangeWorkingDirectoryEventHandler& ChangeWorkingDirectoryEventHandler::operator=( const ChangeWorkingDirectoryEventHandler& rhs )
{
    if( this != &rhs )
    {}
    return *this;
}
