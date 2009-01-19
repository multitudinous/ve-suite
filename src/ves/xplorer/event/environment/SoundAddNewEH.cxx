/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2009 by Iowa State University
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

#include <ves/xplorer/event/environment/SoundAddNewEH.h>
#include <ves/xplorer/Model.h>
#include <ves/xplorer/ModelHandler.h>

#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>

using namespace ves::xplorer::event;
using namespace ves::xplorer;
using namespace ves::open::xml;

////////////////////////////////////////////////////////////////////
SoundAddNewEventHandler::SoundAddNewEventHandler()
{
    _activeModel = 0;
}
///////////////////////////////////////////////////////////////////
SoundAddNewEventHandler
::SoundAddNewEventHandler( const SoundAddNewEventHandler& ceh )
{
    _activeModel = ceh._activeModel;
}
/////////////////////////////////////////////////////////////////////
SoundAddNewEventHandler::~SoundAddNewEventHandler()
{}
///////////////////////////////////////////////////////////////////////////////////////
SoundAddNewEventHandler&
SoundAddNewEventHandler::operator=( const SoundAddNewEventHandler& rhs )
{
    if( &rhs != this )
    {
        _activeModel = rhs._activeModel;
    }
    return *this;
}
//////////////////////////////////////////////////////////////////////////////////////////////
void SoundAddNewEventHandler::SetGlobalBaseObject( ves::xplorer::GlobalBase* baseObject )
{
    ///is this overkill????
    try
    {
        if( baseObject )
        {
            _activeModel = dynamic_cast<ves::xplorer::Model*>( baseObject );
        }
        else
        {
            _activeModel = ves::xplorer::ModelHandler::instance()->GetActiveModel();
        }
    }
    catch ( ... )
    {
        _activeModel = 0;
        std::cout << "Invalid object passed to TextureBasedEventHandler!" << std::endl;
    }
}
/////////////////////////////////////////////////////////////////////////////////////
void SoundAddNewEventHandler::Execute( const ves::open::xml::XMLObjectPtr& veXMLObject )
{
    try
    {
        if( _activeModel )
        {
            CommandPtr command = boost::dynamic_pointer_cast<ves::open::xml::Command>( veXMLObject );
            DataValuePairPtr soundName = command->GetDataValuePair( "Sound Name" );
            std::string guiName;
            soundName->GetData( guiName );

            std::string fileName;
            DataValuePairPtr soundFile = command->GetDataValuePair( "Sound Filename" );
            soundFile->GetData( fileName );
            _activeModel->AddNewSound( guiName, fileName );
        }
    }
    catch ( ... )
    {
        std::cout << "Invalid Model!!" << std::endl;
        std::cout << "SoundAddNewEventHandler::_operateOnNode()" << std::endl;
    }

}
