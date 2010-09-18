/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2010 by Iowa State University
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

// --- VE-Suite Includes --- //
#include <ves/xplorer/event/environment/ManipulatorEventHandler.h>

#include <ves/xplorer/GlobalBase.h>
#include <ves/xplorer/DeviceHandler.h>

#include <ves/xplorer/scenegraph/SceneManager.h>

#include <ves/xplorer/scenegraph/manipulator/ManipulatorManager.h>
#include <ves/xplorer/scenegraph/manipulator/TransformManipulator.h>

#include <ves/open/xml/XMLObject.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>

// --- VR Juggler Includes --- //
#include <boost/filesystem/operations.hpp>
#include <boost/filesystem/path.hpp>

// --- C/C++ Includes --- //
#include <string>

#ifdef WIN32
#include <direct.h>
#else
#include <unistd.h>
#endif

using namespace ves::xplorer::event;

////////////////////////////////////////////////////////////////////////////////
ManipulatorEventHandler::ManipulatorEventHandler()
    :
    ves::xplorer::event::EventHandler()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
ManipulatorEventHandler::ManipulatorEventHandler(
    const ManipulatorEventHandler& meh )
    :
    ves::xplorer::event::EventHandler()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
ManipulatorEventHandler::~ManipulatorEventHandler()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void ManipulatorEventHandler::SetGlobalBaseObject(
    ves::xplorer::GlobalBase* modelHandler )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void ManipulatorEventHandler::Execute(
    const ves::open::xml::XMLObjectPtr& veXMLObject )
{
    ves::open::xml::CommandPtr command =
        boost::dynamic_pointer_cast< ves::open::xml::Command >( veXMLObject );

    ves::open::xml::DataValuePairPtr manipulatorDVP =
        command->GetDataValuePair( "MANIPULATOR_DVP" );
    if( !manipulatorDVP )
    {
        return;
    }

    scenegraph::manipulator::ManipulatorManager& manipulatorManager =
        scenegraph::SceneManager::instance()->GetManipulatorManager();
    scenegraph::manipulator::TransformManipulator* sceneManipulator =
        manipulatorManager.GetSceneManipulator();

    std::string data;
    manipulatorDVP->GetData( data );
    if( data == "ENABLE" )
    {
        manipulatorManager.Enable();
        sceneManipulator->Enable();

        if( ves::xplorer::DeviceHandler::instance()->GetSelectedDCS() )
        {
            sceneManipulator->Show();
        }
    }
    else if( data == "DISABLE" )
    {
        manipulatorManager.Enable( false );
        sceneManipulator->Enable( false );
        sceneManipulator->Hide();
    }
    else if( data == "TRANSLATE" )
    {
        sceneManipulator->SetEnabledModes(
            scenegraph::manipulator::TransformationType::TRANSLATE_COMPOUND );
        sceneManipulator->DefaultForm();
    }
    else if( data == "ROTATE" )
    {
        sceneManipulator->SetEnabledModes(
            scenegraph::manipulator::TransformationType::ROTATE_COMPOUND );
        sceneManipulator->DefaultForm();
    }
    else if( data == "SCALE" )
    {
        sceneManipulator->SetEnabledModes(
            scenegraph::manipulator::TransformationType::SCALE_COMPOUND );
        sceneManipulator->DefaultForm();
    }
    else if( data == "COMBO" )
    {
        sceneManipulator->SetEnabledModes(
            scenegraph::manipulator::TransformationType::ALL );
        sceneManipulator->ComboForm();
    }
}
////////////////////////////////////////////////////////////////////////////////
ManipulatorEventHandler& ManipulatorEventHandler::operator=(
    const ManipulatorEventHandler& meh )
{
    if( this != &meh )
    {
        ves::xplorer::event::EventHandler::operator=( meh );
    }

    return *this;
}
////////////////////////////////////////////////////////////////////////////////
