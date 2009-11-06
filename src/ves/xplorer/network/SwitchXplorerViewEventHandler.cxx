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
// --- VE-Suite Includes --- //
#include <ves/xplorer/network/SwitchXplorerViewEventHandler.h>
#include <ves/xplorer/Model.h>
#include <ves/xplorer/ModelHandler.h>
#include <ves/xplorer/DeviceHandler.h>
#include <ves/xplorer/device/Device.h>
#include <ves/xplorer/device/KeyboardMouse.h>

#include <ves/xplorer/network/cfdExecutive.h>
#include <ves/xplorer/network/NetworkSystemView.h>

#include <ves/xplorer/plugin/PluginBase.h>

#include <ves/xplorer/scenegraph/SceneManager.h>

#include <ves/open/xml/XMLObject.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/FloatArray.h>
#include <ves/open/xml/Transform.h>
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/ParameterBlock.h>
#include <ves/open/xml/model/Model.h>

#include <ves/xplorer/Debug.h>

// --- OSG Includes --- //
#include <osg/MatrixTransform>
#include <osg/Group>

// --- C/C++ Libraries --- //
#include <iostream>

using namespace ves::xplorer::event;
using namespace ves::xplorer;
using namespace ves::xplorer::scenegraph;
using namespace ves::open::xml;
using namespace ves::xplorer::network;

////////////////////////////////////////////////////////////////////////////////
SwitchXplorerViewEventHandler::SwitchXplorerViewEventHandler()
        :
        ves::xplorer::event::EventHandler()
{
    ;
}

////////////////////////////////////////////////////////////////////////////////
SwitchXplorerViewEventHandler::SwitchXplorerViewEventHandler( const SwitchXplorerViewEventHandler& rhs )
        :
        ves::xplorer::event::EventHandler( rhs )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
SwitchXplorerViewEventHandler::~SwitchXplorerViewEventHandler()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
SwitchXplorerViewEventHandler& SwitchXplorerViewEventHandler::operator=( const SwitchXplorerViewEventHandler& rhs )
{
    if( this != &rhs )
    {
        SwitchXplorerViewEventHandler::operator=( rhs );
    }

    return *this;
}
////////////////////////////////////////////////////////////////////////////////
void SwitchXplorerViewEventHandler::SetGlobalBaseObject( ves::xplorer::GlobalBase* model )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void SwitchXplorerViewEventHandler::Execute( const ves::open::xml::XMLObjectPtr& xmlObject )
{
    //If the job is not submitted return
    if( !cfdExecutive::instance()->GetNetworkSystemView() )
    {
        return;
    }

    CommandPtr command( boost::dynamic_pointer_cast<ves::open::xml::Command>( xmlObject ) );
    DataValuePairPtr activeModelDVP =
        command->GetDataValuePair( "CHANGE_XPLORER_VIEW" );

    if( activeModelDVP )
    {
        std::string viewData;
        activeModelDVP->GetData( viewData );
        if( viewData == "CHANGE_XPLORER_VIEW_NETWORK" )
        {
            UpdateNetworkView( command );
            SceneManager::instance()->SetActiveSwitchNode( 2 );

            //centers the network view            
            device::KeyboardMouse* keyboardMouse =
                DeviceHandler::instance()->GetDevice(
                    device::Device::KEYBOARD_MOUSE )->AsKeyboardMouse();
            keyboardMouse->FrameAll();
        }
        else if( viewData == "CHANGE_XPLORER_VIEW_CAD" )
        {
            SceneManager::instance()->SetActiveSwitchNode( 0 );
        }
        else if( viewData == "CHANGE_XPLORER_VIEW_LOGO" )
        {
            SceneManager::instance()->SetActiveSwitchNode( 1 );
        }
        else
        {
            SceneManager::instance()->SetActiveSwitchNode( 0 );
        }
    }
    else
    {
        UpdateNetworkView( command );
    }

    ves::xplorer::DeviceHandler::instance()->SetActiveDCS(
        ves::xplorer::scenegraph::SceneManager::instance()->GetActiveNavSwitchNode() );
}
////////////////////////////////////////////////////////////////////////////////
void SwitchXplorerViewEventHandler::UpdateNetworkView( const ves::open::xml::CommandPtr& cmd )
{
    osg::ref_ptr< osg::Group > tempDCS = SceneManager::instance()->GetNetworkDCS();
    if (tempDCS->getNumChildren() > 0)
    {
        tempDCS->removeChildren( 0, tempDCS->getNumChildren() );
    }

    DataValuePairPtr dvp = cmd->GetDataValuePair("SUBNET_ID");
    std::string netId; 
    dvp->GetData(netId);
    //osg::ref_ptr< osg::Group > tempGroup = networkLayout.DrawNetwork();
    osg::ref_ptr< osg::Group > tempGroup = cfdExecutive::instance()->
        GetNetworkSystemView()->DrawNetwork( netId );
    if( tempGroup.valid() )
    {
        tempDCS->addChild( tempGroup.get() );
    }
}
