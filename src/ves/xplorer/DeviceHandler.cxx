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

// --- VE-Suite Includes --- //
#include <ves/xplorer/DeviceHandler.h>
#include <ves/xplorer/ModelHandler.h>

#include <ves/xplorer/device/KeyboardMouse.h>
#include <ves/xplorer/device/Wand.h>
#include <ves/xplorer/device/Tablet.h>

#include <ves/xplorer/event/EventHandler.h>
#include <ves/xplorer/event/device/DeviceEH.h>
#include <ves/xplorer/event/device/DeviceModeEH.h>
#include <ves/xplorer/event/cad/UnselectObjectsEventHandler.h>
#include <ves/xplorer/event/device/CenterPointJumpEventHandler.h>
#include <ves/xplorer/event/device/KeyboardMouseEH.h>
#include <ves/xplorer/event/environment/NavigationDataEventHandler.h>

#include <ves/xplorer/scenegraph/SceneManager.h>

#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>

// --- OSG Includes --- //
#include <osg/BoundingSphere>

using namespace ves::xplorer;

vprSingletonImp( DeviceHandler );

////////////////////////////////////////////////////////////////////////////////
DeviceHandler::DeviceHandler()
    :
    mActiveDCS(
        ves::xplorer::scenegraph::SceneManager::instance()->GetWorldDCS() ),
    mSelectedDCS( 0 ),
    mDeviceMode( "World Navigation" ),
    mCenterPoint( 0, 0.1, 0 ),
    mCenterPointThreshold( 0.5f ),
    mCenterPointJump( 10.0f )
{
    //Initialize Devices
    mDevices[ std::string( "Tablet" ) ] =
        new ves::xplorer::Tablet();
    mDevices[ std::string( "Wand" ) ] =
        new ves::xplorer::Wand();
    mDevices[ std::string( "KeyboardMouse" ) ] =
        new ves::xplorer::KeyboardMouse();

    //Set properties in Devices
    std::map< std::string, ves::xplorer::Device* >::const_iterator itr;
    for( itr = mDevices.begin(); itr != mDevices.end(); itr++ )
    {
        itr->second->SetActiveDCS( mActiveDCS.get() );
        itr->second->SetSelectedDCS( mSelectedDCS.get() );
        itr->second->SetCenterPoint( &mCenterPoint );
        itr->second->SetCenterPointThreshold( &mCenterPointThreshold );
        itr->second->SetCenterPointJump( &mCenterPointJump );
    }

    mActiveDevice = mDevices[ "KeyboardMouse" ];

    mEventHandlers[ std::string( "CHANGE_DEVICE" ) ] =
        new ves::xplorer::event::DeviceEventHandler();
    mEventHandlers[ std::string( "CHANGE_DEVICE_MODE" ) ] =
        new ves::xplorer::event::DeviceModeEventHandler();
    mEventHandlers[ std::string( "UNSELECT_OBJECTS" ) ] =
        new ves::xplorer::event::UnselectObjectsEventHandler();
    mEventHandlers[ std::string( "CHANGE_CENTERPOINT_MODE" ) ] =
        new ves::xplorer::event::CenterPointJumpEventHandler();
    mEventHandlers[ std::string( "TRACKBALL_PROPERTIES" ) ] =
        new ves::xplorer::event::KeyboardMouseEventHandler();
    mEventHandlers[ std::string( "Navigation_Data" ) ] =
        new ves::xplorer::event::NavigationDataEventHandler();
    
    mResetPosition.resize( 3 );
}
////////////////////////////////////////////////////////////////////////////////
DeviceHandler::~DeviceHandler()
{
    //Delete mDevices in map
    for( std::map< std::string, ves::xplorer::Device* >::iterator
            itr = mDevices.begin(); itr != mDevices.end(); ++itr )
    {
        delete itr->second;
    }

    mDevices.clear();
}
////////////////////////////////////////////////////////////////////////////////
void DeviceHandler::ExecuteCommands()
{
    std::map< std::string, ves::xplorer::event::EventHandler* >::iterator
        currentEventHandler;
    if( ModelHandler::instance()->GetXMLCommand() )
    {
        currentEventHandler = mEventHandlers.find(
            ModelHandler::instance()->GetXMLCommand()->GetCommandName() );

        if( currentEventHandler != mEventHandlers.end() )
        {
            //Set properties in Devices
            std::map< std::string, ves::xplorer::Device* >::const_iterator itr;
            for( itr = mDevices.begin(); itr != mDevices.end(); itr++ )
            {
                itr->second->SetActiveDCS( mActiveDCS.get() );
                itr->second->SetSelectedDCS( mSelectedDCS.get() );
            }

            currentEventHandler->second->SetGlobalBaseObject( mActiveDevice );
            currentEventHandler->second->Execute(
                ModelHandler::instance()->GetXMLCommand() );
            //Tablet and Wand is always active and need updated...
            if( ModelHandler::instance()->GetXMLCommand()->GetCommandName() ==
                "Navigation_Data" )
            {
                currentEventHandler->second->SetGlobalBaseObject(
                    mDevices[ "Tablet" ] );
                currentEventHandler->second->Execute(
                    ModelHandler::instance()->GetXMLCommand() );
                currentEventHandler->second->SetGlobalBaseObject(
                    mDevices[ "Wand" ] );
                currentEventHandler->second->Execute(
                    ModelHandler::instance()->GetXMLCommand() );
            }
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void DeviceHandler::SetActiveDevice( const std::string& activeDevice )
{
    std::map< std::string, ves::xplorer::Device* >::iterator itr =
        mDevices.find( activeDevice );
    if( itr != mDevices.end() )
    {
        mActiveDevice = mDevices[ activeDevice ];
    }
}
////////////////////////////////////////////////////////////////////////////////
void DeviceHandler::SetDeviceMode( const std::string& deviceMode )
{
    mDeviceMode = deviceMode;

    if( mDeviceMode == "World Navigation" )
    {
        mActiveDCS = ves::xplorer::scenegraph::SceneManager::instance()->
                         GetActiveSwitchNode();
        mActiveDevice->SetActiveDCS( mActiveDCS.get() );
    }
    else if( mDeviceMode == "Object Navigation" )
    {
        if( mSelectedDCS.valid() )
        {
            mActiveDCS = mSelectedDCS;
        }

        mActiveDevice->SetActiveDCS( mActiveDCS.get() );
    }
}
////////////////////////////////////////////////////////////////////////////////
void DeviceHandler::SetCenterPointJumpMode( const std::string& jumpMode )
{
    if( jumpMode == "Small" )
    {
        mCenterPointJump = 10.0;
        mCenterPointThreshold = 0.5;
    }
    else if( jumpMode == "Medium" )
    {
        mCenterPointJump = 100.0;
        mCenterPointThreshold = 5.0;
    }
    else if( jumpMode == "Large" )
    {
        mCenterPointJump = 1000.0;
        mCenterPointThreshold = 50.0;
    }
    else if( jumpMode == "Bounding Box" )
    {
        mCenterPointJump = mActiveDCS->getBound().radius();
        mCenterPointThreshold = mCenterPointJump * 0.05;
    }
}
////////////////////////////////////////////////////////////////////////////////
void DeviceHandler::UnselectObjects()
{
    mActiveDCS = ves::xplorer::scenegraph::SceneManager::instance()->
        GetActiveSwitchNode();
    mActiveDevice->SetActiveDCS( mActiveDCS.get() );

    mSelectedDCS->SetTechnique( "Default" );
    mSelectedDCS = 0;
    mActiveDevice->SetSelectedDCS( mSelectedDCS.get() );
}
////////////////////////////////////////////////////////////////////////////////
void DeviceHandler::ProcessDeviceEvents()
{
    //Update Device properties
    ExecuteCommands();

    if( mDeviceMode == "World Navigation" ||
        mDeviceMode == "Object Navigation" )
    {
        mDevices[ "Wand" ]->UpdateNavigation();
        mDevices[ "KeyboardMouse" ]->UpdateNavigation();

        if( ( mActiveDevice != mDevices[ "Wand" ] ) && 
            ( mActiveDevice != mDevices[ "KeyboardMouse" ] ) )
        {
            mActiveDevice->UpdateNavigation();
        }
    }
    else if( mDeviceMode == "Selection" )
    {
        mActiveDevice->UpdateSelection();
    }

    //Get the active dcs from the scenemanager
    //mActiveDCS = ves::xplorer::scenegraph::SceneManager::instance()->
        //GetActiveSwitchNode();

    //Get the selected dcs from the active device
    mSelectedDCS = mActiveDevice->GetSelectedDCS();

    //Always do this be default
    mDevices[ "Tablet" ]->UpdateNavigation();
}
////////////////////////////////////////////////////////////////////////////////
ves::xplorer::Device* DeviceHandler::GetDevice( const std::string& device )
{
    return mDevices[ device ];
}
////////////////////////////////////////////////////////////////////////////////
ves::xplorer::Device* DeviceHandler::GetActiveDevice()
{
    return mActiveDevice;
}
////////////////////////////////////////////////////////////////////////////////
void DeviceHandler::SetResetWorldPosition( osg::Quat& quat, 
    std::vector< double >& pos )
{
    mResetAxis = quat;
    mResetPosition = pos;
    
    for( std::map< std::string, ves::xplorer::Device* >::const_iterator 
        itr = mDevices.begin(); itr != mDevices.end(); itr++ )
    {
        itr->second->SetResetWorldPosition( mResetAxis, mResetPosition );
    }
}
////////////////////////////////////////////////////////////////////////////////
void DeviceHandler::GetResetWorldPosition(
    osg::Quat& quat, std::vector< double >& pos )
{
    quat = mResetAxis;
    pos = mResetPosition;
}
////////////////////////////////////////////////////////////////////////////////
