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
#include <ves/xplorer/DeviceHandler.h>
#include <ves/xplorer/command/CommandManager.h>

#include <ves/xplorer/device/KeyboardMouse.h>
#include <ves/xplorer/device/Wand.h>
#include <ves/xplorer/device/Tablet.h>
#include <ves/xplorer/device/Gloves.h>

#include <ves/xplorer/scenegraph/SceneManager.h>

#include <ves/xplorer/scenegraph/manipulator/ManipulatorManager.h>
#include <ves/xplorer/scenegraph/manipulator/TransformManipulator.h>

#include <ves/xplorer/event/EventHandler.h>
#include <ves/xplorer/event/device/DeviceEH.h>
#include <ves/xplorer/event/device/DeviceModeEH.h>
#include <ves/xplorer/event/device/CenterPointEventHandler.h>
#include <ves/xplorer/event/device/DraggerScalingEventHandler.h>
#include <ves/xplorer/event/cad/UnselectObjectsEventHandler.h>
#include <ves/xplorer/event/environment/NavigationDataEventHandler.h>

#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>

// --- OSG Includes --- //
#include <osg/BoundingSphere>

using namespace ves::xplorer;
using namespace ves::xplorer::command;

vprSingletonImp( DeviceHandler );

////////////////////////////////////////////////////////////////////////////////
DeviceHandler::DeviceHandler()
    :
    mActiveDCS( scenegraph::SceneManager::instance()->GetActiveNavSwitchNode() ),
    mSelectedDCS( NULL ),
    mDeviceMode( "World Navigation" ),
    mResetCenterPointPosition( 0.0, 0.1, 0.0 ),
    mCenterPoint( mResetCenterPointPosition ),
    mCenterPointThreshold( 0.1 ),
    mCenterPointJump( 10.0 ),
    m_deviceBeingProcessed( NULL )
{
    //Initialize glove device
    device::Device* device( NULL );
    device = new device::Gloves();
    m_deviceMap[ device::Device::GLOVES ] = device;

    //Initialize keyboard mouse device
    device = new device::KeyboardMouse();
    device->Enable();
    m_deviceMap[ device::Device::KEYBOARD_MOUSE ] = device;

    //Initialize tablet device
    device = new device::Wand();
    device->Enable();
    m_deviceMap[ device::Device::TABLET ] = device;

    //Initialize eand device
    device = new device::Tablet();
    device->Enable();
    m_deviceMap[ device::Device::WAND ] = device;

    device = NULL;

    //Set properties in Devices
    DeviceMap::const_iterator itr = m_deviceMap.begin();
    for( itr; itr != m_deviceMap.end(); ++itr )
    {
        device::Device* device = itr->second;
        device->SetCenterPoint( &mCenterPoint );
        device->SetCenterPointThreshold( &mCenterPointThreshold );
        device->SetCenterPointJump( &mCenterPointJump );
        device->SetResetWorldPosition( &mResetAxis, &mResetPosition );
    }

    mEventHandlers[ "ENABLE_DEVICE" ] =
        new event::DeviceEventHandler();
    mEventHandlers[ "CHANGE_DEVICE_MODE" ] =
        new event::DeviceModeEventHandler();
    mEventHandlers[ "UNSELECT_OBJECTS" ] =
        new event::UnselectObjectsEventHandler();
    mEventHandlers[ "CENTER_POINT_UPDATE" ] =
        new event::CenterPointEventHandler();
    mEventHandlers[ "Navigation_Data" ] =
        new event::NavigationDataEventHandler();
    mEventHandlers[ "DRAGGER_SCALING_VALUE" ] =
        new event::DraggerScalingEventHandler();
    
    mResetPosition.resize( 3 );
}
////////////////////////////////////////////////////////////////////////////////
DeviceHandler::~DeviceHandler()
{
    //Delete m_deviceMap in map
    DeviceMap::iterator itr = m_deviceMap.begin();
    for( itr; itr != m_deviceMap.end(); ++itr )
    {
        delete itr->second;
    }

    m_deviceMap.clear();
}
////////////////////////////////////////////////////////////////////////////////
void DeviceHandler::EnableDevice(
    const device::Device::Type& type, const bool& enable )
{
    DeviceMap::const_iterator itr = m_deviceMap.find( type );
    if( itr != m_deviceMap.end() )
    {
        device::Device* device = itr->second;
        device->Enable( enable );
    }
}
////////////////////////////////////////////////////////////////////////////////
void DeviceHandler::ExecuteCommands()
{
    const ves::open::xml::CommandPtr tempCommand = 
        CommandManager::instance()->GetXMLCommand();
    if( !tempCommand )
    {
        return;
    }

    std::map< std::string, event::EventHandler* >::iterator ehItr =
        mEventHandlers.find( tempCommand->GetCommandName() );
    if( ehItr == mEventHandlers.end() )
    {
        return;
    }

    event::EventHandler* tempEvent = ehItr->second;
    DeviceMap::const_iterator itr = m_deviceMap.begin();
    for( itr; itr != m_deviceMap.end(); ++itr )
    {
        device::Device* device = itr->second;
        if( device->IsEnabled() )
        {
            tempEvent->SetGlobalBaseObject( device );
            tempEvent->Execute( tempCommand );
        } 
    }
}
////////////////////////////////////////////////////////////////////////////////
device::Device* const DeviceHandler::GetDevice(
    const device::Device::Type& type ) const
{
    DeviceMap::const_iterator itr = m_deviceMap.find( type );
    if( itr == m_deviceMap.end() )
    {
        //Error output
        return NULL;
    }

    return itr->second;
}
////////////////////////////////////////////////////////////////////////////////
device::Device* const DeviceHandler::GetDeviceBeingProcessed() const
{
    return m_deviceBeingProcessed;
}
////////////////////////////////////////////////////////////////////////////////
scenegraph::DCS* const DeviceHandler::GetActiveDCS() const
{
    return mActiveDCS.get();
}
////////////////////////////////////////////////////////////////////////////////
void DeviceHandler::GetResetWorldPosition(
    osg::Quat& quat, std::vector< double >& pos )
{
    quat = mResetAxis;
    pos = mResetPosition;
}
////////////////////////////////////////////////////////////////////////////////
scenegraph::DCS* const DeviceHandler::GetSelectedDCS() const
{
    return mSelectedDCS.get();
}
////////////////////////////////////////////////////////////////////////////////
void DeviceHandler::ProcessDeviceEvents()
{
    //Update device properties
    ExecuteCommands();

    //Process device events
    DeviceMap::const_iterator itr = m_deviceMap.begin();
    for( itr; itr != m_deviceMap.end(); ++itr )
    {
        m_deviceBeingProcessed = itr->second;
        if( m_deviceBeingProcessed->IsEnabled() )
        {
            m_deviceBeingProcessed->ProcessEvents();
        }
    }

    m_deviceBeingProcessed = NULL;
}
////////////////////////////////////////////////////////////////////////////////
void DeviceHandler::ResetCenterPoint()
{
    mCenterPoint = mResetCenterPointPosition;
}
////////////////////////////////////////////////////////////////////////////////
void DeviceHandler::SetActiveDCS( scenegraph::DCS* activeDCS )
{
    mActiveDCS = activeDCS;
}
////////////////////////////////////////////////////////////////////////////////
void DeviceHandler::SetDeviceMode( const std::string& deviceMode )
{
    mDeviceMode = deviceMode;

    if( mDeviceMode == "World Navigation" )
    {
        mActiveDCS = 
            scenegraph::SceneManager::instance()->GetActiveNavSwitchNode();
    }
    else if( mDeviceMode == "Object Navigation" )
    {
        if( mSelectedDCS.valid() )
        {
            mActiveDCS = mSelectedDCS;
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void DeviceHandler::SetCenterPoint( gmtl::Point3d* centerPoint )
{
    mCenterPoint = *centerPoint;
}
////////////////////////////////////////////////////////////////////////////////
void DeviceHandler::SetCenterPointJumpMode( const std::string& jumpMode )
{
    if( jumpMode == "Small" )
    {
        mCenterPointJump = 10.0;
        mCenterPointThreshold = 0.1;
    }
    else if( jumpMode == "Medium" )
    {
        mCenterPointJump = 100.0;
        mCenterPointThreshold = 1.0;
    }
    else if( jumpMode == "Large" )
    {
        mCenterPointJump = 1000.0;
        mCenterPointThreshold = 10.0;
    }
    else if( jumpMode == "Bounding Box" )
    {
        mCenterPointJump = mActiveDCS->getBound().radius();
        mCenterPointThreshold = mCenterPointJump * 0.01;
    }
}
////////////////////////////////////////////////////////////////////////////////
void DeviceHandler::SetResetWorldPosition(
    osg::Quat& quat, std::vector< double >& pos )
{
    mResetAxis = quat;
    mResetPosition = pos;
    
    /*
    DeviceMap::const_iterator itr;
    for( itr = m_deviceMap.begin(); itr != m_deviceMap.end(); ++itr )
    {
        itr->second->SetResetWorldPosition( mResetAxis, mResetPosition );
    }
    */
}
////////////////////////////////////////////////////////////////////////////////
void DeviceHandler::SetSelectedDCS( scenegraph::DCS* selectedDCS )
{
    mSelectedDCS = selectedDCS;

    scenegraph::manipulator::ManipulatorManager* manipulatorManager =
        scenegraph::SceneManager::instance()->GetManipulatorManager();
    scenegraph::manipulator::TransformManipulator* sceneManipulator =
        manipulatorManager->GetSceneManipulator();

    if( sceneManipulator->IsEnabled() && !sceneManipulator->getNodeMask() )
    {
        sceneManipulator->Show();
    }
}
////////////////////////////////////////////////////////////////////////////////
void DeviceHandler::UnselectObjects()
{
    mActiveDCS = scenegraph::SceneManager::instance()->GetWorldDCS();

    if( mSelectedDCS.valid() )
    {
        mSelectedDCS->SetTechnique( "Default" );
        mSelectedDCS = NULL;

        scenegraph::manipulator::ManipulatorManager* manipulatorManager =
            scenegraph::SceneManager::instance()->GetManipulatorManager();
        scenegraph::manipulator::TransformManipulator* sceneManipulator =
            manipulatorManager->GetSceneManipulator();

        if( sceneManipulator->IsEnabled() )
        {
            sceneManipulator->Hide();
            sceneManipulator->Disconnect();
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
