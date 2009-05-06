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
#include <ves/xplorer/ModelHandler.h>

#include <ves/xplorer/device/KeyboardMouse.h>
#include <ves/xplorer/device/Wand.h>
#include <ves/xplorer/device/Tablet.h>
#include <ves/xplorer/device/Gloves.h>

#include <ves/xplorer/scenegraph/SceneManager.h>

#include <ves/xplorer/scenegraph/physics/PhysicsSimulator.h>
#include <ves/xplorer/scenegraph/physics/CharacterController.h>

#include <ves/xplorer/event/EventHandler.h>
#include <ves/xplorer/event/device/DeviceEH.h>
#include <ves/xplorer/event/device/DeviceModeEH.h>
#include <ves/xplorer/event/device/CenterPointEventHandler.h>
#include <ves/xplorer/event/device/KeyboardMouseEH.h>
#include <ves/xplorer/event/cad/UnselectObjectsEventHandler.h>
#include <ves/xplorer/event/environment/NavigationDataEventHandler.h>

#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>

// --- OSG Includes --- //
#include <osg/BoundingSphere>

using namespace ves::xplorer;

namespace vx = ves::xplorer;
namespace vxd = ves::xplorer::device;
namespace vxs = vx::scenegraph;

vprSingletonImp( DeviceHandler );

////////////////////////////////////////////////////////////////////////////////
DeviceHandler::DeviceHandler()
    :
    mActiveDCS( vxs::SceneManager::instance()->GetActiveNavSwitchNode() ),
    mSelectedDCS( NULL ),
    mDeviceMode( "World Navigation" ),
    mResetCenterPointPosition( 0.0, 0.1, 0.0 ),
    mCenterPoint( mResetCenterPointPosition ),
    mCenterPointThreshold( 0.1 ),
    mCenterPointJump( 10.0 ),
    m_guiEventAdapter( new osgGA::GUIEventAdapter() )
{
    //Initialize Devices
    mDevices[ "Tablet" ] = new vxd::Tablet();
    mDevices[ "Wand" ] = new vxd::Wand();
    mDevices[ "KeyboardMouse" ] = new vxd::KeyboardMouse();
    mDevices[ "Gloves" ] = new vxd::Gloves();

    mTabletDevice = mDevices[ "Tablet" ];
    mGlovesDevice = mDevices[ "Gloves" ];
    mWandDevice = mDevices[ "Wand" ];
    mKMDevice = mDevices[ "KeyboardMouse" ];
    
    //Set properties in Devices
    vxs::CharacterController* characterController =
        vxs::SceneManager::instance()->GetCharacterController();
    characterController->Initialize();
    std::map< const std::string, vxd::Device* >::const_iterator itr;
    for( itr = mDevices.begin(); itr != mDevices.end(); ++itr )
    {
        vxd::Device* device = itr->second;
        device->SetCenterPoint( &mCenterPoint );
        device->SetCenterPointThreshold( &mCenterPointThreshold );
        device->SetCenterPointJump( &mCenterPointJump );
        device->SetResetWorldPosition( &mResetAxis, &mResetPosition );
        device->SetCharacterController( characterController );
    }

    mActiveDevice = mDevices.find( "KeyboardMouse" )->second;

    mEventHandlers[ "CHANGE_DEVICE" ] =
        new vx::event::DeviceEventHandler();
    mEventHandlers[ "CHANGE_DEVICE_MODE" ] =
        new vx::event::DeviceModeEventHandler();
    mEventHandlers[ "UNSELECT_OBJECTS" ] =
        new vx::event::UnselectObjectsEventHandler();
    mEventHandlers[ "CENTER_POINT_UPDATE" ] =
        new vx::event::CenterPointEventHandler();
    mEventHandlers[ "TRACKBALL_PROPERTIES" ] =
        new vx::event::KeyboardMouseEventHandler();
    mEventHandlers[ "Navigation_Data" ] =
        new vx::event::NavigationDataEventHandler();
    
    mResetPosition.resize( 3 );
}
////////////////////////////////////////////////////////////////////////////////
DeviceHandler::~DeviceHandler()
{
    //Delete mDevices in map
    std::map< const std::string, vxd::Device* >::iterator itr;
    for( itr = mDevices.begin(); itr != mDevices.end(); ++itr )
    {
        delete itr->second;
    }

    mDevices.clear();
}
////////////////////////////////////////////////////////////////////////////////
void DeviceHandler::ExecuteCommands()
{
    std::map< std::string, vx::event::EventHandler* >::iterator
        currentEventHandler;
    const ves::open::xml::CommandPtr tempCommand = 
        ModelHandler::instance()->GetXMLCommand();
    if( tempCommand )
    {
        currentEventHandler = mEventHandlers.find(
            tempCommand->GetCommandName() );

        if( currentEventHandler != mEventHandlers.end() )
        {
            vx::event::EventHandler* tempEvent = 
                currentEventHandler->second;
            tempEvent->SetGlobalBaseObject( mActiveDevice );
            tempEvent->Execute( tempCommand );

            //Tablet and Wand are always active and need updated...
            if( tempCommand->GetCommandName() == "Navigation_Data" )
            {
                tempEvent->SetGlobalBaseObject( mTabletDevice );
                tempEvent->Execute( tempCommand );
                tempEvent->SetGlobalBaseObject( mWandDevice );
                tempEvent->Execute( tempCommand );
            }
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
vxs::DCS* const DeviceHandler::GetActiveDCS() const
{
    return mActiveDCS.get();
}
////////////////////////////////////////////////////////////////////////////////
vxd::Device* const DeviceHandler::GetActiveDevice() const
{
    return mActiveDevice;
}
////////////////////////////////////////////////////////////////////////////////
vxd::Device* const DeviceHandler::GetDevice(
    const std::string& deviceName ) const
{
    return mDevices.find( deviceName )->second;
}
////////////////////////////////////////////////////////////////////////////////
const vxd::GUIActionAdapter& DeviceHandler::GetGUIActionAdapter() const
{
    return m_guiActionAdapter;
}
////////////////////////////////////////////////////////////////////////////////
osgGA::GUIEventAdapter* const DeviceHandler::GetGUIEventAdapter() const
{
    return m_guiEventAdapter.get();
}
////////////////////////////////////////////////////////////////////////////////
void DeviceHandler::GetResetWorldPosition(
    osg::Quat& quat, std::vector< double >& pos )
{
    quat = mResetAxis;
    pos = mResetPosition;
}
////////////////////////////////////////////////////////////////////////////////
vxs::DCS* const DeviceHandler::GetSelectedDCS() const
{
    return mSelectedDCS.get();
}
////////////////////////////////////////////////////////////////////////////////
void DeviceHandler::ProcessDeviceEvents()
{
    //Update Device properties
    ExecuteCommands();

    if( mDeviceMode == "Selection" )
    {
        mActiveDevice->UpdateSelection();
    }
    else
    {
        mWandDevice->UpdateNavigation();
        mKMDevice->UpdateNavigation();

        if( ( mActiveDevice != mWandDevice ) && 
            ( mActiveDevice != mKMDevice ) )
        {
            mActiveDevice->UpdateNavigation();
        }
    }

    //Always do this by default
    mTabletDevice->UpdateNavigation();
    //Always do this by default
    mGlovesDevice->UpdateNavigation();
}
////////////////////////////////////////////////////////////////////////////////
void DeviceHandler::ResetCenterPoint()
{
    mCenterPoint = mResetCenterPointPosition;
}
////////////////////////////////////////////////////////////////////////////////
void DeviceHandler::SetActiveDCS( vxs::DCS* activeDCS )
{
    mActiveDCS = activeDCS;
}
////////////////////////////////////////////////////////////////////////////////
void DeviceHandler::SetActiveDevice( const std::string& activeDevice )
{
    std::map< const std::string, vxd::Device* >::const_iterator itr =
        mDevices.find( activeDevice );
    if( itr != mDevices.end() )
    {
        mActiveDevice = itr->second;
        /*
        std::cout << "|\tDeviceHandler::SetActiveDevice = "
                  << activeDevice << std::endl;
        */
        if( activeDevice == "Gloves" )
        {
            mActiveDevice->Initialize();
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void DeviceHandler::SetDeviceMode( const std::string& deviceMode )
{
    mDeviceMode = deviceMode;

    if( mDeviceMode == "World Navigation" )
    {
        mActiveDCS = 
            vxs::SceneManager::instance()->GetActiveNavSwitchNode();
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
    std::map< const std::string, vxd::Device* >::const_iterator itr;
    for( itr = mDevices.begin(); itr != mDevices.end(); ++itr )
    {
        itr->second->SetResetWorldPosition( mResetAxis, mResetPosition );
    }
    */
}
////////////////////////////////////////////////////////////////////////////////
void DeviceHandler::SetSelectedDCS( vxs::DCS* selectedDCS )
{
    mSelectedDCS = selectedDCS;
}
////////////////////////////////////////////////////////////////////////////////
void DeviceHandler::UnselectObjects()
{
    mActiveDCS = vxs::SceneManager::instance()->GetWorldDCS();

    if( mSelectedDCS.valid() )
    {
        mSelectedDCS->SetTechnique( "Default" );
        mSelectedDCS = 0;
    }
}
////////////////////////////////////////////////////////////////////////////////
