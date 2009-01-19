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

#include <ves/xplorer/event/EventHandler.h>
#include <ves/xplorer/event/device/DeviceEH.h>
#include <ves/xplorer/event/device/DeviceModeEH.h>
#include <ves/xplorer/event/cad/UnselectObjectsEventHandler.h>
#include <ves/xplorer/event/device/CenterPointEventHandler.h>
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
    mResetCenterPointPosition( 0.0, 0.1, 0.0 ),
    mCenterPoint( mResetCenterPointPosition ),
    mCenterPointThreshold( 0.1 ),
    mCenterPointJump( 10.0 )
{
    //Initialize Devices
    mDevices[ "Tablet" ] =
        new ves::xplorer::Tablet();
    mDevices[ "Wand" ] =
        new ves::xplorer::Wand();
    mDevices[ "KeyboardMouse" ] =
        new ves::xplorer::KeyboardMouse();
    mDevices[ "Gloves" ] =
        new ves::xplorer::Gloves();

    //Set properties in Devices
    std::map< const std::string, ves::xplorer::Device* >::const_iterator itr;
    for( itr = mDevices.begin(); itr != mDevices.end(); itr++ )
    {
        itr->second->SetCenterPoint( &mCenterPoint );
        itr->second->SetCenterPointThreshold( &mCenterPointThreshold );
        itr->second->SetCenterPointJump( &mCenterPointJump );
        itr->second->SetResetWorldPosition( &mResetAxis, &mResetPosition );
    }

    mActiveDevice = mDevices.find( "KeyboardMouse" )->second;

    mEventHandlers[ "CHANGE_DEVICE" ] =
        new ves::xplorer::event::DeviceEventHandler();
    mEventHandlers[ "CHANGE_DEVICE_MODE" ] =
        new ves::xplorer::event::DeviceModeEventHandler();
    mEventHandlers[ "UNSELECT_OBJECTS" ] =
        new ves::xplorer::event::UnselectObjectsEventHandler();
    mEventHandlers[ "CENTER_POINT_UPDATE" ] =
        new ves::xplorer::event::CenterPointEventHandler();
    mEventHandlers[ "TRACKBALL_PROPERTIES" ] =
        new ves::xplorer::event::KeyboardMouseEventHandler();
    mEventHandlers[ "Navigation_Data" ] =
        new ves::xplorer::event::NavigationDataEventHandler();
    
    mResetPosition.resize( 3 );
}
////////////////////////////////////////////////////////////////////////////////
DeviceHandler::~DeviceHandler()
{
    //Delete mDevices in map
    std::map< const std::string, ves::xplorer::Device* >::iterator itr;
    for( itr = mDevices.begin(); itr != mDevices.end(); ++itr )
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
            currentEventHandler->second->SetGlobalBaseObject( mActiveDevice );
            currentEventHandler->second->Execute(
                ModelHandler::instance()->GetXMLCommand() );

            //Tablet and Wand are always active and need updated...
            if( ModelHandler::instance()->GetXMLCommand()->
                GetCommandName() == "Navigation_Data" )
            {
                currentEventHandler->second->SetGlobalBaseObject(
                    mDevices.find( "Tablet" )->second );
                currentEventHandler->second->Execute(
                    ModelHandler::instance()->GetXMLCommand() );
                currentEventHandler->second->SetGlobalBaseObject(
                    mDevices.find( "Wand" )->second );
                currentEventHandler->second->Execute(
                    ModelHandler::instance()->GetXMLCommand() );
            }
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
ves::xplorer::scenegraph::DCS* const DeviceHandler::GetActiveDCS() const
{
    return mActiveDCS.get();
}
////////////////////////////////////////////////////////////////////////////////
ves::xplorer::Device* const DeviceHandler::GetActiveDevice() const
{
    return mActiveDevice;
}
////////////////////////////////////////////////////////////////////////////////
ves::xplorer::Device* const DeviceHandler::GetDevice(
    const std::string& deviceName ) const
{
    return mDevices.find( deviceName )->second;
}
////////////////////////////////////////////////////////////////////////////////
void DeviceHandler::GetResetWorldPosition(
    osg::Quat& quat, std::vector< double >& pos )
{
    quat = mResetAxis;
    pos = mResetPosition;
}
////////////////////////////////////////////////////////////////////////////////
ves::xplorer::scenegraph::DCS* const DeviceHandler::GetSelectedDCS() const
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
        mDevices.find( "Wand" )->second->UpdateNavigation();
        mDevices.find( "KeyboardMouse" )->second->UpdateNavigation();

        if( ( mActiveDevice != mDevices.find( "Wand" )->second ) && 
            ( mActiveDevice != mDevices.find( "KeyboardMouse" )->second ) )
        {
            mActiveDevice->UpdateNavigation();
        }
    }

    //Always do this by default
    mDevices.find( "Tablet" )->second->UpdateNavigation();
    //Always do this by default
    mDevices.find( "Gloves" )->second->UpdateNavigation();
}
////////////////////////////////////////////////////////////////////////////////
void DeviceHandler::ResetCenterPoint()
{
    mCenterPoint = mResetCenterPointPosition;
}
////////////////////////////////////////////////////////////////////////////////
void DeviceHandler::SetActiveDCS( ves::xplorer::scenegraph::DCS* activeDCS )
{
    mActiveDCS = activeDCS;
}
////////////////////////////////////////////////////////////////////////////////
void DeviceHandler::SetActiveDevice( const std::string& activeDevice )
{
    std::map< const std::string, ves::xplorer::Device* >::const_iterator itr =
        mDevices.find( activeDevice );
    if( itr != mDevices.end() )
    {
        mActiveDevice = itr->second;
        //std::cout << "|\tDeviceHandler::SetActiveDevice = " << activeDevice << std::endl;
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
            ves::xplorer::scenegraph::SceneManager::instance()->GetWorldDCS();
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
    std::map< const std::string, ves::xplorer::Device* >::const_iterator itr;
    for( itr = mDevices.begin(); itr != mDevices.end(); ++itr )
    {
        itr->second->SetResetWorldPosition( mResetAxis, mResetPosition );
    }
    */
}
////////////////////////////////////////////////////////////////////////////////
void DeviceHandler::SetSelectedDCS( ves::xplorer::scenegraph::DCS* selectedDCS )
{
    mSelectedDCS = selectedDCS;
}
////////////////////////////////////////////////////////////////////////////////
void DeviceHandler::UnselectObjects()
{
    mActiveDCS = ves::xplorer::scenegraph::SceneManager::instance()->
        GetWorldDCS();

    if( mSelectedDCS.valid() )
    {
        mSelectedDCS->SetTechnique( "Default" );
        mSelectedDCS = 0;
    }
}
////////////////////////////////////////////////////////////////////////////////
