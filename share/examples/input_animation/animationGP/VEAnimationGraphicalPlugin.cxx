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

// --- My Includes --- //
#include "VEAnimationGraphicalPlugin.h"

// --- VE-Suite Includes --- //
#include <ves/open/xml/model/Model.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/shader/Shader.h>
#include <ves/open/xml/DataValuePair.h>

#include <ves/xplorer/scenegraph/SceneManager.h>

#include <ves/xplorer/device/KeyboardMouse.h>

#include <gadget/Type/KeyboardMouse/KeyEvent.h>
#include <gadget/Type/KeyboardMouse/MouseEvent.h>
#include <gadget/Type/KeyboardMouseInterface.h>

// --- OSG Includes --- //
#include <osg/MatrixTransform>
#include <osg/AnimationPath>
#include <osg/ShapeDrawable>
#include <osg/Sequence>

#include <osgText/Text>

#include <osgDB/ReadFile>

#include <osgSim/ColorRange>
#include <osg/Vec3d>

#include <boost/lexical_cast.hpp>

using namespace opcgp;

////////////////////////////////////////////////////////////////////////////////
VEAnimationGraphicalPlugin::VEAnimationGraphicalPlugin()
    :
    ves::xplorer::plugin::PluginBase(),
    m_keyboard( 0 )
{
    m_valveHeight = 0;
    mObjectName = "DSPlugin"; //name of the sheet
    ///Set the name of the commands we want to capture from the dynsim unit
    mEventHandlerMap[ "OPCData" ] = this;
}
////////////////////////////////////////////////////////////////////////////////
VEAnimationGraphicalPlugin::~VEAnimationGraphicalPlugin()
{
/*    if( !mSceneManager )
    {
        return;
    }

    osg::ref_ptr< osg::Group > rootNode =
        mSceneManager->GetRootNode();

    if( !rootNode.valid() )
    {
        return;
    }
    
    rootNode->removeChild( _roomGeometry.get() );

    for( std::map< int, osg::ref_ptr< display::DigitalGauge > >::iterator
            itr = _gauges.begin(); itr != _gauges.end(); ++itr )
    {
        rootNode->removeChild( itr->second.get() );
    }*/
}
////////////////////////////////////////////////////////////////////////////////
void VEAnimationGraphicalPlugin::InitializeNode( osg::Group* veworldDCS )
{
    PluginBase::InitializeNode( veworldDCS );

    m_keyboard = 
        dynamic_cast< ves::xplorer::device::KeyboardMouse* >( mDevice );

    //m_idleGeometry = osgDB::readNodeFile( "valve/valve.idle.osg" );
    //m_openGeometry = osgDB::readNodeFile( "valve/valve.opening.osg" );
    //m_closeGeometry = osgDB::readNodeFile( "valve/valve.closing.osg" );

    m_idleGeometry = osgDB::readNodeFile( "Valve/handwheel.ive" );
    m_openGeometry = osgDB::readNodeFile( "Valve/stem.ive" );
    m_closeGeometry = osgDB::readNodeFile( "Valve/valve.ive" );
    
    //m_valueAnimation = new osg::Switch();
    //m_valueAnimation->addChild( m_idleGeometry.get() );
    //m_valueAnimation->addChild( m_openGeometry.get() );
    //m_valueAnimation->addChild( m_closeGeometry.get() );
    //m_valueAnimation->setSingleChildOn( 0 );

	m_rotationDCS = new ves::xplorer::scenegraph::DCS();
	m_translationDCS = new ves::xplorer::scenegraph::DCS();

    m_rotationDCS->addChild( m_idleGeometry.get() );
    m_translationDCS->addChild( m_openGeometry.get() );
    m_rotationDCS->addChild( m_translationDCS.get() );

    mDCS->addChild( m_closeGeometry.get() );
    mDCS->addChild( m_rotationDCS.get() );

    double rot[3] = { 90.0, 0.0, 0.0 };
    double pos[3] = {-1000.0, -532.0, -10.0 };
    double scale[3] = { 0.14, 0.14, 0.16 };
    //mDCS->SetTranslationArray( pos );
    //mDCS->SetScaleArray( scale );
    //mDCS->SetRotationArray( rot );
}
////////////////////////////////////////////////////////////////////////////////
void VEAnimationGraphicalPlugin::PreFrameUpdate()
{

    double rotRate = 5;
    double transRate = 0.0039;

    double* tempTrans = m_translationDCS->GetVETranslationArray();
    /*if ( gmtl::Math::abs( tempTrans[1] -  m_valveHeight ) > ( transRate) )
    {
        double* tempRot = m_rotationDCS->GetRotationArray();
        tempRot[2] += rotRate;
        m_rotationDCS->SetRotationArray( tempRot );

        if( m_valveHeight > tempTrans[1])
        {
            tempTrans[1] += transRate;
        }
        else
        {
            tempTrans[1] -= transRate;
        }
        m_translationDCS->SetTranslationArray( tempTrans );
    }*/

    if ( gmtl::Math::abs( tempTrans[1] -  m_valveHeight ) > ( transRate) )
    {
        double* tempRot = m_rotationDCS->GetRotationArray();
        tempRot[2] += rotRate;
        if( m_valveHeight > tempTrans[1])
        {
            tempRot[2] -= rotRate;
        }
        else
        {
            tempRot[2] += rotRate;
        }
        m_rotationDCS->SetRotationArray( tempRot );

    }
	
    tempTrans[1] = m_valveHeight;
    m_translationDCS->SetTranslationArray( tempTrans );
    //Process key board event
/*    if( m_keyboard )
    {
        gadget::KeyboardMousePtr tempKeys = 
            m_keyboard->GetKeyboardMouseVRJDevice();
            
        //Get the event queue
        gadget::KeyboardMouse::EventQueue evt_queue =
            tempKeys->getEventQueue();
        
        //Return if no events occurred
        if( evt_queue.empty() )
        {
            return;
        }
        
        //Get the modifier key values
        bool mKeyNone = tempKeys->modifierOnly( gadget::KEY_NONE );
        bool mKeyShift = tempKeys->modifierOnly( gadget::KEY_SHIFT );
        bool mKeyAlt = tempKeys->modifierOnly( gadget::KEY_ALT );
        
        //Iterate over the keyboard and mouse events
        gadget::KeyboardMouse::EventQueue::iterator i;
        for( i = evt_queue.begin(); i != evt_queue.end(); ++i )
        {
            const gadget::EventType type = ( *i )->type();
            
            switch( type )
            {
                case gadget::MouseButtonPressEvent:
                {
                    gadget::MouseEventPtr mouse_evt =
                        boost::dynamic_pointer_cast< gadget::MouseEvent >( *i );

                    mButton = mouse_evt->getButton();
                    
                    if( mButton == gadget::MBUTTON1)
                    {
                        m_valueAnimation->setSingleChildOn( 1 );
                    }
                    else if( mButton == gadget::MBUTTON3)
                    {
                        m_valueAnimation->setSingleChildOn( 2 );
                    }
                    break;
                }
                case gadget::MouseButtonReleaseEvent:
                {
                    m_valueAnimation->setSingleChildOn( 0 );
                    break;
                }
            }
        }
    }*/
}
////////////////////////////////////////////////////////////////////////////////
void VEAnimationGraphicalPlugin::SetCurrentCommand(
    ves::open::xml::CommandPtr command )
{
    if( !command )
    {
        return;
    }
    
    /*if( command->GetDataValuePair("MY_VALVE") )
    {
        std::string percent;
        command->GetDataValuePair("MY_VALVE")->GetData( percent );
        double test = boost::lexical_cast<double>( percent );
        
        m_valveHeight = 0.125 * test;
    }*/

    if( command->GetDataValuePair("MY_VALVE") )
    {
        std::string percent;
        command->GetDataValuePair("MY_VALVE")->GetData( percent );
        double test = boost::lexical_cast<double>( percent );
        
        m_valveHeight = -0.125 * test;
    }

    const std::string commandName = command->GetCommandName();
    std::cout << "Command Name " << commandName << std::endl;
    
    size_t numDVPs = command->GetNumberOfDataValuePairs();
    for( size_t i = 0; i < numDVPs; ++i )
    {
        std::cout << command->GetDataValuePair( i )->GetDataName() << std::endl;
        std::string percent;
        command->GetDataValuePair(i)->GetData( percent );
        std::cout << percent << std::endl;
        ///Do something with the dvp data        
        //Get the specific dvp for the valve position
        //m_rotationDCS
        //m_translationDCS
		
		//Convert the valve data to an angle
		//Set a matrix value to rotate the valve
		
		//Move the shaft or rotate the shaft
    }
}
////////////////////////////////////////////////////////////////////////////////
