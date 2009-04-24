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

// --- C/C++ Libraries --- //


////////////////////////////////////////////////////////////////////////////////
VEAnimationGraphicalPlugin::VEAnimationGraphicalPlugin()
    :
    PluginBase(),
    m_keyboard( 0 )
{
    mObjectName = "input_animation";
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

    //Create the gauges
    osg::ref_ptr< osg::Group > rootNode =
        ves::xplorer::scenegraph::SceneManager::instance()->GetRootNode();

    m_keyboard = 
        dynamic_cast< ves::xplorer::KeyboardMouse* >( mDevice );
/*    osg::ref_ptr< osg::Node > temp = osgDB::readNodeFile( "Models/fermentor_room.ive" );
    _roomGeometry->addChild( temp.get() );
    rootNode->addChild( _roomGeometry.get() );
    mDCS->addChild( fermentorGroup.get() );

    _fermentorGeometry = osgDB::readNodeFile( "Models/fermentor_noimpeller.ive" );
    _impellerGeometry = osgDB::readNodeFile( "Models/impeller_fixed.ive" );
    _tankGeometry = osgDB::readNodeFile( "Models/opaque_tank.ive" );*/
}
////////////////////////////////////////////////////////////////////////////////
void VEAnimationGraphicalPlugin::PreFrameUpdate()
{
    //Process key board event
    if( m_keyboard )
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
            }
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
