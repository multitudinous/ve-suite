/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2012 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
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
/*  -*-c++-*-
 *  Copyright (C) 2008 Cedric Pinson <mornifle@plopbyte.net>
 *
 * This library is open source and may be redistributed and/or modified under
 * the terms of the OpenSceneGraph Public License (OSGPL) version 0.0 or
 * (at your option) any later version.  The full license is in LICENSE file
 * included with this distribution, and on the openscenegraph.org website.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * OpenSceneGraph Public License for more details.
 *
 * Authors:
 * Cedric Pinson <mornifle@plopbyte.net>
 * jeremy Moles <jeremy@emperorlinux.com>
*/

#include <ves/xplorer/scenegraph/util/CharacterAnimation.h>
#include <ves/xplorer/scenegraph/util/AnimationManagerFinder.h>

#include <ves/xplorer/scenegraph/SceneManager.h>

#include <iostream>
#include <osg/io_utils>
#include <osg/Geometry>
#include <osg/MatrixTransform>
#include <osg/Geode>
#include <osgViewer/Viewer>
#include <osgViewer/ViewerEventHandlers>
#include <osgWidget/ViewerEventHandlers>
#include <osgGA/TrackballManipulator>
#include <osgGA/StateSetManipulator>
#include <osgDB/ReadFile>
#include <osgAnimation/AnimationManagerBase>
#include <osgAnimation/Bone>

////////////////////////////////////////////////////////////////////////////////
CharacterAnimation::CharacterAnimation()
    :
    _model( 0 ),
    _focus( 0 )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
CharacterAnimation::~CharacterAnimation()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
bool CharacterAnimation::list()
{
    //std::cout << "Animation List:" << std::endl;
    for( osgAnimation::AnimationMap::iterator it = _map.begin(); it != _map.end(); ++it )
    {
        std::cout << "|\t " << it->first << std::endl;
    }

    return true;
}
////////////////////////////////////////////////////////////////////////////////
bool CharacterAnimation::play()
{
    if( _focus < m_amv.size() )
    {
        std::cout << "|\tPlay " << m_amv[_focus] << std::endl;
        _model->playAnimation( _map[m_amv[_focus]].get() );
        return true;
    }

    return false;
}
////////////////////////////////////////////////////////////////////////////////
bool CharacterAnimation::stop()
{
    if( _focus < m_amv.size() )
    {
        std::cout << "|\tStop " << m_amv[_focus] << std::endl;
        _model->stopAnimation( _map[m_amv[_focus]].get() );
        return true;
    }
    return false;
}
////////////////////////////////////////////////////////////////////////////////
bool CharacterAnimation::next()
{
    _focus = ( _focus + 1 ) % _map.size();
    std::cout << "Current now is " << m_amv[_focus] << std::endl;
    return true;
}
////////////////////////////////////////////////////////////////////////////////
bool CharacterAnimation::previous()
{
    _focus = ( _map.size() + _focus - 1 ) % _map.size();
    std::cout << "Current now is " << m_amv[_focus] << std::endl;
    return true;
}
////////////////////////////////////////////////////////////////////////////////
bool CharacterAnimation::playByName( const std::string& name )
{
    for( unsigned int i = 0; i < m_amv.size(); i++ )
    {
        if( m_amv[i] == name )
        {
            _focus = i;
        }
    }

    _model->playAnimation( _map[name].get() );
    return true;
}
////////////////////////////////////////////////////////////////////////////////
const std::string& CharacterAnimation::getCurrentAnimationName() const
{
    return m_amv[_focus];
}
////////////////////////////////////////////////////////////////////////////////
const std::vector< std::string >& CharacterAnimation::getAnimationMap() const
{
    return m_amv;
}
////////////////////////////////////////////////////////////////////////////////
osg::Group* CharacterAnimation::Register( std::string const& fileName )
{
    osg::Group* root = new osg::Group();

    //for( size_t i = 0; i < fileName.size(); ++i )
    {
        osg::Group* node = dynamic_cast<osg::Group*>( osgDB::readNodeFile( fileName ) );
        //dynamic_cast<osgAnimation::AnimationManager*>(osgDB::readNodeFile(psr[1]));
        if( !node )
        {
            std::cout << "No data loaded" << std::endl;
            return 0;
        }
        root->addChild( node );
    }

    // Set our Singleton's model.
    AnimationManagerFinder finder;
    root->accept( finder );
    if( finder._am.valid() )
    {
        root->setUpdateCallback( finder._am.get() );
        CharacterAnimation::setModel( finder._am.get() );
    }
    else
    {
        osg::notify( osg::WARN ) << "no osgAnimation::AnimationManagerBase found in the subgraph, no animations available" << std::endl;
    }

    root->setNodeMask( 0x0001 );
    return root;
}
////////////////////////////////////////////////////////////////////////////////
