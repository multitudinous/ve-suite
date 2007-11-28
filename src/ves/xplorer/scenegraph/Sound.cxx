/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2007 by Iowa State University
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
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifdef VE_SOUND

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/Sound.h>
#include <ves/xplorer/scenegraph/DCS.h>

// --- OSG Includes --- //
#include <osg/Geode>
#include <osg/ShapeDrawable>

// --- osgAL Includes --- //
#include <osgAL/SoundManager>
#include <osgAL/SoundNode>
#include <osgAL/SoundState>

// --- C/C++ Libraries --- //
#include <iostream>

using namespace ves::xplorer::scenegraph;

////////////////////////////////////////////////////////////////////////////////
Sound::Sound( ves::xplorer::scenegraph::DCS* parent, osgAL::SoundManager* soundManager )
:
m_soundManager( soundManager ),
m_soundNode( new osgAL::SoundNode() ),
m_soundState( new osgAL::SoundState() ),
m_sample( 0 ),
m_soundGeode( new osg::Geode() )
{
    parent->addChild( m_soundNode.get() );
    parent->addChild( m_soundGeode.get() );
    m_soundNode->setSoundState( m_soundState.get() );
}
////////////////////////////////////////////////////////////////////////////////
Sound::~Sound()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void Sound::Draw()
{
    //Create a drawable so we can "see" the sound
    osg::ref_ptr< osg::Drawable > drawable = m_soundGeode->getDrawable( 0 );
    if( drawable.valid() )
    {
        m_soundGeode->removeDrawable( drawable.get() );
    }

    osg::ref_ptr< osg::TessellationHints > hints = new osg::TessellationHints();
    hints->setDetailRatio( 0.5f );
    m_soundGeode->addDrawable( new osg::ShapeDrawable( new osg::Sphere( osg::Vec3( 0.0f, 0.0f, 0.0f ), 10 ), hints.get() ) );
}
////////////////////////////////////////////////////////////////////////////////
void Sound::LoadFile( std::string name )
{
    bool addToCache = true;
    m_sample = m_soundManager->getSample( name, addToCache );
    m_soundState->setName( name );

    //Create a new soundstate, give it the name of the file we loaded.
    m_soundState->setSample( m_sample.get() );

    m_soundState->setGain( 1.0f );
    m_soundState->setReferenceDistance( 60 );
    m_soundState->setRolloffFactor( 4 );
    m_soundState->setPlay( true );
    m_soundState->setLooping( true );

    //Allocate a hardware soundsource to this soundstate( priority 10 )
    m_soundState->allocateSource( 10, false );
}
////////////////////////////////////////////////////////////////////////////////
osgAL::SoundNode* Sound::GetSoundNode()
{
    return m_soundNode.get();
}
////////////////////////////////////////////////////////////////////////////////

#endif // end VE_SOUND
