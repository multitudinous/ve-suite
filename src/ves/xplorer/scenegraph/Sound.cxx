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

namespace ves
{
namespace xplorer
{
namespace scenegraph
{

////////////////////////////////////////////////////////////////////////////////
Sound::Sound( const std::string& name,
              ves::xplorer::scenegraph::DCS* parentDCS )
        :
        m_soundManager( osgAL::SoundManager::instance() ),
        m_dcs( new ves::xplorer::scenegraph::DCS() ),
        m_soundGeode( new osg::Geode() ),
        m_sample( 0 ),
        m_soundState( new osgAL::SoundState( name ) ),
        m_soundNode( new osgAL::SoundNode( m_soundState.get() ) )
{
    parentDCS->AddChild( m_dcs.get() );
    m_dcs->addChild( m_soundNode.get() );
    m_dcs->addChild( m_soundGeode.get() );
}
////////////////////////////////////////////////////////////////////////////////
Sound::Sound( const std::string& name,
              ves::xplorer::scenegraph::DCS* parentDCS,
              osgAL::SoundManager* soundManager )
        :
        m_soundManager( soundManager ),
        m_dcs( new ves::xplorer::scenegraph::DCS() ),
        m_soundGeode( new osg::Geode() ),
        m_sample( 0 ),
        m_soundState( new osgAL::SoundState( name, m_soundManager ) ),
        m_soundNode( new osgAL::SoundNode( m_soundState.get(), m_soundManager ) )
{
    parentDCS->AddChild( m_dcs.get() );
    m_dcs->addChild( m_soundNode.get() );
    m_dcs->addChild( m_soundGeode.get() );
}
////////////////////////////////////////////////////////////////////////////////
Sound::~Sound()
{
    //m_soundManager->removeSoundState( m_soundState.get() );
    m_soundState->releaseSource();
}
////////////////////////////////////////////////////////////////////////////////
Sound &Sound::operator=( const Sound &sound )
{
    if( &sound != this )
    {
        m_soundManager = sound.m_soundManager;
        m_dcs = sound.m_dcs;
        m_soundGeode = sound.m_soundGeode;
        m_sample = sound.m_sample;
        m_soundState = sound.m_soundState;
        m_soundNode = sound.m_soundNode;
    }

    return *this;
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
    m_soundGeode->addDrawable( new osg::ShapeDrawable( 
        new osg::Sphere( osg::Vec3( 0.0f, 0.0f, 0.0f ), 10 ), hints.get() ) );
}
////////////////////////////////////////////////////////////////////////////////
void Sound::LoadFile( const std::string fileName )
{
    m_fileName = fileName;
    bool addToCache = false;

    //m_sample = m_soundManager->getSample( fileName, addToCache );
    m_sample = new openalpp::Sample( fileName );

    //Create a new soundstate, give it the name of the file we loaded.
    m_soundState->setSample( m_sample.get() );

    m_soundState->setGain( 1.0f );
    m_soundState->setPitch( 1.0f );
    m_soundState->setSoundCone( 0.0, 360.0, 1.0 );
    m_soundState->setReferenceDistance( 5 );
    m_soundState->setRolloffFactor( 4 );
    m_soundState->setPlay( true );
    m_soundState->setLooping( false );

    //Allocate a hardware soundsource to this soundstate( priority 10 )
    m_soundState->allocateSource( 10, false );

    m_soundManager->addSoundState( m_soundState.get() );
    //m_soundManager->pushSoundEvent( m_soundState.get(), 10 );

    m_soundState->apply();
}
////////////////////////////////////////////////////////////////////////////////
void Sound::PushSoundEvent( int priority )
{
    osg::ref_ptr< osgAL::SoundState > temp = 
        new osgAL::SoundState( "temp", m_soundManager );
    temp->setSample( m_sample.get() );
    temp->setPosition( m_soundState->getPosition() );
    temp->setSoundCone( 0.0, 360.0, 1.0 );
    temp->setReferenceDistance( 5 );
    temp->setPlay( true );
    temp->setLooping( false );

    m_soundManager->pushSoundEvent( temp.get(), priority );
}
////////////////////////////////////////////////////////////////////////////////
void Sound::Pause()
{
    m_soundState->setStopMethod( openalpp::Paused );
    m_soundState->setPlay( false );
    m_soundState->setGain( 0.0 );
}
////////////////////////////////////////////////////////////////////////////////
void Sound::Play()
{
    m_soundState->setPlay( true );
    m_soundState->setGain( 1.0 );
}
////////////////////////////////////////////////////////////////////////////////
void Sound::Stop()
{
    m_soundState->setStopMethod( openalpp::Stopped );
    m_soundState->setPlay( false );
}
////////////////////////////////////////////////////////////////////////////////
osgAL::SoundState* Sound::GetSoundState()
{
    return m_soundState.get();
}
////////////////////////////////////////////////////////////////////////////////

} // end scenegraph
} // end xplorer
} // end ves

#endif // end VE_SOUND
