/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2011 by Iowa State University
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
#include <ves/xplorer/event/environment/EnvironmentSlots.h>

#include <ves/xplorer/scenegraph/physics/PhysicsSimulator.h>
#include <ves/xplorer/scenegraph/HeadsUpDisplay.h>
#include <ves/xplorer/scenegraph/SceneManager.h>

#include <ves/xplorer/EnvironmentHandler.h>

#ifdef VE_SOUND
// --- osgAL Includes --- //
#include <osgAudio/SoundManager.h>
#include <osgAudio/SoundRoot.h>
#include <osgAudio/SoundNode.h>
#include <osgAudio/SoundState.h>
#endif //VE_SOUND

#include <osgDB/FileUtils>

namespace ves
{
namespace xplorer
{
namespace event
{
namespace environment
{
////////////////////////////////////////////////////////////////////////////////
void EnablePhysicsDebugging( bool const& enable )
{
    ves::xplorer::scenegraph::PhysicsSimulator::instance()->SetDebuggingOn( enable );
}
////////////////////////////////////////////////////////////////////////////////
void DisplayFrameRate( bool const& display )
{
    ves::xplorer::EnvironmentHandler::instance()->GetHeadsUpDisplay()
            ->SetFrameRateFlag( display );
}
////////////////////////////////////////////////////////////////////////////////
void DisplayCoordinateSystem( bool const& display )
{
    ves::xplorer::EnvironmentHandler::instance()->GetHeadsUpDisplay()
            ->SetCoordSysFlag( display );
}
////////////////////////////////////////////////////////////////////////////////
void SetAmbientAudioFile( std::string const& filename )
{
#ifdef VE_SOUND
    // Create a sample, load a .wav file.
    osg::ref_ptr< osgAudio::SoundState > sound_state = 
        osgAudio::SoundManager::instance()->findSoundState( filename );
    if( !sound_state.valid() )
    {
        sound_state = new osgAudio::SoundState( filename );
        ///Priority 10
        sound_state->allocateSource( 10 );
        ///Add the sound sample
        sound_state->
            setSample( new osgAudio::Sample( osgDB::findDataFile( filename ) ) );
        //sound_state->setGain(0.7f);
        //sound_state->setReferenceDistance(10);
        // Make it an ambient (heard everywhere) sound
        sound_state->setAmbient( true );
        // Loop the sound forever
        sound_state->setLooping( true );
        // Start playing the music!
        sound_state->setPlay( true );
        
        osgAudio::SoundManager::instance()->addSoundState(sound_state.get());
    }
#else
    std::cout << "VE-Suite is not compiled with audio support " 
        << filename << "." << std::endl;
#endif
}
////////////////////////////////////////////////////////////////////////////////
void UpdateBackgroundColor( bool const, std::vector< double > const& color )
{
    ves::xplorer::scenegraph::SceneManager::instance()->SetBackgroundColor( color );
    ves::xplorer::EnvironmentHandler::instance()->
        GetHeadsUpDisplay()->SetTextColor( color );
}
////////////////////////////////////////////////////////////////////////////////
}
}
}
}
