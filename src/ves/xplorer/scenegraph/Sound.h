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
#ifndef SOUND_H
#define SOUND_H

#ifdef VE_SOUND

// --- VE-Suite Includes --- //
#include <ves/VEConfig.h>

// --- OSG Includes --- //
#include <osg/ref_ptr>

namespace osg
{
class Geode;
}

namespace osgAudio
{
class SoundManager;
class SoundNode;
class SoundState;
class Sample;
}

// --- C/C++ Libraries --- //
#include <string>

namespace ves
{
namespace xplorer
{
namespace scenegraph
{
class DCS;

/*!\file Sound.h
 */

/*!\class ves::xplorer::scenegraph::Sound
 *
 */

/*!\namespace ves::xplorer::scenegraph
 *
 */
class VE_SCENEGRAPH_EXPORTS Sound
{
public:
    Sound( const std::string& name,
           ves::xplorer::scenegraph::DCS* parentDCS );

    ///Constructor for use only with multiple applications on windows.
    ///This will prevent multiple instances of the osgAL::SoundManager singleton.
    Sound( const std::string& name,
           ves::xplorer::scenegraph::DCS* parentDCS,
           osgAudio::SoundManager* soundManager );

    ~Sound();

    Sound &operator=( const Sound &sound );

    void Draw();

    void LoadFile( const std::string fileName );

    void PushSoundEvent( int priority );

    void Pause();
    void Play();
    void Stop();

    osgAudio::SoundState* GetSoundState();

private:
    osgAudio::SoundManager* m_soundManager;

    osg::ref_ptr< ves::xplorer::scenegraph::DCS > mDCS;
    osg::ref_ptr< osg::Geode > m_soundGeode;

    osg::ref_ptr< osgAudio::Sample > m_sample;
    osg::ref_ptr< osgAudio::SoundState > m_soundState;
    osg::ref_ptr< osgAudio::SoundNode > m_soundNode;

    std::string m_fileName;

};
}
}
}

#endif // end VE_SOUND

#endif // end SOUND_H
