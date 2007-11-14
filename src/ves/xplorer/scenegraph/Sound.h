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
 * Date modified: $Date: 2007-09-24 15:35:46 -0500 (Mon, 24 Sep 2007) $
 * Version:       $Rev: 9142 $
 * Author:        $Author: jbkoch $
 * Id:            $Id: Sound.h 9142 2007-09-24 20:35:46Z jbkoch $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef SOUND_H
#define SOUND_H

/*!\file Sound.h
*/

/*!\class ves::xplorer::scenegraph::Sound
* 
*/

/*!\namespace ves::xplorer::scenegraph
*
*/

#ifdef VE_SOUND

// --- VE-Suite Includes --- //
#include <ves/VEConfig.h>

// --- OSG Includes --- //
#include <osg/ref_ptr>

namespace osgAL
{
    class SoundNode;
    class SoundState;
}

namespace openalpp
{
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
class VE_SCENEGRAPH_EXPORTS Sound
{
public:
    Sound();
    ~Sound();

    void Draw();
    void LoadFile( std::string name );
    osgAL::SoundNode* GetSoundNode();
    
private:
    //osg::ref_ptr< osg::Geode > m_soundGeode;
    osg::ref_ptr< osgAL::SoundNode > m_soundNode;
    osg::ref_ptr< osgAL::SoundState > m_soundState;
    osg::ref_ptr< openalpp::Sample > m_sample;

};
}
}
}

#endif //VE_SOUND

#endif //SOUND_H