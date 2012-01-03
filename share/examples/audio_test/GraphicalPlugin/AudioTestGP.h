/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2012 by Iowa State University
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
#ifndef AUDIO_TEST_GP_H
#define AUDIO_TEST_GP_H

// --- VE-Suite Includes --- //
#include <ves/xplorer/plugin/PluginBase.h>

#include <string>

namespace osgAudio
{
class SoundState;
}

namespace audio
{
class VE_USER_PLUGIN_EXPORTS AudioTestGP :
    public ves::xplorer::plugin::PluginBase
{
public:
    ///Constructor
    AudioTestGP();
    ///Destructor
    virtual ~AudioTestGP();

    ///Add all of the data to the scenegraph
    virtual void InitializeNode( osg::Group* veworldDCS );
    ///Remove this plugin from xplorer and the sg
    virtual void RemoveSelfFromSG();

protected:

private:
    ///Create the animation paths for the planes
    osg::AnimationPath* createAnimationPath(const osg::Vec3& center,float radius,double looptime);
    ///Create the base plane for the planes
    osg::Node* createBase(const osg::Vec3& center,float radius);
    ///Create the moving planes
    osg::Node* createMovingModel(const osg::Vec3& center, float radius);
    ///Create the sound state for the planes
    osgAudio::SoundState* createSoundState(const std::string& file);
    ///Create the base group for the planes
    osg::Node* createModel();
};

CREATE_VES_XPLORER_PLUGIN_ENTRY_POINT( AudioTestGP )

} //end audio

#endif //AUDIO_TEST_GP_H
