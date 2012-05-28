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

#ifndef __SOUND_UTILITIES_H__
#define __SOUND_UTILITIES_H__ 1


#include <osgAudio/SoundState.h>

#include "Material.h"
#include "SoundTable.h"


class SoundUtilities
{
public:
    static SoundUtilities* instance();
    void shutdown( osg::Node* root );
    ~SoundUtilities();

    // Play the soundFile or sample at the given position.
    void playSound( const osg::Vec3& pos, const std::string& soundFile, float gain = 1.f );
    void playSound( const osg::Vec3& pos, osgAudio::Sample* sample, float gain = 1.f );

    // Collision between two materials.
    void collide( const Material::MaterialType& matA, const Material::MaterialType& matB, const osg::Vec3& pos, float gain = 1.f );

    // One material sliding against the other.
    void slide( const Material::MaterialType& matA, const Material::MaterialType& matB, const osg::Vec3& pos );

    // One material moving.
    void move( const Material::MaterialType& mat, const osg::Vec3& pos );


    // Add a sound to a Node. Sound will loop.
    void addSound( osg::Node* node, const std::string& soundFile, float gain = 1.f );
    void addSound( osg::Node* node, osgAudio::Sample* sample, float gain = 1.f );

    // Remove a sound from a Node.
    bool removeSound( osg::Node* node );

    // Specify an ambient sound.
    void setAmbient( const std::string& soundFile, float gain = 1.f );
    void setAmbient( bool ambientEnabled );
    bool getAmbient() const;

protected:
    SoundUtilities();

    static SoundUtilities* _s_instance;

    void init();

    // For playing one-time sounds.
    osg::ref_ptr< osgAudio::SoundState > _soundState;


    // 2D tables to look up sounds by two materials, for colliding or sliding objects.
    SoundTable< Material::MaterialType > _collideTable;
    SoundTable< Material::MaterialType > _slideTable;

    // 1D map to look up sounds by one material, for moving objects.
    SoundTable< Material::MaterialType > _moveTable;
};


// __SOUND_UTILITIES_H__
#endif
