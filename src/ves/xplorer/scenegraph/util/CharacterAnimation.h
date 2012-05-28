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


#ifndef CHARACTER_ANIMATION_H
#define CHARACTER_ANIMATION_H

#include <osg/Node>
#include <osgDB/ReadFile>
#include <osgAnimation/BasicAnimationManager>
#include <ves/VEConfig.h>

class VE_SCENEGRAPH_EXPORTS CharacterAnimation
{
public:
    ///Constructor
    CharacterAnimation();
    ///Destructor
    ~CharacterAnimation();

public:
    ///???
    /*static CharacterAnimation& instance()
    {
        static CharacterAnimation avmc;
        return avmc;
    }*/

    ///???
    bool setModel( osgAnimation::BasicAnimationManager* model )
    {
        //CharacterAnimation& self = instance();
        _model = model;
        for( osgAnimation::AnimationList::const_iterator it =
                    _model->getAnimationList().begin();
                it != _model->getAnimationList().end(); ++it )
        {
            _map[( *it )->getName()] = *it;
        }

        for( osgAnimation::AnimationMap::iterator it = _map.begin();
                it != _map.end(); ++it )
        {
            m_amv.push_back( it->first );
        }

        return true;
    }

    ///???
    bool list();

    ///???
    bool play();

    ///???
    bool stop();

    ///???
    bool next();

    ///???
    bool previous();

    ///???
    bool playByName( const std::string& name );

    ///???
    const std::string& getCurrentAnimationName() const;

    ///???
    const std::vector< std::string >& getAnimationMap() const;

    ///???
    osg::Group* Register( std::string const& fileName );

private:
    ///???
    osg::ref_ptr<osgAnimation::BasicAnimationManager> _model;
    ///???
    osgAnimation::AnimationMap _map;
    ///???
    std::vector< std::string > m_amv;
    ///???
    unsigned int _focus;
};

#endif ///CHARACTER_ANIMATION_H
