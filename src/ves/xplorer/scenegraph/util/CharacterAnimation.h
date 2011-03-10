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

class CharacterAnimation 
{
private:
    ///Constructor
    CharacterAnimation();
    ///Destructor
    ~CharacterAnimation();
    
public:
    ///???
    static CharacterAnimation& instance() 
    {
        static CharacterAnimation avmc;
        return avmc;
    }

    ///???
    static bool setModel(osgAnimation::BasicAnimationManager* model) 
    {
        CharacterAnimation& self = instance();
        self._model = model;
        for( osgAnimation::AnimationList::const_iterator it = 
            self._model->getAnimationList().begin(); 
            it != self._model->getAnimationList().end(); ++it )
        {
            self._map[(*it)->getName()] = *it;
        }

        for( osgAnimation::AnimationMap::iterator it = self._map.begin(); 
            it != self._map.end(); ++it )
        {
            self.m_amv.push_back(it->first);
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
    bool playByName(const std::string& name);

    ///???
    const std::string& getCurrentAnimationName() const;

    ///???
    const std::vector< std::string >& getAnimationMap() const;

    ///???
    void Register( std::string fileName );

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
