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

#ifndef __SOUND_TABLE_H__
#define __SOUND_TABLE_H__ 1


#include <osgAudio/SoundManager.h>
#include <osgAudio/Sample.h>

#include <string>
#include <map>


struct SoundData;

template< class T >
class SoundTable {
public:
    SoundTable();
    ~SoundTable();

    void setDefaultSound( std::string& soundFile );

    // 2D table
    void addSound( const T& mat0, const T& mat1, std::string& soundFile );
    osgAudio::Sample* getSound( const T& mat0, const T& mat1 );

    // 1D map
    void addSound( const T& mat, std::string& soundFile );
    osgAudio::Sample* getSound( const T& mat );

protected:
    typedef std::map< T, SoundData > SoundMap;
    typedef std::map< T, SoundMap > Table;
    //typedef std::map< T, SoundMap >::iterator TableConstIter;
    
    Table _table;
    SoundMap _map;

    osg::ref_ptr< osgAudio::Sample > _defaultSample;
};


struct SoundData {
    SoundData();
    ~SoundData();

    SoundData& operator=( const SoundData& rhs );

    bool _default;

    std::string _fileName;
    osg::ref_ptr< osgAudio::Sample > _sample;
};


template< class T >
SoundTable< T >::SoundTable()
{
    std::string defaultFileName( "a.wav" );
    setDefaultSound( defaultFileName );
}
template< class T >
SoundTable< T >::~SoundTable()
{
}


template< class T > void
SoundTable< T >::setDefaultSound( std::string& soundFile )
{
    const bool addToCache( true );
    _defaultSample = osgAudio::SoundManager::instance()->getSample( soundFile, addToCache );
}


template< class T > void
SoundTable< T >::addSound( const T& mat0, const T& mat1, std::string& soundFile )
{
    const bool addToCache( true );
    osg::ref_ptr< osgAudio::Sample > sample = 
        osgAudio::SoundManager::instance()->getSample( soundFile, addToCache );

    SoundData& sd0( _table[ mat0 ][ mat1 ] );
    sd0._default = false;
    sd0._fileName = soundFile;
    sd0._sample = sample.get();

    // Mirror
    SoundData& sd1( _table[ mat1 ][ mat0 ] );
    sd1._default = false;
    sd1._fileName = soundFile;
    sd1._sample = sample.get();
}

template< class T > osgAudio::Sample*
SoundTable< T >::getSound( const T& mat0, const T& mat1 )
{
    /*SoundData* sd = 0;
    if( _table.find( mat0 ) != _table.end() )
    {
        if( _table[ mat0 ].find( mat1 ) != _table[ mat0 ].end() )
        {
            sd = &(_table[ mat0 ][ mat1 ]);
        }
    }*/
    SoundData& sd( _table[ mat0 ][ mat1 ] );
    if( !sd._sample.valid() )
    {
        if( !_defaultSample.valid() )
        {
            return 0;
        }
        
        //osg::ref_ptr< osgAudio::Sample > sample = new osgAudio::Sample( *_defaultSample.get() );
        //return sample.release();
        return( _defaultSample.get() );
    }
    else
    {
        //osg::ref_ptr< osgAudio::Sample > sample = new osgAudio::Sample( *sd._sample.get() );
        //return sample.release();
        return( sd._sample.get() );
    }
}


template< class T > void
SoundTable< T >::addSound( const T& mat, std::string& soundFile )
{
    const bool addToCache( true );
    osg::ref_ptr< osgAudio::Sample > sample(
        osgAudio::SoundManager::instance()->getSample( soundFile, addToCache ) );

    SoundData& sd( _map[ mat ] );
    sd._default = false;
    sd._fileName = soundFile;
    sd._sample = sample;
}

template< class T > osgAudio::Sample*
SoundTable< T >::getSound( const T& mat )
{
    if( _map.find( mat ) == _map.end() )
        return( _defaultSample.get() );
    else
        return( _map[ mat ]._sample.get() );
}



// __SOUND_TABLE_H__
#endif
