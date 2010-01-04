/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2009 by Iowa State University
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
#include <ves/xplorer/volume/ExternalPixelBufferObject.h>

// --- OSG Includes --- //
#include <osg/Image>
#include <osg/State>

using namespace ves::xplorer::volume;

////////////////////////////////////////////////////////////////////////////////
ExternalPixelBufferObject::ExternalPixelBufferObject( osg::Image* image )
    :
    osg::BufferObject()
{
#if ( ( OSG_VERSION_MAJOR >= 2 ) && ( OSG_VERSION_MINOR >= 9 ) && ( OSG_VERSION_PATCH >= 6 ) )
    setTarget( GL_PIXEL_UNPACK_BUFFER_ARB );
    setUsage( GL_STREAM_DRAW_ARB );

    osg::notify( osg::INFO )
        << "Constructing PixelBufferObject for image=" << image << std::endl;

    setBufferData( 0, image );
#else
    _target = GL_PIXEL_UNPACK_BUFFER_ARB;
    _usage = GL_STREAM_DRAW_ARB;
    _bufferEntryImagePair.second = image;
#endif
    m_useExternalData = false;
    m_data = 0;
}
////////////////////////////////////////////////////////////////////////////////Í
/** Copy constructor using CopyOp to manage deep vs shallow copy.*/
ExternalPixelBufferObject::ExternalPixelBufferObject(
    const ExternalPixelBufferObject& pbo,
    const osg::CopyOp& copyop )
    :
#if !( ( OSG_VERSION_MAJOR >= 2 ) && ( OSG_VERSION_MINOR >= 9 ) && ( OSG_VERSION_PATCH >= 6 ) )
    _bufferEntryImagePair( pbo._bufferEntryImagePair ),
#endif
    osg::BufferObject( pbo, copyop )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
ExternalPixelBufferObject::~ExternalPixelBufferObject()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
#if ( ( OSG_VERSION_MAJOR >= 2 ) && ( OSG_VERSION_MINOR >= 9 ) && ( OSG_VERSION_PATCH >= 6 ) )
bool ExternalPixelBufferObject::isPBOSupported( unsigned int contextID ) const
{
    return _glBufferObjects[ contextID ]->isPBOSupported();
}
////////////////////////////////////////////////////////////////////////////////
void ExternalPixelBufferObject::unbindBuffer( unsigned int contextID ) const
{
    osg::GLBufferObject::Extensions* extensions =
        osg::GLBufferObject::getExtensions( contextID, true );

    switch( _mode[ contextID ] )
    {
    /*
    case READ:
    {
        extensions->glBindBuffer( GL_PIXEL_UNPACK_BUFFER_ARB, 0 );
        break;
    }
    case WRITE:
    {
        extensions->glBindBuffer( GL_PIXEL_PACK_BUFFER_ARB, 0 );
        break;
    }
    */
    default:
    {
        extensions->glBindBuffer( _profile._target, 0 );
        break;
    }
    }

    _mode[ contextID ] = 0;
}
////////////////////////////////////////////////////////////////////////////////
#else
unsigned int ExternalPixelBufferObject::offset() const
{
    return _bufferEntryImagePair.first.offset;
}
#endif
////////////////////////////////////////////////////////////////////////////////
void ExternalPixelBufferObject::compileBuffer( osg::State& state ) const
{
#if ( ( OSG_VERSION_MAJOR >= 2 ) && ( OSG_VERSION_MINOR >= 9 ) && ( OSG_VERSION_PATCH >= 6 ) )
    unsigned int contextID = state.getContextID();
    if( _profile._size == 0 )
    {
        return;
    }

    osg::GLBufferObject* bo = getOrCreateGLBufferObject( contextID );
    if( !bo || !bo->isDirty() )
    {
        return;
    }
    else
    {
         bo->bindBuffer();
    }

    bo->_extensions->glBindBuffer( _profile._target, bo->getGLObjectID() );
    bo->_extensions->glBufferData(
        _profile._target, _profile._size, NULL, _profile._usage );
    bo->_extensions->glBindBuffer( _profile._target, 0 );
#else
    unsigned int contextID = state.getContextID();

    _compiledList[ contextID ] = 1;

    osg::Image* image = _bufferEntryImagePair.second;

    _bufferEntryImagePair.first.modifiedCount[ contextID ] =
        image->getModifiedCount();
    if( !image->valid() )
    {
        return;
    }
    
    osg::BufferObject::Extensions* extensions = getExtensions(contextID,true);

    GLuint& pbo = buffer( contextID );
    if( pbo == 0 )
    {
        //Building for the first time
        _totalSize = image->getTotalSizeInBytes();

        //Don't generate buffer if size is zero
        if( _totalSize == 0 )
        {
            return;
        }

        extensions->glGenBuffers( 1, &pbo );
        extensions->glBindBuffer( _target, pbo );
        extensions->glBufferData( _target, _totalSize, NULL, _usage );
    }
    else
    {
        extensions->glBindBuffer( _target, pbo );

        if( _totalSize != image->getTotalSizeInBytes() )
        {
            //Resize PBO
            _totalSize = image->getTotalSizeInBytes();
            extensions->glBufferData( _target, _totalSize, NULL, _usage );
        }
    }

    //osg::Timer_t start_tick = osg::Timer::instance()->tick();

    void* pboMemory = extensions->glMapBuffer( _target, GL_WRITE_ONLY_ARB );

    //Copy data across
    if( m_useExternalData )
    {
        memcpy( pboMemory, m_data, _totalSize );
        _bufferEntryImagePair.first.modifiedCount[ contextID ] = 1;
    }
    else
    {
        memcpy( pboMemory, image->data(), _totalSize );
        _bufferEntryImagePair.first.modifiedCount[ contextID ] =
            image->getModifiedCount();
    }

    //Unmap the texture image buffer
    extensions->glUnmapBuffer( _target );

    //osg::notify( osg::NOTICE )
        //<< "pbo _totalSize = " << _totalSize << std::endl;
    //osg::notify( osg::NOTICE )
        //<< "pbo " << osg::Timer::instance()->delta_m(
            //start_tick, osg::Timer::instance()->tick() ) << "ms" << std::endl;
#endif
}
////////////////////////////////////////////////////////////////////////////////
void ExternalPixelBufferObject::resizeGLObjectBuffers( unsigned int maxSize )
{
#if ( ( OSG_VERSION_MAJOR >= 2 ) && ( OSG_VERSION_MINOR >= 9 ) && ( OSG_VERSION_PATCH >= 6 ) )
    osg::BufferObject::resizeGLObjectBuffers( maxSize );

    _mode.resize( maxSize );
#else
    osg::BufferObject::resizeGLObjectBuffers( maxSize );
    _bufferEntryImagePair.first.modifiedCount.resize( maxSize );
#endif
}
////////////////////////////////////////////////////////////////////////////////
void ExternalPixelBufferObject::setImage( osg::Image* image )
{
#if ( ( OSG_VERSION_MAJOR >= 2 ) && ( OSG_VERSION_MINOR >= 9 ) && ( OSG_VERSION_PATCH >= 6 ) )
    setBufferData( 0, image );
#else
    if( _bufferEntryImagePair.second == image )
    {
        return;
    }

    _bufferEntryImagePair.second = image;

    dirty();
#endif
}
////////////////////////////////////////////////////////////////////////////////
osg::Image* ExternalPixelBufferObject::getImage()
{
#if ( ( OSG_VERSION_MAJOR >= 2 ) && ( OSG_VERSION_MINOR >= 9 ) && ( OSG_VERSION_PATCH >= 6 ) )
    return dynamic_cast< osg::Image* >( getBufferData( 0 ) );
#else
    return _bufferEntryImagePair.second;
#endif
}
////////////////////////////////////////////////////////////////////////////////
const osg::Image* ExternalPixelBufferObject::getImage() const
{
#if ( ( OSG_VERSION_MAJOR >= 2 ) && ( OSG_VERSION_MINOR >= 9 ) && ( OSG_VERSION_PATCH >= 6 ) )
    return dynamic_cast< const osg::Image* >( getBufferData( 0 ) );
#else
    return _bufferEntryImagePair.second;
#endif
}
////////////////////////////////////////////////////////////////////////////////
void ExternalPixelBufferObject::UpdateData( unsigned char* data )
{
    m_data = data;
    m_useExternalData = true;
    dirty();
}
////////////////////////////////////////////////////////////////////////////////
