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

#include <ves/xplorer/volume/ExternalPixelBufferObject.h>
#include <osg/Image>
#include <osg/State>
using namespace ves::xplorer::volume;
//////////////////////////////////////////////////////////////////////////////////
//
//  ExternalPixelBufferObject
//
ExternalPixelBufferObject::ExternalPixelBufferObject(osg::Image* image):
    BufferObject()
{
    _target = GL_PIXEL_UNPACK_BUFFER_ARB;
    _usage = GL_STREAM_DRAW_ARB;
    _bufferEntryImagePair.second = image;
    m_useExternalData = false;
    m_data = 0;
}

/** Copy constructor using CopyOp to manage deep vs shallow copy.*/
ExternalPixelBufferObject::ExternalPixelBufferObject(const ExternalPixelBufferObject& buffer,
                                                     const osg::CopyOp& copyop):
osg::BufferObject(buffer,copyop),
_bufferEntryImagePair(buffer._bufferEntryImagePair)
{
}
///////////////////////////////////////////////////////
ExternalPixelBufferObject::~ExternalPixelBufferObject()
{
}
///////////////////////////////////////////////////////////
void ExternalPixelBufferObject::setImage(osg::Image* image)
{
    if (_bufferEntryImagePair.second == image) return;

    _bufferEntryImagePair.second = image;

    dirty();
}
//////////////////////////////////////////////////////////////////////
void ExternalPixelBufferObject::compileBuffer(osg::State& state) const
{
    unsigned int contextID = state.getContextID();

    _compiledList[contextID] = 1;

    osg::Image* image = _bufferEntryImagePair.second;

    _bufferEntryImagePair.first.modifiedCount[contextID] = image->getModifiedCount();
    if (!image->valid()) return;

    osg::BufferObject::Extensions* extensions = getExtensions(contextID,true);

    GLuint& pbo = buffer(contextID);
    if (pbo==0)
    {
        // building for the first time.

        _totalSize = image->getTotalSizeInBytes();

        // don't generate buffer if size is zero.
        if (_totalSize==0) return;

        extensions->glGenBuffers(1, &pbo);
        extensions->glBindBuffer(_target, pbo);
        extensions->glBufferData(_target, _totalSize, NULL, _usage);

    }
    else
    {
        extensions->glBindBuffer(_target, pbo);

        if (_totalSize != image->getTotalSizeInBytes())
        {
            // resize PBO.
            _totalSize = image->getTotalSizeInBytes();
            extensions->glBufferData(_target, _totalSize, NULL, _usage);
        }
    }

//    osg::Timer_t start_tick = osg::Timer::instance()->tick();

    void* pboMemory = extensions->glMapBuffer(_target,
                 GL_WRITE_ONLY_ARB);

    // copy data across
    if( m_useExternalData )
    {
        memcpy( pboMemory, m_data, _totalSize );
        _bufferEntryImagePair.first.modifiedCount[contextID] = 1;
    }
    else
    {
        memcpy(pboMemory, image->data(), _totalSize);
        _bufferEntryImagePair.first.modifiedCount[contextID] = image->getModifiedCount();
    }

    // Unmap the texture image buffer
    extensions->glUnmapBuffer(_target);


//    osg::notify(osg::NOTICE)<<"pbo _totalSize="<<_totalSize<<std::endl;
//    osg::notify(osg::NOTICE)<<"pbo "<<osg::Timer::instance()->delta_m(start_tick,osg::Timer::instance()->tick())<<"ms"<<std::endl;
}
///////////////////////////////////////////////////////////////////////////
void ExternalPixelBufferObject::resizeGLObjectBuffers(unsigned int maxSize)
{
    osg::BufferObject::resizeGLObjectBuffers(maxSize);

    _bufferEntryImagePair.first.modifiedCount.resize(maxSize);
}
/////////////////////////////////////////////////////////////////
void ExternalPixelBufferObject::UpdateData( unsigned char* data )
{
    m_data = data;
    m_useExternalData = true;
    dirty();
}
