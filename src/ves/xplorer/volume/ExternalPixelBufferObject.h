/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2008 by Iowa State University
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

#ifndef PIXEL_BUFFER_OBJECT_H
#define PIXEL_BUFFER_OBJECT_H

#include <ves/VEConfig.h>
#include <osg/BufferObject>
namespace osg
{
    class Image;
    class State;
}
namespace ves
{
namespace xplorer
{
namespace volume
{
class  VE_TEXTURE_BASED_EXPORTS ExternalPixelBufferObject : public osg::BufferObject
{
    public:

        ExternalPixelBufferObject(osg::Image* image=0);

        /** Copy constructor using CopyOp to manage deep vs shallow copy.*/
        ExternalPixelBufferObject( const ExternalPixelBufferObject& pbo,
                           const osg::CopyOp& copyop=osg::CopyOp::SHALLOW_COPY);

        META_Object(osg,ExternalPixelBufferObject);

        typedef std::pair< osg::BufferObject::BufferEntry, osg::Image* > BufferEntryImagePair;

        void setImage(osg::Image* image);
        void UpdateData( unsigned char* data );

        osg::Image* getImage() { return _bufferEntryImagePair.second; }
        const osg::Image* getImage() const { return _bufferEntryImagePair.second; }

        unsigned int offset() const { return _bufferEntryImagePair.first.offset; }

        virtual void compileBuffer(osg::State& state) const;

        /** Resize any per context GLObject buffers to specified size. */
        virtual void resizeGLObjectBuffers(unsigned int maxSize);

    protected:
        unsigned char* m_data;
        bool m_useExternalData;

        virtual ~ExternalPixelBufferObject();

        BufferEntryImagePair _bufferEntryImagePair;
};
}
}
}
#endif //PIXEL_BUFFER_OBJECT_H

