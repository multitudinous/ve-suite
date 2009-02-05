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

#ifndef UNIT_IN_OUT_H
#define UNIT_IN_OUT_H

// --- VE-Suite Includes --- //
#include <ves/VEConfig.h>

#include <ves/xplorer/scenegraph/rtt/Unit.h>

// --- OSG Includes --- //
namespace osg
{
class Texture;
class FrameBufferObject;
}

namespace ves
{
namespace xplorer
{
namespace scenegraph
{
namespace rtt
{
class VE_SCENEGRAPH_EXPORTS UnitInOut : public Unit
{
public:    
    ///Constructor
    UnitInOut();

    ///Copy Constructor
    UnitInOut(
        const UnitInOut& unitInOut,
        const osg::CopyOp& copyop = osg::CopyOp::SHALLOW_COPY );

    ///
    META_Node( rtt, UnitInOut );

    ///Initialze the default Processing unit
    virtual void Initialize();

    ///Get framebuffer object used by this ppu
    ///\return
    osg::FrameBufferObject* GetFrameBufferObject();

    ///The types can be used to specify the type of the output texture
    enum TextureType
    {
        //Texture is a osg::Texture2D
        TEXTURE_2D,

        //Texture is a osg::TextureCubeMap
        TEXTURE_CUBEMAP,

        //3D texture is used of the output
        TEXTURE_3D
    };

    ///Specify the type of the output texture
    ///\param textureType
    void SetOutputTextureType( TextureType textureType );

    ///Get the type of the output texture
    ///\return
    TextureType GetOutputTextureType() const;

    ///Set internal format which will be used by creating the textures
    ///\param format
    void SetOutputInternalFormat( GLenum format );

    ///Get internal format which is used by the output textures
    ///\return
    GLenum GetOutputInternalFormat() const;

    ///Set an output texture
    ///\param outputTexture Texture used as output of this ppu 
    ///\param mrt MRT (multiple rendering target) index of this output
    void SetOutputTexture( osg::Texture* outputTexture, int mrt = 0 );

    ///Return or create output texture for the specified MRT index
    ///\param mrt
    ///\return
    virtual osg::Texture* GetOrCreateOutputTexture( int mrt = 0 );

    ///Set a mrt to texture map for output textures
    void SetOutputTextureMap( const TextureMap& textureMap );

protected:
    ///Destructor
    virtual ~UnitInOut();

    ///Viewport changed
    virtual void NoticeChangeViewport();

    ///
    virtual void AssignOutputTexture();

    ///
    virtual void AssignFBO();

private:
    ///Framebuffer object where results are written
    osg::ref_ptr< osg::FrameBufferObject > mFBO;

    ///Output texture type
    TextureType mOutputType;

    ///Internal format of the output texture
    GLenum mOutputInternalFormat;

};
} //end rtt
} //end scenegraph
} //end xplorer
} //end ves

#endif //UNIT_IN_OUT_H
