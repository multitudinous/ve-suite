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
#include <ves/xplorer/scenegraph/rtt/Utility.h>

// --- OSG Includes --- //
#include <osg/Texture1D>
#include <osg/Texture2D>
#include <osg/Texture3D>
#include <osg/TextureCubeMap>
#include <osg/TextureRectangle>
#include <osg/Texture2DArray>

// --- C/C++ Includes --- //

namespace ves
{
namespace xplorer
{
namespace scenegraph
{
namespace rtt
{

////////////////////////////////////////////////////////////////////////////////
GLenum CreateSourceTextureFormat( GLenum internalFormat )
{
    switch( internalFormat )
    {
        case GL_LUMINANCE32F_ARB:
        case GL_LUMINANCE16F_ARB:
            return GL_LUMINANCE;

        case GL_LUMINANCE_ALPHA32F_ARB:
        case GL_LUMINANCE_ALPHA16F_ARB:
            return GL_LUMINANCE_ALPHA;

        case GL_RGB32F_ARB:
        case GL_RGB16F_ARB:
            return GL_RGB;

        case GL_RGBA32F_ARB:
        case GL_RGBA16F_ARB:
            return GL_RGBA;

        case GL_LUMINANCE32UI_EXT:
        case GL_LUMINANCE32I_EXT:
        case GL_LUMINANCE16UI_EXT:
        case GL_LUMINANCE16I_EXT:
        case GL_LUMINANCE8UI_EXT:
        case GL_LUMINANCE8I_EXT:
            return GL_LUMINANCE_INTEGER_EXT;

        case GL_LUMINANCE_ALPHA32UI_EXT:
        case GL_LUMINANCE_ALPHA32I_EXT:
        case GL_LUMINANCE_ALPHA16UI_EXT:
        case GL_LUMINANCE_ALPHA16I_EXT:
        case GL_LUMINANCE_ALPHA8UI_EXT:
        case GL_LUMINANCE_ALPHA8I_EXT:
            return GL_LUMINANCE_ALPHA_INTEGER_EXT;

        case GL_RGB32UI_EXT:
        case GL_RGB32I_EXT:
        case GL_RGB16UI_EXT:
        case GL_RGB16I_EXT:
        case GL_RGB8UI_EXT:
        case GL_RGB8I_EXT:
            return GL_RGB_INTEGER_EXT;

        case GL_RGBA32UI_EXT:
        case GL_RGBA32I_EXT:
        case GL_RGBA16UI_EXT:
        case GL_RGBA16I_EXT:
        case GL_RGBA8UI_EXT:
        case GL_RGBA8I_EXT:
            return GL_RGBA_INTEGER_EXT;

        default:
            return internalFormat;
    }
}
////////////////////////////////////////////////////////////////////////////////
osg::Uniform::Type ConvertTextureToUniformType( osg::Texture* texture )
{
    
    if( dynamic_cast< osg::Texture1D* >( texture ) )
    {
        return osg::Uniform::SAMPLER_1D;
    }
    if( dynamic_cast< osg::Texture2D* >( texture ) )
    {
        return osg::Uniform::SAMPLER_2D;
    }
    if( dynamic_cast< osg::Texture3D* >( texture ) )
    {
        return osg::Uniform::SAMPLER_3D;
    }
    if( dynamic_cast< osg::TextureCubeMap* >( texture ) )
    {
        return osg::Uniform::SAMPLER_CUBE;
    }
    if( dynamic_cast< osg::TextureRectangle* >( texture ) )
    {
        return osg::Uniform::SAMPLER_2D;
    }
    if( dynamic_cast< osg::Texture2DArray* >( texture ) )
    {
        return osg::Uniform::SAMPLER_2D_ARRAY;
    }
    
    return osg::Uniform::UNDEFINED;
}
////////////////////////////////////////////////////////////////////////////////
/*
unsigned int ComputeTextureSizeInBytes( osg::Texture* texture )
{
    if( texture == NULL )
    {
        return 0;
    }

    int width = texture->getTextureWidth();
    int height = texture->getTextureHeight();
    int depth = texture->getTextureDepth();
    if( depth == 0 )
    {
        depth = 1;
    }

    if( height == 0 )
    {
        height = 1;
    }

    GLint intFormat = texture->getInternalFormat();
    GLenum type = osg::Image::computeFormatDataType( intFormat );
    unsigned int rowWidth =
        osg::Image::computeRowWidthInBytes( width, intFormat, type, 1 );

    return rowWidth * height * depth;
}
*/
////////////////////////////////////////////////////////////////////////////////

}; //end rtt
}  //end scenegraph
}  //end xplorer
}  //end ves