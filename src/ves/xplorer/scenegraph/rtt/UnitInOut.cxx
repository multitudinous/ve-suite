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
#include <ves/xplorer/scenegraph/rtt/UnitInOut.h>
#include <ves/xplorer/scenegraph/rtt/Utility.h>

// --- OSG Includes --- //
#include <osg/Geode>
#include <osg/Image>
#include <osg/Texture2D>
#include <osg/GL2Extensions>
#include <osg/FrameBufferObject>

// --- C/C++ Includes --- //

#include <iostream>

using namespace ves::xplorer::scenegraph::rtt;

////////////////////////////////////////////////////////////////////////////////
//Helper class for filling the generated texture with default pixel values
class Subload2DCallback : public osg::Texture2D::SubloadCallback
{
public:
    //Fill texture with default pixel values
    void load( const osg::Texture2D &texture, osg::State &state ) const
    {
        //Create temporary image which is initialized with 0 values
        osg::ref_ptr< osg::Image > image = new osg::Image();
        image->allocateImage(
            texture.getTextureWidth() ? texture.getTextureWidth() : 1,
            texture.getTextureHeight() ? texture.getTextureHeight() : 1,
            1,
            texture.getSourceFormat() ? texture.getSourceFormat() : texture.getInternalFormat(),
            texture.getSourceType() ? texture.getSourceType() : GL_UNSIGNED_BYTE );

        //Fill the image with 0 values
        memset( image->data(), 0, image->getTotalSizeInBytesIncludingMipmaps() * sizeof( unsigned char ) );

        //Create the texture in usual OpenGL way
        glTexImage2D( GL_TEXTURE_2D, 0, texture.getInternalFormat(),
            texture.getTextureWidth(), texture.getTextureHeight(), texture.getBorderWidth(),
            texture.getSourceFormat() ? texture.getSourceFormat() : texture.getInternalFormat(),
            texture.getSourceType() ? texture.getSourceType() : GL_UNSIGNED_BYTE,
            image->data() );
    }

    //No subload because while we want to subload the texture should be already valid
    void subload( const osg::Texture2D &texture, osg::State &state ) const
    {
        ;
    }
};

////////////////////////////////////////////////////////////////////////////////
UnitInOut::UnitInOut()
    :
    Unit(),
    mFBO( new osg::FrameBufferObject() ),
    mOutputType( TEXTURE_2D ),
    mOutputInternalFormat( GL_RGBA16F_ARB )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
UnitInOut::UnitInOut( const UnitInOut& unitInOut, const osg::CopyOp& copyop )
    :
    Unit( unitInOut, copyop ),
    mFBO( unitInOut.mFBO ),
    mOutputType( unitInOut.mOutputType ),
    mOutputInternalFormat( unitInOut.mOutputInternalFormat )
{

}
////////////////////////////////////////////////////////////////////////////////
UnitInOut::~UnitInOut()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void UnitInOut::Initialize()
{
    //Initialize default
    Unit::Initialize();

    //Setup a geode and the drawable as childs of this unit
    if( !mDrawable.valid() )
    {
        CreateTexturedQuadDrawable();
    }
    mGeode->removeDrawables( 0, mGeode->getNumDrawables() );
    mGeode->addDrawable( mDrawable.get() );
    mGeode->setCullingActive( false );
    
    AssignOutputTexture();
    AssignFBO();
}
////////////////////////////////////////////////////////////////////////////////
osg::FrameBufferObject* UnitInOut::GetFrameBufferObject()
{
    return mFBO.get();
}
////////////////////////////////////////////////////////////////////////////////
void UnitInOut::SetOutputTextureType( UnitInOut::TextureType textureType )
{
    mOutputType = textureType;
}
////////////////////////////////////////////////////////////////////////////////
UnitInOut::TextureType UnitInOut::GetOutputTextureType() const
{
    return mOutputType;
}
////////////////////////////////////////////////////////////////////////////////
void UnitInOut::SetOutputInternalFormat( GLenum format )
{
    mOutputInternalFormat = format;

    //Now generate output texture's and assign them to fbo
    TextureMap::iterator itr = mOutputTextures.begin();
    for( itr; itr != mOutputTextures.end(); ++itr )
    {
        if( itr->second.valid() )
        {
            itr->second->setInternalFormat( mOutputInternalFormat );
            itr->second->setSourceFormat(
                CreateSourceTextureFormat( mOutputInternalFormat ) );
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
GLenum UnitInOut::GetOutputInternalFormat() const
{
    return mOutputInternalFormat;
}
////////////////////////////////////////////////////////////////////////////////
void UnitInOut::SetOutputTexture( osg::Texture* outputTexture, int mrt )
{
    if( outputTexture )
    {
        mOutputTextures[ mrt ] = outputTexture;
    }
}
////////////////////////////////////////////////////////////////////////////////
osg::Texture* UnitInOut::CreateOutputTexture( int mrt )
{
    //If already exists, then return back
    osg::Texture* newTexture( 0 );
    if( mOutputTextures.find( mrt ) !=  mOutputTextures.end() )
    {
        newTexture = mOutputTextures[ mrt ].get();

        return newTexture;
    }

    //If not exists, then do allocate it
    if( mOutputType == TEXTURE_2D )
    {
        newTexture = new osg::Texture2D();
        dynamic_cast< osg::Texture2D* >( newTexture )->setSubloadCallback(
            new Subload2DCallback() );
        if( mViewport.valid() )
        {
            dynamic_cast< osg::Texture2D* >( newTexture )->setTextureSize(
                static_cast< int >( mViewport->width() ),
                static_cast< int >( mViewport->height() ) );
        }
    }
    else
    {
        osg::notify( osg::FATAL )
            << "rtt::UnitInOut::getOrCreateOutputTexture() - "
            << getName() << " non-supported texture type specified!"
            << std::endl;

        return NULL;
    }

    //Setup texture parameters
    newTexture->setResizeNonPowerOfTwoHint( false );
    newTexture->setWrap( osg::Texture::WRAP_S, osg::Texture::CLAMP_TO_EDGE );
    newTexture->setWrap( osg::Texture::WRAP_T, osg::Texture::CLAMP_TO_EDGE );
    newTexture->setWrap( osg::Texture::WRAP_R, osg::Texture::CLAMP_TO_EDGE );
    newTexture->setBorderColor( osg::Vec4d( 0.0, 0.0, 0.0, 0.0 ) );
    newTexture->setInternalFormat( GetOutputInternalFormat() );
    newTexture->setSourceFormat(
        CreateSourceTextureFormat( GetOutputInternalFormat() ) );
    //newTexture->setSourceType(
        //osg::Image::computeFormatDataType( GetOutputInternalFormat() ) );

    //Check if the input texture was in nearest mode
    if( GetInputTexture( 0 ) && GetInputTexture( 0 )->getFilter(
            osg::Texture2D::MIN_FILTER ) == osg::Texture2D::NEAREST )
    {
        newTexture->setFilter(
            osg::Texture2D::MIN_FILTER, osg::Texture2D::NEAREST );
    }
    else
    {
        newTexture->setFilter(
            osg::Texture2D::MIN_FILTER, osg::Texture2D::LINEAR );
    }

    if( GetInputTexture( 0 ) && GetInputTexture( 0 )->getFilter(
            osg::Texture2D::MAG_FILTER ) == osg::Texture2D::NEAREST )
    {
        newTexture->setFilter(
            osg::Texture2D::MAG_FILTER, osg::Texture2D::NEAREST );
    }
    else
    {
        newTexture->setFilter(
            osg::Texture2D::MAG_FILTER, osg::Texture2D::LINEAR );
    }

    //Set new texture
    mOutputTextures[ mrt ] = newTexture;

    return newTexture;
}
////////////////////////////////////////////////////////////////////////////////
void UnitInOut::SetOutputTextureMap( const TextureMap& textureMap )
{
    mOutputTextures = textureMap;
}
////////////////////////////////////////////////////////////////////////////////
void UnitInOut::NoticeChangeViewport()
{
    //Change size of the result texture according to the viewport
    TextureMap::iterator itr = mOutputTextures.begin();
    for( itr; itr != mOutputTextures.end(); ++itr )
    {
        if( itr->second.valid() )
        {
            //If texture type is a 2D texture
            if( dynamic_cast< osg::Texture2D* >( itr->second.get() ) != NULL )
            {
                //Change size
                osg::Texture2D* texture =
                    dynamic_cast< osg::Texture2D* >( itr->second.get() );
                texture->setTextureSize(
                    static_cast< int >( mViewport->width() ),
                    static_cast< int >( mViewport->height() ) );
            }
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void UnitInOut::AssignOutputTexture()
{
    //Get output texture
    osg::Texture* texture = CreateOutputTexture( 0 );

    //If the output texture is NULL, hence generate one
    if( texture )
    {
        //Check that the viewport must be valid at this time
        //Check it to set the size if texture is fresh
        if( !mViewport.valid() )
        {
            osg::notify( osg::FATAL )
                << "rtt::UnitInOut::AssignOutputTexture(): "
                << getName()
                << "cannot set output texture size - invalid viewport!"
                << std::endl;
        }

        //Check whenever the output texture is a 2D texture
        osg::Texture2D* texture2D = dynamic_cast< osg::Texture2D* >( texture );
        if( texture2D != NULL )
        {
            mFBO->setAttachment( osg::Camera::BufferComponent(
                osg::Camera::COLOR_BUFFER0 ),
                osg::FrameBufferAttachment( texture2D ) );
        }

        //Output texture type is not supported
        osg::notify( osg::FATAL )
            << "rtt::UnitInOut::assignOutputTexture(): "
            << getName()
            << " cannot attach output texture to FBO!"
            << std::endl;
    }
}
////////////////////////////////////////////////////////////////////////////////
void UnitInOut::AssignFBO()
{
    getOrCreateStateSet()->setAttribute(
        mFBO.get(),
        osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
}
////////////////////////////////////////////////////////////////////////////////
