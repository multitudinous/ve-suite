/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2011 by Iowa State University
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
#include <cstring>

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
#if VES_USE_FBO_CAMERA
    m_fboCamera( new osg::Camera() ),
#else
    mFBO( new osg::FrameBufferObject() ),
#endif
    mOutputType( TEXTURE_2D ),
    mOutputInternalFormat( GL_RGBA16F_ARB )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
UnitInOut::UnitInOut( const UnitInOut& unitInOut, const osg::CopyOp& copyop )
    :
    Unit( unitInOut, copyop ),
#if VES_USE_FBO_CAMERA
    m_fboCamera( unitInOut.m_fboCamera ),
#else
    mFBO( unitInOut.mFBO ),
#endif
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
    mGeode->setName( "unitinout" );
    AssignOutputTexture();
    AssignFBO();
}
////////////////////////////////////////////////////////////////////////////////
/*osg::FrameBufferObject* UnitInOut::GetFrameBufferObject()
{
    return mFBO.get();
}*/
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
    for( TextureMap::iterator itr = mOutputTextures.begin(); 
        itr != mOutputTextures.end(); ++itr )
    {
        /*if( itr->second.valid() )
        {
            itr->second->setInternalFormat( mOutputInternalFormat );
            itr->second->setSourceFormat(
                CreateSourceTextureFormat( mOutputInternalFormat ) );
        }*/
        osg::Texture* texture = itr->get();
        if( texture )
        {
            texture->setInternalFormat( mOutputInternalFormat );
            texture->setSourceFormat( CreateSourceTextureFormat( mOutputInternalFormat ) );
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
GLenum UnitInOut::GetOutputInternalFormat() const
{
    return mOutputInternalFormat;
}
////////////////////////////////////////////////////////////////////////////////
/*void UnitInOut::SetOutputTexture( osg::Texture* outputTexture, int mrt )
{
    if( outputTexture )
    {
        mOutputTextures.push_back( outputTexture );
        //mOutputTextures[ mrt ] = outputTexture;
    }
}*/
////////////////////////////////////////////////////////////////////////////////
osg::Texture* UnitInOut::CreateOutputTexture( size_t mrt )
{
    //If already exists, then return back
    osg::Texture* newTexture( 0 );
    /*if( mOutputTextures.find( mrt ) !=  mOutputTextures.end() )
    {
        newTexture = mOutputTextures[ mrt ].get();

        return newTexture;
    }*/
    if( mrt < mOutputTextures.size() )
    {
        return mOutputTextures.at( mrt ).get();
    }

    //If not exists, then do allocate it
    if( mOutputType == TEXTURE_2D )
    {
        newTexture = new osg::Texture2D();
        //dynamic_cast< osg::Texture2D* >( newTexture )->setSubloadCallback(
        //    new Subload2DCallback() );
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
    //newTexture->setResizeNonPowerOfTwoHint( false );
    newTexture->setWrap( osg::Texture::WRAP_S, osg::Texture::CLAMP_TO_EDGE );
    newTexture->setWrap( osg::Texture::WRAP_T, osg::Texture::CLAMP_TO_EDGE );
    //newTexture->setWrap( osg::Texture::WRAP_R, osg::Texture::CLAMP_TO_EDGE );
    newTexture->setBorderColor( osg::Vec4d( 0.0, 0.0, 0.0, 0.0 ) );
    newTexture->setInternalFormat( GetOutputInternalFormat() );
    newTexture->setSourceFormat(
        CreateSourceTextureFormat( GetOutputInternalFormat() ) );
    newTexture->setSourceType( GL_UNSIGNED_BYTE );

    //newTexture->setSourceType(
    //    osg::Image::computeFormatDataType( GetOutputInternalFormat() ) );

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
    //mOutputTextures[ mrt ] = newTexture;
    mOutputTextures.push_back( newTexture );

    return newTexture;
}
////////////////////////////////////////////////////////////////////////////////
void UnitInOut::SetOutputTextureMap( const TextureMap& textureMap )
{
    mOutputTextures = textureMap;
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
#if !VES_USE_FBO_CAMERA
            mFBO->setAttachment( osg::Camera::BufferComponent(
                osg::Camera::COLOR_BUFFER0 ),
                osg::FrameBufferAttachment( texture2D ) );
#else
            m_fboCamera->setRenderOrder( osg::Camera::POST_RENDER );
            m_fboCamera->setReferenceFrame( osg::Camera::ABSOLUTE_RF );
            m_fboCamera->setViewMatrix( osg::Matrix::identity() );
            m_fboCamera->setProjectionMatrix(
                osg::Matrix::ortho( 0.0, 1.0, 0.0, 1.0, 0.0, 1.0 ) );
            m_fboCamera->setClearMask( GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT | GL_STENCIL_BUFFER_BIT );
            m_fboCamera->setRenderTargetImplementation( osg::Camera::FRAME_BUFFER_OBJECT );
            m_fboCamera->setClearColor( osg::Vec4( 0.0, 1.0, 1.0, 0.0 ) );
            m_fboCamera->attach( osg::Camera::COLOR_BUFFER0, texture2D );
            m_fboCamera->setViewport( mViewport );
            //m_fboCamera->setComputeNearFarMode(
            //  osg::CullSettings::COMPUTE_NEAR_FAR_USING_BOUNDING_VOLUMES );
            m_fboCamera->addChild( mGeode.get() );
#endif
            return;
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
#if !VES_USE_FBO_CAMERA
    getOrCreateStateSet()->setAttribute(
        mFBO.get(),
        osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
#else
    addChild( m_fboCamera.get() );
#endif
}
////////////////////////////////////////////////////////////////////////////////
