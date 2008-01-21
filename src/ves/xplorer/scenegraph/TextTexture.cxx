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
#ifdef _OSG
#include <ves/xplorer/scenegraph/TextTexture.h>
#include <osgText/Text>
#include <osg/CameraNode>
#include <osg/Texture2D>

using namespace ves::xplorer::scenegraph;

///////////////////////////////////////////////////////////////////////
TextTexture::TextTexture( unsigned int textureResolutionX,
                          unsigned int textureResolutionY,
                          std::string fontFile )
{
    _font = fontFile;
    _textureResolution[0] = textureResolutionX;
    _textureResolution[1] = textureResolutionY;
    //_ttUpdateCallback = new TextUpdateCallback(this);

    _textColor[0] = 0.f;
    _textColor[1] = 0.f;
    _textColor[2] = 0.f;
    _textColor[3] = 1.f;

    _text = new osgText::Text();
    _text->setAlignment( osgText::Text::CENTER_CENTER );
    _text->setFont( _font );
    _text->setColor( osg::Vec4( _textColor[0],
                                _textColor[1],
                                _textColor[2],
                                _textColor[3] ) );
    _text->setCharacterSize( 10.0 );
    _text->setLayout( osgText::Text::LEFT_TO_RIGHT );

    ///this may need adjusting
    _fbo = new osg::CameraNode();
    _fbo->setClearColor( osg::Vec4( 0.1f, 0.1f, 0.3f, 1.0f ) );
    _fbo->setClearMask( GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT );

    _texture = new osg::Texture2D();
    _texture->setTextureSize( _textureResolution[0], _textureResolution[1] );
    _texture->setInternalFormat( GL_RGBA );
    _texture->setFilter( osg::Texture2D::MIN_FILTER, osg::Texture2D::LINEAR );
    _texture->setFilter( osg::Texture2D::MAG_FILTER, osg::Texture2D::LINEAR );
    _fboInitialized = false;
}
/////////////////////////////////////////////////////
TextTexture::TextTexture( const TextTexture& ttexture,
                          const osg::CopyOp& copyop )
        :
        ves::xplorer::scenegraph::Geode( ttexture, copyop )
{
    _font = ttexture._font;
    _textureResolution[0] = ttexture._textureResolution[0];
    _textureResolution[1] = ttexture._textureResolution[1];
    //_ttUpdateCallback = ttexture._ttUpdateCallback;
    _fbo = new osg::CameraNode( *ttexture._fbo );
    _texture = new osg::Texture2D( *ttexture._texture );
    _text = new osgText::Text( *ttexture._text );
    _textColor[0] = ttexture._textColor[0];
    _textColor[1] = ttexture._textColor[1];
    _textColor[2] = ttexture._textColor[2];
    _textColor[3] = ttexture._textColor[3];
    _fboInitialized = ttexture._fboInitialized;
}
///////////////////////////
TextTexture::~TextTexture()
{}
///////////////////////////////////////////////
void TextTexture::SetFont( std::string fontFile )
{
    _font = fontFile;
}
//////////////////////////////////
void TextTexture::_initializeFBO()
{
    const osg::BoundingSphere& bs = getBound();

    if( !bs.valid() )
    {
        return;
    }

    float znear = 1.0f * bs.radius();
    float zfar  = 3.0f * bs.radius();

    // 2:1 aspect ratio as per flag geomtry below.
    float proj_top   = 0.25f * znear;
    float proj_right = 0.5f * znear;

    znear *= 0.9f;
    zfar *= 1.1f;

    // set up projection.
    _fbo->setProjectionMatrixAsFrustum( -proj_right, proj_right, -proj_top, proj_top, znear, zfar );

    // set view
    _fbo->setReferenceFrame( osg::Transform::ABSOLUTE_RF );
    _fbo->setViewMatrixAsLookAt( bs.center() - osg::Vec3( 0.0f, 2.0f, 0.0f )*bs.radius(), bs.center(), osg::Vec3( 0.0f, 0.0f, 1.0f ) );

    // set viewport
    _fbo->setViewport( 0, 0, _textureResolution[0], _textureResolution[1] );

    // set the camera to render before the main camera.
#if (OSG_VERSION_MAJOR>=2)
    _fbo->setRenderOrder( osg::Camera::PRE_RENDER );
#else
    _fbo->setRenderOrder( osg::CameraNode::PRE_RENDER );
#endif
    // tell the camera to use OpenGL frame buffer object where supported.
#if (OSG_VERSION_MAJOR>=2)
    _fbo->setRenderTargetImplementation( osg::Camera::FRAME_BUFFER_OBJECT );
#else
    _fbo->setRenderTargetImplementation( osg::CameraNode::FRAME_BUFFER_OBJECT );
#endif

#if (OSG_VERSION_MAJOR>=2)
    // attach the texture and use it as the color buffer.
    _fbo->attach( osg::Camera::COLOR_BUFFER, _texture.get() );
#else
    _fbo->attach( osg::CameraNode::COLOR_BUFFER, _texture.get() );
#endif

    // add subgraph to render
    _fbo->addChild( this );

}
/////////////////////////////////////////////////
void TextTexture::UpdateText( std::string newText )
{
    _text->setFont( _font );
    _text->setColor( osg::Vec4( _textColor[0],
                                _textColor[1],
                                _textColor[2],
                                _textColor[3] ) );
    _text->setText( newText );
    _text->dirtyBound();
    _text->computeBound();
    if( !_fboInitialized )
    {
        addDrawable( _text.get() );
        _initializeFBO();
        _text->setAutoRotateToScreen( true );
        this->_fboInitialized = true;
    }
    dirtyBound();
    computeBound();
}
/////////////////////////////////////////////
#if (OSG_VERSION_MAJOR>=2)
osg::Camera* TextTexture::GetCameraNode()
#else
osg::CameraNode* TextTexture::GetCameraNode()
#endif
{
    if( _fbo.valid() )
    {
        return _fbo.get();
    }
    return 0;
}

/////////////////////////////////////////
osg::Texture2D* TextTexture::GetTexture()
{
    if( _texture.valid() )
    {
        return _texture.get();
    }
    return 0;
}
#endif
