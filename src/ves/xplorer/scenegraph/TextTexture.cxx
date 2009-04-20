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
#include <ves/xplorer/scenegraph/TextTexture.h>
#include <osgText/Text>
#include <osg/Camera>
#include <osg/Texture2D>
#include <osg/Group>
#include <osg/Image>
#include <osgDB/ReadFile>

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

    _textColor[0] = 1.f;
    _textColor[1] = 1.f;
    _textColor[2] = 1.f;
    _textColor[3] = 1.f;

    _text = new osgText::Text();
    _text->setAlignment( osgText::Text::CENTER_CENTER );
    _text->setFont( _font );
    _text->setColor( osg::Vec4( _textColor[0],
                                _textColor[1],
                                _textColor[2],
                                _textColor[3] ) );
    _text->setCharacterSize( 0.2f );
    _text->setLayout( osgText::Text::LEFT_TO_RIGHT );

    ///this may need adjusting
    _fbo = new osg::Camera();
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
        osg::Geode( ttexture, copyop )
{
    _font = ttexture._font;
    _textureResolution[0] = ttexture._textureResolution[0];
    _textureResolution[1] = ttexture._textureResolution[1];
    //_ttUpdateCallback = ttexture._ttUpdateCallback;
    _fbo = new osg::Camera( *ttexture._fbo );
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
    _fbo->setRenderOrder( osg::Camera::PRE_RENDER );

    // tell the camera to use OpenGL frame buffer object where supported.
    _fbo->setRenderTargetImplementation( osg::Camera::FRAME_BUFFER_OBJECT );

    // attach the texture and use it as the color buffer.
    _fbo->attach( osg::Camera::COLOR_BUFFER, _texture.get() );

    // add subgraph to render
    _fbo->addChild( m_bgTexture.get() );
    m_bgTexture->addChild( this );
}
////////////////////////////////////////////////////////////////////////////////
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
        LoadBackgroundTexture();
        _initializeFBO();
        _text->setAutoRotateToScreen( true );
        this->_fboInitialized = true;
    }
    dirtyBound();
    computeBound();
}
////////////////////////////////////////////////////////////////////////////////
osg::Camera* TextTexture::GetCameraNode()
{
    if( _fbo.valid() )
    {
        return _fbo.get();
    }
    return 0;
}
////////////////////////////////////////////////////////////////////////////////
osg::Texture2D* TextTexture::GetTexture()
{
    if( _texture.valid() )
    {
        return _texture.get();
    }
    return 0;
}
////////////////////////////////////////////////////////////////////////////////
void TextTexture::LoadBackgroundTexture()
{
    osg::ref_ptr< osg::Texture2D > rainbowTexture = new osg::Texture2D();
    rainbowTexture->setFilter( osg::Texture2D::MIN_FILTER, osg::Texture2D::LINEAR );
    rainbowTexture->setFilter( osg::Texture2D::MAG_FILTER, osg::Texture2D::LINEAR );
    rainbowTexture->setWrap( osg::Texture2D::WRAP_S, osg::Texture2D::CLAMP_TO_EDGE );
    rainbowTexture->setWrap( osg::Texture2D::WRAP_T, osg::Texture2D::CLAMP_TO_EDGE );
    rainbowTexture->setImage( osgDB::readImageFile( "Draft2.png" ) );

    m_bgTexture = new osg::Group();

    float viewportOriginX, viewportOriginY, viewportWidth, viewportHeight;
    viewportOriginX = 0.0f;
    viewportOriginY = 0.0f;
    viewportWidth = 1.0f;
    viewportHeight = 1.0f;
    
    float lx, ly, ux, uy;
    //Straight mapping from ( 0 to 1 ) viewport space to
    //                      ( 0 to 1 ) ortho projection space
    lx = viewportOriginX;
    ly = viewportOriginY;
    ux = viewportOriginX + viewportWidth;
    uy = viewportOriginY + viewportHeight;
    
    //Get the vertex coordinates for the quad
    osg::ref_ptr< osg::Vec3Array > quadVertices = new osg::Vec3Array();
    quadVertices->resize( 4 );
    
    (*quadVertices)[ 0 ].set( lx, ly, 0.0 );
    (*quadVertices)[ 1 ].set( ux, ly, 0.0 );
    (*quadVertices)[ 2 ].set( ux, uy, 0.0 );
    (*quadVertices)[ 3 ].set( lx, uy, 0.0 );
    
    //Get the texture coordinates for the quad
    osg::ref_ptr< osg::Vec2Array > quadTexCoords = new osg::Vec2Array();
    quadTexCoords->resize( 4 );
    
    (*quadTexCoords)[ 0 ].set( 0.0, 0.0 );
    (*quadTexCoords)[ 1 ].set( 1.0, 0.0 );
    (*quadTexCoords)[ 2 ].set( 1.0, 1.0 );
    (*quadTexCoords)[ 3 ].set( 0.0, 1.0 );
    
    //Create the quad geometry
    osg::ref_ptr< osg::Geometry > quadGeometry = new osg::Geometry();
    quadGeometry->setVertexArray( quadVertices.get() );
    quadGeometry->addPrimitiveSet( new osg::DrawArrays(
                                                       osg::PrimitiveSet::QUADS, 0, quadVertices->size() ) );
    quadGeometry->setTexCoordArray( 0, quadTexCoords.get() );
    quadGeometry->setUseDisplayList( true );

    osg::ref_ptr< osg::Vec4Array > c = new osg::Vec4Array();
    c->push_back( osg::Vec4( 1.0, 1.0, 0., 1. ) );
    quadGeometry->setColorArray( c.get() );
    quadGeometry->setColorBinding( osg::Geometry::BIND_OVERALL );
    //Set the stateset for the quad
    osg::ref_ptr< osg::StateSet > stateset =
    quadGeometry->getOrCreateStateSet();
    stateset->setMode( GL_LIGHTING, osg::StateAttribute::OFF );
    
    osg::Geode* quadGeode = new osg::Geode();
    quadGeode->setCullingActive( false );
    quadGeode->addDrawable( quadGeometry.get() );
    m_bgTexture->addChild( quadGeode );
    //osg::ref_ptr< osg::StateSet > ss = m_bgTexture->getOrCreateStateSet();
    stateset->setTextureAttributeAndModes( 0, rainbowTexture.get(), osg::StateAttribute::ON );
}
