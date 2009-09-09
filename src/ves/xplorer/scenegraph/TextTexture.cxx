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

// --- OSG Includes --- //
#include <osg/Geometry>
#include <osg/Texture2D>
#include <osg/BlendFunc>
#include <osg/Depth>

#include <osgText/Text>

#include <osgDB/ReadFile>
#include <osgDB/FileUtils>

#include <osgBullet/Chart.h>

using namespace ves::xplorer::scenegraph;

//#define VES_SRTT_DEBUG

////////////////////////////////////////////////////////////////////////////////
TextTexture::TextTexture( std::string fontFile )
    :
    osg::Group()
{
    _font = fontFile;

    LoadBackgroundTexture();
    CreateTexturedQuad();
    CreateText();
    CreateChart();
}
////////////////////////////////////////////////////////////////////////////////
TextTexture::TextTexture(
    const TextTexture& ttexture,
    const osg::CopyOp& copyop )
    :
    osg::Group( ttexture, copyop )
{
    _font = ttexture._font;
    //_ttUpdateCallback = ttexture._ttUpdateCallback;

    _texture = new osg::Texture2D( *ttexture._texture );
    m_bodyText = new osgText::Text( *ttexture.m_bodyText );
    m_titleText = new osgText::Text( *ttexture.m_titleText );
    m_title = ttexture.m_title;
    _textColor[ 0 ] = ttexture._textColor[ 0 ];
    _textColor[ 1 ] = ttexture._textColor[ 1 ];
    _textColor[ 2 ] = ttexture._textColor[ 2 ];
    _textColor[ 3 ] = ttexture._textColor[ 3 ];
}
////////////////////////////////////////////////////////////////////////////////
TextTexture::~TextTexture()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void TextTexture::SetFont( std::string fontFile )
{
    _font = fontFile;
}
////////////////////////////////////////////////////////////////////////////////
void TextTexture::UpdateText( std::string newText )
{
    m_bodyText->setFont( _font );
    m_bodyText->setColor(
        osg::Vec4( _textColor[ 0 ], _textColor[ 1 ],
                   _textColor[ 2 ], _textColor[ 3 ] ) );

    m_bodyText->setText( newText );
    //m_bodyText->dirtyBound();
    //m_bodyText->computeBound();
        
    //dirtyBound();
    //computeBound();
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
    _texture = new osg::Texture2D();
    _texture->setInternalFormat( GL_RGBA );
    //GL_RGBA8/GL_UNSIGNED_INT - GL_RGBA16F_ARB/GL_FLOAT 
    _texture->setSourceFormat( GL_RGBA8 );
    //_texture->setInternalFormat( GL_RGBA16F_ARB );
    _texture->setSourceType( GL_UNSIGNED_INT );
    //_texture->setSourceType( GL_FLOAT );
    _texture->setFilter( osg::Texture2D::MIN_FILTER, osg::Texture2D::LINEAR );
    _texture->setFilter( osg::Texture2D::MAG_FILTER, osg::Texture2D::LINEAR );
    _texture->setWrap( osg::Texture2D::WRAP_S, osg::Texture2D::CLAMP_TO_EDGE );
    _texture->setWrap( osg::Texture2D::WRAP_T, osg::Texture2D::CLAMP_TO_EDGE );
    _texture->setImage( osgDB::readImageFile( "Info_Panel-No_Title.png" ) );
}
////////////////////////////////////////////////////////////////////////////////
void TextTexture::CreateTexturedQuad()
{
    osg::ref_ptr< osg::Vec3Array > quadVertices = new osg::Vec3Array();
    quadVertices->resize( 4 );

    (*quadVertices)[ 0 ].set( -1.0,  0.01, -2.0 );
    (*quadVertices)[ 1 ].set(  1.0,  0.01, -2.0 );
    (*quadVertices)[ 2 ].set(  1.0,  0.01,  2.0 );
    (*quadVertices)[ 3 ].set( -1.0,  0.01,  2.0 );

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
    quadGeometry->addPrimitiveSet(
        new osg::DrawArrays(
            osg::PrimitiveSet::QUADS, 0, quadVertices->size() ) );
    quadGeometry->setTexCoordArray( 0, quadTexCoords.get() );
    quadGeometry->setUseDisplayList( true );
#ifndef VES_SRTT_DEBUG
    quadGeometry->setColorBinding( osg::Geometry::BIND_OFF );
#else
    osg::ref_ptr< osg::Vec4Array > colorArray = new osg::Vec4Array();
    colorArray->push_back( osg::Vec4( 0.0, 0.0, 0.0, 1.0 ) );
    quadGeometry->setColorArray( colorArray.get() );
    quadGeometry->setColorBinding( osg::Geometry::BIND_OVERALL );
#endif
    osg::ref_ptr< osg::StateSet > drawable_stateset =
        quadGeometry->getOrCreateStateSet();
    
    osg::ref_ptr< osg::BlendFunc > bf = new osg::BlendFunc();
    bf->setFunction( osg::BlendFunc::SRC_ALPHA, 
                    osg::BlendFunc::ONE_MINUS_SRC_ALPHA );
    drawable_stateset->setMode( GL_BLEND, 
        osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
    drawable_stateset->setAttributeAndModes( bf.get(), 
        osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
    drawable_stateset->
    setRenderBinDetails( 22, std::string( "DepthSortedBin" ) );
    drawable_stateset->setNestRenderBins( false );
    drawable_stateset->
        setAttributeAndModes( new osg::Depth( osg::Depth::ALWAYS ),
        osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
    

    //Set the stateset for the quad
    osg::Geode* texttureGeode = new osg::Geode();
    texttureGeode->setCullingActive( false );
    texttureGeode->addDrawable( quadGeometry.get() );
    
    osg::ref_ptr< osg::StateSet > stateset =
        texttureGeode->getOrCreateStateSet();
    stateset->setMode( GL_LIGHTING, osg::StateAttribute::OFF );    
#ifndef VES_SRTT_DEBUG
    stateset->setTextureAttributeAndModes(
          0, _texture.get(), osg::StateAttribute::ON );
#endif
    
    addChild( texttureGeode );

    getOrCreateStateSet()->addUniform(
        new osg::Uniform( "glowColor", osg::Vec4( 0.0, 0.0, 0.0, 1.0) ) );
    
    std::string shaderName = osgDB::findDataFile( "opacity-ignore-white.fs" );
    osg::ref_ptr< osg::Shader > fragShader = 
        osg::Shader::readShaderFile( osg::Shader::FRAGMENT, shaderName );
    
    osg::ref_ptr< osg::Program > program = new osg::Program();
    program->addShader( fragShader.get() );

    stateset->setAttributeAndModes( program.get(),
        osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
    
    stateset->addUniform(
        new osg::Uniform( "opacityVal", 0.85f ) );

    stateset->addUniform( new osg::Uniform( "tex", 0 ) );
}
////////////////////////////////////////////////////////////////////////////////
void TextTexture::CreateText()
{
    _textColor[ 0 ] = 0.0;
    _textColor[ 1 ] = 0.0;
    _textColor[ 2 ] = 1.0;
    _textColor[ 3 ] = 1.0;
    /////////////////////////////
    //Set the title text
    m_titleText = new osgText::Text();
    m_titleText->setMaximumHeight( 1.5f );
    m_titleText->setMaximumWidth( 1.75f );
    m_titleText->setAxisAlignment( osgText::TextBase::XZ_PLANE );
    m_titleText->setAlignment( osgText::Text::LEFT_TOP );
    m_titleText->setPosition(	osg::Vec3( -0.44f, -0.01f, 1.85f ) );
    
    m_titleText->setFont( _font );
    m_titleText->setColor( osg::Vec4( _textColor[ 0 ],
                               _textColor[ 1 ],
                               _textColor[ 2 ],
                               _textColor[ 3 ] ) );
    m_titleText->setCharacterSize( 0.2 );
    m_titleText->setLayout( osgText::Text::LEFT_TO_RIGHT );
    //m_titleText->setAutoRotateToScreen( true );
    {
        osg::Geode* titleTextGeode = new osg::Geode();
        titleTextGeode->addDrawable( m_titleText.get() );

        titleTextGeode->getOrCreateStateSet()->setMode( GL_LIGHTING, osg::StateAttribute::OFF );    
        
        osg::ref_ptr< osg::StateSet > drawable_stateset = 
            m_titleText->getOrCreateStateSet();
        osg::ref_ptr< osg::BlendFunc > bf = new osg::BlendFunc();
        bf->setFunction( osg::BlendFunc::SRC_ALPHA, 
            osg::BlendFunc::ONE_MINUS_SRC_ALPHA );
        drawable_stateset->setMode( GL_BLEND, 
            osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
        drawable_stateset->setAttributeAndModes( bf.get(), 
            osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
        
        drawable_stateset->setRenderBinDetails( 22, std::string( "DepthSortedBin" ) );
        drawable_stateset->setNestRenderBins( false );
        
        std::string shaderName = osgDB::findDataFile( "null_glow_texture.fs" );
        osg::ref_ptr< osg::Shader > fragShader = 
        osg::Shader::readShaderFile( osg::Shader::FRAGMENT, shaderName );
        
        osg::ref_ptr< osg::Program > program = new osg::Program();
        program->addShader( fragShader.get() );
        
        drawable_stateset->setAttributeAndModes( program.get(),
                                       osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
        
        //stateset->addUniform( new osg::Uniform( "opacityVal", 0.70f ) );
        
        drawable_stateset->addUniform( new osg::Uniform( "tex", 0 ) );
        
        addChild( titleTextGeode );
    }

    /////////////////////////////
    //Set the body text
    m_bodyText = new osgText::Text();
    m_bodyText->setMaximumHeight( 3.5f );
    m_bodyText->setMaximumWidth( 1.75f );
    m_bodyText->setAxisAlignment( osgText::TextBase::XZ_PLANE );
    m_bodyText->setAlignment( osgText::Text::LEFT_TOP );
    m_bodyText->setPosition(	osg::Vec3( -0.86f, -0.01f, 1.5f ) );

    m_bodyText->setFont( _font );
    m_bodyText->setColor( osg::Vec4( _textColor[ 0 ],
                                _textColor[ 1 ],
                                _textColor[ 2 ],
                                _textColor[ 3 ] ) );
    m_bodyText->setCharacterSize( 0.13 );
    m_bodyText->setLayout( osgText::Text::LEFT_TO_RIGHT );
    //m_bodyText->setAutoRotateToScreen( true );
    {
        osg::Geode* textGeode = new osg::Geode();
        textGeode->addDrawable( m_bodyText.get() );

        textGeode->getOrCreateStateSet()->setMode( GL_LIGHTING, osg::StateAttribute::OFF );    
        osg::ref_ptr< osg::StateSet > drawable_stateset = m_titleText->getOrCreateStateSet();

        osg::ref_ptr< osg::BlendFunc > bf = new osg::BlendFunc();
        bf->setFunction( osg::BlendFunc::SRC_ALPHA, 
                        osg::BlendFunc::ONE_MINUS_SRC_ALPHA );
        drawable_stateset->setMode( GL_BLEND, 
                          osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
        drawable_stateset->setAttributeAndModes( bf.get(), 
                                       osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
        
        drawable_stateset->setRenderBinDetails( 22, std::string( "DepthSortedBin" ) );
        drawable_stateset->setNestRenderBins( false );
        
        std::string shaderName = osgDB::findDataFile( "null_glow_texture.fs" );
        osg::ref_ptr< osg::Shader > fragShader = 
        osg::Shader::readShaderFile( osg::Shader::FRAGMENT, shaderName );
        
        osg::ref_ptr< osg::Program > program = new osg::Program();
        program->addShader( fragShader.get() );

        drawable_stateset->setAttributeAndModes( program.get(),
                                       osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
        
        //stateset->addUniform( new osg::Uniform( "opacityVal", 0.70f ) );
        
        drawable_stateset->addUniform( new osg::Uniform( "tex", 0 ) );
        
        addChild( textGeode );
    }
}
////////////////////////////////////////////////////////////////////////////////
void TextTexture::CreateChart()
{
    m_chartSurface = new osgBullet::Chart();
    osg::Vec4 bg( 1.f, 0.f, 0.f, .33f );
    m_chartSurface->setBackgroundColor( bg );
    osg::Vec4 fg( 1.f, 1.f, 0.f, .5f );
    m_chartSurface->setForegroundColor( fg );
    m_chartSurface->setChartLocationAndSize( 0.045, 0.1, 0.19, 0.1 );
    //m_chartSurface->setChartLocationAndSize( 0.05, 0.05, 2, 1 );
    m_chartSurface->createChart();
    //addChild( m_chartSurface->get() );
}
////////////////////////////////////////////////////////////////////////////////
osgBullet::Chart* TextTexture::GetChart()
{
    return m_chartSurface;
}
////////////////////////////////////////////////////////////////////////////////
const std::string& TextTexture::GetTitle()
{
    return m_title;
}
////////////////////////////////////////////////////////////////////////////////
void TextTexture::SetTitle( const std::string& title )
{
    m_title = title;
    
    m_titleText->setFont( _font );
    m_titleText->setColor(
                         osg::Vec4( _textColor[ 0 ], _textColor[ 1 ],
                                   _textColor[ 2 ], _textColor[ 3 ] ) );
    
    m_titleText->setText( m_title );
}
////////////////////////////////////////////////////////////////////////////////
