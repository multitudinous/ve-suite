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

//
#include <ves/xplorer/scenegraph/TextTexture.h>

// --- OSG Includes --- //
#include <osg/Geometry>
#include <osg/Texture2D>
#include <osg/BlendFunc>

#include <osgText/Text>

#include <osgDB/ReadFile>

using namespace ves::xplorer::scenegraph;

//#define VES_SRTT_DEBUG

////////////////////////////////////////////////////////////////////////////////
TextTexture::TextTexture( std::string fontFile )
    :
    osg::Geode()
{
    _font = fontFile;

    LoadBackgroundTexture();
    CreateTexturedQuad();
    CreateText();
}
////////////////////////////////////////////////////////////////////////////////
TextTexture::TextTexture(
    const TextTexture& ttexture,
    const osg::CopyOp& copyop )
    :
    osg::Geode( ttexture, copyop )
{
    _font = ttexture._font;
    //_ttUpdateCallback = ttexture._ttUpdateCallback;

    _texture = new osg::Texture2D( *ttexture._texture );
    _text = new osgText::Text( *ttexture._text );
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
    _text->setFont( _font );
    _text->setColor(
        osg::Vec4( _textColor[ 0 ], _textColor[ 1 ],
                   _textColor[ 2 ], _textColor[ 3 ] ) );

    _text->setText( newText );
    //_text->dirtyBound();
    //_text->computeBound();
        
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
    _texture->setImage( osgDB::readImageFile( "Draft2.tga" ) );
}
////////////////////////////////////////////////////////////////////////////////
void TextTexture::CreateTexturedQuad()
{
    osg::ref_ptr< osg::Vec3Array > quadVertices = new osg::Vec3Array();
    quadVertices->resize( 4 );

    (*quadVertices)[ 0 ].set( -1.0, -1.0, -0.01 );
    (*quadVertices)[ 1 ].set(  1.0, -1.0, -0.01 );
    (*quadVertices)[ 2 ].set(  1.0,  1.0, -0.01 );
    (*quadVertices)[ 3 ].set( -1.0,  1.0, -0.01 );

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
    //Set the stateset for the quad
    osg::ref_ptr< osg::StateSet > stateset =
        quadGeometry->getOrCreateStateSet();
    stateset->setMode( GL_LIGHTING, osg::StateAttribute::OFF );
#ifndef VES_SRTT_DEBUG
    stateset->setTextureAttributeAndModes(
          0, _texture.get(), osg::StateAttribute::ON );
#endif

    setCullingActive( false );
    addDrawable( quadGeometry.get() );
    /*
    osg::ref_ptr< osg::BlendFunc > bf = new osg::BlendFunc();
    bf->setFunction( osg::BlendFunc::SRC_ALPHA, 
                    osg::BlendFunc::ONE_MINUS_SRC_ALPHA );
    stateset->setMode( GL_BLEND, osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
    stateset->setAttributeAndModes( bf.get(), osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
    stateset->setRenderBinDetails( 10, std::string( "DepthSortedBin" ) );
    stateset->setNestRenderBins( false );  
    */
}
////////////////////////////////////////////////////////////////////////////////
void TextTexture::CreateText()
{
    _textColor[ 0 ] = 0.0;
    _textColor[ 1 ] = 0.0;
    _textColor[ 2 ] = 1.0;
    _textColor[ 3 ] = 1.0;

    _text = new osgText::Text();
    _text->setMaximumHeight( 2.2f );
    _text->setMaximumWidth( 1.75f );
    
    _text->setAlignment( osgText::Text::LEFT_TOP );
    _text->setPosition(	osg::Vec3( -0.86f, 0.76f, 0.0f ) ); 	

    _text->setFont( _font );
    _text->setColor( osg::Vec4( _textColor[ 0 ],
                                _textColor[ 1 ],
                                _textColor[ 2 ],
                                _textColor[ 3 ] ) );
    _text->setCharacterSize( 0.1 );
    _text->setLayout( osgText::Text::LEFT_TO_RIGHT );
    //_text->setAutoRotateToScreen( true );

    addDrawable( _text.get() );
}
////////////////////////////////////////////////////////////////////////////////