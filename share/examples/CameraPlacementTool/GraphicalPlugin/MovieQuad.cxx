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

// --- CPT Includes --- //
#include "MovieQuad.h"

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/CADEntity.h>

// --- OSG Includes --- //
#include <osg/Geometry>
#include <osg/Texture2D>
#include <osg/TextureRectangle>
#include <osg/ImageStream>

#include <osgDB/ReadFile>

// --- C/C++ Includes --- //
#include <iostream>

using namespace cpt;

////////////////////////////////////////////////////////////////////////////////
MovieQuad::MovieQuad( ves::xplorer::scenegraph::DCS* parentDCS  )
    :
    mGeode( NULL ),
    mImageStream( NULL ),
    mCADEntity( NULL )
{
    Initialize();

    mCADEntity =
        new ves::xplorer::scenegraph::CADEntity( mGeode.get(), parentDCS );
    SetNameAndDescriptions( "Movie Quad" );

    double position[ 3 ] = { 0.16, 0.0, -0.25 };
    mCADEntity->GetDCS()->SetTranslationArray( position );
}
////////////////////////////////////////////////////////////////////////////////
MovieQuad::~MovieQuad()
{
    mImageStream->quit();

    if( mCADEntity )
    {
        delete mCADEntity;
    }
}
////////////////////////////////////////////////////////////////////////////////
osg::Geometry* MovieQuad::CreateTexturedQuadGeometry(
    const osg::Vec3& pos, float width, float height, osg::Image* image,
    bool useTextureRectangle, bool xzPlane, bool optionFlip )
{
    bool flip = image->getOrigin() == osg::Image::TOP_LEFT;
    if( optionFlip )
    {
        flip = !flip;
    }

    if( useTextureRectangle )
    {
        osg::Geometry* pictureQuad =
            osg::createTexturedQuadGeometry(
                pos, osg::Vec3( width, 0.0, 0.0 ),
                xzPlane ? osg::Vec3( 0.0, height, 0.0 ) :
                          osg::Vec3( 0.0, 0.0, height ),
                0.0, flip ? image->t() : 0.0, image->s(),
                     flip ? 0.0 : image->t() );

        osg::ref_ptr< osg::TextureRectangle > textureRectangle =
            new osg::TextureRectangle( image );
        textureRectangle->setInternalFormat( GL_RGBA16F_ARB );
        textureRectangle->setSourceFormat( GL_RGBA );
        textureRectangle->setSourceType( GL_FLOAT );
        textureRectangle->setFilter(
            osg::Texture2D::MIN_FILTER, osg::Texture::LINEAR );
        textureRectangle->setFilter(
            osg::Texture2D::MAG_FILTER, osg::Texture::LINEAR );
        textureRectangle->setWrap(
            osg::Texture::WRAP_S, osg::Texture::CLAMP_TO_EDGE );
        textureRectangle->setWrap(
            osg::Texture::WRAP_T, osg::Texture::CLAMP_TO_EDGE );


        pictureQuad->getOrCreateStateSet()->setTextureAttributeAndModes(
            3, textureRectangle.get(),
            osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );

        return pictureQuad;
    }
    else
    {
        osg::Geometry* pictureQuad =
            osg::createTexturedQuadGeometry(
                pos, osg::Vec3( width, 0.0, 0.0 ),
                xzPlane ? osg::Vec3( 0.0, height, 0.0 ) :
                          osg::Vec3( 0.0, 0.0, height ),
                0.0, flip ? 1.0 : 0.0 , 1.0, flip ? 0.0 : 1.0 );

        osg::ref_ptr< osg::Texture2D > texture = new osg::Texture2D( image );
        texture->setResizeNonPowerOfTwoHint( false );
        texture->setInternalFormat( GL_RGBA16F_ARB );
        texture->setSourceFormat( GL_RGBA );
        texture->setSourceType( GL_FLOAT );
        texture->setFilter(
            osg::Texture2D::MIN_FILTER, osg::Texture::LINEAR );
        texture->setFilter(
            osg::Texture2D::MAG_FILTER, osg::Texture::LINEAR );
        texture->setWrap( osg::Texture::WRAP_S, osg::Texture::CLAMP_TO_EDGE );
        texture->setWrap( osg::Texture::WRAP_T, osg::Texture::CLAMP_TO_EDGE );

        pictureQuad->getOrCreateStateSet()->setTextureAttributeAndModes(
            3, texture.get(),
            osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );

        return pictureQuad;
    }
}
////////////////////////////////////////////////////////////////////////////////
void MovieQuad::Initialize()
{
    mGeode = new osg::Geode();

    osg::ref_ptr< osg::StateSet > stateSet = mGeode->getOrCreateStateSet();
    stateSet->setMode( GL_LIGHTING, osg::StateAttribute::OFF );

    static const char* shaderSourceTextureRec = {
    "uniform samplerRect movieTexture; \n"

    "void main() \n"
    "{ \n"
        "vec4 texture_color = \n"
            "textureRect( movieTexture, gl_TexCoord[ 0 ].st ); \n"

        "gl_FragColor = texture_color; \n"
    "} \n" };

    static const char* shaderSourceTexture2D = {
    "uniform sampler2D movieTexture; \n"

    "void main() \n"
    "{ \n"
        "vec4 texture_color = \n"
            "texture2D( movieTexture, gl_TexCoord[ 0 ].st ); \n"

        "gl_FragColor = texture_color; \n"
    "} \n" };

    bool useTextureRectangle = false;
    bool xzPlane = true;

    osg::ref_ptr< osg::Program > program = new osg::Program();
    program->addShader(
        new osg::Shader(
            osg::Shader::FRAGMENT,
            useTextureRectangle ? shaderSourceTextureRec :
                                  shaderSourceTexture2D ) );

    stateSet->addUniform( new osg::Uniform( "movieTexture", 3 ) );

    stateSet->setAttribute(
        program.get(),
        osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );

    osg::Vec3 pos( 0.0, 0.0, 0.0 );
    osg::Vec3 topleft = pos;
    osg::Vec3 bottomright = pos;

    osg::ref_ptr< osg::Image > image =
        osgDB::readImageFile( "Movies/Miscanthus-11-25-2008-01b.avi" );
    mImageStream = dynamic_cast< osg::ImageStream* >( image.get() );
    if( mImageStream.valid() )
    {
        mImageStream->play();
    }

    double ratio = 0.00025;
    double width = ratio * image->s();
    double height = ratio * image->t();
    if( image.valid() )
    {
        osg::notify( osg::NOTICE )
            << "width: " << width
            << " height: " << height
            << std::endl;

        mGeode->addDrawable(
            CreateTexturedQuadGeometry(
                pos, width, height, image.get(),
                useTextureRectangle, false, false ) );

        bottomright =
            pos + osg::Vec3( static_cast< double >( width ),
                             static_cast< double >( height ), 0.0 );

        if( xzPlane )
        {
            pos.y() += image->t() * 1.05;
        }
        else
        {
            pos.z() += image->t() * 1.05;
        }
    }
    else
    {
        std::cout << "Unable to read movie file!" << std::endl;
    }
}
////////////////////////////////////////////////////////////////////////////////
void MovieQuad::SetNameAndDescriptions( const std::string& name )
{
    osg::Node::DescriptionList descriptorsList;
    descriptorsList.push_back( "VE_XML_ID" );
    descriptorsList.push_back( "" );
    mCADEntity->GetDCS()->setDescriptions( descriptorsList );
    mCADEntity->GetDCS()->setName( name );
}
////////////////////////////////////////////////////////////////////////////////
