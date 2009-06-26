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
#include "GrinderEntity.h"

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/SceneManager.h>
#include <ves/xplorer/scenegraph/ResourceManager.h>
#include <ves/xplorer/scenegraph/CADEntityHelper.h>

// --- OSG Includes --- //
#include <osg/Geometry>
#include <osg/Texture2D>
#include <osg/TextureRectangle>
#include <osg/ImageStream>

#include <osgDB/ReadFile>

using namespace cpt;

////////////////////////////////////////////////////////////////////////////////
GrinderEntity::GrinderEntity(
    std::string geomFile,
    osg::Group* parentNode,
    ves::xplorer::scenegraph::DCS* pluginDCS,
    ves::xplorer::scenegraph::PhysicsSimulator* physicsSimulator,
    ves::xplorer::scenegraph::ResourceManager* resourceManager )
    :
    CADEntity( geomFile, pluginDCS, false, false, physicsSimulator ),
    m_resourceManager( resourceManager ),
    mParentNode( parentNode )
{
    GetNode()->GetNode()->setNodeMask( 0 );
    Initialize();
    SetNameAndDescriptions( "Movie Quad" );
}
////////////////////////////////////////////////////////////////////////////////
GrinderEntity::~GrinderEntity()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
osg::Geometry* GrinderEntity::CreateTexturedQuadGeometry(
    const osg::Vec3& pos, float width, float height, osg::Image* image,
    bool useTextureRectangle, bool xyPlane, bool optionFlip )
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
                xyPlane ? osg::Vec3( 0.0, height, 0.0 ) : osg::Vec3( 0.0, 0.0, height ),
                0.0, flip ? image->t() : 0.0, image->s(), flip ? 0.0 : image->t() );

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
            0, textureRectangle.get(), osg::StateAttribute::ON );

        return pictureQuad;
    }
    else
    {
        osg::Geometry* pictureQuad =
            osg::createTexturedQuadGeometry(
                pos, osg::Vec3( width, 0.0, 0.0 ),
                xyPlane ? osg::Vec3( 0.0, height, 0.0 ) : osg::Vec3( 0.0, 0.0, height ),
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
            0, texture.get(), osg::StateAttribute::ON );

        return pictureQuad;
    }
}
////////////////////////////////////////////////////////////////////////////////
void GrinderEntity::Initialize()
{
    osg::ref_ptr< osg::Geode > geode = new osg::Geode();
    mParentNode->addChild( geode.get() );

    osg::ref_ptr< osg::StateSet > stateSet = geode->getOrCreateStateSet();
    stateSet->setMode( GL_LIGHTING, osg::StateAttribute::OFF );

    static const char* shaderSourceTextureRec = {
    //"uniform vec4 cutoff_color; \n"
    "uniform samplerRect movieTexture; \n"

    "void main() \n"
    "{ \n"
        "vec4 texture_color = textureRect( movieTexture, gl_TexCoord[ 0 ].st ); \n"

        "gl_FragColor = texture_color; \n"
    "} \n" };

    static const char* shaderSourceTexture2D = {
    "uniform vec4 cutoff_color; \n"
    "uniform sampler2D movieTexture; \n"

    "void main() \n"
    "{ \n"
        "vec4 texture_color = texture2D( movieTexture, gl_TexCoord[ 0 ].st ); \n"

        "gl_FragColor = texture_color; \n"
    "} \n" };

    bool useTextureRectangle = false;
    bool xyPlane = false;

    osg::ref_ptr< osg::Program > program = new osg::Program();
    program->addShader(
        new osg::Shader(
            osg::Shader::FRAGMENT,
            useTextureRectangle ? shaderSourceTextureRec : shaderSourceTexture2D ) );

    stateSet->addUniform( new osg::Uniform( "movieTexture", 0 ) );

    stateSet->setAttribute(
        program.get(), osg::StateAttribute::PROTECTED );

    osg::Vec3 pos( 0.0, 0.0, 0.0 );
    osg::Vec3 topleft = pos;
    osg::Vec3 bottomright = pos;

    osg::ref_ptr< osg::Image > image =
        osgDB::readImageFile( "Movies/Side_View.avi" );
    osg::ref_ptr< osg::ImageStream > imageStream =
        dynamic_cast< osg::ImageStream* >( image.get() );
    if( imageStream.valid() )
    {
        imageStream->play();   
    }

    if( image.valid() )
    {
        osg::notify( osg::NOTICE )
            << "image->s() " << image->s()
            << "image->t()=" << image->t()
            << std::endl;

        geode->addDrawable(
            CreateTexturedQuadGeometry(
                pos, image->s(), image->t(), image.get(),
                useTextureRectangle, false, false ) );

        bottomright =
            pos + osg::Vec3( static_cast< float >( image->s() ),
                             static_cast< float >( image->t() ), 0.0 );

        if( xyPlane )
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
void GrinderEntity::SetNameAndDescriptions( const std::string& name )
{
    osg::Node::DescriptionList descriptorsList;
    descriptorsList.push_back( "VE_XML_ID" );
    descriptorsList.push_back( "" );
    GetDCS()->setDescriptions( descriptorsList );
    GetDCS()->setName( name );
}
////////////////////////////////////////////////////////////////////////////////
