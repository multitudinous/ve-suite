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

// --- My Includes --- //
#include "WaterEntity.h"
#include "TimeUpdateCallback.h"
#include "ViewPositionUpdateCallback.h"

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/CADEntityHelper.h>
#include <ves/xplorer/scenegraph/Sound.h>

// --- osgAL Includes --- //
#ifdef VE_SOUND
#include <osgAL/SoundState>
#endif

// --- OSG Includes --- //
#include <osg/Geometry>
#include <osg/BlendFunc>
#include <osg/Texture3D>
#include <osg/TextureCubeMap>

#include <osgDB/ReadFile>

// --- Bullet Includes --- //
#include <BulletDynamics/Dynamics/btDynamicsWorld.h>

// --- C/C++ Libraries --- //

namespace demo
{

////////////////////////////////////////////////////////////////////////////////
WaterEntity::WaterEntity( std::string geomFile,
                          ves::xplorer::scenegraph::DCS* pluginDCS
#ifdef VE_SOUND
                          , osgAL::SoundManager* soundManager
#endif
                         )
:
CADEntity( geomFile, pluginDCS )
#ifdef VE_SOUND
, m_water( new ves::xplorer::scenegraph::Sound( "Water", GetDCS(), soundManager ) )
#endif
{
#ifdef VE_SOUND
    try
    {
        m_water->LoadFile( "Sounds/Water.wav" );
        m_water->GetSoundState()->setLooping( true );
    }
    catch( ... )
    {
        std::cerr << "Could not load sounds" << std::endl;
    }
#endif
}
////////////////////////////////////////////////////////////////////////////////
WaterEntity::~WaterEntity()
{
#ifdef VE_SOUND
    delete m_water;
#endif
}
////////////////////////////////////////////////////////////////////////////////
void WaterEntity::SetNameAndDescriptions( std::string geomFile )
{
    osg::Node::DescriptionList descriptorsList;
    descriptorsList.push_back( "VE_XML_ID" );
    descriptorsList.push_back( "" );
    GetDCS()->setDescriptions( descriptorsList );
    GetDCS()->setName( geomFile );
}
////////////////////////////////////////////////////////////////////////////////
void WaterEntity::SetShaders( osg::TextureCubeMap* tcm )
{
    SetShaderOne( tcm );
}
////////////////////////////////////////////////////////////////////////////////
void WaterEntity::SetShaderOne( osg::TextureCubeMap* tcm )
{
    char vertexPass[]=
        "uniform vec3 viewPosition; \n"

        "varying vec3 eyePos; \n"
        "varying vec3 normal; \n"

        "void main() \n"
        "{ \n"
            "gl_Position = ftransform(); \n"

            "gl_TexCoord[ 0 ].xyz =vec3( gl_ModelViewMatrix * gl_Vertex ) * 0.8; \n"
            "eyePos = gl_TexCoord[ 0 ].xyz; \n"
            "normal = vec3( gl_NormalMatrix * gl_Normal ); \n"
        "} \n";

    char fragmentPass[]=
        "uniform float time; \n"

        "uniform sampler3D noise; \n"
        "uniform samplerCube skyBox; \n"

        "varying vec3 eyePos; \n"
        "varying vec3 normal; \n"

        "void main() \n"
        "{ \n"
            "float noiseSpeed = 0.18; \n"
            "float waveSpeed = 0.14; \n"

            "vec3 texCoord = gl_TexCoord[ 0 ].xyz; \n"
            "texCoord.x += waveSpeed * time; \n"
            "texCoord.z += noiseSpeed * time; \n"

            "vec4 noisy = texture3D( noise, texCoord ); \n"

            //Signed noise 
            "vec3 bump = 2.0 * noisy.xyz - 1.0; \n"
            "bump.xy *= 0.15; \n"

            //Make sure the normal always points upwards
            "bump.z = 0.8 * abs( bump.z ) + 0.2; \n"

            //Offset the surface normal with the bump
            "bump = normalize( normal + bump ); \n"

            //Find the reflection vector
            "vec3 V = normalize( eyePos ); \n"
            "vec3 R = reflect( V, bump ); \n"
            "vec4 refl = textureCube( skyBox, R ); \n"

            "float lrp = 1.0 - dot( V, bump ); \n"

            "vec4 waterColor = vec4( 0.0, 0.0, 0.0, 1.0 ); \n"
            "float fadeExp = 30.0; \n"
            "float fadeBias = 0.30; \n"
            "vec4 color = mix( waterColor, refl, clamp( fadeBias + pow( lrp, fadeExp ), 0.0, 1.0 ) ); \n"
            "color.a = 0.4; \n"

            //Interpolate between the water color and reflection
            "gl_FragColor = color; \n"
        "} \n";

    osg::ref_ptr< osg::StateSet > stateset = new osg::StateSet();

    osg::ref_ptr< osg::Program > program = new osg::Program();
    osg::ref_ptr< osg::Shader > vertexShader = new osg::Shader( osg::Shader::VERTEX, vertexPass );
    osg::ref_ptr< osg::Shader > fragmentShader = new osg::Shader( osg::Shader::FRAGMENT, fragmentPass );

    program->addShader( vertexShader.get() );
    program->addShader( fragmentShader.get() );

    stateset->setAttribute( program.get(), osg::StateAttribute::ON );

    stateset->setMode( GL_BLEND, osg::StateAttribute::ON );
    stateset->setRenderingHint( osg::StateSet::TRANSPARENT_BIN );

    osg::ref_ptr< osg::BlendFunc > bf = new osg::BlendFunc();
    bf->setFunction( GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA );
    stateset->setAttribute( bf.get(), osg::StateAttribute::ON );

    osg::ref_ptr< osg::Texture3D > texture = new osg::Texture3D();
    texture->setFilter( osg::Texture3D::MIN_FILTER, osg::Texture3D::LINEAR );
    texture->setFilter( osg::Texture3D::MAG_FILTER, osg::Texture3D::LINEAR );
    texture->setWrap( osg::Texture3D::WRAP_S, osg::Texture3D::REPEAT );
    texture->setWrap( osg::Texture3D::WRAP_T, osg::Texture3D::REPEAT );
    texture->setWrap( osg::Texture3D::WRAP_R, osg::Texture3D::REPEAT );
    texture->setImage( osgDB::readImageFile( "Textures/NoiseVolume.dds" ) );

    stateset->setTextureAttributeAndModes( 0, tcm, osg::StateAttribute::ON );
    stateset->setTextureAttributeAndModes( 1, texture.get(), osg::StateAttribute::ON );

    osg::ref_ptr< osg::Uniform > viewPosition = new osg::Uniform( "viewPosition", osg::Vec3( 0, 0, 0 ) );
    osg::ref_ptr< demo::ViewPositionUpdateCallback > viewPositionCallback = new demo::ViewPositionUpdateCallback();
    viewPosition->setUpdateCallback( viewPositionCallback.get() );
    stateset->addUniform( viewPosition.get() );

    osg::ref_ptr< osg::Uniform > time = new osg::Uniform( "time", static_cast< float >( 0.0 ) );
    osg::ref_ptr< demo::TimeUpdateCallback > timeCallback = new demo::TimeUpdateCallback();
    time->setUpdateCallback( timeCallback.get() );
    stateset->addUniform( time.get() );

    osg::ref_ptr< osg::Uniform > skyBox = new osg::Uniform( "skyBox", 0 );
    stateset->addUniform( skyBox.get() );

    osg::ref_ptr< osg::Uniform > noise = new osg::Uniform( "noise", 1 );
    stateset->addUniform( noise.get() );
        
    GetNode()->GetNode()->setStateSet( stateset.get() );
}
////////////////////////////////////////////////////////////////////////////////

} // end demo