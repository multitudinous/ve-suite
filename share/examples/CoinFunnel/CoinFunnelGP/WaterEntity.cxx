// --- My Includes --- //
#include "WaterEntity.h"
#include "TimeUpdateCallback.h"
#include "ViewPositionUpdateCallback.h"

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/CADEntityHelper.h>

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
WaterEntity::WaterEntity( std::string geomFile, ves::xplorer::scenegraph::DCS* pluginDCS )
:
CADEntity( geomFile, pluginDCS )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
WaterEntity::~WaterEntity()
{
    ;
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

        "varying vec3 vTexCoord; \n"
        "varying vec3 vNormal; \n"
        "varying vec3 vViewVec; \n"

        "void main() \n"
        "{ \n"
            "gl_Position = ftransform(); \n"

            "vTexCoord = gl_Vertex.xyz * 0.8; \n"
            "vViewVec = vTexCoord - viewPosition; \n"
            "vNormal = gl_Normal; \n"
        "} \n";

    char fragmentPass[]=
        "uniform float time; \n"

        "uniform sampler3D noise; \n"
        "uniform samplerCube skyBox; \n"

        "varying vec3 vTexCoord; \n"
        "varying vec3 vNormal; \n"
        "varying vec3 vViewVec; \n"

        "void main() \n"
        "{ \n"
            "vec3 tcoord = vTexCoord; \n"

            "float noiseSpeed = 0.18; \n"
            "float waveSpeed = 0.34; \n"
            "tcoord.x += waveSpeed * time; \n"
            "tcoord.z += noiseSpeed * time; \n"

            "vec4 noisy = texture3D( noise, tcoord ); \n"

            //Signed noise 
            "vec3 bump = 2.0 * noisy.xyz - 1.0; \n"
            "bump.xy *= 0.15; \n"

            //Make sure the normal always points upwards
            "bump.z = 0.8 * abs( bump.z ) + 0.2; \n"

            //Offset the surface normal with the bump
            "bump = normalize( vNormal + bump ); \n"

            //Find the reflection vector
            "vec3 reflVec = reflect( vViewVec, bump ); \n"
            "vec4 refl = textureCube( skyBox, reflVec.yzx ); \n"

            "float lrp = 1.0 - dot( -normalize( vViewVec ), bump ); \n"

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